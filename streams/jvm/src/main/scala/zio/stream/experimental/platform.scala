package zio.stream.experimental

import zio.{Canceler, Chunk, ChunkBuilder, FiberFailure, Queue, UIO, ZIO, ZManaged, stream}

trait ZStreamPlatformSpecificConstructors {
  self: ZStream.type =>

  /**
   * Creates a stream from an asynchronous callback that can be called multiple times.
   * The optionality of the error type `E` can be used to signal the end of the stream,
   * by setting it to `None`.
   */
  def effectAsync[R, E, A](
    register: (ZIO[R, Option[E], Chunk[A]] => Unit) => Unit,
    outputBuffer: Int = 16
  ): ZStream[R, E, A] =
    effectAsyncMaybe(
      callback => {
        register(callback)
        None
      },
      outputBuffer
    )

  /**
   * Creates a stream from an asynchronous callback that can be called multiple times.
   * The registration of the callback returns either a canceler or synchronously returns a stream.
   * The optionality of the error type `E` can be used to signal the end of the stream, by
   * setting it to `None`.
   */
  def effectAsyncInterrupt[R, E, A](
    register: (ZIO[R, Option[E], Chunk[A]] => Unit) => Either[Canceler[R], ZStream[R, E, A]],
    outputBuffer: Int = 16
  ): ZStream[R, E, A] =
    ZStream.unwrapManaged(for {
      output  <- Queue.bounded[stream.Take[E, A]](outputBuffer).toManagedWith(_.shutdown)
      runtime <- ZManaged.runtime[R]
      eitherStream <- ZManaged.succeed {
                        register { k =>
                          try {
                            runtime.unsafeRun(stream.Take.fromPull(k).flatMap(output.offer))
                            ()
                          } catch {
                            case FiberFailure(c) if c.interrupted =>
                          }
                        }
                      }
    } yield {
      eitherStream match {
        case Right(value) => ZStream.unwrap(output.shutdown as value)
        case Left(canceler) =>
          lazy val loop: ZChannel[Any, Any, Any, Any, E, Chunk[A], Unit] =
            ZChannel.unwrap(
              output.take
                .flatMap(_.done)
                .fold(
                  maybeError =>
                    ZChannel.fromEffect(output.shutdown) *>
                      maybeError
                        .fold[ZChannel[Any, Any, Any, Any, E, Chunk[A], Unit]](ZChannel.end(()))(ZChannel.fail(_)),
                  a => ZChannel.write(a) *> loop
                )
            )

          new ZStream(loop).ensuring(canceler)
      }
    })

  /**
   * Creates a stream from an asynchronous callback that can be called multiple times
   * The registration of the callback itself returns an effect. The optionality of the
   * error type `E` can be used to signal the end of the stream, by setting it to `None`.
   */
  def effectAsyncM[R, E, A](
    register: (ZIO[R, Option[E], Chunk[A]] => Unit) => ZIO[R, E, Any],
    outputBuffer: Int = 16
  ): ZStream[R, E, A] =
    new ZStream(ZChannel.unwrapManaged(for {
      output  <- Queue.bounded[stream.Take[E, A]](outputBuffer).toManagedWith(_.shutdown)
      runtime <- ZManaged.runtime[R]
      _ <- register { k =>
             try {
               runtime.unsafeRun(stream.Take.fromPull(k).flatMap(output.offer))
               ()
             } catch {
               case FiberFailure(c) if c.interrupted =>
             }
           }.toManaged
    } yield {
      lazy val loop: ZChannel[Any, Any, Any, Any, E, Chunk[A], Unit] = ZChannel.unwrap(
        output.take
          .flatMap(_.done)
          .foldCauseM(
            maybeError =>
              output.shutdown as (maybeError.failureOrCause match {
                case Left(Some(failure)) =>
                  ZChannel.fail(failure)
                case Left(None) =>
                  ZChannel.end(())
                case Right(cause) =>
                  ZChannel.halt(cause)
              }),
            a => ZIO.succeedNow(ZChannel.write(a) *> loop)
          )
      )

      loop
    }))

  /**
   * Creates a stream from an asynchronous callback that can be called multiple times.
   * The registration of the callback can possibly return the stream synchronously.
   * The optionality of the error type `E` can be used to signal the end of the stream,
   * by setting it to `None`.
   */
  def effectAsyncMaybe[R, E, A](
    register: (ZIO[R, Option[E], Chunk[A]] => Unit) => Option[ZStream[R, E, A]],
    outputBuffer: Int = 16
  ): ZStream[R, E, A] =
    effectAsyncInterrupt(k => register(k).toRight(UIO.unit), outputBuffer)

  /**
   * Creates a stream from an blocking iterator that may throw exceptions.
   */
  def fromBlockingIterator[A](iterator: => Iterator[A], maxChunkSize: Int = 1): ZStream[Any, Throwable, A] =
    new ZStream(
      ZChannel.fromEffect(ZIO.effect(iterator)).flatMap { iterator =>
        if (maxChunkSize <= 1) {
          def loop: ZChannel[Any, Any, Any, Any, Throwable, Chunk[A], Unit] =
            if (iterator.hasNext)
              ZChannel.fromEffect(ZIO.effectBlocking(Chunk.single(iterator.next()))).flatMap(ZChannel.write) *> loop
            else
              ZChannel.unit

          loop
        } else {
          val builder  = ChunkBuilder.make[A](maxChunkSize)
          val blocking = ZChannel.fromEffect(ZIO.effectBlocking(builder += iterator.next()))

          def loop(i: Int): ZChannel[Any, Any, Any, Any, Throwable, Chunk[A], Unit] = {
            val hasNext = iterator.hasNext

            if (i < maxChunkSize && hasNext) {
              blocking *> loop(i + 1)
            } else {
              val chunk = builder.result()

              if (chunk.isEmpty) {
                ZChannel.unit
              } else if (hasNext) {
                builder.clear()
                ZChannel.write(chunk) *> loop(0)
              } else {
                ZChannel.write(chunk) *> ZChannel.unit
              }
            }
          }

          loop(0)
        }
      }
    )

  /**
   * Creates a stream from an blocking Java iterator that may throw exceptions.
   */
  def fromBlockingJavaIterator[A](
    iter: => java.util.Iterator[A],
    maxChunkSize: Int = 1
  ): ZStream[Any, Throwable, A] =
    fromBlockingIterator(
      new Iterator[A] {
        def next(): A        = iter.next
        def hasNext: Boolean = iter.hasNext
      },
      maxChunkSize
    )
}

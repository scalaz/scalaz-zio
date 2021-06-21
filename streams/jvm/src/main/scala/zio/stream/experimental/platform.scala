package zio.stream.experimental

import zio.{Canceler, Chunk, FiberFailure, Managed, Promise, Queue, UIO, UManaged, ZIO, ZManaged, stream}

import java.io._
import java.net.InetSocketAddress
import java.nio.channels.{AsynchronousServerSocketChannel, AsynchronousSocketChannel, CompletionHandler, FileChannel}
import java.nio.file.Path
import java.nio.{Buffer, ByteBuffer}

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
   * Creates a stream of bytes from a file at the specified path.
   */
  def fromFile(path: => Path, chunkSize: Int = ZStream.DefaultChunkSize): ZStream[Any, Throwable, Byte] =
    ZStream
      .bracket(ZIO.effectBlockingInterrupt(FileChannel.open(path)))(chan => ZIO.effectBlocking(chan.close()).orDie)
      .flatMap { channel =>
        ZStream.fromEffect(UIO(ByteBuffer.allocate(chunkSize))).flatMap { reusableBuffer =>
          ZStream.repeatEffectChunkOption(
            for {
              bytesRead <- ZIO.effectBlockingInterrupt(channel.read(reusableBuffer)).asSomeError
              _         <- ZIO.fail(None).when(bytesRead == -1)
              chunk <- UIO {
                         reusableBuffer.flip()
                         Chunk.fromByteBuffer(reusableBuffer)
                       }
            } yield chunk
          )
        }
      }

  /**
   * Creates a stream from a `java.io.InputStream`.
   * Note: the input stream will not be explicitly closed after it is exhausted.
   */
  def fromInputStream(
    is: => InputStream,
    chunkSize: Int = ZStream.DefaultChunkSize
  ): ZStream[Any, IOException, Byte] =
    ZStream.fromEffect(UIO(is)).flatMap { capturedIs =>
      ZStream.repeatEffectChunkOption {
        for {
          bufArray  <- UIO(Array.ofDim[Byte](chunkSize))
          bytesRead <- ZIO.effectBlockingIO(capturedIs.read(bufArray)).asSomeError
          bytes <- if (bytesRead < 0)
                     ZIO.fail(None)
                   else if (bytesRead == 0)
                     UIO(Chunk.empty)
                   else if (bytesRead < chunkSize)
                     UIO(Chunk.fromArray(bufArray).take(bytesRead))
                   else
                     UIO(Chunk.fromArray(bufArray))
        } yield bytes
      }
    }

  /**
   * Creates a stream from the resource specified in `path`
   */
  final def fromResource(
    path: String,
    chunkSize: Int = ZStream.DefaultChunkSize
  ): ZStream[Any, IOException, Byte] =
    ZStream.managed {
      ZManaged.fromAutoCloseable {
        ZIO.effectBlockingIO(getClass.getClassLoader.getResourceAsStream(path.replace('\\', '/'))).flatMap { x =>
          if (x == null)
            ZIO.fail(new FileNotFoundException(s"No such resource: '$path'"))
          else
            ZIO.succeedNow(x)
        }
      }
    }.flatMap(is => fromInputStream(is, chunkSize = chunkSize))

  /**
   * Creates a stream from a `java.io.InputStream`. Ensures that the input
   * stream is closed after it is exhausted.
   */
  def fromInputStreamEffect[R](
    is: ZIO[R, IOException, InputStream],
    chunkSize: Int = ZStream.DefaultChunkSize
  ): ZStream[R, IOException, Byte] =
    fromInputStreamManaged(is.toManaged(is => ZIO.effectTotal(is.close())), chunkSize)

  /**
   * Creates a stream from a managed `java.io.InputStream` value.
   */
  def fromInputStreamManaged[R](
    is: ZManaged[R, IOException, InputStream],
    chunkSize: Int = ZStream.DefaultChunkSize
  ): ZStream[R, IOException, Byte] =
    ZStream.managed(is).flatMap(fromInputStream(_, chunkSize))

  /**
   * Creates a stream from `java.io.Reader`.
   */
  def fromReader(reader: => Reader, chunkSize: Int = ZStream.DefaultChunkSize): ZStream[Any, IOException, Char] =
    ZStream.fromEffect(UIO(reader)).flatMap { capturedReader =>
      ZStream.repeatEffectChunkOption {
        for {
          bufArray  <- UIO(Array.ofDim[Char](chunkSize))
          bytesRead <- ZIO.effectBlockingIO(capturedReader.read(bufArray)).asSomeError
          chars <- if (bytesRead < 0)
                     ZIO.fail(None)
                   else if (bytesRead == 0)
                     UIO(Chunk.empty)
                   else if (bytesRead < chunkSize)
                     UIO(Chunk.fromArray(bufArray).take(bytesRead))
                   else
                     UIO(Chunk.fromArray(bufArray))
        } yield chars
      }
    }

  /**
   * Creates a stream from an effect producing `java.io.Reader`.
   */
  def fromReaderEffect[R](
    reader: => ZIO[R, IOException, Reader],
    chunkSize: Int = ZStream.DefaultChunkSize
  ): ZStream[R, IOException, Char] =
    fromReaderManaged(reader.toManaged(r => ZIO.effectTotal(r.close())), chunkSize)

  /**
   * Creates a stream from managed `java.io.Reader`.
   */
  def fromReaderManaged[R](
    reader: => ZManaged[R, IOException, Reader],
    chunkSize: Int = ZStream.DefaultChunkSize
  ): ZStream[R, IOException, Char] =
    ZStream.managed(reader).flatMap(fromReader(_, chunkSize))

  /**
   * Creates a stream from a callback that writes to `java.io.OutputStream`.
   * Note: the input stream will be closed after the `write` is done.
   */
  def fromOutputStreamWriter(
    write: OutputStream => Unit,
    chunkSize: Int = ZStream.DefaultChunkSize
  ): ZStream[Any, Throwable, Byte] = {
    def from(in: InputStream, out: OutputStream, err: Promise[Throwable, None.type]) = {
      val readIn = fromInputStream(in, chunkSize).ensuring(ZIO.effectTotal(in.close()))
      val writeOut = ZStream.fromEffect {
        ZIO
          .effectBlockingInterrupt(write(out))
          .run
          .tap(exit => err.done(exit.as(None)))
          .ensuring(ZIO.effectTotal(out.close()))
      }

      val handleError = ZStream.fromEffectOption(err.await.some)
      readIn.drainFork(writeOut) ++ handleError
    }

    for {
      out    <- ZStream.fromEffect(ZIO.effectTotal(new PipedOutputStream()))
      in     <- ZStream.fromEffect(ZIO.effectTotal(new PipedInputStream(out)))
      err    <- ZStream.fromEffect(Promise.make[Throwable, None.type])
      result <- from(in, out, err)
    } yield result
  }

  /**
   * Creates a stream from a Java stream
   */
  final def fromJavaStream[R, A](stream: => java.util.stream.Stream[A]): ZStream[R, Throwable, A] =
    ZStream.fromJavaIterator(stream.iterator())

  /**
   * Creates a stream from a Java stream
   */
  final def fromJavaStreamEffect[R, A](
    stream: ZIO[R, Throwable, java.util.stream.Stream[A]]
  ): ZStream[R, Throwable, A] =
    ZStream.fromJavaIteratorEffect(stream.flatMap(s => UIO(s.iterator())))

  /**
   * Creates a stream from a managed Java stream
   */
  final def fromJavaStreamManaged[R, A](
    stream: ZManaged[R, Throwable, java.util.stream.Stream[A]]
  ): ZStream[R, Throwable, A] =
    ZStream.fromJavaIteratorManaged(stream.mapM(s => UIO(s.iterator())))

  /**
   * Creates a stream from a Java stream
   */
  final def fromJavaStreamTotal[A](stream: => java.util.stream.Stream[A]): ZStream[Any, Nothing, A] =
    ZStream.fromJavaIteratorTotal(stream.iterator())

  /**
   * Create a stream of accepted connection from server socket
   * Emit socket `Connection` from which you can read / write and ensure it is closed after it is used
   */
  def fromSocketServer(
    port: Int,
    host: Option[String] = None
  ): ZStream[Any, Throwable, Connection] =
    for {
      server <- ZStream.managed(ZManaged.fromAutoCloseable(ZIO.effectBlocking {
                  AsynchronousServerSocketChannel
                    .open()
                    .bind(
                      host.fold(new InetSocketAddress(port))(new InetSocketAddress(_, port))
                    )
                }))

      registerConnection <- ZStream.managed(ZManaged.scope)

      conn <- ZStream.repeatEffect {
                ZIO
                  .effectAsync[Any, Throwable, UManaged[Connection]] { callback =>
                    server.accept(
                      null,
                      new CompletionHandler[AsynchronousSocketChannel, Void]() {
                        self =>
                        override def completed(socket: AsynchronousSocketChannel, attachment: Void): Unit =
                          callback(ZIO.succeed(Connection.make(socket)))

                        override def failed(exc: Throwable, attachment: Void): Unit = callback(ZIO.fail(exc))
                      }
                    )
                  }
                  .flatMap(managedConn => registerConnection(managedConn).map(_._2))
              }
    } yield conn

  /**
   * Accepted connection made to a specific channel `AsynchronousServerSocketChannel`
   */
  class Connection(socket: AsynchronousSocketChannel) {

    /**
     * Read the entire `AsynchronousSocketChannel` by emitting a `Chunk[Byte]`
     * The caller of this function is NOT responsible for closing the `AsynchronousSocketChannel`.
     */
    def read: ZStream[Any, Throwable, Byte] =
      ZStream.unfoldChunkM(0) {
        case -1 => ZIO.succeed(Option.empty)
        case _ =>
          val buff = ByteBuffer.allocate(ZStream.DefaultChunkSize)

          ZIO.effectAsync[Any, Throwable, Option[(Chunk[Byte], Int)]] { callback =>
            socket.read(
              buff,
              null,
              new CompletionHandler[Integer, Void] {
                override def completed(bytesRead: Integer, attachment: Void): Unit = {
                  (buff: Buffer).flip()
                  callback(ZIO.succeed(Option(Chunk.fromByteBuffer(buff) -> bytesRead.toInt)))
                }

                override def failed(error: Throwable, attachment: Void): Unit = callback(ZIO.fail(error))
              }
            )
          }
      }

    /**
     * Write the entire Chuck[Byte] to the socket channel.
     * The caller of this function is NOT responsible for closing the `AsynchronousSocketChannel`.
     *
     * The sink will yield the count of bytes written.
     */
    def write: ZSink[Any, Throwable, Byte, Throwable, Nothing, Int] =
      ZSink.foldLeftChunksM[Any, Throwable, Byte, Int](0) { case (nbBytesWritten, c) =>
        ZIO.effectAsync[Any, Throwable, Int] { callback =>
          socket.write(
            ByteBuffer.wrap(c.toArray),
            null,
            new CompletionHandler[Integer, Void] {
              override def completed(result: Integer, attachment: Void): Unit =
                callback(ZIO.succeed(nbBytesWritten + result.toInt))

              override def failed(error: Throwable, attachment: Void): Unit = callback(ZIO.fail(error))
            }
          )
        }
      }

    /**
     * Close the underlying socket
     */
    def close(): UIO[Unit] = ZIO.effectTotal(socket.close())
  }

  object Connection {

    /**
     * Create a `Managed` connection
     */
    def make(socket: AsynchronousSocketChannel): UManaged[Connection] =
      Managed.make(ZIO.succeed(new Connection(socket)))(_.close())
  }
}

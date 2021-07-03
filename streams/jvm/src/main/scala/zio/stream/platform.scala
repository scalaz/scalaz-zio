/*
 * Copyright 2018-2021 John A. De Goes and the ZIO Contributors
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package zio.stream

import zio._
import zio.stream.compression._

import java.io._
import java.net.{InetSocketAddress, SocketAddress}
import java.nio.channels.{AsynchronousServerSocketChannel, AsynchronousSocketChannel, CompletionHandler, FileChannel}
import java.nio.file.StandardOpenOption._
import java.nio.file.{OpenOption, Path}
import java.nio.{Buffer, ByteBuffer}
import java.util.zip.{DataFormatException, Inflater}
import java.{util => ju}
import scala.annotation.tailrec

trait ZSinkPlatformSpecificConstructors {
  self: ZSink.type =>

  /**
   * Uses the provided `OutputStream` to create a [[ZSink]] that consumes byte chunks
   * and writes them to the `OutputStream`. The sink will yield the count of bytes written.
   *
   * The caller of this function is responsible for closing the `OutputStream`.
   */
  final def fromOutputStream(
    os: OutputStream
  ): ZSink[Any, IOException, Byte, Byte, Long] = fromOutputStreamManaged(ZManaged.succeedNow(os))

  /**
   * Uses the provided `OutputStream` resource to create a [[ZSink]] that consumes byte chunks
   * and writes them to the `OutputStream`. The sink will yield the count of bytes written.
   *
   * The `OutputStream` will be automatically closed after the stream is finished or an error occurred.
   */
  final def fromOutputStreamManaged(
    os: ZManaged[Any, IOException, OutputStream]
  ): ZSink[Any, IOException, Byte, Byte, Long] =
    ZSink.managed(os) { out =>
      ZSink.foldLeftChunksZIO(0L) { (bytesWritten, byteChunk: Chunk[Byte]) =>
        ZIO.attemptBlockingInterrupt {
          val bytes = byteChunk.toArray
          out.write(bytes)
          bytesWritten + bytes.length
        }.refineOrDie { case e: IOException =>
          e
        }
      }
    }

  /**
   * Uses the provided `Path` to create a [[ZSink]] that consumes byte chunks
   * and writes them to the `File`. The sink will yield count of bytes written.
   */
  final def fromFile(
    path: => Path,
    position: Long = 0L,
    options: Set[OpenOption] = Set(WRITE, TRUNCATE_EXISTING, CREATE)
  ): ZSink[Any, Throwable, Byte, Byte, Long] = {
    val managedChannel = ZManaged.acquireReleaseWith(
      ZIO
        .attemptBlockingInterrupt(
          FileChannel
            .open(
              path,
              options.foldLeft(new ju.HashSet[OpenOption]()) { (acc, op) =>
                acc.add(op); acc
              } // for avoiding usage of different Java collection converters for different scala versions
            )
            .position(position)
        )
    )(chan => ZIO.attemptBlocking(chan.close()).orDie)

    val writer: ZSink[Any, Throwable, Byte, Byte, Unit] = ZSink.managed(managedChannel) { chan =>
      ZSink.foreachChunk[Any, Throwable, Byte](byteChunk =>
        ZIO.attemptBlockingInterrupt {
          chan.write(ByteBuffer.wrap(byteChunk.toArray))
        }
      )
    }
    writer &> ZSink.count
  }
}

trait ZStreamPlatformSpecificConstructors {
  self: ZStream.type =>

  /**
   * Creates a stream from an asynchronous callback that can be called multiple times.
   * The optionality of the error type `E` can be used to signal the end of the stream,
   * by setting it to `None`.
   */
  def async[R, E, A](
    register: (ZIO[R, Option[E], Chunk[A]] => Unit) => Unit,
    outputBuffer: Int = 16
  ): ZStream[R, E, A] =
    asyncMaybe(
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
  def asyncInterrupt[R, E, A](
    register: (ZIO[R, Option[E], Chunk[A]] => Unit) => Either[Canceler[R], ZStream[R, E, A]],
    outputBuffer: Int = 16
  ): ZStream[R, E, A] =
    ZStream {
      for {
        output  <- Queue.bounded[stream.Take[E, A]](outputBuffer).toManagedWith(_.shutdown)
        runtime <- ZIO.runtime[R].toManaged
        eitherStream <- ZManaged.succeed {
                          register(k =>
                            try {
                              runtime.unsafeRun(stream.Take.fromPull(k).flatMap(output.offer))
                              ()
                            } catch {
                              case FiberFailure(c) if c.interrupted =>
                            }
                          )
                        }
        pull <- eitherStream match {
                  case Left(canceler) =>
                    (for {
                      done <- ZRef.makeManaged(false)
                    } yield done.get.flatMap {
                      if (_) Pull.end
                      else
                        output.take.flatMap(_.done).onError(_ => done.set(true) *> output.shutdown)
                    }).ensuring(canceler)
                  case Right(stream) => output.shutdown.toManaged *> stream.process
                }
      } yield pull
    }

  /**
   * Creates a stream from an asynchronous callback that can be called multiple times
   * The registration of the callback itself returns an effect. The optionality of the
   * error type `E` can be used to signal the end of the stream, by setting it to `None`.
   */
  def asyncZIO[R, E, A](
    register: (ZIO[R, Option[E], Chunk[A]] => Unit) => ZIO[R, E, Any],
    outputBuffer: Int = 16
  ): ZStream[R, E, A] =
    managed {
      for {
        output  <- Queue.bounded[stream.Take[E, A]](outputBuffer).toManagedWith(_.shutdown)
        runtime <- ZIO.runtime[R].toManaged
        _ <- register { k =>
               try {
                 runtime.unsafeRun(stream.Take.fromPull(k).flatMap(output.offer))
                 ()
               } catch {
                 case FiberFailure(c) if c.interrupted =>
               }
             }.toManaged
        done <- ZRef.makeManaged(false)
        pull = done.get.flatMap {
                 if (_)
                   Pull.end
                 else
                   output.take.flatMap(_.done).onError(_ => done.set(true) *> output.shutdown)
               }
      } yield pull
    }.flatMap(repeatZIOChunkOption(_))

  /**
   * Creates a stream from an asynchronous callback that can be called multiple times.
   * The registration of the callback can possibly return the stream synchronously.
   * The optionality of the error type `E` can be used to signal the end of the stream,
   * by setting it to `None`.
   */
  def asyncMaybe[R, E, A](
    register: (ZIO[R, Option[E], Chunk[A]] => Unit) => Option[ZStream[R, E, A]],
    outputBuffer: Int = 16
  ): ZStream[R, E, A] =
    ZStream {
      for {
        output  <- Queue.bounded[stream.Take[E, A]](outputBuffer).toManagedWith(_.shutdown)
        runtime <- ZIO.runtime[R].toManaged
        maybeStream <- ZManaged.succeed {
                         register { k =>
                           try {
                             runtime.unsafeRun(stream.Take.fromPull(k).flatMap(output.offer))
                             ()
                           } catch {
                             case FiberFailure(c) if c.interrupted =>
                           }
                         }
                       }
        pull <- maybeStream match {
                  case Some(stream) => output.shutdown.toManaged *> stream.process
                  case None =>
                    for {
                      done <- ZRef.makeManaged(false)
                    } yield done.get.flatMap {
                      if (_)
                        Pull.end
                      else
                        output.take.flatMap(_.done).onError(_ => done.set(true) *> output.shutdown)
                    }
                }
      } yield pull
    }

  /**
   * Creates a stream from an asynchronous callback that can be called multiple times.
   * The optionality of the error type `E` can be used to signal the end of the stream,
   * by setting it to `None`.
   */
  @deprecated("use async", "2.0.0")
  def effectAsync[R, E, A](
    register: (ZIO[R, Option[E], Chunk[A]] => Unit) => Unit,
    outputBuffer: Int = 16
  ): ZStream[R, E, A] =
    async(register, outputBuffer)

  /**
   * Creates a stream from an asynchronous callback that can be called multiple times.
   * The registration of the callback returns either a canceler or synchronously returns a stream.
   * The optionality of the error type `E` can be used to signal the end of the stream, by
   * setting it to `None`.
   */
  @deprecated("use asyncInterrupt", "2.0.0")
  def effectAsyncInterrupt[R, E, A](
    register: (ZIO[R, Option[E], Chunk[A]] => Unit) => Either[Canceler[R], ZStream[R, E, A]],
    outputBuffer: Int = 16
  ): ZStream[R, E, A] =
    asyncInterrupt(register, outputBuffer)

  /**
   * Creates a stream from an asynchronous callback that can be called multiple times
   * The registration of the callback itself returns an effect. The optionality of the
   * error type `E` can be used to signal the end of the stream, by setting it to `None`.
   */
  @deprecated("use asyncZIO", "2.0.0")
  def effectAsyncM[R, E, A](
    register: (ZIO[R, Option[E], Chunk[A]] => Unit) => ZIO[R, E, Any],
    outputBuffer: Int = 16
  ): ZStream[R, E, A] =
    asyncZIO(register, outputBuffer)

  /**
   * Creates a stream from an asynchronous callback that can be called multiple times.
   * The registration of the callback can possibly return the stream synchronously.
   * The optionality of the error type `E` can be used to signal the end of the stream,
   * by setting it to `None`.
   */
  @deprecated("use asyncMaybe", "2.0.0")
  def effectAsyncMaybe[R, E, A](
    register: (ZIO[R, Option[E], Chunk[A]] => Unit) => Option[ZStream[R, E, A]],
    outputBuffer: Int = 16
  ): ZStream[R, E, A] =
    asyncMaybe(register, outputBuffer)

  /**
   * Creates a stream from an blocking iterator that may throw exceptions.
   */
  def fromBlockingIterator[A](iterator: => Iterator[A], maxChunkSize: Int = 1): ZStream[Any, Throwable, A] =
    ZStream {
      ZManaged
        .attempt(iterator)
        .fold(
          Pull.fail,
          iterator =>
            ZIO.suspendSucceed {
              if (maxChunkSize <= 1) {
                if (iterator.isEmpty) Pull.end
                else ZIO.attemptBlocking(Chunk.single(iterator.next())).asSomeError
              } else {
                val builder  = ChunkBuilder.make[A](maxChunkSize)
                val blocking = ZIO.attemptBlocking(builder += iterator.next())

                def go(i: Int): ZIO[Any, Throwable, Unit] =
                  ZIO.when(i < maxChunkSize && iterator.hasNext)(blocking *> go(i + 1))

                go(0).asSomeError.flatMap { _ =>
                  val chunk = builder.result()
                  if (chunk.isEmpty) Pull.end else Pull.emit(chunk)
                }
              }
            }
        )
    }

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

  /**
   * Creates a stream of bytes from a file at the specified path.
   */
  def fromFile(path: => Path, chunkSize: Int = ZStream.DefaultChunkSize): ZStream[Any, Throwable, Byte] =
    ZStream
      .acquireReleaseWith(ZIO.attemptBlockingInterrupt(FileChannel.open(path)))(chan =>
        ZIO.attemptBlocking(chan.close()).orDie
      )
      .flatMap { channel =>
        ZStream.fromZIO(UIO(ByteBuffer.allocate(chunkSize))).flatMap { reusableBuffer =>
          ZStream.repeatZIOChunkOption(
            for {
              bytesRead <- ZIO.attemptBlockingInterrupt(channel.read(reusableBuffer)).mapError(Some(_))
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
    ZStream.fromZIO(UIO(is)).flatMap { capturedIs =>
      ZStream.repeatZIOChunkOption {
        for {
          bufArray  <- UIO(Array.ofDim[Byte](chunkSize))
          bytesRead <- ZIO.attemptBlockingIO(capturedIs.read(bufArray)).mapError(Some(_))
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
        ZIO.attemptBlockingIO(getClass.getClassLoader.getResourceAsStream(path.replace('\\', '/'))).flatMap { x =>
          if (x == null)
            ZIO.fail(new FileNotFoundException(s"No such resource: '$path'"))
          else
            ZIO.succeed(x)
        }
      }
    }.flatMap(is => fromInputStream(is, chunkSize = chunkSize))

  /**
   * Creates a stream from a `java.io.InputStream`. Ensures that the input
   * stream is closed after it is exhausted.
   */
  @deprecated("use fromInputStreamZIO", "2.0.0")
  def fromInputStreamEffect[R](
    is: ZIO[R, IOException, InputStream],
    chunkSize: Int = ZStream.DefaultChunkSize
  ): ZStream[R, IOException, Byte] =
    fromInputStreamZIO(is, chunkSize)

  /**
   * Creates a stream from a `java.io.InputStream`. Ensures that the input
   * stream is closed after it is exhausted.
   */
  def fromInputStreamZIO[R](
    is: ZIO[R, IOException, InputStream],
    chunkSize: Int = ZStream.DefaultChunkSize
  ): ZStream[R, IOException, Byte] =
    fromInputStreamManaged(is.toManagedWith(is => ZIO.succeed(is.close())), chunkSize)

  /**
   * Creates a stream from a managed `java.io.InputStream` value.
   */
  def fromInputStreamManaged[R](
    is: ZManaged[R, IOException, InputStream],
    chunkSize: Int = ZStream.DefaultChunkSize
  ): ZStream[R, IOException, Byte] =
    ZStream
      .managed(is)
      .flatMap(fromInputStream(_, chunkSize))

  /**
   * Creates a stream from `java.io.Reader`.
   */
  def fromReader(reader: => Reader, chunkSize: Int = ZStream.DefaultChunkSize): ZStream[Any, IOException, Char] =
    ZStream.fromZIO(UIO(reader)).flatMap { capturedReader =>
      ZStream.repeatZIOChunkOption {
        for {
          bufArray  <- UIO(Array.ofDim[Char](chunkSize))
          bytesRead <- ZIO.attemptBlockingIO(capturedReader.read(bufArray)).mapError(Some(_))
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
  @deprecated("use fromReaderZIO", "2.0.0")
  def fromReaderEffect[R](
    reader: => ZIO[R, IOException, Reader],
    chunkSize: Int = ZStream.DefaultChunkSize
  ): ZStream[R, IOException, Char] =
    fromReaderZIO(reader, chunkSize)

  /**
   * Creates a stream from managed `java.io.Reader`.
   */
  def fromReaderManaged[R](
    reader: => ZManaged[R, IOException, Reader],
    chunkSize: Int = ZStream.DefaultChunkSize
  ): ZStream[R, IOException, Char] =
    ZStream.managed(reader).flatMap(fromReader(_, chunkSize))

  /**
   * Creates a stream from an effect producing `java.io.Reader`.
   */
  def fromReaderZIO[R](
    reader: => ZIO[R, IOException, Reader],
    chunkSize: Int = ZStream.DefaultChunkSize
  ): ZStream[R, IOException, Char] =
    fromReaderManaged(reader.toManagedWith(r => ZIO.succeed(r.close())), chunkSize)

  /**
   * Creates a stream from a callback that writes to `java.io.OutputStream`.
   * Note: the input stream will be closed after the `write` is done.
   */
  def fromOutputStreamWriter(
    write: OutputStream => Unit,
    chunkSize: Int = ZStream.DefaultChunkSize
  ): ZStream[Any, Throwable, Byte] = {
    def from(in: InputStream, out: OutputStream, err: Promise[Throwable, None.type]) = {
      val readIn = fromInputStream(in, chunkSize).ensuring(ZIO.succeed(in.close()))
      val writeOut = ZStream.fromZIO {
        ZIO
          .attemptBlockingInterrupt(write(out))
          .exit
          .tap(exit => err.done(exit.as(None)))
          .ensuring(ZIO.succeed(out.close()))
      }

      val handleError = ZStream.fromZIOOption(err.await.some)
      readIn.drainFork(writeOut) ++ handleError
    }

    for {
      out    <- ZStream.fromZIO(ZIO.succeed(new PipedOutputStream()))
      in     <- ZStream.fromZIO(ZIO.succeed(new PipedInputStream(out)))
      err    <- ZStream.fromZIO(Promise.make[Throwable, None.type])
      result <- from(in, out, err)
    } yield result
  }

  /**
   * Creates a stream from a Java stream
   */
  final def fromJavaStream[R, A](stream: => ju.stream.Stream[A]): ZStream[R, Throwable, A] =
    ZStream.fromJavaIterator(stream.iterator())

  /**
   * Creates a stream from a Java stream
   */
  @deprecated("use fromJavaStreamZIO", "2.0.0")
  final def fromJavaStreamEffect[R, A](stream: ZIO[R, Throwable, ju.stream.Stream[A]]): ZStream[R, Throwable, A] =
    fromJavaStreamZIO(stream)

  /**
   * Creates a stream from a managed Java stream
   */
  final def fromJavaStreamManaged[R, A](stream: ZManaged[R, Throwable, ju.stream.Stream[A]]): ZStream[R, Throwable, A] =
    ZStream.fromJavaIteratorManaged(stream.mapZIO(s => UIO(s.iterator())))

  /**
   * Creates a stream from a Java stream
   */
  final def fromJavaStreamSucceed[A](stream: => ju.stream.Stream[A]): ZStream[Any, Nothing, A] =
    ZStream.fromJavaIteratorSucceed(stream.iterator())

  /**
   * Creates a stream from a Java stream
   */
  @deprecated("use fromJavaStreamSucceed", "2.0.0")
  final def fromJavaStreamTotal[A](stream: => ju.stream.Stream[A]): ZStream[Any, Nothing, A] =
    fromJavaStreamSucceed(stream)

  /**
   * Creates a stream from a Java stream
   */
  final def fromJavaStreamZIO[R, A](stream: ZIO[R, Throwable, ju.stream.Stream[A]]): ZStream[R, Throwable, A] =
    ZStream.fromJavaIteratorZIO(stream.flatMap(s => UIO(s.iterator())))

  /**
   * Create a stream of accepted connection from server socket
   * Emit socket `Connection` from which you can read / write and ensure it is closed after it is used
   */
  def fromSocketServer(
    port: Int,
    host: Option[String] = None
  ): ZStream[Any, Throwable, Connection] =
    for {
      server <- ZStream.managed(ZManaged.fromAutoCloseable(ZIO.attemptBlocking {
                  AsynchronousServerSocketChannel
                    .open()
                    .bind(
                      host.fold(new InetSocketAddress(port))(new InetSocketAddress(_, port))
                    )
                }))

      registerConnection <- ZStream.managed(ZManaged.scope)

      conn <- ZStream.repeatZIO {
                IO.async[Throwable, UManaged[Connection]] { callback =>
                  server.accept(
                    null,
                    new CompletionHandler[AsynchronousSocketChannel, Void]() {
                      self =>
                      override def completed(socket: AsynchronousSocketChannel, attachment: Void): Unit =
                        callback(ZIO.succeed(Connection.make(socket)))

                      override def failed(exc: Throwable, attachment: Void): Unit = callback(ZIO.fail(exc))
                    }
                  )
                }.flatMap(managedConn => registerConnection(managedConn).map(_._2))
              }
    } yield conn

  /**
   * Accepted connection made to a specific channel `AsynchronousServerSocketChannel`
   */
  class Connection(socket: AsynchronousSocketChannel) {

    /**
     * The remote address, i.e. the connected client
     */
    def remoteAddress: IO[IOException, SocketAddress] =
      ZIO.attempt(socket.getRemoteAddress).refineToOrDie[IOException]

    /**
     * The local address, i.e. our server
     */
    def localAddress: IO[IOException, SocketAddress] =
      ZIO.attempt(socket.getLocalAddress).refineToOrDie[IOException]

    /**
     * Read the entire `AsynchronousSocketChannel` by emitting a `Chunk[Byte]`
     * The caller of this function is NOT responsible for closing the `AsynchronousSocketChannel`.
     */
    def read: Stream[Throwable, Byte] =
      ZStream.unfoldChunkZIO(0) {
        case -1 => ZIO.succeed(Option.empty)
        case _ =>
          val buff = ByteBuffer.allocate(ZStream.DefaultChunkSize)

          IO.async[Throwable, Option[(Chunk[Byte], Int)]] { callback =>
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
    def write: Sink[Throwable, Byte, Nothing, Int] =
      ZSink.foldLeftChunksZIO(0) { case (nbBytesWritten, c) =>
        IO.async[Throwable, Int] { callback =>
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
    def close(): UIO[Unit] =
      ZIO.succeed(socket.close())

    /**
     * Close only the write, so the remote end will see EOF
     */
    def closeWrite(): UIO[Unit] =
      ZIO.succeed(socket.shutdownOutput()).unit
  }

  object Connection {

    /**
     * Create a `Managed` connection
     */
    def make(socket: AsynchronousSocketChannel): UManaged[Connection] =
      Managed.acquireReleaseWith(ZIO.succeed(new Connection(socket)))(_.close())
  }

  trait ZStreamConstructorPlatformSpecific extends ZStreamConstructorLowPriority1 {

    /**
     * Constructs a `ZStream[Any, IOException, Byte]` from a
     * `java.io.InputStream`.
     */
    implicit def InputStreamConstructor[InputStreamLike <: InputStream]
      : WithOut[InputStreamLike, ZStream[Any, IOException, Byte]] =
      new ZStreamConstructor[InputStreamLike] {
        type Out = ZStream[Any, IOException, Byte]
        def make(input: => InputStreamLike): ZStream[Any, IOException, Byte] =
          ZStream.fromInputStream(input)
      }

    /**
     * Constructs a `ZStream[Any, IOException, Byte]` from a
     * `ZManaged[R, java.io.IOException, java.io.InputStream]`.
     */
    implicit def InputStreamManagedConstructor[R, E <: IOException, InputStreamLike <: InputStream]
      : WithOut[ZManaged[R, E, InputStreamLike], ZStream[R, IOException, Byte]] =
      new ZStreamConstructor[ZManaged[R, E, InputStreamLike]] {
        type Out = ZStream[R, IOException, Byte]
        def make(input: => ZManaged[R, E, InputStreamLike]): ZStream[R, IOException, Byte] =
          ZStream.fromInputStreamManaged(input)
      }

    /**
     * Constructs a `ZStream[Any, IOException, Byte]` from a
     * `ZIO[R, java.io.IOException, java.io.InputStream]`.
     */
    implicit def InputStreamZIOConstructor[R, E <: IOException, InputStreamLike <: InputStream]
      : WithOut[ZIO[R, E, InputStreamLike], ZStream[R, IOException, Byte]] =
      new ZStreamConstructor[ZIO[R, E, InputStreamLike]] {
        type Out = ZStream[R, IOException, Byte]
        def make(input: => ZIO[R, E, InputStreamLike]): ZStream[R, IOException, Byte] =
          ZStream.fromInputStreamZIO(input)
      }

    /**
     * Constructs a `ZStream[Any, Throwable, A]` from a
     * `java.util.stream.Stream[A]`.
     */
    implicit def JavaStreamConstructor[A, StreamLike[A] <: ju.stream.Stream[A]]
      : WithOut[StreamLike[A], ZStream[Any, Throwable, A]] =
      new ZStreamConstructor[StreamLike[A]] {
        type Out = ZStream[Any, Throwable, A]
        def make(input: => StreamLike[A]): ZStream[Any, Throwable, A] =
          ZStream.fromJavaStream(input)
      }

    /**
     * Constructs a `ZStream[Any, Throwable, A]` from a
     * `ZManaged[R, Throwable, java.util.stream.Stream[A]]`.
     */
    implicit def JavaStreamManagedConstructor[R, E <: Throwable, A, StreamLike[A] <: ju.stream.Stream[A]]
      : WithOut[ZManaged[R, E, StreamLike[A]], ZStream[R, Throwable, A]] =
      new ZStreamConstructor[ZManaged[R, E, StreamLike[A]]] {
        type Out = ZStream[R, Throwable, A]
        def make(input: => ZManaged[R, E, StreamLike[A]]): ZStream[R, Throwable, A] =
          ZStream.fromJavaStreamManaged(input)
      }

    /**
     * Constructs a `ZStream[Any, Throwable, A]` from a
     * `ZIO[R, Throwable, java.util.stream.Stream[A]]`.
     */
    implicit def JavaStreamZIOConstructor[R, E <: Throwable, A, StreamLike[A] <: ju.stream.Stream[A]]
      : WithOut[ZIO[R, E, StreamLike[A]], ZStream[R, Throwable, A]] =
      new ZStreamConstructor[ZIO[R, E, StreamLike[A]]] {
        type Out = ZStream[R, Throwable, A]
        def make(input: => ZIO[R, E, StreamLike[A]]): ZStream[R, Throwable, A] =
          ZStream.fromJavaStreamZIO(input)
      }

    /**
     * Constructs a `ZStream[Any, IOException, Char]` from a `java.io.Reader`.
     */
    implicit def ReaderConstructor[ReaderLike <: Reader]: WithOut[ReaderLike, ZStream[Any, IOException, Char]] =
      new ZStreamConstructor[ReaderLike] {
        type Out = ZStream[Any, IOException, Char]
        def make(input: => ReaderLike): ZStream[Any, IOException, Char] =
          ZStream.fromReader(input)
      }

    /**
     * Constructs a `ZStream[Any, IOException, Char]` from a
     * `ZManaged[R, java.io.IOException, java.io.Reader]`.
     */
    implicit def ReaderManagedConstructor[R, E <: IOException, ReaderLike <: Reader]
      : WithOut[ZManaged[R, E, ReaderLike], ZStream[R, IOException, Char]] =
      new ZStreamConstructor[ZManaged[R, E, ReaderLike]] {
        type Out = ZStream[R, IOException, Char]
        def make(input: => ZManaged[R, E, ReaderLike]): ZStream[R, IOException, Char] =
          ZStream.fromReaderManaged(input)
      }

    /**
     * Constructs a `ZStream[Any, IOException, Char]` from a
     * `ZIO[R, java.io.IOException, java.io.Reader]`.
     */
    implicit def ReaderZIOConstructor[R, E <: IOException, ReaderLike <: Reader]
      : WithOut[ZIO[R, E, ReaderLike], ZStream[R, IOException, Char]] =
      new ZStreamConstructor[ZIO[R, E, ReaderLike]] {
        type Out = ZStream[R, IOException, Char]
        def make(input: => ZIO[R, E, ReaderLike]): ZStream[R, IOException, Char] =
          ZStream.fromReaderZIO(input)
      }
  }
}

trait ZTransducerPlatformSpecificConstructors {
  self: ZTransducer.type =>

  /**
   * Compresses stream with 'deflate' method described in https://tools.ietf.org/html/rfc1951.
   * Each incoming chunk is compressed at once, so it can utilize thread for long time if chunks are big.
   *
   * @param bufferSize Size of internal buffer used for pulling data from deflater, affects performance.
   * @param noWrap     Whether output stream is wrapped in ZLIB header and trailer. For HTTP 'deflate' content-encoding should be false, see https://tools.ietf.org/html/rfc2616.
   */
  def deflate(
    bufferSize: Int = 64 * 1024,
    noWrap: Boolean = false,
    level: CompressionLevel = CompressionLevel.DefaultCompression,
    strategy: CompressionStrategy = CompressionStrategy.DefaultStrategy,
    flushMode: FlushMode = FlushMode.NoFlush
  ): ZTransducer[Any, Nothing, Byte, Byte] =
    ZTransducer(Deflate.makeDeflater(bufferSize, noWrap, level, strategy, flushMode))

  /**
   * Decompresses deflated stream. Compression method is described in https://tools.ietf.org/html/rfc1951.
   *
   * @param noWrap     Whether is wrapped in ZLIB header and trailer, see https://tools.ietf.org/html/rfc1951.
   *                   For HTTP 'deflate' content-encoding should be false, see https://tools.ietf.org/html/rfc2616.
   * @param bufferSize Size of buffer used internally, affects performance.
   */
  def inflate(
    bufferSize: Int = 64 * 1024,
    noWrap: Boolean = false
  ): ZTransducer[Any, CompressionException, Byte, Byte] = {
    def makeInflater: ZManaged[Any, Nothing, Option[Chunk[Byte]] => ZIO[Any, CompressionException, Chunk[Byte]]] =
      ZManaged
        .acquireReleaseWith(ZIO.succeed((new Array[Byte](bufferSize), new Inflater(noWrap)))) { case (_, inflater) =>
          ZIO.succeed(inflater.end())
        }
        .map {
          case (buffer, inflater) => {
            case None =>
              ZIO.attempt {
                if (inflater.finished()) {
                  inflater.reset()
                  Chunk.empty
                } else {
                  throw CompressionException("Inflater is not finished when input stream completed")
                }
              }.refineOrDie { case e: DataFormatException =>
                CompressionException(e)
              }
            case Some(chunk) =>
              ZIO.attempt {
                inflater.setInput(chunk.toArray)
                pullAllOutput(inflater, buffer, chunk)
              }.refineOrDie { case e: DataFormatException =>
                CompressionException(e)
              }
          }
        }

    // Pulls all available output from the inflater.
    def pullAllOutput(
      inflater: Inflater,
      buffer: Array[Byte],
      input: Chunk[Byte]
    ): Chunk[Byte] = {
      @tailrec
      def next(acc: Chunk[Byte]): Chunk[Byte] = {
        val read      = inflater.inflate(buffer)
        val remaining = inflater.getRemaining()
        val current   = Chunk.fromArray(ju.Arrays.copyOf(buffer, read))
        if (remaining > 0) {
          if (read > 0) next(acc ++ current)
          else if (inflater.finished()) {
            val leftover = input.takeRight(remaining)
            inflater.reset()
            inflater.setInput(leftover.toArray)
            next(acc ++ current)
          } else {
            // Impossible happened (aka programmer error). Die.
            throw new Exception("read = 0, remaining > 0, not finished")
          }
        } else if (read > 0) next(acc ++ current)
        else acc ++ current
      }

      if (inflater.needsInput()) Chunk.empty else next(Chunk.empty)
    }

    ZTransducer(makeInflater)
  }

  /**
   * @param bufferSize Size of buffer used internally, affects performance.
   * @param level
   * @param strategy
   * @param flushMode
   * @return
   */
  def gzip(
    bufferSize: Int = 64 * 1024,
    level: CompressionLevel = CompressionLevel.DefaultCompression,
    strategy: CompressionStrategy = CompressionStrategy.DefaultStrategy,
    flushMode: FlushMode = FlushMode.NoFlush
  ): ZTransducer[Any, Nothing, Byte, Byte] =
    ZTransducer(
      ZManaged
        .acquireReleaseWith(Gzipper.make(bufferSize, level, strategy, flushMode))(gzipper =>
          ZIO.succeed(gzipper.close())
        )
        .map { gzipper =>
          {
            case None        => gzipper.onNone
            case Some(chunk) => gzipper.onChunk(chunk)
          }
        }
    )

  /**
   * Decompresses gzipped stream. Compression method is described in https://tools.ietf.org/html/rfc1952.
   *
   * @param bufferSize Size of buffer used internally, affects performance.
   */
  def gunzip(bufferSize: Int = 64 * 1024): ZTransducer[Any, CompressionException, Byte, Byte] =
    ZTransducer(
      ZManaged
        .acquireReleaseWith(Gunzipper.make(bufferSize))(gunzipper => ZIO.succeed(gunzipper.close()))
        .map { gunzipper =>
          {
            case None        => gunzipper.onNone
            case Some(chunk) => gunzipper.onChunk(chunk)
          }
        }
    )
}

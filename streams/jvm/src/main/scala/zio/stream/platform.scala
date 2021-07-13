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
import zio.blocking.{Blocking, effectBlocking, effectBlockingIO}
import zio.stream.compression._

import java.io._
import java.net.{InetSocketAddress, SocketAddress}
import java.nio.channels.{
  AsynchronousServerSocketChannel,
  AsynchronousSocketChannel,
  ClosedChannelException,
  CompletionHandler,
  FileChannel
}
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
  ): ZSink[Blocking, IOException, Byte, Byte, Long] = fromOutputStreamManaged(ZManaged.succeedNow(os))

  /**
   * Uses the provided `OutputStream` resource to create a [[ZSink]] that consumes byte chunks
   * and writes them to the `OutputStream`. The sink will yield the count of bytes written.
   *
   * The `OutputStream` will be automatically closed after the stream is finished or an error occurred.
   */
  final def fromOutputStreamManaged(
    os: ZManaged[Blocking, IOException, OutputStream]
  ): ZSink[Blocking, IOException, Byte, Byte, Long] =
    ZSink.managed(os) { out =>
      ZSink.foldLeftChunksM(0L) { (bytesWritten, byteChunk: Chunk[Byte]) =>
        blocking.effectBlockingInterrupt {
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
  ): ZSink[Blocking, Throwable, Byte, Byte, Long] = {
    val managedChannel = ZManaged.make(
      blocking
        .effectBlockingInterrupt(
          FileChannel
            .open(
              path,
              options.foldLeft(new ju.HashSet[OpenOption]()) { (acc, op) =>
                acc.add(op); acc
              } // for avoiding usage of different Java collection converters for different scala versions
            )
            .position(position)
        )
    )(chan => effectBlocking(chan.close()).orDie)

    val writer: ZSink[Blocking, Throwable, Byte, Byte, Unit] = ZSink.managed(managedChannel) { chan =>
      ZSink.foreachChunk[Blocking, Throwable, Byte](byteChunk =>
        blocking.effectBlockingInterrupt {
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
    ZStream {
      for {
        output  <- Queue.bounded[stream.Take[E, A]](outputBuffer).toManaged(_.shutdown)
        runtime <- ZIO.runtime[R].toManaged_
        eitherStream <- ZManaged.effectTotal {
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
                  case Right(stream) => output.shutdown.toManaged_ *> stream.process
                }
      } yield pull
    }

  /**
   * Creates a stream from an asynchronous callback that can be called multiple times
   * The registration of the callback itself returns an effect. The optionality of the
   * error type `E` can be used to signal the end of the stream, by setting it to `None`.
   */
  def effectAsyncM[R, E, A](
    register: (ZIO[R, Option[E], Chunk[A]] => Unit) => ZIO[R, E, Any],
    outputBuffer: Int = 16
  ): ZStream[R, E, A] =
    managed {
      for {
        output  <- Queue.bounded[stream.Take[E, A]](outputBuffer).toManaged(_.shutdown)
        runtime <- ZIO.runtime[R].toManaged_
        _ <- register { k =>
               try {
                 runtime.unsafeRun(stream.Take.fromPull(k).flatMap(output.offer))
                 ()
               } catch {
                 case FiberFailure(c) if c.interrupted =>
               }
             }.toManaged_
        done <- ZRef.makeManaged(false)
        pull = done.get.flatMap {
                 if (_)
                   Pull.end
                 else
                   output.take.flatMap(_.done).onError(_ => done.set(true) *> output.shutdown)
               }
      } yield pull
    }.flatMap(repeatEffectChunkOption(_))

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
    ZStream {
      for {
        output  <- Queue.bounded[stream.Take[E, A]](outputBuffer).toManaged(_.shutdown)
        runtime <- ZIO.runtime[R].toManaged_
        maybeStream <- ZManaged.effectTotal {
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
                  case Some(stream) => output.shutdown.toManaged_ *> stream.process
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
   * Creates a stream from an blocking iterator that may throw exceptions.
   */
  def fromBlockingIterator[A](iterator: => Iterator[A], maxChunkSize: Int = 1): ZStream[Blocking, Throwable, A] =
    ZStream {
      ZManaged
        .effect(iterator)
        .fold(
          Pull.fail,
          iterator =>
            ZIO.effectSuspendTotal {
              if (maxChunkSize <= 1) {
                if (iterator.isEmpty) Pull.end
                else effectBlocking(Chunk.single(iterator.next())).asSomeError
              } else {
                val builder  = ChunkBuilder.make[A](maxChunkSize)
                val blocking = effectBlocking(builder += iterator.next())

                def go(i: Int): ZIO[Blocking, Throwable, Unit] =
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
  ): ZStream[Blocking, Throwable, A] =
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
  def fromFile(path: => Path, chunkSize: Int = ZStream.DefaultChunkSize): ZStream[Blocking, Throwable, Byte] =
    ZStream
      .bracket(blocking.effectBlockingInterrupt(FileChannel.open(path)))(chan => effectBlocking(chan.close()).orDie)
      .flatMap { channel =>
        ZStream.fromEffect(UIO(ByteBuffer.allocate(chunkSize))).flatMap { reusableBuffer =>
          ZStream.repeatEffectChunkOption(
            for {
              bytesRead <- blocking.effectBlockingInterrupt(channel.read(reusableBuffer)).mapError(Some(_))
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
  ): ZStream[Blocking, IOException, Byte] =
    ZStream.fromEffect(UIO(is)).flatMap { capturedIs =>
      ZStream.repeatEffectChunkOption {
        for {
          bufArray  <- UIO(Array.ofDim[Byte](chunkSize))
          bytesRead <- blocking.effectBlockingIO(capturedIs.read(bufArray)).mapError(Some(_))
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
  ): ZStream[Blocking, IOException, Byte] =
    ZStream.managed {
      ZManaged.fromAutoCloseable {
        effectBlockingIO(getClass.getClassLoader.getResourceAsStream(path.replace('\\', '/'))).flatMap { x =>
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
  def fromInputStreamEffect[R](
    is: ZIO[R, IOException, InputStream],
    chunkSize: Int = ZStream.DefaultChunkSize
  ): ZStream[R with Blocking, IOException, Byte] =
    fromInputStreamManaged(is.toManaged(is => ZIO.effectTotal(is.close())), chunkSize)

  /**
   * Creates a stream from a managed `java.io.InputStream` value.
   */
  def fromInputStreamManaged[R](
    is: ZManaged[R, IOException, InputStream],
    chunkSize: Int = ZStream.DefaultChunkSize
  ): ZStream[R with Blocking, IOException, Byte] =
    ZStream
      .managed(is)
      .flatMap(fromInputStream(_, chunkSize))

  /**
   * Creates a stream from `java.io.Reader`.
   */
  def fromReader(reader: => Reader, chunkSize: Int = ZStream.DefaultChunkSize): ZStream[Blocking, IOException, Char] =
    ZStream.fromEffect(UIO(reader)).flatMap { capturedReader =>
      ZStream.repeatEffectChunkOption {
        for {
          bufArray  <- UIO(Array.ofDim[Char](chunkSize))
          bytesRead <- blocking.effectBlockingIO(capturedReader.read(bufArray)).mapError(Some(_))
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
  ): ZStream[R with Blocking, IOException, Char] =
    fromReaderManaged(reader.toManaged(r => ZIO.effectTotal(r.close())), chunkSize)

  /**
   * Creates a stream from managed `java.io.Reader`.
   */
  def fromReaderManaged[R](
    reader: => ZManaged[R, IOException, Reader],
    chunkSize: Int = ZStream.DefaultChunkSize
  ): ZStream[R with Blocking, IOException, Char] =
    ZStream.managed(reader).flatMap(fromReader(_, chunkSize))

  /**
   * Creates a stream from a callback that writes to `java.io.OutputStream`.
   * Note: the input stream will be closed after the `write` is done.
   */
  def fromOutputStreamWriter(
    write: OutputStream => Unit,
    chunkSize: Int = ZStream.DefaultChunkSize
  ): ZStream[Blocking, Throwable, Byte] = {
    def from(in: InputStream, out: OutputStream, err: Promise[Throwable, None.type]) = {
      val readIn = fromInputStream(in, chunkSize).ensuring(ZIO.effectTotal(in.close()))
      val writeOut = ZStream.fromEffect {
        blocking
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
  final def fromJavaStream[R, A](stream: => ju.stream.Stream[A]): ZStream[R, Throwable, A] =
    ZStream.fromJavaIterator(stream.iterator())

  /**
   * Creates a stream from a Java stream
   */
  final def fromJavaStreamEffect[R, A](stream: ZIO[R, Throwable, ju.stream.Stream[A]]): ZStream[R, Throwable, A] =
    ZStream.fromJavaIteratorEffect(stream.flatMap(s => UIO(s.iterator())))

  /**
   * Creates a stream from a managed Java stream
   */
  final def fromJavaStreamManaged[R, A](stream: ZManaged[R, Throwable, ju.stream.Stream[A]]): ZStream[R, Throwable, A] =
    ZStream.fromJavaIteratorManaged(stream.mapM(s => UIO(s.iterator())))

  /**
   * Creates a stream from a Java stream
   */
  final def fromJavaStreamTotal[A](stream: => ju.stream.Stream[A]): ZStream[Any, Nothing, A] =
    ZStream.fromJavaIteratorTotal(stream.iterator())

  /**
   * Create a stream of accepted connection from server socket
   * Emit socket `Connection` from which you can read / write and ensure it is closed after it is used
   */
  def fromSocketServer(
    port: Int,
    host: Option[String] = None
  ): ZStream[Blocking, Throwable, Connection] =
    for {
      server <- ZStream.managed(ZManaged.fromAutoCloseable(effectBlocking {
                  AsynchronousServerSocketChannel
                    .open()
                    .bind(
                      host.fold(new InetSocketAddress(port))(new InetSocketAddress(_, port))
                    )
                }))

      registerConnection <- ZStream.managed(ZManaged.scope)

      conn <- ZStream.repeatEffect {
                IO.effectAsync[Throwable, UManaged[Connection]] { callback =>
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
    def remoteAddress: IO[IOException, SocketAddress] = IO
      .effect(socket.getRemoteAddress)
      .refineToOrDie[IOException]

    /**
     * The local address, i.e. our server
     */
    def localAddress: IO[IOException, SocketAddress] = IO
      .effect(socket.getLocalAddress)
      .refineToOrDie[IOException]

    /**
     * Read the entire `AsynchronousSocketChannel` by emitting a `Chunk[Byte]`
     * The caller of this function is NOT responsible for closing the `AsynchronousSocketChannel`.
     */
    def read: Stream[Throwable, Byte] =
      ZStream.unfoldChunkM(0) {
        case -1 => ZIO.succeed(Option.empty)
        case _ =>
          val buff = ByteBuffer.allocate(ZStream.DefaultChunkSize)

          IO.effectAsync[Throwable, Option[(Chunk[Byte], Int)]] { callback =>
            socket.read(
              buff,
              null,
              new CompletionHandler[Integer, Void] {
                override def completed(bytesRead: Integer, attachment: Void): Unit = {
                  (buff: Buffer).flip()
                  callback(ZIO.succeed(Option(Chunk.fromByteBuffer(buff) -> bytesRead.toInt)))
                }

                override def failed(error: Throwable, attachment: Void): Unit = error match {
                  case _: ClosedChannelException => callback(ZIO.succeed(Option.empty))
                  case _                         => callback(ZIO.fail(error))
                }
              }
            )
          }
      }

    /**
     * Write the entire Chuck[Byte] to the socket channel.
     * The caller of this function is NOT responsible for closing the `AsynchronousSocketChannel`.
     */
    def write: Sink[Throwable, Byte, Nothing, Int] =
      ZSink.foldLeftChunksM(0) {
        case (nbBytesWritten, c) => {
          val buffer = ByteBuffer.wrap(c.toArray)
          IO.effectAsync[Throwable, Int] { callback =>
            var totalWritten = 0
            socket.write(
              buffer,
              buffer,
              new CompletionHandler[Integer, ByteBuffer] {
                override def completed(result: Integer, attachment: ByteBuffer): Unit = {
                  totalWritten += result
                  if (attachment.hasRemaining)
                    socket.write(attachment, attachment, this)
                  else
                    callback(ZIO.succeed(nbBytesWritten + totalWritten))
                }

                override def failed(error: Throwable, attachment: ByteBuffer): Unit = error match {
                  case _: ClosedChannelException => callback(ZIO.succeed(0))
                  case _                         => callback(ZIO.fail(error))
                }
              }
            )
          }
        }
      }

    /**
     * Close the underlying socket
     */
    def close(): UIO[Unit] = ZIO.effectTotal(socket.close())

    /**
     * Close only the write, so the remote end will see EOF
     */
    def closeWrite(): UIO[Unit] = ZIO.effectTotal(socket.shutdownOutput()).unit
  }

  object Connection {

    /**
     * Create a `Managed` connection
     */
    def make(socket: AsynchronousSocketChannel): UManaged[Connection] =
      Managed.make(ZIO.succeed(new Connection(socket)))(_.close())
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
        .make(ZIO.effectTotal((new Array[Byte](bufferSize), new Inflater(noWrap)))) { case (_, inflater) =>
          ZIO.effectTotal(inflater.end())
        }
        .map {
          case (buffer, inflater) => {
            case None =>
              ZIO.effect {
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
              ZIO.effect {
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
        .make(Gzipper.make(bufferSize, level, strategy, flushMode))(gzipper => ZIO.effectTotal(gzipper.close()))
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
        .make(Gunzipper.make(bufferSize))(gunzipper => ZIO.effectTotal(gunzipper.close()))
        .map { gunzipper =>
          {
            case None        => gunzipper.onNone
            case Some(chunk) => gunzipper.onChunk(chunk)
          }
        }
    )
}

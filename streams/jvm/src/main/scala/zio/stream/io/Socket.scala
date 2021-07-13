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
package zio.stream.io

import zio.blocking.{Blocking, effectBlockingIO}
import zio.stream.ZStream._
import zio.stream.{Sink, Stream, ZStream}
import zio.{IO, _}

import java.net.{InetSocketAddress, SocketAddress}
import java.nio.channels.{AsynchronousSocketChannel, CompletionHandler}

/**
 * This module provides additional TCP Socket functionality that leverages the
 * functionality implemented in JVM specific [[zio.stream.ZStream.fromSocketServer]].
 *
 * The primary purpose is to ensure the underlying NIO Socket does not leak.
 * It is tricky to managed the three components (Stream sourcing butes, Stream sinking bytes and Socket itself)
 * of a TCP Socket using ZIO Managed functionality.
 *
 * The code also provides standard solutions to common usage patterns.
 *
 * The server side patterns all use a function that maps the remote address of the connection
 * to a function that handles the connection. That provides the identity of the client and allows
 * different functionality to be provided for different clients.
 */
object Socket {

  /**
   * Create a Connection from client socket
   */
  final def fromSocketClient(
    port: Int,
    host: String
  ): ZIO[Blocking, Throwable, Connection] =
    for {
      socket <- effectBlockingIO(AsynchronousSocketChannel.open())

      conn <-
        IO.effectAsync[Throwable, Connection] { callback =>
          socket.connect(
            new InetSocketAddress(host, port),
            socket,
            new CompletionHandler[Void, AsynchronousSocketChannel]() {
              self =>
              override def completed(ignored: Void, attachment: AsynchronousSocketChannel): Unit =
                callback(ZIO.succeed(new Connection(attachment)))

              override def failed(exc: Throwable, attachment: AsynchronousSocketChannel): Unit =
                callback(ZIO.fail(exc))
            }
          )
        }

    } yield conn

  // --- Server only ---

  /**
   * The request stream of bytes from the client is effectfully mapped to a  response stream of bytes
   *
   * The mapping is determined by the client remote address
   */
  final def handlerServer(
    f: SocketAddress => Stream[Throwable, Byte] => Stream[Throwable, Byte]
  )(c: Connection): ZIO[Blocking, Throwable, Unit] =
    (for {
      remote <- c.remoteAddress
      _      <- f(remote)(c.read).run(c.write)
    } yield ())
      .ensuring(c.close())

  /**
   * The request stream of bytes from the client is effectfully mapped to a  response stream of bytes
   *
   *  The mapping is determined by the client remote address
   */
  final def handlerServerM(
    f: SocketAddress => Stream[Throwable, Byte] => IO[Throwable, Stream[Throwable, Byte]]
  )(c: Connection): ZIO[Blocking, Throwable, Unit] =
    (for {
      remote <- c.remoteAddress
      end    <- f(remote)(c.read)
      _      <- end.run(c.write)
    } yield ())
      .ensuring(c.close())

  /**
   * Independently connect the request and response streams from/to the client.
   *
   * The stream and sink are determined by the client remote address
   *
   * Socket is closed when both streams complete
   */
  final def bidiServer(
    f: SocketAddress => (
      Stream[Throwable, Byte] => ZIO[Any, Throwable, Unit],
      Sink[Throwable, Byte, Throwable, Int] => ZIO[Any, Throwable, Unit]
    )
  )(c: Connection): ZIO[Any, Throwable, Unit] = for {
    remote    <- c.remoteAddress
    (up, down) = f(remote)
    result    <- bidi(up, down)(c)
  } yield result

  // --- client or server

  /**
   * Independently connect the request and response streams from/to the client.
   *
   * Socket is closed when both streams complete
   */
  final def bidi(
    request: Stream[Throwable, Byte] => ZIO[Any, Throwable, Unit],
    response: Sink[Throwable, Byte, Throwable, Int] => ZIO[Any, Throwable, Unit]
  )(c: Connection): ZIO[Any, Throwable, Unit] = (for {
    forked <- response(c.write).fork
    _      <- request(c.read)
    _      <- forked.await
  } yield ())
    .ensuring(c.close())

  //  -- client only ---

  /**
   * Send request to distant end and return complete response.
   *
   * Corresponds to HTTP 1.0 interaction.
   */
  final def requestChunk(request: Chunk[Byte])(c: Connection): ZIO[Blocking, Throwable, Chunk[Byte]] =
    (for {
      _        <- (ZStream.fromChunk(request).run(c.write) *> c.closeWrite()).fork
      response <- c.read.runCollect
    } yield response).ensuring(c.close())

  /**
   * Send request to distant end and return streamed response
   *
   * Corresponds to Server Sent Event
   */
  final def requestStream(request: Chunk[Byte])(c: Connection): ZIO[Any, Nothing, ZStream[Any, Throwable, Byte]] = for {
    _ <- (ZStream.fromChunk(request).run(c.write) *> c.closeWrite()).fork
  } yield c.read.ensuring(c.close())

}

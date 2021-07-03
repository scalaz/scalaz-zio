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

import java.io.{IOException, InputStream}
import scala.concurrent.Future

trait ZSinkPlatformSpecificConstructors

trait ZStreamPlatformSpecificConstructors { self: ZStream.type =>

  /**
   * Creates a stream from an asynchronous callback that can be called multiple times.
   * The optionality of the error type `E` can be used to signal the end of the stream,
   * by setting it to `None`.
   */
  def async[R, E, A](
    register: (ZIO[R, Option[E], Chunk[A]] => Future[Boolean]) => Unit,
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
    register: (ZIO[R, Option[E], Chunk[A]] => Future[Boolean]) => Either[Canceler[R], ZStream[R, E, A]],
    outputBuffer: Int = 16
  ): ZStream[R, E, A] =
    ZStream {
      for {
        output  <- Queue.bounded[stream.Take[E, A]](outputBuffer).toManagedWith(_.shutdown)
        runtime <- ZIO.runtime[R].toManaged
        eitherStream <- ZManaged.succeed {
                          register(k =>
                            try {
                              runtime.unsafeRunToFuture(stream.Take.fromPull(k).flatMap(output.offer))
                            } catch {
                              case FiberFailure(c) if c.interrupted =>
                                Future.successful(false)
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
    register: (ZIO[R, Option[E], Chunk[A]] => Future[Boolean]) => ZIO[R, E, Any],
    outputBuffer: Int = 16
  ): ZStream[R, E, A] =
    managed {
      for {
        output  <- Queue.bounded[stream.Take[E, A]](outputBuffer).toManagedWith(_.shutdown)
        runtime <- ZIO.runtime[R].toManaged
        _ <- register { k =>
               try {
                 runtime.unsafeRunToFuture(stream.Take.fromPull(k).flatMap(output.offer))
               } catch {
                 case FiberFailure(c) if c.interrupted =>
                   Future.successful(false)
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
    register: (ZIO[R, Option[E], Chunk[A]] => Future[Boolean]) => Option[ZStream[R, E, A]],
    outputBuffer: Int = 16
  ): ZStream[R, E, A] =
    ZStream {
      for {
        output  <- Queue.bounded[stream.Take[E, A]](outputBuffer).toManagedWith(_.shutdown)
        runtime <- ZIO.runtime[R].toManaged
        maybeStream <- ZManaged.succeed {
                         register { k =>
                           try {
                             runtime.unsafeRunToFuture(stream.Take.fromPull(k).flatMap(output.offer))
                           } catch {
                             case FiberFailure(c) if c.interrupted =>
                               Future.successful(false)
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
    register: (ZIO[R, Option[E], Chunk[A]] => Future[Boolean]) => Unit,
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
    register: (ZIO[R, Option[E], Chunk[A]] => Future[Boolean]) => Either[Canceler[R], ZStream[R, E, A]],
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
    register: (ZIO[R, Option[E], Chunk[A]] => Future[Boolean]) => ZIO[R, E, Any],
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
    register: (ZIO[R, Option[E], Chunk[A]] => Future[Boolean]) => Option[ZStream[R, E, A]],
    outputBuffer: Int = 16
  ): ZStream[R, E, A] =
    asyncMaybe(register, outputBuffer)

  /**
   * Creates a stream from a [[java.io.InputStream]]
   */
  def fromInputStream(
    is: => InputStream,
    chunkSize: Int = ZStream.DefaultChunkSize
  ): ZStream[Any, IOException, Byte] =
    ZStream {
      for {
        done       <- Ref.make(false).toManaged
        capturedIs <- Managed.succeed(is)
        pull = {
          def go: ZIO[Any, Option[IOException], Chunk[Byte]] = done.get.flatMap {
            if (_) Pull.end
            else
              for {
                bufArray <- UIO(Array.ofDim[Byte](chunkSize))
                bytesRead <- Task(capturedIs.read(bufArray))
                               .refineToOrDie[IOException]
                               .mapError(Some(_))
                bytes <- if (bytesRead < 0)
                           done.set(true) *> Pull.end
                         else if (bytesRead == 0)
                           go
                         else if (bytesRead < bufArray.length)
                           Pull.emit(Chunk.fromArray(bufArray).take(bytesRead))
                         else
                           Pull.emit(Chunk.fromArray(bufArray))
              } yield bytes
          }

          go
        }
      } yield pull
    }

  /**
   * Creates a stream from a [[java.io.InputStream]]. Ensures that the input
   * stream is closed after it is exhausted.
   */
  @deprecated("use fromInputStreamZIO", "2.0.0")
  def fromInputStreamEffect[R](
    is: ZIO[R, IOException, InputStream],
    chunkSize: Int = ZStream.DefaultChunkSize
  ): ZStream[R, IOException, Byte] =
    fromInputStreamZIO(is, chunkSize)

  /**
   * Creates a stream from a [[java.io.InputStream]]. Ensures that the input
   * stream is closed after it is exhausted.
   */
  def fromInputStreamZIO[R](
    is: ZIO[R, IOException, InputStream],
    chunkSize: Int = ZStream.DefaultChunkSize
  ): ZStream[R, IOException, Byte] =
    fromInputStreamManaged(is.toManagedWith(is => ZIO.succeed(is.close())), chunkSize)

  /**
   * Creates a stream from a managed [[java.io.InputStream]] value.
   */
  def fromInputStreamManaged[R](
    is: ZManaged[R, IOException, InputStream],
    chunkSize: Int = ZStream.DefaultChunkSize
  ): ZStream[R, IOException, Byte] =
    ZStream
      .managed(is)
      .flatMap(fromInputStream(_, chunkSize))

  trait ZStreamConstructorPlatformSpecific extends ZStreamConstructorLowPriority1 {

    /**
     * Constructs a `ZStream[Any, IOException, Byte]` from a
     * `java.io.InputStream`.
     */
    implicit val InputStreamConstructor: WithOut[InputStream, ZStream[Any, IOException, Byte]] =
      new ZStreamConstructor[InputStream] {
        type Out = ZStream[Any, IOException, Byte]
        def make(input: => InputStream): ZStream[Any, IOException, Byte] =
          ZStream.fromInputStream(input)
      }

    /**
     * Constructs a `ZStream[Any, IOException, Byte]` from a
     * `ZManaged[R, java.io.IOException, java.io.InputStream]`.
     */
    implicit def InputStreamManagedConstructor[R, E <: IOException]
      : WithOut[ZManaged[R, E, InputStream], ZStream[R, IOException, Byte]] =
      new ZStreamConstructor[ZManaged[R, E, InputStream]] {
        type Out = ZStream[R, IOException, Byte]
        def make(input: => ZManaged[R, E, InputStream]): ZStream[R, IOException, Byte] =
          ZStream.fromInputStreamManaged(input)
      }

    /**
     * Constructs a `ZStream[Any, IOException, Byte]` from a
     * `ZIO[R, java.io.IOException, java.io.InputStream]`.
     */
    implicit def InputStreamZIOConstructor[R, E <: IOException]
      : WithOut[ZIO[R, E, InputStream], ZStream[R, IOException, Byte]] =
      new ZStreamConstructor[ZIO[R, E, InputStream]] {
        type Out = ZStream[R, IOException, Byte]
        def make(input: => ZIO[R, E, InputStream]): ZStream[R, IOException, Byte] =
          ZStream.fromInputStreamZIO(input)
      }
  }
}
trait StreamPlatformSpecificConstructors

trait ZTransducerPlatformSpecificConstructors

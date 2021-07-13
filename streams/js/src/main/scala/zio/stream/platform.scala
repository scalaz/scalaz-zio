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
  def effectAsync[R, E, A](
    register: (ZIO[R, Option[E], Chunk[A]] => Future[Boolean]) => Unit,
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
    register: (ZIO[R, Option[E], Chunk[A]] => Future[Boolean]) => Either[Canceler[R], ZStream[R, E, A]],
    outputBuffer: Int = 16
  ): ZStream[R, E, A] =
    ZStream {
      for {
        output  <- Queue.bounded[stream.Take[E, A]](outputBuffer).toManaged(_.shutdown)
        runtime <- ZIO.runtime[R].toManaged_
        eitherStream <- ZManaged.effectTotal {
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
                  case Right(stream) => output.shutdown.toManaged_ *> stream.process
                }
      } yield pull
    }

  /**
   * Creates a stream from an asynchronous callback that can be called multiple times.
   * The registration of the callback itself returns an a managed resource.
   * The optionality of the error type `E` can be used to signal the end of the
   * stream, by setting it to `None`.
   */
  def effectAsyncManaged[R, E, A](
    register: (ZIO[R, Option[E], Chunk[A]] => Future[Boolean]) => ZManaged[R, E, Any],
    outputBuffer: Int = 16
  ): ZStream[R, E, A] =
    managed {
      for {
        output  <- Queue.bounded[stream.Take[E, A]](outputBuffer).toManaged(_.shutdown)
        runtime <- ZIO.runtime[R].toManaged_
        _ <- register { k =>
               try {
                 runtime.unsafeRunToFuture(stream.Take.fromPull(k).flatMap(output.offer))
               } catch {
                 case FiberFailure(c) if c.interrupted =>
                   Future.successful(false)
               }
             }
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
   * Creates a stream from an asynchronous callback that can be called multiple times
   * The registration of the callback itself returns an effect. The optionality of the
   * error type `E` can be used to signal the end of the stream, by setting it to `None`.
   */
  def effectAsyncM[R, E, A](
    register: (ZIO[R, Option[E], Chunk[A]] => Future[Boolean]) => ZIO[R, E, Any],
    outputBuffer: Int = 16
  ): ZStream[R, E, A] =
    managed {
      for {
        output  <- Queue.bounded[stream.Take[E, A]](outputBuffer).toManaged(_.shutdown)
        runtime <- ZIO.runtime[R].toManaged_
        _ <- register { k =>
               try {
                 runtime.unsafeRunToFuture(stream.Take.fromPull(k).flatMap(output.offer))
               } catch {
                 case FiberFailure(c) if c.interrupted =>
                   Future.successful(false)
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
    register: (ZIO[R, Option[E], Chunk[A]] => Future[Boolean]) => Option[ZStream[R, E, A]],
    outputBuffer: Int = 16
  ): ZStream[R, E, A] =
    ZStream {
      for {
        output  <- Queue.bounded[stream.Take[E, A]](outputBuffer).toManaged(_.shutdown)
        runtime <- ZIO.runtime[R].toManaged_
        maybeStream <- ZManaged.effectTotal {
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
   * Creates a stream from a `java.io.InputStream`
   */
  def fromInputStream(
    is: => InputStream,
    chunkSize: Int = ZStream.DefaultChunkSize
  ): ZStream[Any, IOException, Byte] =
    ZStream {
      for {
        done       <- Ref.make(false).toManaged_
        capturedIs <- Managed.effectTotal(is)
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
    ZStream
      .managed(is)
      .flatMap(fromInputStream(_, chunkSize))

}
trait StreamPlatformSpecificConstructors

trait ZTransducerPlatformSpecificConstructors

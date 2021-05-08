/*
 * Copyright 2021 John A. De Goes and the ZIO Contributors
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

package zio.test.environment

import zio.duration._
import zio.internal.Scheduler
import zio.{UIO, ZIO}

import java.time.Instant
import java.util.concurrent._
import java.util.concurrent.atomic.AtomicReference
import java.util.{Collections, List}
import scala.annotation.tailrec

trait TestClockPlatformSpecific { self: TestClock.Test =>

  def scheduler: UIO[Scheduler] =
    ZIO.runtime[Any].map { runtime =>
      new Scheduler {
        def schedule(runnable: Runnable, duration: Duration): Scheduler.CancelToken = {
          val canceler =
            runtime.unsafeRunAsyncCancelable((sleep(duration) *> ZIO.effectTotal(runnable.run())))(_ => ())
          () => canceler(zio.Fiber.Id.None).interrupted
        }

        def asScheduledExecutorService: ScheduledExecutorService = {
          val executor = runtime.platform.executor

          def compute[A](a: => A): Either[Throwable, A] =
            try {
              Right(a)
            } catch {
              case t: CancellationException => Left(t)
              case t: InterruptedException  => Left(t)
              case t: OutOfMemoryError      => throw t
              case t: Throwable             => Left(new ExecutionException(t))
            }

          def now(): Instant =
            Instant.ofEpochMilli(clockState.unsafeGet.duration.toMillis)

          def scheduleFuture[S, A](initial: Instant, s: S)(
            f: (Instant, S) => Option[(Instant, S)]
          )(a: => A): ScheduledFuture[A] = {

            sealed trait State

            final case class Done(result: Either[Throwable, A])                        extends State
            final case class Running(cancelToken: Scheduler.CancelToken, end: Instant) extends State
            final case class Scheduling(end: Instant)                                  extends State

            val state = new AtomicReference[State](Scheduling(initial))
            val latch = new CountDownLatch(1)

            def loop(end: Instant, s: S): Unit = {
              val updatedState = state.updateAndGet {
                case Done(result) => Done(result)
                case _            => Scheduling(end)
              }
              if (updatedState == Scheduling(end)) {
                val start    = now()
                val interval = Duration.fromInterval(start, end)
                val cancelToken = schedule(
                  () =>
                    executor.submitOrThrow { () =>
                      compute(a) match {
                        case Left(t) =>
                          state.set(Done(Left(t)))
                          latch.countDown()
                        case Right(a) =>
                          val end = now()
                          f(end, s) match {
                            case Some((interval, s)) =>
                              loop(interval, s)
                            case None =>
                              state.set(Done(Right(a)))
                              latch.countDown()
                          }
                      }
                    },
                  interval
                )
                val currentState = state.getAndUpdate {
                  case Scheduling(end) => Running(cancelToken, end)
                  case state           => state
                }
                if (currentState != Scheduling(end)) {
                  cancelToken()
                  ()
                }
              }
            }

            loop(initial, s)

            new ScheduledFuture[A] { self =>
              def compareTo(that: Delayed): Int =
                if (self eq that) 0
                else self.getDelay(TimeUnit.NANOSECONDS).compareTo(that.getDelay(TimeUnit.NANOSECONDS))
              def getDelay(unit: TimeUnit): Long =
                state.get match {
                  case Done(_) => 0L
                  case Running(_, end) =>
                    unit.convert(Duration.fromInterval(now(), end).toMillis, TimeUnit.MILLISECONDS)
                  case Scheduling(end) =>
                    unit.convert(Duration.fromInterval(now(), end).toMillis, TimeUnit.MILLISECONDS)
                }
              def cancel(mayInterruptIfRunning: Boolean): Boolean = {
                val currentState = state.getAndUpdate {
                  case Done(result) => Done(result)
                  case _            => Done(Left(new InterruptedException))
                }
                currentState match {
                  case Done(_)                 => false
                  case Running(cancelToken, _) => cancelToken()
                  case Scheduling(_)           => true
                }
              }
              def isCancelled(): Boolean =
                state.get match {
                  case Done(Left(_: InterruptedException)) => true
                  case _                                   => false
                }
              def isDone(): Boolean =
                latch.getCount() == 0L
              def get(): A = {
                latch.await()
                state.get match {
                  case Done(result) => result.fold(t => throw t, identity)
                  case _            => throw new Error(s"Defect in zio.internal.Timer#asScheduledExecutorService")
                }
              }
              def get(timeout: Long, unit: TimeUnit): A = {
                latch.await(timeout, unit)
                state.get match {
                  case Done(result) => result.fold(t => throw t, identity)
                  case _            => throw new Error(s"Defect in zio.internal.Timer#asScheduledExecutorService")
                }
              }
            }
          }

          new AbstractExecutorService with ScheduledExecutorService {
            def awaitTermination(timeout: Long, unit: TimeUnit): Boolean =
              false
            def execute(command: Runnable): Unit = {
              executor.submit(command)
              ()
            }
            def isShutdown(): Boolean =
              false
            def isTerminated(): Boolean =
              false
            def schedule(runnable: Runnable, delay: Long, unit: TimeUnit): ScheduledFuture[Any] = {
              val start = now()
              val end   = start.plus(Duration(delay, unit))
              scheduleFuture(end, ())((_, _) => None)(runnable.run())
            }
            def schedule[A](callable: Callable[A], delay: Long, unit: TimeUnit): ScheduledFuture[A] = {
              val end = now().plus(Duration(delay, unit))
              scheduleFuture(end, ())((_, _) => None)(callable.call())
            }
            def scheduleAtFixedRate(
              runnable: Runnable,
              initialDelay: Long,
              period: Long,
              unit: TimeUnit
            ): ScheduledFuture[Any] = {
              val start = now()
              val end   = start.plus(Duration(initialDelay, unit))

              @tailrec
              def loop(now: Instant, start: Instant): Option[(Instant, Instant)] = {
                val end = start.plus(Duration(period, unit))
                if (now.compareTo(end) < 0) Some((start, end))
                else loop(now, end)
              }

              scheduleFuture(end, end.plus(Duration(period, unit)))(loop)(runnable.run())
            }
            def scheduleWithFixedDelay(
              runnable: Runnable,
              initialDelay: Long,
              delay: Long,
              unit: TimeUnit
            ): ScheduledFuture[Any] = {
              val start = now()
              val end   = start.plus(Duration(initialDelay, unit))
              scheduleFuture(end, ())((now, _) => Some((now.plus(Duration(delay, unit)), ())))(runnable.run())
            }
            def shutdown(): Unit =
              ()
            def shutdownNow(): List[Runnable] =
              Collections.emptyList[Runnable]
          }
        }
      }

    }
}

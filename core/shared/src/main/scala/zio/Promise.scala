/*
 * Copyright 2017-2020 John A. De Goes and the ZIO Contributors
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

package zio

import java.util.concurrent.atomic.AtomicReference

import scala.collection.immutable.LongMap

import zio.Promise.internal._

/**
 * A promise represents an asynchronous variable, of [[zio.IO]] type, that can
 * be set exactly once, with the ability for an arbitrary number of fibers to
 * suspend (by calling `await`) and automatically resume when the variable is
 * set.
 *
 * Promises can be used for building primitive actions whose completions
 * require the coordinated action of multiple fibers, and for building
 * higher-level concurrent or asynchronous structures.
 * {{{
 * for {
 *   promise <- Promise.make[Nothing, Int]
 *   _       <- promise.succeed(42).delay(1.second).fork
 *   value   <- promise.await // Resumes when forked fiber completes promise
 * } yield value
 * }}}
 */
final class Promise[E, A] private (private val state: AtomicReference[State[E, A]], blockingOn: List[Fiber.Id])
    extends Serializable {

  /**
   * Retrieves the value of the promise, suspending the fiber running the action
   * until the result is available.
   */
  def await: IO[E, A] =
    IO.effectAsyncInterrupt[E, A](
      k => {
        var result = null.asInstanceOf[Either[Canceler[Any], IO[E, A]]]
        var retry  = true

        while (retry) {
          val oldState = state.get

          val newState = oldState match {
            case Pending(joiners, nextKey) =>
              result = Left(interruptJoiner(nextKey))

              Pending(joiners.updated(nextKey, k), next(nextKey))
            case s @ Done(value) =>
              result = Right(value)

              s
          }

          retry = !state.compareAndSet(oldState, newState)
        }

        result
      },
      blockingOn
    )

  /**
   * Kills the promise with the specified error, which will be propagated to all
   * fibers waiting on the value of the promise.
   */
  def die(e: Throwable): UIO[Boolean] = completeWith(IO.die(e))

  /**
   * Exits the promise with the specified exit, which will be propagated to all
   * fibers waiting on the value of the promise.
   */
  def done(e: Exit[E, A]): UIO[Boolean] = completeWith(IO.done(e))

  /**
   * Completes the promise with the result of the specified effect. If the
   * promise has already been completed, the method will produce false.
   *
   * Note that [[Promise.completeWith]] will be much faster, so consider using
   * that if you do not need to memoize the result of the specified effect.
   */
  def complete(io: IO[E, A]): UIO[Boolean] =
    io.to(this)

  /**
   * Completes the promise with the specified effect. If the promise has
   * already been completed, the method will produce false.
   *
   * Note that since the promise is completed with an effect, the effect will
   * be evaluated each time the value of the promise is retrieved through
   * combinators such as `await`, potentially producing different results if
   * the effect produces different results on subsequent evaluations. In this
   * case te meaning of the "exactly once" guarantee of `Promise` is that the
   * promise can be completed with exactly one effect. For a version that
   * completes the promise with the result of an effect see
   * [[Promise.complete]].
   */
  def completeWith(io: IO[E, A]): UIO[Boolean] =
    IO.effectTotal {
      var action: () => Boolean = null.asInstanceOf[() => Boolean]
      var retry                 = true

      while (retry) {
        val oldState = state.get

        val newState = oldState match {
          case Pending(joiners, _) =>
            action = () => {
              joiners.foreachValue { f =>
                val _ = f(io)
              }; true
            }

            Done(io)

          case Done(_) =>
            action = Promise.ConstFalse

            oldState
        }

        retry = !state.compareAndSet(oldState, newState)
      }

      action()
    }

  /**
   * Fails the promise with the specified error, which will be propagated to all
   * fibers waiting on the value of the promise.
   */
  def fail(e: E): UIO[Boolean] = completeWith(IO.fail(e))

  /**
   * Halts the promise with the specified cause, which will be propagated to all
   * fibers waiting on the value of the promise.
   */
  def halt(e: Cause[E]): UIO[Boolean] = completeWith(IO.halt(e))

  /**
   * Completes the promise with interruption. This will interrupt all fibers
   * waiting on the value of the promise as by the fiber calling this method.
   */
  def interrupt: UIO[Boolean] = ZIO.fiberId.flatMap(id => completeWith(IO.interruptAs(id)))

  /**
   * Completes the promise with interruption. This will interrupt all fibers
   * waiting on the value of the promise as by the specified fiber.
   */
  def interruptAs(fiberId: Fiber.Id): UIO[Boolean] = completeWith(IO.interruptAs(fiberId))

  /**
   * Checks for completion of this Promise. Produces true if this promise has
   * already been completed with a value or an error and false otherwise.
   */
  def isDone: UIO[Boolean] =
    IO.effectTotal(state.get() match {
      case Done(_)       => true
      case Pending(_, _) => false
    })

  /**
   * Checks for completion of this Promise. Returns the result effect if this
   * promise has already been completed or a `None` otherwise.
   */
  def poll: UIO[Option[IO[E, A]]] =
    IO.effectTotal(state.get).flatMap {
      case Pending(_, _) => IO.succeedNow(None)
      case Done(io)      => IO.succeedNow(Some(io))
    }

  /**
   * Completes the promise with the specified value.
   */
  def succeed(a: A): UIO[Boolean] = completeWith(IO.succeedNow(a))

  private def interruptJoiner(key: Long): Canceler[Any] = IO.effectTotal {
    var retry = true

    while (retry) {
      val oldState = state.get

      val newState = oldState match {
        case Pending(joiners, nextKey) =>
          Pending(joiners - key, nextKey)

        case Done(_) =>
          oldState
      }

      retry = !state.compareAndSet(oldState, newState)
    }
  }

  private[zio] def unsafeDone(io: IO[E, A]): Unit = {
    var retry: Boolean                    = true
    var joiners: LongMap[IO[E, A] => Any] = null

    while (retry) {
      val oldState = state.get

      val newState = oldState match {
        case Pending(js, _) =>
          joiners = js
          Done(io)
        case _ => oldState
      }

      retry = !state.compareAndSet(oldState, newState)
    }

    if (joiners ne null) joiners.foreachValue { f =>
      val _ = f(io)
    }
  }

  private def next(l: Long): Long =
    if (l == 0L) throw new RuntimeException("ReleaseMap wrapped around")
    else if (l == Long.MinValue) Long.MaxValue
    else l - 1
}
object Promise {
  private val ConstFalse: () => Boolean = () => false

  private[zio] object internal {
    sealed abstract class State[E, A]                                                extends Serializable with Product
    final case class Pending[E, A](joiners: LongMap[IO[E, A] => Any], nextKey: Long) extends State[E, A]
    final case class Done[E, A](value: IO[E, A])                                     extends State[E, A]
  }

  /**
   * Makes a new promise to be completed by the fiber creating the promise.
   */
  def make[E, A]: UIO[Promise[E, A]] = ZIO.fiberId.flatMap(makeAs(_))

  /**
   * Makes a new promise to be completed by the fiber with the specified id.
   */
  def makeAs[E, A](fiberId: Fiber.Id): UIO[Promise[E, A]] =
    ZIO.effectTotal(unsafeMake(fiberId))

  /**
   * Makes a new managed promise to be completed by the fiber creating the promise.
   */
  def makeManaged[E, A]: UManaged[Promise[E, A]] =
    make[E, A].toManaged_

  private[zio] def unsafeMake[E, A](fiberId: Fiber.Id): Promise[E, A] =
    new Promise[E, A](
      new AtomicReference[State[E, A]](new internal.Pending[E, A](LongMap.empty, initialKey)),
      fiberId :: Nil
    )

  private val initialKey: Long = -1L
}

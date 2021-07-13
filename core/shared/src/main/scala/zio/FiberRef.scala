/*
 * Copyright 2019-2021 John A. De Goes and the ZIO Contributors
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

/**
 * Fiber's counterpart for Java's `ThreadLocal`. Value is automatically propagated
 * to child on fork and merged back in after joining child.
 * {{{
 * for {
 *   fiberRef <- FiberRef.make("Hello world!")
 *   child <- fiberRef.set("Hi!).fork
 *   result <- child.join
 * } yield result
 * }}}
 *
 * `result` will be equal to "Hi!" as changes done by child were merged on join.
 *
 * FiberRef#make also allows specifying how the values will be combined when joining.
 * By default this will use the value of the joined fiber.
 * {{{
 * for {
 *   fiberRef <- FiberRef.make(0, math.max)
 *   child    <- fiberRef.update(_ + 1).fork
 *   _        <- fiberRef.update(_ + 2)
 *   _        <- child.join
 *   value    <- fiberRef.get
 * } yield value
 * }}}
 *
 * `value` will be 2 as the value in the joined fiber is lower and we specified `max` as our combine function.
 *
 * @param initial
 * @tparam A
 */
final class FiberRef[A] private[zio] (
  private[zio] val initial: A,
  private[zio] val fork: A => A,
  private[zio] val join: (A, A) => A,
  private[zio] val link: A => Unit
) extends Serializable { self =>

  /**
   * Reads the value associated with the current fiber. Returns initial value if
   * no value was `set` or inherited from parent.
   */
  val get: UIO[A] = modify(v => (v, v))

  /**
   * Atomically sets the value associated with the current fiber and returns
   * the old value.
   */
  def getAndSet(a: A): UIO[A] =
    modify(v => (v, a))

  /**
   * Atomically modifies the `FiberRef` with the specified function and returns
   * the old value.
   */
  def getAndUpdate(f: A => A): UIO[A] = modify { v =>
    val result = f(v)
    (v, result)
  }

  /**
   * Atomically modifies the `FiberRef` with the specified partial function and
   * returns the old value.
   * If the function is undefined on the current value it doesn't change it.
   */
  def getAndUpdateSome(pf: PartialFunction[A, A]): UIO[A] = modify { v =>
    val result = pf.applyOrElse[A, A](v, identity)
    (v, result)
  }

  /**
   * Returns an `IO` that runs with `value` bound to the current fiber.
   *
   * Guarantees that fiber data is properly restored via `bracket`.
   */
  def locally[R, E, B](value: A)(use: ZIO[R, E, B]): ZIO[R, E, B] =
    for {
      oldValue <- get
      b        <- set(value).bracket_(set(oldValue))(use)
    } yield b

  /**
   * Atomically modifies the `FiberRef` with the specified function, which computes
   * a return value for the modification. This is a more powerful version of
   * `update`.
   */
  def modify[B](f: A => (B, A)): UIO[B] = new ZIO.FiberRefModify(this, f)

  /**
   * Atomically modifies the `FiberRef` with the specified partial function, which computes
   * a return value for the modification if the function is defined in the current value
   * otherwise it returns a default value.
   * This is a more powerful version of `updateSome`.
   */
  def modifySome[B](default: B)(pf: PartialFunction[A, (B, A)]): UIO[B] = modify { v =>
    pf.applyOrElse[A, (B, A)](v, _ => (default, v))
  }

  /**
   * Sets the value associated with the current fiber.
   */
  def set(value: A): UIO[Unit] = modify(_ => ((), value))

  /**
   * Atomically modifies the `FiberRef` with the specified function.
   */
  def update(f: A => A): UIO[Unit] = modify { v =>
    val result = f(v)
    ((), result)
  }

  /**
   * Atomically modifies the `FiberRef` with the specified function and returns
   * the result.
   */
  def updateAndGet(f: A => A): UIO[A] = modify { v =>
    val result = f(v)
    (result, result)
  }

  /**
   * Returns an `IO` that runs with result of calling the specified function
   * bound to the current fiber.
   *
   * Guarantees that fiber data is properly restored via `bracket`.
   */
  def updateLocally[R, E, B](f: A => A)(use: ZIO[R, E, B]): ZIO[R, E, B] =
    for {
      oldValue <- get
      b        <- set(f(oldValue)).bracket_(set(oldValue))(use)
    } yield b

  /**
   * Returns an `IO` that runs with result of calling the specified partial
   * function bound to the current fiber.
   *
   * Guarantees that fiber data is properly restored via `bracket`.
   */
  def updateSomeLocally[R, E, B](pf: PartialFunction[A, A])(use: ZIO[R, E, B]): ZIO[R, E, B] =
    for {
      oldValue <- get
      value     = pf.applyOrElse[A, A](oldValue, identity)
      b        <- set(value).bracket_(set(oldValue))(use)
    } yield b

  /**
   * Atomically modifies the `FiberRef` with the specified partial function.
   * If the function is undefined on the current value it doesn't change it.
   */
  def updateSome(pf: PartialFunction[A, A]): UIO[Unit] = modify { v =>
    val result = pf.applyOrElse[A, A](v, identity)
    ((), result)
  }

  /**
   * Atomically modifies the `FiberRef` with the specified partial function.
   * If the function is undefined on the current value it returns the old value
   * without changing it.
   */
  def updateSomeAndGet(pf: PartialFunction[A, A]): UIO[A] = modify { v =>
    val result = pf.applyOrElse[A, A](v, identity)
    (result, result)
  }

  /**
   * Returns a `ThreadLocal` that can be used to interact with this `FiberRef` from side effecting code.
   *
   * This feature is meant to be used for integration with side effecting code, that needs to access fiber specific data,
   * like MDC contexts and the like. The returned `ThreadLocal` will be backed by this `FiberRef` on all threads that are
   * currently managed by ZIO, and behave like an ordinary `ThreadLocal` on all other threads.
   */
  def unsafeAsThreadLocal: UIO[ThreadLocal[A]] =
    ZIO.effectTotal {
      new ThreadLocal[A] {
        override def get(): A = {
          val fiberContext = Fiber._currentFiber.get()

          Option {
            if (fiberContext eq null) null
            else fiberContext.fiberRefLocals.get(self)
          }.map(_.asInstanceOf[A]).getOrElse(super.get())
        }

        override def set(a: A): Unit = {
          val fiberContext = Fiber._currentFiber.get()
          val fiberRef     = self.asInstanceOf[FiberRef[Any]]

          if (fiberContext eq null) super.set(a)
          else fiberContext.fiberRefLocals.put(fiberRef, a)

          ()
        }

        override def remove(): Unit = {
          val fiberContext = Fiber._currentFiber.get()
          val fiberRef     = self.asInstanceOf[FiberRef[Any]]

          if (fiberContext eq null) super.remove()
          else {
            fiberContext.fiberRefLocals.remove(fiberRef)
            ()
          }
        }

        override def initialValue(): A = initial
      }
    }
}

object FiberRef extends Serializable {

  /**
   * Creates a new `FiberRef` with given initial value.
   */
  def make[A](
    initial: A,
    fork: A => A = (a: A) => a,
    join: (A, A) => A = ((_: A, a: A) => a),
    link: A => Unit = (_: A) => ()
  ): UIO[FiberRef[A]] =
    new ZIO.FiberRefNew(initial, fork, join, link)
}

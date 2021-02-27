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

package zio.internal.tracing

import zio.ZIO

/**
 * Marks a ZIO utility function that wraps a user-supplied lambda.
 *
 * Allows access to the underlying lambda so that stacktraces can
 * point to supplied user's code, instead of ZIO internals.
 *
 * NOTE: it being an `abstract class`, not trait is important as type casing
 * on class appears to be about 10 times faster in [[zio.internal.FiberContext.unwrap]]
 * hot-spot.
 */
private[zio] abstract class ZIOFn extends Serializable {
  def underlying: AnyRef
}

private[zio] abstract class ZIOFn1[-A, +B] extends ZIOFn with Function[A, B] {
  def underlying: AnyRef
}

private[zio] abstract class ZIOFn2[-A, -B, +C] extends ZIOFn with Function2[A, B, C] {
  def underlying: AnyRef
}

private[zio] object ZIOFn {
  def apply[A, B](traceAs: AnyRef)(real: A => B): ZIOFn1[A, B] = new ZIOFn1[A, B] {
    final val underlying: AnyRef = traceAs
    def apply(a: A): B           = real(a)
  }

  /**
   * Adds the specified lambda to the execution trace before `zio` is evaluated
   */
  @noinline
  def recordTrace[R, E, A](lambda: AnyRef)(zio: ZIO[R, E, A]): ZIO[R, E, A] =
    ZIO.unit.flatMap(ZIOFn(lambda)(_ => zio))

  /**
   * Adds the specified lambda to the stack trace during the evaluation of `zio`
   */
  @noinline
  def recordStackTrace[R, E, A](lambda: AnyRef)(zio: ZIO[R, E, A]): ZIO[R, E, A] =
    zio.map(ZIOFn(lambda)(ZIO.identityFn))

  //todo check no performance implications for moving this to object?
  @noinline
  final def unwrap(lambda: AnyRef): AnyRef =
    // This is a huge hot spot, hiding loop under
    // the match allows a faster happy path
    lambda match {
      case fn: ZIOFn =>
        var unwrapped = fn.underlying
        while (unwrapped.isInstanceOf[ZIOFn]) {
          unwrapped = unwrapped.asInstanceOf[ZIOFn].underlying
        }
        unwrapped
      case _ => lambda
    }
}

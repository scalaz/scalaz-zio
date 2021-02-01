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

import zio.internal.macros.{DummyK, ZLayerFromAutoMacros}

private[zio] final class FromLayerAutoPartiallyApplied[R <: Has[_]](val dummy: Boolean = true) extends AnyVal {
  def apply[E](layers: ZLayer[_, E, _]*)(implicit dummyK: DummyK[R]): ZLayer[Any, E, R] =
    macro ZLayerFromAutoMacros.fromAutoImpl[E, R]
}

private[zio] final class FromLayerAutoDebugPartiallyApplied[R <: Has[_]](val dummy: Boolean = true) extends AnyVal {
  def apply[E](layers: ZLayer[_, E, _]*)(implicit dummyK: DummyK[R]): ZLayer[Any, E, R] =
    macro ZLayerFromAutoMacros.fromAutoDebugImpl[E, R]
}

private[zio] trait ZLayerCompanionVersionSpecific {

  /**
   * Automatically assembles a layer for the provided type.
   * The type of the target layer[s] must be provided.
   *
   * {{{
   * ZLayer.fromAuto[A with B](A.live, B.live)
   * }}}
   */
  def fromAuto[R <: Has[_]] =
    new FromLayerAutoPartiallyApplied[R]

  /**
   * Generates a visualization of the automatically assembled
   * final ZLayer. The type of the target layer[s] must be provided.
   *
   * {{{
   * ZLayer.fromAutoDebug[App](App.live, UserService.live, ...)
   *
   * >                            App.live
   * >                        ┌──────┴──────────────────────┐
   * >                UserService.live                Console.live
   * >        ┌──────────────┴┬──────────────┐
   * >  UserRepo.live  Analytics.live  Console.live
   * >        │
   * >  Console.live
   * >  }}}
   */
  def fromAutoDebug[R <: Has[_]] =
    new FromLayerAutoDebugPartiallyApplied[R]

}
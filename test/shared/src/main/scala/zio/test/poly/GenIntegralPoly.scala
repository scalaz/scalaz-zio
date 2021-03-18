/*
 * Copyright 2020-2021 John A. De Goes and the ZIO Contributors
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

package zio.test.poly

import zio.test.{Gen, Sized}
import zio.{Has, Random}

/**
 * `GenIntegralPoly` provides evidence that instances of `Gen[T]` and
 * `Integral[T]` exist for some concrete but unknown type `T`.
 */
trait GenIntegralPoly extends GenNumericPoly {
  override val numT: Integral[T]
}

object GenIntegralPoly {

  /**
   * Constructs an instance of `GenIntegralPoly` using the specified `Gen`
   * and `Integral` instances, existentially hiding the underlying type.
   */
  def apply[A](gen: Gen[Has[Random] with Has[Sized], A], num: Integral[A]): GenIntegralPoly =
    new GenIntegralPoly {
      type T = A
      val genT = gen
      val numT = num
    }

  /**
   * Provides evidence that instances of `Gen` and `Integral` exist for bytes.
   */
  val byte: GenIntegralPoly =
    GenIntegralPoly(Gen.anyByte, Numeric.ByteIsIntegral)

  /**
   * Provides evidence that instances of `Gen` and `Integral` exist for
   * characters.
   */
  val char: GenIntegralPoly =
    GenIntegralPoly(Gen.anyChar, Numeric.CharIsIntegral)

  /**
   * A generator of polymorphic values constrainted to have an `Integral`
   * instance.
   */
  lazy val genIntegralPoly: Gen[Has[Random], GenIntegralPoly] =
    Gen.elements(byte, char, int, long, short)

  /**
   * Provides evidence that instances of `Gen` and `Integral` exist for
   * integers.
   */
  val int: GenIntegralPoly =
    GenIntegralPoly(Gen.anyInt, Numeric.IntIsIntegral)

  /**
   * Provides evidence that instances of `Gen` and `Integral` exist for longs.
   */
  val long: GenIntegralPoly =
    GenIntegralPoly(Gen.anyLong, Numeric.LongIsIntegral)

  /**
   * Provides evidence that instances of `Gen` and `Integral` exist for shorts.
   */
  val short: GenIntegralPoly =
    GenIntegralPoly(Gen.anyShort, Numeric.ShortIsIntegral)
}

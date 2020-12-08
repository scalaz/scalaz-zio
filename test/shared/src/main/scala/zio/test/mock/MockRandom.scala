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

package zio.test.mock

import java.util.UUID

import zio.random.Random
import zio.{ Chunk, Has, UIO, URLayer, ZLayer }

object MockRandom extends Mock[Random] {

  object NextBoolean       extends Effect[Unit, Nothing, Boolean]
  object NextBytes         extends Effect[Int, Nothing, Chunk[Byte]]
  object NextDouble        extends Effect[Unit, Nothing, Double]
  object NextDoubleBetween extends Effect[(Double, Double), Nothing, Double]
  object NextFloat         extends Effect[Unit, Nothing, Float]
  object NextFloatBetween  extends Effect[(Float, Float), Nothing, Float]
  object NextGaussian      extends Effect[Unit, Nothing, Double]
  object NextInt           extends Effect[Unit, Nothing, Int]
  object NextIntBetween    extends Effect[(Int, Int), Nothing, Int]
  object NextIntBounded    extends Effect[Int, Nothing, Int]
  object NextLong          extends Effect[Unit, Nothing, Long]
  object NextLongBetween   extends Effect[(Long, Long), Nothing, Long]
  object NextLongBounded   extends Effect[Long, Nothing, Long]
  object NextPrintableChar extends Effect[Unit, Nothing, Char]
  object NextString        extends Effect[Int, Nothing, String]
  object NextUuid          extends Effect[Unit, Nothing, UUID]
  object SetSeed           extends Effect[Long, Nothing, Unit]
  object Shuffle           extends Effect[Iterable[Any], Nothing, Iterable[Any]]

  val compose: URLayer[Has[Proxy], Random] =
    ZLayer.fromService(proxy =>
      new Random.Service {
        val nextBoolean: UIO[Boolean]                = proxy(NextBoolean)
        def nextBytes(length: Int): UIO[Chunk[Byte]] = proxy(NextBytes, length)
        val nextDouble: UIO[Double]                  = proxy(NextDouble)
        def nextDoubleBetween(minInclusive: Double, maxExclusive: Double): UIO[Double] =
          proxy(NextDoubleBetween, minInclusive, maxExclusive)
        val nextFloat: UIO[Float] = proxy(NextFloat)
        def nextFloatBetween(minInclusive: Float, maxExclusive: Float): UIO[Float] =
          proxy(NextFloatBetween, minInclusive, maxExclusive)
        val nextGaussian: UIO[Double] = proxy(NextGaussian)
        val nextInt: UIO[Int]         = proxy(NextInt)
        def nextIntBetween(minInclusive: Int, maxExclusive: Int): UIO[Int] =
          proxy(NextIntBetween, minInclusive, maxExclusive)
        def nextIntBounded(n: Int): UIO[Int] = proxy(NextIntBounded, n)
        val nextLong: UIO[Long]              = proxy(NextLong)
        def nextLongBetween(minInclusive: Long, maxExclusive: Long): UIO[Long] =
          proxy(NextLongBetween, minInclusive, maxExclusive)
        def nextLongBounded(n: Long): UIO[Long]  = proxy(NextLongBounded, n)
        val nextPrintableChar: UIO[Char]         = proxy(NextPrintableChar)
        def nextString(length: Int): UIO[String] = proxy(NextString, length)
        def nextUuid: UIO[UUID]                  = proxy(NextUuid)
        def setSeed(seed: Long): UIO[Unit]       = proxy(SetSeed, seed)
        def shuffle[A, Collection[+Element] <: Iterable[Element]](
          collection: Collection[A]
        )(implicit bf: BuildFrom[Collection[A], A, Collection[A]]): UIO[Collection[A]] =
          proxy(Shuffle, collection).asInstanceOf[UIO[Collection[A]]]
      }
    )
}

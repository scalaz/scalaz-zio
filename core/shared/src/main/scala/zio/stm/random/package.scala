package zio.stm

import java.util.UUID

import zio._

package object random {
  type TRandom = Has[TRandom.Service]

  object TRandom extends Serializable {

    trait Service {
      def nextBoolean: STM[Nothing, Boolean]
      def nextBytes(length: Int): STM[Nothing, Chunk[Byte]]
      def nextDouble: STM[Nothing, Double]
      def nextDoubleBetween(minInclusive: Double, maxExclusive: Double): STM[Nothing, Double]
      def nextFloat: STM[Nothing, Float]
      def nextFloatBetween(minInclusive: Float, maxExclusive: Float): STM[Nothing, Float]
      def nextGaussian: STM[Nothing, Double]
      def nextInt: STM[Nothing, Int]
      def nextIntBetween(minInclusive: Int, maxExclusive: Int): STM[Nothing, Int]
      def nextIntBounded(n: Int): STM[Nothing, Int]
      def nextLong: STM[Nothing, Long]
      def nextLongBetween(minInclusive: Long, maxExclusive: Long): STM[Nothing, Long]
      def nextLongBounded(n: Long): STM[Nothing, Long]
      def nextPrintableChar: STM[Nothing, Char]
      def nextString(length: Int): STM[Nothing, String]
      def nextUuid: STM[Nothing, UUID]
      def setSeed(seed: Long): STM[Nothing, Unit]
      def shuffle[A, Collection[+Element] <: Iterable[Element]](collection: Collection[A])(implicit
        bf: BuildFrom[Collection[A], A, Collection[A]]
      ): STM[Nothing, Collection[A]]
    }

    val any: ZLayer[TRandom, Nothing, TRandom] =
      ZLayer.requires[TRandom]

    val live: ZLayer[zio.random.Random, Nothing, TRandom] =
      ZLayer.fromEffect {
        import PureRandom._
        zio.random.nextLong.flatMap { init =>
          TRef
            .make(init)
            .map { seed =>
              def withSeed[A](f: Seed => (A, Seed)): STM[Nothing, A] =
                seed.modify(f)

              new Service {
                def nextBoolean: USTM[Boolean] =
                  withSeed(rndBoolean)

                def nextBytes(length: Int): USTM[Chunk[Byte]] =
                  withSeed(rndBytes(_, length))

                def nextDouble: USTM[Double] =
                  withSeed(rndDouble)

                def nextDoubleBetween(minInclusive: Double, maxExclusive: Double): USTM[Double] =
                  nextDoubleBetweenWith(minInclusive, maxExclusive)(nextDouble)

                def nextFloat: USTM[Float] =
                  withSeed(rndFloat)

                def nextFloatBetween(minInclusive: Float, maxExclusive: Float): USTM[Float] =
                  nextFloatBetweenWith(minInclusive, maxExclusive)(nextFloat)

                def nextGaussian: USTM[Double] =
                  withSeed(rndGaussian)

                def nextInt: USTM[Int] =
                  withSeed(rndInt)

                def nextIntBetween(minInclusive: Int, maxExclusive: Int): USTM[Int] =
                  nextIntBetweenWith(minInclusive, maxExclusive)(nextInt, nextIntBounded)

                def nextIntBounded(n: Int): USTM[Int] =
                  withSeed(rndInt(_, n))

                def nextLong: USTM[Long] =
                  withSeed(rndLong)

                def nextLongBetween(minInclusive: Long, maxExclusive: Long): USTM[Long] =
                  nextLongBetweenWith(minInclusive, maxExclusive)(nextLong, nextLongBounded)

                def nextLongBounded(n: Long): USTM[Long] =
                  nextLongBoundedWith(n)(nextLong)

                def nextPrintableChar: USTM[Char] =
                  withSeed(rndPrintableChar)

                def nextString(length: Int): USTM[String] =
                  withSeed(rndString(_, length))

                def nextUuid: USTM[UUID] =
                  withSeed(rndUuid)

                def setSeed(newSeed: Long): USTM[Unit] =
                  seed.set(newSeed)

                def shuffle[A, Collection[+Element] <: Iterable[Element]](collection: Collection[A])(implicit
                  bf: BuildFrom[Collection[A], A, Collection[A]]
                ): USTM[Collection[A]] =
                  shuffleWith(nextIntBounded(_), collection)
              }
            }
            .commit
        }
      }

    private[zio] def nextDoubleBetweenWith(minInclusive: Double, maxExclusive: Double)(
      nextDouble: STM[Nothing, Double]
    ): USTM[Double] =
      if (minInclusive >= maxExclusive)
        STM.die(new IllegalArgumentException("invalid bounds"))
      else
        nextDouble.map { n =>
          val result = n * (maxExclusive - minInclusive) + minInclusive
          if (result < maxExclusive) result
          else Math.nextAfter(maxExclusive, Float.NegativeInfinity)
        }

    private[zio] def nextFloatBetweenWith(minInclusive: Float, maxExclusive: Float)(
      nextFloat: STM[Nothing, Float]
    ): USTM[Float] =
      if (minInclusive >= maxExclusive)
        STM.die(new IllegalArgumentException("invalid bounds"))
      else
        nextFloat.map { n =>
          val result = n * (maxExclusive - minInclusive) + minInclusive
          if (result < maxExclusive) result
          else Math.nextAfter(maxExclusive, Float.NegativeInfinity)
        }

    private[zio] def nextIntBetweenWith(
      minInclusive: Int,
      maxExclusive: Int
    )(nextInt: USTM[Int], nextIntBounded: Int => USTM[Int]): USTM[Int] =
      if (minInclusive >= maxExclusive) {
        STM.die(new IllegalArgumentException("invalid bounds"))
      } else {
        val difference = maxExclusive - minInclusive
        if (difference > 0) nextIntBounded(difference).map(_ + minInclusive)
        else nextInt.repeatUntil(n => minInclusive <= n && n < maxExclusive)
      }

    private[zio] def nextLongBetweenWith(
      minInclusive: Long,
      maxExclusive: Long
    )(nextLong: USTM[Long], nextLongBounded: Long => USTM[Long]): USTM[Long] =
      if (minInclusive >= maxExclusive)
        STM.die(new IllegalArgumentException("invalid bounds"))
      else {
        val difference = maxExclusive - minInclusive
        if (difference > 0) nextLongBounded(difference).map(_ + minInclusive)
        else nextLong.repeatUntil(n => minInclusive <= n && n < maxExclusive)
      }

    private[zio] def nextLongBoundedWith(n: Long)(nextLong: USTM[Long]): USTM[Long] =
      if (n <= 0)
        STM.die(new IllegalArgumentException("n must be positive"))
      else {
        nextLong.flatMap { r =>
          val m = n - 1
          if ((n & m) == 0L)
            STM.succeedNow(r & m)
          else {
            def loop(u: Long): USTM[Long] =
              if (u + m - u % m < 0L) nextLong.flatMap(r => loop(r >>> 1))
              else STM.succeedNow(u % n)
            loop(r >>> 1)
          }
        }
      }

    private[zio] def shuffleWith[A, Collection[+Element] <: Iterable[Element]](
      nextIntBounded: Int => USTM[Int],
      collection: Collection[A]
    )(implicit bf: BuildFrom[Collection[A], A, Collection[A]]): USTM[Collection[A]] =
      for {
        buffer <- TArray.fromIterable(collection)
        swap = (i1: Int, i2: Int) =>
                 for {
                   tmp <- buffer(i1)
                   _   <- buffer.updateM(i1, _ => buffer(i2))
                   _   <- buffer.update(i2, _ => tmp)
                 } yield ()
        _ <-
          STM.foreach_((collection.size to 2 by -1).toList)((n: Int) => nextIntBounded(n).flatMap(k => swap(n - 1, k)))
        result <- buffer.toChunk
      } yield bf.fromSpecific(collection)(result)

    private[zio] object PureRandom {
      import scala.util.{ Random => SRandom }

      type Seed = Long

      def rndBoolean(seed: Seed): (Boolean, Seed) = {
        val rng      = new SRandom(seed)
        val boolean  = rng.nextBoolean()
        val nextSeed = rng.nextLong()
        (boolean, nextSeed)
      }

      def rndBytes(seed: Seed, length: Int): (Chunk[Byte], Seed) = {
        val rng   = new SRandom(seed)
        val array = Array.ofDim[Byte](length)
        SRandom.nextBytes(array)
        val nextSeed = rng.nextLong()
        (Chunk.fromArray(array), nextSeed)
      }

      def rndDouble(seed: Seed): (Double, Seed) = {
        val rng      = new SRandom(seed)
        val double   = rng.nextDouble()
        val nextSeed = rng.nextLong()
        (double, nextSeed)
      }

      def rndFloat(seed: Seed): (Float, Seed) = {
        val rng      = new SRandom(seed)
        val float    = rng.nextFloat()
        val nextSeed = rng.nextLong()
        (float, nextSeed)
      }

      def rndGaussian(seed: Seed): (Double, Seed) = {
        val rng      = new SRandom(seed)
        val gaussian = rng.nextGaussian()
        val nextSeed = rng.nextLong()
        (gaussian, nextSeed)
      }

      def rndInt(seed: Seed): (Int, Seed) = {
        val rng      = new SRandom(seed)
        val int      = rng.nextInt()
        val nextSeed = rng.nextLong()
        (int, nextSeed)
      }

      def rndInt(seed: Seed, n: Int): (Int, Seed) = {
        val rng      = new SRandom(seed)
        val int      = rng.nextInt(n)
        val nextSeed = rng.nextLong()
        (int, nextSeed)
      }

      def rndLong(seed: Seed): (Long, Seed) = {
        val rng      = new SRandom(seed)
        val long     = rng.nextLong()
        val nextSeed = rng.nextLong()
        (long, nextSeed)
      }

      def rndPrintableChar(seed: Seed): (Char, Seed) = {
        val rng      = new SRandom(seed)
        val char     = rng.nextPrintableChar()
        val nextSeed = rng.nextLong()
        (char, nextSeed)
      }

      def rndString(seed: Seed, length: Int): (String, Seed) = {
        val rng      = new SRandom(seed)
        val str      = rng.nextString(length)
        val nextSeed = rng.nextLong()
        (str, nextSeed)
      }

      def rndUuid(seed: Seed): (UUID, Seed) = {
        val rng      = new SRandom(seed)
        val msb      = rng.nextLong()
        val lsb      = rng.nextLong()
        val nextSeed = rng.nextLong()
        (new UUID(msb, lsb), nextSeed)
      }
    }
  }

  /**
   * Generates a pseudo-random boolean inside a transaction.
   */
  val nextBoolean: URSTM[TRandom, Boolean] =
    ZSTM.accessM(_.get.nextBoolean)

  /**
   * Generates a pseudo-random chunk of bytes of the specified length inside a transaction.
   */
  def nextBytes(length: => Int): URSTM[TRandom, Chunk[Byte]] =
    ZSTM.accessM(_.get.nextBytes(length))

  /**
   * Generates a pseudo-random, uniformly distributed double between 0.0 and
   * 1.0 inside a transaction.
   */
  val nextDouble: URSTM[TRandom, Double] = ZSTM.accessM(_.get.nextDouble)

  /**
   * Generates a pseudo-random double in the specified range inside a transaction.
   */
  def nextDoubleBetween(minInclusive: Double, maxExclusive: Double): URSTM[TRandom, Double] =
    ZSTM.accessM(_.get.nextDoubleBetween(minInclusive, maxExclusive))

  /**
   * Generates a pseudo-random, uniformly distributed float between 0.0 and
   * 1.0 inside a transaction.
   */
  val nextFloat: URSTM[TRandom, Float] =
    ZSTM.accessM(_.get.nextFloat)

  /**
   * Generates a pseudo-random float in the specified range inside a transaction.
   */
  def nextFloatBetween(minInclusive: Float, maxExclusive: Float): URSTM[TRandom, Float] =
    ZSTM.accessM(_.get.nextFloatBetween(minInclusive, maxExclusive))

  /**
   * Generates a pseudo-random double from a normal distribution with mean 0.0
   * and standard deviation 1.0 inside a transaction.
   */
  val nextGaussian: URSTM[TRandom, Double] =
    ZSTM.accessM(_.get.nextGaussian)

  /**
   * Generates a pseudo-random integer inside a transaction.
   */
  val nextInt: URSTM[TRandom, Int] =
    ZSTM.accessM(_.get.nextInt)

  /**
   * Generates a pseudo-random integer in the specified range inside a transaction.
   */
  def nextIntBetween(minInclusive: Int, maxExclusive: Int): URSTM[TRandom, Int] =
    ZSTM.accessM(_.get.nextIntBetween(minInclusive, maxExclusive))

  /**
   * Generates a pseudo-random integer between 0 (inclusive) and the specified
   * value (exclusive) inside a transaction.
   */
  def nextIntBounded(n: => Int): URSTM[TRandom, Int] =
    ZSTM.accessM(_.get.nextIntBounded(n))

  /**
   * Generates a pseudo-random long inside a transaction.
   */
  val nextLong: URSTM[TRandom, Long] =
    ZSTM.accessM(_.get.nextLong)

  /**
   * Generates a pseudo-random long in the specified range inside a transaction.
   */
  def nextLongBetween(minInclusive: Long, maxExclusive: Long): URSTM[TRandom, Long] =
    ZSTM.accessM(_.get.nextLongBetween(minInclusive, maxExclusive))

  /**
   * Generates a pseudo-random long between 0 (inclusive) and the specified
   * value (exclusive) inside a transaction.
   */
  def nextLongBounded(n: => Long): URSTM[TRandom, Long] =
    ZSTM.accessM(_.get.nextLongBounded(n))

  /**
   * Generates a pseudo-random character from the ASCII range 33-126 inside a transaction.
   */
  val nextPrintableChar: URSTM[TRandom, Char] =
    ZSTM.accessM(_.get.nextPrintableChar)

  /**
   * Generates a pseudo-random string of the specified length inside a transaction.
   */
  def nextString(length: => Int): URSTM[TRandom, String] =
    ZSTM.accessM(_.get.nextString(length))

  /**
   * Generates a pseudo-random UUID inside a transcation.
   */
  def nextUuid: URSTM[TRandom, UUID] =
    ZSTM.accessM(_.get.nextUuid)

  /**
   * Sets the seed of this random number generator inside a transaction.
   */
  def setSeed(seed: Long): URSTM[TRandom, Unit] =
    ZSTM.accessM(_.get.setSeed(seed))

  /**
   * Randomly shuffles the specified list.
   */
  def shuffle[A](list: => List[A]): URSTM[TRandom, List[A]] =
    ZSTM.accessM(_.get.shuffle(list))
}

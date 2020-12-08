package zio

import java.util.UUID

package object random {
  type Random = Has[Random.Service]

  object Random extends Serializable {
    trait Service extends Serializable {
      def nextBoolean: UIO[Boolean]
      def nextBytes(length: Int): UIO[Chunk[Byte]]
      def nextDouble: UIO[Double]
      def nextDoubleBetween(minInclusive: Double, maxExclusive: Double): UIO[Double]
      def nextFloat: UIO[Float]
      def nextFloatBetween(minInclusive: Float, maxExclusive: Float): UIO[Float]
      def nextGaussian: UIO[Double]
      def nextInt: UIO[Int]
      def nextIntBetween(minInclusive: Int, maxExclusive: Int): UIO[Int]
      def nextIntBounded(n: Int): UIO[Int]
      def nextLong: UIO[Long]
      def nextLongBetween(minInclusive: Long, maxExclusive: Long): UIO[Long]
      def nextLongBounded(n: Long): UIO[Long]
      def nextPrintableChar: UIO[Char]
      def nextString(length: Int): UIO[String]
      def nextUuid: UIO[UUID]
      def setSeed(seed: Long): UIO[Unit]
      def shuffle[A, Collection[+Element] <: Iterable[Element]](collection: Collection[A])(implicit
        bf: BuildFrom[Collection[A], A, Collection[A]]
      ): UIO[Collection[A]]
    }

    object Service {
      val live: Service = new Service {
        import scala.util.{ Random => SRandom }

        val nextBoolean: UIO[Boolean] =
          ZIO.effectTotal(SRandom.nextBoolean())
        def nextBytes(length: Int): UIO[Chunk[Byte]] =
          ZIO.effectTotal {
            val array = Array.ofDim[Byte](length)
            SRandom.nextBytes(array)
            Chunk.fromArray(array)
          }
        val nextDouble: UIO[Double] =
          ZIO.effectTotal(SRandom.nextDouble())
        def nextDoubleBetween(minInclusive: Double, maxExclusive: Double): UIO[Double] =
          nextDoubleBetweenWith(minInclusive, maxExclusive)(nextDouble)
        val nextFloat: UIO[Float] =
          ZIO.effectTotal(SRandom.nextFloat())
        def nextFloatBetween(minInclusive: Float, maxExclusive: Float): UIO[Float] =
          nextFloatBetweenWith(minInclusive, maxExclusive)(nextFloat)
        val nextGaussian: UIO[Double] =
          ZIO.effectTotal(SRandom.nextGaussian())
        val nextInt: UIO[Int] =
          ZIO.effectTotal(SRandom.nextInt())
        def nextIntBetween(minInclusive: Int, maxExclusive: Int): UIO[Int] =
          nextIntBetweenWith(minInclusive, maxExclusive)(nextInt, nextIntBounded)
        def nextIntBounded(n: Int): UIO[Int] =
          ZIO.effectTotal(SRandom.nextInt(n))
        val nextLong: UIO[Long] =
          ZIO.effectTotal(SRandom.nextLong())
        def nextLongBetween(minInclusive: Long, maxExclusive: Long): UIO[Long] =
          nextLongBetweenWith(minInclusive, maxExclusive)(nextLong, nextLongBounded)
        def nextLongBounded(n: Long): UIO[Long] =
          Random.nextLongBoundedWith(n)(nextLong)
        val nextPrintableChar: UIO[Char] =
          ZIO.effectTotal(SRandom.nextPrintableChar())
        def nextString(length: Int): UIO[String] =
          ZIO.effectTotal(SRandom.nextString(length))
        def nextUuid: UIO[UUID] =
          ZIO.effectTotal(new UUID(SRandom.nextLong(), SRandom.nextLong()))
        def setSeed(seed: Long): UIO[Unit] =
          ZIO.effectTotal(SRandom.setSeed(seed))
        def shuffle[A, Collection[+Element] <: Iterable[Element]](
          collection: Collection[A]
        )(implicit bf: BuildFrom[Collection[A], A, Collection[A]]): UIO[Collection[A]] =
          Random.shuffleWith(nextIntBounded(_), collection)
      }
    }

    val any: ZLayer[Random, Nothing, Random] =
      ZLayer.requires[Random]

    val live: Layer[Nothing, Random] =
      ZLayer.succeed(Service.live)

    private[zio] def nextDoubleBetweenWith(minInclusive: Double, maxExclusive: Double)(
      nextDouble: UIO[Double]
    ): UIO[Double] =
      if (minInclusive >= maxExclusive)
        UIO.die(new IllegalArgumentException("invalid bounds"))
      else
        nextDouble.map { n =>
          val result = n * (maxExclusive - minInclusive) + minInclusive
          if (result < maxExclusive) result
          else Math.nextAfter(maxExclusive, Float.NegativeInfinity)
        }

    private[zio] def nextFloatBetweenWith(minInclusive: Float, maxExclusive: Float)(
      nextFloat: UIO[Float]
    ): UIO[Float] =
      if (minInclusive >= maxExclusive)
        UIO.die(new IllegalArgumentException("invalid bounds"))
      else
        nextFloat.map { n =>
          val result = n * (maxExclusive - minInclusive) + minInclusive
          if (result < maxExclusive) result
          else Math.nextAfter(maxExclusive, Float.NegativeInfinity)
        }

    private[zio] def nextIntBetweenWith(
      minInclusive: Int,
      maxExclusive: Int
    )(nextInt: UIO[Int], nextIntBounded: Int => UIO[Int]): UIO[Int] =
      if (minInclusive >= maxExclusive) {
        UIO.die(new IllegalArgumentException("invalid bounds"))
      } else {
        val difference = maxExclusive - minInclusive
        if (difference > 0) nextIntBounded(difference).map(_ + minInclusive)
        else nextInt.repeatUntil(n => minInclusive <= n && n < maxExclusive)
      }

    private[zio] def nextLongBetweenWith(
      minInclusive: Long,
      maxExclusive: Long
    )(nextLong: UIO[Long], nextLongBounded: Long => UIO[Long]): UIO[Long] =
      if (minInclusive >= maxExclusive)
        UIO.die(new IllegalArgumentException("invalid bounds"))
      else {
        val difference = maxExclusive - minInclusive
        if (difference > 0) nextLongBounded(difference).map(_ + minInclusive)
        else nextLong.repeatUntil(n => minInclusive <= n && n < maxExclusive)
      }

    private[zio] def nextLongBoundedWith(n: Long)(nextLong: UIO[Long]): UIO[Long] =
      if (n <= 0)
        UIO.die(new IllegalArgumentException("n must be positive"))
      else {
        nextLong.flatMap { r =>
          val m = n - 1
          if ((n & m) == 0L)
            UIO.succeedNow(r & m)
          else {
            def loop(u: Long): UIO[Long] =
              if (u + m - u % m < 0L) nextLong.flatMap(r => loop(r >>> 1))
              else UIO.succeedNow(u % n)
            loop(r >>> 1)
          }
        }
      }

    private[zio] def shuffleWith[A, Collection[+Element] <: Iterable[Element]](
      nextIntBounded: Int => UIO[Int],
      collection: Collection[A]
    )(implicit bf: BuildFrom[Collection[A], A, Collection[A]]): UIO[Collection[A]] =
      for {
        buffer <- ZIO.effectTotal {
                    val buffer = new scala.collection.mutable.ArrayBuffer[A]
                    buffer ++= collection
                  }
        swap = (i1: Int, i2: Int) =>
                 ZIO.effectTotal {
                   val tmp = buffer(i1)
                   buffer(i1) = buffer(i2)
                   buffer(i2) = tmp
                   buffer
                 }
        _ <-
          ZIO.foreach_((collection.size to 2 by -1).toList)((n: Int) => nextIntBounded(n).flatMap(k => swap(n - 1, k)))
      } yield bf.fromSpecific(collection)(buffer)
  }

  /**
   * generates a pseudo-random boolean.
   */
  val nextBoolean: URIO[Random, Boolean] =
    ZIO.accessM(_.get.nextBoolean)

  /**
   * Generates a pseudo-random chunk of bytes of the specified length.
   */
  def nextBytes(length: => Int): ZIO[Random, Nothing, Chunk[Byte]] =
    ZIO.accessM(_.get.nextBytes(length))

  /**
   * Generates a pseudo-random, uniformly distributed double between 0.0 and
   * 1.0.
   */
  val nextDouble: URIO[Random, Double] = ZIO.accessM(_.get.nextDouble)

  /**
   * Generates a pseudo-random double in the specified range.
   */
  def nextDoubleBetween(minInclusive: Double, maxExclusive: Double): URIO[Random, Double] =
    ZIO.accessM(_.get.nextDoubleBetween(minInclusive, maxExclusive))

  /**
   * Generates a pseudo-random, uniformly distributed float between 0.0 and
   * 1.0.
   */
  val nextFloat: URIO[Random, Float] =
    ZIO.accessM(_.get.nextFloat)

  /**
   * Generates a pseudo-random float in the specified range.
   */
  def nextFloatBetween(minInclusive: Float, maxExclusive: Float): URIO[Random, Float] =
    ZIO.accessM(_.get.nextFloatBetween(minInclusive, maxExclusive))

  /**
   * Generates a pseudo-random double from a normal distribution with mean 0.0
   * and standard deviation 1.0.
   */
  val nextGaussian: URIO[Random, Double] =
    ZIO.accessM(_.get.nextGaussian)

  /**
   * Generates a pseudo-random integer.
   */
  val nextInt: URIO[Random, Int] =
    ZIO.accessM(_.get.nextInt)

  /**
   * Generates a pseudo-random integer in the specified range.
   */
  def nextIntBetween(minInclusive: Int, maxExclusive: Int): URIO[Random, Int] =
    ZIO.accessM(_.get.nextIntBetween(minInclusive, maxExclusive))

  /**
   * Generates a pseudo-random integer between 0 (inclusive) and the specified
   * value (exclusive).
   */
  def nextIntBounded(n: => Int): URIO[Random, Int] =
    ZIO.accessM(_.get.nextIntBounded(n))

  /**
   * Generates a pseudo-random long.
   */
  val nextLong: URIO[Random, Long] =
    ZIO.accessM(_.get.nextLong)

  /**
   * Generates a pseudo-random long in the specified range.
   */
  def nextLongBetween(minInclusive: Long, maxExclusive: Long): URIO[Random, Long] =
    ZIO.accessM(_.get.nextLongBetween(minInclusive, maxExclusive))

  /**
   * Generates a pseudo-random long between 0 (inclusive) and the specified
   * value (exclusive).
   */
  def nextLongBounded(n: => Long): URIO[Random, Long] =
    ZIO.accessM(_.get.nextLongBounded(n))

  /**
   * Generates a pseudo-random character from the ASCII range 33-126.
   */
  val nextPrintableChar: URIO[Random, Char] =
    ZIO.accessM(_.get.nextPrintableChar)

  /**
   * Generates a pseudo-random string of the specified length.
   */
  def nextString(length: => Int): URIO[Random, String] =
    ZIO.accessM(_.get.nextString(length))

  /**
   * Generates a pseudo-random UUID.
   */
  def nextUuid: URIO[Random, UUID] =
    ZIO.accessM(_.get.nextUuid)

  /**
   * Sets the seed of this random number generator.
   */
  def setSeed(seed: Long): URIO[Random, Unit] =
    ZIO.accessM(_.get.setSeed(seed))

  /**
   * Randomly shuffles the specified list.
   */
  def shuffle[A](list: => List[A]): ZIO[Random, Nothing, List[A]] =
    ZIO.accessM(_.get.shuffle(list))
}

package zio.chunks

import org.openjdk.jmh.annotations._
import zio._

import java.util.concurrent.TimeUnit

@State(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@Measurement(iterations = 5, timeUnit = TimeUnit.SECONDS, time = 3)
@Warmup(iterations = 5, timeUnit = TimeUnit.SECONDS, time = 3)
@Fork(1)
class MixedChunkBenchmarks {
  @Param(Array("1000"))
  var size: Int = _

  var chunk: Chunk[Int] = _

  @Setup(Level.Trial)
  def setup(): Unit = {
    val array = (1 to size).toArray
    val whole = Chunk.fromArray(array)

    val firstQuarter  = whole.take(250)
    val secondQuarter = whole.drop(250).take(250)
    val thirdQuarter  = whole.drop(500).take(250)
    val fourthQuarter = whole.drop(750)

    val firstTwoHundred  = firstQuarter.take(200)
    val secondTwoHundred = firstQuarter.drop(200) ++ secondQuarter.take(150)
    val thirdTwoHundred  = secondQuarter.drop(150) ++ thirdQuarter.take(100)
    val fourthTwoHundred = thirdQuarter.drop(100) ++ fourthQuarter.take(50)
    val fifthTwoHundred  = fourthQuarter.drop(50)

    val firstHundredFifty         = firstTwoHundred.take(150)
    val secondThreeHundred        = firstTwoHundred.drop(150) ++ secondTwoHundred ++ thirdTwoHundred.take(50)
    val thirdFifty                = thirdTwoHundred.drop(50).take(50)
    val fourthTwoHundredFifty     = thirdTwoHundred.drop(100) ++ fourthTwoHundred.take(150)
    val fifthHundred              = fourthTwoHundred.drop(150) ++ fifthTwoHundred.take(50)
    val sixthOne                  = fifthTwoHundred.drop(1).take(1)
    val seventhHundredNinetyEight = fifthHundred.drop(2).take(198)
    val lastOne                   = fifthTwoHundred.drop(199)

    chunk = firstHundredFifty ++ secondThreeHundred ++ thirdFifty ++
      fourthTwoHundredFifty ++ fifthHundred ++ sixthOne ++
      seventhHundredNinetyEight ++ lastOne
  }

  @Benchmark
  def fold(): Int = chunk.foldLeft(0)(_ + _)

  @Benchmark
  def filterM(): Chunk[Int] =
    IOBenchmarks.unsafeRun(chunk.filterM[Any, Nothing](n => ZIO.succeed(n % 2 == 0)))

  @Benchmark
  def map(): Chunk[Int] = chunk.map(_ * 2)

  @Benchmark
  def flatMap(): Chunk[Int] = chunk.flatMap(n => Chunk(n + 2))

  @Benchmark
  def find(): Option[Int] = chunk.find(_ > 2)

  @Benchmark
  def mapM(): UIO[Unit] = chunk.mapM_(_ => ZIO.unit)

  @Benchmark
  def foldM(): Int =
    IOBenchmarks.unsafeRun(chunk.foldM[Any, Nothing, Int](0)((s, a) => ZIO.succeed(s + a)))

}

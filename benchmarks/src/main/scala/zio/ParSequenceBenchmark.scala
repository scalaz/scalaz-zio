package zio

import java.util.concurrent.TimeUnit

import scala.concurrent.Await
import scala.concurrent.duration.Duration

import monix.execution.Scheduler.Implicits.global
import monix.eval.{ Task => MTask }

import cats.effect.{ IO => CIO }
import org.openjdk.jmh.annotations._
import cats.effect.implicits._
import cats.implicits._
import IOBenchmarks.unsafeRun
import scala.concurrent.Future

@Measurement(iterations = 10, time = 3, timeUnit = TimeUnit.SECONDS)
@Warmup(iterations = 10, time = 3, timeUnit = TimeUnit.SECONDS)
@Fork(2)
@Threads(1)
@State(Scope.Thread)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
class ParSequenceBenchmark {
  @Param(Array("100", "1000"))
  var count: Int = _

  val parallelism: Int = 10

  @Benchmark
  def catsSequence(): Long = {
    val tasks  = (0 until count).map(_ => CIO(1)).toList
    val result = tasks.sequence.map(_.sum.toLong)
    result.unsafeRunSync()
  }

  @Benchmark
  def catsParSequence(): Long = {
    val tasks  = (0 until count).map(_ => CIO(1)).toList
    val result = tasks.parSequence.map(_.sum.toLong)
    result.unsafeRunSync()
  }

  @Benchmark
  def catsParSequenceN(): Long = {
    val tasks  = (0 until count).map(_ => CIO(1)).toList
    val result = tasks.parSequenceN(parallelism).map(_.sum.toLong)
    result.unsafeRunSync()
  }

  @Benchmark
  def monixSequence(): Long = {
    val tasks  = (0 until count).map(_ => MTask.eval(1)).toList
    val result = MTask.sequence(tasks).map(_.sum.toLong)
    result.runSyncUnsafe()
  }
  @Benchmark
  def monixGather(): Long = {
    val tasks  = (0 until count).map(_ => MTask.eval(1)).toList
    val result = MTask.gather(tasks).map(_.sum.toLong)
    result.runSyncUnsafe()
  }
  @Benchmark
  def monixGatherUnordered(): Long = {
    val tasks  = (0 until count).map(_ => MTask.eval(1)).toList
    val result = MTask.gatherUnordered(tasks).map(_.sum.toLong)
    result.runSyncUnsafe()
  }

  @Benchmark
  def monixGatherN(): Long = {
    val tasks  = (0 until count).map(_ => MTask.eval(1)).toList
    val result = MTask.gatherN(parallelism)(tasks).map(_.sum.toLong)
    result.runSyncUnsafe()
  }

  @Benchmark
  def zioSequence(): Long = {
    val tasks  = (0 until count).map(_ => ZIO.effectTotal(1)).toList
    val result = ZIO.sequence(tasks).map(_.sum.toLong)
    unsafeRun(result)
  }

  @Benchmark
  def zioParSequence(): Long = {
    val tasks  = (0 until count).map(_ => ZIO.effectTotal(1)).toList
    val result = ZIO.collectAllPar(tasks).map(_.sum.toLong)
    unsafeRun(result)
  }

  @Benchmark
  def zioParSequenceN(): Long = {
    val tasks  = (0 until count).map(_ => ZIO.effectTotal(1)).toList
    val result = ZIO.collectAllParN(parallelism)(tasks).map(_.sum.toLong)
    unsafeRun(result)
  }

  @Benchmark
  def futureSequence(): Long = {
    val futures         = (0 until count).map(_ => Future(1)).toList
    val f: Future[Long] = Future.sequence(futures).map(_.sum.toLong)
    Await.result(f, Duration.Inf)
  }

}

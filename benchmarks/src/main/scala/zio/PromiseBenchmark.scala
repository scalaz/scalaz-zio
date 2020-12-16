package zio

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._

@State(Scope.Thread)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(1)
@Threads(1)
class PromiseBenchmark {

  @Param(Array("100000"))
  var n: Int = _

  @Benchmark
  def tracedAwaitComplete(): Unit =
    createAwaitComplete(IOBenchmarks.TracedRuntime)

  @Benchmark
  def unTracedAwaitComplete(): Unit =
    createAwaitComplete(IOBenchmarks)

  @Benchmark
  def tracedAwaitUnsafeDone(): Unit =
    createAwaitUnsafeDone(IOBenchmarks.TracedRuntime)

  @Benchmark
  def unTracedAwaitUnsafeDone(): Unit =
    createAwaitUnsafeDone(IOBenchmarks)

  @Benchmark
  def tracedAwaitInterrupt(): Unit =
    createAwaitInterrupt(IOBenchmarks.TracedRuntime)

  @Benchmark
  def unTracedAwaitInterrupt(): Unit =
    createAwaitInterrupt(IOBenchmarks)

  private def createAwaitComplete(runtime: Runtime[Any]): Unit = runtime.unsafeRun {
    for {
      promise <- Promise.make[Nothing, Unit]
      joiners <- ZIO.loop(1)(_ <= n, _ + 1)(_ => promise.await.fork)
      _       <- promise.succeed(())
      _       <- ZIO.foreach_(joiners)(_.join)
    } yield ()
  }

  private def createAwaitUnsafeDone(runtime: Runtime[Any]): Unit = runtime.unsafeRun {
    for {
      promise <- Promise.make[Nothing, Unit]
      joiners <- ZIO.loop(1)(_ <= n, _ + 1)(_ => promise.await.fork)
      _       <- UIO.effectTotal(promise.unsafeDone(IO.unit))
      _       <- ZIO.foreach_(joiners)(_.join)
    } yield ()
  }

  private def createAwaitInterrupt(runtime: Runtime[Any]): Unit = runtime.unsafeRun {
    for {
      promise <- Promise.make[Nothing, Unit]
      joiners <- ZIO.loop(1)(_ <= n, _ + 1)(_ => promise.await.fork)
      _       <- ZIO.foreach_(joiners)(_.interrupt)
    } yield ()
  }
}

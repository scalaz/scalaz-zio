package zio

import java.util.concurrent.atomic.AtomicReference
import java.util.function.UnaryOperator

import scala.collection.immutable.SortedSet

import zio.Supervisor.Propagation
import zio.clock._
import zio.console._
import zio.duration._
import zio.test.TestAspect._
import zio.test._
import zio.test.environment.{ Live, TestClock }

// This test reproduces the behavior reported in https://github.com/zio/zio/issues/4384
// The general idea is to have a supervisor that can be used to regularly pull the complete
// Fiber.Dumps. Then we spin up some work to be done in the background, thereby causing
// changes to one or more stack traces.
// Eventually the dump poll will fail with a NullPointerException like in the sample output below.
// The original issue was found while monitoring a sample app with ZMX.

// Will produce something like
// sbt:core-tests> test:testOnly *FiberDumpSpec
// Starting test
// DEAHGIFBKJC
// BHIECAKGJFDDFJGKACEIHB

// BHIECAKGJFDDFJGKACEIHB

// Encountered null value in content of SingleThreadedRingBuffer, capacity=100, size=14, current=14, nullAt=0
// java.lang.NullPointerException
//   | => zat zio.ZTrace.$anonfun$prettyPrint$1(ZTrace.scala:36)
//         at scala.collection.immutable.List.map(List.scala:246)
// 	       at zio.ZTrace.prettyPrint(ZTrace.scala:36)
// 	       at zio.internal.FiberRenderer$.$anonfun$prettyPrint$5(FiberRenderer.scala:52)
// 	       at scala.Option.fold(Option.scala:263)
//        at zio.internal.FiberRenderer$.prettyPrint(FiberRenderer.scala:52)
//        at zio.internal.FiberRenderer$.$anonfun$prettyPrintM$1(FiberRenderer.scala:20)
//        at zio.internal.FiberContext.evaluateNow(FiberContext.scala:344)
//        at zio.internal.FiberContext.$anonfun$evaluateLater$1(FiberContext.scala:778)
//        at java.base/java.util.concurrent.ThreadPoolExecutor.runWorker(ThreadPoolExecutor.java:1128)
//        at java.base/java.util.concurrent.ThreadPoolExecutor$Worker.run(ThreadPoolExecutor.java:628)
//        at java.base/java.lang.Thread.run(Thread.java:834)
// [info] - The Fiber context should
// [info]   - allow to regularly poll the dump of all current fibers
// [info]     Fiber failed.
// [info]     An interrupt was produced by #79.
// [info]
// [info]     Fiber:Id(1605794931062,78) was supposed to continue to:
// [info]       a future continuation at zio.ZIO$._IdentityFn(ZIO.scala:3959)
// [info]       a future continuation at zio.ZIO.onInterrupt(ZIO.scala:981)
// [info]       a future continuation at zio.test.environment.package$TestClock$Test.sleep(package.scala:332)
// [info]       a future continuation at zio.Schedule.driver(Schedule.scala:347)
// [info]       a future continuation at zio.Schedule.driver(Schedule.scala:349)
// [info]       a future continuation at zio.ZIO.scheduleFrom(ZIO.scala:1684)
// [info]
// [info]     Fiber:Id(1605794931062,78) execution trace:
// [info]       at zio.Promise.await(Promise.scala:50)
// [info]       at zio.ZIO$.effectAsyncInterrupt(ZIO.scala:2574)
// [info]       at zio.ZIO$.effectAsyncInterrupt(ZIO.scala:2574)
// [info]       at zio.test.environment.package$TestClock$Test.sleep(package.scala:332)
// [info]       at zio.internal.FiberContext$InterruptExit$.apply(FiberContext.scala:161)
// [info]       at zio.internal.FiberContext$InterruptExit$.apply(FiberContext.scala:154)

object RepeatedFiberDumpSpec extends ZIOBaseSpec {

  override def runner: TestRunner[zio.test.environment.TestEnvironment, Any] =
    defaultTestRunner.withPlatform(_.withSupervisor(simpleSupervisor))

  override def spec: ZSpec[Live with TestClock with Clock with Console with Live with Annotations, Any] =
    (suite("The Fiber context should")(
      pollDumpForever
    )) @@ timed @@ timeout(90.seconds)

  private val pollDumpForever = testM("allow to regularly poll the dump of all current fibers")(for {
    _ <- timeWarp.fork
    // Start a number of busy Fibers
    _ <- ZIO.foreach(0.to(10))(_ => recurringWork.fork)
    f <- ZIO.sleep(30.seconds).fork
    fd <- (printDumps(simpleSupervisor).onError { t =>
            for {
              _ <- ZIO.succeed(t.dieOption.foreach(_.printStackTrace()))
              _ <- f.interrupt
            } yield ()
          }).schedule(Schedule.spaced(1.second)).fork
    _ <- f.join
    _ <- fd.interrupt
  } yield assertCompletes) // If we end up here the bug might have been fixed

  // Advance the test clock every 10 millis by a second
  private def timeWarp = for {
    _ <-
      Live
        .withLive(environment.TestClock.adjust(java.time.Duration.ofSeconds(1)))(_.repeat(Schedule.spaced(100.millis)))
  } yield ()

  // Create a fiber that does something in a loop, regularly calling itself .....
  private def recurringWork: ZIO[Clock with Console, Nothing, Unit] = {

    def go: ZIO[Clock with Console, Nothing, Unit] =
      putStr(s"") *> go.schedule(Schedule.duration(1.second)).flatMap(_ => ZIO.unit)
    go
  }

  private def getDumps(sv: Supervisor[SortedSet[Fiber.Runtime[Any, Any]]]): UIO[Iterable[Fiber.Dump]] =
    sv.value.flatMap(fibers => Fiber.dump(fibers.toSeq: _*))

  private def printDumps(sv: Supervisor[SortedSet[Fiber.Runtime[Any, Any]]]): ZIO[Console, Throwable, String] = for {
    dumps <- getDumps(sv)
    text  <- IO.foreach(dumps)(_.prettyPrintM)
  } yield (text.mkString("\n"))

  // A simple supervisor that
  private lazy val simpleSupervisor: Supervisor[SortedSet[Fiber.Runtime[Any, Any]]] =
    new Supervisor[SortedSet[Fiber.Runtime[Any, Any]]] {

      private[this] val fibers: AtomicReference[Map[Fiber.Id, Fiber.Runtime[Any, Any]]] = new AtomicReference(
        Map.empty
      )

      override def value: zio.UIO[SortedSet[Fiber.Runtime[Any, Any]]] =
        UIO(SortedSet(fibers.get.values.toSeq: _*))

      override private[zio] def unsafeOnStart[R, E, A](
        environment: R,
        effect: ZIO[R, E, A],
        parent: Option[Fiber.Runtime[Any, Any]],
        fiber: Fiber.Runtime[E, A]
      ): Supervisor.Propagation = {

        fibers.updateAndGet(new UnaryOperator[Map[Fiber.Id, Fiber.Runtime[Any, Any]]] {
          override def apply(t: Map[Fiber.Id, Fiber.Runtime[Any, Any]]): Map[Fiber.Id, Fiber.Runtime[Any, Any]] =
            t ++ Map(fiber.id -> fiber)
        })

        Propagation.Continue
      }

      override private[zio] def unsafeOnEnd[R, E, A](
        value: Exit[E, A],
        fiber: Fiber.Runtime[E, A]
      ): Propagation = {

        fibers.updateAndGet(new UnaryOperator[Map[Fiber.Id, Fiber.Runtime[Any, Any]]] {
          override def apply(t: Map[Fiber.Id, Fiber.Runtime[Any, Any]]): Map[Fiber.Id, Fiber.Runtime[Any, Any]] =
            t - fiber.id
        })

        Propagation.Continue
      }
    }
}

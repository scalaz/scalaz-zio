/*
 * Copyright 2019-2021 John A. De Goes and the ZIO Contributors
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

package zio.test

import zio.clock.Clock
import zio.console.Console
import zio.duration._
import zio.random.Random
import zio.system.System
import zio.{PlatformSpecific => _, _}

import java.io.{EOFException, IOException}
import java.time.{Instant, LocalDateTime, OffsetDateTime, ZoneId}
import java.util.concurrent.TimeUnit
import scala.collection.immutable.{Queue, SortedSet}
import scala.math.{log, sqrt}

/**
 * The `environment` package contains testable versions of all the standard ZIO
 * environment types through the [[TestClock]], [[TestConsole]],
 * [[TestSystem]], and [[TestRandom]] modules. See the documentation on the
 * individual modules for more detail about using each of them.
 *
 * If you are using ZIO Test and extending `RunnableSpec` a
 * `TestEnvironment` containing all of them will be automatically provided to
 * each of your tests. Otherwise, the easiest way to use the test implementations
 * in ZIO Test is by providing the `TestEnvironment` to your program.
 *
 * {{{
 * import zio.test.environment._
 *
 * myProgram.provideLayer(testEnvironment)
 * }}}
 *
 * Then all environmental effects, such as printing to the console or
 * generating random numbers, will be implemented by the `TestEnvironment` and
 * will be fully testable. When you do need to access the "live" environment,
 * for example to print debugging information to the console, just use the
 * `live` combinator along with the effect as your normally would.
 *
 * If you are only interested in one of the test implementations for your
 * application, you can also access them a la carte through the `make` method
 * on each module. Each test module requires some data on initialization.
 * Default data is included for each as `DefaultData`.
 *
 * {{{
 * import zio.test.environment._
 *
 * myProgram.provideM(TestConsole.make(TestConsole.DefaultData))
 * }}}
 *
 * Finally, you can create a `Test` object that implements the test interface
 * directly using the `makeTest` method. This can be useful when you want to
 * access some testing functionality without using the environment type.
 *
 * {{{
 * import zio.test.environment._
 *
 * for {
 *   testRandom <- TestRandom.makeTest(TestRandom.DefaultData)
 *   n          <- testRandom.nextInt
 * } yield n
 * }}}
 *
 * This can also be useful when you are creating a more complex environment
 * to provide the implementation for test services that you mix in.
 */
package object environment extends PlatformSpecific {
  type Live        = Has[Live.Service]
  type TestClock   = Has[TestClock.Service]
  type TestConsole = Has[TestConsole.Service]
  type TestRandom  = Has[TestRandom.Service]
  type TestSystem  = Has[TestSystem.Service]

  val liveEnvironment: Layer[Nothing, ZEnv] = ZEnv.live

  val testEnvironment: Layer[Nothing, TestEnvironment] =
    ZEnv.live >>> TestEnvironment.live

  /**
   * Provides an effect with the "real" environment as opposed to the test
   * environment. This is useful for performing effects such as timing out
   * tests, accessing the real time, or printing to the real console.
   */
  def live[E, A](zio: ZIO[ZEnv, E, A]): ZIO[Live, E, A] =
    Live.live(zio)

  /**
   * Transforms this effect with the specified function. The test environment
   * will be provided to this effect, but the live environment will be provided
   * to the transformation function. This can be useful for applying
   * transformations to an effect that require access to the "real" environment
   * while ensuring that the effect itself uses the test environment.
   *
   * {{{
   *  withLive(test)(_.timeout(duration))
   * }}}
   */
  def withLive[R, E, E1, A, B](
    zio: ZIO[R, E, A]
  )(f: IO[E, A] => ZIO[ZEnv, E1, B]): ZIO[R with Live, E1, B] =
    Live.withLive(zio)(f)

  /**
   * The `Live` trait provides access to the "live" environment from within the
   * test environment for effects such as printing test results to the console or
   * timing out tests where it is necessary to access the real environment.
   *
   * The easiest way to access the "live" environment is to use the `live` method
   * with an effect that would otherwise access the test environment.
   *
   * {{{
   * import zio.clock
   * import zio.test.environment._
   *
   * val realTime = live(clock.nanoTime)
   * }}}
   *
   * The `withLive` method can be used to apply a transformation to an effect
   * with the live environment while ensuring that the effect itself still runs
   * with the test environment, for example to time out a test. Both of these
   * methods are re-exported in the `environment` package for easy availability.
   */
  object Live {

    trait Service {
      def provide[E, A](zio: ZIO[ZEnv, E, A]): IO[E, A]
    }

    /**
     * Constructs a new `Live` service that implements the `Live` interface.
     * This typically should not be necessary as `TestEnvironment` provides
     * access to live versions of all the standard ZIO environment types but
     * could be useful if you are mixing in interfaces to create your own
     * environment type.
     */
    def default: ZLayer[ZEnv, Nothing, Live] =
      ZLayer(ZManaged.access[ZEnv] { zenv =>
        Has(new Live.Service {
          def provide[E, A](zio: ZIO[ZEnv, E, A]): IO[E, A] =
            zio.provide(zenv)
        })
      })

    /**
     * Provides an effect with the "live" environment.
     */
    def live[E, A](zio: ZIO[ZEnv, E, A]): ZIO[Live, E, A] =
      ZIO.accessM(_.get.provide(zio))

    /**
     * Provides a transformation function with access to the live environment
     * while ensuring that the effect itself is provided with the test
     * environment.
     */
    def withLive[R <: Live, E, E1, A, B](
      zio: ZIO[R, E, A]
    )(f: IO[E, A] => ZIO[ZEnv, E1, B]): ZIO[R, E1, B] =
      ZIO.environment[R].flatMap(r => live(f(zio.provide(r))))
  }

  /**
   * `TestClock` makes it easy to deterministically and efficiently test
   * effects involving the passage of time.
   *
   * Instead of waiting for actual time to pass, `sleep` and methods
   * implemented in terms of it schedule effects to take place at a given clock
   * time. Users can adjust the clock time using the `adjust` and `setTime`
   * methods, and all effects scheduled to take place on or before that time
   * will automatically be run in order.
   *
   * For example, here is how we can test `ZIO#timeout` using `TestClock:
   *
   * {{{
   *  import zio.ZIO
   *  import zio.duration._
   *  import zio.test.environment.TestClock
   *
   *  for {
   *    fiber  <- ZIO.sleep(5.minutes).timeout(1.minute).fork
   *    _      <- TestClock.adjust(1.minute)
   *    result <- fiber.join
   *  } yield result == None
   * }}}
   *
   * Note how we forked the fiber that `sleep` was invoked on. Calls to `sleep`
   * and methods derived from it will semantically block until the time is set
   * to on or after the time they are scheduled to run. If we didn't fork the
   * fiber on which we called sleep we would never get to set the time on the
   * line below. Thus, a useful pattern when using `TestClock` is to fork the
   * effect being tested, then adjust the clock time, and finally verify that
   * the expected effects have been performed.
   *
   * For example, here is how we can test an effect that recurs with a fixed
   * delay:
   *
   * {{{
   *  import zio.Queue
   *  import zio.duration._
   *  import zio.test.environment.TestClock
   *
   *  for {
   *    q <- Queue.unbounded[Unit]
   *    _ <- q.offer(()).delay(60.minutes).forever.fork
   *    a <- q.poll.map(_.isEmpty)
   *    _ <- TestClock.adjust(60.minutes)
   *    b <- q.take.as(true)
   *    c <- q.poll.map(_.isEmpty)
   *    _ <- TestClock.adjust(60.minutes)
   *    d <- q.take.as(true)
   *    e <- q.poll.map(_.isEmpty)
   *  } yield a && b && c && d && e
   * }}}
   *
   * Here we verify that no effect is performed before the recurrence period,
   * that an effect is performed after the recurrence period, and that the
   * effect is performed exactly once. The key thing to note here is that after
   * each recurrence the next recurrence is scheduled to occur at the
   * appropriate time in the future, so when we adjust the clock by 60 minutes
   * exactly one value is placed in the queue, and when we adjust the clock by
   * another 60 minutes exactly one more value is placed in the queue.
   */
  object TestClock extends Serializable {

    trait Service extends Restorable {
      def adjust(duration: Duration): UIO[Unit]
      def setDateTime(dateTime: OffsetDateTime): UIO[Unit]
      def setTime(duration: Duration): UIO[Unit]
      def setTimeZone(zone: ZoneId): UIO[Unit]
      def sleeps: UIO[List[Duration]]
      def timeZone: UIO[ZoneId]
    }

    final case class Test(
      clockState: Ref.Atomic[TestClock.Data],
      live: Live.Service,
      annotations: Annotations.Service,
      warningState: RefM[TestClock.WarningData]
    ) extends Clock.Service
        with TestClock.Service
        with TestClockPlatformSpecific {

      /**
       * Increments the current clock time by the specified duration. Any
       * effects that were scheduled to occur on or before the new time will be
       * run in order.
       */
      def adjust(duration: Duration): UIO[Unit] =
        warningDone *> run(_ + duration)

      /**
       * Returns the current clock time as an `OffsetDateTime`.
       */
      def currentDateTime: UIO[OffsetDateTime] =
        clockState.get.map(data => toDateTime(data.duration, data.timeZone))

      /**
       * Returns the current clock time in the specified time unit.
       */
      def currentTime(unit: TimeUnit): UIO[Long] =
        clockState.get.map(data => unit.convert(data.duration.toMillis, TimeUnit.MILLISECONDS))

      /**
       * Returns the current clock time in nanoseconds.
       */
      val nanoTime: UIO[Long] =
        clockState.get.map(_.duration.toNanos)

      /**
       * Returns the current clock time as an `Instant`.
       */
      val instant: UIO[Instant] =
        clockState.get.map(data => toInstant(data.duration))

      /**
       * Returns the current clock time as a `LocalDateTime`.
       */
      val localDateTime: UIO[LocalDateTime] =
        clockState.get.map(data => toLocalDateTime(data.duration, data.timeZone))

      /**
       * Saves the `TestClock`'s current state in an effect which, when run,
       * will restore the `TestClock` state to the saved state
       */
      val save: UIO[UIO[Unit]] =
        for {
          clockData <- clockState.get
        } yield clockState.set(clockData)

      /**
       * Sets the current clock time to the specified `OffsetDateTime`. Any
       * effects that were scheduled to occur on or before the new time will
       * be run in order.
       */
      def setDateTime(dateTime: OffsetDateTime): UIO[Unit] =
        setTime(fromDateTime(dateTime))

      /**
       * Sets the current clock time to the specified time in terms of duration
       * since the epoch. Any effects that were scheduled to occur on or before
       * the new time will immediately be run in order.
       */
      def setTime(duration: Duration): UIO[Unit] =
        warningDone *> run(_ => duration)

      /**
       * Sets the time zone to the specified time zone. The clock time in
       * terms of nanoseconds since the epoch will not be adjusted and no
       * scheduled effects will be run as a result of this method.
       */
      def setTimeZone(zone: ZoneId): UIO[Unit] =
        clockState.update(_.copy(timeZone = zone))

      /**
       * Semantically blocks the current fiber until the clock time is equal
       * to or greater than the specified duration. Once the clock time is
       * adjusted to on or after the duration, the fiber will automatically be
       * resumed.
       */
      def sleep(duration: Duration): UIO[Unit] =
        for {
          promise <- Promise.make[Nothing, Unit]
          shouldAwait <- clockState.modify { data =>
                           val end = data.duration + duration
                           if (end > data.duration)
                             (true, data.copy(sleeps = (end, promise) :: data.sleeps))
                           else
                             (false, data)
                         }
          _ <- if (shouldAwait) warningStart *> promise.await else promise.succeed(())
        } yield ()

      /**
       * Returns a list of the times at which all queued effects are scheduled
       * to resume.
       */
      lazy val sleeps: UIO[List[Duration]] =
        clockState.get.map(_.sleeps.map(_._1))

      /**
       * Returns the time zone.
       */
      lazy val timeZone: UIO[ZoneId] =
        clockState.get.map(_.timeZone)

      /**
       * Cancels the warning message that is displayed if a test is using time
       * but is not advancing the `TestClock`.
       */
      private[TestClock] val warningDone: UIO[Unit] =
        warningState.updateSomeM[Any, Nothing] {
          case WarningData.Start          => ZIO.succeedNow(WarningData.done)
          case WarningData.Pending(fiber) => fiber.interrupt.as(WarningData.done)
        }

      /**
       * Polls until all descendants of this fiber are done or suspended.
       */
      private lazy val awaitSuspended: UIO[Unit] =
        suspended
          .zipWith(live.provide(ZIO.sleep(10.milliseconds)) *> suspended)(_ == _)
          .filterOrFail(identity)(())
          .eventually
          .unit

      /**
       * Delays for a short period of time.
       */
      private lazy val delay: UIO[Unit] =
        live.provide(ZIO.sleep(5.milliseconds))

      /**
       * Captures a "snapshot" of the identifier and status of all fibers in
       * this test other than the current fiber. Fails with the `Unit` value if
       * any of these fibers are not done or suspended. Note that because we
       * cannot synchronize on the status of multiple fibers at the same time
       * this snapshot may not be fully consistent.
       */
      private lazy val freeze: IO[Unit, Map[Fiber.Id, Fiber.Status]] =
        supervisedFibers.flatMap { fibers =>
          IO.foldLeft(fibers)(Map.empty[Fiber.Id, Fiber.Status]) { (map, fiber) =>
            fiber.status.flatMap {
              case done @ Fiber.Status.Done                          => IO.succeedNow(map + (fiber.id -> done))
              case suspended @ Fiber.Status.Suspended(_, _, _, _, _) => IO.succeedNow(map + (fiber.id -> suspended))
              case _                                                 => IO.fail(())
            }
          }
        }

      /**
       * Returns a set of all fibers in this test.
       */
      def supervisedFibers: UIO[SortedSet[Fiber.Runtime[Any, Any]]] =
        ZIO.descriptorWith { descriptor =>
          annotations.get(TestAnnotation.fibers).flatMap {
            case Left(_) => ZIO.succeedNow(SortedSet.empty[Fiber.Runtime[Any, Any]])
            case Right(refs) =>
              ZIO
                .foreach(refs)(ref => ZIO.effectTotal(ref.get))
                .map(_.foldLeft(SortedSet.empty[Fiber.Runtime[Any, Any]])(_ ++ _))
                .map(_.filter(_.id != descriptor.id))
          }
        }

      /**
       * Constructs a `Duration` from an `OffsetDateTime`.
       */
      private def fromDateTime(dateTime: OffsetDateTime): Duration =
        Duration(dateTime.toInstant.toEpochMilli, TimeUnit.MILLISECONDS)

      /**
       * Runs all effects scheduled to occur on or before the specified
       * duration, which may depend on the current time, in order.
       */
      private def run(f: Duration => Duration): UIO[Unit] =
        awaitSuspended *>
          clockState.modify { data =>
            val end = f(data.duration)
            data.sleeps.sortBy(_._1) match {
              case (duration, promise) :: sleeps if duration <= end =>
                (Some((end, promise)), Data(duration, sleeps, data.timeZone))
              case _ => (None, Data(end, data.sleeps, data.timeZone))
            }
          }.flatMap {
            case None => UIO.unit
            case Some((end, promise)) =>
              promise.succeed(()) *>
                ZIO.yieldNow *>
                run(_ => end)
          }

      /**
       * Returns whether all descendants of this fiber are done or suspended.
       */
      private lazy val suspended: IO[Unit, Map[Fiber.Id, Fiber.Status]] =
        freeze.zip(delay *> freeze).flatMap { case (first, last) =>
          if (first == last) ZIO.succeedNow(first)
          else ZIO.fail(())
        }

      /**
       * Constructs an `OffsetDateTime` from a `Duration` and a `ZoneId`.
       */
      private def toDateTime(duration: Duration, timeZone: ZoneId): OffsetDateTime =
        OffsetDateTime.ofInstant(toInstant(duration), timeZone)

      /**
       * Constructs a `LocalDateTime` from a `Duration` and a `ZoneId`.
       */
      private def toLocalDateTime(duration: Duration, timeZone: ZoneId): LocalDateTime =
        LocalDateTime.ofInstant(toInstant(duration), timeZone)

      /**
       * Constructs an `Instant` from a `Duration`.
       */
      private def toInstant(duration: Duration): Instant =
        Instant.ofEpochMilli(duration.toMillis)

      /**
       * Forks a fiber that will display a warning message if a test is using
       * time but is not advancing the `TestClock`.
       */
      private val warningStart: UIO[Unit] =
        warningState.updateSomeM { case WarningData.Start =>
          for {
            fiber <- live.provide(console.putStrLn(warning).delay(5.seconds)).interruptible.fork
          } yield WarningData.pending(fiber)
        }

    }

    /**
     * Constructs a new `Test` object that implements the `TestClock`
     * interface. This can be useful for mixing in with implementations of
     * other interfaces.
     */
    def live(data: Data): ZLayer[Live with Annotations, Nothing, Clock with TestClock] =
      ZLayer.fromServicesManyManaged[Live.Service, Annotations.Service, Any, Nothing, Clock with TestClock] {
        (live: Live.Service, annotations: Annotations.Service) =>
          for {
            ref  <- ZManaged.effectTotal(Ref.unsafeMake(data))
            refM <- RefM.makeManaged(WarningData.start)
            test <- Managed.make(UIO(Test(ref, live, annotations, refM)))(_.warningDone)
          } yield Has.allOf[Clock.Service, TestClock.Service](test, test)
      }

    val any: ZLayer[Clock with TestClock, Nothing, Clock with TestClock] =
      ZLayer.requires[Clock with TestClock]

    val default: ZLayer[Live with Annotations, Nothing, Clock with TestClock] =
      live(Data(Duration.Zero, Nil, ZoneId.of("UTC")))

    /**
     * Accesses a `TestClock` instance in the environment and increments the
     * time by the specified duration, running any actions scheduled for on or
     * before the new time in order.
     */
    def adjust(duration: => Duration): URIO[TestClock, Unit] =
      ZIO.accessM(_.get.adjust(duration))

    /**
     * Accesses a `TestClock` instance in the environment and saves the clock
     * state in an effect which, when run, will restore the `TestClock` to the
     * saved state.
     */
    val save: ZIO[TestClock, Nothing, UIO[Unit]] =
      ZIO.accessM(_.get.save)

    /**
     * Accesses a `TestClock` instance in the environment and sets the clock
     * time to the specified `OffsetDateTime`, running any actions scheduled
     * for on or before the new time in order.
     */
    def setDateTime(dateTime: => OffsetDateTime): URIO[TestClock, Unit] =
      ZIO.accessM(_.get.setDateTime(dateTime))

    /**
     * Accesses a `TestClock` instance in the environment and sets the clock
     * time to the specified time in terms of duration since the epoch,
     * running any actions scheduled for on or before the new time in order.
     */
    def setTime(duration: => Duration): URIO[TestClock, Unit] =
      ZIO.accessM(_.get.setTime(duration))

    /**
     * Accesses a `TestClock` instance in the environment, setting the time
     * zone to the specified time zone. The clock time in terms of nanoseconds
     * since the epoch will not be altered and no scheduled actions will be
     * run as a result of this effect.
     */
    def setTimeZone(zone: => ZoneId): URIO[TestClock, Unit] =
      ZIO.accessM(_.get.setTimeZone(zone))

    /**
     * Accesses a `TestClock` instance in the environment and returns a list
     * of times that effects are scheduled to run.
     */
    val sleeps: ZIO[TestClock, Nothing, List[Duration]] =
      ZIO.accessM(_.get.sleeps)

    /**
     * Accesses a `TestClock` instance in the environment and returns the current
     * time zone.
     */
    val timeZone: URIO[TestClock, ZoneId] =
      ZIO.accessM(_.get.timeZone)

    /**
     * `Data` represents the state of the `TestClock`, including the clock time
     * and time zone.
     */
    final case class Data(
      duration: Duration,
      sleeps: List[(Duration, Promise[Nothing, Unit])],
      timeZone: ZoneId
    )

    /**
     * `WarningData` describes the state of the warning message that is
     * displayed if a test is using time by is not advancing the `TestClock`.
     * The possible states are `Start` if a test has not used time, `Pending`
     * if a test has used time but has not adjusted the `TestClock`, and `Done`
     * if a test has adjusted the `TestClock` or the warning message has
     * already been displayed.
     */
    sealed abstract class WarningData

    object WarningData {

      case object Start                                     extends WarningData
      final case class Pending(fiber: Fiber[Nothing, Unit]) extends WarningData
      case object Done                                      extends WarningData

      /**
       * State indicating that a test has not used time.
       */
      val start: WarningData = Start

      /**
       * State indicating that a test has used time but has not adjusted the
       * `TestClock` with a reference to the fiber that will display the
       * warning message.
       */
      def pending(fiber: Fiber[Nothing, Unit]): WarningData = Pending(fiber)

      /**
       * State indicating that a test has used time or the warning message has
       * already been displayed.
       */
      val done: WarningData = Done
    }

    /**
     * The warning message that will be displayed if a test is using time but
     * is not advancing the `TestClock`.
     */
    private val warning =
      "Warning: A test is using time, but is not advancing the test clock, " +
        "which may result in the test hanging. Use TestClock.adjust to " +
        "manually advance the time."
  }

  /**
   * `TestConsole` provides a testable interface for programs interacting with
   * the console by modeling input and output as reading from and writing to
   * input and output buffers maintained by `TestConsole` and backed by a
   * `Ref`.
   *
   * All calls to `putStr` and `putStrLn` using the `TestConsole` will write
   * the string to the output buffer and all calls to `getStrLn` will take a
   * string from the input buffer. To facilitate debugging, by default output
   * will also be rendered to standard output. You can enable or disable this
   * for a scope using `debug`, `silent`, or the corresponding test aspects.
   *
   * `TestConsole` has several methods to access and manipulate the content of
   * these buffers including `feedLines` to feed strings to the input  buffer
   * that will then be returned by calls to `getStrLn`, `output` to get the
   * content of the output buffer from calls to `putStr` and `putStrLn`, and
   * `clearInput` and `clearOutput` to clear the respective buffers.
   *
   * Together, these functions make it easy to test programs interacting with
   * the console.
   *
   * {{{
   * import zio.console._
   * import zio.test.environment.TestConsole
   * import zio.ZIO
   *
   * val sayHello = for {
   *   name <- getStrLn
   *   _    <- putStrLn("Hello, " + name + "!")
   * } yield ()
   *
   * for {
   *   _ <- TestConsole.feedLines("John", "Jane", "Sally")
   *   _ <- ZIO.collectAll(List.fill(3)(sayHello))
   *   result <- TestConsole.output
   * } yield result == Vector("Hello, John!\n", "Hello, Jane!\n", "Hello, Sally!\n")
   * }}}
   */
  object TestConsole extends Serializable {

    trait Service extends Restorable {
      def clearInput: UIO[Unit]
      def clearOutput: UIO[Unit]
      def debug[R, E, A](zio: ZIO[R, E, A]): ZIO[R, E, A]
      def feedLines(lines: String*): UIO[Unit]
      def output: UIO[Vector[String]]
      def outputErr: UIO[Vector[String]]
      def silent[R, E, A](zio: ZIO[R, E, A]): ZIO[R, E, A]
    }

    case class Test(
      consoleState: Ref[TestConsole.Data],
      live: Live.Service,
      debugState: FiberRef[Boolean]
    ) extends Console.Service
        with TestConsole.Service {

      /**
       * Clears the contents of the input buffer.
       */
      val clearInput: UIO[Unit] =
        consoleState.update(data => data.copy(input = List.empty))

      /**
       * Clears the contents of the output buffer.
       */
      val clearOutput: UIO[Unit] =
        consoleState.update(data => data.copy(output = Vector.empty))

      /**
       * Runs the specified effect with the `TestConsole` set to debug mode,
       * so that console output is rendered to standard output in addition to
       * being written to the output buffer.
       */
      def debug[R, E, A](zio: ZIO[R, E, A]): ZIO[R, E, A] =
        debugState.locally(true)(zio)

      /**
       * Writes the specified sequence of strings to the input buffer. The
       * first string in the sequence will be the first to be taken. These
       * strings will be taken before any strings that were previously in the
       * input buffer.
       */
      def feedLines(lines: String*): UIO[Unit] =
        consoleState.update(data => data.copy(input = lines.toList ::: data.input))

      /**
       * Takes the first value from the input buffer, if one exists, or else
       * fails with an `EOFException`.
       */
      val getStrLn: IO[IOException, String] = {
        for {
          input <- consoleState.get.flatMap(d =>
                     IO.fromOption(d.input.headOption)
                       .orElseFail(new EOFException("There is no more input left to read"))
                   )
          _ <- consoleState.update(data => Data(data.input.tail, data.output, data.errOutput))
        } yield input
      }

      /**
       * Returns the contents of the output buffer. The first value written to
       * the output buffer will be the first in the sequence.
       */
      val output: UIO[Vector[String]] =
        consoleState.get.map(_.output)

      /**
       * Returns the contents of the error output buffer. The first value written to
       * the error output buffer will be the first in the sequence.
       */
      val outputErr: UIO[Vector[String]] =
        consoleState.get.map(_.errOutput)

      /**
       * Writes the specified string to the output buffer.
       */
      override def putStr(line: String): UIO[Unit] =
        consoleState.update { data =>
          Data(data.input, data.output :+ line, data.errOutput)
        } *> live.provide(console.putStr(line)).whenM(debugState.get)

      /**
       * Writes the specified string to the error buffer.
       */
      override def putStrErr(line: String): UIO[Unit] =
        consoleState.update { data =>
          Data(data.input, data.output, data.errOutput :+ line)
        } *> live.provide(console.putStr(line)).whenM(debugState.get)

      /**
       * Writes the specified string to the output buffer followed by a newline
       * character.
       */
      override def putStrLn(line: String): UIO[Unit] =
        consoleState.update { data =>
          Data(data.input, data.output :+ s"$line\n", data.errOutput)
        } *> live.provide(console.putStrLn(line)).whenM(debugState.get)

      /**
       * Writes the specified string to the error buffer followed by a newline
       * character.
       */
      override def putStrLnErr(line: String): UIO[Unit] =
        consoleState.update { data =>
          Data(data.input, data.output, data.errOutput :+ s"$line\n")
        } *> live.provide(console.putStrLn(line)).whenM(debugState.get)

      /**
       * Saves the `TestConsole`'s current state in an effect which, when run,
       * will restore the `TestConsole` state to the saved state.
       */
      val save: UIO[UIO[Unit]] =
        for {
          consoleData <- consoleState.get
        } yield consoleState.set(consoleData)

      /**
       * Runs the specified effect with the `TestConsole` set to silent mode,
       * so that console output is only written to the output buffer and not
       * rendered to standard output.
       */
      def silent[R, E, A](zio: ZIO[R, E, A]): ZIO[R, E, A] =
        debugState.locally(false)(zio)
    }

    /**
     * Constructs a new `Test` object that implements the `TestConsole`
     * interface. This can be useful for mixing in with implementations of other
     * interfaces.
     */
    def make(data: Data, debug: Boolean = true): ZLayer[Live, Nothing, Console with TestConsole] =
      ZLayer.fromServiceManyM { (live: Live.Service) =>
        for {
          ref      <- Ref.make(data)
          debugRef <- FiberRef.make(debug)
          test      = Test(ref, live, debugRef)
        } yield Has.allOf[Console.Service, TestConsole.Service](test, test)
      }

    val any: ZLayer[Console with TestConsole, Nothing, Console with TestConsole] =
      ZLayer.requires[Console with TestConsole]

    val debug: ZLayer[Live, Nothing, Console with TestConsole] =
      make(Data(Nil, Vector()), true)

    val silent: ZLayer[Live, Nothing, Console with TestConsole] =
      make(Data(Nil, Vector()), false)

    /**
     * Accesses a `TestConsole` instance in the environment and clears the input
     * buffer.
     */
    val clearInput: URIO[TestConsole, Unit] =
      ZIO.accessM(_.get.clearInput)

    /**
     * Accesses a `TestConsole` instance in the environment and clears the output
     * buffer.
     */
    val clearOutput: URIO[TestConsole, Unit] =
      ZIO.accessM(_.get.clearOutput)

    /**
     * Accesses a `TestConsole` instance in the environment and runs the
     * specified effect with the `TestConsole` set to debug mode, so that
     * console output is rendered to standard output in addition to being
     * written to the output buffer.
     */
    def debug[R <: TestConsole, E, A](zio: ZIO[R, E, A]): ZIO[R, E, A] =
      ZIO.accessM(_.get.debug(zio))

    /**
     * Accesses a `TestConsole` instance in the environment and writes the
     * specified sequence of strings to the input buffer.
     */
    def feedLines(lines: String*): URIO[TestConsole, Unit] =
      ZIO.accessM(_.get.feedLines(lines: _*))

    /**
     * Accesses a `TestConsole` instance in the environment and returns the
     * contents of the output buffer.
     */
    val output: ZIO[TestConsole, Nothing, Vector[String]] =
      ZIO.accessM(_.get.output)

    /**
     * Accesses a `TestConsole` instance in the environment and returns the
     * contents of the error buffer.
     */
    val outputErr: ZIO[TestConsole, Nothing, Vector[String]] =
      ZIO.accessM(_.get.outputErr)

    /**
     * Accesses a `TestConsole` instance in the environment and saves the
     * console state in an effect which, when run, will restore the
     * `TestConsole` to the saved state.
     */
    val save: ZIO[TestConsole, Nothing, UIO[Unit]] =
      ZIO.accessM(_.get.save)

    /**
     * Accesses a `TestConsole` instance in the environment and runs the
     * specified effect with the `TestConsole` set to silent mode, so that
     * console output is only written to the output buffer and not rendered to
     * standard output.
     */
    def silent[R <: TestConsole, E, A](zio: ZIO[R, E, A]): ZIO[R, E, A] =
      ZIO.accessM(_.get.silent(zio))

    /**
     * The state of the `TestConsole`.
     */
    final case class Data(
      input: List[String] = List.empty,
      output: Vector[String] = Vector.empty,
      errOutput: Vector[String] = Vector.empty
    )
  }

  /**
   * `TestRandom` allows for deterministically testing effects involving
   * randomness.
   *
   * `TestRandom` operates in two modes. In the first mode, `TestRandom` is a
   * purely functional pseudo-random number generator. It will generate
   * pseudo-random values just like `scala.util.Random` except that no internal
   * state is mutated. Instead, methods like `nextInt` describe state transitions
   * from one random state to another that are automatically composed together
   * through methods like `flatMap`. The random seed can be set using `setSeed`
   * and `TestRandom` is guaranteed to return the same sequence of values for any
   * given seed. This is useful for deterministically generating a sequence of
   * pseudo-random values and powers the property based testing functionality in
   * ZIO Test.
   *
   * In the second mode, `TestRandom` maintains an internal buffer of values that
   * can be "fed" with methods such as `feedInts` and then when random values of
   * that type are generated they will first be taken from the buffer. This is
   * useful for verifying that functions produce the expected output for a given
   * sequence of "random" inputs.
   *
   * {{{
   * import zio.random._
   * import zio.test.environment.TestRandom
   *
   * for {
   *   _ <- TestRandom.feedInts(4, 5, 2)
   *   x <- random.nextIntBounded(6)
   *   y <- random.nextIntBounded(6)
   *   z <- random.nextIntBounded(6)
   * } yield x + y + z == 11
   * }}}
   *
   * `TestRandom` will automatically take values from the buffer if a value of
   * the appropriate type is available and otherwise generate a pseudo-random
   * value, so there is nothing you need to do to switch between the two modes.
   * Just generate random values as you normally would to get pseudo-random
   * values, or feed in values of your own to get those values back. You can also
   * use methods like `clearInts` to clear the buffer of values of a given type
   * so you can fill the buffer with new values or go back to pseudo-random
   * number generation.
   */
  object TestRandom extends Serializable {

    trait Service extends Restorable {
      def clearBooleans: UIO[Unit]
      def clearBytes: UIO[Unit]
      def clearChars: UIO[Unit]
      def clearDoubles: UIO[Unit]
      def clearFloats: UIO[Unit]
      def clearInts: UIO[Unit]
      def clearLongs: UIO[Unit]
      def clearStrings: UIO[Unit]
      def feedBooleans(booleans: Boolean*): UIO[Unit]
      def feedBytes(bytes: Chunk[Byte]*): UIO[Unit]
      def feedChars(chars: Char*): UIO[Unit]
      def feedDoubles(doubles: Double*): UIO[Unit]
      def feedFloats(floats: Float*): UIO[Unit]
      def feedInts(ints: Int*): UIO[Unit]
      def feedLongs(longs: Long*): UIO[Unit]
      def feedStrings(strings: String*): UIO[Unit]
      def getSeed: UIO[Long]
      def setSeed(seed: Long): UIO[Unit]
    }

    /**
     * Adapted from @gzmo work in Scala.js (https://github.com/scala-js/scala-js/pull/780)
     */
    final case class Test(randomState: Ref[Data], bufferState: Ref[Buffer])
        extends Random.Service
        with TestRandom.Service {

      /**
       * Clears the buffer of booleans.
       */
      val clearBooleans: UIO[Unit] =
        bufferState.update(_.copy(booleans = List.empty))

      /**
       * Clears the buffer of bytes.
       */
      val clearBytes: UIO[Unit] =
        bufferState.update(_.copy(bytes = List.empty))

      /**
       * Clears the buffer of characters.
       */
      val clearChars: UIO[Unit] =
        bufferState.update(_.copy(chars = List.empty))

      /**
       * Clears the buffer of doubles.
       */
      val clearDoubles: UIO[Unit] =
        bufferState.update(_.copy(doubles = List.empty))

      /**
       * Clears the buffer of floats.
       */
      val clearFloats: UIO[Unit] =
        bufferState.update(_.copy(floats = List.empty))

      /**
       * Clears the buffer of integers.
       */
      val clearInts: UIO[Unit] =
        bufferState.update(_.copy(integers = List.empty))

      /**
       * Clears the buffer of longs.
       */
      val clearLongs: UIO[Unit] =
        bufferState.update(_.copy(longs = List.empty))

      /**
       * Clears the buffer of strings.
       */
      val clearStrings: UIO[Unit] =
        bufferState.update(_.copy(strings = List.empty))

      /**
       * Feeds the buffer with specified sequence of booleans. The first value in
       * the sequence will be the first to be taken. These values will be taken
       * before any values that were previously in the buffer.
       */
      def feedBooleans(booleans: Boolean*): UIO[Unit] =
        bufferState.update(data => data.copy(booleans = booleans.toList ::: data.booleans))

      /**
       * Feeds the buffer with specified sequence of chunks of bytes. The first
       * value in the sequence will be the first to be taken. These values will
       * be taken before any values that were previously in the buffer.
       */
      def feedBytes(bytes: Chunk[Byte]*): UIO[Unit] =
        bufferState.update(data => data.copy(bytes = bytes.toList ::: data.bytes))

      /**
       * Feeds the buffer with specified sequence of characters. The first value
       * in the sequence will be the first to be taken. These values will be
       * taken before any values that were previously in the buffer.
       */
      def feedChars(chars: Char*): UIO[Unit] =
        bufferState.update(data => data.copy(chars = chars.toList ::: data.chars))

      /**
       * Feeds the buffer with specified sequence of doubles. The first value in
       * the sequence will be the first to be taken. These values will be taken
       * before any values that were previously in the buffer.
       */
      def feedDoubles(doubles: Double*): UIO[Unit] =
        bufferState.update(data => data.copy(doubles = doubles.toList ::: data.doubles))

      /**
       * Feeds the buffer with specified sequence of floats. The first value in
       * the sequence will be the first to be taken. These values will be taken
       * before any values that were previously in the buffer.
       */
      def feedFloats(floats: Float*): UIO[Unit] =
        bufferState.update(data => data.copy(floats = floats.toList ::: data.floats))

      /**
       * Feeds the buffer with specified sequence of integers. The first value in
       * the sequence will be the first to be taken. These values will be taken
       * before any values that were previously in the buffer.
       */
      def feedInts(ints: Int*): UIO[Unit] =
        bufferState.update(data => data.copy(integers = ints.toList ::: data.integers))

      /**
       * Feeds the buffer with specified sequence of longs. The first value in
       * the sequence will be the first to be taken. These values will be taken
       * before any values that were previously in the buffer.
       */
      def feedLongs(longs: Long*): UIO[Unit] =
        bufferState.update(data => data.copy(longs = longs.toList ::: data.longs))

      /**
       * Feeds the buffer with specified sequence of strings. The first value in
       * the sequence will be the first to be taken. These values will be taken
       * before any values that were previously in the buffer.
       */
      def feedStrings(strings: String*): UIO[Unit] =
        bufferState.update(data => data.copy(strings = strings.toList ::: data.strings))

      /**
       * Gets the seed of this `TestRandom`.
       */
      val getSeed: UIO[Long] =
        randomState.get.map { case Data(seed1, seed2, _) =>
          ((seed1.toLong << 24) | seed2) ^ 0x5deece66dL
        }

      /**
       * Takes a boolean from the buffer if one exists or else generates a
       * pseudo-random boolean.
       */
      lazy val nextBoolean: UIO[Boolean] =
        getOrElse(bufferedBoolean)(randomBoolean)

      /**
       * Takes a chunk of bytes from the buffer if one exists or else generates a
       * pseudo-random chunk of bytes of the specified length.
       */
      def nextBytes(length: Int): UIO[Chunk[Byte]] =
        getOrElse(bufferedBytes)(randomBytes(length))

      /**
       * Takes a double from the buffer if one exists or else generates a
       * pseudo-random, uniformly distributed double between 0.0 and 1.0.
       */
      lazy val nextDouble: UIO[Double] =
        getOrElse(bufferedDouble)(randomDouble)

      /**
       * Takes a double from the buffer if one exists or else generates a
       * pseudo-random double in the specified range.
       */
      def nextDoubleBetween(minInclusive: Double, maxExclusive: Double): UIO[Double] =
        getOrElse(bufferedDouble)(randomDoubleBetween(minInclusive, maxExclusive))

      /**
       * Takes a float from the buffer if one exists or else generates a
       * pseudo-random, uniformly distributed float between 0.0 and 1.0.
       */
      lazy val nextFloat: UIO[Float] =
        getOrElse(bufferedFloat)(randomFloat)

      /**
       * Takes a float from the buffer if one exists or else generates a
       * pseudo-random float in the specified range.
       */
      def nextFloatBetween(minInclusive: Float, maxExclusive: Float): UIO[Float] =
        getOrElse(bufferedFloat)(randomFloatBetween(minInclusive, maxExclusive))

      /**
       * Takes a double from the buffer if one exists or else generates a
       * pseudo-random double from a normal distribution with mean 0.0 and
       * standard deviation 1.0.
       */
      lazy val nextGaussian: UIO[Double] =
        getOrElse(bufferedDouble)(randomGaussian)

      /**
       * Takes an integer from the buffer if one exists or else generates a
       * pseudo-random integer.
       */
      lazy val nextInt: UIO[Int] =
        getOrElse(bufferedInt)(randomInt)

      /**
       * Takes an integer from the buffer if one exists or else generates a
       * pseudo-random integer in the specified range.
       */
      def nextIntBetween(minInclusive: Int, maxExclusive: Int): UIO[Int] =
        getOrElse(bufferedInt)(randomIntBetween(minInclusive, maxExclusive))

      /**
       * Takes an integer from the buffer if one exists or else generates a
       * pseudo-random integer between 0 (inclusive) and the specified value
       * (exclusive).
       */
      def nextIntBounded(n: Int): UIO[Int] =
        getOrElse(bufferedInt)(randomIntBounded(n))

      /**
       * Takes a long from the buffer if one exists or else generates a
       * pseudo-random long.
       */
      lazy val nextLong: UIO[Long] =
        getOrElse(bufferedLong)(randomLong)

      /**
       * Takes a long from the buffer if one exists or else generates a
       * pseudo-random long in the specified range.
       */
      def nextLongBetween(minInclusive: Long, maxExclusive: Long): UIO[Long] =
        getOrElse(bufferedLong)(randomLongBetween(minInclusive, maxExclusive))

      /**
       * Takes a long from the buffer if one exists or else generates a
       * pseudo-random long between 0 (inclusive) and the specified value
       * (exclusive).
       */
      def nextLongBounded(n: Long): UIO[Long] =
        getOrElse(bufferedLong)(randomLongBounded(n))

      /**
       * Takes a character from the buffer if one exists or else generates a
       * pseudo-random character from the ASCII range 33-126.
       */
      lazy val nextPrintableChar: UIO[Char] =
        getOrElse(bufferedChar)(randomPrintableChar)

      /**
       * Takes a string from the buffer if one exists or else generates a
       * pseudo-random string of the specified length.
       */
      def nextString(length: Int): UIO[String] =
        getOrElse(bufferedString)(randomString(length))

      /**
       * Saves the `TestRandom`'s current state in an effect which, when run,
       * will restore the `TestRandom` state to the saved state.
       */
      val save: UIO[UIO[Unit]] =
        for {
          randomData <- randomState.get
          bufferData <- bufferState.get
        } yield randomState.set(randomData) *> bufferState.set(bufferData)

      /**
       * Sets the seed of this `TestRandom` to the specified value.
       */
      def setSeed(seed: Long): UIO[Unit] =
        randomState.set {
          val newSeed = (seed ^ 0x5deece66dL) & ((1L << 48) - 1)
          val seed1   = (newSeed >>> 24).toInt
          val seed2   = newSeed.toInt & ((1 << 24) - 1)
          Data(seed1, seed2, Queue.empty)
        }

      /**
       * Randomly shuffles the specified list.
       */
      def shuffle[A, Collection[+Element] <: Iterable[Element]](
        list: Collection[A]
      )(implicit bf: BuildFrom[Collection[A], A, Collection[A]]): UIO[Collection[A]] =
        Random.shuffleWith(randomIntBounded, list)

      private def bufferedBoolean(buffer: Buffer): (Option[Boolean], Buffer) =
        (
          buffer.booleans.headOption,
          buffer.copy(booleans = buffer.booleans.drop(1))
        )

      private def bufferedBytes(buffer: Buffer): (Option[Chunk[Byte]], Buffer) =
        (
          buffer.bytes.headOption,
          buffer.copy(bytes = buffer.bytes.drop(1))
        )

      private def bufferedChar(buffer: Buffer): (Option[Char], Buffer) =
        (
          buffer.chars.headOption,
          buffer.copy(chars = buffer.chars.drop(1))
        )

      private def bufferedDouble(buffer: Buffer): (Option[Double], Buffer) =
        (
          buffer.doubles.headOption,
          buffer.copy(doubles = buffer.doubles.drop(1))
        )

      private def bufferedFloat(buffer: Buffer): (Option[Float], Buffer) =
        (
          buffer.floats.headOption,
          buffer.copy(floats = buffer.floats.drop(1))
        )

      private def bufferedInt(buffer: Buffer): (Option[Int], Buffer) =
        (
          buffer.integers.headOption,
          buffer.copy(integers = buffer.integers.drop(1))
        )

      private def bufferedLong(buffer: Buffer): (Option[Long], Buffer) =
        (
          buffer.longs.headOption,
          buffer.copy(longs = buffer.longs.drop(1))
        )

      private def bufferedString(buffer: Buffer): (Option[String], Buffer) =
        (
          buffer.strings.headOption,
          buffer.copy(strings = buffer.strings.drop(1))
        )

      private def getOrElse[A](buffer: Buffer => (Option[A], Buffer))(random: UIO[A]): UIO[A] =
        bufferState.modify(buffer).flatMap(_.fold(random)(UIO.succeedNow))

      @inline
      private def leastSignificantBits(x: Double): Int =
        toInt(x) & ((1 << 24) - 1)

      @inline
      private def mostSignificantBits(x: Double): Int =
        toInt((x / (1 << 24).toDouble))

      private def randomBits(bits: Int): UIO[Int] =
        randomState.modify { data =>
          val multiplier  = 0x5deece66dL
          val multiplier1 = (multiplier >>> 24).toInt
          val multiplier2 = multiplier.toInt & ((1 << 24) - 1)
          val product1    = data.seed2.toDouble * multiplier1.toDouble + data.seed1.toDouble * multiplier2.toDouble
          val product2    = data.seed2.toDouble * multiplier2.toDouble + 0xb
          val newSeed1    = (mostSignificantBits(product2) + leastSignificantBits(product1)) & ((1 << 24) - 1)
          val newSeed2    = leastSignificantBits(product2)
          val result      = (newSeed1 << 8) | (newSeed2 >> 16)
          (result >>> (32 - bits), Data(newSeed1, newSeed2, data.nextNextGaussians))
        }

      private val randomBoolean: UIO[Boolean] =
        randomBits(1).map(_ != 0)

      private def randomBytes(length: Int): UIO[Chunk[Byte]] = {
        //  Our RNG generates 32 bit integers so to maximize efficiency we want to
        //  pull 8 bit bytes from the current integer until it is exhausted
        //  before generating another random integer
        def loop(i: Int, rnd: UIO[Int], n: Int, acc: UIO[List[Byte]]): UIO[List[Byte]] =
          if (i == length)
            acc.map(_.reverse)
          else if (n > 0)
            rnd.flatMap(rnd => loop(i + 1, UIO.succeedNow(rnd >> 8), n - 1, acc.map(rnd.toByte :: _)))
          else
            loop(i, nextInt, (length - i) min 4, acc)

        loop(0, randomInt, length min 4, UIO.succeedNow(List.empty[Byte])).map(Chunk.fromIterable)
      }

      private val randomDouble: UIO[Double] =
        for {
          i1 <- randomBits(26)
          i2 <- randomBits(27)
        } yield ((i1.toDouble * (1L << 27).toDouble) + i2.toDouble) / (1L << 53).toDouble

      private def randomDoubleBetween(minInclusive: Double, maxExclusive: Double): UIO[Double] =
        Random.nextDoubleBetweenWith(minInclusive, maxExclusive)(randomDouble)

      private val randomFloat: UIO[Float] =
        randomBits(24).map(i => (i.toDouble / (1 << 24).toDouble).toFloat)

      private def randomFloatBetween(minInclusive: Float, maxExclusive: Float): UIO[Float] =
        Random.nextFloatBetweenWith(minInclusive, maxExclusive)(randomFloat)

      private val randomGaussian: UIO[Double] =
        //  The Box-Muller transform generates two normally distributed random
        //  doubles, so we store the second double in a queue and check the
        //  queue before computing a new pair of values to avoid wasted work.
        randomState.modify { case Data(seed1, seed2, queue) =>
          queue.dequeueOption.fold((Option.empty[Double], Data(seed1, seed2, queue))) { case (d, queue) =>
            (Some(d), Data(seed1, seed2, queue))
          }
        }.flatMap {
          case Some(nextNextGaussian) => UIO.succeedNow(nextNextGaussian)
          case None =>
            def loop: UIO[(Double, Double, Double)] =
              randomDouble.zip(randomDouble).flatMap { case (d1, d2) =>
                val x      = 2 * d1 - 1
                val y      = 2 * d2 - 1
                val radius = x * x + y * y
                if (radius >= 1 || radius == 0) loop else UIO.succeedNow((x, y, radius))
              }
            loop.flatMap { case (x, y, radius) =>
              val c = sqrt(-2 * log(radius) / radius)
              randomState.modify { case Data(seed1, seed2, queue) =>
                (x * c, Data(seed1, seed2, queue.enqueue(y * c)))
              }
            }
        }

      private val randomInt: UIO[Int] =
        randomBits(32)

      private def randomIntBounded(n: Int): UIO[Int] =
        if (n <= 0)
          UIO.die(new IllegalArgumentException("n must be positive"))
        else if ((n & -n) == n)
          randomBits(31).map(_ >> Integer.numberOfLeadingZeros(n))
        else {
          def loop: UIO[Int] =
            randomBits(31).flatMap { i =>
              val value = i % n
              if (i - value + (n - 1) < 0) loop
              else UIO.succeedNow(value)
            }
          loop
        }

      private def randomIntBetween(minInclusive: Int, maxExclusive: Int): UIO[Int] =
        Random.nextIntBetweenWith(minInclusive, maxExclusive)(randomInt, randomIntBounded)

      private val randomLong: UIO[Long] =
        for {
          i1 <- randomBits(32)
          i2 <- randomBits(32)
        } yield ((i1.toLong << 32) + i2)

      private def randomLongBounded(n: Long): UIO[Long] =
        Random.nextLongBoundedWith(n)(randomLong)

      private def randomLongBetween(minInclusive: Long, maxExclusive: Long): UIO[Long] =
        Random.nextLongBetweenWith(minInclusive, maxExclusive)(randomLong, randomLongBounded)

      private val randomPrintableChar: UIO[Char] =
        randomIntBounded(127 - 33).map(i => (i + 33).toChar)

      private def randomString(length: Int): UIO[String] = {
        val safeChar = randomIntBounded(0xd800 - 1).map(i => (i + 1).toChar)
        UIO.collectAll(List.fill(length)(safeChar)).map(_.mkString)
      }

      @inline
      private def toInt(x: Double): Int =
        (x.asInstanceOf[Long] | 0.asInstanceOf[Long]).asInstanceOf[Int]
    }

    /**
     * An arbitrary initial seed for the `TestRandom`.
     */
    val DefaultData: Data = Data(1071905196, 1911589680)

    /**
     * The seed of the `TestRandom`.
     */
    final case class Data(
      seed1: Int,
      seed2: Int,
      private[TestRandom] val nextNextGaussians: Queue[Double] = Queue.empty
    )

    /**
     * Accesses a `TestRandom` instance in the environment and clears the buffer
     * of booleans.
     */
    val clearBooleans: URIO[TestRandom, Unit] =
      ZIO.accessM(_.get.clearBooleans)

    /**
     * Accesses a `TestRandom` instance in the environment and clears the buffer
     * of bytes.
     */
    val clearBytes: URIO[TestRandom, Unit] =
      ZIO.accessM(_.get.clearBytes)

    /**
     * Accesses a `TestRandom` instance in the environment and clears the buffer
     * of characters.
     */
    val clearChars: URIO[TestRandom, Unit] =
      ZIO.accessM(_.get.clearChars)

    /**
     * Accesses a `TestRandom` instance in the environment and clears the buffer
     * of doubles.
     */
    val clearDoubles: URIO[TestRandom, Unit] =
      ZIO.accessM(_.get.clearDoubles)

    /**
     * Accesses a `TestRandom` instance in the environment and clears the buffer
     * of floats.
     */
    val clearFloats: URIO[TestRandom, Unit] =
      ZIO.accessM(_.get.clearFloats)

    /**
     * Accesses a `TestRandom` instance in the environment and clears the buffer
     * of integers.
     */
    val clearInts: URIO[TestRandom, Unit] =
      ZIO.accessM(_.get.clearInts)

    /**
     * Accesses a `TestRandom` instance in the environment and clears the buffer
     * of longs.
     */
    val clearLongs: URIO[TestRandom, Unit] =
      ZIO.accessM(_.get.clearLongs)

    /**
     * Accesses a `TestRandom` instance in the environment and clears the buffer
     * of strings.
     */
    val clearStrings: URIO[TestRandom, Unit] =
      ZIO.accessM(_.get.clearStrings)

    /**
     * Accesses a `TestRandom` instance in the environment and feeds the buffer
     * with the specified sequence of booleans.
     */
    def feedBooleans(booleans: Boolean*): URIO[TestRandom, Unit] =
      ZIO.accessM(_.get.feedBooleans(booleans: _*))

    /**
     * Accesses a `TestRandom` instance in the environment and feeds the buffer
     * with the specified sequence of chunks of bytes.
     */
    def feedBytes(bytes: Chunk[Byte]*): URIO[TestRandom, Unit] =
      ZIO.accessM(_.get.feedBytes(bytes: _*))

    /**
     * Accesses a `TestRandom` instance in the environment and feeds the buffer
     * with the specified sequence of characters.
     */
    def feedChars(chars: Char*): URIO[TestRandom, Unit] =
      ZIO.accessM(_.get.feedChars(chars: _*))

    /**
     * Accesses a `TestRandom` instance in the environment and feeds the buffer
     * with the specified sequence of doubles.
     */
    def feedDoubles(doubles: Double*): URIO[TestRandom, Unit] =
      ZIO.accessM(_.get.feedDoubles(doubles: _*))

    /**
     * Accesses a `TestRandom` instance in the environment and feeds the buffer
     * with the specified sequence of floats.
     */
    def feedFloats(floats: Float*): URIO[TestRandom, Unit] =
      ZIO.accessM(_.get.feedFloats(floats: _*))

    /**
     * Accesses a `TestRandom` instance in the environment and feeds the buffer
     * with the specified sequence of integers.
     */
    def feedInts(ints: Int*): URIO[TestRandom, Unit] =
      ZIO.accessM(_.get.feedInts(ints: _*))

    /**
     * Accesses a `TestRandom` instance in the environment and feeds the buffer
     * with the specified sequence of longs.
     */
    def feedLongs(longs: Long*): URIO[TestRandom, Unit] =
      ZIO.accessM(_.get.feedLongs(longs: _*))

    /**
     * Accesses a `TestRandom` instance in the environment and feeds the buffer
     * with the specified sequence of strings.
     */
    def feedStrings(strings: String*): URIO[TestRandom, Unit] =
      ZIO.accessM(_.get.feedStrings(strings: _*))

    /**
     * Accesses a `TestRandom` instance in the environment and gets the seed.
     */
    val getSeed: URIO[TestRandom, Long] =
      ZIO.accessM(_.get.getSeed)

    /**
     * Constructs a new `TestRandom` with the specified initial state. This can
     * be useful for providing the required environment to an effect that
     * requires a `Random`, such as with `ZIO#provide`.
     */
    def make(data: Data): Layer[Nothing, Random with TestRandom] =
      ZLayer.fromEffectMany(for {
        data   <- Ref.make(data)
        buffer <- Ref.make(Buffer())
        test    = Test(data, buffer)
      } yield Has.allOf[Random.Service, TestRandom.Service](test, test))

    val any: ZLayer[Random with TestRandom, Nothing, Random with TestRandom] =
      ZLayer.requires[Random with TestRandom]

    val deterministic: Layer[Nothing, Random with TestRandom] =
      make(DefaultData)

    val random: ZLayer[Clock, Nothing, Random with TestRandom] =
      (ZLayer.service[Clock.Service] ++ deterministic) >>>
        (ZLayer.fromFunctionManyM { (env: Clock with Random with TestRandom) =>
          val random     = env.get[Random.Service]
          val testRandom = env.get[TestRandom.Service]

          for {
            time <- env.get[Clock.Service].nanoTime
            _    <- env.get[TestRandom.Service].setSeed(time)
          } yield Has.allOf[Random.Service, TestRandom.Service](random, testRandom)
        })

    /**
     * Constructs a new `Test` object that implements the `TestRandom` interface.
     * This can be useful for mixing in with implementations of other interfaces.
     */
    def makeTest(data: Data): UIO[Test] =
      for {
        data   <- Ref.make(data)
        buffer <- Ref.make(Buffer())
      } yield Test(data, buffer)

    /**
     * Accesses a `TestRandom` instance in the environment and saves the random
     * state in an effect which, when run, will restore the `TestRandom` to the
     * saved state.
     */
    val save: ZIO[TestRandom, Nothing, UIO[Unit]] =
      ZIO.accessM(_.get.save)

    /**
     * Accesses a `TestRandom` instance in the environment and sets the seed to
     * the specified value.
     */
    def setSeed(seed: => Long): URIO[TestRandom, Unit] =
      ZIO.accessM(_.get.setSeed(seed))

    /**
     * The buffer of the `TestRandom`.
     */
    final case class Buffer(
      booleans: List[Boolean] = List.empty,
      bytes: List[Chunk[Byte]] = List.empty,
      chars: List[Char] = List.empty,
      doubles: List[Double] = List.empty,
      floats: List[Float] = List.empty,
      integers: List[Int] = List.empty,
      longs: List[Long] = List.empty,
      strings: List[String] = List.empty
    )
  }

  /**
   * `TestSystem` supports deterministic testing of effects involving system
   * properties. Internally, `TestSystem` maintains mappings of environment
   * variables and system properties that can be set and accessed. No actual
   * environment variables or system properties will be accessed or set as a
   * result of these actions.
   *
   * {{{
   * import zio.system
   * import zio.test.environment.TestSystem
   *
   * for {
   *   _      <- TestSystem.putProperty("java.vm.name", "VM")
   *   result <- system.property("java.vm.name")
   * } yield result == Some("VM")
   * }}}
   */
  object TestSystem extends Serializable {

    trait Service extends Restorable {
      def putEnv(name: String, value: String): UIO[Unit]
      def putProperty(name: String, value: String): UIO[Unit]
      def setLineSeparator(lineSep: String): UIO[Unit]
      def clearEnv(variable: String): UIO[Unit]
      def clearProperty(prop: String): UIO[Unit]
    }

    final case class Test(systemState: Ref[TestSystem.Data]) extends System.Service with TestSystem.Service {

      /**
       * Returns the specified environment variable if it exists.
       */
      def env(variable: String): IO[SecurityException, Option[String]] =
        systemState.get.map(_.envs.get(variable))

      /**
       * Returns the specified environment variable if it exists or else the
       * specified fallback value.
       */
      def envOrElse(variable: String, alt: => String): IO[SecurityException, String] =
        System.envOrElseWith(variable, alt)(env)

      /**
       * Returns the specified environment variable if it exists or else the
       * specified optional fallback value.
       */
      def envOrOption(variable: String, alt: => Option[String]): IO[SecurityException, Option[String]] =
        System.envOrOptionWith(variable, alt)(env)

      val envs: ZIO[Any, SecurityException, Map[String, String]] =
        systemState.get.map(_.envs)

      /**
       * Returns the system line separator.
       */
      val lineSeparator: UIO[String] =
        systemState.get.map(_.lineSeparator)

      val properties: ZIO[Any, Throwable, Map[String, String]] =
        systemState.get.map(_.properties)

      /**
       * Returns the specified system property if it exists.
       */
      def property(prop: String): IO[Throwable, Option[String]] =
        systemState.get.map(_.properties.get(prop))

      /**
       * Returns the specified system property if it exists or else the
       * specified fallback value.
       */
      def propertyOrElse(prop: String, alt: => String): IO[Throwable, String] =
        System.propertyOrElseWith(prop, alt)(property)

      /**
       * Returns the specified system property if it exists or else the
       * specified optional fallback value.
       */
      def propertyOrOption(prop: String, alt: => Option[String]): IO[Throwable, Option[String]] =
        System.propertyOrOptionWith(prop, alt)(property)

      /**
       * Adds the specified name and value to the mapping of environment
       * variables maintained by this `TestSystem`.
       */
      def putEnv(name: String, value: String): UIO[Unit] =
        systemState.update(data => data.copy(envs = data.envs.updated(name, value)))

      /**
       * Adds the specified name and value to the mapping of system properties
       * maintained by this `TestSystem`.
       */
      def putProperty(name: String, value: String): UIO[Unit] =
        systemState.update(data => data.copy(properties = data.properties.updated(name, value)))

      /**
       * Sets the system line separator maintained by this `TestSystem` to the
       * specified value.
       */
      def setLineSeparator(lineSep: String): UIO[Unit] =
        systemState.update(_.copy(lineSeparator = lineSep))

      /**
       * Clears the mapping of environment variables.
       */
      def clearEnv(variable: String): UIO[Unit] =
        systemState.update(data => data.copy(envs = data.envs - variable))

      /**
       * Clears the mapping of system properties.
       */
      def clearProperty(prop: String): UIO[Unit] =
        systemState.update(data => data.copy(properties = data.properties - prop))

      /**
       * Saves the `TestSystem``'s current state in an effect which, when run, will restore the `TestSystem`
       * state to the saved state.
       */
      val save: UIO[UIO[Unit]] =
        for {
          systemData <- systemState.get
        } yield systemState.set(systemData)
    }

    /**
     * The default initial state of the `TestSystem` with no environment variable
     * or system property mappings and the system line separator set to the new
     * line character.
     */
    val DefaultData: Data = Data(Map(), Map(), "\n")

    /**
     * Constructs a new `TestSystem` with the specified initial state. This can
     * be useful for providing the required environment to an effect that
     * requires a `Console`, such as with `ZIO#provide`.
     */
    def live(data: Data): Layer[Nothing, System with TestSystem] =
      ZLayer.fromEffectMany(
        Ref.make(data).map(ref => Has.allOf[System.Service, TestSystem.Service](Test(ref), Test(ref)))
      )

    val any: ZLayer[System with TestSystem, Nothing, System with TestSystem] =
      ZLayer.requires[System with TestSystem]

    val default: Layer[Nothing, System with TestSystem] =
      live(DefaultData)

    /**
     * Accesses a `TestSystem` instance in the environment and adds the specified
     * name and value to the mapping of environment variables.
     */
    def putEnv(name: => String, value: => String): URIO[TestSystem, Unit] =
      ZIO.accessM(_.get.putEnv(name, value))

    /**
     * Accesses a `TestSystem` instance in the environment and adds the specified
     * name and value to the mapping of system properties.
     */
    def putProperty(name: => String, value: => String): URIO[TestSystem, Unit] =
      ZIO.accessM(_.get.putProperty(name, value))

    /**
     * Accesses a `TestSystem` instance in the environment and saves the system state in an effect which, when run,
     * will restore the `TestSystem` to the saved state
     */
    val save: ZIO[TestSystem, Nothing, UIO[Unit]] =
      ZIO.accessM(_.get.save)

    /**
     * Accesses a `TestSystem` instance in the environment and sets the line
     * separator to the specified value.
     */
    def setLineSeparator(lineSep: => String): URIO[TestSystem, Unit] =
      ZIO.accessM(_.get.setLineSeparator(lineSep))

    /**
     * Accesses a `TestSystem` instance in the environment and clears the mapping
     * of environment variables.
     */
    def clearEnv(variable: => String): URIO[TestSystem, Unit] =
      ZIO.accessM(_.get.clearEnv(variable))

    /**
     * Accesses a `TestSystem` instance in the environment and clears the mapping
     * of system properties.
     */
    def clearProperty(prop: => String): URIO[TestSystem, Unit] =
      ZIO.accessM(_.get.clearProperty(prop))

    /**
     * The state of the `TestSystem`.
     */
    final case class Data(
      properties: Map[String, String] = Map.empty,
      envs: Map[String, String] = Map.empty,
      lineSeparator: String = "\n"
    )
  }
}

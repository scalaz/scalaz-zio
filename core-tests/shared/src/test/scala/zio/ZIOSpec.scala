package zio

import zio.Cause._
import zio.LatchOps._
import zio.internal.Platform
import zio.test.Assertion._
import zio.test.TestAspect.{flaky, forked, ignore, jvm, jvmOnly, nonFlaky, scala2Only}
import zio.test._
import zio.test.environment.{Live, TestClock}

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

object ZIOSpec extends ZIOBaseSpec {

  import ZIOTag._

  def spec: ZSpec[Environment, Failure] = suite("ZIOSpec")(
    suite("***")(
      testM("splits the environment") {
        val zio1 = ZIO.fromFunction((n: Int) => n + 2)
        val zio2 = ZIO.fromFunction((n: Int) => n * 3)
        val zio3 = zio1 *** zio2
        assertM(zio3.provide((4, 5)))(equalTo((6, 15)))
      }
    ),
    suite("absorbWith")(
      testM("on fail") {
        assertM(TaskExampleError.absorbWith(identity).exit)(fails(equalTo(ExampleError)))
      },
      testM("on die") {
        assertM(TaskExampleDie.absorbWith(identity).exit)(fails(equalTo(ExampleError)))
      },
      testM("on success") {
        assertM(ZIO.succeed(1).absorbWith(_ => ExampleError))(equalTo(1))
      }
    ) @@ zioTag(errors),
    suite("acquireReleaseWith")(
      testM("acquireReleaseWith happy path") {
        for {
          release <- Ref.make(false)
          result <-
            ZIO.acquireReleaseWith(IO.succeed(42), (_: Int) => release.set(true), (a: Int) => ZIO.succeed(a + 1))
          released <- release.get
        } yield assert(result)(equalTo(43)) && assert(released)(isTrue)
      },
      testM("acquireRelease happy path") {
        for {
          release  <- Ref.make(false)
          result   <- IO.succeed(42).acquireRelease(release.set(true), ZIO.succeed(0))
          released <- release.get
        } yield assert(result)(equalTo(0)) && assert(released)(isTrue)
      },
      testM("acquireReleaseExitWith happy path") {
        for {
          release <- Ref.make(false)
          result <- ZIO.acquireReleaseExitWith(
                      IO.succeed(42),
                      (_: Int, _: Exit[Any, Any]) => release.set(true),
                      (_: Int) => IO.succeed(0L)
                    )
          released <- release.get
        } yield assert(result)(equalTo(0L)) && assert(released)(isTrue)
      },
      testM("acquireReleaseExitWith error handling") {
        val releaseDied: Throwable = new RuntimeException("release died")
        for {
          exit <- ZIO
                    .acquireReleaseExitWith[Any, String, Int, Int](
                      ZIO.succeed(42),
                      (_, _) => ZIO.die(releaseDied),
                      _ => ZIO.fail("use failed")
                    )
                    .exit
          cause <- exit.foldZIO(cause => ZIO.succeed(cause), _ => ZIO.fail("effect should have failed"))
        } yield assert(cause.failures)(equalTo(List("use failed"))) &&
          assert(cause.defects)(equalTo(List(releaseDied)))
      } @@ zioTag(errors)
    ),
    suite("acquireReleaseWith + disconnect")(
      testM("acquireReleaseWith happy path") {
        for {
          release <- Ref.make(false)
          result <- ZIO
                      .acquireReleaseWith(IO.succeed(42), (_: Int) => release.set(true), (a: Int) => ZIO.succeed(a + 1))
                      .disconnect
          released <- release.get
        } yield assert(result)(equalTo(43)) && assert(released)(isTrue)
      },
      testM("acquireRelease happy path") {
        for {
          release  <- Ref.make(false)
          result   <- IO.succeed(42).acquireRelease(release.set(true), ZIO.succeed(0)).disconnect
          released <- release.get
        } yield assert(result)(equalTo(0)) && assert(released)(isTrue)
      },
      testM("acquireReleaseExitWith happy path") {
        for {
          release <- Ref.make(false)
          result <- ZIO
                      .acquireReleaseExitWith(
                        IO.succeed(42),
                        (_: Int, _: Exit[Any, Any]) => release.set(true),
                        (_: Int) => IO.succeed(0L)
                      )
                      .disconnect
          released <- release.get
        } yield assert(result)(equalTo(0L)) && assert(released)(isTrue)
      },
      testM("acquireReleaseExitWith error handling") {
        val releaseDied: Throwable = new RuntimeException("release died")
        for {
          exit <- ZIO
                    .acquireReleaseExitWith[Any, String, Int, Int](
                      ZIO.succeed(42),
                      (_, _) => ZIO.die(releaseDied),
                      _ => ZIO.fail("use failed")
                    )
                    .disconnect
                    .exit
          cause <- exit.foldZIO(cause => ZIO.succeed(cause), _ => ZIO.fail("effect should have failed"))
        } yield assert(cause.failures)(equalTo(List("use failed"))) &&
          assert(cause.defects)(equalTo(List(releaseDied)))
      } @@ zioTag(errors),
      testM("acquireReleaseExitWith beast mode error handling") {
        val releaseDied: Throwable = new RuntimeException("release died")
        for {
          released <- ZRef.make(false)
          exit <- ZIO
                    .acquireReleaseExitWith[Any, String, Int, Int](
                      ZIO.succeed(42),
                      (_, _) => released.set(true),
                      _ => throw releaseDied
                    )
                    .disconnect
                    .exit
          cause      <- exit.foldZIO(cause => ZIO.succeed(cause), _ => ZIO.fail("effect should have failed"))
          isReleased <- released.get
        } yield assert(cause.defects)(equalTo(List(releaseDied))) && assert(isReleased)(isTrue)
      } @@ zioTag(errors)
    ),
    suite("cached")(
      testM("returns new instances after duration") {
        def incrementAndGet(ref: Ref[Int]): UIO[Int] = ref.updateAndGet(_ + 1)
        for {
          ref   <- Ref.make(0)
          cache <- incrementAndGet(ref).cached(60.minutes)
          a     <- cache
          _     <- TestClock.adjust(59.minutes)
          b     <- cache
          _     <- TestClock.adjust(1.minute)
          c     <- cache
          _     <- TestClock.adjust(59.minutes)
          d     <- cache
        } yield assert(a)(equalTo(b)) && assert(b)(not(equalTo(c))) && assert(c)(equalTo(d))
      },
      testM("correctly handles an infinite duration time to live") {
        for {
          ref            <- Ref.make(0)
          getAndIncrement = ref.modify(curr => (curr, curr + 1))
          cached         <- getAndIncrement.cached(Duration.Infinity)
          a              <- cached
          b              <- cached
          c              <- cached
        } yield assert((a, b, c))(equalTo((0, 0, 0)))
      }
    ),
    suite("cachedInvalidate")(
      testM("returns new instances after duration") {
        def incrementAndGet(ref: Ref[Int]): UIO[Int] = ref.updateAndGet(_ + 1)
        for {
          ref                 <- Ref.make(0)
          tuple               <- incrementAndGet(ref).cachedInvalidate(60.minutes)
          (cached, invalidate) = tuple
          a                   <- cached
          _                   <- TestClock.adjust(59.minutes)
          b                   <- cached
          _                   <- invalidate
          c                   <- cached
          _                   <- TestClock.adjust(1.minute)
          d                   <- cached
          _                   <- TestClock.adjust(59.minutes)
          e                   <- cached
        } yield assert(a)(equalTo(b)) &&
          assert(b)(not(equalTo(c))) &&
          assert(c)(equalTo(d)) &&
          assert(d)(not(equalTo(e)))
      }
    ),
    suite("catchAllDefect")(
      testM("recovers from all defects") {
        val s   = "division by zero"
        val zio = ZIO.die(new IllegalArgumentException(s))
        for {
          result <- zio.catchAllDefect(e => ZIO.succeed(e.getMessage))
        } yield assert(result)(equalTo(s))
      },
      testM("leaves errors") {
        val t   = new IllegalArgumentException("division by zero")
        val zio = ZIO.fail(t)
        for {
          exit <- zio.catchAllDefect(e => ZIO.succeed(e.getMessage)).exit
        } yield assert(exit)(fails(equalTo(t)))
      },
      testM("leaves values") {
        val t   = new IllegalArgumentException("division by zero")
        val zio = ZIO.succeed(t)
        for {
          result <- zio.catchAllDefect(e => ZIO.succeed(e.getMessage))
        } yield assert(result)((equalTo(t)))
      }
    ) @@ zioTag(errors),
    suite("catchSomeCause")(
      testM("catches matching cause") {
        ZIO.interrupt.catchSomeCause {
          case c if c.interrupted => ZIO.succeed(true)
        }.sandbox.map(
          assert(_)(isTrue)
        )
      },
      testM("fails if cause doesn't match") {
        ZIO.fiberId.flatMap { fiberId =>
          ZIO.interrupt.catchSomeCause {
            case c if (!c.interrupted) => ZIO.succeed(true)
          }.sandbox.either.map(
            assert(_)(isLeft(equalTo(Cause.interrupt(fiberId))))
          )
        }
      }
    ) @@ zioTag(errors),
    suite("catchSomeDefect")(
      testM("recovers from some defects") {
        val s   = "division by zero"
        val zio = ZIO.die(new IllegalArgumentException(s))
        for {
          result <- zio.catchSomeDefect { case e: IllegalArgumentException =>
                      ZIO.succeed(e.getMessage)
                    }
        } yield assert(result)(equalTo(s))
      },
      testM("leaves the rest") {
        val t   = new IllegalArgumentException("division by zero")
        val zio = ZIO.die(t)
        for {
          exit <- zio.catchSomeDefect { case e: NumberFormatException =>
                    ZIO.succeed(e.getMessage)
                  }.exit
        } yield assert(exit)(dies(equalTo(t)))
      },
      testM("leaves errors") {
        val t   = new IllegalArgumentException("division by zero")
        val zio = ZIO.fail(t)
        for {
          exit <- zio.catchSomeDefect { case e: IllegalArgumentException =>
                    ZIO.succeed(e.getMessage)
                  }.exit
        } yield assert(exit)(fails(equalTo(t)))
      },
      testM("leaves values") {
        val t   = new IllegalArgumentException("division by zero")
        val zio = ZIO.succeed(t)
        for {
          result <- zio.catchSomeDefect { case e: IllegalArgumentException =>
                      ZIO.succeed(e.getMessage)
                    }
        } yield assert(result)((equalTo(t)))
      }
    ) @@ zioTag(errors),
    suite("collect")(
      testM("returns failure ignoring value") {
        val goodCase =
          exactlyOnce(0)(_.collect(s"value was not 0")({ case v @ 0 => v })).sandbox.either

        val badCase =
          exactlyOnce(1)(_.collect(s"value was not 0")({ case v @ 0 => v })).sandbox.either
            .map(_.left.map(_.failureOrCause))

        assertM(goodCase)(isRight(equalTo(0))) &&
        assertM(badCase)(isLeft(isLeft(equalTo("value was not 0"))))
      }
    ),
    suite("collectAllPar")(
      testM("returns the list in the same order") {
        val list = List(1, 2, 3).map(IO.succeed[Int](_))
        val res  = IO.collectAllPar(list)
        assertM(res)(equalTo(List(1, 2, 3)))
      },
      testM("is referentially transparent") {
        for {
          counter <- Ref.make(0)
          op       = counter.getAndUpdate(_ + 1)
          ops3     = ZIO.collectAllPar(List(op, op, op))
          ops6     = ops3.zipPar(ops3)
          res     <- ops6
        } yield assert(res._1)(not(equalTo(res._2)))
      }
    ),
    suite("collectAllParN")(
      testM("returns results in the same order") {
        val list = List(1, 2, 3).map(IO.succeed[Int](_))
        val res  = IO.collectAllParN(2)(list)
        assertM(res)(equalTo(List(1, 2, 3)))
      }
    ),
    suite("collectAllParNDiscard")(
      testM("preserves failures") {
        val tasks = List.fill(10)(ZIO.fail(new RuntimeException))
        assertM(ZIO.collectAllParNDiscard(5)(tasks).flip)(anything)
      }
    ),
    suite("collectFirst")(
      testM("collects the first value for which the effectual functions returns Some") {
        checkM(Gen.listOf(Gen.anyInt), Gen.partialFunction(Gen.anyInt)) { (as, pf) =>
          def f(n: Int): UIO[Option[Int]] = UIO.succeed(pf.lift(n))
          assertM(ZIO.collectFirst(as)(f))(equalTo(as.collectFirst(pf)))
        }
      }
    ),
    suite("collectZIO")(
      testM("returns failure ignoring value") {
        val goodCase =
          exactlyOnce(0)(
            _.collectZIO[Any, String, Int]("Predicate failed!")({ case v @ 0 => ZIO.succeed(v) })
          ).sandbox.either

        val partialBadCase =
          exactlyOnce(0)(
            _.collectZIO("Predicate failed!")({ case v @ 0 => ZIO.fail("Partial failed!") })
          ).sandbox.either
            .map(_.left.map(_.failureOrCause))

        val badCase =
          exactlyOnce(1)(_.collectZIO("Predicate failed!")({ case v @ 0 => ZIO.succeed(v) })).sandbox.either
            .map(_.left.map(_.failureOrCause))

        assertM(goodCase)(isRight(equalTo(0))) &&
        assertM(partialBadCase)(isLeft(isLeft(equalTo("Partial failed!")))) &&
        assertM(badCase)(isLeft(isLeft(equalTo("Predicate failed!"))))
      }
    ),
    suite("companion object method consistency")(
      testM("absolve") {
        checkM(Gen.alphaNumericString) { str =>
          val ioEither: UIO[Either[Nothing, String]] = IO.succeed(Right(str))
          for {
            abs1 <- ioEither.absolve
            abs2 <- IO.absolve(ioEither)
          } yield assert(abs1)(equalTo(abs2))
        }
      },
      testM("firstSuccessOf") {
        val io  = IO.succeed(testString)
        val ios = List.empty[UIO[String]]
        for {
          race1 <- io.firstSuccessOf(ios)
          race2 <- IO.firstSuccessOf(io, ios)
        } yield assert(race1)(equalTo(race2))
      },
      testM("flatten") {
        checkM(Gen.alphaNumericString) { str =>
          for {
            flatten1 <- IO.succeed(IO.succeed(str)).flatten
            flatten2 <- IO.flatten(IO.succeed(IO.succeed(str)))
          } yield assert(flatten1)(equalTo(flatten2))
        }
      },
      testM("raceAll") {
        val io  = IO.succeed(testString)
        val ios = List.empty[UIO[String]]
        for {
          race1 <- io.raceAll(ios)
          race2 <- IO.raceAll(io, ios)
        } yield assert(race1)(equalTo(race2))
      }
    ),
    suite("done")(
      testM("Check done lifts exit result into IO") {

        val fiberId                       = Fiber.Id(0L, 123L)
        val error                         = exampleError
        val completed                     = Exit.succeed(1)
        val interrupted: Exit[Error, Int] = Exit.interrupt(fiberId)
        val terminated: Exit[Error, Int]  = Exit.die(error)
        val failed: Exit[Error, Int]      = Exit.fail(error)

        assertM(IO.done(completed))(equalTo(1)) &&
        assertM(IO.done(interrupted).exit)(isInterrupted) &&
        assertM(IO.done(terminated).exit)(dies(equalTo(error))) &&
        assertM(IO.done(failed).exit)(fails(equalTo(error)))
      }
    ),
    suite("executor")(
      testM("retrieves the current executor for this effect") {
        val executor = internal.Executor.fromExecutionContext(100) {
          scala.concurrent.ExecutionContext.Implicits.global
        }
        for {
          default <- ZIO.executor
          global  <- ZIO.executor.lock(executor)
        } yield assert(default)(not(equalTo(global)))
      }
    ),
    suite("repeatUntil")(
      testM("repeatUntil repeats until condition is true") {
        for {
          in     <- Ref.make(10)
          out    <- Ref.make(0)
          _      <- (in.updateAndGet(_ - 1) <* out.update(_ + 1)).repeatUntil(_ == 0)
          result <- out.get
        } yield assert(result)(equalTo(10))
      },
      testM("repeatUntil always evaluates effect at least once") {
        for {
          ref    <- Ref.make(0)
          _      <- ref.update(_ + 1).repeatUntil(_ => true)
          result <- ref.get
        } yield assert(result)(equalTo(1))
      }
    ),
    suite("repeatUntilEquals")(
      testM("repeatUntilEquals repeats until result is equal to predicate") {
        for {
          q      <- Queue.unbounded[Int]
          _      <- q.offerAll(List(1, 2, 3, 4, 5, 6))
          acc    <- Ref.make(0)
          _      <- (q.take <* acc.update(_ + 1)).repeatUntilEquals(5)
          result <- acc.get
        } yield assert(result)(equalTo(5))
      }
    ),
    suite("repeatUntilZIO")(
      testM("repeatUntilZIO repeat until effectful condition is true") {
        for {
          in     <- Ref.make(10)
          out    <- Ref.make(0)
          _      <- (in.updateAndGet(_ - 1) <* out.update(_ + 1)).repeatUntilZIO(v => UIO.succeed(v == 0))
          result <- out.get
        } yield assert(result)(equalTo(10))
      },
      testM("repeatUntilZIO always evaluates effect at least once") {
        for {
          ref    <- Ref.make(0)
          _      <- ref.update(_ + 1).repeatUntilZIO(_ => UIO.succeed(true))
          result <- ref.get
        } yield assert(result)(equalTo(1))
      }
    ),
    suite("repeatWhile")(
      testM("repeatWhile repeats while condition is true") {
        for {
          in     <- Ref.make(10)
          out    <- Ref.make(0)
          _      <- (in.updateAndGet(_ - 1) <* out.update(_ + 1)).repeatWhile(_ >= 0)
          result <- out.get
        } yield assert(result)(equalTo(11))
      },
      testM("repeatWhile always evaluates effect at least once") {
        for {
          ref    <- Ref.make(0)
          _      <- ref.update(_ + 1).repeatWhile(_ => false)
          result <- ref.get
        } yield assert(result)(equalTo(1))
      }
    ),
    suite("repeatWhileEquals")(
      testM("repeatWhileEquals repeats while result equals predicate") {
        for {
          q      <- Queue.unbounded[Int]
          _      <- q.offerAll(List(0, 0, 0, 0, 1, 2))
          acc    <- Ref.make(0)
          _      <- (q.take <* acc.update(_ + 1)).repeatWhileEquals(0)
          result <- acc.get
        } yield assert(result)(equalTo(5))
      }
    ),
    suite("repeatWhileZIO")(
      testM("repeatWhileZIO repeats while condition is true") {
        for {
          in     <- Ref.make(10)
          out    <- Ref.make(0)
          _      <- (in.updateAndGet(_ - 1) <* out.update(_ + 1)).repeatWhileZIO(v => UIO.succeed(v >= 0))
          result <- out.get
        } yield assert(result)(equalTo(11))
      },
      testM("repeatWhileZIO always evaluates effect at least once") {
        for {
          ref    <- Ref.make(0)
          _      <- ref.update(_ + 1).repeatWhileZIO(_ => UIO.succeed(false))
          result <- ref.get
        } yield assert(result)(equalTo(1))
      }
    ),
    suite("eventually")(
      testM("succeeds eventually") {
        def effect(ref: Ref[Int]) =
          ref.get.flatMap(n => if (n < 10) ref.update(_ + 1) *> IO.fail("Ouch") else UIO.succeed(n))

        val test = for {
          ref <- Ref.make(0)
          n   <- effect(ref).eventually
        } yield n

        assertM(test)(equalTo(10))
      }
    ),
    suite("exists")(
      testM("determines whether any element satisfies the effectual predicate") {
        checkM(Gen.listOf(Gen.anyInt), Gen.function(Gen.boolean)) { (as, f) =>
          val actual   = IO.exists(as)(a => IO.succeed(f(a)))
          val expected = as.exists(f)
          assertM(actual)(equalTo(expected))
        }
      }
    ),
    suite("filter")(
      testM("filters a collection using an effectual predicate") {
        val as = Iterable(2, 4, 6, 3, 5, 6)
        for {
          ref     <- Ref.make(List.empty[Int])
          results <- ZIO.filter(as)(a => ref.update(a :: _).as(a % 2 == 0))
          effects <- ref.get.map(_.reverse)
        } yield assert(results)(equalTo(List(2, 4, 6, 6))) &&
          assert(effects)(equalTo(List(2, 4, 6, 3, 5, 6)))
      },
      testM("filters a set using an effectual predicate") {
        val as = Set(2, 3, 4, 5, 6, 7)
        for {
          ref     <- Ref.make(Set.empty[Int])
          results <- ZIO.filter(as)(a => ref.update(_ + a).as(a % 2 == 0))
          effects <- ref.get.map(_.map(_ + 1))
        } yield assert(results)(equalTo(Set(2, 4, 6))) &&
          assert(effects)(equalTo(Set(3, 4, 5, 6, 7, 8)))
      }
    ),
    suite("filterNot")(
      testM("filters a collection using an effectual predicate") {
        val as = Iterable(2, 4, 6, 3, 5, 6)
        for {
          ref     <- Ref.make(List.empty[Int])
          results <- ZIO.filterNot(as)(a => ref.update(a :: _).as(a % 2 == 0))
          effects <- ref.get.map(_.reverse)
        } yield assert(results)(equalTo(List(3, 5))) &&
          assert(effects)(equalTo(List(2, 4, 6, 3, 5, 6)))
      },
      testM("filters a set using an effectual predicate") {
        val as = Set(2, 3, 4, 5, 6, 7)
        for {
          ref     <- Ref.make(Set.empty[Int])
          results <- ZIO.filterNot(as)(a => ref.update(_ + a).as(a % 2 == 0))
          effects <- ref.get.map(_.map(_ + 1))
        } yield assert(results)(equalTo(Set(3, 5, 7))) &&
          assert(effects)(equalTo(Set(3, 4, 5, 6, 7, 8)))
      }
    ),
    suite("filterPar")(
      testM("filters a collection in parallel using an effectual predicate") {
        val as = Iterable(2, 4, 6, 3, 5, 6, 10, 11, 15, 17, 20, 22, 23, 25, 28)
        for {
          results <- ZIO.filterPar(as)(a => UIO(a % 2 == 0))
        } yield assert(results)(equalTo(List(2, 4, 6, 6, 10, 20, 22, 28)))
      }
    ),
    suite("filterNotPar")(
      testM(
        "filters a collection in parallel using an effectual predicate, removing all elements that satisfy the predicate"
      ) {
        val as = Iterable(2, 4, 6, 3, 5, 6, 10, 11, 15, 17, 20, 22, 23, 25, 28)
        for {
          results <- ZIO.filterNotPar(as)(a => UIO(a % 2 == 0))
        } yield assert(results)(equalTo(List(3, 5, 11, 15, 17, 23, 25)))
      }
    ),
    suite("filterOrElse")(
      testM("returns checked failure ignoring value") {
        val goodCase =
          exactlyOnce(0)(_.filterOrElse[Any, String, Int](_ == 0)(ZIO.fail(s"Predicate failed!"))).sandbox.either

        val badCase =
          exactlyOnce(1)(_.filterOrElse[Any, String, Int](_ == 0)(ZIO.fail(s"Predicate failed!"))).sandbox.either
            .map(_.left.map(_.failureOrCause))

        assertM(goodCase)(isRight(equalTo(0))) &&
        assertM(badCase)(isLeft(isLeft(equalTo("Predicate failed!"))))
      }
    ),
    suite("filterOrElseWith")(
      testM("returns checked failure from held value") {
        val goodCase =
          exactlyOnce(0)(_.filterOrElseWith[Any, String, Int](_ == 0)(a => ZIO.fail(s"$a was not 0"))).sandbox.either

        val badCase =
          exactlyOnce(1)(_.filterOrElseWith[Any, String, Int](_ == 0)(a => ZIO.fail(s"$a was not 0"))).sandbox.either
            .map(_.left.map(_.failureOrCause))

        assertM(goodCase)(isRight(equalTo(0))) &&
        assertM(badCase)(isLeft(isLeft(equalTo("1 was not 0"))))
      }
    ),
    suite("filterOrFail")(
      testM("returns failure ignoring value") {
        val goodCase =
          exactlyOnce(0)(_.filterOrFail(_ == 0)("Predicate failed!")).sandbox.either

        val badCase =
          exactlyOnce(1)(_.filterOrFail(_ == 0)("Predicate failed!")).sandbox.either
            .map(_.left.map(_.failureOrCause))

        assertM(goodCase)(isRight(equalTo(0))) &&
        assertM(badCase)(isLeft(isLeft(equalTo("Predicate failed!"))))
      }
    ) @@ zioTag(errors),
    suite("flattenErrorOption")(
      testM("fails when given Some error") {
        val task: IO[String, Int] = IO.fail(Some("Error")).flattenErrorOption("Default")
        assertM(task.exit)(fails(equalTo("Error")))
      },
      testM("fails with Default when given None error") {
        val task: IO[String, Int] = IO.fail(None).flattenErrorOption("Default")
        assertM(task.exit)(fails(equalTo("Default")))
      },
      testM("succeeds when given a value") {
        val task: IO[String, Int] = IO.succeed(1).flattenErrorOption("Default")
        assertM(task)(equalTo(1))
      }
    ) @@ zioTag(errors),
    suite("foldLeft")(
      testM("with a successful step function sums the list properly") {
        checkM(Gen.listOf(Gen.anyInt)) { l =>
          val res = IO.foldLeft(l)(0)((acc, el) => IO.succeed(acc + el))
          assertM(res)(equalTo(l.sum))
        }
      },
      testM("`with a failing step function returns a failed IO") {
        checkM(Gen.listOf1(Gen.anyInt)) { l =>
          val res = IO.foldLeft(l)(0)((_, _) => IO.fail("fail"))
          assertM(res.exit)(fails(equalTo("fail")))
        }
      } @@ zioTag(errors),
      testM("run sequentially from left to right") {
        checkM(Gen.listOf1(Gen.anyInt)) { l =>
          val res = IO.foldLeft(l)(List.empty[Int])((acc, el) => IO.succeed(el :: acc))
          assertM(res)(equalTo(l.reverse))
        }
      }
    ),
    suite("foldRight")(
      testM("with a successful step function sums the list properly") {
        checkM(Gen.listOf(Gen.anyInt)) { l =>
          val res = IO.foldRight(l)(0)((el, acc) => IO.succeed(acc + el))
          assertM(res)(equalTo(l.sum))
        }
      },
      testM("`with a failing step function returns a failed IO") {
        checkM(Gen.listOf1(Gen.anyInt)) { l =>
          val res = IO.foldRight(l)(0)((_, _) => IO.fail("fail"))
          assertM(res.exit)(fails(equalTo("fail")))
        }
      } @@ zioTag(errors),
      testM("run sequentially from right to left") {
        checkM(Gen.listOf1(Gen.anyInt)) { l =>
          val res = IO.foldRight(l)(List.empty[Int])((el, acc) => IO.succeed(el :: acc))
          assertM(res)(equalTo(l))
        }
      }
    ),
    suite("forall")(
      testM("determines whether all elements satisfy the effectual predicate") {
        checkM(Gen.listOf(Gen.anyInt), Gen.function(Gen.boolean)) { (as, f) =>
          val actual   = IO.forall(as)(a => IO.succeed(f(a)))
          val expected = as.forall(f)
          assertM(actual)(equalTo(expected))
        }
      }
    ),
    suite("foreach")(
      testM("returns the list of results") {
        checkAllM(functionIOGen, listGen) { (f, list) =>
          val res = IO.foreach(list)(f)
          assertM(res)(isSubtype[List[Int]](anything) && hasSize(equalTo(100)))
        }
      },
      testM("both evaluates effects and returns the list of results in the same order") {
        val list = List("1", "2", "3")
        for {
          ref     <- Ref.make(List.empty[String])
          res     <- IO.foreach(list)(x => ref.update(_ :+ x) *> IO.succeed[Int](x.toInt))
          effects <- ref.get
        } yield assert(effects)(equalTo(list)) && assert(res)(equalTo(List(1, 2, 3)))
      },
      testM("fails if one of the effects fails") {
        val list = List("1", "h", "3")
        val res  = IO.foreach(list)(x => IO.succeed[Int](x.toInt))
        assertM(res.exit)(dies(isSubtype[NumberFormatException](anything)))
      } @@ zioTag(errors)
    ),
    suite("foreachDiscard")(
      testM("runs effects in order") {
        val as = List(1, 2, 3, 4, 5)
        for {
          ref <- Ref.make(List.empty[Int])
          _   <- ZIO.foreachDiscard(as)(a => ref.update(_ :+ a))
          rs  <- ref.get
        } yield assert(rs)(equalTo(as))
      },
      testM("can be run twice") {
        val as = List(1, 2, 3, 4, 5)
        for {
          ref <- Ref.make(0)
          zio  = ZIO.foreachDiscard(as)(a => ref.update(_ + a))
          _   <- zio
          _   <- zio
          sum <- ref.get
        } yield assert(sum)(equalTo(30))
      }
    ),
    suite("foreach for Option")(
      testM("succeeds with None given None") {
        val task: UIO[Option[Int]] = IO.foreach(None)((str: String) => IO.succeed(str.length))
        assertM(task)(isNone)
      },
      testM("succeeds with Some given Some") {
        for {
          optRes <- IO.foreach(Some("success"))(str => IO.succeed(str.length))
        } yield assert(optRes)(equalTo(Some(7)))
      },
      testM("fails if the optional effect fails") {
        val opt = Some("h")
        val res = IO.foreach(opt)(x => IO.succeed[Int](x.toInt))
        assertM(res.exit)(dies(isSubtype[NumberFormatException](anything)))
      }
    ),
    suite("foreachPar")(
      testM("runs single task") {
        val as      = List(2)
        val results = IO.foreachPar(as)(a => IO.succeed(2 * a))
        assertM(results)(equalTo(List(4)))
      },
      testM("runs two tasks") {
        val as      = List(2, 3)
        val results = IO.foreachPar(as)(a => IO.succeed(2 * a))
        assertM(results)(equalTo(List(4, 6)))
      },
      testM("runs many tasks") {
        val as      = (1 to 1000)
        val results = IO.foreachPar(as)(a => IO.succeed(2 * a))
        assertM(results)(equalTo(as.map(2 * _)))
      },
      testM("runs a task that fails") {
        val as = (1 to 10)
        val results = IO
          .foreachPar(as) {
            case 5 => IO.fail("Boom!")
            case a => IO.succeed(2 * a)
          }
          .flip
        assertM(results)(equalTo("Boom!"))
      },
      testM("runs two failed tasks") {
        val as = (1 to 10)
        val results = IO
          .foreachPar(as) {
            case 5 => IO.fail("Boom1!")
            case 8 => IO.fail("Boom2!")
            case a => IO.succeed(2 * a)
          }
          .flip
        assertM(results)(equalTo("Boom1!") || equalTo("Boom2!"))
      },
      testM("runs a task that dies") {
        val as = (1 to 10)
        val results = IO
          .foreachPar(as) {
            case 5 => IO.dieMessage("Boom!")
            case a => IO.succeed(2 * a)
          }
          .exit
        assertM(results)(dies(hasMessage(equalTo("Boom!"))))
      },
      testM("runs a task that is interrupted") {
        val as = (1 to 10)
        val results = IO
          .foreachPar(as) {
            case 5 => IO.interrupt
            case a => IO.succeed(2 * a)
          }
          .exit
        assertM(results)(isInterrupted)
      } @@ zioTag(interruption),
      testM("runs a task that throws an unsuspended exception") {
        def f(i: Int): Task[Int] = throw new Exception(i.toString)
        for {
          _ <- IO.foreachPar(1 to 1)(f).exit
        } yield assertCompletes
      },
      testM("returns results in the same order") {
        val list = List("1", "2", "3")
        val res  = IO.foreachPar(list)(x => IO.succeed[Int](x.toInt))
        assertM(res)(equalTo(List(1, 2, 3)))
      },
      testM("returns results in the same order for Chunk") {
        val chunk = Chunk("1", "2", "3")
        val res   = IO.foreachPar(chunk)(x => IO.succeed[Int](x.toInt))
        assertM(res)(equalTo(Chunk(1, 2, 3)))
      },
      testM("runs effects in parallel") {
        assertM(for {
          p <- Promise.make[Nothing, Unit]
          _ <- UIO.foreachPar(List(UIO.never, p.succeed(())))(a => a).fork
          _ <- p.await
        } yield true)(isTrue)
      },
      testM("runs effects in parallel for Chunk") {
        assertM(for {
          p <- Promise.make[Nothing, Unit]
          _ <- UIO.foreachPar(Chunk(UIO.never, p.succeed(()), UIO.never))(a => a).fork
          _ <- p.await
        } yield true)(isTrue)
      },
      testM("propagates error") {
        val ints = List(1, 2, 3, 4, 5, 6)
        val odds = ZIO.foreachPar(ints)(n => if (n % 2 != 0) ZIO.succeed(n) else ZIO.fail("not odd"))
        assertM(odds.flip)(equalTo("not odd"))
      } @@ zioTag(errors),
      testM("interrupts effects on first failure") {
        for {
          ref     <- Ref.make(false)
          promise <- Promise.make[Nothing, Unit]
          actions = List(
                      ZIO.never,
                      ZIO.succeed(1),
                      ZIO.fail("C"),
                      promise.await *> ref.set(true)
                    )
          e <- ZIO.foreachPar(actions)(a => a).flip
          v <- ref.get
        } yield assert(e)(equalTo("C")) && assert(v)(isFalse)
      } @@ zioTag(interruption),
      testM("does not kill fiber when forked on the parent scope") {
        for {
          ref    <- Ref.make(0)
          fibers <- ZIO.foreachPar(1 to 100)(_ => ref.update(_ + 1).fork)
          _      <- ZIO.foreach(fibers)(_.await)
          value  <- ref.get
        } yield assert(value)(equalTo(100))
      }
    ),
    suite("foreachParDiscard")(
      testM("accumulates errors") {
        def task(started: Ref[Int], trigger: Promise[Nothing, Unit])(i: Int): IO[Int, Unit] =
          started.updateAndGet(_ + 1) >>= { count =>
            IO.when(count == 3)(trigger.succeed(())) *> trigger.await *> IO.fail(i)
          }

        for {
          started <- Ref.make(0)
          trigger <- Promise.make[Nothing, Unit]

          errors <- IO
                      .foreachParDiscard(1 to 3)(i => task(started, trigger)(i).uninterruptible)
                      .foldCause(cause => cause.failures.toSet, _ => Set.empty[Int])
        } yield assert(errors)(equalTo(Set(1, 2, 3)))
      } @@ zioTag(errors),
      testM("runs all effects") {
        val as = Seq(1, 2, 3, 4, 5)
        for {
          ref <- Ref.make(Seq.empty[Int])
          _   <- ZIO.foreachParDiscard(as)(a => ref.update(_ :+ a))
          rs  <- ref.get
        } yield assert(rs)(hasSize(equalTo(as.length))) &&
          assert(rs.toSet)(equalTo(as.toSet))
      },
      testM("runs all effects for Chunk") {
        val as = Chunk(1, 2, 3, 4, 5)
        for {
          ref <- Ref.make(Seq.empty[Int])
          _   <- ZIO.foreachParDiscard(as)(a => ref.update(_ :+ a))
          rs  <- ref.get
        } yield assert(rs)(hasSize(equalTo(as.length))) &&
          assert(rs.toSet)(equalTo(as.toList.toSet))
      },
      testM("completes on empty input") {
        ZIO.foreachParDiscard(Nil)(_ => ZIO.unit).as(assertCompletes)
      }
    ),
    suite("foreachParN")(
      testM("returns the list of results in the appropriate order") {
        val list = List(1, 2, 3)
        val res  = IO.foreachParN(2)(list)(x => IO.succeed(x.toString))
        assertM(res)(equalTo(List("1", "2", "3")))
      },
      testM("works on large lists") {
        val n   = 10
        val seq = List.range(0, 100000)
        val res = IO.foreachParN(n)(seq)(UIO.succeed(_))
        assertM(res)(equalTo(seq))
      },
      testM("runs effects in parallel") {
        val io = for {
          p <- Promise.make[Nothing, Unit]
          _ <- UIO.foreachParN(2)(List(UIO.never, p.succeed(())))(identity).fork
          _ <- p.await
        } yield true
        assertM(io)(isTrue)
      },
      testM("propagates error") {
        val ints = List(1, 2, 3, 4, 5, 6)
        val odds = ZIO.foreachParN(4)(ints)(n => if (n % 2 != 0) ZIO.succeed(n) else ZIO.fail("not odd"))
        assertM(odds.either)(isLeft(equalTo("not odd")))
      } @@ zioTag(errors),
      testM("interrupts effects on first failure") {
        val actions = List(
          ZIO.never,
          ZIO.succeed(1),
          ZIO.fail("C")
        )
        val io = ZIO.foreachParN(4)(actions)(a => a)
        assertM(io.either)(isLeft(equalTo("C")))
      } @@ zioTag(errors, interruption)
    ),
    suite("foreachParNDiscard")(
      testM("runs all effects") {
        val as = Seq(1, 2, 3, 4, 5)
        for {
          ref <- Ref.make(Seq.empty[Int])
          _   <- ZIO.foreachParNDiscard(2)(as)(a => ref.update(_ :+ a))
          rs  <- ref.get
        } yield assert(rs)(hasSize(equalTo(as.length))) &&
          assert(rs.toSet)(equalTo(as.toSet))
      }
    ),
    suite("forkAll")(
      testM("returns the list of results in the same order") {
        val list = List(1, 2, 3).map(IO.succeed[Int](_))
        val res  = IO.forkAll(list).flatMap[Any, Nothing, List[Int]](_.join)
        assertM(res)(equalTo(List(1, 2, 3)))
      },
      testM("happy-path") {
        val list = (1 to 1000).toList
        assertM(ZIO.forkAll(list.map(a => ZIO.succeed(a))).flatMap(_.join))(equalTo(list))
      },
      testM("empty input") {
        assertM(ZIO.forkAll(List.empty[ZIO[Any, Nothing, Unit]]).flatMap(_.join))(equalTo(List.empty))
      },
      testM("propagate failures") {
        val boom             = new Exception
        val fail: Task[Unit] = ZIO.fail(boom)
        for {
          fiber  <- ZIO.forkAll(List(fail))
          result <- fiber.join.flip
        } yield assert(result)(equalTo(boom))
      },
      testM("propagates defects") {
        val boom                                 = new Exception("boom")
        val die: UIO[Unit]                       = ZIO.die(boom)
        def joinDefect(fiber: Fiber[Nothing, _]) = fiber.join.sandbox.flip
        for {
          fiber1 <- ZIO.forkAll(List(die))
          fiber2 <- ZIO.forkAll(List(die, ZIO.succeed(42)))
          fiber3 <- ZIO.forkAll(List(die, ZIO.succeed(42), ZIO.never))

          result1 <- joinDefect(fiber1)
          result2 <- joinDefect(fiber2)
          result3 <- joinDefect(fiber3)
        } yield {
          assert(result1)(equalTo(Cause.die(boom))) && {
            assert(result2)(equalTo(Cause.die(boom))) ||
            (assert(result2.dieOption)(isSome(equalTo(boom))) && assert(result2.interrupted)(isTrue))
          } && {
            assert(result3.dieOption)(isSome(equalTo(boom))) && assert(result3.interrupted)(isTrue)
          }
        }
      } @@ nonFlaky,
      testM("infers correctly") {
        for {
          ref    <- Ref.make(0)
          worker  = ZIO.never
          workers = List.fill(4)(worker)
          fiber  <- ZIO.forkAll(workers)
          _      <- fiber.interrupt
          value  <- ref.get
        } yield assert(value)(equalTo(0))
      }
    ),
    suite("forkAs")(
      testM("child has specified name") {
        for {
          fiber <- Fiber.fiberName.get.forkAs("child")
          name  <- fiber.join
        } yield assert(name)(isSome(equalTo("child")))
      },
      testM("parent name is unchanged") {
        for {
          _    <- ZIO.unit.forkAs("child")
          name <- Fiber.fiberName.get
        } yield assert(name)(isNone)
      },
      testM("parent does not inherit child name on join") {
        for {
          fiber <- ZIO.unit.forkAs("child")
          _     <- fiber.join
          name  <- Fiber.fiberName.get
        } yield assert(name)(isNone)
      }
    ),
    suite("forkIn") {
      testM("fiber forked in a closed scope does not run") {
        for {
          ref   <- Ref.make(false)
          open  <- ZScope.make[Exit[Any, Any]]
          _     <- open.close(Exit.unit)
          fiber <- ref.set(true).forkIn(open.scope)
          exit  <- fiber.await
          value <- ref.get
        } yield assert(exit)(isInterrupted) && assert(value)(isFalse)
      }
    },
    suite("forkWithErrorHandler")(
      testM("calls provided function when task fails") {
        for {
          p <- Promise.make[Nothing, Unit]
          _ <- ZIO.fail(()).forkWithErrorHandler(p.succeed(_).unit)
          _ <- p.await
        } yield assertCompletes
      }
    ) @@ zioTag(errors),
    suite("from")(
      test("Attempt") {
        trait A
        lazy val a: A                             = ???
        lazy val actual                           = ZIO.from(a)
        lazy val expected: ZIO[Any, Throwable, A] = actual
        lazy val _                                = expected
        assertCompletes
      },
      test("Either") {
        trait E
        trait A
        lazy val either: Either[E, A]     = ???
        lazy val actual                   = ZIO.from(either)
        lazy val expected: ZIO[Any, E, A] = actual
        lazy val _                        = expected
        assertCompletes
      },
      test("EitherCause") {
        trait E
        trait A
        lazy val eitherCause: Either[Cause[E], A] = ???
        lazy val actual                           = ZIO.from(eitherCause)
        lazy val expected: ZIO[Any, E, A]         = actual
        lazy val _                                = expected
        assertCompletes
      },
      test("EitherCauseLeft") {
        trait E
        trait A
        lazy val eitherCauseLeft: Left[Cause[E], A] = ???
        lazy val actual                             = ZIO.from(eitherCauseLeft)
        lazy val expected: ZIO[Any, E, A]           = actual
        lazy val _                                  = expected
        assertCompletes
      },
      test("EitherCauseRight") {
        trait E
        trait A
        lazy val eitherCauseRight: Right[Cause[E], A] = ???
        lazy val actual                               = ZIO.from(eitherCauseRight)
        lazy val expected: ZIO[Any, E, A]             = actual
        lazy val _                                    = expected
        assertCompletes
      },
      test("EitherLeft") {
        trait E
        trait A
        lazy val eitherLeft: Left[E, A]   = ???
        lazy val actual                   = ZIO.from(eitherLeft)
        lazy val expected: ZIO[Any, E, A] = actual
        lazy val _                        = expected
        assertCompletes
      },
      test("EitherRight") {
        trait E
        trait A
        lazy val eitherRight: Right[E, A] = ???
        lazy val actual                   = ZIO.from(eitherRight)
        lazy val expected: ZIO[Any, E, A] = actual
        lazy val _                        = expected
        assertCompletes
      },
      test("Fiber") {
        trait E
        trait A
        lazy val fiber: Fiber[E, A]       = ???
        lazy val actual                   = ZIO.from(fiber)
        lazy val expected: ZIO[Any, E, A] = actual
        lazy val _                        = expected
        assertCompletes
      },
      test("FiberRuntime") {
        trait E
        trait A
        lazy val fiberRuntime: Fiber.Runtime[E, A] = ???
        lazy val actual                            = ZIO.from(fiberRuntime)
        lazy val expected: ZIO[Any, E, A]          = actual
        lazy val _                                 = expected
        assertCompletes
      },
      test("FiberSynthetic") {
        trait E
        trait A
        lazy val fiberSynthetic: Fiber.Synthetic[E, A] = ???
        lazy val actual                                = ZIO.from(fiberSynthetic)
        lazy val expected: ZIO[Any, E, A]              = actual
        lazy val _                                     = expected
        assertCompletes
      },
      test("FiberZIO") {
        trait R
        trait E
        trait E1 extends E
        trait E2 extends E
        trait A
        lazy val fiberZIO: ZIO[R, E1, Fiber[E2, A]] = ???
        lazy val actual                             = ZIO.from(fiberZIO)
        lazy val expected: ZIO[R, E, A]             = actual
        lazy val _                                  = expected
        assertCompletes
      },
      test("FiberZIORuntime") {
        trait R
        trait E
        trait E1 extends E
        trait E2 extends E
        trait A
        lazy val fiberZIORuntime: ZIO[R, E1, Fiber.Runtime[E2, A]] = ???
        lazy val actual                                            = ZIO.from(fiberZIORuntime)
        lazy val expected: ZIO[R, E, A]                            = actual
        lazy val _                                                 = expected
        assertCompletes
      },
      test("FiberZIOSynthetic") {
        trait R
        trait E
        trait E1 extends E
        trait E2 extends E
        trait A
        lazy val fiberZIOSynthetic: ZIO[R, E1, Fiber.Synthetic[E2, A]] = ???
        lazy val actual                                                = ZIO.from(fiberZIOSynthetic)
        lazy val expected: ZIO[R, E, A]                                = actual
        lazy val _                                                     = expected
        assertCompletes
      },
      test("Function") {
        trait R
        trait A
        trait FunctionLike[In, Out] extends Function1[In, Out]
        lazy val function: FunctionLike[R, A] = ???
        lazy val actual                       = ZIO.from(function)
        lazy val expected: ZIO[R, Nothing, A] = actual
        lazy val _                            = expected
        assertCompletes
      },
      test("FunctionEither") {
        trait R
        trait E
        trait A
        trait FunctionLike[In, Out] extends Function1[In, Out]
        lazy val functionEither: FunctionLike[R, Either[E, A]] = ???
        lazy val actual                                        = ZIO.from(functionEither)
        lazy val expected: ZIO[R, E, A]                        = actual
        lazy val _                                             = expected
        assertCompletes
      },
      test("FunctionEitherLeft") {
        trait R
        trait E
        trait A
        trait FunctionLike[In, Out] extends Function1[In, Out]
        lazy val functionEitherLeft: FunctionLike[R, Left[E, A]] = ???
        lazy val actual                                          = ZIO.from(functionEitherLeft)
        lazy val expected: ZIO[R, E, A]                          = actual
        lazy val _                                               = expected
        assertCompletes
      },
      test("FunctionEitherRight") {
        trait R
        trait E
        trait A
        trait FunctionLike[In, Out] extends Function1[In, Out]
        lazy val functionEitherRight: FunctionLike[R, Right[E, A]] = ???
        lazy val actual                                            = ZIO.from(functionEitherRight)
        lazy val expected: ZIO[R, E, A]                            = actual
        lazy val _                                                 = expected
        assertCompletes
      },
      test("FunctionFuture") {
        trait R
        trait A
        trait FunctionLike[In, Out] extends Function1[In, Out]
        trait FutureLike[A]         extends scala.concurrent.Future[A]
        lazy val functionFuture: FunctionLike[R, FutureLike[A]] = ???
        lazy val actual                                         = ZIO.from(functionFuture)
        lazy val expected: ZIO[R, Throwable, A]                 = actual
        lazy val _                                              = expected
        assertCompletes
      },
      test("FunctionZIO") {
        trait R
        trait E
        trait A
        trait FunctionLike[In, Out] extends Function1[In, Out]
        lazy val functionZIO: FunctionLike[R, IO[E, A]] = ???
        lazy val actual                                 = ZIO.from(functionZIO)
        lazy val expected: ZIO[R, E, A]                 = actual
        lazy val _                                      = expected
        assertCompletes
      },
      test("Future") {
        trait A
        trait FutureLike[A] extends scala.concurrent.Future[A]
        lazy val future: scala.concurrent.ExecutionContext => FutureLike[A] = ???
        lazy val actual                                                     = ZIO.from(future)
        lazy val expected: ZIO[Any, Throwable, A]                           = actual
        lazy val _                                                          = expected
        assertCompletes
      },
      test("Option") {
        trait A
        lazy val option: Option[A]                      = ???
        lazy val actual                                 = ZIO.from(option)
        lazy val expected: ZIO[Any, Option[Nothing], A] = actual
        lazy val _                                      = expected
        assertCompletes
      },
      test("OptionNone") {
        lazy val optionNone: None.type                        = ???
        lazy val actual                                       = ZIO.from(optionNone)
        lazy val expected: ZIO[Any, Option[Nothing], Nothing] = actual
        lazy val _                                            = expected
        assertCompletes
      },
      test("OptionSome") {
        trait A
        lazy val optionSome: Some[A]                    = ???
        lazy val actual                                 = ZIO.from(optionSome)
        lazy val expected: ZIO[Any, Option[Nothing], A] = actual
        lazy val _                                      = expected
        assertCompletes
      },
      test("PromiseScala") {
        trait A
        trait PromiseLike[A] extends scala.concurrent.Promise[A]
        lazy val promiseScala: PromiseLike[A]     = ???
        lazy val actual                           = ZIO.from(promiseScala)
        lazy val expected: ZIO[Any, Throwable, A] = actual
        lazy val _                                = expected
        assertCompletes
      },
      test("Try") {
        trait A
        lazy val tryScala: scala.util.Try[A]      = ???
        lazy val actual                           = ZIO.from(tryScala)
        lazy val expected: ZIO[Any, Throwable, A] = actual
        lazy val _                                = expected
        assertCompletes
      },
      test("TryFailure") {
        trait A
        lazy val tryFailure: scala.util.Failure[A] = ???
        lazy val actual                            = ZIO.from(tryFailure)
        lazy val expected: ZIO[Any, Throwable, A]  = actual
        lazy val _                                 = expected
        assertCompletes
      },
      test("TrySuccess") {
        trait A
        lazy val trySuccess: scala.util.Success[A] = ???
        lazy val actual                            = ZIO.from(trySuccess)
        lazy val expected: ZIO[Any, Throwable, A]  = actual
        lazy val _                                 = expected
        assertCompletes
      }
    ),
    suite("fromFutureInterrupt")(
      testM("running Future can be interrupted") {
        import java.util.concurrent.atomic.AtomicInteger

        import scala.concurrent.{ExecutionContext, Future}
        def infiniteFuture(ref: AtomicInteger)(implicit ec: ExecutionContext): Future[Nothing] =
          Future(ref.getAndIncrement()).flatMap(_ => infiniteFuture(ref))
        for {
          ref   <- ZIO.succeed(new AtomicInteger(0))
          fiber <- ZIO.fromFutureInterrupt(ec => infiniteFuture(ref)(ec)).fork
          _     <- fiber.interrupt
          v1    <- ZIO.succeed(ref.get)
          _     <- Live.live(Clock.sleep(10.milliseconds))
          v2    <- ZIO.succeed(ref.get)
        } yield assert(v1)(equalTo(v2))
      },
      testM("interruption blocks on interruption of the Future") {
        import scala.concurrent.Promise
        for {
          latch  <- ZIO.succeed(Promise[Unit]())
          fiber  <- ZIO.fromFutureInterrupt { _ => latch.success(()); Promise[Unit]().future }.forkDaemon
          _      <- ZIO.fromFuture(_ => latch.future).orDie
          result <- Live.withLive(fiber.interrupt)(_.timeout(10.milliseconds))
        } yield assert(result)(isNone)
      }
    ) @@ zioTag(future, interruption) @@ jvmOnly,
    suite("head")(
      testM("on non empty list") {
        assertM(ZIO.succeed(List(1, 2, 3)).head.either)(isRight(equalTo(1)))
      },
      testM("on empty list") {
        assertM(ZIO.succeed(List.empty).head.either)(isLeft(isNone))
      },
      testM("on failure") {
        assertM(ZIO.fail("Fail").head.either)(isLeft(isSome(equalTo("Fail"))))
      } @@ zioTag(errors)
    ),
    suite("ifZIO")(
      testM("runs `onTrue` if result of `b` is `true`") {
        val zio = ZIO.ifZIO(ZIO.succeed(true))(ZIO.succeed(true), ZIO.succeed(false))
        assertM(zio)(isTrue)
      },
      testM("runs `onFalse` if result of `b` is `false`") {
        val zio = ZIO.ifZIO(ZIO.succeed(false))(ZIO.succeed(true), ZIO.succeed(false))
        assertM(zio)(isFalse)
      },
      testM("infers correctly") {
        trait R
        trait R1 extends R
        trait E1
        trait E extends E1
        trait A
        val b: ZIO[R, E, Boolean]   = ZIO.succeed(true)
        val onTrue: ZIO[R1, E1, A]  = ZIO.succeed(new A {})
        val onFalse: ZIO[R1, E1, A] = ZIO.succeed(new A {})
        val _                       = ZIO.ifZIO(b)(onTrue, onFalse)
        ZIO.succeed(assertCompletes)
      }
    ),
    suite("ignore")(
      testM("return success as Unit") {
        assertM(ZIO.succeed(11).ignore)(equalTo(()))
      },
      testM("return failure as Unit") {
        assertM(ZIO.fail(123).ignore)(equalTo(()))
      } @@ zioTag(errors),
      testM("not catch throwable") {
        assertM(ZIO.die(ExampleError).ignore.exit)(dies(equalTo(ExampleError)))
      } @@ zioTag(errors)
    ),
    suite("isFailure")(
      testM("returns true when the effect is a failure") {
        assertM(ZIO.fail("fail").isFailure)(isTrue)
      },
      testM("returns false when the effect is a success") {
        assertM(ZIO.succeed("succeed").isFailure)(isFalse)
      }
    ),
    suite("isSuccess")(
      testM("returns false when the effect is a failure") {
        assertM(ZIO.fail("fail").isSuccess)(isFalse)
      },
      testM("returns true when the effect is a success") {
        assertM(ZIO.succeed("succeed").isSuccess)(isTrue)
      }
    ),
    suite("iterate")(
      testM("iterates with the specified effectual function") {
        for {
          result <- ZIO.iterate(100)(_ > 0)(a => ZIO.succeed(a - 1))
        } yield assert(result)(equalTo(0))
      }
    ),
    suite("left")(
      testM("on Left value") {
        assertM(ZIO.succeed(Left("Left")).left)(equalTo("Left"))
      },
      testM("on Right value") {
        assertM(ZIO.succeed(Right("Right")).left.exit)(fails(isRight(equalTo("Right"))))
      },
      testM("on failure") {
        assertM(ZIO.fail("Fail").left.exit)(fails(isLeft(equalTo("Fail"))))
      } @@ zioTag(errors)
    ),
    suite("loop")(
      testM("loops with the specified effectual function") {
        for {
          ref    <- Ref.make(List.empty[Int])
          _      <- ZIO.loop(0)(_ < 5, _ + 1)(a => ref.update(a :: _))
          result <- ref.get.map(_.reverse)
        } yield assert(result)(equalTo(List(0, 1, 2, 3, 4)))
      },
      testM("collects the results into a list") {
        for {
          result <- ZIO.loop(0)(_ < 5, _ + 2)(a => ZIO.succeed(a * 3))
        } yield assert(result)(equalTo(List(0, 6, 12)))
      }
    ),
    suite("loopDiscard")(
      testM("loops with the specified effectual function") {
        for {
          ref    <- Ref.make(List.empty[Int])
          _      <- ZIO.loopDiscard(0)(_ < 5, _ + 1)(a => ref.update(a :: _))
          result <- ref.get.map(_.reverse)
        } yield assert(result)(equalTo(List(0, 1, 2, 3, 4)))
      }
    ),
    suite("mapBoth")(
      testM("maps over both error and value channels") {
        checkM(Gen.anyInt) { i =>
          val res = IO.fail(i).mapBoth(_.toString, identity).either
          assertM(res)(isLeft(equalTo(i.toString)))
        }
      }
    ),
    suite("mapEffect")(
      testM("returns an effect whose success is mapped by the specified side effecting function") {
        val task = ZIO.succeed("123").mapAttempt(_.toInt)
        assertM(task)(equalTo(123))
      },
      testM("translates any thrown exceptions into typed failed effects") {
        val task = ZIO.succeed("hello").mapAttempt(_.toInt)
        assertM(task.exit)(fails(isSubtype[NumberFormatException](anything)))
      }
    ),
    suite("mapN")(
      testM("with Tuple2") {
        checkM(Gen.anyInt, Gen.alphaNumericString) { (int: Int, str: String) =>
          def f(i: Int, s: String): String = i.toString + s
          val actual                       = ZIO.mapN(ZIO.succeed(int), ZIO.succeed(str))(f)
          val expected                     = f(int, str)
          assertM(actual)(equalTo(expected))
        }
      },
      testM("with Tuple3") {
        checkM(Gen.anyInt, Gen.alphaNumericString, Gen.alphaNumericString) { (int: Int, str1: String, str2: String) =>
          def f(i: Int, s1: String, s2: String): String = i.toString + s1 + s2
          val actual                                    = ZIO.mapN(ZIO.succeed(int), ZIO.succeed(str1), ZIO.succeed(str2))(f)
          val expected                                  = f(int, str1, str2)
          assertM(actual)(equalTo(expected))
        }
      },
      testM("with Tuple4") {
        checkM(Gen.anyInt, Gen.alphaNumericString, Gen.alphaNumericString, Gen.alphaNumericString) {
          (int: Int, str1: String, str2: String, str3: String) =>
            def f(i: Int, s1: String, s2: String, s3: String): String = i.toString + s1 + s2 + s3
            val actual =
              ZIO.mapN(ZIO.succeed(int), ZIO.succeed(str1), ZIO.succeed(str2), ZIO.succeed(str3))(f)
            val expected = f(int, str1, str2, str3)
            assertM(actual)(equalTo(expected))
        }
      }
    ),
    suite("mapParN")(
      testM("with Tuple2") {
        checkM(Gen.anyInt, Gen.alphaNumericString) { (int: Int, str: String) =>
          def f(i: Int, s: String): String = i.toString + s
          val actual                       = ZIO.mapParN(ZIO.succeed(int), ZIO.succeed(str))(f)
          val expected                     = f(int, str)
          assertM(actual)(equalTo(expected))
        }
      },
      testM("with Tuple3") {
        checkM(Gen.anyInt, Gen.alphaNumericString, Gen.alphaNumericString) { (int: Int, str1: String, str2: String) =>
          def f(i: Int, s1: String, s2: String): String = i.toString + s1 + s2
          val actual                                    = ZIO.mapParN(ZIO.succeed(int), ZIO.succeed(str1), ZIO.succeed(str2))(f)
          val expected                                  = f(int, str1, str2)
          assertM(actual)(equalTo(expected))
        }
      },
      testM("with Tuple4") {
        checkM(Gen.anyInt, Gen.alphaNumericString, Gen.alphaNumericString, Gen.alphaNumericString) {
          (int: Int, str1: String, str2: String, str3: String) =>
            def f(i: Int, s1: String, s2: String, s3: String): String = i.toString + s1 + s2 + s3
            val actual =
              ZIO.mapParN(ZIO.succeed(int), ZIO.succeed(str1), ZIO.succeed(str2), ZIO.succeed(str3))(f)
            val expected = f(int, str1, str2, str3)
            assertM(actual)(equalTo(expected))
        }
      }
    ),
    suite("memoize")(
      testM("non-memoized returns new instances on repeated calls") {
        val io = Random.nextString(10)
        (io <*> io)
          .map(tuple => assert(tuple._1)(not(equalTo(tuple._2))))
      },
      testM("memoized returns the same instance on repeated calls") {
        val ioMemo = Random.nextString(10).memoize
        ioMemo
          .flatMap(io => io <*> io)
          .map(tuple => assert(tuple._1)(equalTo(tuple._2)))
      },
      testM("memoized function returns the same instance on repeated calls") {
        for {
          memoized <- ZIO.memoize((n: Int) => Random.nextString(n))
          a        <- memoized(10)
          b        <- memoized(10)
          c        <- memoized(11)
          d        <- memoized(11)
        } yield assert(a)(equalTo(b)) &&
          assert(b)(not(equalTo(c))) &&
          assert(c)(equalTo(d))
      }
    ),
    suite("merge")(
      testM("on flipped result") {
        val zio: IO[Int, Int] = ZIO.succeed(1)

        for {
          a <- zio.merge
          b <- zio.flip.merge
        } yield assert(a)(equalTo(b))
      }
    ),
    suite("mergeAll")(
      testM("return zero element on empty input") {
        val zeroElement = 42
        val nonZero     = 43
        UIO.mergeAll(Nil)(zeroElement)((_, _) => nonZero).map {
          assert(_)(equalTo(zeroElement))
        }
      },
      testM("merge list using function") {
        val effects = List(3, 5, 7).map(UIO.succeed(_))
        UIO.mergeAll(effects)(zero = 1)(_ + _).map {
          assert(_)(equalTo(1 + 3 + 5 + 7))
        }
      },
      testM("return error if it exists in list") {
        val effects = List(UIO.unit, ZIO.fail(1))
        val merged  = ZIO.mergeAll(effects)(zero = ())((_, _) => ())
        assertM(merged.exit)(fails(equalTo(1)))
      } @@ zioTag(errors)
    ),
    suite("mergeAllPar")(
      testM("return zero element on empty input") {
        val zeroElement = 42
        val nonZero     = 43
        UIO.mergeAllPar(Nil)(zeroElement)((_, _) => nonZero).map {
          assert(_)(equalTo(zeroElement))
        }
      },
      testM("merge list using function") {
        val effects = List(3, 5, 7).map(UIO.succeed(_))
        UIO.mergeAllPar(effects)(zero = 1)(_ + _).map {
          assert(_)(equalTo(1 + 3 + 5 + 7))
        }
      },
      testM("return error if it exists in list") {
        val effects = List(UIO.unit, ZIO.fail(1))
        val merged  = ZIO.mergeAllPar(effects)(zero = ())((_, _) => ())
        assertM(merged.exit)(fails(equalTo(1)))
      } @@ zioTag(errors)
    ),
    suite("none")(
      testM("on Some fails with None") {
        val task: IO[Option[Throwable], Unit] = Task(Some(1)).none
        assertM(task.exit)(fails(isNone))
      },
      testM("on None succeeds with ()") {
        val task: IO[Option[Throwable], Unit] = Task(None).none
        assertM(task)(isUnit)
      },
      testM("fails with Some(ex) when effect fails with ex") {
        val ex                                = new RuntimeException("Failed Task")
        val task: IO[Option[Throwable], Unit] = Task.fail(ex).none
        assertM(task.exit)(fails(isSome(equalTo(ex))))
      } @@ zioTag(errors)
    ),
    suite("negate")(
      testM("on true returns false") {
        assertM(ZIO.succeed(true).negate)(equalTo(false))
      },
      testM("on false returns true") {
        assertM(ZIO.succeed(false).negate)(equalTo(true))
      }
    ),
    suite("noneOrFail")(
      testM("on None succeeds with Unit") {
        val option: Option[String] = None
        for {
          value <- ZIO.noneOrFail(option)
        } yield {
          assert(value)(equalTo(()))
        }
      },
      testM("on Some fails") {
        for {
          value <- ZIO.noneOrFail(Some("v")).catchAll(e => ZIO.succeed(e))
        } yield {
          assert(value)(equalTo("v"))
        }
      } @@ zioTag(errors)
    ),
    suite("noneOrFailWith")(
      testM("on None succeeds with Unit") {
        val option: Option[String]       = None
        val adaptError: String => String = identity
        for {
          value <- ZIO.noneOrFailWith(option)(adaptError)
        } yield {
          assert(value)(equalTo(()))
        }
      },
      testM("on Some fails") {
        for {
          value <- ZIO.noneOrFailWith(Some("value"))((v: String) => v + v).catchAll(e => ZIO.succeed(e))
        } yield {
          assert(value)(equalTo("valuevalue"))
        }
      } @@ zioTag(errors)
    ),
    suite("once")(
      testM("returns an effect that will only be executed once") {
        for {
          ref    <- Ref.make(0)
          zio    <- ref.update(_ + 1).once
          _      <- ZIO.collectAllPar(ZIO.replicate(100)(zio))
          result <- ref.get
        } yield assert(result)(equalTo(1))
      }
    ),
    suite("onExit")(
      testM("executes that a cleanup function runs when effect succeeds") {
        for {
          ref <- Ref.make(false)
          _ <- ZIO.unit.onExit {
                 case Exit.Success(_) => ref.set(true)
                 case _               => UIO.unit
               }
          p <- ref.get
        } yield assert(p)(isTrue)
      },
      testM("ensures that a cleanup function runs when an effect fails") {
        for {
          ref <- Ref.make(false)
          _ <- ZIO
                 .die(new RuntimeException)
                 .onExit {
                   case Exit.Failure(c) if c.died => ref.set(true)
                   case _                         => UIO.unit
                 }
                 .sandbox
                 .ignore
          p <- ref.get
        } yield assert(p)(isTrue)
      },
      testM("ensures that a cleanup function runs when an effect is interrupted") {
        for {
          latch1 <- Promise.make[Nothing, Unit]
          latch2 <- Promise.make[Nothing, Unit]
          fiber <- (latch1.succeed(()) *> ZIO.never).onExit {
                     case Exit.Failure(c) if c.interrupted => latch2.succeed(())
                     case _                                => UIO.unit
                   }.fork
          _ <- latch1.await
          _ <- fiber.interrupt
          _ <- latch2.await
        } yield assertCompletes
      } @@ zioTag(interruption)
    ),
    suite("option")(
      testM("return success in Some") {
        implicit val canFail = CanFail
        assertM(ZIO.succeed(11).option)(equalTo(Some(11)))
      },
      testM("return failure as None") {
        assertM(ZIO.fail(123).option)(equalTo(None))
      } @@ zioTag(errors),
      testM("not catch throwable") {
        implicit val canFail = CanFail
        assertM(ZIO.die(ExampleError).option.exit)(dies(equalTo(ExampleError)))
      } @@ zioTag(errors),
      testM("catch throwable after sandboxing") {
        assertM(ZIO.die(ExampleError).sandbox.option)(equalTo(None))
      } @@ zioTag(errors)
    ),
    suite("optional")(
      testM("fails when given Some error") {
        val task: IO[String, Option[Int]] = IO.fail(Some("Error")).unoption
        assertM(task.exit)(fails(equalTo("Error")))
      } @@ zioTag(errors),
      testM("succeeds with None given None error") {
        val task: IO[String, Option[Int]] = IO.fail(None).unoption
        assertM(task)(isNone)
      } @@ zioTag(errors),
      testM("succeeds with Some given a value") {
        val task: IO[String, Option[Int]] = IO.succeed(1).unoption
        assertM(task)(isSome(equalTo(1)))
      }
    ),
    suite("orElse")(
      testM("does not recover from defects") {
        val ex               = new Exception("Died")
        val fiberId          = Fiber.Id(0L, 123L)
        implicit val canFail = CanFail
        for {
          plain <- (ZIO.die(ex) <> IO.unit).exit
          both  <- (ZIO.failCause(Cause.Both(interrupt(fiberId), die(ex))) <> IO.unit).exit
          thn   <- (ZIO.failCause(Cause.Then(interrupt(fiberId), die(ex))) <> IO.unit).exit
          fail  <- (ZIO.fail(ex) <> IO.unit).exit
        } yield assert(plain)(dies(equalTo(ex))) &&
          assert(both)(dies(equalTo(ex))) &&
          assert(thn)(dies(equalTo(ex))) &&
          assert(fail)(succeeds(isUnit))
      },
      testM("left failed and right died with kept cause") {
        val z1                = Task.fail(new Throwable("1"))
        val z2: Task[Nothing] = Task.die(new Throwable("2"))
        val orElse: Task[Boolean] = z1.orElse(z2).catchAllCause {
          case Traced(Die(e: Throwable), _) => Task(e.getMessage == "2")
          case _                            => Task(false)
        }
        assertM(orElse)(equalTo(true))
      },
      testM("left failed and right failed with kept cause") {
        val z1                = Task.fail(new Throwable("1"))
        val z2: Task[Nothing] = Task.fail(new Throwable("2"))
        val orElse: Task[Boolean] = z1.orElse(z2).catchAllCause {
          case Traced(Fail(e: Throwable), _) => Task(e.getMessage == "2")
          case _                             => Task(false)
        }
        assertM(orElse)(equalTo(true))
      },
      testM("is associative") {
        val smallInts = Gen.int(0, 100)
        val causes    = Gen.causes(smallInts, Gen.throwable)
        val successes = Gen.successes(smallInts)
        val exits     = Gen.either(causes, successes).map(_.fold(Exit.failCause, Exit.succeed))
        checkM(exits, exits, exits) { (exit1, exit2, exit3) =>
          val zio1  = ZIO.done(exit1)
          val zio2  = ZIO.done(exit2)
          val zio3  = ZIO.done(exit3)
          val left  = (zio1 orElse zio2) orElse zio3
          val right = zio1 orElse (zio2 orElse zio3)
          for {
            left  <- left.exit
            right <- right.exit
          } yield assert(left)(equalTo(right))
        }
      }
    ) @@ zioTag(errors),
    suite("orElseFail")(
      testM("executes this effect and returns its value if it succeeds") {
        implicit val canFail = CanFail
        assertM(ZIO.succeed(true).orElseFail(false))(isTrue)
      },
      testM("otherwise fails with the specified error") {
        assertM(ZIO.fail(false).orElseFail(true).flip)(isTrue)
      } @@ zioTag(errors)
    ),
    suite("orElseOptional")(
      testM("produces the value of this effect if it succeeds") {
        val zio = ZIO.succeed("succeed").orElseOptional(ZIO.succeed("orElse"))
        assertM(zio)(equalTo("succeed"))
      },
      testM("produces the value of this effect if it fails with some error") {
        val zio = ZIO.fail(Some("fail")).orElseOptional(ZIO.succeed("orElse"))
        assertM(zio.exit)(fails(isSome(equalTo("fail"))))
      },
      testM("produces the value of the specified effect if it fails with none") {
        val zio = ZIO.fail(None).orElseOptional(ZIO.succeed("orElse"))
        assertM(zio)(equalTo("orElse"))
      }
    ),
    suite("orElseSucceed")(
      testM("executes this effect and returns its value if it succeeds") {
        implicit val canFail = CanFail
        assertM(ZIO.succeed(true).orElseSucceed(false))(isTrue)
      },
      testM("otherwise succeeds with the specified value") {
        assertM(ZIO.fail(false).orElseSucceed(true))(isTrue)
      } @@ zioTag(errors)
    ),
    suite("parallelErrors")(
      testM("oneFailure") {
        for {
          f1     <- IO.fail("error1").fork
          f2     <- IO.succeed("success1").fork
          errors <- f1.zip(f2).join.parallelErrors[String].flip
        } yield assert(errors)(equalTo(List("error1")))
      },
      testM("allFailures") {
        for {
          f1     <- IO.fail("error1").fork
          f2     <- IO.fail("error2").fork
          errors <- f1.zip(f2).join.parallelErrors[String].flip
        } yield assert(errors)(
          equalTo(List("error1", "error2")) ||
            equalTo(List("error1")) ||
            equalTo(List("error2"))
        )
      } @@ nonFlaky
    ) @@ zioTag(errors),
    suite("partition")(
      testM("collects only successes") {
        implicit val canFail = CanFail
        val in               = List.range(0, 10)
        for {
          res <- ZIO.partition(in)(a => ZIO.succeed(a))
        } yield assert(res._1)(isEmpty) && assert(res._2)(equalTo(in))
      },
      testM("collects only failures") {
        val in = List.fill(10)(0)
        for {
          res <- ZIO.partition(in)(a => ZIO.fail(a))
        } yield assert(res._1)(equalTo(in)) && assert(res._2)(isEmpty)
      } @@ zioTag(errors),
      testM("collects failures and successes") {
        val in = List.range(0, 10)
        for {
          res <- ZIO.partition(in)(a => if (a % 2 == 0) ZIO.fail(a) else ZIO.succeed(a))
        } yield assert(res._1)(equalTo(List(0, 2, 4, 6, 8))) && assert(res._2)(equalTo(List(1, 3, 5, 7, 9)))
      } @@ zioTag(errors),
      testM("evaluates effects in correct order") {
        implicit val canFail = CanFail
        val as               = List(2, 4, 6, 3, 5, 6)
        for {
          ref     <- Ref.make(List.empty[Int])
          _       <- ZIO.partition(as)(a => ref.update(a :: _))
          effects <- ref.get.map(_.reverse)
        } yield assert(effects)(equalTo(List(2, 4, 6, 3, 5, 6)))
      }
    ),
    suite("partitionPar")(
      testM("collects a lot of successes") {
        implicit val canFail = CanFail
        val in               = List.range(0, 1000)
        for {
          res <- ZIO.partitionPar(in)(a => ZIO.succeed(a))
        } yield assert(res._1)(isEmpty) && assert(res._2)(equalTo(in))
      },
      testM("collects failures") {
        val in = List.fill(10)(0)
        for {
          res <- ZIO.partitionPar(in)(a => ZIO.fail(a))
        } yield assert(res._1)(equalTo(in)) && assert(res._2)(isEmpty)
      } @@ zioTag(errors),
      testM("collects failures and successes") {
        val in = List.range(0, 10)
        for {
          res <- ZIO.partitionPar(in)(a => if (a % 2 == 0) ZIO.fail(a) else ZIO.succeed(a))
        } yield assert(res._1)(equalTo(List(0, 2, 4, 6, 8))) && assert(res._2)(equalTo(List(1, 3, 5, 7, 9)))
      } @@ zioTag(errors)
    ),
    suite("partitionParN")(
      testM("collects a lot of successes") {
        implicit val canFail = CanFail
        val in               = List.range(0, 1000)
        for {
          res <- ZIO.partitionParN(3)(in)(a => ZIO.succeed(a))
        } yield assert(res._1)(isEmpty) && assert(res._2)(equalTo(in))
      },
      testM("collects failures") {
        val in = List.fill(10)(0)
        for {
          res <- ZIO.partitionParN(3)(in)(a => ZIO.fail(a))
        } yield assert(res._1)(equalTo(in)) && assert(res._2)(isEmpty)
      } @@ zioTag(errors),
      testM("collects failures and successes") {
        val in = List.range(0, 10)
        for {
          res <- ZIO.partitionParN(3)(in)(a => if (a % 2 == 0) ZIO.fail(a) else ZIO.succeed(a))
        } yield assert(res._1)(equalTo(List(0, 2, 4, 6, 8))) && assert(res._2)(equalTo(List(1, 3, 5, 7, 9)))
      } @@ zioTag(errors)
    ),
    suite("provideCustomLayer")(
      testM("provides the part of the environment that is not part of the `ZEnv`") {
        val loggingLayer: ZLayer[Any, Nothing, Logging] = Logging.live
        val zio: ZIO[ZEnv with Logging, Nothing, Unit]  = ZIO.unit
        val zio2: URIO[ZEnv, Unit]                      = zio.provideCustomLayer(loggingLayer)
        assertM(zio2)(anything)
      }
    ),
    suite("provideSomeLayer")(
      testM("can split environment into two parts") {
        val clockLayer: ZLayer[Any, Nothing, Has[Clock]]         = Clock.live
        val zio: ZIO[Has[Clock] with Has[Random], Nothing, Unit] = ZIO.unit
        val zio2: URIO[Has[Random], Unit]                        = zio.provideSomeLayer[Has[Random]](clockLayer)
        assertM(zio2)(anything)
      }
    ),
    suite("raceAll")(
      testM("returns first success") {
        assertM(ZIO.fail("Fail").raceAll(List(IO.succeed(24))))(equalTo(24))
      },
      testM("returns last failure") {
        assertM(Live.live(ZIO.sleep(100.millis) *> ZIO.fail(24)).raceAll(List(ZIO.fail(25))).flip)(equalTo(24))
      } @@ flaky @@ zioTag(errors),
      testM("returns success when it happens after failure") {
        assertM(ZIO.fail(42).raceAll(List(IO.succeed(24) <* Live.live(ZIO.sleep(100.millis)))))(equalTo(24))
      } @@ zioTag(errors)
    ),
    suite("reduceAllPar")(
      testM("return zero element on empty input") {
        val zeroElement = 42
        val nonZero     = 43
        UIO.reduceAllPar(UIO.succeed(zeroElement), Nil)((_, _) => nonZero).map {
          assert(_)(equalTo(zeroElement))
        }
      },
      testM("reduce list using function") {
        val zeroElement  = UIO.succeed(1)
        val otherEffects = List(3, 5, 7).map(UIO.succeed(_))
        UIO.reduceAllPar(zeroElement, otherEffects)(_ + _).map {
          assert(_)(equalTo(1 + 3 + 5 + 7))
        }
      },
      testM("return error if zero is an error") {
        val zeroElement  = ZIO.fail(1)
        val otherEffects = List(UIO.unit, UIO.unit)
        val reduced      = ZIO.reduceAllPar(zeroElement, otherEffects)((_, _) => ())
        assertM(reduced.exit)(fails(equalTo(1)))
      } @@ zioTag(errors),
      testM("return error if it exists in list") {
        val zeroElement = UIO.unit
        val effects     = List(UIO.unit, ZIO.fail(1))
        val reduced     = ZIO.reduceAllPar(zeroElement, effects)((_, _) => ())
        assertM(reduced.exit)(fails(equalTo(1)))
      } @@ zioTag(errors)
    ),
    suite("replicate")(
      testM("zero") {
        val lst: Iterable[UIO[Int]] = ZIO.replicate(0)(ZIO.succeed(12))
        assertM(ZIO.collectAll(lst))(equalTo(List.empty))
      },
      testM("negative") {
        val anotherList: Iterable[UIO[Int]] = ZIO.replicate(-2)(ZIO.succeed(12))
        assertM(ZIO.collectAll(anotherList))(equalTo(List.empty))
      },
      testM("positive") {
        val lst: Iterable[UIO[Int]] = ZIO.replicate(2)(ZIO.succeed(12))
        assertM(ZIO.collectAll(lst))(equalTo(List(12, 12)))
      }
    ),
    suite("retryUntil")(
      testM("retryUntil retries until condition is true") {
        for {
          in     <- Ref.make(10)
          out    <- Ref.make(0)
          _      <- (in.updateAndGet(_ - 1) <* out.update(_ + 1)).flipWith(_.retryUntil(_ == 0))
          result <- out.get
        } yield assert(result)(equalTo(10))
      },
      testM("retryUntil runs at least once") {
        for {
          ref    <- Ref.make(0)
          _      <- ref.update(_ + 1).flipWith(_.retryUntil(_ => true))
          result <- ref.get
        } yield assert(result)(equalTo(1))
      }
    ),
    suite("retryUntilEquals")(
      testM("retryUntilEquals retries until error equals predicate") {
        for {
          q      <- Queue.unbounded[Int]
          _      <- q.offerAll(List(1, 2, 3, 4, 5, 6))
          acc    <- Ref.make(0)
          _      <- (q.take <* acc.update(_ + 1)).flipWith(_.retryUntilEquals(5))
          result <- acc.get
        } yield assert(result)(equalTo(5))
      }
    ),
    suite("retryUntilZIO")(
      testM("retryUntilZIO retries until condition is true") {
        for {
          in     <- Ref.make(10)
          out    <- Ref.make(0)
          _      <- (in.updateAndGet(_ - 1) <* out.update(_ + 1)).flipWith(_.retryUntilZIO(v => UIO.succeed(v == 0)))
          result <- out.get
        } yield assert(result)(equalTo(10))
      },
      testM("retryUntilZIO runs at least once") {
        for {
          ref    <- Ref.make(0)
          _      <- ref.update(_ + 1).flipWith(_.retryUntilZIO(_ => UIO.succeed(true)))
          result <- ref.get
        } yield assert(result)(equalTo(1))
      }
    ),
    suite("retryWhile")(
      testM("retryWhile retries while condition is true") {
        for {
          in     <- Ref.make(10)
          out    <- Ref.make(0)
          _      <- (in.updateAndGet(_ - 1) <* out.update(_ + 1)).flipWith(_.retryWhile(_ >= 0))
          result <- out.get
        } yield assert(result)(equalTo(11))
      },
      testM("retryWhile runs at least once") {
        for {
          ref    <- Ref.make(0)
          _      <- ref.update(_ + 1).flipWith(_.retryWhile(_ => false))
          result <- ref.get
        } yield assert(result)(equalTo(1))
      }
    ),
    suite("retryWhileEquals")(
      testM("retryWhileEquals retries while error equals predicate") {
        for {
          q      <- Queue.unbounded[Int]
          _      <- q.offerAll(List(0, 0, 0, 0, 1, 2))
          acc    <- Ref.make(0)
          _      <- (q.take <* acc.update(_ + 1)).flipWith(_.retryWhileEquals(0))
          result <- acc.get
        } yield assert(result)(equalTo(5))
      }
    ),
    suite("retryWhileZIO")(
      testM("retryWhileZIO retries while condition is true") {
        for {
          in     <- Ref.make(10)
          out    <- Ref.make(0)
          _      <- (in.updateAndGet(_ - 1) <* out.update(_ + 1)).flipWith(_.retryWhileZIO(v => UIO.succeed(v >= 0)))
          result <- out.get
        } yield assert(result)(equalTo(11))
      },
      testM("retryWhileZIO runs at least once") {
        for {
          ref    <- Ref.make(0)
          _      <- ref.update(_ + 1).flipWith(_.retryWhileZIO(_ => UIO.succeed(false)))
          result <- ref.get
        } yield assert(result)(equalTo(1))
      }
    ),
    suite("right")(
      testM("on Right value") {
        assertM(ZIO.succeed(Right("Right")).right)(equalTo("Right"))
      },
      testM("on Left value") {
        assertM(ZIO.succeed(Left("Left")).right.exit)(fails(isLeft(equalTo("Left"))))
      },
      testM("on failure") {
        assertM(ZIO.fail("Fail").right.exit)(fails(isRight(equalTo("Fail"))))
      } @@ zioTag(errors)
    ),
    suite("refineToOrDie")(
      testM("does not compile when refined type is not subtype of error type") {
        val result = typeCheck {
          """
          ZIO
            .fail(new RuntimeException("BOO!"))
            .refineToOrDie[Error]
            """
        }
        val expected =
          "type arguments [Error] do not conform to method refineToOrDie's type parameter bounds [E1 <: RuntimeException]"
        assertM(result)(isLeft(equalTo(expected)))
      } @@ scala2Only
    ) @@ zioTag(errors),
    suite("some")(
      testM("extracts the value from Some") {
        val task: IO[Option[Throwable], Int] = Task(Some(1)).some
        assertM(task)(equalTo(1))
      },
      testM("make a task from a defined option") {
        assertM(Task.getOrFail(Some(1)))(equalTo(1))
      },
      testM("make a task from an empty option") {
        assertM(Task.getOrFail(None).exit)(fails(isSubtype[NoSuchElementException](anything)))
      } @@ zioTag(errors),
      testM("fails on None") {
        val task: IO[Option[Throwable], Int] = Task(None).some
        assertM(task.exit)(fails(isNone))
      } @@ zioTag(errors),
      testM("fails when given an exception") {
        val ex                               = new RuntimeException("Failed Task")
        val task: IO[Option[Throwable], Int] = Task.fail(ex).some
        assertM(task.exit)(fails(isSome(equalTo(ex))))
      } @@ zioTag(errors)
    ),
    suite("someOrFailException")(
      testM("extracts the optional value") {
        assertM(ZIO.some(42).someOrFailException)(equalTo(42))
      },
      testM("fails when given a None") {
        val task = ZIO.succeed(Option.empty[Int]).someOrFailException
        assertM(task.exit)(fails(isSubtype[NoSuchElementException](anything)))
      } @@ zioTag(errors),
      suite("without another error type")(
        testM("succeed something") {
          assertM(ZIO.succeed(Option(3)).someOrFailException)(equalTo(3))
        },
        testM("succeed nothing") {
          assertM(ZIO.succeed(None: Option[Int]).someOrFailException.exit)(fails(Assertion.anything))
        } @@ zioTag(errors)
      ),
      suite("with throwable as base error type")(
        testM("return something") {
          assertM(Task(Option(3)).someOrFailException)(equalTo(3))
        }
      ),
      suite("with exception as base error type")(
        testM("return something") {
          assertM((ZIO.succeed(Option(3)): IO[Exception, Option[Int]]).someOrFailException)(equalTo(3))
        }
      )
    ),
    suite("reject")(
      testM("returns failure ignoring value") {
        val goodCase =
          exactlyOnce(0)(_.reject({ case v if v != 0 => "Partial failed!" })).sandbox.either

        val badCase =
          exactlyOnce(1)(_.reject({ case v if v != 0 => "Partial failed!" })).sandbox.either
            .map(_.left.map(_.failureOrCause))

        assertM(goodCase)(isRight(equalTo(0))) &&
        assertM(badCase)(isLeft(isLeft(equalTo("Partial failed!"))))
      }
    ) @@ zioTag(errors),
    suite("rejectZIO")(
      testM("Check `rejectZIO` returns failure ignoring value") {
        val goodCase =
          exactlyOnce(0)(
            _.rejectZIO[Any, String]({ case v if v != 0 => ZIO.succeed("Partial failed!") })
          ).sandbox.either

        val partialBadCase =
          exactlyOnce(1)(_.rejectZIO({ case v if v != 0 => ZIO.fail("Partial failed!") })).sandbox.either
            .map(_.left.map(_.failureOrCause))

        val badCase =
          exactlyOnce(1)(_.rejectZIO({ case v if v != 0 => ZIO.fail("Partial failed!") })).sandbox.either
            .map(_.left.map(_.failureOrCause))

        assertM(goodCase)(isRight(equalTo(0))) &&
        assertM(partialBadCase)(isLeft(isLeft(equalTo("Partial failed!")))) &&
        assertM(badCase)(isLeft(isLeft(equalTo("Partial failed!"))))
      }
    ),
    suite("RTS synchronous correctness")(
      testM("widen Nothing") {
        val op1 = IO.succeed[String]("1")
        val op2 = IO.succeed[String]("2")

        assertM(op1.zipWith(op2)(_ + _))(equalTo("12"))
      },
      testM("succeed must be lazy") {
        val io =
          try {
            IO.succeed(throw ExampleError)
            IO.succeed(true)
          } catch {
            case _: Throwable => IO.succeed(false)
          }

        assertM(io)(isTrue)
      },
      testM("suspend must be lazy") {
        val io =
          try {
            IO.suspend(throw ExampleError)
            IO.succeed(false)
          } catch {
            case _: Throwable => IO.succeed(true)
          }

        assertM(io)(isFalse)
      },
      testM("suspendSucceed must not catch throwable") {
        val io = ZIO.suspendSucceed[Any, Nothing, Any](throw ExampleError).sandbox.either
        assertM(io)(isLeft(equalTo(Cause.die(ExampleError))))
      },
      testM("suspend must catch throwable") {
        val io = ZIO.suspend[Any, Nothing](throw ExampleError).either
        assertM(io)(isLeft(equalTo(ExampleError)))
      },
      testM("suspendWith must catch throwable") {
        val io = ZIO.suspendWith[Any, Nothing]((_, _) => throw ExampleError).either
        assertM(io)(isLeft(equalTo(ExampleError)))
      },
      testM("suspendSucceed must be evaluatable") {
        assertM(IO.suspendSucceed(IO.succeed(42)))(equalTo(42))
      },
      testM("point, bind, map") {
        def fibIo(n: Int): Task[BigInt] =
          if (n <= 1) IO.succeed(n)
          else
            for {
              a <- fibIo(n - 1)
              b <- fibIo(n - 2)
            } yield a + b

        assertM(fibIo(10))(equalTo(fib(10)))
      },
      testM("effect, bind, map") {
        def fibIo(n: Int): Task[BigInt] =
          if (n <= 1) IO.attempt(n)
          else
            for {
              a <- fibIo(n - 1)
              b <- fibIo(n - 2)
            } yield a + b

        assertM(fibIo(10))(equalTo(fib(10)))
      },
      testM("effect, bind, map, redeem") {
        def fibIo(n: Int): Task[BigInt] =
          if (n <= 1) Task.attempt[BigInt](throw ExampleError).catchAll(_ => Task.attempt(n))
          else
            for {
              a <- fibIo(n - 1)
              b <- fibIo(n - 2)
            } yield a + b

        assertM(fibIo(10))(equalTo(fib(10)))
      },
      testM("sync effect") {
        def sumIo(n: Int): Task[Int] =
          if (n <= 0) IO.succeed(0)
          else IO.succeed(n).flatMap(b => sumIo(n - 1).map(a => a + b))

        assertM(sumIo(1000))(equalTo(sum(1000)))
      },
      testM("deep effects") {
        def incLeft(n: Int, ref: Ref[Int]): Task[Int] =
          if (n <= 0) ref.get
          else incLeft(n - 1, ref) <* ref.update(_ + 1)

        def incRight(n: Int, ref: Ref[Int]): Task[Int] =
          if (n <= 0) ref.get
          else ref.update(_ + 1) *> incRight(n - 1, ref)

        val l =
          for {
            ref <- Ref.make(0)
            v   <- incLeft(100, ref)
          } yield v == 0

        val r =
          for {
            ref <- Ref.make(0)
            v   <- incRight(1000, ref)
          } yield v == 1000

        assertM(l.zipWith(r)(_ && _))(isTrue)
      },
      testM("flip must make error into value") {
        val io = IO.fail(ExampleError).flip
        assertM(io)(equalTo(ExampleError))
      } @@ zioTag(errors),
      testM("flip must make value into error") {
        val io = IO.succeed(42).flip
        assertM(io.either)(isLeft(equalTo(42)))
      } @@ zioTag(errors),
      testM("flipping twice returns identical value") {
        val io = IO.succeed(42)
        assertM(io.flip.flip)(equalTo(42))
      }
    ),
    suite("RTS failure")(
      testM("error in sync effect") {
        val io = IO.attempt[Unit](throw ExampleError).fold[Option[Throwable]](Some(_), _ => None)
        assertM(io)(isSome(equalTo(ExampleError)))
      } @@ zioTag(errors),
      testM("attempt . fail") {
        val io1 = TaskExampleError.either
        val io2 = IO.suspendSucceed(IO.suspendSucceed(TaskExampleError).either)

        io1.zipWith(io2) { case (r1, r2) =>
          assert(r1)(isLeft(equalTo(ExampleError))) && assert(r2)(isLeft(equalTo(ExampleError)))
        }
      } @@ zioTag(errors),
      testM("deep attempt sync effect error") {
        assertM(deepErrorEffect(100).either)(isLeft(equalTo(ExampleError)))
      } @@ zioTag(errors),
      testM("deep attempt fail error") {
        assertM(deepErrorFail(100).either)(isLeft(equalTo(ExampleError)))
      } @@ zioTag(errors),
      testM("attempt . sandbox . terminate") {
        val io = IO.succeed[Int](throw ExampleError).sandbox.either
        assertM(io)(isLeft(equalTo(Cause.die(ExampleError))))
      } @@ zioTag(errors),
      testM("fold . sandbox . terminate") {
        val io = IO.succeed[Int](throw ExampleError).sandbox.fold(Some(_), Function.const(None))
        assertM(io)(isSome(equalTo(Cause.die(ExampleError))))
      } @@ zioTag(errors),
      testM("catch sandbox terminate") {
        val io = IO.succeed(throw ExampleError).sandbox.merge
        assertM(io)(equalTo(Cause.die(ExampleError)))
      } @@ zioTag(errors),
      testM("uncaught fail") {
        assertM(TaskExampleError.exit)(fails(equalTo(ExampleError)))
      } @@ zioTag(errors),
      testM("uncaught sync effect error") {
        val io = IO.succeed[Int](throw ExampleError)
        assertM(io.exit)(dies(equalTo(ExampleError)))
      } @@ zioTag(errors),
      testM("deep uncaught sync effect error") {
        assertM(deepErrorEffect(100).exit)(fails(equalTo(ExampleError)))
      } @@ zioTag(errors),
      testM("catch failing finalizers with fail") {
        val io = IO
          .fail(ExampleError)
          .ensuring(IO.succeed(throw InterruptCause1))
          .ensuring(IO.succeed(throw InterruptCause2))
          .ensuring(IO.succeed(throw InterruptCause3))

        val expectedCause = Cause.fail(ExampleError) ++
          Cause.die(InterruptCause1) ++
          Cause.die(InterruptCause2) ++
          Cause.die(InterruptCause3)

        assertM(io.exit)(equalTo(Exit.failCause(expectedCause)))
      } @@ zioTag(errors),
      testM("catch failing finalizers with terminate") {
        val io = IO
          .die(ExampleError)
          .ensuring(IO.succeed(throw InterruptCause1))
          .ensuring(IO.succeed(throw InterruptCause2))
          .ensuring(IO.succeed(throw InterruptCause3))

        val expectedCause = Cause.die(ExampleError) ++
          Cause.die(InterruptCause1) ++
          Cause.die(InterruptCause2) ++
          Cause.die(InterruptCause3)

        assertM(io.exit)(equalTo(Exit.failCause(expectedCause)))
      } @@ zioTag(errors),
      testM("run preserves interruption status") {
        for {
          p    <- Promise.make[Nothing, Unit]
          f    <- (p.succeed(()) *> IO.never).exit.fork
          _    <- p.await
          _    <- f.interrupt
          test <- f.await.map(_.interrupted)
        } yield assert(test)(isTrue)
      } @@ zioTag(interruption),
      testM("run swallows inner interruption") {
        for {
          p   <- Promise.make[Nothing, Int]
          _   <- IO.interrupt.exit *> p.succeed(42)
          res <- p.await
        } yield assert(res)(equalTo(42))
      } @@ zioTag(interruption),
      testM("timeout a long computation") {
        val io = (Clock.sleep(5.seconds) *> IO.succeed(true)).timeout(10.millis)
        assertM(Live.live(io))(isNone)
      },
      testM("timeout a long computation with an error") {
        val io = (Clock.sleep(5.seconds) *> IO.succeed(true)).timeoutFail(false)(10.millis)
        assertM(Live.live(io.exit))(fails(equalTo(false)))
      },
      testM("timeout a long computation with a cause") {
        val cause = Cause.die(new Error("BOOM"))
        val io    = (Clock.sleep(5.seconds) *> IO.succeed(true)).timeoutFailCause(cause)(10.millis)
        assertM(Live.live(io.sandbox.flip))(equalTo(cause))
      },
      testM("timeout repetition of uninterruptible effect") {
        val effect = ZIO.unit.uninterruptible.forever

        assertM(Live.live(effect.timeout(1.second)))(isNone)
      } @@ jvmOnly @@ zioTag(interruption),
      testM("timeout in uninterruptible region") {
        val effect = (ZIO.unit.timeout(Duration.Infinity)).uninterruptible
        assertM(effect)(isSome(isUnit))
      } @@ zioTag(interruption),
      testM("catchAllCause") {
        val io =
          for {
            _ <- ZIO.succeed(42)
            f <- ZIO.fail("Uh oh!")
          } yield f

        assertM(io.catchAllCause(ZIO.succeed(_)))(equalTo(Cause.fail("Uh oh!")))
      } @@ zioTag(errors),
      testM("exception in fromFuture does not kill fiber") {
        val io = ZIO.fromFuture(_ => throw ExampleError).either
        assertM(io)(isLeft(equalTo(ExampleError)))
      } @@ zioTag(errors, future)
    ),
    suite("RTS finalizers")(
      testM("fail ensuring") {
        var finalized = false

        val io = Task.fail(ExampleError).ensuring(IO.succeed { finalized = true; () })

        for {
          a1 <- assertM(io.exit)(fails(equalTo(ExampleError)))
          a2  = assert(finalized)(isTrue)
        } yield a1 && a2
      } @@ zioTag(errors),
      testM("fail on error") {
        @volatile var finalized = false

        val cleanup: Cause[Throwable] => UIO[Unit] =
          _ => IO.succeed[Unit] { finalized = true; () }

        val io = Task.fail(ExampleError).onError(cleanup)

        for {
          a1 <- assertM(io.exit)(fails(equalTo(ExampleError)))
          a2  = assert(finalized)(isTrue)
        } yield a1 && a2
      } @@ zioTag(errors),
      testM("finalizer errors not caught") {
        val e2 = new Error("e2")
        val e3 = new Error("e3")

        val io = TaskExampleError.ensuring(IO.die(e2)).ensuring(IO.die(e3))

        val expectedCause: Cause[Throwable] =
          Cause.Then(Cause.fail(ExampleError), Cause.Then(Cause.die(e2), Cause.die(e3)))

        assertM(io.sandbox.flip)(equalTo(expectedCause))
      } @@ zioTag(errors),
      testM("finalizer errors reported") {
        @volatile var reported: Exit[Nothing, Int] = null

        val io = IO
          .succeed[Int](42)
          .ensuring(IO.die(ExampleError))
          .fork
          .flatMap(_.await.flatMap[Any, Nothing, Any](e => UIO.succeed { reported = e }))

        for {
          a1 <- assertM(io)(anything)
          a2  = assert(reported.succeeded)(isFalse)
        } yield a1 && a2
      } @@ zioTag(errors),
      testM("acquireReleaseWith exit is usage result") {
        val io = IO.acquireReleaseWith(IO.unit)(_ => IO.unit)(_ => IO.succeed[Int](42))
        assertM(io)(equalTo(42))
      },
      testM("error in just acquisition") {
        val io = IO.acquireReleaseWith(TaskExampleError)(_ => IO.unit)(_ => IO.unit)
        assertM(io.exit)(fails(equalTo(ExampleError)))
      } @@ zioTag(errors),
      testM("error in just release") {
        val io = IO.acquireReleaseWith(IO.unit)(_ => IO.die(ExampleError))(_ => IO.unit)
        assertM(io.exit)(dies(equalTo(ExampleError)))
      } @@ zioTag(errors),
      testM("error in just usage") {
        val io = IO.acquireReleaseWith(IO.unit)(_ => IO.unit)(_ => IO.fail(ExampleError))
        assertM(io.exit)(fails(equalTo(ExampleError)))
      } @@ zioTag(errors),
      testM("rethrown caught error in acquisition") {
        val io = IO.absolve(IO.acquireReleaseWith(TaskExampleError)(_ => IO.unit)(_ => IO.unit).either)
        assertM(io.flip)(equalTo(ExampleError))
      } @@ zioTag(errors),
      testM("rethrown caught error in release") {
        val io = IO.acquireReleaseWith(IO.unit)(_ => IO.die(ExampleError))(_ => IO.unit)
        assertM(io.exit)(dies(equalTo(ExampleError)))
      } @@ zioTag(errors),
      testM("rethrown caught error in usage") {
        val io = IO.absolve(IO.unit.acquireRelease(IO.unit)(TaskExampleError).either)
        assertM(io.exit)(fails(equalTo(ExampleError)))
      } @@ zioTag(errors),
      testM("test eval of async fail") {
        val io1 = IO.unit.acquireRelease(AsyncUnit[Nothing])(asyncExampleError[Unit])
        val io2 = AsyncUnit[Throwable].acquireRelease(IO.unit)(asyncExampleError[Unit])

        for {
          a1 <- assertM(io1.exit)(fails(equalTo(ExampleError)))
          a2 <- assertM(io2.exit)(fails(equalTo(ExampleError)))
          a3 <- assertM(IO.absolve(io1.either).exit)(fails(equalTo(ExampleError)))
          a4 <- assertM(IO.absolve(io2.either).exit)(fails(equalTo(ExampleError)))
        } yield a1 && a2 && a3 && a4
      },
      testM("acquireReleaseWith regression 1") {
        def makeLogger: Ref[List[String]] => String => UIO[Unit] =
          (ref: Ref[List[String]]) => (line: String) => ref.update(_ ::: List(line))

        val io =
          for {
            ref <- Ref.make[List[String]](Nil)
            log  = makeLogger(ref)
            f <- ZIO
                   .acquireReleaseWith(
                     ZIO.acquireReleaseWith(ZIO.unit)(_ =>
                       log("start 1") *> Clock.sleep(10.millis) *> log("release 1")
                     )(_ => ZIO.unit)
                   )(_ => log("start 2") *> Clock.sleep(10.millis) *> log("release 2"))(_ => ZIO.unit)
                   .fork
            _ <- (ref.get <* Clock.sleep(1.millis)).repeatUntil(_.contains("start 1"))
            _ <- f.interrupt
            _ <- (ref.get <* Clock.sleep(1.millis)).repeatUntil(_.contains("release 2"))
            l <- ref.get
          } yield l

        assertM(Live.live(io))(hasSameElements(List("start 1", "release 1", "start 2", "release 2")))
      } @@ zioTag(regression) @@ jvmOnly,
      testM("interrupt waits for finalizer") {
        val io =
          for {
            r  <- Ref.make(false)
            p1 <- Promise.make[Nothing, Unit]
            p2 <- Promise.make[Nothing, Int]
            s <- (p1.succeed(()) *> p2.await)
                   .ensuring(r.set(true) *> Clock.sleep(10.millis))
                   .fork
            _    <- p1.await
            _    <- s.interrupt
            test <- r.get
          } yield test

        assertM(Live.live(io))(isTrue)
      } @@ zioTag(interruption)
    ),
    suite("RTS synchronous stack safety")(
      testM("deep map of now") {
        assertM(deepMapNow(10000))(equalTo(10000))
      },
      testM("deep map of sync effect") {
        assertM(deepMapEffect(10000))(equalTo(10000))
      },
      testM("deep attempt") {
        val io = (0 until 10000).foldLeft(IO.attempt(()))((acc, _) => acc.either.unit)
        assertM(io)(equalTo(()))
      },
      testM("deep flatMap") {
        def fib(n: Int, a: BigInt = 0, b: BigInt = 1): IO[Error, BigInt] =
          IO.succeed(a + b).flatMap { b2 =>
            if (n > 0)
              fib(n - 1, b, b2)
            else
              IO.succeed(b2)
          }

        val expected = BigInt(
          "113796925398360272257523782552224175572745930353730513145086634176691092536145985470146129334641866902783673042322088625863396052888690096969577173696370562180400527049497109023054114771394568040040412172632376"
        )

        assertM(fib(1000))(equalTo(expected))
      },
      testM("deep absolve/attempt is identity") {
        implicit val canFail = CanFail
        val io               = (0 until 1000).foldLeft(IO.succeed(42))((acc, _) => IO.absolve(acc.either))

        assertM(io)(equalTo(42))
      },
      testM("deep async absolve/attempt is identity") {
        val io = (0 until 1000).foldLeft(IO.async[Int, Int](k => k(IO.succeed(42)))) { (acc, _) =>
          IO.absolve(acc.either)
        }

        assertM(io)(equalTo(42))
      }
    ),
    suite("RTS asynchronous correctness")(
      testM("simple async must return") {
        val io = IO.async[Throwable, Int](k => k(IO.succeed(42)))
        assertM(io)(equalTo(42))
      },
      testM("simple asyncZIO must return") {
        val io = IO.asyncZIO[Throwable, Int](k => IO.succeed(k(IO.succeed(42))))
        assertM(io)(equalTo(42))
      },
      testM("deep asyncZIO doesn't block threads") {
        def stackIOs(count: Int): URIO[Has[Clock], Int] =
          if (count <= 0) IO.succeed(42)
          else asyncIO(stackIOs(count - 1))

        def asyncIO(cont: URIO[Has[Clock], Int]): URIO[Has[Clock], Int] =
          ZIO.asyncZIO[Has[Clock], Nothing, Int] { k =>
            Clock.sleep(5.millis) *> cont *> IO.succeed(k(IO.succeed(42)))
          }

        val procNum = java.lang.Runtime.getRuntime.availableProcessors()

        val io = stackIOs(procNum + 1)

        assertM(Live.live(io))(equalTo(42))
      } @@ jvmOnly,
      testM("interrupt of asyncZIO register") {
        for {
          release <- Promise.make[Nothing, Unit]
          acquire <- Promise.make[Nothing, Unit]
          fiber <- IO
                     .asyncZIO[Nothing, Unit] { _ =>
                       // This will never complete because we never call the callback
                       acquire.succeed(()).acquireRelease(release.succeed(()))(IO.never)
                     }
                     .disconnect
                     .fork
          _ <- acquire.await
          _ <- fiber.interruptFork
          a <- release.await
        } yield assert(a)(isUnit)
      } @@ zioTag(interruption) @@ ignore,
      testM("async should not resume fiber twice after interruption") {
        for {
          step            <- Promise.make[Nothing, Unit]
          unexpectedPlace <- Ref.make(List.empty[Int])
          runtime         <- ZIO.runtime[Has[Live]]
          fork <- ZIO
                    .async[Any, Nothing, Unit] { k =>
                      runtime.unsafeRunAsync {
                        step.await *> ZIO.succeed(k(unexpectedPlace.update(1 :: _)))
                      }
                    }
                    .ensuring(ZIO.async[Any, Nothing, Unit] { _ =>
                      runtime.unsafeRunAsync {
                        step.succeed(())
                      }
                    //never complete
                    })
                    .ensuring(unexpectedPlace.update(2 :: _))
                    .forkDaemon
          result     <- Live.withLive(fork.interrupt)(_.timeout(5.seconds))
          unexpected <- unexpectedPlace.get
        } yield {
          assert(unexpected)(isEmpty) &&
          assert(result)(isNone) // timeout happens
        }
      } @@ zioTag(interruption) @@ flaky,
      testM("asyncMaybe should not resume fiber twice after synchronous result") {
        for {
          step            <- Promise.make[Nothing, Unit]
          unexpectedPlace <- Ref.make(List.empty[Int])
          runtime         <- ZIO.runtime[Has[Live]]
          fork <- ZIO
                    .asyncMaybe[Any, Nothing, Unit] { k =>
                      runtime.unsafeRunAsync {
                        step.await *> ZIO.succeed(k(unexpectedPlace.update(1 :: _)))
                      }
                      Some(IO.unit)
                    }
                    .flatMap { _ =>
                      ZIO.async[Any, Nothing, Unit] { _ =>
                        runtime.unsafeRunAsync {
                          step.succeed(())
                        }
                      //never complete
                      }
                    }
                    .ensuring(unexpectedPlace.update(2 :: _))
                    .uninterruptible
                    .forkDaemon
          result     <- Live.withLive(fork.interrupt)(_.timeout(5.seconds))
          unexpected <- unexpectedPlace.get
        } yield {
          assert(unexpected)(isEmpty) &&
          assert(result)(isNone) // timeout happens
        }
      } @@ flaky,
      testM("sleep 0 must return") {
        assertM(Live.live(Clock.sleep(1.nanos)))(isUnit)
      },
      testM("shallow bind of async chain") {
        val io = (0 until 10).foldLeft[Task[Int]](IO.succeed[Int](0)) { (acc, _) =>
          acc.flatMap(n => IO.async[Throwable, Int](_(IO.succeed(n + 1))))
        }

        assertM(io)(equalTo(10))
      },
      testM("asyncZIO can fail before registering") {
        val zio = ZIO
          .asyncZIO[Any, String, Nothing](_ => ZIO.fail("Ouch"))
          .flip

        assertM(zio)(equalTo("Ouch"))
      } @@ zioTag(errors),
      testM("asyncZIO can defect before registering") {
        val zio = ZIO
          .asyncZIO[Any, String, Unit](_ => ZIO.succeed(throw new Error("Ouch")))
          .exit
          .map(_.fold(_.defects.headOption.map(_.getMessage), _ => None))

        assertM(zio)(isSome(equalTo("Ouch")))
      } @@ zioTag(errors)
    ),
    suite("RTS concurrency correctness")(
      testM("shallow fork/join identity") {
        for {
          f <- IO.succeed(42).fork
          r <- f.join
        } yield assert(r)(equalTo(42))
      },
      testM("deep fork/join identity") {
        val n = 20
        assertM(concurrentFib(n))(equalTo(fib(n)))
      } @@ jvmOnly,
      testM("asyncZIO creation is interruptible") {
        for {
          release <- Promise.make[Nothing, Int]
          acquire <- Promise.make[Nothing, Unit]
          task = IO.asyncZIO[Nothing, Unit] { _ =>
                   // This will never complete because the callback is never invoked
                   IO.acquireReleaseWith(acquire.succeed(()))(_ => release.succeed(42).unit)(_ => IO.never)
                 }
          fiber <- task.fork
          _     <- acquire.await
          _     <- fiber.interrupt
          a     <- release.await
        } yield assert(a)(equalTo(42))
      } @@ zioTag(interruption) @@ ignore,
      testM("asyncInterrupt runs cancel token on interrupt") {
        for {
          release <- Promise.make[Nothing, Int]
          latch    = scala.concurrent.Promise[Unit]()
          async    = IO.asyncInterrupt[Nothing, Nothing] { _ => latch.success(()); Left(release.succeed(42).unit) }
          fiber   <- async.fork
          _ <- IO.async[Throwable, Unit] { k =>
                 latch.future.onComplete {
                   case Success(a) => k(IO.succeed(a))
                   case Failure(t) => k(IO.fail(t))
                 }(scala.concurrent.ExecutionContext.global)
               }
          _      <- fiber.interrupt
          result <- release.await
        } yield assert(result)(equalTo(42))
      } @@ zioTag(interruption),
      testM("daemon fiber is unsupervised") {
        def child(ref: Ref[Boolean]) = withLatch(release => (release *> UIO.never).ensuring(ref.set(true)))

        for {
          ref   <- Ref.make[Boolean](false)
          fiber <- child(ref).forkDaemon.fork
          _     <- fiber.join
          b     <- ref.get
        } yield assert(b)(isFalse)
      } @@ zioTag(supervision),
      testM("daemon fiber race interruption") {
        def plus1(latch: Promise[Nothing, Unit], finalizer: UIO[Any]) =
          (latch.succeed(()) *> ZIO.sleep(1.hour)).onInterrupt(finalizer)

        (for {
          interruptionRef <- Ref.make(0)
          latch1Start     <- Promise.make[Nothing, Unit]
          latch2Start     <- Promise.make[Nothing, Unit]
          inc              = interruptionRef.update(_ + 1)
          left             = plus1(latch1Start, inc)
          right            = plus1(latch2Start, inc)
          fiber           <- (left race right).fork
          _               <- latch1Start.await *> latch2Start.await *> fiber.interrupt
          interrupted     <- interruptionRef.get
        } yield assert(interrupted)(equalTo(2)))
      } @@ zioTag(interruption) @@ flaky,
      testM("race in daemon is executed") {
        for {
          latch1 <- Promise.make[Nothing, Unit]
          latch2 <- Promise.make[Nothing, Unit]
          p1     <- Promise.make[Nothing, Unit]
          p2     <- Promise.make[Nothing, Unit]
          loser1  = ZIO.acquireReleaseWith(latch1.succeed(()))(_ => p1.succeed(()))(_ => ZIO.infinity)
          loser2  = ZIO.acquireReleaseWith(latch2.succeed(()))(_ => p2.succeed(()))(_ => ZIO.infinity)
          fiber  <- (loser1 race loser2).forkDaemon
          _      <- latch1.await
          _      <- latch2.await
          _      <- fiber.interrupt
          res1   <- p1.await
          res2   <- p2.await
        } yield assert(res1)(isUnit) && assert(res2)(isUnit)
      },
      testM("supervise fibers") {
        def makeChild(n: Int): URIO[Has[Clock], Fiber[Nothing, Unit]] =
          (Clock.sleep(20.millis * n.toDouble) *> ZIO.infinity).fork

        val io =
          for {
            counter <- Ref.make(0)
            _ <- (makeChild(1) *> makeChild(2)).ensuringChildren { fs =>
                   fs.foldLeft(IO.unit)((acc, f) => acc *> f.interrupt *> counter.update(_ + 1))
                 }
            value <- counter.get
          } yield value

        assertM(Live.live(io))(equalTo(2))
      } @@ zioTag(supervision) @@ forked @@ flaky, // Due to weak supervision, this test is expected to fail sometimes
      testM("race of fail with success") {
        val io = IO.fail(42).race(IO.succeed(24)).either
        assertM(io)(isRight(equalTo(24)))
      },
      testM("race of terminate with success") {
        val io = IO.die(new Throwable {}).race(IO.succeed(24))
        assertM(io)(equalTo(24))
      },
      testM("race of fail with fail") {
        val io = IO.fail(42).race(IO.fail(42)).either
        assertM(io)(isLeft(equalTo(42)))
      },
      testM("race of value & never") {
        val io = IO.succeed(42).race(IO.never)
        assertM(io)(equalTo(42))
      },
      testM("race in uninterruptible region") {
        val effect = (ZIO.unit.race(ZIO.infinity)).uninterruptible
        assertM(effect)(isUnit)
      },
      testM("race of two forks does not interrupt winner") {
        for {
          ref    <- Ref.make(0)
          fibers <- Ref.make(Set.empty[Fiber[Any, Any]])
          latch  <- Promise.make[Nothing, Unit]
          scope  <- ZIO.descriptor.map(_.scope)
          effect = ZIO.uninterruptibleMask { restore =>
                     restore(latch.await.onInterrupt(ref.update(_ + 1))).forkIn(scope).tap(f => fibers.update(_ + f))
                   }
          awaitAll = fibers.get.flatMap(Fiber.awaitAll(_))
          _       <- effect race effect
          value2  <- latch.succeed(()) *> awaitAll *> ref.get
        } yield assert(value2)(isLessThanEqualTo(1))
      },
      testM("firstSuccessOf of values") {
        val io = IO.firstSuccessOf(IO.fail(0), List(IO.succeed(100))).either
        assertM(io)(isRight(equalTo(100)))
      },
      testM("firstSuccessOf of failures") {
        val io = ZIO.firstSuccessOf(IO.fail(0).delay(10.millis), List(IO.fail(101))).either
        assertM(Live.live(io))(isLeft(equalTo(101)))
      },
      testM("firstSuccessOF of failures & 1 success") {
        val io = ZIO.firstSuccessOf(IO.fail(0), List(IO.succeed(102).delay(1.millis))).either
        assertM(Live.live(io))(isRight(equalTo(102)))
      },
      testM("raceFirst interrupts loser on success") {
        for {
          s      <- Promise.make[Nothing, Unit]
          effect <- Promise.make[Nothing, Int]
          winner  = s.await *> IO.fromEither(Right(()))
          loser   = ZIO.acquireReleaseWith(s.succeed(()))(_ => effect.succeed(42))(_ => ZIO.infinity)
          race    = winner raceFirst loser
          _      <- race
          b      <- effect.await
        } yield assert(b)(equalTo(42))
      } @@ zioTag(interruption),
      testM("raceFirst interrupts loser on failure") {
        for {
          s      <- Promise.make[Nothing, Unit]
          effect <- Promise.make[Nothing, Int]
          winner  = s.await *> IO.fromEither(Left(new Exception))
          loser   = ZIO.acquireReleaseWith(s.succeed(()))(_ => effect.succeed(42))(_ => ZIO.infinity)
          race    = winner raceFirst loser
          _      <- race.either
          b      <- effect.await
        } yield assert(b)(equalTo(42))
      } @@ zioTag(interruption),
      testM("mergeAll") {
        val io = IO.mergeAll(List("a", "aa", "aaa", "aaaa").map(IO.succeed[String](_)))(0)((b, a) => b + a.length)

        assertM(io)(equalTo(10))
      },
      testM("mergeAllEmpty") {
        val io = IO.mergeAll(List.empty[UIO[Int]])(0)(_ + _)
        assertM(io)(equalTo(0))
      },
      testM("reduceAll") {
        val io = IO.reduceAll(IO.succeed(1), List(2, 3, 4).map(IO.succeed[Int](_)))(_ + _)
        assertM(io)(equalTo(10))
      },
      testM("reduceAll Empty List") {
        val io = IO.reduceAll(IO.succeed(1), Seq.empty)(_ + _)
        assertM(io)(equalTo(1))
      },
      testM("timeout of failure") {
        val io = IO.fail("Uh oh").timeout(1.hour)
        assertM(Live.live(io).exit)(fails(equalTo("Uh oh")))
      },
      testM("timeout of terminate") {
        val io: ZIO[Has[Clock], Nothing, Option[Int]] = IO.die(ExampleError).timeout(1.hour)
        assertM(Live.live(io).exit)(dies(equalTo(ExampleError)))
      }
    ),
    suite("RTS option tests")(
      testM("lifting a value to an option") {
        assertM(ZIO.some(42))(isSome(equalTo(42)))
      },
      testM("using the none value") {
        assertM(ZIO.none)(isNone)
      }
    ),
    suite("RTS either helper tests")(
      testM("lifting a value into right") {
        assertM(ZIO.right(42))(isRight(equalTo(42)))
      },
      testM("lifting a value into left") {
        assertM(ZIO.left(42))(isLeft(equalTo(42)))
      }
    ),
    suite("RTS interruption")(
      testM("sync forever is interruptible") {
        val io =
          for {
            f <- IO.succeed[Int](1).forever.fork
            _ <- f.interrupt
          } yield true

        assertM(io)(isTrue)
      },
      testM("interrupt of never") {
        val io =
          for {
            fiber <- IO.never.fork
            _     <- fiber.interrupt
          } yield 42

        assertM(io)(equalTo(42))
      },
      testM("asyncZIO is interruptible") {
        val io =
          for {
            fiber <- IO.asyncZIO[Nothing, Nothing](_ => IO.never).fork
            _     <- fiber.interrupt
          } yield 42

        assertM(io)(equalTo(42))
      },
      testM("async is interruptible") {
        val io =
          for {
            fiber <- IO.async[Nothing, Nothing](_ => ()).fork
            _     <- fiber.interrupt
          } yield 42

        assertM(io)(equalTo(42))
      },
      testM("acquireReleaseWith is uninterruptible") {
        val io =
          for {
            promise <- Promise.make[Nothing, Unit]
            fiber   <- (promise.succeed(()) <* IO.never).acquireReleaseWith(_ => IO.unit)(_ => IO.unit).forkDaemon
            res     <- promise.await *> fiber.interrupt.timeoutTo(42)(_ => 0)(1.second)
          } yield res

        assertM(Live.live(io))(equalTo(42))
      },
      testM("acquireReleaseExitWith is uninterruptible") {
        val io =
          for {
            promise <- Promise.make[Nothing, Unit]
            fiber <-
              IO
                .acquireReleaseExitWith(promise.succeed(()) *> IO.never *> IO.succeed(1))((_, _: Exit[Any, Any]) =>
                  IO.unit
                )(_ => IO.unit: IO[Nothing, Unit])
                .forkDaemon
            res <- promise.await *> fiber.interrupt.timeoutTo(42)(_ => 0)(1.second)
          } yield res

        assertM(Live.live(io))(equalTo(42))
      },
      testM("acquireReleaseWith use is interruptible") {
        for {
          fiber <- IO.unit.acquireReleaseWith(_ => IO.unit)(_ => IO.never).fork
          res   <- fiber.interrupt
        } yield assert(res)(isInterrupted)
      },
      testM("acquireReleaseExitWith use is interruptible") {
        for {
          fiber <- IO.acquireReleaseExitWith(IO.unit)((_, _: Exit[Any, Any]) => IO.unit)(_ => IO.never).fork
          res   <- fiber.interrupt.timeoutTo(42)(_ => 0)(1.second)
        } yield assert(res)(equalTo(0))
      },
      testM("acquireReleaseWith release called on interrupt") {
        val io =
          for {
            p1 <- Promise.make[Nothing, Unit]
            p2 <- Promise.make[Nothing, Unit]
            fiber <-
              IO.acquireReleaseWith(IO.unit)(_ => p2.succeed(()) *> IO.unit)(_ => p1.succeed(()) *> IO.never).fork
            _ <- p1.await
            _ <- fiber.interrupt
            _ <- p2.await
          } yield ()

        assertM(io.timeoutTo(42)(_ => 0)(1.second))(equalTo(0))
      },
      testM("acquireReleaseExitWith release called on interrupt") {
        for {
          done <- Promise.make[Nothing, Unit]
          fiber <-
            withLatch { release =>
              IO.acquireReleaseExitWith(IO.unit)((_, _: Exit[Any, Any]) => done.succeed(()))(_ => release *> IO.never)
                .fork
            }

          _ <- fiber.interrupt
          r <- done.await.timeoutTo(42)(_ => 0)(60.second)
        } yield assert(r)(equalTo(0))
      },
      testM("acquireReleaseWith acquire returns immediately on interrupt") {
        for {
          p1 <- Promise.make[Nothing, Unit]
          p2 <- Promise.make[Nothing, Int]
          p3 <- Promise.make[Nothing, Unit]
          s <- (p1.succeed(()) *> p2.await)
                 .acquireReleaseWith(_ => p3.await)(_ => IO.unit)
                 .disconnect
                 .fork
          _   <- p1.await
          res <- s.interrupt
          _   <- p3.succeed(())
        } yield assert(res)(isInterrupted)
      },
      testM("acquireReleaseExitWith disconnect acquire returns immediately on interrupt") {
        for {
          p1 <- Promise.make[Nothing, Unit]
          p2 <- Promise.make[Nothing, Unit]
          p3 <- Promise.make[Nothing, Unit]
          s <- IO
                 .acquireReleaseExitWith(p1.succeed(()) *> p2.await)((_, _: Exit[Any, Any]) => p3.await)(_ =>
                   IO.unit: IO[Nothing, Unit]
                 )
                 .disconnect
                 .fork
          _   <- p1.await
          res <- s.interrupt
          _   <- p3.succeed(())
        } yield assert(res)(isInterrupted)
      },
      testM("acquireReleaseWith disconnect use is interruptible") {
        for {
          fiber <- IO.unit.acquireReleaseWith(_ => IO.unit)(_ => IO.never).disconnect.fork
          res   <- fiber.interrupt
        } yield assert(res)(isInterrupted)
      },
      testM("acquireReleaseExitWith disconnect use is interruptible") {
        for {
          fiber <- IO.acquireReleaseExitWith(IO.unit)((_, _: Exit[Any, Any]) => IO.unit)(_ => IO.never).disconnect.fork
          res   <- Live.live(fiber.interrupt.timeoutTo(42)(_ => 0)(1.second))
        } yield assert(res)(equalTo(0))
      },
      testM("acquireReleaseWith disconnect release called on interrupt in separate fiber") {
        val io =
          for {
            p1 <- Promise.make[Nothing, Unit]
            p2 <- Promise.make[Nothing, Unit]
            fiber <- IO
                       .acquireReleaseWith(IO.unit)(_ => p2.succeed(()) *> IO.unit)(_ => p1.succeed(()) *> IO.never)
                       .disconnect
                       .fork
            _ <- p1.await
            _ <- fiber.interrupt
            _ <- p2.await
          } yield ()

        assertM(Live.live(io).timeoutTo(false)(_ => true)(10.seconds))(isTrue)
      },
      testM("acquireReleaseExitWith disconnect release called on interrupt in separate fiber") {
        for {
          done <- Promise.make[Nothing, Unit]
          fiber <-
            withLatch { release =>
              IO.acquireReleaseExitWith(IO.unit)((_, _: Exit[Any, Any]) => done.succeed(()))(_ => release *> IO.never)
                .disconnect
                .fork
            }

          _ <- fiber.interrupt
          r <- Live.live(done.await.timeoutTo(false)(_ => true)(10.seconds))
        } yield assert(r)(isTrue)
      },
      testM("catchAll + ensuring + interrupt") {
        implicit val canFail = CanFail
        for {
          cont <- Promise.make[Nothing, Unit]
          p1   <- Promise.make[Nothing, Boolean]
          f1   <- (cont.succeed(()) *> IO.never).catchAll(IO.fail).ensuring(p1.succeed(true)).fork
          _    <- cont.await
          _    <- f1.interrupt
          res  <- p1.await
        } yield assert(res)(isTrue)
      },
      testM("finalizer can detect interruption") {
        for {
          p1 <- Promise.make[Nothing, Boolean]
          c  <- Promise.make[Nothing, Unit]
          f1 <- (c.succeed(()) *> ZIO.never)
                  .ensuring(IO.descriptor.flatMap(d => p1.succeed(d.interrupters.nonEmpty)))
                  .fork
          _   <- c.await
          _   <- f1.interrupt
          res <- p1.await
        } yield assert(res)(isTrue)
      },
      testM("interruption of raced") {
        for {
          ref   <- Ref.make(0)
          cont1 <- Promise.make[Nothing, Unit]
          cont2 <- Promise.make[Nothing, Unit]
          make   = (p: Promise[Nothing, Unit]) => (p.succeed(()) *> ZIO.infinity).onInterrupt(ref.update(_ + 1))
          raced <- (make(cont1) race (make(cont2))).fork
          _     <- cont1.await *> cont2.await
          _     <- raced.interrupt
          count <- ref.get
        } yield assert(count)(equalTo(2))
      },
      testM("recovery of error in finalizer") {
        for {
          recovered <- Ref.make(false)
          fiber <- withLatch { release =>
                     (release *> ZIO.never)
                       .ensuring(
                         (ZIO.unit *> ZIO.fail("Uh oh")).catchAll(_ => recovered.set(true))
                       )
                       .fork
                   }
          _     <- fiber.interrupt
          value <- recovered.get
        } yield assert(value)(isTrue)
      } @@ zioTag(errors),
      testM("recovery of interruptible") {
        for {
          recovered <- Ref.make(false)
          fiber <- withLatch { release =>
                     (release *> ZIO.never.interruptible)
                       .foldCauseZIO(
                         cause => recovered.set(cause.interrupted),
                         _ => recovered.set(false)
                       )
                       .uninterruptible
                       .fork
                   }
          _     <- fiber.interrupt
          value <- recovered.get
        } yield assert(value)(isTrue)
      },
      testM("sandbox of interruptible") {
        for {
          selfId    <- ZIO.fiberId
          recovered <- Ref.make[Option[Either[Cause[Nothing], Any]]](None)
          fiber <- withLatch { release =>
                     (release *> ZIO.never.interruptible).sandbox.either
                       .flatMap(exit => recovered.set(Some(exit)))
                       .uninterruptible
                       .fork
                   }
          _     <- fiber.interrupt
          value <- recovered.get
        } yield assert(value)(isSome(isLeft(equalTo(Cause.interrupt(selfId)))))
      },
      testM("run of interruptible") {
        for {
          recovered <- Ref.make[Option[Exit[Nothing, Any]]](None)
          fiber <- withLatch { release =>
                     (release *> ZIO.never.interruptible).exit
                       .flatMap(exit => recovered.set(Some(exit)))
                       .uninterruptible
                       .fork
                   }
          _     <- fiber.interrupt
          value <- recovered.get
        } yield assert(value)(isSome(isInterrupted))
      },
      testM("alternating interruptibility") {
        for {
          counter <- Ref.make(0)
          fiber <- withLatch { release =>
                     ((((release *> ZIO.never.interruptible.exit *> counter
                       .update(_ + 1)).uninterruptible).interruptible).exit
                       *> counter.update(_ + 1)).uninterruptible.fork
                   }
          _     <- fiber.interrupt
          value <- counter.get
        } yield assert(value)(equalTo(2))
      },
      testM("interruption after defect") {
        for {
          ref <- Ref.make(false)
          fiber <- withLatch { release =>
                     (ZIO.attempt(throw new Error).exit *> release *> ZIO.never)
                       .ensuring(ref.set(true))
                       .fork
                   }
          _     <- fiber.interrupt
          value <- ref.get
        } yield assert(value)(isTrue)
      },
      testM("interruption after defect 2") {
        for {
          ref <- Ref.make(false)
          fiber <- withLatch { release =>
                     (ZIO.attempt(throw new Error).exit *> release *> ZIO.unit.forever)
                       .ensuring(ref.set(true))
                       .fork
                   }
          _     <- fiber.interrupt
          value <- ref.get
        } yield assert(value)(isTrue)
      },
      testM("disconnect returns immediately on interrupt") {
        for {
          p1 <- Promise.make[Nothing, Unit]
          s <- (p1.succeed(()) *> ZIO.never)
                 .ensuring(ZIO.never)
                 .disconnect
                 .fork
          _   <- p1.await
          res <- s.interrupt
        } yield assert(res)(isInterrupted)
      },
      testM("disconnected effect that is then interrupted eventually performs interruption") {
        val io =
          for {
            r  <- Ref.make(false)
            p1 <- Promise.make[Nothing, Unit]
            p3 <- Promise.make[Nothing, Unit]
            s <- (p1.succeed(()) *> ZIO.never)
                   .ensuring(r.set(true) *> Clock.sleep(10.millis) *> p3.succeed(()))
                   .disconnect
                   .fork
            _    <- p1.await
            _    <- s.interrupt
            _    <- p3.await
            test <- r.get
          } yield test

        assertM(Live.live(io))(isTrue)
      },
      testM("cause reflects interruption") {
        val io =
          for {
            finished <- Ref.make(false)
            fiber    <- withLatch(release => (release *> ZIO.fail("foo")).catchAll(_ => finished.set(true)).fork)
            exit     <- fiber.interrupt
            finished <- finished.get
          } yield exit.interrupted == true || finished == true

        assertM(io)(isTrue)
      } @@ jvm(nonFlaky),
      testM("acquireRelease use inherits interrupt status") {
        val io =
          for {
            ref <- Ref.make(false)
            fiber1 <- withLatch { (release2, await2) =>
                        withLatch { release1 =>
                          release1
                            .acquireRelease(ZIO.unit, await2 *> Clock.sleep(10.millis) *> ref.set(true))
                            .uninterruptible
                            .fork
                        } <* release2
                      }
            _     <- fiber1.interrupt
            value <- ref.get
          } yield value

        assertM(Live.live(io))(isTrue)
      },
      testM("acquireRelease use inherits interrupt status 2") {
        val io =
          for {
            latch1 <- Promise.make[Nothing, Unit]
            latch2 <- Promise.make[Nothing, Unit]
            ref    <- Ref.make(false)
            fiber1 <- latch1
                        .succeed(())
                        .acquireReleaseExitWith[Has[Clock], Nothing, Unit](
                          (_: Boolean, _: Exit[Any, Any]) => ZIO.unit,
                          (_: Boolean) => latch2.await *> Clock.sleep(10.millis) *> ref.set(true).unit
                        )
                        .uninterruptible
                        .fork
            _     <- latch1.await
            _     <- latch2.succeed(())
            _     <- fiber1.interrupt
            value <- ref.get
          } yield value

        assertM(Live.live(io))(isTrue)
      },
      testM("async can be uninterruptible") {
        val io =
          for {
            ref <- Ref.make(false)
            fiber <- withLatch { release =>
                       (release *> Clock.sleep(10.millis) *> ref.set(true).unit).uninterruptible.fork
                     }
            _     <- fiber.interrupt
            value <- ref.get
          } yield value

        assertM(Live.live(io))(isTrue)
      },
      testM("closing scope is uninterruptible") {
        for {
          ref     <- Ref.make(false)
          promise <- Promise.make[Nothing, Unit]
          child    = promise.succeed(()) *> Live.live(ZIO.sleep(10.milliseconds)) *> ref.set(true)
          parent   = child.uninterruptible.fork *> promise.await
          fiber   <- parent.fork
          _       <- promise.await
          _       <- fiber.interrupt
          value   <- ref.get
        } yield assert(value)(isTrue)
      },
      testM("asyncInterrupt cancelation") {
        for {
          ref <- ZIO.succeed(new java.util.concurrent.atomic.AtomicInteger(0))
          effect = ZIO.asyncInterrupt[Any, Nothing, Any] { _ =>
                     ref.incrementAndGet()
                     Left(ZIO.succeed(ref.decrementAndGet()))
                   }
          _     <- ZIO.unit.race(effect)
          value <- ZIO.succeed(ref.get())
        } yield assert(value)(equalTo(0))
      } @@ jvm(nonFlaky(100000))
    ) @@ zioTag(interruption),
    suite("RTS environment")(
      testM("provide is modular") {
        val zio =
          for {
            v1 <- ZIO.environment[Int]
            v2 <- ZIO.environment[Int].provide(2)
            v3 <- ZIO.environment[Int]
          } yield (v1, v2, v3)

        assertM(zio.provide(4))(equalTo((4, 2, 4)))
      },
      testM("async can use environment") {
        val zio = ZIO.async[Int, Nothing, Int](cb => cb(ZIO.environment[Int]))
        assertM(zio.provide(10))(equalTo(10))
      }
    ),
    suite("RTS forking inheritability")(
      testM("interruption status is heritable") {
        for {
          latch <- Promise.make[Nothing, Unit]
          ref   <- Ref.make(InterruptStatus.interruptible)
          _     <- ZIO.uninterruptible((ZIO.checkInterruptible(ref.set) *> latch.succeed(())).fork *> latch.await)
          v     <- ref.get
        } yield assert(v)(equalTo(InterruptStatus.uninterruptible))
      } @@ zioTag(interruption),
      testM("executor is heritable") {
        val executor = internal.Executor.fromExecutionContext(100) {
          scala.concurrent.ExecutionContext.Implicits.global
        }
        val pool = ZIO.succeed(Platform.getCurrentThreadGroup)
        val io = for {
          parentPool <- pool
          childPool  <- pool.fork.flatMap(_.join)
        } yield assert(parentPool)(equalTo(childPool))
        io.lock(executor)
      } @@ jvm(nonFlaky(100))
    ),
    suite("serviceWith")(
      testM("effectfully accesses a service in the environment") {
        val zio = ZIO.serviceWith[Int](int => UIO(int + 3))
        assertM(zio.inject(ZLayer.succeed(0)))(equalTo(3))
      }
    ),
    suite("schedule")(
      testM("runs effect for each recurrence of the schedule") {
        for {
          ref     <- Ref.make[List[Duration]](List.empty)
          effect   = Clock.nanoTime.flatMap(duration => ref.update(duration.nanoseconds :: _))
          schedule = Schedule.spaced(1.second) && Schedule.recurs(5)
          _       <- effect.schedule(schedule).fork
          _       <- TestClock.adjust(5.seconds)
          value   <- ref.get.map(_.reverse)
        } yield assert(value)(equalTo(List(1.second, 2.seconds, 3.seconds, 4.seconds, 5.seconds)))
      }
    ),
    suite("someOrElse")(
      testM("extracts the value from Some") {
        assertM(UIO.succeed(Some(1)).someOrElse(2))(equalTo(1))
      },
      testM("falls back to the default value if None") {
        assertM(UIO.succeed(None).someOrElse(42))(equalTo(42))
      },
      testM("does not change failed state") {
        assertM(ZIO.fail(ExampleError).someOrElse(42).exit)(fails(equalTo(ExampleError)))
      } @@ zioTag(errors)
    ),
    suite("someOrElseZIO")(
      testM("extracts the value from Some") {
        assertM(UIO.succeed(Some(1)).someOrElseZIO(UIO.succeed(2)))(equalTo(1))
      },
      testM("falls back to the default effect if None") {
        assertM(UIO.succeed(None).someOrElseZIO(UIO.succeed(42)))(equalTo(42))
      },
      testM("does not change failed state") {
        assertM(ZIO.fail(ExampleError).someOrElseZIO(UIO.succeed(42)).exit)(fails(equalTo(ExampleError)))
      } @@ zioTag(errors)
    ),
    suite("someOrFail")(
      testM("extracts the optional value") {
        val task: Task[Int] = UIO(Some(42)).someOrFail(exampleError)
        assertM(task)(equalTo(42))
      },
      testM("fails when given a None") {
        val task: Task[Int] = UIO(Option.empty[Int]).someOrFail(exampleError)
        assertM(task.exit)(fails(equalTo(exampleError)))
      } @@ zioTag(errors)
    ),
    suite("summarized")(
      testM("returns summary and value") {
        for {
          counter  <- Ref.make(0)
          increment = counter.updateAndGet(_ + 1)
          result   <- increment.summarized(increment)((_, _))
        } yield {
          val ((start, end), value) = result
          assert(start)(equalTo(1)) &&
          assert(value)(equalTo(2)) &&
          assert(end)(equalTo(3))
        }
      }
    ),
    suite("tapCause")(
      testM("effectually peeks at the cause of the failure of this effect") {
        for {
          ref    <- Ref.make(false)
          result <- ZIO.dieMessage("die").tapCause(_ => ref.set(true)).exit
          effect <- ref.get
        } yield assert(result)(dies(hasMessage(equalTo("die")))) &&
          assert(effect)(isTrue)
      }
    ),
    suite("tapDefect")(
      testM("effectually peeks at the cause of the failure of this effect") {
        for {
          ref    <- Ref.make(false)
          result <- ZIO.dieMessage("die").tapDefect(_ => ref.set(true)).exit
          effect <- ref.get
        } yield assert(result)(dies(hasMessage(equalTo("die")))) &&
          assert(effect)(isTrue)
      }
    ),
    suite("tapEither")(
      testM("effectually peeks at the failure of this effect") {
        for {
          ref <- Ref.make(0)
          _ <- IO.fail(42)
                 .tapEither {
                   case Left(value) => ref.set(value)
                   case Right(_)    => ref.set(-1)
                 }
                 .exit
          effect <- ref.get
        } yield assert(effect)(equalTo(42))
      },
      testM("effectually peeks at the success of this effect") {
        for {
          ref <- Ref.make(0)
          _ <- Task(42).tapEither {
                 case Left(_)      => ref.set(-1)
                 case Right(value) => ref.set(value)
               }.exit
          effect <- ref.get
        } yield assert(effect)(equalTo(42))
      }
    ),
    suite("tapSome")(
      testM("is identity if the function doesn't match") {
        for {
          ref    <- Ref.make(false)
          result <- ref.set(true).as(42).tapSome(PartialFunction.empty)
          effect <- ref.get
        } yield assert(result)(equalTo(42)) &&
          assert(effect)(isTrue)
      },
      testM("runs the effect if the function matches") {
        for {
          ref    <- Ref.make(0)
          result <- ref.set(10).as(42).tapSome { case r => ref.set(r) }
          effect <- ref.get
        } yield assert(result)(equalTo(42)) &&
          assert(effect)(equalTo(42))
      }
    ),
    suite("timeout disconnect")(
      testM("returns `Some` with the produced value if the effect completes before the timeout elapses") {
        assertM(ZIO.unit.disconnect.timeout(100.millis))(isSome(isUnit))
      },
      testM("returns `None` otherwise") {
        for {
          fiber  <- ZIO.never.uninterruptible.disconnect.timeout(100.millis).fork
          _      <- TestClock.adjust(100.millis)
          result <- fiber.join
        } yield assert(result)(isNone)
      }
    ),
    suite("transplant")(
      testM("preserves supervision relationship of nested fibers") {
        for {
          latch1 <- Promise.make[Nothing, Unit]
          latch2 <- Promise.make[Nothing, Unit]
          fiber <- ZIO.transplant { grafter =>
                     grafter {
                       val zio = for {
                         _ <- (latch1.succeed(()) *> ZIO.infinity).onInterrupt(latch2.succeed(())).fork
                         _ <- ZIO.infinity
                       } yield ()
                       zio.fork
                     }
                   }
          _ <- latch1.await
          _ <- fiber.interrupt
          _ <- latch2.await
        } yield assertCompletes
      }
    ),
    testM("unleft") {
      checkM(Gen.oneOf(Gen.failures(Gen.anyInt), Gen.successes(Gen.either(Gen.anyInt, Gen.anyInt)))) { zio =>
        for {
          actual   <- zio.left.unleft.exit
          expected <- zio.exit
        } yield assert(actual)(equalTo(expected))
      }
    },
    suite("unless")(
      testM("executes correct branch only") {
        for {
          effectRef <- Ref.make(0)
          _         <- effectRef.set(1).unless(true)
          val1      <- effectRef.get
          _         <- effectRef.set(2).unless(false)
          val2      <- effectRef.get
          failure    = new Exception("expected")
          _         <- IO.fail(failure).unless(true)
          failed    <- IO.fail(failure).unless(false).either
        } yield {
          assert(val1)(equalTo(0)) &&
          assert(val2)(equalTo(2)) &&
          assert(failed)(isLeft(equalTo(failure)))
        }
      }
    ),
    suite("unlessZIO")(
      testM("executes condition effect and correct branch") {
        for {
          effectRef     <- Ref.make(0)
          conditionRef  <- Ref.make(0)
          conditionTrue  = conditionRef.update(_ + 1).as(true)
          conditionFalse = conditionRef.update(_ + 1).as(false)
          _             <- effectRef.set(1).unlessZIO(conditionTrue)
          val1          <- effectRef.get
          conditionVal1 <- conditionRef.get
          _             <- effectRef.set(2).unlessZIO(conditionFalse)
          val2          <- effectRef.get
          conditionVal2 <- conditionRef.get
          failure        = new Exception("expected")
          _             <- IO.fail(failure).unlessZIO(conditionTrue)
          failed        <- IO.fail(failure).unlessZIO(conditionFalse).either
        } yield {
          assert(val1)(equalTo(0)) &&
          assert(conditionVal1)(equalTo(1)) &&
          assert(val2)(equalTo(2)) &&
          assert(conditionVal2)(equalTo(2)) &&
          assert(failed)(isLeft(equalTo(failure)))
        }
      },
      testM("infers correctly") {
        trait R
        trait R1 extends R
        trait E1
        trait E extends E1
        trait A
        val b: ZIO[R, E, Boolean] = ZIO.succeed(true)
        val zio: ZIO[R1, E1, A]   = ZIO.succeed(new A {})
        val _                     = ZIO.unlessZIO(b)(zio)
        ZIO.succeed(assertCompletes)
      }
    ),
    suite("unrefine")(
      testM("converts some fiber failures into errors") {
        val s    = "division by zero"
        val zio1 = ZIO.die(new IllegalArgumentException(s))
        val zio2 = zio1.unrefine { case e: IllegalArgumentException => e.getMessage }
        assertM(zio2.exit)(fails(equalTo(s)))
      },
      testM("leaves the rest") {
        val t    = new IllegalArgumentException("division by zero")
        val zio1 = ZIO.die(t)
        val zio2 = zio1.unrefine { case e: NumberFormatException => e.getMessage }
        assertM(zio2.exit)(dies(equalTo(t)))
      }
    ),
    suite("unrefineTo")(
      testM("converts some fiber failures into errors") {
        val t    = new IllegalArgumentException("division by zero")
        val zio1 = ZIO.die(t)
        val zio2 = zio1.unrefineTo[IllegalArgumentException]
        assertM(zio2.exit)(fails(equalTo(t)))
      },
      testM("leaves the rest") {
        val t    = new IllegalArgumentException("division by zero")
        val zio1 = ZIO.die(t)
        val zio2 = zio1.unrefineTo[NumberFormatException]
        assertM(zio2.exit)(dies(equalTo(t)))
      }
    ) @@ zioTag(errors),
    suite("unrefineWith")(
      testM("converts some fiber failures into errors") {
        val s    = "division by zero"
        val zio1 = ZIO.die(new IllegalArgumentException(s))
        val zio2 = zio1.unrefineWith { case e: IllegalArgumentException => Option(e.getMessage) }(_ => None)
        assertM(zio2.exit)(fails(isSome(equalTo(s))))
      },
      testM("leaves the rest") {
        val t    = new IllegalArgumentException("division by zero")
        val zio1 = ZIO.die(t)
        val zio2 = zio1.unrefineWith { case e: NumberFormatException => Option(e.getMessage) }(_ => None)
        assertM(zio2.exit)(dies(equalTo(t)))
      },
      testM("uses the specified function to convert the `E` into an `E1`") {
        val zio1 = ZIO.fail("fail")
        val zio2 = zio1.unrefineWith { case e: IllegalArgumentException => Option(e.getMessage) }(_ => None)
        assertM(zio2.exit)(fails(isNone))
      }
    ) @@ zioTag(errors),
    testM("right") {
      checkM(Gen.oneOf(Gen.failures(Gen.anyInt), Gen.successes(Gen.either(Gen.anyInt, Gen.anyInt)))) { zio =>
        for {
          actual   <- zio.right.unright.exit
          expected <- zio.exit
        } yield assert(actual)(equalTo(expected))
      }
    },
    suite("unsandbox")(
      testM("unwraps exception") {
        val failure: IO[Cause[Exception], String] = IO.fail(fail(new Exception("fail")))
        val success: IO[Cause[Any], Int]          = IO.succeed(100)
        for {
          message <- failure.unsandbox.foldZIO(e => IO.succeed(e.getMessage), _ => IO.succeed("unexpected"))
          result  <- success.unsandbox
        } yield assert(message)(equalTo("fail")) && assert(result)(equalTo(100))
      },
      testM("no information is lost during composition") {
        val causes = Gen.causes(Gen.anyString, Gen.throwable)
        def cause[R, E](zio: ZIO[R, E, Nothing]): ZIO[R, Nothing, Cause[E]] =
          zio.foldCauseZIO(ZIO.succeed(_), ZIO.fail)
        checkM(causes) { c =>
          for {
            result <- cause(ZIO.failCause(c).sandbox.mapErrorCause(e => e.untraced).unsandbox)
          } yield assert(result)(equalTo(c)) &&
            assert(result.prettyPrint)(equalTo(c.prettyPrint))
        }
      }
    ),
    suite("updateService")(
      testM("updates a service in the environment") {
        val zio = for {
          a <- ZIO.service[Int].updateService[Int](_ + 1)
          b <- ZIO.service[Int]
        } yield (a, b)
        assertM(zio.provideLayer(ZLayer.succeed(0)))(equalTo((1, 0)))
      }
    ),
    suite("validate")(
      testM("returns all errors if never valid") {
        val in                      = List.fill(10)(0)
        def fail[A](a: A): IO[A, A] = IO.fail(a)
        val res                     = IO.validate(in)(fail).flip
        assertM(res)(equalTo(in))
      } @@ zioTag(errors),
      testM("accumulate errors and ignore successes") {
        implicit val canFail = CanFail
        val in               = List.range(0, 10)
        val res              = ZIO.validate(in)(a => if (a % 2 == 0) ZIO.succeed(a) else ZIO.fail(a))
        assertM(res.flip)(equalTo(List(1, 3, 5, 7, 9)))
      } @@ zioTag(errors),
      testM("accumulate successes") {
        implicit val canFail = CanFail
        val in               = List.range(0, 10)
        val res              = IO.validate(in)(a => ZIO.succeed(a))
        assertM(res)(equalTo(in))
      }
    ),
    suite("validateDiscard")(
      testM("returns all errors if never valid") {
        val in                            = List.fill(10)(0)
        def fail[A](a: A): IO[A, Nothing] = IO.fail(a)
        val res                           = IO.validate(in)(fail).flip
        assertM(res)(equalTo(in))
      } @@ zioTag(errors)
    ),
    suite("validatePar")(
      testM("returns all errors if never valid") {
        val in                      = List.fill(1000)(0)
        def fail[A](a: A): IO[A, A] = IO.fail(a)
        val res                     = IO.validatePar(in)(fail).flip
        assertM(res)(equalTo(in))
      } @@ zioTag(errors),
      testM("accumulate errors and ignore successes") {
        implicit val canFail = CanFail
        val in               = List.range(0, 10)
        val res              = ZIO.validatePar(in)(a => if (a % 2 == 0) ZIO.succeed(a) else ZIO.fail(a))
        assertM(res.flip)(equalTo(List(1, 3, 5, 7, 9)))
      } @@ zioTag(errors),
      testM("accumulate successes") {
        implicit val canFail = CanFail
        val in               = List.range(0, 10)
        val res              = IO.validatePar(in)(a => ZIO.succeed(a))
        assertM(res)(equalTo(in))
      }
    ),
    suite("validateParDiscard")(testM("returns all errors if never valid") {
      val in                            = List.fill(10)(0)
      def fail[A](a: A): IO[A, Nothing] = IO.fail(a)
      val res                           = IO.validateParDiscard(in)(fail).flip
      assertM(res)(equalTo(in))
    } @@ zioTag(errors)),
    suite("validateFirst")(
      testM("returns all errors if never valid") {
        val in  = List.fill(10)(0)
        val res = IO.validateFirst(in)(a => ZIO.fail(a)).flip
        assertM(res)(equalTo(in))
      } @@ zioTag(errors),
      testM("runs sequentially and short circuits on first success validation") {
        implicit val canFail = CanFail
        val in               = List.range(1, 10)
        val f                = (a: Int) => if (a == 6) ZIO.succeed(a) else ZIO.fail(a)

        for {
          counter    <- Ref.make(0)
          res        <- ZIO.validateFirst(in)(a => counter.update(_ + 1) *> f(a))
          assertions <- assertM(ZIO.succeed(res))(equalTo(6)) && assertM(counter.get)(equalTo(6))
        } yield assertions
      },
      testM("returns errors in correct order") {
        val as = List(2, 4, 6, 3, 5, 6)
        for {
          results <- ZIO.validateFirst(as)(ZIO.fail(_)).flip
        } yield assert(results)(equalTo(List(2, 4, 6, 3, 5, 6)))
      } @@ zioTag(errors)
    ),
    suite("validateFirstPar")(
      testM("returns all errors if never valid") {
        val in  = List.fill(1000)(0)
        val res = IO.validateFirstPar(in)(a => ZIO.fail(a)).flip
        assertM(res)(equalTo(in))
      } @@ zioTag(errors),
      testM("returns success if valid") {
        implicit val canFail = CanFail
        val in               = List.range(1, 10)
        val f                = (a: Int) => if (a == 6) ZIO.succeed(a) else ZIO.fail(a)
        val res              = ZIO.validateFirstPar(in)(f(_))
        assertM(res)(equalTo(6))
      }
    ),
    suite("validateWith")(
      testM("succeeds") {
        assertM(ZIO(1).validateWith(ZIO(2))(_ + _))(equalTo(3))
      },
      testM("fails") {
        assertM(ZIO(1).validate(ZIO.fail(2)).sandbox.either)(isLeft(equalTo(Cause.Fail(2))))
      },
      testM("combines both cause") {
        assertM(ZIO.fail(1).validate(ZIO.fail(2)).sandbox.either)(
          isLeft(equalTo(Cause.Then(Cause.Fail(1), Cause.Fail(2))))
        )
      }
    ),
    suite("when")(
      testM("executes correct branch only") {
        for {
          effectRef <- Ref.make(0)
          _         <- effectRef.set(1).when(false)
          val1      <- effectRef.get
          _         <- effectRef.set(2).when(true)
          val2      <- effectRef.get
          failure    = new Exception("expected")
          _         <- IO.fail(failure).when(false)
          failed    <- IO.fail(failure).when(true).either
        } yield {
          assert(val1)(equalTo(0)) &&
          assert(val2)(equalTo(2)) &&
          assert(failed)(isLeft(equalTo(failure)))
        }
      }
    ),
    suite("whenCase")(
      testM("executes correct branch only") {
        val v1: Option[Int] = None
        val v2: Option[Int] = Some(0)
        for {
          ref  <- Ref.make(false)
          _    <- ZIO.whenCase(v1) { case Some(_) => ref.set(true) }
          res1 <- ref.get
          _    <- ZIO.whenCase(v2) { case Some(_) => ref.set(true) }
          res2 <- ref.get
        } yield assert(res1)(isFalse) && assert(res2)(isTrue)
      }
    ),
    suite("whenCaseZIO")(
      testM("executes condition effect and correct branch") {
        val v1: Option[Int] = None
        val v2: Option[Int] = Some(0)
        for {
          ref  <- Ref.make(false)
          _    <- ZIO.whenCaseZIO(IO.succeed(v1)) { case Some(_) => ref.set(true) }
          res1 <- ref.get
          _    <- ZIO.whenCaseZIO(IO.succeed(v2)) { case Some(_) => ref.set(true) }
          res2 <- ref.get
        } yield assert(res1)(isFalse) && assert(res2)(isTrue)
      }
    ),
    suite("whenZIO")(
      testM("executes condition effect and correct branch") {
        for {
          effectRef     <- Ref.make(0)
          conditionRef  <- Ref.make(0)
          conditionTrue  = conditionRef.update(_ + 1).as(true)
          conditionFalse = conditionRef.update(_ + 1).as(false)
          _             <- effectRef.set(1).whenZIO(conditionFalse)
          val1          <- effectRef.get
          conditionVal1 <- conditionRef.get
          _             <- effectRef.set(2).whenZIO(conditionTrue)
          val2          <- effectRef.get
          conditionVal2 <- conditionRef.get
          failure        = new Exception("expected")
          _             <- IO.fail(failure).whenZIO(conditionFalse)
          failed        <- IO.fail(failure).whenZIO(conditionTrue).either
        } yield {
          assert(val1)(equalTo(0)) &&
          assert(conditionVal1)(equalTo(1)) &&
          assert(val2)(equalTo(2)) &&
          assert(conditionVal2)(equalTo(2)) &&
          assert(failed)(isLeft(equalTo(failure)))
        }
      },
      testM("infers correctly") {
        trait R
        trait R1 extends R
        trait E1
        trait E extends E1
        trait A
        val b: ZIO[R, E, Boolean] = ZIO.succeed(true)
        val zio: ZIO[R1, E1, A]   = ZIO.succeed(new A {})
        val _                     = ZIO.whenZIO(b)(zio)
        ZIO.succeed(assertCompletes)
      }
    ),
    suite("withFilter")(
      testM("tuple value is extracted correctly from task") {
        for {
          (i, j, k) <- Task((1, 2, 3))
        } yield assert((i, j, k))(equalTo((1, 2, 3)))
      },
      testM("condition in for-comprehension syntax works correctly for task") {
        for {
          n <- Task(3) if n > 0
        } yield assert(n)(equalTo(3))
      },
      testM("unsatisfied condition should fail with NoSuchElementException") {
        val task =
          for {
            n <- Task(3) if n > 10
          } yield n
        assertM(task.exit)(fails(isSubtype[NoSuchElementException](anything)))
      },
      testM("withFilter doesn't compile with IO that fails with type other than Throwable") {
        val result = typeCheck {
          """
            import zio._
            val io: IO[String, Int] = IO.succeed(1)
            for {
              n <- io if n > 0
            } yield n
              """
        }

        val expected =
          "Pattern guards are only supported when the error type is a supertype of NoSuchElementException. However, your effect has String for the error type."
        if (TestVersion.isScala2) assertM(result)(isLeft(equalTo(expected)))
        else assertM(result)(isLeft(anything))
      }
    ),
    test("zip is compositional") {
      lazy val x1: Task[Int]                          = ???
      lazy val x2: Task[Unit]                         = ???
      lazy val x3: Task[String]                       = ???
      lazy val x4: Task[Boolean]                      = ???
      lazy val actual                                 = x1 <*> x2 <*> x3 <*> x4
      lazy val expected: Task[(Int, String, Boolean)] = actual
      lazy val _                                      = expected
      assertCompletes
    },
    suite("zipPar")(
      testM("does not swallow exit causes of loser") {
        ZIO.interrupt.zipPar(IO.interrupt).exit.map {
          case Exit.Failure(cause) => assert(cause.interruptors)(not(isEmpty))
          case _                   => assert(false)(isTrue)
        }
      },
      testM("does not report failure when interrupting loser after it succeeded") {
        val io          = ZIO.interrupt.zipPar(IO.succeed(1))
        val interrupted = io.sandbox.either.map(_.left.map(_.interrupted))
        assertM(interrupted)(isLeft(isTrue))
      } @@ zioTag(interruption),
      testM("passes regression 1") {
        val io =
          IO.succeed[Int](1).zipPar(IO.succeed[Int](2)).flatMap(t => IO.succeed(t._1 + t._2)).map(_ == 3)
        assertM(io)(isTrue)
      } @@ zioTag(regression) @@ jvm(nonFlaky),
      testM("paralellizes simple success values") {
        def countdown(n: Int): UIO[Int] =
          if (n == 0) IO.succeed(0)
          else
            IO.succeed[Int](1).zipPar(IO.succeed[Int](2)).flatMap(t => countdown(n - 1).map(y => t._1 + t._2 + y))

        assertM(countdown(50))(equalTo(150))
      },
      testM("does not kill fiber when forked on parent scope") {

        for {
          latch1 <- Promise.make[Nothing, Unit]
          latch2 <- Promise.make[Nothing, Unit]
          latch3 <- Promise.make[Nothing, Unit]
          ref1   <- Ref.make(false)
          left = ZIO
                   .uninterruptibleMask(restore => latch2.succeed(()) *> restore(latch1.await *> ZIO.succeed("foo")))
                   .onInterrupt(ref1.set(true))
          right                         = latch3.succeed(()).as(42)
          _                            <- (latch2.await *> latch3.await *> latch1.succeed(())).fork
          result                       <- left.fork zipPar right
          (leftInnerFiber, rightResult) = result
          leftResult                   <- leftInnerFiber.await
          interrupted                  <- ref1.get
        } yield assert(interrupted)(isFalse) && assert(leftResult)(succeeds(equalTo("foo"))) && assert(rightResult)(
          equalTo(42)
        )
      }
    ),
    suite("toFuture")(
      testM("should fail with ZTrace attached") {
        for {
          future <- ZIO.fail(new Throwable(new IllegalArgumentException)).toFuture
          result <- ZIO.fromFuture(_ => future).either
        } yield assert(result)(isLeft(hasThrowableCause(hasThrowableCause(hasMessage(containsString("Fiber:Id("))))))
      }
    ) @@ zioTag(future),
    suite("resurrect")(
      testM("should fail checked") {
        val error: Exception   = new Exception("msg")
        val effect: Task[Unit] = ZIO.fail(error).unit.orDie.resurrect
        assertM(effect.either)(isLeft(equalTo(error)))
      }
    ),
    suite("options")(
      testM("basic option test") {
        for {
          value <- ZIO.getOrFailUnit(Some("foo"))
        } yield {
          assert(value)(equalTo("foo"))
        }
      },
      testM("side effect unit in option test") {
        for {
          value <- ZIO.getOrFailUnit(None).catchAll(_ => ZIO.succeed("Controlling unit side-effect"))
        } yield {
          assert(value)(equalTo("Controlling unit side-effect"))
        }
      }
    ),
    suite("promises")(
      testM("promise test") {
        val func: String => String = s => s.toUpperCase
        for {
          promise <- ZIO.succeed(scala.concurrent.Promise[String]())
          _ <- ZIO.attempt {
                 Try(func("hello world from future")) match {
                   case Success(value)     => promise.success(value)
                   case Failure(exception) => promise.failure(exception)
                 }
               }.fork
          value <- ZIO.fromPromiseScala(promise)
        } yield {
          assert(value)(equalTo("HELLO WORLD FROM FUTURE"))
        }
      },
      testM("promise supplier test") {
        val func: Unit => String = _ => "hello again from future"
        for {
          promise <- ZIO.succeed(scala.concurrent.Promise[String]())
          _ <- ZIO.attempt {
                 Try(func(())) match {
                   case Success(value)     => promise.success(value)
                   case Failure(exception) => promise.failure(exception)
                 }
               }.fork
          value <- ZIO.fromPromiseScala(promise)
        } yield {
          assert(value)(equalTo("hello again from future"))
        }
      },
      testM("promise ugly path test") {
        val func: String => String = s => s.toUpperCase
        for {
          promise <- ZIO.succeed(scala.concurrent.Promise[String]())
          _ <- ZIO.attempt {
                 Try(func(null)) match {
                   case Success(value)     => promise.success(value)
                   case Failure(exception) => promise.failure(exception)
                 }
               }.fork
          value <- ZIO
                     .fromPromiseScala(promise)
                     .catchAll(_ => ZIO.succeed("Controlling side-effect of function passed to promise"))
        } yield {
          assert(value)(equalTo("Controlling side-effect of function passed to promise"))
        }
      }
    )
  )

  def functionIOGen: Gen[Has[Random] with Has[Sized], String => Task[Int]] =
    Gen.function[Has[Random] with Has[Sized], String, Task[Int]](Gen.successes(Gen.anyInt))

  def listGen: Gen[Has[Random] with Has[Sized], List[String]] =
    Gen.listOfN(100)(Gen.alphaNumericString)

  val exampleError = new Error("something went wrong")

  def exactlyOnce[R, A, A1](value: A)(func: UIO[A] => ZIO[R, String, A1]): ZIO[R, String, A1] =
    Ref.make(0).flatMap { ref =>
      for {
        res   <- func(ref.update(_ + 1) *> ZIO.succeed(value))
        count <- ref.get
        _ <- if (count != 1) {
               ZIO.fail("Accessed more than once")
             } else {
               ZIO.unit
             }
      } yield res
    }

  val testString = "supercalifragilisticexpialadocious"

  implicit class ZioOfTestResultOps[R, E](val res: ZIO[R, E, TestResult]) {
    def &&[R1](that: ZIO[R1, E, TestResult]): ZIO[R1 with R, E, TestResult] = res.zipWith(that)(_ && _)
  }

  val ExampleError    = new Throwable("Oh noes!")
  val InterruptCause1 = new Throwable("Oh noes 1!")
  val InterruptCause2 = new Throwable("Oh noes 2!")
  val InterruptCause3 = new Throwable("Oh noes 3!")

  val TaskExampleError: Task[Int] = IO.fail[Throwable](ExampleError)

  val TaskExampleDie: Task[Int] = IO.succeed(throw ExampleError)

  def asyncExampleError[A]: Task[A] =
    IO.async[Throwable, A](_(IO.fail(ExampleError)))

  def sum(n: Int): Int =
    if (n <= 0) 0
    else n + sum(n - 1)

  def deepMapNow(n: Int): UIO[Int] = {
    @tailrec
    def loop(n: Int, acc: UIO[Int]): UIO[Int] =
      if (n <= 0) acc
      else loop(n - 1, acc.map(_ + 1))

    loop(n, IO.succeed(0))
  }

  def deepMapEffect(n: Int): UIO[Int] = {
    @tailrec
    def loop(n: Int, acc: UIO[Int]): UIO[Int] =
      if (n <= 0) acc
      else loop(n - 1, acc.map(_ + 1))

    loop(n, IO.succeed(0))
  }

  def deepErrorEffect(n: Int): Task[Unit] =
    if (n == 0) IO.attempt(throw ExampleError)
    else IO.unit *> deepErrorEffect(n - 1)

  def deepErrorFail(n: Int): Task[Unit] =
    if (n == 0) IO.fail(ExampleError)
    else IO.unit *> deepErrorFail(n - 1)

  def fib(n: Int): BigInt =
    if (n <= 1) n
    else fib(n - 1) + fib(n - 2)

  def concurrentFib(n: Int): Task[BigInt] =
    if (n <= 1) IO.succeed[BigInt](n)
    else
      for {
        f1 <- concurrentFib(n - 1).fork
        f2 <- concurrentFib(n - 2).fork
        v1 <- f1.join
        v2 <- f2.join
      } yield v1 + v2

  def AsyncUnit[E]: IO[E, Unit] = IO.async[E, Unit](_(IO.unit))

  type Logging = Has[Logging.Service]

  object Logging {
    trait Service
    val live: ZLayer[Any, Nothing, Logging] = ZLayer.succeed(new Logging.Service {})
  }
}

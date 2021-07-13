package zio

import com.github.ghik.silencer.silent
import zio.test.Assertion._
import zio.test.TestAspect.nonFlaky
import zio.test._

object ZRefMSpec extends ZIOBaseSpec {

  import ZIOTag._

  def spec: ZSpec[Environment, Failure] = suite("ZRefMSpec")(
    suite("atomic")(
      test("get") {
        for {
          refM  <- RefM.make(current)
          value <- refM.get
        } yield assert(value)(equalTo(current))
      },
      test("getAndUpdate") {
        for {
          refM   <- RefM.make(current)
          value1 <- refM.getAndUpdateZIO(_ => IO.succeed(update))
          value2 <- refM.get
        } yield assert(value1)(equalTo(current)) && assert(value2)(equalTo(update))
      },
      test("getAndUpdate with failure") {
        for {
          refM  <- RefM.make[String](current)
          value <- refM.getAndUpdateZIO(_ => IO.fail(failure)).exit
        } yield assert(value)(fails(equalTo(failure)))
      },
      test("getAndUpdateSome") {
        for {
          refM   <- RefM.make[State](Active)
          value1 <- refM.getAndUpdateSomeZIO { case Closed => IO.succeed(Active) }
          value2 <- refM.get
        } yield assert(value1)(equalTo(Active)) && assert(value2)(equalTo(Active))
      },
      test("getAndUpdateSome twice") {
        for {
          refM   <- RefM.make[State](Active)
          value1 <- refM.getAndUpdateSomeZIO { case Active => IO.succeed(Changed) }
          value2 <- refM.getAndUpdateSomeZIO {
                      case Active  => IO.succeed(Changed)
                      case Changed => IO.succeed(Closed)
                    }
          value3 <- refM.get
        } yield assert(value1)(equalTo(Active)) && assert(value2)(equalTo(Changed)) && assert(value3)(equalTo(Closed))
      },
      test("getAndUpdateSome with failure") {
        for {
          refM  <- RefM.make[State](Active)
          value <- refM.getAndUpdateSomeZIO { case Active => IO.fail(failure) }.exit
        } yield assert(value)(fails(equalTo(failure)))
      },
      test("interrupt parent fiber and update") {
        for {
          promise    <- Promise.make[Nothing, RefM[State]]
          latch      <- Promise.make[Nothing, Unit]
          makeAndWait = promise.complete(RefM.make[State](Active)) *> latch.await
          fiber      <- makeAndWait.fork
          refM       <- promise.await
          _          <- fiber.interrupt
          value      <- refM.updateAndGetZIO(_ => ZIO.succeed(Closed))
        } yield assert(value)(equalTo(Closed))
      } @@ zioTag(interruption),
      test("modify") {
        for {
          refM  <- RefM.make(current)
          r     <- refM.modifyZIO(_ => IO.succeed(("hello", update)))
          value <- refM.get
        } yield assert(r)(equalTo("hello")) && assert(value)(equalTo(update))
      },
      test("modify with failure") {
        for {
          refM <- RefM.make[String](current)
          r    <- refM.modifyZIO(_ => IO.fail(failure)).exit
        } yield assert(r)(fails(equalTo(failure)))
      } @@ zioTag(errors),
      test("modify twice") {
        for {
          refM   <- RefM.make[State](Active)
          r1     <- refM.modifySomeZIO("doesn't change the state") { case Active => IO.succeed("changed" -> Changed) }
          value1 <- refM.get
          r2 <- refM.modifySomeZIO("doesn't change the state") {
                  case Active  => IO.succeed("changed" -> Changed)
                  case Changed => IO.succeed("closed" -> Closed)
                }
          value2 <- refM.get
        } yield assert(r1)(equalTo("changed")) &&
          assert(value1)(equalTo(Changed)) &&
          assert(r2)(equalTo("closed")) &&
          assert(value2)(equalTo(Closed))
      },
      test("modifySome") {
        for {
          refM  <- RefM.make[State](Active)
          r     <- refM.modifySomeZIO("State doesn't change") { case Closed => IO.succeed("active" -> Active) }
          value <- refM.get
        } yield assert(r)(equalTo("State doesn't change")) && assert(value)(equalTo(Active))
      },
      test("modifySome with failure not triggered") {
        for {
          refM <- RefM.make[State](Active)
          r <-
            refM.modifySomeZIO("State doesn't change") { case Closed => IO.fail(failure) }.orDieWith(new Exception(_))
          value <- refM.get
        } yield assert(r)(equalTo("State doesn't change")) && assert(value)(equalTo(Active))
      } @@ zioTag(errors),
      test("modifySome with failure") {
        for {
          refM  <- RefM.make[State](Active)
          value <- refM.modifySomeZIO("State doesn't change") { case Active => IO.fail(failure) }.exit
        } yield assert(value)(fails(equalTo(failure)))
      } @@ zioTag(errors),
      test("modifySome with fatal error") {
        for {
          refM  <- RefM.make[State](Active)
          value <- refM.modifySomeZIO("State doesn't change") { case Active => IO.dieMessage(fatalError) }.exit
        } yield assert(value)(dies(hasMessage(equalTo(fatalError))))
      } @@ zioTag(errors),
      test("set") {
        for {
          refM  <- RefM.make(current)
          _     <- refM.set(update)
          value <- refM.get
        } yield assert(value)(equalTo(update))
      },
      test("updateAndGet") {
        for {
          refM  <- RefM.make(current)
          value <- refM.updateAndGetZIO(_ => IO.succeed(update))
        } yield assert(value)(equalTo(update))
      },
      test("updateAndGet with failure") {
        for {
          refM  <- RefM.make[String](current)
          value <- refM.updateAndGetZIO(_ => IO.fail(failure)).exit
        } yield assert(value)(fails(equalTo(failure)))
      } @@ zioTag(errors),
      test("updateSomeAndGet") {
        for {
          refM  <- RefM.make[State](Active)
          value <- refM.updateSomeAndGetZIO { case Closed => IO.succeed(Active) }
        } yield assert(value)(equalTo(Active))
      },
      test("updateSomeAndGet twice") {
        for {
          refM   <- RefM.make[State](Active)
          value1 <- refM.updateSomeAndGetZIO { case Active => IO.succeed(Changed) }
          value2 <- refM.updateSomeAndGetZIO {
                      case Active  => IO.succeed(Changed)
                      case Changed => IO.succeed(Closed)
                    }
        } yield assert(value1)(equalTo(Changed)) && assert(value2)(equalTo(Closed))
      },
      test("updateSomeAndGet with failure") {
        for {
          refM  <- RefM.make[State](Active)
          value <- refM.updateSomeAndGetZIO { case Active => IO.fail(failure) }.exit
        } yield assert(value)(fails(equalTo(failure)))
      } @@ zioTag(errors)
    ),
    suite("derived")(
      test("atomicity") {
        for {
          ref   <- DerivedAll.make(0)
          _     <- ZIO.collectAllPar(ZIO.replicate(100)(ref.updateZIO(n => ZIO.succeed(n + 1))))
          value <- ref.get
        } yield assert(value)(equalTo(100))
      },
      test("get") {
        for {
          refM  <- Derived.make(current)
          value <- refM.get
        } yield assert(value)(equalTo(current))
      },
      test("getAndUpdate") {
        for {
          refM   <- Derived.make(current)
          value1 <- refM.getAndUpdateZIO(_ => IO.succeed(update))
          value2 <- refM.get
        } yield assert(value1)(equalTo(current)) && assert(value2)(equalTo(update))
      },
      test("getAndUpdate with failure") {
        for {
          refM  <- Derived.make[String](current)
          value <- refM.getAndUpdateZIO(_ => IO.fail(failure)).exit
        } yield assert(value)(fails(equalTo(failure)))
      },
      test("getAndUpdateSome") {
        for {
          refM   <- Derived.make[State](Active)
          value1 <- refM.getAndUpdateSomeZIO { case Closed => IO.succeed(Active) }
          value2 <- refM.get
        } yield assert(value1)(equalTo(Active)) && assert(value2)(equalTo(Active))
      },
      test("getAndUpdateSome twice") {
        for {
          refM   <- Derived.make[State](Active)
          value1 <- refM.getAndUpdateSomeZIO { case Active => IO.succeed(Changed) }
          value2 <- refM.getAndUpdateSomeZIO {
                      case Active  => IO.succeed(Changed)
                      case Changed => IO.succeed(Closed)
                    }
          value3 <- refM.get
        } yield assert(value1)(equalTo(Active)) && assert(value2)(equalTo(Changed)) && assert(value3)(equalTo(Closed))
      },
      test("getAndUpdateSome with failure") {
        for {
          refM  <- Derived.make[State](Active)
          value <- refM.getAndUpdateSomeZIO { case Active => IO.fail(failure) }.exit
        } yield assert(value)(fails(equalTo(failure)))
      },
      test("interrupt parent fiber and update") {
        for {
          promise    <- Promise.make[Nothing, RefM[State]]
          latch      <- Promise.make[Nothing, Unit]
          makeAndWait = promise.complete(Derived.make[State](Active)) *> latch.await
          fiber      <- makeAndWait.fork
          refM       <- promise.await
          _          <- fiber.interrupt
          value      <- refM.updateAndGetZIO(_ => ZIO.succeed(Closed))
        } yield assert(value)(equalTo(Closed))
      } @@ zioTag(interruption),
      test("modify") {
        for {
          refM  <- Derived.make(current)
          r     <- refM.modifyZIO(_ => IO.succeed(("hello", update)))
          value <- refM.get
        } yield assert(r)(equalTo("hello")) && assert(value)(equalTo(update))
      },
      test("modify with failure") {
        for {
          refM <- Derived.make[String](current)
          r    <- refM.modifyZIO(_ => IO.fail(failure)).exit
        } yield assert(r)(fails(equalTo(failure)))
      } @@ zioTag(errors),
      test("modify twice") {
        for {
          refM   <- Derived.make[State](Active)
          r1     <- refM.modifySomeZIO("doesn't change the state") { case Active => IO.succeed("changed" -> Changed) }
          value1 <- refM.get
          r2 <- refM.modifySomeZIO("doesn't change the state") {
                  case Active  => IO.succeed("changed" -> Changed)
                  case Changed => IO.succeed("closed" -> Closed)
                }
          value2 <- refM.get
        } yield assert(r1)(equalTo("changed")) &&
          assert(value1)(equalTo(Changed)) &&
          assert(r2)(equalTo("closed")) &&
          assert(value2)(equalTo(Closed))
      },
      test("modifySome") {
        for {
          refM  <- Derived.make[State](Active)
          r     <- refM.modifySomeZIO("State doesn't change") { case Closed => IO.succeed("active" -> Active) }
          value <- refM.get
        } yield assert(r)(equalTo("State doesn't change")) && assert(value)(equalTo(Active))
      },
      test("modifySome with failure not triggered") {
        for {
          refM <- Derived.make[State](Active)
          r <-
            refM.modifySomeZIO("State doesn't change") { case Closed => IO.fail(failure) }.orDieWith(new Exception(_))
          value <- refM.get
        } yield assert(r)(equalTo("State doesn't change")) && assert(value)(equalTo(Active))
      } @@ zioTag(errors),
      test("modifySome with failure") {
        for {
          refM  <- Derived.make[State](Active)
          value <- refM.modifySomeZIO("State doesn't change") { case Active => IO.fail(failure) }.exit
        } yield assert(value)(fails(equalTo(failure)))
      } @@ zioTag(errors),
      test("modifySome with fatal error") {
        for {
          refM  <- Derived.make[State](Active)
          value <- refM.modifySomeZIO("State doesn't change") { case Active => IO.dieMessage(fatalError) }.exit
        } yield assert(value)(dies(hasMessage(equalTo(fatalError))))
      } @@ zioTag(errors),
      test("set") {
        for {
          refM  <- Derived.make(current)
          _     <- refM.set(update)
          value <- refM.get
        } yield assert(value)(equalTo(update))
      },
      test("updateAndGet") {
        for {
          refM  <- Derived.make(current)
          value <- refM.updateAndGetZIO(_ => IO.succeed(update))
        } yield assert(value)(equalTo(update))
      },
      test("updateAndGet with failure") {
        for {
          refM  <- Derived.make[String](current)
          value <- refM.updateAndGetZIO(_ => IO.fail(failure)).exit
        } yield assert(value)(fails(equalTo(failure)))
      } @@ zioTag(errors),
      test("updateSomeAndGet") {
        for {
          refM  <- Derived.make[State](Active)
          value <- refM.updateSomeAndGetZIO { case Closed => IO.succeed(Active) }
        } yield assert(value)(equalTo(Active))
      },
      test("updateSomeAndGet twice") {
        for {
          refM   <- Derived.make[State](Active)
          value1 <- refM.updateSomeAndGetZIO { case Active => IO.succeed(Changed) }
          value2 <- refM.updateSomeAndGetZIO {
                      case Active  => IO.succeed(Changed)
                      case Changed => IO.succeed(Closed)
                    }
        } yield assert(value1)(equalTo(Changed)) && assert(value2)(equalTo(Closed))
      },
      test("updateSomeAndGet with failure") {
        for {
          refM  <- Derived.make[State](Active)
          value <- refM.updateSomeAndGetZIO { case Active => IO.fail(failure) }.exit
        } yield assert(value)(fails(equalTo(failure)))
      } @@ zioTag(errors)
    ),
    suite("derivedAll")(
      test("atomicity") {
        for {
          ref   <- DerivedAll.make(0)
          _     <- ZIO.collectAllPar(ZIO.replicate(100)(ref.updateZIO(n => ZIO.succeed(n + 1))))
          value <- ref.get
        } yield assert(value)(equalTo(100))
      },
      test("get") {
        for {
          refM  <- DerivedAll.make(current)
          value <- refM.get
        } yield assert(value)(equalTo(current))
      },
      test("getAndUpdate") {
        for {
          refM   <- DerivedAll.make(current)
          value1 <- refM.getAndUpdateZIO(_ => IO.succeed(update))
          value2 <- refM.get
        } yield assert(value1)(equalTo(current)) && assert(value2)(equalTo(update))
      },
      test("getAndUpdate with failure") {
        for {
          refM  <- DerivedAll.make[String](current)
          value <- refM.getAndUpdateZIO(_ => IO.fail(failure)).exit
        } yield assert(value)(fails(equalTo(failure)))
      },
      test("getAndUpdateSome") {
        for {
          refM   <- DerivedAll.make[State](Active)
          value1 <- refM.getAndUpdateSomeZIO { case Closed => IO.succeed(Active) }
          value2 <- refM.get
        } yield assert(value1)(equalTo(Active)) && assert(value2)(equalTo(Active))
      },
      test("getAndUpdateSome twice") {
        for {
          refM   <- DerivedAll.make[State](Active)
          value1 <- refM.getAndUpdateSomeZIO { case Active => IO.succeed(Changed) }
          value2 <- refM.getAndUpdateSomeZIO {
                      case Active  => IO.succeed(Changed)
                      case Changed => IO.succeed(Closed)
                    }
          value3 <- refM.get
        } yield assert(value1)(equalTo(Active)) && assert(value2)(equalTo(Changed)) && assert(value3)(equalTo(Closed))
      },
      test("getAndUpdateSome with failure") {
        for {
          refM  <- DerivedAll.make[State](Active)
          value <- refM.getAndUpdateSomeZIO { case Active => IO.fail(failure) }.exit
        } yield assert(value)(fails(equalTo(failure)))
      },
      test("interrupt parent fiber and update") {
        for {
          promise    <- Promise.make[Nothing, RefM[State]]
          latch      <- Promise.make[Nothing, Unit]
          makeAndWait = promise.complete(DerivedAll.make[State](Active)) *> latch.await
          fiber      <- makeAndWait.fork
          refM       <- promise.await
          _          <- fiber.interrupt
          value      <- refM.updateAndGetZIO(_ => ZIO.succeed(Closed))
        } yield assert(value)(equalTo(Closed))
      } @@ zioTag(interruption),
      test("modify") {
        for {
          refM  <- DerivedAll.make(current)
          r     <- refM.modifyZIO(_ => IO.succeed(("hello", update)))
          value <- refM.get
        } yield assert(r)(equalTo("hello")) && assert(value)(equalTo(update))
      },
      test("modify with failure") {
        for {
          refM <- DerivedAll.make[String](current)
          r    <- refM.modifyZIO(_ => IO.fail(failure)).exit
        } yield assert(r)(fails(equalTo(failure)))
      } @@ zioTag(errors),
      test("modify twice") {
        for {
          refM   <- DerivedAll.make[State](Active)
          r1     <- refM.modifySomeZIO("doesn't change the state") { case Active => IO.succeed("changed" -> Changed) }
          value1 <- refM.get
          r2 <- refM.modifySomeZIO("doesn't change the state") {
                  case Active  => IO.succeed("changed" -> Changed)
                  case Changed => IO.succeed("closed" -> Closed)
                }
          value2 <- refM.get
        } yield assert(r1)(equalTo("changed")) &&
          assert(value1)(equalTo(Changed)) &&
          assert(r2)(equalTo("closed")) &&
          assert(value2)(equalTo(Closed))
      },
      test("modifySome") {
        for {
          refM  <- DerivedAll.make[State](Active)
          r     <- refM.modifySomeZIO("State doesn't change") { case Closed => IO.succeed("active" -> Active) }
          value <- refM.get
        } yield assert(r)(equalTo("State doesn't change")) && assert(value)(equalTo(Active))
      },
      test("modifySome with failure not triggered") {
        for {
          refM <- DerivedAll.make[State](Active)
          r <-
            refM.modifySomeZIO("State doesn't change") { case Closed => IO.fail(failure) }.orDieWith(new Exception(_))
          value <- refM.get
        } yield assert(r)(equalTo("State doesn't change")) && assert(value)(equalTo(Active))
      } @@ zioTag(errors),
      test("modifySome with failure") {
        for {
          refM  <- DerivedAll.make[State](Active)
          value <- refM.modifySomeZIO("State doesn't change") { case Active => IO.fail(failure) }.exit
        } yield assert(value)(fails(equalTo(failure)))
      } @@ zioTag(errors),
      test("modifySome with fatal error") {
        for {
          refM  <- DerivedAll.make[State](Active)
          value <- refM.modifySomeZIO("State doesn't change") { case Active => IO.dieMessage(fatalError) }.exit
        } yield assert(value)(dies(hasMessage(equalTo(fatalError))))
      } @@ zioTag(errors),
      test("set") {
        for {
          refM  <- DerivedAll.make(current)
          _     <- refM.set(update)
          value <- refM.get
        } yield assert(value)(equalTo(update))
      },
      test("updateAndGet") {
        for {
          refM  <- DerivedAll.make(current)
          value <- refM.updateAndGetZIO(_ => IO.succeed(update))
        } yield assert(value)(equalTo(update))
      },
      test("updateAndGet with failure") {
        for {
          refM  <- DerivedAll.make[String](current)
          value <- refM.updateAndGetZIO(_ => IO.fail(failure)).exit
        } yield assert(value)(fails(equalTo(failure)))
      } @@ zioTag(errors),
      test("updateSomeAndGet") {
        for {
          refM  <- DerivedAll.make[State](Active)
          value <- refM.updateSomeAndGetZIO { case Closed => IO.succeed(Active) }
        } yield assert(value)(equalTo(Active))
      },
      test("updateSomeAndGet twice") {
        for {
          refM   <- DerivedAll.make[State](Active)
          value1 <- refM.updateSomeAndGetZIO { case Active => IO.succeed(Changed) }
          value2 <- refM.updateSomeAndGetZIO {
                      case Active  => IO.succeed(Changed)
                      case Changed => IO.succeed(Closed)
                    }
        } yield assert(value1)(equalTo(Changed)) && assert(value2)(equalTo(Closed))
      },
      test("updateSomeAndGet with failure") {
        for {
          refM  <- DerivedAll.make[State](Active)
          value <- refM.updateSomeAndGetZIO { case Active => IO.fail(failure) }.exit
        } yield assert(value)(fails(equalTo(failure)))
      } @@ zioTag(errors)
    ),
    suite("zip")(
      test("updates can be performed atomically") {
        for {
          left    <- RefM.make(0)
          right   <- RefM.make(1)
          composed = left <*> right
          effect   = composed.update { case (a, b) => (b, a + b) }
          _       <- ZIO.collectAllParDiscard(ZIO.replicate(20)(effect))
          tuple   <- composed.get
          (a, b)   = tuple
        } yield assert(a)(equalTo(6765)) && assert(b)(equalTo(10946))
      } @@ nonFlaky,
      test("partial writes cannot be observed by other fibers") {
        for {
          left    <- RefM.make(0)
          right   <- RefM.make(0)
          composed = left <*> right
          effect   = composed.getAndUpdate { case (a, b) => (a + 1, b + 1) }
          _       <- ZIO.forkAllDiscard(ZIO.replicate(100)(effect))
          tuple   <- composed.get
          (a, b)   = tuple
        } yield assert(a)(equalTo(b))
      } @@ nonFlaky,
      test("is compositional") {
        lazy val x1: RefM[Int]                         = ???
        lazy val x2: RefM[Unit]                        = ???
        lazy val x3: RefM[String]                      = ???
        lazy val x4: RefM[Boolean]                     = ???
        lazy val actual                                = x1 <*> x2 <*> x3 <*> x4
        lazy val expected: Ref[(Int, String, Boolean)] = actual
        lazy val _                                     = expected
        assertCompletes
      }
    ),
    suite("combinators")(
      test("dequeueRef") {
        for {
          data        <- RefM.dequeueRef(0): @silent("deprecated")
          (ref, queue) = data
          _           <- ZIO.collectAllPar(ZIO.replicate(100)(ref.update(_ + 1)))
          value       <- ZIO.collectAll(ZIO.replicate(100)(queue.take))
        } yield assert(value)(equalTo((1 to 100).toList))
      }
    )
  )

  val (current, update) = ("value", "new value")
  val failure           = "failure"
  val fatalError        = ":-0"

  sealed trait State
  case object Active  extends State
  case object Changed extends State
  case object Closed  extends State

  object Derived {
    def make[A](a: A): UIO[RefM[A]] =
      RefM.make(a).map(ref => ref.foldZIO(identity, identity, ZIO.succeed(_), ZIO.succeed(_)))
  }

  object DerivedAll {
    def make[A](a: A): UIO[RefM[A]] =
      RefM.make(a).map(ref => ref.foldAllZIO(identity, identity, identity, a => _ => ZIO.succeed(a), ZIO.succeed(_)))
  }
}

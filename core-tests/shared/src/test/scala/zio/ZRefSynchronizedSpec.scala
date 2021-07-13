package zio

import zio.test.Assertion._
import zio.test.TestAspect.nonFlaky
import zio.test._

object ZRefMSpec extends ZIOBaseSpec {

  import ZIOTag._

  def spec: ZSpec[Environment, Failure] = suite("ZRefSynchronizedSpec")(
    suite("atomic")(
      test("get") {
        for {
          ref   <- Ref.Synchronized.make(current)
          value <- ref.get
        } yield assert(value)(equalTo(current))
      },
      test("getAndUpdate") {
        for {
          ref    <- Ref.Synchronized.make(current)
          value1 <- ref.getAndUpdateZIO(_ => IO.succeed(update))
          value2 <- ref.get
        } yield assert(value1)(equalTo(current)) && assert(value2)(equalTo(update))
      },
      test("getAndUpdate with failure") {
        for {
          ref   <- Ref.Synchronized.make[String](current)
          value <- ref.getAndUpdateZIO(_ => IO.fail(failure)).exit
        } yield assert(value)(fails(equalTo(failure)))
      },
      test("getAndUpdateSome") {
        for {
          ref    <- Ref.Synchronized.make[State](Active)
          value1 <- ref.getAndUpdateSomeZIO { case Closed => IO.succeed(Active) }
          value2 <- ref.get
        } yield assert(value1)(equalTo(Active)) && assert(value2)(equalTo(Active))
      },
      test("getAndUpdateSome twice") {
        for {
          ref    <- Ref.Synchronized.make[State](Active)
          value1 <- ref.getAndUpdateSomeZIO { case Active => IO.succeed(Changed) }
          value2 <- ref.getAndUpdateSomeZIO {
                      case Active  => IO.succeed(Changed)
                      case Changed => IO.succeed(Closed)
                    }
          value3 <- ref.get
        } yield assert(value1)(equalTo(Active)) && assert(value2)(equalTo(Changed)) && assert(value3)(equalTo(Closed))
      },
      test("getAndUpdateSome with failure") {
        for {
          ref   <- Ref.Synchronized.make[State](Active)
          value <- ref.getAndUpdateSomeZIO { case Active => IO.fail(failure) }.exit
        } yield assert(value)(fails(equalTo(failure)))
      },
      test("interrupt parent fiber and update") {
        for {
          promise    <- Promise.make[Nothing, Ref.Synchronized[State]]
          latch      <- Promise.make[Nothing, Unit]
          makeAndWait = promise.complete(Ref.Synchronized.make[State](Active)) *> latch.await
          fiber      <- makeAndWait.fork
          ref        <- promise.await
          _          <- fiber.interrupt
          value      <- ref.updateAndGetZIO(_ => ZIO.succeed(Closed))
        } yield assert(value)(equalTo(Closed))
      } @@ zioTag(interruption),
      test("modify") {
        for {
          ref   <- Ref.Synchronized.make(current)
          r     <- ref.modifyZIO(_ => IO.succeed(("hello", update)))
          value <- ref.get
        } yield assert(r)(equalTo("hello")) && assert(value)(equalTo(update))
      },
      test("modify with failure") {
        for {
          ref <- Ref.Synchronized.make[String](current)
          r   <- ref.modifyZIO(_ => IO.fail(failure)).exit
        } yield assert(r)(fails(equalTo(failure)))
      } @@ zioTag(errors),
      test("modify twice") {
        for {
          ref    <- Ref.Synchronized.make[State](Active)
          r1     <- ref.modifySomeZIO("doesn't change the state") { case Active => IO.succeed("changed" -> Changed) }
          value1 <- ref.get
          r2 <- ref.modifySomeZIO("doesn't change the state") {
                  case Active  => IO.succeed("changed" -> Changed)
                  case Changed => IO.succeed("closed" -> Closed)
                }
          value2 <- ref.get
        } yield assert(r1)(equalTo("changed")) &&
          assert(value1)(equalTo(Changed)) &&
          assert(r2)(equalTo("closed")) &&
          assert(value2)(equalTo(Closed))
      },
      test("modifySome") {
        for {
          ref   <- Ref.Synchronized.make[State](Active)
          r     <- ref.modifySomeZIO("State doesn't change") { case Closed => IO.succeed("active" -> Active) }
          value <- ref.get
        } yield assert(r)(equalTo("State doesn't change")) && assert(value)(equalTo(Active))
      },
      test("modifySome with failure not triggered") {
        for {
          ref <- Ref.Synchronized.make[State](Active)
          r <-
            ref.modifySomeZIO("State doesn't change") { case Closed => IO.fail(failure) }.orDieWith(new Exception(_))
          value <- ref.get
        } yield assert(r)(equalTo("State doesn't change")) && assert(value)(equalTo(Active))
      } @@ zioTag(errors),
      test("modifySome with failure") {
        for {
          ref   <- Ref.Synchronized.make[State](Active)
          value <- ref.modifySomeZIO("State doesn't change") { case Active => IO.fail(failure) }.exit
        } yield assert(value)(fails(equalTo(failure)))
      } @@ zioTag(errors),
      test("modifySome with fatal error") {
        for {
          ref   <- Ref.Synchronized.make[State](Active)
          value <- ref.modifySomeZIO("State doesn't change") { case Active => IO.dieMessage(fatalError) }.exit
        } yield assert(value)(dies(hasMessage(equalTo(fatalError))))
      } @@ zioTag(errors),
      test("set") {
        for {
          ref   <- Ref.Synchronized.make(current)
          _     <- ref.set(update)
          value <- ref.get
        } yield assert(value)(equalTo(update))
      },
      test("updateAndGet") {
        for {
          ref   <- Ref.Synchronized.make(current)
          value <- ref.updateAndGetZIO(_ => IO.succeed(update))
        } yield assert(value)(equalTo(update))
      },
      test("updateAndGet with failure") {
        for {
          ref   <- Ref.Synchronized.make[String](current)
          value <- ref.updateAndGetZIO(_ => IO.fail(failure)).exit
        } yield assert(value)(fails(equalTo(failure)))
      } @@ zioTag(errors),
      test("updateSomeAndGet") {
        for {
          ref   <- Ref.Synchronized.make[State](Active)
          value <- ref.updateSomeAndGetZIO { case Closed => IO.succeed(Active) }
        } yield assert(value)(equalTo(Active))
      },
      test("updateSomeAndGet twice") {
        for {
          ref    <- Ref.Synchronized.make[State](Active)
          value1 <- ref.updateSomeAndGetZIO { case Active => IO.succeed(Changed) }
          value2 <- ref.updateSomeAndGetZIO {
                      case Active  => IO.succeed(Changed)
                      case Changed => IO.succeed(Closed)
                    }
        } yield assert(value1)(equalTo(Changed)) && assert(value2)(equalTo(Closed))
      },
      test("updateSomeAndGet with failure") {
        for {
          ref   <- Ref.Synchronized.make[State](Active)
          value <- ref.updateSomeAndGetZIO { case Active => IO.fail(failure) }.exit
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
          ref   <- Derived.make(current)
          value <- ref.get
        } yield assert(value)(equalTo(current))
      },
      test("getAndUpdate") {
        for {
          ref    <- Derived.make(current)
          value1 <- ref.getAndUpdateZIO(_ => IO.succeed(update))
          value2 <- ref.get
        } yield assert(value1)(equalTo(current)) && assert(value2)(equalTo(update))
      },
      test("getAndUpdate with failure") {
        for {
          ref   <- Derived.make[String](current)
          value <- ref.getAndUpdateZIO(_ => IO.fail(failure)).exit
        } yield assert(value)(fails(equalTo(failure)))
      },
      test("getAndUpdateSome") {
        for {
          ref    <- Derived.make[State](Active)
          value1 <- ref.getAndUpdateSomeZIO { case Closed => IO.succeed(Active) }
          value2 <- ref.get
        } yield assert(value1)(equalTo(Active)) && assert(value2)(equalTo(Active))
      },
      test("getAndUpdateSome twice") {
        for {
          ref    <- Derived.make[State](Active)
          value1 <- ref.getAndUpdateSomeZIO { case Active => IO.succeed(Changed) }
          value2 <- ref.getAndUpdateSomeZIO {
                      case Active  => IO.succeed(Changed)
                      case Changed => IO.succeed(Closed)
                    }
          value3 <- ref.get
        } yield assert(value1)(equalTo(Active)) && assert(value2)(equalTo(Changed)) && assert(value3)(equalTo(Closed))
      },
      test("getAndUpdateSome with failure") {
        for {
          ref   <- Derived.make[State](Active)
          value <- ref.getAndUpdateSomeZIO { case Active => IO.fail(failure) }.exit
        } yield assert(value)(fails(equalTo(failure)))
      },
      test("interrupt parent fiber and update") {
        for {
          promise    <- Promise.make[Nothing, Ref.Synchronized[State]]
          latch      <- Promise.make[Nothing, Unit]
          makeAndWait = promise.complete(Derived.make[State](Active)) *> latch.await
          fiber      <- makeAndWait.fork
          ref        <- promise.await
          _          <- fiber.interrupt
          value      <- ref.updateAndGetZIO(_ => ZIO.succeed(Closed))
        } yield assert(value)(equalTo(Closed))
      } @@ zioTag(interruption),
      test("modify") {
        for {
          ref   <- Derived.make(current)
          r     <- ref.modifyZIO(_ => IO.succeed(("hello", update)))
          value <- ref.get
        } yield assert(r)(equalTo("hello")) && assert(value)(equalTo(update))
      },
      test("modify with failure") {
        for {
          ref <- Derived.make[String](current)
          r   <- ref.modifyZIO(_ => IO.fail(failure)).exit
        } yield assert(r)(fails(equalTo(failure)))
      } @@ zioTag(errors),
      test("modify twice") {
        for {
          ref    <- Derived.make[State](Active)
          r1     <- ref.modifySomeZIO("doesn't change the state") { case Active => IO.succeed("changed" -> Changed) }
          value1 <- ref.get
          r2 <- ref.modifySomeZIO("doesn't change the state") {
                  case Active  => IO.succeed("changed" -> Changed)
                  case Changed => IO.succeed("closed" -> Closed)
                }
          value2 <- ref.get
        } yield assert(r1)(equalTo("changed")) &&
          assert(value1)(equalTo(Changed)) &&
          assert(r2)(equalTo("closed")) &&
          assert(value2)(equalTo(Closed))
      },
      test("modifySome") {
        for {
          ref   <- Derived.make[State](Active)
          r     <- ref.modifySomeZIO("State doesn't change") { case Closed => IO.succeed("active" -> Active) }
          value <- ref.get
        } yield assert(r)(equalTo("State doesn't change")) && assert(value)(equalTo(Active))
      },
      test("modifySome with failure not triggered") {
        for {
          ref <- Derived.make[State](Active)
          r <-
            ref.modifySomeZIO("State doesn't change") { case Closed => IO.fail(failure) }.orDieWith(new Exception(_))
          value <- ref.get
        } yield assert(r)(equalTo("State doesn't change")) && assert(value)(equalTo(Active))
      } @@ zioTag(errors),
      test("modifySome with failure") {
        for {
          ref   <- Derived.make[State](Active)
          value <- ref.modifySomeZIO("State doesn't change") { case Active => IO.fail(failure) }.exit
        } yield assert(value)(fails(equalTo(failure)))
      } @@ zioTag(errors),
      test("modifySome with fatal error") {
        for {
          ref   <- Derived.make[State](Active)
          value <- ref.modifySomeZIO("State doesn't change") { case Active => IO.dieMessage(fatalError) }.exit
        } yield assert(value)(dies(hasMessage(equalTo(fatalError))))
      } @@ zioTag(errors),
      test("set") {
        for {
          ref   <- Derived.make(current)
          _     <- ref.set(update)
          value <- ref.get
        } yield assert(value)(equalTo(update))
      },
      test("updateAndGet") {
        for {
          ref   <- Derived.make(current)
          value <- ref.updateAndGetZIO(_ => IO.succeed(update))
        } yield assert(value)(equalTo(update))
      },
      test("updateAndGet with failure") {
        for {
          ref   <- Derived.make[String](current)
          value <- ref.updateAndGetZIO(_ => IO.fail(failure)).exit
        } yield assert(value)(fails(equalTo(failure)))
      } @@ zioTag(errors),
      test("updateSomeAndGet") {
        for {
          ref   <- Derived.make[State](Active)
          value <- ref.updateSomeAndGetZIO { case Closed => IO.succeed(Active) }
        } yield assert(value)(equalTo(Active))
      },
      test("updateSomeAndGet twice") {
        for {
          ref    <- Derived.make[State](Active)
          value1 <- ref.updateSomeAndGetZIO { case Active => IO.succeed(Changed) }
          value2 <- ref.updateSomeAndGetZIO {
                      case Active  => IO.succeed(Changed)
                      case Changed => IO.succeed(Closed)
                    }
        } yield assert(value1)(equalTo(Changed)) && assert(value2)(equalTo(Closed))
      },
      test("updateSomeAndGet with failure") {
        for {
          ref   <- Derived.make[State](Active)
          value <- ref.updateSomeAndGetZIO { case Active => IO.fail(failure) }.exit
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
          ref   <- DerivedAll.make(current)
          value <- ref.get
        } yield assert(value)(equalTo(current))
      },
      test("getAndUpdate") {
        for {
          ref    <- DerivedAll.make(current)
          value1 <- ref.getAndUpdateZIO(_ => IO.succeed(update))
          value2 <- ref.get
        } yield assert(value1)(equalTo(current)) && assert(value2)(equalTo(update))
      },
      test("getAndUpdate with failure") {
        for {
          ref   <- DerivedAll.make[String](current)
          value <- ref.getAndUpdateZIO(_ => IO.fail(failure)).exit
        } yield assert(value)(fails(equalTo(failure)))
      },
      test("getAndUpdateSome") {
        for {
          ref    <- DerivedAll.make[State](Active)
          value1 <- ref.getAndUpdateSomeZIO { case Closed => IO.succeed(Active) }
          value2 <- ref.get
        } yield assert(value1)(equalTo(Active)) && assert(value2)(equalTo(Active))
      },
      test("getAndUpdateSome twice") {
        for {
          ref    <- DerivedAll.make[State](Active)
          value1 <- ref.getAndUpdateSomeZIO { case Active => IO.succeed(Changed) }
          value2 <- ref.getAndUpdateSomeZIO {
                      case Active  => IO.succeed(Changed)
                      case Changed => IO.succeed(Closed)
                    }
          value3 <- ref.get
        } yield assert(value1)(equalTo(Active)) && assert(value2)(equalTo(Changed)) && assert(value3)(equalTo(Closed))
      },
      test("getAndUpdateSome with failure") {
        for {
          ref   <- DerivedAll.make[State](Active)
          value <- ref.getAndUpdateSomeZIO { case Active => IO.fail(failure) }.exit
        } yield assert(value)(fails(equalTo(failure)))
      },
      test("interrupt parent fiber and update") {
        for {
          promise    <- Promise.make[Nothing, Ref.Synchronized[State]]
          latch      <- Promise.make[Nothing, Unit]
          makeAndWait = promise.complete(DerivedAll.make[State](Active)) *> latch.await
          fiber      <- makeAndWait.fork
          ref        <- promise.await
          _          <- fiber.interrupt
          value      <- ref.updateAndGetZIO(_ => ZIO.succeed(Closed))
        } yield assert(value)(equalTo(Closed))
      } @@ zioTag(interruption),
      test("modify") {
        for {
          ref   <- DerivedAll.make(current)
          r     <- ref.modifyZIO(_ => IO.succeed(("hello", update)))
          value <- ref.get
        } yield assert(r)(equalTo("hello")) && assert(value)(equalTo(update))
      },
      test("modify with failure") {
        for {
          ref <- DerivedAll.make[String](current)
          r   <- ref.modifyZIO(_ => IO.fail(failure)).exit
        } yield assert(r)(fails(equalTo(failure)))
      } @@ zioTag(errors),
      test("modify twice") {
        for {
          ref    <- DerivedAll.make[State](Active)
          r1     <- ref.modifySomeZIO("doesn't change the state") { case Active => IO.succeed("changed" -> Changed) }
          value1 <- ref.get
          r2 <- ref.modifySomeZIO("doesn't change the state") {
                  case Active  => IO.succeed("changed" -> Changed)
                  case Changed => IO.succeed("closed" -> Closed)
                }
          value2 <- ref.get
        } yield assert(r1)(equalTo("changed")) &&
          assert(value1)(equalTo(Changed)) &&
          assert(r2)(equalTo("closed")) &&
          assert(value2)(equalTo(Closed))
      },
      test("modifySome") {
        for {
          ref   <- DerivedAll.make[State](Active)
          r     <- ref.modifySomeZIO("State doesn't change") { case Closed => IO.succeed("active" -> Active) }
          value <- ref.get
        } yield assert(r)(equalTo("State doesn't change")) && assert(value)(equalTo(Active))
      },
      test("modifySome with failure not triggered") {
        for {
          ref <- DerivedAll.make[State](Active)
          r <-
            ref.modifySomeZIO("State doesn't change") { case Closed => IO.fail(failure) }.orDieWith(new Exception(_))
          value <- ref.get
        } yield assert(r)(equalTo("State doesn't change")) && assert(value)(equalTo(Active))
      } @@ zioTag(errors),
      test("modifySome with failure") {
        for {
          ref   <- DerivedAll.make[State](Active)
          value <- ref.modifySomeZIO("State doesn't change") { case Active => IO.fail(failure) }.exit
        } yield assert(value)(fails(equalTo(failure)))
      } @@ zioTag(errors),
      test("modifySome with fatal error") {
        for {
          ref   <- DerivedAll.make[State](Active)
          value <- ref.modifySomeZIO("State doesn't change") { case Active => IO.dieMessage(fatalError) }.exit
        } yield assert(value)(dies(hasMessage(equalTo(fatalError))))
      } @@ zioTag(errors),
      test("set") {
        for {
          ref   <- DerivedAll.make(current)
          _     <- ref.set(update)
          value <- ref.get
        } yield assert(value)(equalTo(update))
      },
      test("updateAndGet") {
        for {
          ref   <- DerivedAll.make(current)
          value <- ref.updateAndGetZIO(_ => IO.succeed(update))
        } yield assert(value)(equalTo(update))
      },
      test("updateAndGet with failure") {
        for {
          ref   <- DerivedAll.make[String](current)
          value <- ref.updateAndGetZIO(_ => IO.fail(failure)).exit
        } yield assert(value)(fails(equalTo(failure)))
      } @@ zioTag(errors),
      test("updateSomeAndGet") {
        for {
          ref   <- DerivedAll.make[State](Active)
          value <- ref.updateSomeAndGetZIO { case Closed => IO.succeed(Active) }
        } yield assert(value)(equalTo(Active))
      },
      test("updateSomeAndGet twice") {
        for {
          ref    <- DerivedAll.make[State](Active)
          value1 <- ref.updateSomeAndGetZIO { case Active => IO.succeed(Changed) }
          value2 <- ref.updateSomeAndGetZIO {
                      case Active  => IO.succeed(Changed)
                      case Changed => IO.succeed(Closed)
                    }
        } yield assert(value1)(equalTo(Changed)) && assert(value2)(equalTo(Closed))
      },
      test("updateSomeAndGet with failure") {
        for {
          ref   <- DerivedAll.make[State](Active)
          value <- ref.updateSomeAndGetZIO { case Active => IO.fail(failure) }.exit
        } yield assert(value)(fails(equalTo(failure)))
      } @@ zioTag(errors)
    ),
    suite("zip")(
      test("updates can be performed atomically") {
        for {
          left    <- Ref.Synchronized.make(0)
          right   <- Ref.Synchronized.make(1)
          composed = left <*> right
          effect   = composed.update { case (a, b) => (b, a + b) }
          _       <- ZIO.collectAllParDiscard(ZIO.replicate(20)(effect))
          tuple   <- composed.get
          (a, b)   = tuple
        } yield assert(a)(equalTo(6765)) && assert(b)(equalTo(10946))
      },
      test("partial writes cannot be observed by other fibers") {
        for {
          left    <- Ref.Synchronized.make(0)
          right   <- Ref.Synchronized.make(0)
          composed = left <*> right
          effect   = composed.getAndUpdate { case (a, b) => (a + 1, b + 1) }
          _       <- ZIO.forkAllDiscard(ZIO.replicate(100)(effect))
          tuple   <- composed.get
          (a, b)   = tuple
        } yield assert(a)(equalTo(b))
      }
    ) @@ nonFlaky
  )

  val (current, update) = ("value", "new value")
  val failure           = "failure"
  val fatalError        = ":-0"

  sealed trait State
  case object Active  extends State
  case object Changed extends State
  case object Closed  extends State

  object Derived {
    def make[A](a: A): UIO[Ref.Synchronized[A]] =
      Ref.Synchronized.make(a).map(ref => ref.foldZIO(identity, identity, ZIO.succeed(_), ZIO.succeed(_)))
  }

  object DerivedAll {
    def make[A](a: A): UIO[Ref.Synchronized[A]] =
      Ref.Synchronized
        .make(a)
        .map(ref => ref.foldAllZIO(identity, identity, identity, a => _ => ZIO.succeed(a), ZIO.succeed(_)))
  }
}

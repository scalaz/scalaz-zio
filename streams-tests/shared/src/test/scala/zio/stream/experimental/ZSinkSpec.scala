package zio.stream.experimental

import zio._
import zio.test.Assertion._
import zio.test.TestAspect.jvmOnly
import zio.test._
import zio.test.environment.TestClock

object ZSinkSpec extends ZIOBaseSpec {

  import ZIOTag._

  def spec: ZSpec[Environment, Failure] = {
    suite("ZSinkSpec")(
      suite("Constructors")(
        suite("collectAllN")(
          testM("respects the given limit") {
            ZStream
              .fromChunk(Chunk(1, 2, 3, 4))
              .transduce(ZSink.collectAllN[Nothing, Int](3))
              .runCollect
              .map(assert(_)(equalTo(Chunk(Chunk(1, 2, 3), Chunk(4)))))
          },
          testM("produces empty trailing chunks") {
            ZStream
              .fromChunk(Chunk(1, 2, 3, 4))
              .transduce(ZSink.collectAllN[Nothing, Int](4))
              .runCollect
              .map(assert(_)(equalTo(Chunk(Chunk(1, 2, 3, 4), Chunk()))))
          },
          testM("handles empty input") {
            ZStream
              .fromChunk(Chunk.empty: Chunk[Int])
              .transduce(ZSink.collectAllN[Nothing, Int](3))
              .runCollect
              .map(assert(_)(equalTo(Chunk(Chunk()))))
          }
        ),
        testM("collectAllToSet")(
          assertM(
            ZStream(1, 2, 3, 3, 4)
              .run(ZSink.collectAllToSet[Nothing, Int])
          )(equalTo(Set(1, 2, 3, 4)))
        ),
        suite("collectAllToSetN")(
          testM("respect the given limit") {
            ZStream
              .fromChunks(Chunk(1, 2, 1), Chunk(2, 3, 3, 4))
              .transduce(ZSink.collectAllToSetN[Nothing, Int](3))
              .runCollect
              .map(assert(_)(equalTo(Chunk(Set(1, 2, 3), Set(4)))))
          },
          testM("handles empty input") {
            ZStream
              .fromChunk(Chunk.empty: Chunk[Int])
              .transduce(ZSink.collectAllToSetN[Nothing, Int](3))
              .runCollect
              .map(assert(_)(equalTo(Chunk(Set.empty[Int]))))
          }
        ),
        testM("collectAllToMap")(
          assertM(
            ZStream
              .range(0, 10)
              .run(ZSink.collectAllToMap((_: Int) % 3)(_ + _))
          )(equalTo(Map[Int, Int](0 -> 18, 1 -> 12, 2 -> 15)))
        ),
        suite("collectAllToMapN")(
          testM("respects the given limit") {
            ZStream
              .fromChunk(Chunk(1, 1, 2, 2, 3, 2, 4, 5))
              .transduce(ZSink.collectAllToMapN(2)((_: Int) % 3)(_ + _))
              .runCollect
              .map(assert(_)(equalTo(Chunk(Map(1 -> 2, 2 -> 4), Map(0 -> 3, 2 -> 2), Map(1 -> 4, 2 -> 5)))))
          },
          testM("collects as long as map size doesn't exceed the limit") {
            ZStream
              .fromChunks(Chunk(0, 1, 2), Chunk(3, 4, 5), Chunk(6, 7, 8, 9))
              .transduce(ZSink.collectAllToMapN(3)((_: Int) % 3)(_ + _))
              .runCollect
              .map(assert(_)(equalTo(Chunk(Map(0 -> 18, 1 -> 12, 2 -> 15)))))
          },
          testM("handles empty input") {
            ZStream
              .fromChunk(Chunk.empty: Chunk[Int])
              .transduce(ZSink.collectAllToMapN(2)((_: Int) % 3)(_ + _))
              .runCollect
              .map(assert(_)(equalTo(Chunk(Map.empty[Int, Int]))))
          }
        ),
        suite("accessSink")(
          testM("accessSink") {
            assertM(
              ZStream("ignore this")
                .run(ZSink.accessSink[String](ZSink.succeed(_)).provide("use this"))
            )(equalTo("use this"))
          }
        ),
        suite("collectAllWhileWith")(
          testM("example 1") {
            ZIO
              .foreach(List(1, 3, 20)) { chunkSize =>
                assertM(
                  ZStream
                    .fromChunk(Chunk.fromIterable(1 to 10))
                    .chunkN(chunkSize)
                    .run(ZSink.sum[Nothing, Int].collectAllWhileWith(-1)((s: Int) => s == s)(_ + _))
                )(equalTo(54))
              }
              .map(_.reduce(_ && _))
          },
          testM("example 2") {
            val sink = ZSink
              .head[Nothing, Int]
              .collectAllWhileWith[List[Int]](Nil)((a: Option[Int]) => a.fold(true)(_ < 5))(
                (a: List[Int], b: Option[Int]) => a ++ b.toList
              )
            val stream = ZStream.fromChunk(Chunk.fromIterable(1 to 100))
            assertM((stream ++ stream).chunkN(3).run(sink))(equalTo(List(1, 2, 3, 4)))
          }
        ),
        testM("head")(
          checkM(Gen.listOf(Gen.small(Gen.chunkOfN(_)(Gen.anyInt)))) { chunks =>
            val headOpt = ZStream.fromChunks(chunks: _*).run(ZSink.head[Nothing, Int])
            assertM(headOpt)(equalTo(chunks.flatMap(_.toSeq).headOption))
          }
        ),
        testM("last")(
          checkM(Gen.listOf(Gen.small(Gen.chunkOfN(_)(Gen.anyInt)))) { chunks =>
            val lastOpt = ZStream.fromChunks(chunks: _*).run(ZSink.last)
            assertM(lastOpt)(equalTo(chunks.flatMap(_.toSeq).lastOption))
          }
        ),
        suite("managed")(
          testM("happy path") {
            for {
              closed <- Ref.make[Boolean](false)
              res     = ZManaged.acquireReleaseWith(ZIO.succeed(100))(_ => closed.set(true))
              sink = ZSink.managed[Any, Any, Any, Any, Int, Nothing, (Long, Boolean)](res)(m =>
                       ZSink.count.mapZIO(cnt => closed.get.map(cl => (cnt + m, cl)))
                     )
              resAndState <- ZStream(1, 2, 3).run(sink)
              finalState  <- closed.get
            } yield {
              assert(resAndState._1)(equalTo(103L)) && assert(resAndState._2)(isFalse) && assert(finalState)(isTrue)
            }
          },
          testM("sad path") {
            for {
              closed     <- Ref.make[Boolean](false)
              res         = ZManaged.acquireReleaseWith(ZIO.succeed(100))(_ => closed.set(true))
              sink        = ZSink.managed[Any, String, Any, String, Int, Nothing, String](res)(_ => ZSink.succeed("ok"))
              r          <- ZStream.fail("fail").run(sink).either
              finalState <- closed.get
            } yield assert(r)(equalTo(Right("ok"))) && assert(finalState)(isTrue)
          }
        )
      ),
      testM("map")(
        assertM(ZStream.range(1, 10).run(ZSink.succeed(1).map(_.toString)))(
          equalTo("1")
        )
      ),
      suite("mapZIO")(
        testM("happy path")(
          assertM(ZStream.range(1, 10).run(ZSink.succeed(1).mapZIO(e => ZIO.succeed(e + 1))))(
            equalTo(2)
          )
        ),
        testM("failure")(
          assertM(ZStream.range(1, 10).run(ZSink.succeed(1).mapZIO(_ => ZIO.fail("fail"))).flip)(
            equalTo("fail")
          )
        )
      ),
      testM("filterInput")(
        assertM(ZStream.range(1, 10).run(ZSink.collectAll.filterInput(_ % 2 == 0)))(
          equalTo(Chunk(2, 4, 6, 8))
        )
      ),
      suite("filterInputZIO")(
        testM("happy path")(
          assertM(ZStream.range(1, 10).run(ZSink.collectAll.filterInputZIO(i => ZIO.succeed(i % 2 == 0))))(
            equalTo(Chunk(2, 4, 6, 8))
          )
        ),
        testM("failure")(
          assertM(ZStream.range(1, 10).run(ZSink.collectAll.filterInputZIO(_ => ZIO.fail("fail"))).flip)(
            equalTo("fail")
          )
        )
      ),
      testM("mapError")(
        assertM(ZStream.range(1, 10).run(ZSink.fail("fail").mapError(s => s + "!")).either)(
          equalTo(Left("fail!"))
        )
      ),
      testM("as")(
        assertM(ZStream.range(1, 10).run(ZSink.succeed(1).as("as")))(
          equalTo("as")
        )
      ),
      suite("contramap")(
        testM("happy path") {
          val parser = ZSink.collectAll[Nothing, Int].contramap[String](_.toInt)
          assertM(ZStream("1", "2", "3").run(parser))(equalTo(Chunk(1, 2, 3)))
        },
        testM("error") {
          val parser = ZSink.fail("Ouch").contramap[String](_.toInt)
          assertM(ZStream("1", "2", "3").run(parser).either)(isLeft(equalTo("Ouch")))
        } @@ zioTag(errors)
      ),
      suite("contramapChunks")(
        testM("happy path") {
          val parser = ZSink.collectAll[Nothing, Int].contramapChunks[String](_.map(_.toInt))
          assertM(ZStream("1", "2", "3").run(parser))(equalTo(Chunk(1, 2, 3)))
        },
        testM("error") {
          val parser = ZSink.fail("Ouch").contramapChunks[String](_.map(_.toInt))
          assertM(ZStream("1", "2", "3").run(parser).either)(isLeft(equalTo("Ouch")))
        } @@ zioTag(errors)
      ),
      suite("contramapZIO")(
        testM("happy path") {
          val parser = ZSink.collectAll[Throwable, Int].contramapZIO[Any, Throwable, String](a => ZIO.attempt(a.toInt))
          assertM(ZStream("1", "2", "3").run(parser))(equalTo(Chunk(1, 2, 3)))
        },
        testM("error") {
          val parser = ZSink.fail("Ouch").contramapZIO[Any, Throwable, String](a => ZIO.attempt(a.toInt))
          assertM(ZStream("1", "2", "3").run(parser).either)(isLeft(equalTo("Ouch")))
        } @@ zioTag(errors),
        testM("error in transformation") {
          val parser = ZSink.collectAll[Throwable, Int].contramapZIO[Any, Throwable, String](a => ZIO.attempt(a.toInt))
          assertM(ZStream("1", "a").run(parser).either)(isLeft(hasMessage(equalTo("For input string: \"a\""))))
        } @@ zioTag(errors)
      ),
      suite("contramapChunksZIO")(
        testM("happy path") {
          val parser =
            ZSink
              .collectAll[Throwable, Int]
              .contramapChunksZIO[Any, Throwable, String](_.mapZIO(a => ZIO.attempt(a.toInt)))
          assertM(ZStream("1", "2", "3").run(parser))(equalTo(Chunk(1, 2, 3)))
        },
        testM("error") {
          val parser =
            ZSink.fail("Ouch").contramapChunksZIO[Any, Throwable, String](_.mapZIO(a => ZIO.attempt(a.toInt)))
          assertM(ZStream("1", "2", "3").run(parser).either)(isLeft(equalTo("Ouch")))
        } @@ zioTag(errors),
        testM("error in transformation") {
          val parser =
            ZSink
              .collectAll[Throwable, Int]
              .contramapChunksZIO[Any, Throwable, String](_.mapZIO(a => ZIO.attempt(a.toInt)))
          assertM(ZStream("1", "a").run(parser).either)(isLeft(hasMessage(equalTo("For input string: \"a\""))))
        } @@ zioTag(errors)
      ),
      testM("collectAllWhile") {
        val sink   = ZSink.collectAllWhile[Nothing, Int](_ < 5)
        val input  = List(Chunk(3, 4, 5, 6, 7, 2), Chunk.empty, Chunk(3, 4, 5, 6, 5, 4, 3, 2), Chunk.empty)
        val result = ZStream.fromChunks(input: _*).transduce(sink).runCollect
        assertM(result)(equalTo(Chunk(Chunk(3, 4), Chunk(), Chunk(), Chunk(2, 3, 4), Chunk(), Chunk(), Chunk(4, 3, 2))))
      },
      testM("collectAllWhileM") {
        val sink   = ZSink.collectAllWhileZIO[Any, Nothing, Int]((i: Int) => ZIO.succeed(i < 5))
        val input  = List(Chunk(3, 4, 5, 6, 7, 2), Chunk.empty, Chunk(3, 4, 5, 6, 5, 4, 3, 2), Chunk.empty)
        val result = ZStream.fromChunks(input: _*).transduce(sink).runCollect
        assertM(result)(equalTo(Chunk(Chunk(3, 4), Chunk(), Chunk(), Chunk(2, 3, 4), Chunk(), Chunk(), Chunk(4, 3, 2))))
      },
      testM("foldLeft equivalence with Chunk#foldLeft")(
        checkM(
          Gen.small(ZStreamGen.pureStreamGen(Gen.anyInt, _)),
          Gen.function2(Gen.anyString),
          Gen.anyString
        ) { (s, f, z) =>
          for {
            xs <- s.run(ZSink.foldLeft(z)(f))
            ys <- s.runCollect.map(_.foldLeft(z)(f))
          } yield assert(xs)(equalTo(ys))
        }
      ),
      suite("foldZIO")(
        testM("empty")(
          assertM(
            ZStream.empty
              .transduce(
                ZSink.foldZIO(0)(_ => true)((x, y: Int) => ZIO.succeed(x + y))
              )
              .runCollect
          )(equalTo(Chunk(0)))
        ),
        testM("short circuits") {
          val empty: ZStream[Any, Nothing, Int]     = ZStream.empty
          val single: ZStream[Any, Nothing, Int]    = ZStream.succeed(1)
          val double: ZStream[Any, Nothing, Int]    = ZStream(1, 2)
          val failed: ZStream[Any, String, Nothing] = ZStream.fail("Ouch")

          def run[E](stream: ZStream[Any, E, Int]) =
            (for {
              effects <- Ref.make[List[Int]](Nil)
              exit <- stream
                        .transduce(ZSink.foldZIO(0)(_ => true) { (_, a: Int) =>
                          effects.update(a :: _) *> UIO.succeed(30)
                        })
                        .runCollect
              result <- effects.get
            } yield exit -> result).exit

          (assertM(run(empty))(succeeds(equalTo((Chunk(0), Nil)))) <*>
            assertM(run(single))(succeeds(equalTo((Chunk(30), List(1))))) <*>
            assertM(run(double))(succeeds(equalTo((Chunk(30), List(2, 1))))) <*>
            assertM(run(failed))(fails(equalTo("Ouch")))).map { case (r1, r2, r3, r4) =>
            r1 && r2 && r3 && r4
          }
        },
        testM("equivalence with List#foldLeft") {
          val ioGen = ZStreamGen.successes(Gen.anyString)
          checkM(Gen.small(ZStreamGen.pureStreamGen(Gen.anyInt, _)), Gen.function2(ioGen), ioGen) { (s, f, z) =>
            for {
              sinkResult <- z.flatMap(z => s.run(ZSink.foldLeftZIO(z)(f)))
              foldResult <- s.runFold(List[Int]())((acc, el) => el :: acc)
                              .map(_.reverse)
                              .flatMap(_.foldLeft(z)((acc, el) => acc.flatMap(f(_, el))))
                              .exit
            } yield assert(foldResult.succeeded)(isTrue) implies assert(foldResult)(succeeds(equalTo(sinkResult)))
          }
        }
      ),
      suite("fold")(
        testM("empty")(
          assertM(
            ZStream.empty
              .transduce(ZSink.fold[Nothing, Int, Int](0)(_ => true)(_ + _))
              .runCollect
          )(equalTo(Chunk(0)))
        ),
        testM("short circuits") {
          val empty: ZStream[Any, Nothing, Int]     = ZStream.empty
          val single: ZStream[Any, Nothing, Int]    = ZStream.succeed(1)
          val double: ZStream[Any, Nothing, Int]    = ZStream(1, 2)
          val failed: ZStream[Any, String, Nothing] = ZStream.fail("Ouch")

          def run[E](stream: ZStream[Any, E, Int]) =
            (for {
              effects <- Ref.make[List[Int]](Nil)
              exit <- stream
                        .transduce(ZSink.foldZIO(0)(_ => true) { (_, a: Int) =>
                          effects.update(a :: _) *> UIO.succeed(30)
                        })
                        .runCollect
              result <- effects.get
            } yield (exit, result)).exit

          (assertM(run(empty))(succeeds(equalTo((Chunk(0), Nil)))) <*>
            assertM(run(single))(succeeds(equalTo((Chunk(30), List(1))))) <*>
            assertM(run(double))(succeeds(equalTo((Chunk(30), List(2, 1))))) <*>
            assertM(run(failed))(fails(equalTo("Ouch")))).map { case (r1, r2, r3, r4) =>
            r1 && r2 && r3 && r4
          }
        },
        testM("termination in the middle")(
          assertM(ZStream.range(1, 10).run(ZSink.fold[Nothing, Int, Int](0)(_ <= 5)((a, b) => a + b)))(equalTo(6))
        ),
        testM("immediate termination")(
          assertM(ZStream.range(1, 10).run(ZSink.fold[Nothing, Int, Int](0)(_ <= -1)((a, b) => a + b)))(equalTo(0))
        ),
        testM("termination in the middle")(
          assertM(ZStream.range(1, 10).run(ZSink.fold[Nothing, Int, Int](0)(_ <= 500)((a, b) => a + b)))(equalTo(45))
        )
      ),
      testM("foldUntil")(
        assertM(
          ZStream[Long](1, 1, 1, 1, 1, 1)
            .transduce(ZSink.foldUntil(0L, 3)(_ + (_: Long)))
            .runCollect
        )(equalTo(Chunk(3L, 3L, 0L)))
      ),
      testM("foldUntilM")(
        assertM(
          ZStream[Long](1, 1, 1, 1, 1, 1)
            .transduce(ZSink.foldUntilZIO(0L, 3)((s, a: Long) => UIO.succeedNow(s + a)))
            .runCollect
        )(equalTo(Chunk(3L, 3L, 0L)))
      ),
      testM("foldWeighted")(
        assertM(
          ZStream[Long](1, 5, 2, 3)
            .transduce(
              ZSink.foldWeighted(List[Long]())((_, x: Long) => x * 2, 12)((acc, el) => el :: acc).map(_.reverse)
            )
            .runCollect
        )(equalTo(Chunk(List(1L, 5L), List(2L, 3L))))
      ),
      suite("foldWeightedDecompose")(
        testM("simple example")(
          assertM(
            ZStream(1, 5, 1)
              .transduce(
                ZSink
                  .foldWeightedDecompose(List[Int]())(
                    (_, i: Int) => i.toLong,
                    4,
                    (i: Int) =>
                      if (i > 1) Chunk(i - 1, 1)
                      else Chunk(i)
                  )((acc, el) => el :: acc)
                  .map(_.reverse)
              )
              .runCollect
          )(equalTo(Chunk(List(1, 3), List(1, 1, 1))))
        ),
        testM("empty stream")(
          assertM(
            ZStream.empty
              .transduce(
                ZSink.foldWeightedDecompose(0)((_, x: Int) => x.toLong, 1000, Chunk.single(_: Int))(_ + _)
              )
              .runCollect
          )(equalTo(Chunk(0)))
        )
      ),
      testM("foldWeightedM")(
        assertM(
          ZStream[Long](1, 5, 2, 3)
            .transduce(
              ZSink
                .foldWeightedZIO(List.empty[Long])((_, a: Long) => UIO.succeedNow(a * 2), 12)((acc, el) =>
                  UIO.succeedNow(el :: acc)
                )
                .map(_.reverse)
            )
            .runCollect
        )(equalTo(Chunk(List(1L, 5L), List(2L, 3L))))
      ),
      suite("foldWeightedDecomposeM")(
        testM("simple example")(
          assertM(
            ZStream(1, 5, 1)
              .transduce(
                ZSink
                  .foldWeightedDecomposeZIO(List.empty[Int])(
                    (_, i: Int) => UIO.succeedNow(i.toLong),
                    4,
                    (i: Int) =>
                      UIO.succeedNow(
                        if (i > 1) Chunk(i - 1, 1)
                        else Chunk(i)
                      )
                  )((acc, el) => UIO.succeedNow(el :: acc))
                  .map(_.reverse)
              )
              .runCollect
          )(equalTo(Chunk(List(1, 3), List(1, 1, 1))))
        ),
        testM("empty stream")(
          assertM(
            ZStream.empty
              .transduce(
                ZSink.foldWeightedDecomposeZIO[Any, Nothing, Int, Int](0)(
                  (_, x) => ZIO.succeed(x.toLong),
                  1000,
                  x => ZIO.succeed(Chunk.single(x))
                )((x, y) => ZIO.succeed(x + y))
              )
              .runCollect
          )(equalTo(Chunk(0)))
        )
      ),
      suite("fail")(
        testM("handles leftovers") {
          val s =
            ZSink
              .fail("boom")
              .foldSink(err => ZSink.collectAll[String, Int].map(c => (c, err)), _ => sys.error("impossible"))
          assertM(ZStream(1, 2, 3).run(s))(equalTo((Chunk(1, 2, 3), "boom")))
        }
      ),
      // suite("foreach")(
      //   testM("preserves leftovers in case of failure") {
      //     for {
      //       acc <- Ref.make[Int](0)
      //       s    = ZSink.foreach[Any, String, Int]((i: Int) => if (i == 4) ZIO.fail("boom") else acc.update(_ + i))
      //       sink = s.foldSink(_ => ZSink.collectAll[String, Int], _ => sys.error("impossible"))
      //       leftover <- ZStream.fromChunks(Chunk(1, 2), Chunk(3, 4, 5)).run(sink)
      //       sum      <- acc.get
      //     } yield {
      //       assert(sum)(equalTo(6)) && assert(leftover)(equalTo(Chunk(5)))
      //     }
      //   }
      // ),
      suite("foreachWhile")(
        testM("handles leftovers") {
          val leftover = ZStream
            .fromIterable(1 to 5)
            .run(ZSink.foreachWhile((n: Int) => ZIO.succeed(n <= 3)).exposeLeftover)
            .map(_._2)
          assertM(leftover)(equalTo(Chunk(4, 5)))
        }
      ),
      suite("fromEffect")(
        testM("result is ok") {
          val s = ZSink.fromZIO(ZIO.succeed("ok"))
          assertM(ZStream(1, 2, 3).run(s))(
            equalTo("ok")
          )
        }
      ),
      suite("succeed")(
        testM("result is ok") {
          assertM(ZStream(1, 2, 3).run(ZSink.succeed("ok")))(
            equalTo("ok")
          )
        }
      ),
      suite("Combinators")(
        //      testM("raceBoth") {
        //        checkM(Gen.listOf(Gen.int(0, 10)), Gen.boolean, Gen.boolean) { (ints, success1, success2) =>
        //          val stream = ints ++ (if (success1) List(20) else Nil) ++ (if (success2) List(40) else Nil)
        //          sinkRaceLaw(ZStream.fromIterable(Random.shuffle(stream)), findSink(20), findSink(40))
        //        }
        //      },
        //      suite("zipWithPar")(
        //        testM("coherence") {
        //          checkM(Gen.listOf(Gen.int(0, 10)), Gen.boolean, Gen.boolean) { (ints, success1, success2) =>
        //            val stream = ints ++ (if (success1) List(20) else Nil) ++ (if (success2) List(40) else Nil)
        //            SinkUtils
        //              .zipParLaw(ZStream.fromIterable(Random.shuffle(stream)), findSink(20), findSink(40))
        //          }
        //        },
        //        suite("zipRight (*>)")(
        //          testM("happy path") {
        //            assertM(ZStream(1, 2, 3).run(ZSink.head.zipParRight(ZSink.succeed[Int, String]("Hello"))))(
        //              equalTo(("Hello"))
        //            )
        //          }
        //        ),
        //        suite("zipWith")(testM("happy path") {
        //          assertM(ZStream(1, 2, 3).run(ZSink.head.zipParLeft(ZSink.succeed[Int, String]("Hello"))))(
        //            equalTo(Some(1))
        //          )
        //        })
        //      ),
        testM("untilOutputZIO with head sink") {
          val sink = ZSink.head[Nothing, Int].untilOutputZIO(h => ZIO.succeed(h.fold(false)(_ >= 10)))
          val assertions = ZIO.foreach(Chunk(1, 3, 7, 20)) { n =>
            assertM(ZStream.fromIterable(1 to 100).chunkN(n).run(sink))(equalTo(Some(Some(10))))
          }
          assertions.map(tst => tst.reduce(_ && _))
        },
        testM("untilOutputZIO take sink across multiple chunks") {
          val sink = ZSink.take[Nothing, Int](4).untilOutputZIO(s => ZIO.succeed(s.sum > 10))

          assertM(ZStream.fromIterable(1 to 8).chunkN(2).run(sink))(equalTo(Some(Chunk(5, 6, 7, 8))))
        },
        testM("untilOutputZIO empty stream terminates with none") {
          assertM(
            ZStream.fromIterable(List.empty[Int]).run(ZSink.sum[Nothing, Int].untilOutputZIO(s => ZIO.succeed(s > 0)))
          )(equalTo(None))
        },
        testM("untilOutputZIO unsatisfied condition terminates with none") {
          assertM(
            ZStream
              .fromIterable(List(1, 2))
              .run(ZSink.head[Nothing, Int].untilOutputZIO(h => ZIO.succeed(h.fold(false)(_ >= 3))))
          )(equalTo(None))
        },
        suite("flatMap")(
          testM("non-empty input") {
            assertM(
              ZStream(1, 2, 3)
                .run(ZSink.head[Nothing, Int].flatMap((x: Option[Int]) => ZSink.succeed(x)))
            )(equalTo(Some(1)))
          },
          testM("empty input") {
            assertM(ZStream.empty.run(ZSink.head[Nothing, Int].flatMap(h => ZSink.succeed(h))))(
              equalTo(None)
            )
          },
          testM("with leftovers") {
            val headAndCount =
              ZSink.head[Nothing, Int].flatMap((h: Option[Int]) => ZSink.count.map(cnt => (h, cnt)))
            checkM(Gen.listOf(Gen.small(Gen.chunkOfN(_)(Gen.anyInt)))) { chunks =>
              ZStream.fromChunks(chunks: _*).run(headAndCount).map {
                case (head, count) => {
                  assert(head)(equalTo(chunks.flatten.headOption)) &&
                  assert(count + head.toList.size)(equalTo(chunks.map(_.size.toLong).sum))
                }
              }
            }
          },
          testM("leftovers are kept in order") {
            Ref.make(Chunk[Chunk[Int]]()).flatMap { readData =>
              def takeN(n: Int) =
                ZSink.take[Nothing, Int](n).mapZIO(c => readData.update(_ :+ c))

              def taker(data: Chunk[Chunk[Int]], n: Int): (Chunk[Int], Chunk[Chunk[Int]], Boolean) = {
                import scala.collection.mutable
                val buffer   = mutable.Buffer(data: _*)
                val builder  = mutable.Buffer[Int]()
                var wasSplit = false

                while (builder.size < n && buffer.nonEmpty) {
                  val popped = buffer.remove(0)

                  if ((builder.size + popped.size) <= n) builder ++= popped
                  else {
                    val splitIndex  = n - builder.size
                    val (take, ret) = popped.splitAt(splitIndex)
                    builder ++= take
                    buffer.prepend(ret)

                    if (splitIndex > 0)
                      wasSplit = true
                  }
                }

                (Chunk.fromIterable(builder), Chunk.fromIterable(buffer), wasSplit)
              }

              val gen =
                for {
                  sequenceSize <- Gen.int(1, 50)
                  takers       <- Gen.int(1, 5)
                  takeSizes    <- Gen.listOfN(takers)(Gen.int(1, sequenceSize))
                  inputs       <- Gen.chunkOfN(sequenceSize)(ZStreamGen.tinyChunkOf(Gen.anyInt))
                  (expectedTakes, leftoverInputs, wasSplit) = takeSizes.foldLeft((Chunk[Chunk[Int]](), inputs, false)) {
                                                                case ((takenChunks, leftover, _), takeSize) =>
                                                                  val (taken, rest, wasSplit) =
                                                                    taker(leftover, takeSize)
                                                                  (takenChunks :+ taken, rest, wasSplit)
                                                              }
                  expectedLeftovers = if (wasSplit) leftoverInputs.head
                                      else Chunk()
                } yield (inputs, takeSizes, expectedTakes, expectedLeftovers)

              checkM(gen) { case (inputs, takeSizes, expectedTakes, expectedLeftovers) =>
                val takingSinks = takeSizes.map(takeN(_)).reduce(_ *> _).channel.doneCollect
                val channel     = ZChannel.writeAll(inputs: _*) >>> takingSinks

                (channel.run <*> readData.getAndSet(Chunk())).map { case (leftovers, _, takenChunks) =>
                  assert(leftovers.flatten)(equalTo(expectedLeftovers)) &&
                    assert(takenChunks)(equalTo(expectedTakes))
                }
              }
            }
          } @@ jvmOnly
        ),
        suite("take")(
          testM("take")(
            checkM(Gen.chunkOf(Gen.small(Gen.chunkOfN(_)(Gen.anyInt))), Gen.anyInt) { (chunks, n) =>
              ZStream
                .fromChunks(chunks: _*)
                .peel(ZSink.take[Nothing, Int](n))
                .flatMap { case (chunk, stream) =>
                  stream.runCollect.toManaged.map { leftover =>
                    assert(chunk)(equalTo(chunks.flatten.take(n))) &&
                    assert(leftover)(equalTo(chunks.flatten.drop(n)))
                  }
                }
                .useNow
            }
          )
        ),
        testM("timed") {
          for {
            f <- ZStream.fromIterable(1 to 10).mapZIO(i => Clock.sleep(10.millis).as(i)).run(ZSink.timed).fork
            _ <- TestClock.adjust(100.millis)
            r <- f.join
          } yield assert(r)(isGreaterThanEqualTo(100.millis))
        },
        suite("utf8Decode")(
          testM("running with regular strings") {
            checkM(Gen.anyString.map(s => Chunk.fromArray(s.getBytes("UTF-8")))) { bytes =>
              ZStream
                .fromChunk(bytes)
                .run(ZSink.utf8Decode)
                .map(result =>
                  assert(bytes)(isNonEmpty) implies assert(result)(
                    isSome(equalTo(new String(bytes.toArray[Byte], "UTF-8")))
                  )
                )
            }
          },
          testM("transducing with regular strings") {
            checkM(Gen.anyString.map(s => Chunk.fromArray(s.getBytes("UTF-8")).map(Chunk.single(_)))) { byteChunks =>
              ZStream
                .fromChunks(byteChunks.toList: _*)
                .transduce(ZSink.utf8Decode.map(_.getOrElse("")))
                .run(ZSink.mkString)
                .map { result =>
                  assert(byteChunks.flatten)(equalTo(Chunk.fromArray(result.getBytes("UTF-8"))))
                }
            }
          },
          testM("empty byte chunk results with None") {
            val bytes = Chunk()
            ZStream
              .fromChunk(bytes)
              .run(ZSink.utf8Decode)
              .map(assert(_)(isNone))
          },
          testM("incomplete chunk 1") {
            val bom  = Chunk.fromArray(Array[Byte](-17, -69, -65))
            val data = Array(0xc2.toByte, 0xa2.toByte)

            ZStream
              .fromChunks(bom, Chunk(data(0)), Chunk(data(1)))
              .run(ZSink.utf8Decode.exposeLeftover)
              .map { case (string, bytes) =>
                assert(string)(isSome(equalTo(new String(data, "UTF-8")))) &&
                  assert(bytes)(isEmpty)
              }
          },
          testM("incomplete chunk 2") {
            val bom  = Chunk.fromArray(Array[Byte](-17, -69, -65))
            val data = Array(0xe0.toByte, 0xa4.toByte, 0xb9.toByte)

            ZStream
              .fromChunks(bom, Chunk(data(0), data(1)), Chunk(data(2)))
              .run(ZSink.utf8Decode.exposeLeftover)
              .map { case (string, bytes) =>
                assert(string)(isSome(equalTo(new String(data, "UTF-8")))) &&
                  assert(bytes)(isEmpty)
              }
          },
          testM("incomplete chunk 3") {
            val bom  = Chunk.fromArray(Array[Byte](-17, -69, -65))
            val data = Array(0xf0.toByte, 0x90.toByte, 0x8d.toByte, 0x88.toByte)

            ZStream
              .fromChunks(bom, Chunk(data(0), data(1), data(2)), Chunk(data(3)))
              .run(ZSink.utf8Decode.exposeLeftover)
              .map { case (string, bytes) =>
                assert(string)(isSome(equalTo(new String(data, "UTF-8")))) &&
                  assert(bytes)(isEmpty)
              }
          },
          testM("chunk with leftover") {
            ZStream
              .fromChunk(Chunk(0xf0.toByte, 0x90.toByte, 0x8d.toByte, 0x88.toByte, 0xf0.toByte, 0x90.toByte))
              .run(ZSink.utf8Decode)
              .map { result =>
                assert(result.map(s => Chunk.fromArray(s.getBytes)))(
                  isSome(equalTo(Chunk.fromArray(Array(0xf0.toByte, 0x90.toByte, 0x8d.toByte, 0x88.toByte))))
                )
              }
          },
          testM("handle byte order mark") {
            checkM(Gen.anyString) { s =>
              ZStream
                .fromChunk(Chunk[Byte](-17, -69, -65) ++ Chunk.fromArray(s.getBytes("UTF-8")))
                .transduce(ZSink.utf8Decode)
                .runCollect
                .map { result =>
                  assert(result.collect { case Some(s) => s }.mkString)(equalTo(s))
                }
            }
          }
        )
      )
    )
  }
}

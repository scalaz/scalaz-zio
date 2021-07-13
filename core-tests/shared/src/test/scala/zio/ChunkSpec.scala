package zio

import zio.random.Random
import zio.test.Assertion._
import zio.test.TestAspect.exceptScala211
import zio.test._

object ChunkSpec extends ZIOBaseSpec {

  case class Value(i: Int) extends AnyVal

  import ZIOTag._

  val intGen: Gen[Random, Int] = Gen.int(-10, 10)

  def toBoolFn[R <: Random, A]: Gen[R, A => Boolean] =
    Gen.function(Gen.boolean)

  def tinyChunks[R <: Random, A](a: Gen[R, A]): Gen[R, Chunk[A]] =
    Gen.chunkOfBounded(0, 3)(a)

  def smallChunks[R <: Random, A](a: Gen[R, A]): Gen[R with Sized, Chunk[A]] =
    Gen.small(Gen.chunkOfN(_)(a))

  def mediumChunks[R <: Random, A](a: Gen[R, A]): Gen[R with Sized, Chunk[A]] =
    Gen.medium(Gen.chunkOfN(_)(a))

  def largeChunks[R <: Random, A](a: Gen[R, A]): Gen[R with Sized, Chunk[A]] =
    Gen.large(Gen.chunkOfN(_)(a))

  def chunkWithIndex[R <: Random, A](a: Gen[R, A]): Gen[R with Sized, (Chunk[A], Int)] =
    for {
      chunk <- Gen.chunkOfBounded(1, 100)(a)
      idx   <- Gen.int(0, chunk.length - 1)
    } yield (chunk, idx)

  def spec: ZSpec[Environment, Failure] = suite("ChunkSpec")(
    suite("size/length")(
      test("concatenated size must match length") {
        val chunk = Chunk.empty ++ Chunk.fromArray(Array(1, 2)) ++ Chunk(3, 4, 5) ++ Chunk.single(6)
        assert(chunk.size)(equalTo(chunk.length))
      },
      test("empty size must match length") {
        val chunk = Chunk.empty
        assert(chunk.size)(equalTo(chunk.length))
      },
      test("fromArray size must match length") {
        val chunk = Chunk.fromArray(Array(1, 2, 3))
        assert(chunk.size)(equalTo(chunk.length))
      },
      test("fromIterable size must match length") {
        val chunk = Chunk.fromIterable(List("1", "2", "3"))
        assert(chunk.size)(equalTo(chunk.length))
      },
      test("single size must match length") {
        val chunk = Chunk.single(true)
        assert(chunk.size)(equalTo(chunk.length))
      }
    ),
    suite("append")(
      testM("apply") {
        val chunksWithIndex: Gen[Random with Sized, (Chunk[Int], Chunk[Int], Int)] =
          for {
            p  <- Gen.boolean
            as <- Gen.chunkOf(Gen.anyInt)
            bs <- Gen.chunkOf1(Gen.anyInt)
            n  <- Gen.int(0, as.length + bs.length - 1)
          } yield if (p) (as, bs, n) else (bs, as, n)
        check(chunksWithIndex) { case (as, bs, n) =>
          val actual   = bs.foldLeft(as)(_ :+ _).apply(n)
          val expected = (as ++ bs).apply(n)
          assert(actual)(equalTo(expected))
        }
      },
      testM("buffer full") {
        check(Gen.chunkOf(Gen.anyInt), Gen.chunkOf(Gen.anyInt)) { (as, bs) =>
          def addAll[A](l: Chunk[A], r: Chunk[A]): Chunk[A] = r.foldLeft(l)(_ :+ _)
          val actual                                        = List.fill(100)(bs).foldLeft(as)(addAll)
          val expected                                      = List.fill(100)(bs).foldLeft(as)(_ ++ _)
          assert(actual)(equalTo(expected))
        }
      },
      testM("buffer used") {
        checkM(Gen.chunkOf(Gen.anyInt), Gen.chunkOf(Gen.anyInt)) { (as, bs) =>
          val effect   = ZIO.succeed(bs.foldLeft(as)(_ :+ _))
          val actual   = ZIO.collectAllPar(ZIO.replicate(100)(effect))
          val expected = as ++ bs
          assertM(actual)(forall(equalTo(expected)))
        }
      },
      testM("equals") {
        check(Gen.chunkOf(Gen.anyInt), Gen.chunkOf(Gen.anyInt)) { (as, bs) =>
          val actual   = bs.foldLeft(as)(_ :+ _)
          val expected = as ++ bs
          assert(actual)(equalTo(expected))
        }
      },
      testM("length") {
        check(Gen.chunkOf(Gen.anyInt), smallChunks(Gen.anyInt)) { (as, bs) =>
          val actual   = bs.foldLeft(as)(_ :+ _).length
          val expected = (as ++ bs).length
          assert(actual)(equalTo(expected))
        }
      },
      test("returns most specific type") {
        val seq = (zio.Chunk("foo"): Seq[String]) :+ "post1"
        assert(seq)(isSubtype[Chunk[String]](equalTo(Chunk("foo", "post1"))))
      }
    ),
    suite("prepend")(
      testM("apply") {
        val chunksWithIndex: Gen[Random with Sized, (Chunk[Int], Chunk[Int], Int)] =
          for {
            p  <- Gen.boolean
            as <- Gen.chunkOf(Gen.anyInt)
            bs <- Gen.chunkOf1(Gen.anyInt)
            n  <- Gen.int(0, as.length + bs.length - 1)
          } yield if (p) (as, bs, n) else (bs, as, n)
        check(chunksWithIndex) { case (as, bs, n) =>
          val actual   = as.foldRight(bs)(_ +: _).apply(n)
          val expected = (as ++ bs).apply(n)
          assert(actual)(equalTo(expected))
        }
      },
      testM("buffer full") {
        check(Gen.chunkOf(Gen.anyInt), Gen.chunkOf(Gen.anyInt)) { (as, bs) =>
          def addAll[A](l: Chunk[A], r: Chunk[A]): Chunk[A] = l.foldRight(r)(_ +: _)
          val actual                                        = List.fill(100)(as).foldRight(bs)(addAll)
          val expected                                      = List.fill(100)(as).foldRight(bs)(_ ++ _)
          assert(actual)(equalTo(expected))
        }
      },
      testM("buffer used") {
        checkM(Gen.chunkOf(Gen.anyInt), Gen.chunkOf(Gen.anyInt)) { (as, bs) =>
          val effect   = ZIO.succeed(as.foldRight(bs)(_ +: _))
          val actual   = ZIO.collectAllPar(ZIO.replicate(100)(effect))
          val expected = as ++ bs
          assertM(actual)(forall(equalTo(expected)))
        }
      },
      testM("equals") {
        check(Gen.chunkOf(Gen.anyInt), Gen.chunkOf(Gen.anyInt)) { (as, bs) =>
          val actual   = as.foldRight(bs)(_ +: _)
          val expected = as ++ bs
          assert(actual)(equalTo(expected))
        }
      },
      testM("length") {
        check(Gen.chunkOf(Gen.anyInt), smallChunks(Gen.anyInt)) { (as, bs) =>
          val actual   = as.foldRight(bs)(_ +: _).length
          val expected = (as ++ bs).length
          assert(actual)(equalTo(expected))
        }
      },
      test("returns most specific type") {
        val seq = "pre1" +: (zio.Chunk("foo"): Seq[String])
        assert(seq)(isSubtype[Chunk[String]](equalTo(Chunk("pre1", "foo"))))
      }
    ),
    testM("apply") {
      check(chunkWithIndex(Gen.unit)) { case (chunk, i) =>
        assert(chunk.apply(i))(equalTo(chunk.toList.apply(i)))
      }
    },
    suite("specialized accessors")(
      testM("boolean") {
        check(chunkWithIndex(Gen.boolean)) { case (chunk, i) =>
          assert(chunk.boolean(i))(equalTo(chunk.toList.apply(i)))
        }
      },
      testM("byte") {
        check(chunkWithIndex(Gen.byte(0, 127))) { case (chunk, i) =>
          assert(chunk.byte(i))(equalTo(chunk.toList.apply(i)))
        }
      },
      testM("char") {
        check(chunkWithIndex(Gen.char(33, 123))) { case (chunk, i) =>
          assert(chunk.char(i))(equalTo(chunk.toList.apply(i)))
        }
      },
      testM("short") {
        check(chunkWithIndex(Gen.short(5, 100))) { case (chunk, i) =>
          assert(chunk.short(i))(equalTo(chunk.toList.apply(i)))
        }
      },
      testM("int") {
        check(chunkWithIndex(Gen.int(1, 142))) { case (chunk, i) =>
          assert(chunk.int(i))(equalTo(chunk.toList.apply(i)))
        }
      },
      testM("long") {
        check(chunkWithIndex(Gen.long(1, 142))) { case (chunk, i) =>
          assert(chunk.long(i))(equalTo(chunk.toList.apply(i)))
        }
      },
      testM("float") {
        check(chunkWithIndex(Gen.double(0.0, 100.0).map(_.toFloat))) { case (chunk, i) =>
          assert(chunk.float(i))(equalTo(chunk.toList.apply(i)))
        }
      },
      testM("double") {
        check(chunkWithIndex(Gen.double(1.0, 200.0))) { case (chunk, i) =>
          assert(chunk.double(i))(equalTo(chunk.toList.apply(i)))
        }
      }
    ),
    testM("corresponds") {
      val genChunk    = smallChunks(intGen)
      val genFunction = Gen.function[Random with Sized, (Int, Int), Boolean](Gen.boolean).map(Function.untupled(_))
      check(genChunk, genChunk, genFunction) { (as, bs, f) =>
        val actual   = as.corresponds(bs)(f)
        val expected = as.toList.corresponds(bs.toList)(f)
        assert(actual)(equalTo(expected))
      }
    },
    testM("fill") {
      val smallInt = Gen.int(-10, 10)
      check(smallInt, smallInt) { (n, elem) =>
        val actual   = Chunk.fill(n)(elem)
        val expected = Chunk.fromArray(Array.fill(n)(elem))
        assert(actual)(equalTo(expected))
      }
    },
    test("splitWhere") {
      assert(Chunk(1, 2, 3, 4).splitWhere(_ == 2))(equalTo((Chunk(1), Chunk(2, 3, 4))))
    },
    testM("length") {
      check(largeChunks(intGen))(chunk => assert(chunk.length)(equalTo(chunk.toList.length)))
    },
    testM("equality") {
      check(mediumChunks(intGen), mediumChunks(intGen)) { (c1, c2) =>
        assert(c1.equals(c2))(equalTo(c1.toList.equals(c2.toList)))
      }
    },
    test("inequality") {
      assert(Chunk(1, 2, 3, 4, 5))(Assertion.not(equalTo(Chunk(1, 2, 3, 4, 5, 6))))
    },
    testM("materialize") {
      check(mediumChunks(intGen))(c => assert(c.materialize.toList)(equalTo(c.toList)))
    },
    testM("foldLeft") {
      check(Gen.alphaNumericString, Gen.function2(Gen.alphaNumericString), smallChunks(Gen.alphaNumericString)) {
        (s0, f, c) => assert(c.fold(s0)(f))(equalTo(c.toArray.foldLeft(s0)(f)))
      }
    },
    test("foldRight") {
      val chunk    = Chunk("a") ++ Chunk("b") ++ Chunk("c")
      val actual   = chunk.foldRight("d")(_ + _)
      val expected = "abcd"
      assert(actual)(equalTo(expected))
    },
    test("mapAccum") {
      assert(Chunk(1, 1, 1).mapAccum(0)((s, el) => (s + el, s + el)))(equalTo((3, Chunk(1, 2, 3))))
    },
    suite("mapAccumM")(
      testM("mapAccumM happy path") {
        checkM(smallChunks(Gen.anyInt), smallChunks(Gen.anyInt), Gen.anyInt, Gen.function2(Gen.anyInt <*> Gen.anyInt)) {
          (left, right, s, f) =>
            val actual = (left ++ right).mapAccumM[Any, Nothing, Int, Int](s)((s, a) => UIO.succeed(f(s, a)))
            val expected = (left ++ right).foldLeft[(Int, Chunk[Int])]((s, Chunk.empty)) { case ((s0, bs), a) =>
              val (s1, b) = f(s0, a)
              (s1, bs :+ b)
            }
            assertM(actual)(equalTo(expected))
        }
      },
      testM("mapAccumM error") {
        Chunk(1, 1, 1).mapAccumM(0)((_, _) => IO.fail("Ouch")).either.map(assert(_)(isLeft(equalTo("Ouch"))))
      } @@ zioTag(errors)
    ),
    testM("map") {
      val fn = Gen.function[Random with Sized, Int, Int](intGen)
      check(smallChunks(intGen), fn)((c, f) => assert(c.map(f).toList)(equalTo(c.toList.map(f))))
    },
    suite("mapM")(
      testM("mapM happy path")(checkM(mediumChunks(intGen), Gen.function(Gen.boolean)) { (chunk, f) =>
        chunk.mapM(s => UIO.succeed(f(s))).map(assert(_)(equalTo(chunk.map(f))))
      }),
      testM("mapM error") {
        Chunk(1, 2, 3).mapM(_ => IO.fail("Ouch")).either.map(assert(_)(equalTo(Left("Ouch"))))
      } @@ zioTag(errors)
    ),
    testM("flatMap") {
      val fn = Gen.function[Random with Sized, Int, Chunk[Int]](smallChunks(intGen))
      check(smallChunks(intGen), fn) { (c, f) =>
        assert(c.flatMap(f).toList)(equalTo(c.toList.flatMap(f.andThen(_.toList))))
      }
    },
    testM("headOption") {
      check(mediumChunks(intGen))(c => assert(c.headOption)(equalTo(c.toList.headOption)))
    },
    testM("lastOption") {
      check(mediumChunks(intGen))(c => assert(c.lastOption)(equalTo(c.toList.lastOption)))
    },
    testM("indexWhere") {
      val fn = Gen.function[Random with Sized, Int, Boolean](Gen.boolean)
      check(smallChunks(intGen), smallChunks(intGen), fn, intGen) { (left, right, p, from) =>
        val actual   = (left ++ right).indexWhere(p, from)
        val expected = (left.toVector ++ right.toVector).indexWhere(p, from)
        assert(actual)(equalTo(expected))
      }
    } @@ exceptScala211,
    testM("exists") {
      val fn = Gen.function[Random with Sized, Int, Boolean](Gen.boolean)
      check(mediumChunks(intGen), fn)((chunk, p) => assert(chunk.exists(p))(equalTo(chunk.toList.exists(p))))
    },
    testM("indexOf") {
      val fn = Gen.function[Random with Sized, Int, Boolean](Gen.boolean)
      check(mediumChunks(intGen), fn)((chunk, p) => assert(chunk.indexOf(p))(equalTo(chunk.toList.indexOf(p))))
    },
    testM("forall") {
      val fn = Gen.function[Random with Sized, Int, Boolean](Gen.boolean)
      check(mediumChunks(intGen), fn)((chunk, p) => assert(chunk.forall(p))(equalTo(chunk.toList.forall(p))))
    },
    testM("find") {
      val fn = Gen.function[Random with Sized, Int, Boolean](Gen.boolean)
      check(mediumChunks(intGen), fn)((chunk, p) => assert(chunk.find(p))(equalTo(chunk.toList.find(p))))
    },
    testM("filter") {
      val fn = Gen.function[Random with Sized, Int, Boolean](Gen.boolean)
      check(mediumChunks(intGen), fn)((chunk, p) => assert(chunk.filter(p).toList)(equalTo(chunk.toList.filter(p))))
    },
    suite("filterM")(
      testM("filterM happy path")(checkM(mediumChunks(intGen), Gen.function(Gen.boolean)) { (chunk, p) =>
        chunk.filterM(s => UIO.succeed(p(s))).map(assert(_)(equalTo(chunk.filter(p))))
      }),
      testM("filterM error") {
        Chunk(1, 2, 3).filterM(_ => IO.fail("Ouch")).either.map(assert(_)(equalTo(Left("Ouch"))))
      } @@ zioTag(errors)
    ),
    testM("drop chunk") {
      check(largeChunks(intGen), intGen)((chunk, n) => assert(chunk.drop(n).toList)(equalTo(chunk.toList.drop(n))))
    },
    testM("take chunk") {
      check(chunkWithIndex(Gen.unit)) { case (c, n) =>
        assert(c.take(n).toList)(equalTo(c.toList.take(n)))
      }
    },
    testM("dropWhile chunk") {
      check(mediumChunks(intGen), toBoolFn[Random, Int]) { (c, p) =>
        assert(c.dropWhile(p).toList)(equalTo(c.toList.dropWhile(p)))
      }
    },
    suite("dropWhileM")(
      testM("dropWhileM happy path") {
        assertM(Chunk(1, 2, 3, 4, 5).dropWhileM(el => UIO.succeed(el % 2 == 1)))(equalTo(Chunk(2, 3, 4, 5)))
      },
      testM("dropWhileM error") {
        Chunk(1, 1, 1).dropWhileM(_ => IO.fail("Ouch")).either.map(assert(_)(isLeft(equalTo("Ouch"))))
      } @@ zioTag(errors)
    ),
    testM("takeWhile chunk") {
      check(mediumChunks(intGen), toBoolFn[Random, Int]) { (c, p) =>
        assert(c.takeWhile(p).toList)(equalTo(c.toList.takeWhile(p)))
      }
    },
    suite("takeWhileM")(
      testM("takeWhileM happy path") {
        assertM(Chunk(1, 2, 3, 4, 5).takeWhileM(el => UIO.succeed(el % 2 == 1)))(equalTo(Chunk(1)))
      },
      testM("takeWhileM error") {
        Chunk(1, 1, 1).dropWhileM(_ => IO.fail("Ouch")).either.map(assert(_)(isLeft(equalTo("Ouch"))))
      } @@ zioTag(errors)
    ),
    testM("toArray") {
      check(mediumChunks(Gen.alphaNumericString))(c => assert(c.toArray.toList)(equalTo(c.toList)))
    },
    test("non-homogeneous element type") {
      trait Animal
      trait Cat extends Animal
      trait Dog extends Animal

      val vector   = Vector(new Cat {}, new Dog {}, new Animal {})
      val actual   = Chunk.fromIterable(vector).map(identity)
      val expected = Chunk.fromArray(vector.toArray)

      assert(actual)(equalTo(expected))
    },
    test("toArray for an empty Chunk of type String") {
      assert(Chunk.empty.toArray[String])(equalTo(Array.empty[String]))
    },
    test("to Array for an empty Chunk using filter") {
      assert(Chunk(1).filter(_ == 2).map(_.toString).toArray[String])(equalTo(Array.empty[String]))
    },
    testM("toArray with elements of type String") {
      check(mediumChunks(Gen.alphaNumericString))(c => assert(c.toArray.toList)(equalTo(c.toList)))
    },
    test("toArray for a Chunk of any type") {
      val v: Vector[Any] = Vector("String", 1, Value(2))
      assert(Chunk.fromIterable(v).toArray.toVector)(equalTo(v))
    },
    suite("collect")(
      test("collect empty Chunk") {
        assert(Chunk.empty.collect { case _ => 1 })(isEmpty)
      },
      testM("collect chunk") {
        val pfGen = Gen.partialFunction[Random with Sized, Int, Int](intGen)
        check(mediumChunks(intGen), pfGen)((c, pf) => assert(c.collect(pf).toList)(equalTo(c.toList.collect(pf))))
      }
    ),
    suite("collectM")(
      testM("collectM empty Chunk") {
        assertM(Chunk.empty.collectM { case _ => UIO.succeed(1) })(equalTo(Chunk.empty))
      },
      testM("collectM chunk") {
        val pfGen = Gen.partialFunction[Random with Sized, Int, UIO[Int]](Gen.successes(intGen))
        checkM(mediumChunks(intGen), pfGen) { (c, pf) =>
          for {
            result   <- c.collectM(pf).map(_.toList)
            expected <- UIO.collectAll(c.toList.collect(pf))
          } yield assert(result)(equalTo(expected))
        }
      },
      testM("collectM chunk that fails") {
        Chunk(1, 2).collectM { case 2 => IO.fail("Ouch") }.either.map(assert(_)(isLeft(equalTo("Ouch"))))
      } @@ zioTag(errors)
    ),
    suite("collectWhile")(
      test("collectWhile empty Chunk") {
        assert(Chunk.empty.collectWhile { case _ => 1 })(isEmpty)
      },
      testM("collectWhile chunk") {
        val pfGen = Gen.partialFunction[Random with Sized, Int, Int](intGen)
        check(mediumChunks(intGen), pfGen) { (c, pf) =>
          assert(c.collectWhile(pf).toList)(equalTo(c.toList.takeWhile(pf.isDefinedAt).map(pf.apply)))
        }
      }
    ),
    suite("collectWhileM")(
      testM("collectWhileM empty Chunk") {
        assertM(Chunk.empty.collectWhileM { case _ => UIO.succeed(1) })(equalTo(Chunk.empty))
      },
      testM("collectWhileM chunk") {
        val pfGen = Gen.partialFunction[Random with Sized, Int, UIO[Int]](Gen.successes(intGen))
        checkM(mediumChunks(intGen), pfGen) { (c, pf) =>
          for {
            result   <- c.collectWhileM(pf).map(_.toList)
            expected <- UIO.foreach(c.toList.takeWhile(pf.isDefinedAt))(pf.apply)
          } yield assert(result)(equalTo(expected))
        }
      },
      testM("collectWhileM chunk that fails") {
        Chunk(1, 2).collectWhileM { case _ => IO.fail("Ouch") }.either.map(assert(_)(isLeft(equalTo("Ouch"))))
      } @@ zioTag(errors)
    ),
    testM("foreach") {
      check(mediumChunks(intGen)) { c =>
        var sum = 0
        c.foreach(sum += _)

        assert(sum)(equalTo(c.toList.sum))
      }
    },
    testM("concat chunk") {
      check(smallChunks(intGen), smallChunks(intGen)) { (c1, c2) =>
        assert((c1 ++ c2).toList)(equalTo(c1.toList ++ c2.toList))
      }
    },
    test("chunk transitivity") {
      val c1 = Chunk(1, 2, 3)
      val c2 = Chunk(1, 2, 3)
      val c3 = Chunk(1, 2, 3)
      assert(c1 == c2 && c2 == c3 && c1 == c3)(Assertion.isTrue)
    },
    test("chunk symmetry") {
      val c1 = Chunk(1, 2, 3)
      val c2 = Chunk(1, 2, 3)
      assert(c1 == c2 && c2 == c1)(Assertion.isTrue)
    },
    test("chunk reflexivity") {
      val c1 = Chunk(1, 2, 3)
      assert(c1 == c1)(Assertion.isTrue)
    },
    test("chunk negation") {
      val c1 = Chunk(1, 2, 3)
      val c2 = Chunk(1, 2, 3)
      assert(c1 != c2 == !(c1 == c2))(Assertion.isTrue)
    },
    test("chunk substitutivity") {
      val c1 = Chunk(1, 2, 3)
      val c2 = Chunk(1, 2, 3)
      assert(c1 == c2 && c1.toString == c2.toString)(Assertion.isTrue)
    },
    test("chunk consistency") {
      val c1 = Chunk(1, 2, 3)
      val c2 = Chunk(1, 2, 3)
      assert(c1 == c2 && c1.hashCode == c2.hashCode)(Assertion.isTrue)
    },
    test("nullArrayBug") {
      val c = Chunk.fromArray(Array(1, 2, 3, 4, 5))

      // foreach should not throw
      c.foreach(_ => ())

      assert(c.filter(_ => false).map(_ * 2).length)(equalTo(0))
    },
    test("toArrayOnConcatOfSlice") {
      val onlyOdd: Int => Boolean = _ % 2 != 0
      val concat = Chunk(1, 1, 1).filter(onlyOdd) ++
        Chunk(2, 2, 2).filter(onlyOdd) ++
        Chunk(3, 3, 3).filter(onlyOdd)

      val array = concat.toArray

      assert(array)(equalTo(Array(1, 1, 1, 3, 3, 3)))
    },
    test("toArrayOnConcatOfEmptyAndInts") {
      assert(Chunk.empty ++ Chunk.fromArray(Array(1, 2, 3)))(equalTo(Chunk(1, 2, 3)))
    },
    test("filterConstFalseResultsInEmptyChunk") {
      assert(Chunk.fromArray(Array(1, 2, 3)).filter(_ => false))(equalTo(Chunk.empty))
    },
    testM("zip") {
      check(Gen.chunkOf(Gen.anyInt), Gen.chunkOf(Gen.anyInt)) { (as, bs) =>
        val actual   = as.zip(bs).toList
        val expected = as.toList.zip(bs.toList)
        assert(actual)(equalTo(expected))
      }
    },
    test("zipAll") {
      val a = Chunk(1, 2, 3)
      val b = Chunk(1, 2)
      val c = Chunk((Some(1), Some(1)), (Some(2), Some(2)), (Some(3), Some(3)))
      val d = Chunk((Some(1), Some(1)), (Some(2), Some(2)), (Some(3), None))
      val e = Chunk((Some(1), Some(1)), (Some(2), Some(2)), (None, Some(3)))
      assert(a.zipAll(a))(equalTo(c)) &&
      assert(a.zipAll(b))(equalTo(d)) &&
      assert(b.zipAll(a))(equalTo(e))
    },
    test("zipAllWith") {
      assert(Chunk(1, 2, 3).zipAllWith(Chunk(3, 2, 1))(_ => 0, _ => 0)(_ + _))(equalTo(Chunk(4, 4, 4))) &&
      assert(Chunk(1, 2, 3).zipAllWith(Chunk(3, 2))(_ => 0, _ => 0)(_ + _))(equalTo(Chunk(4, 4, 0))) &&
      assert(Chunk(1, 2).zipAllWith(Chunk(3, 2, 1))(_ => 0, _ => 0)(_ + _))(equalTo(Chunk(4, 4, 0)))
    },
    test("zipWithIndex") {
      val (ch1, ch2) = Chunk("a", "b", "c", "d").splitAt(2)
      val ch         = ch1 ++ ch2
      assert(ch.zipWithIndex.toList)(equalTo(ch.toList.zipWithIndex))
    },
    testM("zipWithIndex on concatenated chunks") {
      check(smallChunks(intGen), smallChunks(intGen)) { (c1, c2) =>
        val items   = (c1 ++ c2).zipWithIndex.map(_._1)
        val indices = (c1 ++ c2).zipWithIndex.map(_._2)

        assert(items.toList)(equalTo(c1.toList ++ c2.toList)) &&
        assert(indices.toList)(equalTo((0 until (c1.size + c2.size)).toList))
      }
    },
    testM("zipWithIndexFrom on concatenated chunks") {
      check(smallChunks(intGen), smallChunks(intGen), Gen.int(0, 10)) { (c1, c2, from) =>
        val items   = (c1 ++ c2).zipWithIndexFrom(from).map(_._1)
        val indices = (c1 ++ c2).zipWithIndexFrom(from).map(_._2)

        assert(items.toList)(equalTo(c1.toList ++ c2.toList)) &&
        assert(indices.toList)(equalTo((from until (c1.size + c2.size + from)).toList))
      }
    },
    test("partitionMap") {
      val as       = Chunk(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)
      val (bs, cs) = as.partitionMap(n => if (n % 2 == 0) Left(n) else Right(n))
      assert(bs)(equalTo(Chunk(0, 2, 4, 6, 8))) &&
      assert(cs)(equalTo(Chunk(1, 3, 5, 7, 9)))
    },
    test("stack safety concat") {
      val n  = 1000000
      val as = List.range(0, n).foldRight[Chunk[Int]](Chunk.empty)((a, as) => Chunk(a) ++ as)
      assert(as.toArray)(equalTo(Array.range(0, n)))
    },
    test("stack safety append") {
      val n  = 1000000
      val as = List.range(0, n).foldRight[Chunk[Int]](Chunk.empty)((a, as) => as :+ a)
      assert(as.toArray)(equalTo(Array.range(0, n).reverse))
    },
    test("stack safety prepend") {
      val n  = 1000000
      val as = List.range(0, n).foldRight[Chunk[Int]](Chunk.empty)((a, as) => a +: as)
      assert(as.toArray)(equalTo(Array.range(0, n)))
    },
    test("stack safety concat and append") {
      val n = 100000
      val as = List.range(0, n).foldRight[Chunk[Int]](Chunk.empty) { (a, as) =>
        if (a % 2 == 0) as :+ a else as ++ Chunk(a)
      }
      assert(as.toArray)(equalTo(Array.range(0, n).reverse))
    },
    test("stack safety concat and prepend") {
      val n = 100000
      val as = List.range(0, n).foldRight[Chunk[Int]](Chunk.empty) { (a, as) =>
        if (a % 2 == 0) a +: as else Chunk(a) ++ as
      }
      assert(as.toArray)(equalTo(Array.range(0, n)))
    },
    test("toArray does not throw ClassCastException") {
      val chunk = Chunk("a")
      val array = chunk.toArray
      assert(array)(anything)
    },
    testM("foldWhileM") {
      assertM(
        Chunk
          .fromIterable(List(2))
          .foldWhileM(0)(i => i <= 2) { case (i: Int, i1: Int) =>
            if (i < 2) IO.succeed(i + i1)
            else IO.succeed(i)
          }
      )(equalTo(2))
    },
    test("Tags.fromValue is safe on Scala.is") {
      val _ = Chunk(1, 128)
      assertCompletes
    },
    testM("chunks can be constructed from heterogeneous collections") {
      check(Gen.listOf(Gen.oneOf(Gen.anyInt, Gen.anyString, Gen.none))) { as =>
        assert(Chunk.fromIterable(as).toList)(equalTo(as))
      }
    },
    test("unfold") {
      assert(
        Chunk.unfold(0)(n => if (n < 10) Some((n, n + 1)) else None)
      )(equalTo(Chunk.fromIterable(0 to 9)))
    },
    testM("unfoldM") {
      assertM(
        Chunk.unfoldM(0)(n => if (n < 10) IO.some((n, n + 1)) else IO.none)
      )(equalTo(Chunk.fromIterable(0 to 9)))
    },
    testM("split") {
      val smallInts = Gen.small(n => Gen.const(n), 1)
      val chunks    = Gen.chunkOf(Gen.anyInt)
      check(smallInts, chunks) { (n, chunk) =>
        val groups = chunk.split(n)
        assert(groups.flatten)(equalTo(chunk)) &&
        assert(groups.size)(equalTo(n min chunk.length))
      }
    },
    testM("fromIterator") {
      check(Gen.chunkOf(Gen.anyInt)) { as =>
        assert(Chunk.fromIterator(as.iterator))(equalTo(as))
      }
    },
    suite("unapplySeq")(
      test("matches a nonempty chunk") {
        val chunk = Chunk(1, 2, 3)
        val actual = chunk match {
          case Chunk(x, y, z) => Some((x, y, z))
          case _              => None
        }
        val expected = Some((1, 2, 3))
        assert(actual)(equalTo(expected))
      },
      test("matches an empty chunk") {
        val chunk = Chunk.empty
        val actual = chunk match {
          case Chunk() => Some(())
          case _       => None
        }
        val expected = Some(())
        assert(actual)(equalTo(expected))
      }
    )
  )
}

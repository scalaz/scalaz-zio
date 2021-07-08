package zio.test

import zio.duration.durationInt
import zio.test.SmartTestTypes._
import zio.test.environment.TestClock

import java.time.LocalDateTime
import scala.collection.immutable.SortedSet

object SmartAssertionSpec extends ZIOBaseSpec {

  // Switch TestAspect.failing to TestAspect.identity to easily preview
  // the error messages.
  val failing: TestAspectPoly = TestAspect.failing

  val company: Company = Company("Ziverge", List(User("Bobo", List.tabulate(2)(n => Post(s"Post #$n")))))

  def spec: ZSpec[Environment, Failure] = suite("SmartAssertionSpec")(
    test("Head") {
      val array = Array(1, 8, 2, 3, 888)
      assertTrue(
        !(array(0) == 1),
        array(3) == 10,
        array(1) < 2
      )
    } @@ failing,
    test("missing element") {
      assertTrue(company.users(8).posts.exists(_.title == "hi"))
    } @@ failing,
    test("fails predicate") {
      assertTrue(company.users.head.posts.exists(_.title == "hii"))
    } @@ failing,
    test("nested access") {
      val company = Company("Cool Company", List.empty)
      assertTrue(company.users.head.posts.exists(_.title == "hii"))
    } @@ failing,
    test("boolean method") {
      assertTrue(company.users.head.posts.head.publishDate.isDefined)
    } @@ failing,
    test("boolean method with args") {
      assertTrue(company.users.head.posts.head.publishDate.contains(LocalDateTime.MAX))
    } @@ failing,
    test("forall") {
      val list = List(10, 5, 8, 3, 4)
      assertTrue(list.forall(_ % 2 == 0))
    } @@ failing,
    test("right.get") {
      val myEither: Either[String, Int] = Left("string")
      case class Cool(int: Int)
      assertTrue(myEither.right.get + 1 > 18)
    } @@ failing,
    test("string contains") {
      val myString = "something"
      assertTrue(myString.contains("aoseunoth") && myString == "coool")
    } @@ failing,
    suite("referencing literals")(
      test("List") {
        val list = List(10, 23, 8, 8)
        assertTrue((list intersect List(23)).head + 31 == 3)
      },
      test("Case Class") {
        assertTrue(Company("Nice", List.empty).name.contains("aoeu"))
      },
      test("Array") {
        val array = Array(1, 2, 3, 9, 8)
        assertTrue(array.head == 3)
      },
      test("Object constructor") {
        assertTrue(zio.duration.Duration.fromNanos(1000) == zio.duration.Duration.Zero)
      }
    ) @@ failing,
    suite("contains")(
      test("Option") {
        assertTrue(company.users.head.posts.head.publishDate.contains(LocalDateTime.MAX))
      }
    ) @@ failing,
    suite("Either")(
      test("right.get") {
        val myEither: Either[String, Int] = Left("string")
        assertTrue(myEither.right.get + 1 > 11233)
      }
    ) @@ failing,
    suite("Exceptions")(
      test("throws") {
        case class Bomb(name: String) {
          def getName: String = throw new Error("SPLODE")
        }
        val bomb = Bomb("boomy")
        assertTrue(bomb.getName.contains("HIII"))
      }
    ) @@ failing,
    test(".get") {
      case class Person(name: String, age: Int)
      val result = Some(Person("Kit", 30))
      assertTrue(result.get.name == "Kitttty")
    } @@ failing,
    test("calling a method with args") {
      case class Person(name: String = "Fred", age: Int = 42) {
        def say(words: String*): String = words.mkString(" ")
      }
      val person = Person()
      assertTrue {
        person.say("ping", "pong") == "pong pong!"
      }
    } @@ failing,
    test("calling a method with args") {
      case class Person(name: String = "Fred", age: Int = 42) {
        def say(words: String*): String = words.mkString(" ")
      }
      assertTrue {
        Person().say("ping", "pong") == "pong pong!"
      }
    } @@ failing,
    test("calling a method with args") {
      case class Person(name: String = "Fred", age: Int = 41) {
        def say(words: String*): String = words.mkString(" ")
      }

      val person = Person()

      assertTrue(
        person.say("ping", "pong") != "ping pong",
        !(person.say("ping", "pong") == "ping pong")
      )
    } @@ failing,
    test("contains") {
      val list = Some(List(1, 8, 132, 83))
      assertTrue(list.get.contains(78))
    } @@ failing,
    testM("sleep delays effect until time is adjusted") {
      for {
        ref    <- zio.Ref.make(false)
        _      <- ref.set(true).delay(10.hours).fork
        _      <- TestClock.adjust(9.hours)
        result <- ref.get
      } yield assertTrue(!result)
    },
    test("contains must succeed when iterable contains specified element") {
      assertTrue(Seq("zio1", "scala").contains("scala"))
    },
    test("contains array") {
      assertTrue(Array(1, 2, 3, 4, 8, 9, 1).toList.contains(10))
    } @@ failing,
    test("contains iterable") {
      assertTrue(Seq(1, 2, 3, 4, 8, 10, 1, 1).contains(9))
    } @@ failing,
    test("contains string") {
      assertTrue("Howdy".contains("no"))
    } @@ failing,
    test("endsWith iterable") {
      assertTrue(Seq(1, 2, 3, 4, 8, 10, 1, 1).endsWith(List(9, 10)))
    } @@ failing,
    test("endsWith string") {
      assertTrue("Howdy".endsWith("no"))
    } @@ failing,
    test("duration equality") {
      assertTrue(zio.duration.Duration.fromNanos(1000) == zio.duration.Duration.Zero)
    } @@ failing,
    test("string contains") {
      assertTrue("FUNNY HOUSE".contains("OH NO"))
    } @@ failing,
    test("contains must fail when iterable does not contain specified element") {
      assertTrue(Seq("zio", "scala").contains("java"))
    } @@ failing,
    test("containsString must succeed when string is found") {
      assertTrue("this is a value".contains("a value"))
    },
    test("containsString must return false when the string is not contained") {
      assertTrue("this is a value".contains("_NOTHING_"))
    } @@ failing,
    test("endWith must succeed when the supplied value ends with the specified sequence") {
      assertTrue(List(1, 2, 3, 4, 3).endsWith(List(3, 4, 3)))
    },
    test("startsWith must fail when the supplied value does not end with the specified sequence") {
      assertTrue(List(1, 2, 3, 4, 5).endsWith(List(1, 2, 3)))
    } @@ failing,
    test("endsWithString must succeed when the supplied value ends with the specified string") {
      assertTrue("zio".endsWith("o"))
    },
    test("endsWithString must fail when the supplied value does not end with the specified string") {
      assertTrue("zio".endsWith("z"))
    } @@ failing,
    test("equalTo must succeed when value equals specified value") {
      assertTrue(42 == 42)
    },
    test("equalTo must fail when value does not equal specified value") {
      assertTrue(0 == 42)
    } @@ failing,
    test("equalTo must succeed when array equals specified array") {
      assertTrue(Array(1, 2, 3).sameElements(Array(1, 2, 3)))
    },
    test("equalTo must not have type inference issues") {
      val list: List[Int] = List(1, 2, 3, 4)
      assertTrue(list.filter(_ => false) == List.empty[Int])
    },
    test("exists must succeed when at least one element of iterable satisfy specified assertion") {
      assertTrue(Seq(1, 42, 5).exists(_ == 42))
    },
    test("exists must fail when all elements of iterable do not satisfy specified assertion") {
      val value = Seq(1, 42, 5)
      assertTrue(value.exists(_ == 423))
    } @@ TestAspect.tag("IMPORTANT") @@ failing,
    test("forall must succeed when all elements of iterable satisfy specified assertion") {
      assertTrue(Seq("a", "bb", "ccc").forall(l => l.nonEmpty && l.length <= 3))
    },
    test("forall must fail when one element of iterable do not satisfy specified assertion") {
      assertTrue(Seq("a", "bb", "ccccc").forall(l => l.nonEmpty && l.length <= 3))
    } @@ failing,
    test("forall must succeed when an iterable is empty") {
      val emptySeq = Seq.empty[String]
      assertTrue(emptySeq.forall(l => l.nonEmpty && l.length <= 3))
    },
    test("forall must work with iterables that are not lists") {
      assertTrue(SortedSet(1, 2, 3).forall(_ > 0))
    },
    test("hasAt must fail when an index is outside of a sequence range") {
      assertTrue(Seq(1, 2, 3)(2) == 5)
    } @@ failing,
    test("has at contains") {
      assertTrue(Seq(List(5), List(1, 2, 3), List(1, 2, 3))(2).contains(12))
    } @@ failing,
    test("head") {
      assertTrue(Seq(1, 2, 3, 19).head == 1)
    },
    test("hasAt must succeed when a value is equal to a specific assertion") {
      assertTrue(!(Seq(1, 2, 3)(1) == 2))
    } @@ failing,
    test("hasFirst must succeed when a head is equal to a specific assertion") {
      assertTrue(Seq(1, 2, 3).head == 1)
    },
    test("hasFirst must fail when a head is not equal to a specific assertion") {
      assertTrue(!(Seq(1, 2, 3).head == 1))
    } @@ failing,
    test("hasIntersection must succeed when intersection satisfies specified assertion") {
      val seq = Seq(1, 2, 3, 4, 5)
      assertTrue((seq intersect Seq(4, 5, 6, 7, 8)).length == 105)
    } @@ TestAspect.tag("IMPORTANT") @@ failing,
    test("hasIntersection must succeed when intersection satisfies specified assertion") {
      val seq = Seq(1, 2, 3, 4, 5)

      assertTrue(seq.intersect(Seq(4, 5, 6, 7, 8)).length == 108)
    } @@ TestAspect.tag("IMPORTANT") @@ failing,
    test("hasIntersection must succeed when empty intersection satisfies specified assertion") {
      assertTrue((Seq(1, 2, 3, 4) intersect Seq(5, 6, 7)).isEmpty)
    },
    test("Basic equality") {
      val result = 1
      assertTrue {
        def cool(int: Int) = int * 3

        cool(result) > 400
      }
    } @@ failing,
    test("nested access") {
      case class Ziverge(people: Seq[Person]) {
        def isValid = true
      }
      case class Pet(name: String = "Spike")
      case class Person(name: String, age: Int, pet: Pet)
      val person  = Person("Vigoo", 23, Pet())
      val company = Ziverge(Seq(person))

      val string = "hello"
      assertTrue(
        !company.isValid,
        !(string == "hello"),
        person.age == 2,
        person.age > 10700
      )
    } @@ failing,
    test("hasAt must fail when an index is outside of a sequence range") {
      assertTrue(!(Seq(1, 2, 3)(2) == 3))
    } @@ failing,
    testM("check") {
      check(Gen.anyInt) { int =>
        assertTrue(int < 800)
      }
    } @@ failing,
    suite("Diffing")(
      test("No implicit Diff") {
        val int = 100
        assertTrue(int == 200)
      } @@ failing,
      test("With implicit Diff") {
        val string = "Sunday Everyday"
        assertTrue(string == "Saturday Todays")
      } @@ failing
    ),
    test("Package qualified identifiers") {
      assertTrue(zio.duration.Duration.fromNanos(0) == zio.duration.Duration.Zero)
    }
  )
}

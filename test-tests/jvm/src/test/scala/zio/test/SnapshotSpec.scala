package zio.test

import zio.{Task, UIO, ZIO}
import zio.test.Assertion.equalTo
import zio.test.AssertionM.Render.param
import zio.test.PlatformSpecific
import zio.test.TestAspect.{ignore, tag}

object SnapshotSpec extends ZIOBaseSpec {

  val SNAPSHOT_TEST_FILE = "SNAPSHOT_TEST_FILE"

  /**
   * Makes a new assertion that requires a value equal the specified value.
   */
//  final def equalToSnapshot(expected: String): AssertionM[String] = {
//    new AssertionM[String] {
//      override def render: AssertionM.Render = ???
//
//      override def runM: String => AssertResultM = ???
//    }
//  }
    final def equalToSnapshot[A, B](expected: A)(implicit eql: Eql[A, B]): Assertion[B] =
    Assertion.assertion("equalToSnapshot")() { actual =>
      (actual, expected) match {
        case (left: Array[_], right: Array[_]) => left.sameElements[Any](right)
        case (left, right)                     => left == right
      }
    }


  def snapshotTest[Any, E >: Throwable](label: String)(result: String): ZSpec[Any, E] =
    snapshotTestM(label)(ZIO.succeed(result))


  def noSnapFileName(snapFileName: String): TestResult = {
    val noSnapFileAssertion = Assertion.assertion[String](s"No snapshot $snapFileName file found")()(_ => false)
    BoolAlgebra.failure(FailureDetails(::(AssertionValue(
      noSnapFileAssertion,
      "",
      noSnapFileAssertion.run(""),
    ), Nil)))
  }


  //FIXME move to platform specific package and use also resource reading from there
  def snapshotTestM[R, E >: Throwable](label: String)(result: ZIO[R, E, String]): ZSpec[R, E] =
    testM(label)(
      for {
        actual <- result
        snapFileName <- ZIO.succeed(s"__snapshots__/$label")
        res <- if(getClass.getResource(snapFileName) eq null)
          ZIO.succeed(noSnapFileName(snapFileName))
        else
          ZIO.bracket[R, E, scala.io.Source](
            Task(scala.io.Source.fromInputStream(getClass.getResourceAsStream(snapFileName))))(
            (s: scala.io.Source) => UIO(s.close()))(
            (s: scala.io.Source) => UIO(s.getLines.mkString("\n"))
          ).map((snapshot: String) => CompileVariants.assertProxy(actual, snapshot, label)(equalToSnapshot(snapshot)))
      } yield res
    ) @@ tag(SNAPSHOT_TEST_FILE)

  override def spec: ZSpec[Any, Any] =
    suite("matchSnapshot suite")(
      snapshotTest("firsta")("hellos2"),
      snapshotTestM("second")(Task("HEY")),
      snapshotTestM("no-snap-file")(Task("HEY")),
      //FIXME fails with
      //[info]   - failure
      //[info]      did not satisfy No snapshot __snapshots__/failure file found()
      // But should fail that effect failed
      // firstly evaluate test effect then compare it with result
      snapshotTestM("failure")(ZIO.fail(new Exception("asdf"))),
      test("not a snapshot")(
        assert(1)(equalTo(1))
      )
    )
}

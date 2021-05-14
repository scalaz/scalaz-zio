package zio.test

import zio.test.Assertion.equalTo
import zio.test.ReportingTestUtils._
import zio.test.TestAspect.silent
import zio.test.environment.{TestClock, TestConsole, TestEnvironment, testEnvironment}
import zio.test.render.IntelliJRenderer
import zio.{Clock, Has, Layer, ZIO}

object IntellijRendererSpec extends ZIOBaseSpec {
  import IntelliJRenderUtils._

  def spec: ZSpec[Environment, Failure] =
    suite("IntelliJ Renderer")(
      testM("correctly reports a successful test") {
        assertM(runLog(test1))(equalTo(test1Expected.mkString))
      },
      testM("correctly reports a failed test") {
        assertM(runLog(test3))(equalTo(test3Expected.mkString))
      },
      testM("correctly reports an error in a test") {
        assertM(runLog(test4))(equalTo(test4Expected.mkString))
      },
      testM("correctly reports successful test suite") {
        assertM(runLog(suite1))(equalTo(suite1Expected.mkString))
      },
      testM("correctly reports failed test suite") {
        assertM(runLog(suite2))(equalTo(suite2Expected.mkString))
      },
      testM("correctly reports multiple test suites") {
        assertM(runLog(suite3))(equalTo(suite3Expected.mkString))
      },
      testM("correctly reports empty test suite") {
        assertM(runLog(suite4))(equalTo(suite4Expected.mkString))
      },
      testM("correctly reports failure of simple assertion") {
        assertM(runLog(test5))(equalTo(test5Expected.mkString))
      },
      testM("correctly reports multiple nested failures") {
        assertM(runLog(test6))(equalTo(test6Expected.mkString))
      },
      testM("correctly reports labeled failures") {
        assertM(runLog(test7))(equalTo(test7Expected.mkString))
      },
      testM("correctly reports negated failures") {
        assertM(runLog(test8))(equalTo(test8Expected.mkString))
      },
      testM("correctly reports mock failure of invalid call") {
        assertM(runLog(mock1))(equalTo(mock1Expected.mkString))
      },
      testM("correctly reports mock failure of unmet expectations") {
        assertM(runLog(mock2))(equalTo(mock2Expected.mkString))
      },
      testM("correctly reports mock failure of unexpected call") {
        assertM(runLog(mock3))(equalTo(mock3Expected.mkString))
      },
      testM("correctly reports mock failure of invalid range") {
        assertM(runLog(mock4))(equalTo(mock4Expected.mkString))
      }
    ) @@ silent

  val test1Expected: Vector[String] = Vector(
    testStarted("Addition works fine"),
    testFinished("Addition works fine")
  )

  val test2Expected: Vector[String] = Vector(
    testStarted("Subtraction works fine"),
    testFinished("Subtraction works fine")
  )

  val test3Expected: Vector[String] = Vector(
    testStarted("Value falls within range"),
    testFailed(
      "Value falls within range",
      Vector(
        withOffset(2)(s"${blue("52")} did not satisfy ${cyan("equalTo(42)")}\n"),
        withOffset(2)(
          s"${blue("52")} did not satisfy ${cyan("(") + yellow("equalTo(42)") + cyan(" || (isGreaterThan(5) && isLessThan(10)))")}\n"
        ),
        withOffset(4)(assertSourceLocation() + "\n"),
        withOffset(2)(s"${blue("52")} did not satisfy ${cyan("isLessThan(10)")}\n"),
        withOffset(2)(
          s"${blue("52")} did not satisfy ${cyan("(equalTo(42) || (isGreaterThan(5) && ") + yellow("isLessThan(10)") + cyan("))")}\n"
        ),
        withOffset(4)(assertSourceLocation())
      )
    )
  )

  val test4Expected: Vector[String] = Vector(
    testStarted("Failing test", location = None),
    testFailed(
      "Failing test",
      Vector(
        withOffset(2)("Fiber failed.\n") +
          withOffset(2)("A checked error was not handled.\n") +
          withOffset(2)("Fail\n") +
          withOffset(2)("No ZIO Trace available.")
      )
    )
  )

  val suite1Expected: Vector[String] = Vector(
    suiteStarted("Suite1")
  ) ++ test1Expected ++ test2Expected ++
    Vector(
      suiteFinished("Suite1")
    )

  val suite2Expected: Vector[String] = Vector(
    suiteStarted("Suite2")
  ) ++ test1Expected ++ test2Expected ++ test3Expected ++
    Vector(
      suiteFinished("Suite2")
    )

  val suite3Expected: Vector[String] = Vector(
    suiteStarted("Suite3")
  ) ++ suite1Expected ++ suite2Expected ++ test3Expected ++ Vector(
    suiteFinished("Suite3")
  )

  val suite4Expected: Vector[String] = Vector(
    suiteStarted("Suite4")
  ) ++ suite1Expected ++ Vector(suiteStarted("Empty"), suiteFinished("Empty")) ++
    test3Expected ++ Vector(suiteFinished("Suite4"))

  val test5Expected: Vector[String] = Vector(
    testStarted("Addition works fine"),
    testFailed(
      "Addition works fine",
      Vector(
        withOffset(2)(
          s"${blue(expressionIfNotRedundant(showExpression(1 + 1), 2))} did not satisfy ${cyan("equalTo(3)")}\n"
        ),
        withOffset(4)(assertSourceLocation())
      )
    )
  )

  val test6Expected: Vector[String] = Vector(
    testStarted("Multiple nested failures"),
    testFailed(
      "Multiple nested failures",
      Vector(
        withOffset(2)(s"${blue("3")} did not satisfy ${cyan("isGreaterThan(4)")}\n"),
        withOffset(2)(
          s"${blue("Some(3)")} did not satisfy ${cyan("isSome(") + yellow("isGreaterThan(4)") + cyan(")")}\n"
        ),
        withOffset(2)(
          s"${blue(s"Right(Some(3))")} did not satisfy ${cyan("isRight(") + yellow("isSome(isGreaterThan(4))") + cyan(")")}\n"
        ),
        withOffset(4)(assertSourceLocation())
      )
    )
  )

  val test7Expected: Vector[String] = Vector(
    testStarted("labeled failures"),
    testFailed(
      "labeled failures",
      Vector(
        withOffset(2)(s"${blue("0")} did not satisfy ${cyan("equalTo(1)")}\n"),
        withOffset(2)(
          s"${blue("`c` = Some(0)")} did not satisfy ${cyan("(isSome(") + yellow("equalTo(1)") + cyan(") ?? \"third\")")}\n"
        ),
        withOffset(4)(assertSourceLocation())
      )
    )
  )

  val test8Expected: Vector[String] = Vector(
    testStarted("Not combinator"),
    testFailed(
      "Not combinator",
      Vector(
        withOffset(2)(s"${blue("100")} satisfied ${cyan("equalTo(100)")}\n"),
        withOffset(2)(
          s"${blue("100")} did not satisfy ${cyan("not(") + yellow("equalTo(100)") + cyan(")")}\n"
        ),
        withOffset(4)(assertSourceLocation())
      )
    )
  )

  val mock1Expected: Vector[String] = Vector(
    testStarted("Invalid call"),
    testFailed(
      "Invalid call",
      Vector(
        withOffset(2)(s"${red("- could not find a matching expectation")}\n"),
        withOffset(4)(
          s"${red("- zio.test.mock.module.PureModuleMock.ParameterizedCommand called with invalid arguments")}\n"
        ),
        withOffset(6)(s"${blue("2")} did not satisfy ${cyan("equalTo(1)")}\n"),
        withOffset(4)(s"${red("- invalid call to zio.test.mock.module.PureModuleMock.SingleParam")}\n"),
        withOffset(6)(
          s"expected zio.test.mock.module.PureModuleMock.ParameterizedCommand with arguments ${cyan("equalTo(1)")}"
        )
      )
    )
  )

  val mock2Expected: Vector[String] = Vector(
    testStarted("Unsatisfied expectations"),
    testFailed(
      "Unsatisfied expectations",
      Vector(
        withOffset(2)(s"${red("- unsatisfied expectations")}\n"),
        withOffset(4)(s"in sequential order\n"),
        withOffset(6)(s"""zio.test.mock.module.PureModuleMock.SingleParam with arguments ${cyan("equalTo(2)")}\n"""),
        withOffset(6)(s"""zio.test.mock.module.PureModuleMock.SingleParam with arguments ${cyan("equalTo(3)")}""")
      )
    )
  )

  val mock3Expected: Vector[String] = Vector(
    testStarted("Extra calls"),
    testFailed(
      "Extra calls",
      Vector(
        withOffset(2)(
          s"${red("- unexpected call to zio.test.mock.module.PureModuleMock.ManyParams with arguments")}\n"
        ),
        withOffset(4)(s"${cyan("(2,3,4)")}")
      )
    )
  )

  val mock4Expected: Vector[String] = Vector(
    testStarted("Invalid range"),
    testFailed(
      "Invalid range",
      Vector(
        withOffset(2)(s"""${red("- invalid repetition range 4 to 2 by -1")}""")
      )
    )
  )
}
object IntelliJRenderUtils {
  import IntelliJRenderer.escape

  def suiteStarted(name: String): String =
    s"##teamcity[testSuiteStarted name='$name']" + "\n"

  def suiteFinished(name: String): String =
    s"##teamcity[testSuiteFinished name='$name']" + "\n"

  def testStarted(name: String, location: Option[String] = Some(ReportingTestUtils.sourceFilePath)): String = {
    val loc = location.fold("")(s => s"file://$s:XXX")
    s"##teamcity[testStarted name='$name' locationHint='$loc']" + "\n"
  }

  def testFinished(name: String): String =
    s"##teamcity[testFinished name='$name' duration='']" + "\n"

  def testFailed(name: String, error: Vector[String]): String =
    s"##teamcity[testFailed name='$name' message='Assertion failed:' details='${escape(error.mkString)}']" + "\n"

  def runLog(spec: ZSpec[TestEnvironment, String]): ZIO[TestEnvironment, Nothing, String] =
    for {
      _ <- IntelliJTestRunner(testEnvironment)
             .run(spec)
             .provideLayer[Nothing, TestEnvironment, Has[TestLogger] with Has[Clock]](
               TestLogger.fromConsole ++ TestClock.default
             )
      output <- TestConsole.output
    } yield output.mkString.withNoLineNumbers

  private[this] def IntelliJTestRunner(testEnvironment: Layer[Nothing, TestEnvironment]) =
    TestRunner[TestEnvironment, String](
      executor = TestExecutor.default[TestEnvironment, String](testEnvironment),
      reporter = DefaultTestReporter(IntelliJRenderer, TestAnnotationRenderer.default)
    )
}

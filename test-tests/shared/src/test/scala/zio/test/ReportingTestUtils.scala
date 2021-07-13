package zio.test

import zio.clock.Clock
import zio.test.Assertion.{equalTo, isGreaterThan, isLessThan, isRight, isSome, not}
import zio.test.environment.{TestClock, TestConsole, TestEnvironment, testEnvironment}
import zio.test.mock.Expectation._
import zio.test.mock.internal.InvalidCall._
import zio.test.mock.internal.MockException._
import zio.test.mock.module.{PureModule, PureModuleMock}
import zio.{Cause, Layer, Promise, ZIO}

import java.util.regex.Pattern
import scala.{Console => SConsole}

object ReportingTestUtils {

  val sourceFilePath: String = zio.test.sourcePath

  def expectedSuccess(label: String): String =
    green("+") + " " + label + "\n"

  def expectedFailure(label: String): String =
    red("- " + label) + "\n"

  def expectedIgnored(label: String): String =
    yellow("- ") + yellow(label) + " - " + TestAnnotation.ignored.identifier + " suite" + "\n"

  def withOffset(n: Int)(s: String): String =
    " " * n + s

  def green(s: String): String =
    SConsole.GREEN + s + SConsole.RESET

  def red(s: String): String =
    SConsole.RED + s + SConsole.RESET

  def blue(s: String): String =
    SConsole.BLUE + s + SConsole.RESET

  def cyan(s: String): String =
    SConsole.CYAN + s + SConsole.RESET

  def yellow(s: String): String =
    SConsole.YELLOW + s + SConsole.RESET

  def reportStats(success: Int, ignore: Int, failure: Int): String = {
    val total = success + ignore + failure
    cyan(
      s"Ran $total test${if (total == 1) "" else "s"} in 0 ns: $success succeeded, $ignore ignored, $failure failed"
    ) + "\n"
  }

  def runLog(spec: ZSpec[TestEnvironment, String]): ZIO[TestEnvironment, Nothing, String] =
    for {
      _ <- TestTestRunner(testEnvironment)
             .run(spec)
             .provideLayer[Nothing, TestEnvironment, TestLogger with Clock](TestLogger.fromConsole ++ TestClock.default)
      output <- TestConsole.output
    } yield output.mkString.withNoLineNumbers

  def runSummary(spec: ZSpec[TestEnvironment, String]): ZIO[TestEnvironment, Nothing, String] =
    for {
      results <- TestTestRunner(testEnvironment)
                   .run(spec)
                   .provideLayer[Nothing, TestEnvironment, TestLogger with Clock](
                     TestLogger.fromConsole ++ TestClock.default
                   )
      actualSummary = SummaryBuilder.buildSummary(results)
    } yield actualSummary.summary.withNoLineNumbers

  private[this] def TestTestRunner(testEnvironment: Layer[Nothing, TestEnvironment]) =
    TestRunner[TestEnvironment, String](
      executor = TestExecutor.default[TestEnvironment, String](testEnvironment),
      reporter = DefaultTestReporter(TestAnnotationRenderer.default)
    )

  val test1: ZSpec[Any, Nothing] = test("Addition works fine")(assert(1 + 1)(equalTo(2)))
  val test1Expected: String      = expectedSuccess("Addition works fine")

  val test2: ZSpec[Any, Nothing] = test("Subtraction works fine")(assert(1 - 1)(equalTo(0)))
  val test2Expected: String      = expectedSuccess("Subtraction works fine")

  val test3: ZSpec[Any, Nothing] =
    test("Value falls within range")(assert(52)(equalTo(42) || (isGreaterThan(5) && isLessThan(10))))
  val test3Expected: Vector[String] = Vector(
    expectedFailure("Value falls within range"),
    withOffset(2)(s"${blue("52")} did not satisfy ${cyan("equalTo(42)")}\n"),
    withOffset(2)(
      s"${blue("52")} did not satisfy ${cyan("(") + yellow("equalTo(42)") + cyan(" || (isGreaterThan(5) && isLessThan(10)))")}\n"
    ),
    withOffset(2)(assertSourceLocation() + "\n"),
    withOffset(2)(s"${blue("52")} did not satisfy ${cyan("isLessThan(10)")}\n"),
    withOffset(2)(
      s"${blue("52")} did not satisfy ${cyan("(equalTo(42) || (isGreaterThan(5) && ") + yellow("isLessThan(10)") + cyan("))")}\n"
    ),
    withOffset(2)(assertSourceLocation() + "\n")
  )

  val test4: Spec[Any, TestFailure[String], Nothing] =
    Spec.labeled("Failing test", Spec.test(failed(Cause.fail("Fail")), TestAnnotationMap.empty))
  val test4Expected: Vector[String] = Vector(
    expectedFailure("Failing test"),
    withOffset(2)("Fiber failed.\n") +
      withOffset(2)("A checked error was not handled.\n") +
      withOffset(2)("Fail\n") +
      withOffset(2)("No ZIO Trace available.\n")
  )

  val test5: ZSpec[Any, Nothing] = test("Addition works fine")(assert(1 + 1)(equalTo(3)))
  // the captured expression for `1+1` is different between dotty and 2.x
  def expressionIfNotRedundant(expr: String, value: Any): String =
    Option(expr).filterNot(_ == value.toString).fold(value.toString)(e => s"`$e` = $value")
  val test5Expected: Vector[String] = Vector(
    expectedFailure("Addition works fine"),
    withOffset(2)(
      s"${blue(expressionIfNotRedundant(showExpression(1 + 1), 2))} did not satisfy ${cyan("equalTo(3)")}\n"
    ),
    withOffset(2)(assertSourceLocation() + "\n")
  )

  val test6: ZSpec[Any, Nothing] =
    test("Multiple nested failures")(assert(Right(Some(3)))(isRight(isSome(isGreaterThan(4)))))
  val test6Expected: Vector[String] = Vector(
    expectedFailure("Multiple nested failures"),
    withOffset(2)(s"${blue("3")} did not satisfy ${cyan("isGreaterThan(4)")}\n"),
    withOffset(2)(
      s"${blue("Some(3)")} did not satisfy ${cyan("isSome(") + yellow("isGreaterThan(4)") + cyan(")")}\n"
    ),
    withOffset(2)(
      s"${blue(s"Right(Some(3))")} did not satisfy ${cyan("isRight(") + yellow("isSome(isGreaterThan(4))") + cyan(")")}\n"
    ),
    withOffset(2)(assertSourceLocation() + "\n")
  )

  val test7: ZSpec[Any, Nothing] = testM("labeled failures") {
    for {
      a <- ZIO.effectTotal(Some(1))
      b <- ZIO.effectTotal(Some(1))
      c <- ZIO.effectTotal(Some(0))
      d <- ZIO.effectTotal(Some(1))
    } yield assert(a)(isSome(equalTo(1)).label("first")) &&
      assert(b)(isSome(equalTo(1)).label("second")) &&
      assert(c)(isSome(equalTo(1)).label("third")) &&
      assert(d)(isSome(equalTo(1)).label("fourth"))
  }
  val test7Expected: Vector[String] = Vector(
    expectedFailure("labeled failures"),
    withOffset(2)(s"${blue("0")} did not satisfy ${cyan("equalTo(1)")}\n"),
    withOffset(2)(
      s"${blue("`c` = Some(0)")} did not satisfy ${cyan("(isSome(") + yellow("equalTo(1)") + cyan(") ?? \"third\")")}\n"
    ),
    withOffset(2)(assertSourceLocation() + "\n")
  )

  val test8: ZSpec[Any, Nothing] = test("Not combinator") {
    assert(100)(not(equalTo(100)))
  }
  val test8Expected: Vector[String] = Vector(
    expectedFailure("Not combinator"),
    withOffset(2)(s"${blue("100")} satisfied ${cyan("equalTo(100)")}\n"),
    withOffset(2)(
      s"${blue("100")} did not satisfy ${cyan("not(") + yellow("equalTo(100)") + cyan(")")}\n"
    ),
    withOffset(2)(assertSourceLocation() + "\n")
  )

  val suite1: Spec[Any, TestFailure[Nothing], TestSuccess] = suite("Suite1")(test1, test2)
  val suite1Expected: Vector[String] = Vector(
    expectedSuccess("Suite1"),
    withOffset(2)(test1Expected),
    withOffset(2)(test2Expected)
  )

  val suite2: Spec[Any, TestFailure[Nothing], TestSuccess] = suite("Suite2")(test1, test2, test3)
  val suite2Expected: Vector[String] = Vector(
    expectedFailure("Suite2"),
    withOffset(2)(test1Expected),
    withOffset(2)(test2Expected)
  ) ++ test3Expected.map(withOffset(2)(_))

  val suite3: Spec[Any, TestFailure[Nothing], TestSuccess] = suite("Suite3")(suite1, suite2, test3)
  val suite3Expected: Vector[String] = Vector(expectedFailure("Suite3")) ++
    suite1Expected.map(withOffset(2)) ++
    suite2Expected.map(withOffset(2)) ++
    test3Expected.map(withOffset(2))

  val suite4: Spec[Any, TestFailure[Nothing], TestSuccess] = suite("Suite4")(suite1, suite("Empty")(), test3)
  val suite4Expected: Vector[String] = Vector(expectedFailure("Suite4")) ++
    suite1Expected.map(withOffset(2)) ++
    Vector(withOffset(2)(expectedIgnored("Empty"))) ++
    test3Expected.map(withOffset(2))

  val mock1: ZSpec[Any, Nothing] = test("Invalid call") {
    throw InvalidCallException(
      List(
        InvalidCapability(PureModuleMock.SingleParam, PureModuleMock.ParameterizedCommand, equalTo(1)),
        InvalidArguments(PureModuleMock.ParameterizedCommand, 2, equalTo(1))
      )
    )
  }

  val mock1Expected: Vector[String] = Vector(
    expectedFailure("Invalid call"),
    withOffset(2)(s"${red("- could not find a matching expectation")}\n"),
    withOffset(4)(
      s"${red("- zio.test.mock.module.PureModuleMock.ParameterizedCommand called with invalid arguments")}\n"
    ),
    withOffset(6)(s"${blue("2")} did not satisfy ${cyan("equalTo(1)")}\n"),
    withOffset(4)(s"${red("- invalid call to zio.test.mock.module.PureModuleMock.SingleParam")}\n"),
    withOffset(6)(
      s"expected zio.test.mock.module.PureModuleMock.ParameterizedCommand with arguments ${cyan("equalTo(1)")}\n"
    )
  )

  val mock2: ZSpec[Any, Nothing] = test("Unsatisfied expectations") {
    throw UnsatisfiedExpectationsException(
      PureModuleMock.SingleParam(equalTo(2), value("foo")) ++
        PureModuleMock.SingleParam(equalTo(3), value("bar"))
    )
  }

  val mock2Expected: Vector[String] = Vector(
    expectedFailure("Unsatisfied expectations"),
    withOffset(2)(s"${red("- unsatisfied expectations")}\n"),
    withOffset(4)(s"in sequential order\n"),
    withOffset(6)(s"""zio.test.mock.module.PureModuleMock.SingleParam with arguments ${cyan("equalTo(2)")}\n"""),
    withOffset(6)(s"""zio.test.mock.module.PureModuleMock.SingleParam with arguments ${cyan("equalTo(3)")}\n""")
  )

  val mock3: ZSpec[Any, Nothing] = test("Extra calls") {
    throw UnexpectedCallException(PureModuleMock.ManyParams, (2, "3", 4L))
  }

  val mock3Expected: Vector[String] = Vector(
    expectedFailure("Extra calls"),
    withOffset(2)(s"${red("- unexpected call to zio.test.mock.module.PureModuleMock.ManyParams with arguments")}\n"),
    withOffset(4)(s"${cyan("(2,3,4)")}\n")
  )

  val mock4: ZSpec[Any, Nothing] = test("Invalid range") {
    throw InvalidRangeException(4 to 2 by -1)
  }

  val mock4Expected: Vector[String] = Vector(
    expectedFailure("Invalid range"),
    withOffset(2)(s"""${red("- invalid repetition range 4 to 2 by -1")}\n""")
  )

  val mock5: ZSpec[Any, String] = testM("Failing layer") {
    for {
      promise     <- Promise.make[Nothing, Unit]
      failingLayer = (promise.await *> ZIO.fail("failed!")).toLayer[String]
      mock = PureModuleMock.ZeroParams(value("mocked")).toLayer.tap { _ =>
               promise.succeed(())
             }
      f       = ZIO.serviceWith[PureModule.Service](_.zeroParams) <* ZIO.service[String]
      result <- f.provideLayer(failingLayer ++ mock).untraced
    } yield assert(result)(equalTo("mocked"))
  }

  val mock5Expected: Vector[String] = Vector(
    """.*Failing layer.*""",
    """.*- unsatisfied expectations.*""",
    """\s*zio\.test\.mock\.module\.PureModuleMock\.ZeroParams with arguments.*""",
    """\s*Fiber failed\.""",
    """[\s║╠─]*A checked error was not handled.""",
    """[\s║]*failed!"""
  )

  def assertSourceLocation(): String = cyan(s"☛ $sourceFilePath:XXX")
  implicit class TestOutputOps(output: String) {
    def withNoLineNumbers: String =
      output.replaceAll(Pattern.quote(sourceFilePath + ":") + "\\d+", sourceFilePath + ":XXX")
  }
}

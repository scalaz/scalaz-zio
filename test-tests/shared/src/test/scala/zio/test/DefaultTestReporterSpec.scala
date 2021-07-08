package zio.test

import zio.test.Assertion._
import zio.test.ReportingTestUtils._
import zio.test.TestAspect.silent

object DefaultTestReporterSpec extends ZIOBaseSpec {

  def spec: ZSpec[Environment, Failure] =
    suite("DefaultTestReporterSpec")(
      testM("correctly reports a successful test") {
        assertM(runLog(test1))(equalTo(test1Expected.mkString + reportStats(1, 0, 0)))
      },
      testM("correctly reports a failed test") {
        assertM(runLog(test3))(equalTo(test3Expected.mkString + reportStats(0, 0, 1)))
      },
      testM("correctly reports an error in a test") {
        assertM(runLog(test4))(equalTo(test4Expected.mkString + reportStats(0, 0, 1)))
      },
      testM("correctly reports successful test suite") {
        assertM(runLog(suite1))(equalTo(suite1Expected.mkString + reportStats(2, 0, 0)))
      },
      testM("correctly reports failed test suite") {
        assertM(runLog(suite2))(equalTo(suite2Expected.mkString + reportStats(2, 0, 1)))
      },
      testM("correctly reports multiple test suites") {
        assertM(runLog(suite3))(equalTo(suite3Expected.mkString + reportStats(4, 0, 2)))
      },
      testM("correctly reports empty test suite") {
        assertM(runLog(suite4))(equalTo(suite4Expected.mkString + reportStats(2, 0, 1)))
      },
      testM("correctly reports failure of simple assertion") {
        assertM(runLog(test5))(equalTo(test5Expected.mkString + reportStats(0, 0, 1)))
      },
      testM("correctly reports multiple nested failures") {
        assertM(runLog(test6))(equalTo(test6Expected.mkString + reportStats(0, 0, 1)))
      },
      testM("correctly reports labeled failures") {
        assertM(runLog(test7))(equalTo(test7Expected.mkString + reportStats(0, 0, 1)))
      },
      testM("correctly reports negated failures") {
        assertM(runLog(test8))(equalTo(test8Expected.mkString + reportStats(0, 0, 1)))
      },
      testM("correctly reports mock failure of invalid call") {
        assertM(runLog(mock1))(equalTo(mock1Expected.mkString + reportStats(0, 0, 1)))
      },
      testM("correctly reports mock failure of unmet expectations") {
        assertM(runLog(mock2))(equalTo(mock2Expected.mkString + reportStats(0, 0, 1)))
      },
      testM("correctly reports mock failure of unexpected call") {
        assertM(runLog(mock3))(equalTo(mock3Expected.mkString + reportStats(0, 0, 1)))
      },
      testM("correctly reports mock failure of invalid range") {
        assertM(runLog(mock4))(equalTo(mock4Expected.mkString + reportStats(0, 0, 1)))
      },
      testM("correctly reports failures in presence of a mock") {
        assertM(runLog(mock5).map(_.linesWithSeparators.map(_.stripLineEnd).toSet))(
          mock5Expected.foldLeft[Assertion[Iterable[String]]](anything) { case (a, expectedLine) =>
            a && exists(matchesRegex(expectedLine.stripLineEnd))
          }
        )
      } @@ TestAspect.nonFlaky
    ) @@ silent
}

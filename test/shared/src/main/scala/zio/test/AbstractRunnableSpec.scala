/*
 * Copyright 2019-2021 John A. De Goes and the ZIO Contributors
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package zio.test

import org.portablescala.reflect.annotation.EnableReflectiveInstantiation
import zio.clock.Clock
import zio.duration.Duration
import zio.test.BoolAlgebra.Value
import zio.test.ExecutedSpec.{SuiteCase, TestCase}
import zio.{Has, Task, UIO, URIO, ZIO}

import java.nio.charset.Charset
import java.nio.file.{Files, Path, StandardOpenOption}

@EnableReflectiveInstantiation
abstract class AbstractRunnableSpec {

  type Environment <: Has[_]
  type Failure

  def aspects: List[TestAspect[Nothing, Environment, Nothing, Any]]
  def runner: TestRunner[Environment, Failure]
  def spec: ZSpec[Environment, Failure]

  /**
   * Returns an effect that executes the spec, producing the results of the execution.
   */
  final def run: URIO[TestLogger with Clock, ExecutedSpec[Failure]] =
    runSpec(spec)

  /**
   * Returns an effect that executes a given spec, producing the results of the execution.
   */
  private[zio] def runSpec(
    spec: ZSpec[Environment, Failure]
  ): URIO[TestLogger with Clock, ExecutedSpec[Failure]] =
    runner.run(aspects.foldLeft(spec)(_ @@ _))

  class FixSnapshotsReporter(className: String) extends TestReporter[Failure] {

    def findSourceFile(`class`: Class[_]) = {
      val r = `class`. getResource(".").toURI.toString.split("/")
      val start = r.takeWhile(_ != "target")
      val end =
        r.dropWhile(_ != "target")
          .dropWhile(_ == "target")
          .dropWhile(_.startsWith("scala-"))
          .dropWhile(_ == "test-classes")
      val e = (start ++ List("src") ++ end).mkString("\\")
      println(e.toString)
    }

    def fixFile(label: String, text: String): Task[Unit] = Task{
      println("sssssssssssssss")
//      /home/fokot/projects/zio/test-tests/jvm/target/scala-2.13/test-classes  /zio/test/__snapshots__/firsta
//      /home/fokot/projects/zio/test-tests/shared/src/test/scala               /zio/test/__snapshots__/firsta
      val `class` = Class.forName(className)
      findSourceFile(`class`)
      val fileUrl = `class`.getResource(s"__snapshots__/$label")
//      Files.write(Path.of(fileUrl.toURI), text.getBytes(Charset.defaultCharset()), StandardOpenOption.WRITE, StandardOpenOption.TRUNCATE_EXISTING)
      ()
    }

    override def apply(d: Duration, e: ExecutedSpec[Failure]): URIO[TestLogger, Unit] =
      e.caseValue match {
        case SuiteCase(label, specs) =>
          TestLogger.logLine(label) *> ZIO.foreach_(specs)(apply(d, _))
        case TestCase(label, Left(TestFailure.Assertion(Value(FailureDetails(assertion :: Nil, None)))), _) =>
          fixFile(label, assertion.value.toString).orDie *> TestLogger.logLine(s"$label snapshot updated")
        case TestCase(label, Left(TestFailure.Assertion(_)), _) =>
          TestLogger.logLine(s"$label weird state - should not happen - snapshot not updated")
        case TestCase(label, Left(TestFailure.Runtime(_)), _) =>
          TestLogger.logLine(s"Test $label failed in runtime, snapshot not updated")
        case TestCase(label, Right(_), _) =>
          TestLogger.logLine(s"Test $label passes, snapshot not updated")
      }
  }

  /**
   * Fixes snapshot file or inline snapshot
   */
  private[zio] def fixSnapshot(
    spec: ZSpec[Environment, Failure],
    className: String
  ): URIO[TestLogger with Clock, ExecutedSpec[Failure]] = {

    runner
      .withReporter(new FixSnapshotsReporter(className))
      .run(aspects.foldLeft(spec)(_ @@ _))
  }

  /**
   * the platform used by the runner
   */
  final def platform = runner.platform
}

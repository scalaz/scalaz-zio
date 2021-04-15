/*
 * Copyright 2019-2020 John A. De Goes and the ZIO Contributors
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
import zio.test.ExecutedSpec.{SuiteCase, TestCase}
import zio.{Has, UIO, URIO, ZIO}

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

  object FixSnapshotsReporter extends TestReporter[Failure] {

    override def apply(d: Duration, e: ExecutedSpec[Failure]): URIO[TestLogger, Unit] =
      e.caseValue match {
        case SuiteCase(label, specs) =>
          TestLogger.logLine(label) *> ZIO.foreach_(specs)(apply(d, _))
        case TestCase(label, Left(TestFailure.Assertion(_)), _) =>
          TestLogger.logLine(s"$label snapshot updated")
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
    spec: ZSpec[Environment, Failure]
  ): URIO[TestLogger with Clock, ExecutedSpec[Failure]] = {

    runner
      .withReporter(FixSnapshotsReporter)
      .run(aspects.foldLeft(spec)(_ @@ _))
  }

  /**
   * the platform used by the runner
   */
  final def platform = runner.platform
}

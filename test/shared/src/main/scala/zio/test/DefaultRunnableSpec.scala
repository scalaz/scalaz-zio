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

import zio.clock.Clock
import zio.duration._
import zio.test.environment.TestEnvironment
import zio.{Has, ULayer, URIO, ZIO, ZLayer}

/**
 * A default runnable spec that provides testable versions of all of the
 * modules in ZIO (Clock, Random, etc).
 */
// TODO: could be implemented in terms of CustomRunnableSpec[Has[Any]]
abstract class DefaultRunnableSpec extends RunnableSpec[TestEnvironment, Has[Any], Any] {

  override def aspects: List[TestAspect[Nothing, TestEnvironment, Nothing, Any]] =
    List(TestAspect.timeoutWarning(60.seconds))

  override def runner: TestRunner[TestEnvironment, SharedEnvironment, Any] =
    defaultTestRunner

  override def sharedLayer: ULayer[Has[Any]] =
    DefaultRunnableSpec.none

  /**
   * Returns an effect that executes a given spec, producing the results of the execution.
   */
  private[zio] override def runSpec(
    spec: ZSpec[Environment with SharedEnvironment, Failure]
  ): URIO[SharedEnvironment with TestLogger with Clock, ExecutedSpec[Failure]] =
    runner.run(aspects.foldLeft(spec)(_ @@ _) @@ TestAspect.fibers)

  /**
   * Builds a suite containing a number of other specs.
   */
  def suite[R, E, T](label: String)(specs: Spec[R, E, T]*): Spec[R, E, T] =
    zio.test.suite(label)(specs: _*)

  /**
   * Builds an effectual suite containing a number of other specs.
   */
  def suiteM[R, E, T](label: String)(specs: ZIO[R, E, Iterable[Spec[R, E, T]]]): Spec[R, E, T] =
    zio.test.suiteM(label)(specs)

  /**
   * Builds a spec with a single pure test.
   */
  def test(label: String)(assertion: => TestResult)(implicit loc: SourceLocation): ZSpec[Any, Nothing] =
    zio.test.test(label)(assertion)

  /**
   * Builds a spec with a single effectful test.
   */
  def testM[R, E](label: String)(assertion: => ZIO[R, E, TestResult])(implicit loc: SourceLocation): ZSpec[R, E] =
    zio.test.testM(label)(assertion)
}

object DefaultRunnableSpec {
  val none : ULayer[Has[Any]] = ZLayer.succeed(())
}
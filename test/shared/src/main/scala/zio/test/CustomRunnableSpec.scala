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

import zio.clock.Clock
import zio.{Has, Tag, URIO, URLayer, ZEnv}
import zio.duration._
import zio.test.environment.TestEnvironment

abstract class CustomRunnableSpec[R <: Has[_] : Tag](
//  val customLayer: URLayer[TestEnvironment, R]
  val customLayer: URLayer[ZEnv, R]
) extends RunnableSpec[TestEnvironment, R, Any] {

  override def aspects: List[TestAspect[Nothing, Environment, Nothing, Any]] =
    List(TestAspect.timeoutWarning(60.seconds))

  override def runner: TestRunner[Environment, SharedEnvironment, Any] =
    customTestRunner

  /**
   * Returns an effect that executes a given spec, producing the results of the execution.
   */
  private[zio] override def runSpec(
    spec: ZSpec[Environment with SharedEnvironment, Failure]
  ): URIO[SharedEnvironment with TestLogger with Clock, ExecutedSpec[Failure]] =
    runner.run(aspects.foldLeft(spec)(_ @@ _) @@ TestAspect.fibers)
}

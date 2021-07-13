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

import zio._
import zio.test.render._

/**
 * A `RunnableSpec` has a main function and can be run by the JVM / Scala.js.
 */
abstract class RunnableSpec[R, E] extends AbstractRunnableSpec {
  override type Environment = R
  override type Failure     = E

  private def run(spec: ZSpec[Environment, Failure], testArgs: TestArgs): URIO[Has[TestLogger] with Has[Clock], Int] = {
    val filteredSpec = FilteredSpec(spec, testArgs)
    val testReporter = testArgs.testRenderer.fold(runner.reporter)(createTestReporter)
    for {
      results <- runner.withReporter(testReporter).run(aspects.foldLeft(filteredSpec)(_ @@ _))
      hasFailures = results.exists {
                      case ExecutedSpec.TestCase(_, test, _) => test.isLeft
                      case _                                 => false
                    }
      _ <- TestLogger
             .logLine(SummaryBuilder.buildSummary(results).summary)
             .when(testArgs.printSummary)
    } yield if (hasFailures) 1 else 0
  }

  /**
   * A simple main function that can be used to run the spec.
   */
  final def main(args: Array[String]): Unit = {
    val testArgs = TestArgs.parse(args)
    val runtime  = runner.runtime
    if (TestPlatform.isJVM) {
      val exitCode = runtime.unsafeRun(run(spec, testArgs).provideLayer(runner.bootstrap))
      doExit(exitCode)
    } else if (TestPlatform.isJS) {
      runtime.unsafeRunAsyncWith[Nothing, Int](run(spec, testArgs).provideLayer(runner.bootstrap)) { exit =>
        val exitCode = exit.getOrElse(_ => 1)
        doExit(exitCode)
      }
    }
  }

  private def doExit(exitCode: Int): Unit =
    try if (!isAmmonite) sys.exit(exitCode)
    catch { case _: SecurityException => }

  private def isAmmonite: Boolean =
    sys.env.exists { case (k, v) =>
      k.contains("JAVA_MAIN_CLASS") && v == "ammonite.Main"
    }

  private def createTestReporter(rendererName: String): TestReporter[Failure] = {
    val renderer = rendererName match {
      case "intellij" => IntelliJRenderer
      case _          => TestRenderer.default
    }
    DefaultTestReporter(renderer, TestAnnotationRenderer.default)
  }
}

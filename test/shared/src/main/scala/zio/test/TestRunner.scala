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
import zio.clock.Clock
import zio.console.Console
import zio.internal.Platform
import zio.test.environment.{TestEnvironment, testEnvironment}

/**
 * A `TestRunner[R, E]` encapsulates all the logic necessary to run specs that
 * require an environment `R` and may fail with an error `E`. Test runners
 * require a test executor, a platform, and a reporter.
 */
final case class TestRunner[R <: Has[_], E](
  executor: TestExecutor2[R, E],
  platform: Platform = Platform.makeDefault().withReportFailure(_ => ()),
  reporter: TestReporter[E] = DefaultTestReporter(TestAnnotationRenderer.default),
  bootstrap: Layer[Nothing, Annotations with TestLogger with Clock] =
    ((Console.live >>> TestLogger.fromConsole) ++ Clock.live) ++ Annotations.live
) { self =>

  lazy val runtime: Runtime[Unit] = Runtime((), platform)

  /**
   * Runs the spec, producing the execution results.
   */
  // TODO: `Annotations with TestLogger with Clock` comes up in many places
  //        a type alias might be useful:
  //        type TestRunnerDeps = Annotations with TestLogger with Clock
  def run(spec: ZSpec[R, E]): URIO[R with Annotations with TestLogger with Clock, ExecutedSpec[E]] =
    executor.run(spec, ExecutionStrategy.ParallelN(4)).timed.flatMap { case (duration, results) =>
      reporter(duration, results).as(results)
    }

  /**
   * An unsafe, synchronous run of the specified spec.
   */
  def unsafeRun(
    spec: ZSpec[R, E],
    environment: URLayer[TestEnvironment, R]
  ): ExecutedSpec[E] = {
    val env = (testEnvironment >>> environment) ++ bootstrap
    runtime.unsafeRun(run(spec).provideLayer(env))
  }

  /**
   * An unsafe, asynchronous run of the specified spec.
   */
  def unsafeRunAsync(
    spec: ZSpec[R, E],
    environment: URLayer[TestEnvironment, R]
  )(
    k: ExecutedSpec[E] => Unit
  ): Unit = {
    val env = (testEnvironment >>> environment) ++ bootstrap
    runtime.unsafeRunAsync(run(spec).provideLayer(env)) {
      case Exit.Success(v) => k(v)
      case Exit.Failure(c) => throw FiberFailure(c)
    }
  }

  /**
   * An unsafe, synchronous run of the specified spec.
   */
  def unsafeRunSync(
    spec: ZSpec[R, E],
    environment: URLayer[TestEnvironment, R]
  ): Exit[Nothing, ExecutedSpec[E]] = {
    val env = (testEnvironment >>> environment) ++ bootstrap
    runtime.unsafeRunSync(run(spec).provideLayer(env))
  }

  /**
   * Creates a copy of this runner replacing the reporter.
   */
  def withReporter[E1 >: E](reporter: TestReporter[E1]): TestRunner[R, E] =
    copy(reporter = reporter)

  /**
   * Creates a copy of this runner replacing the platform
   */
  def withPlatform(f: Platform => Platform): TestRunner[R, E] =
    copy(platform = f(platform))

  private[test] def buildRuntime: Managed[Nothing, Runtime[Annotations with TestLogger with Clock]] =
    bootstrap.toRuntime(platform)
}

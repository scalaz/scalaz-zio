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
import zio.internal.Platform
import zio.test.render.TestRenderer

/**
 * A `TestRunner[R, E]` encapsulates all the logic necessary to run specs that
 * require an environment `R` and may fail with an error `E`. Test runners
 * require a test executor, a platform, and a reporter.
 */
final case class TestRunner[R, E](
  executor: TestExecutor[R, E],
  platform: Platform = Platform.makeDefault().withReportFailure(_ => ()),
  reporter: TestReporter[E] = DefaultTestReporter(TestRenderer.default, TestAnnotationRenderer.default),
  bootstrap: Layer[Nothing, Has[TestLogger] with Has[Clock]] = (Console.live >>> TestLogger.fromConsole) ++ Clock.live
) { self =>

  lazy val runtime: Runtime[Unit] = Runtime((), platform)

  /**
   * Runs the spec, producing the execution results.
   */
  def run(spec: ZSpec[R, E]): URIO[Has[TestLogger] with Has[Clock], ExecutedSpec[E]] =
    executor.run(spec, ExecutionStrategy.ParallelN(4)).timed.flatMap { case (duration, results) =>
      reporter(duration, results).as(results)
    }

  /**
   * An unsafe, synchronous run of the specified spec.
   */
  def unsafeRun(
    spec: ZSpec[R, E]
  ): ExecutedSpec[E] =
    runtime.unsafeRun(run(spec).provideLayer(bootstrap))

  /**
   * An unsafe, asynchronous run of the specified spec.
   */
  def unsafeRunAsync(
    spec: ZSpec[R, E]
  )(
    k: ExecutedSpec[E] => Unit
  ): Unit =
    runtime.unsafeRunAsyncWith(run(spec).provideLayer(bootstrap)) {
      case Exit.Success(v) => k(v)
      case Exit.Failure(c) => throw FiberFailure(c)
    }

  /**
   * An unsafe, synchronous run of the specified spec.
   */
  def unsafeRunSync(
    spec: ZSpec[R, E]
  ): Exit[Nothing, ExecutedSpec[E]] =
    runtime.unsafeRunSync(run(spec).provideLayer(bootstrap))

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

  private[test] def buildRuntime: Managed[Nothing, Runtime[Has[TestLogger] with Has[Clock]]] =
    bootstrap.toRuntime(platform)
}

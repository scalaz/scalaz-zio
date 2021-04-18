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

/**
 * A `TestRunner[R0, R1, E]` encapsulates all the logic necessary to run specs
 * that require an environment `R0 with R1` and may fail with an error `E`. Test
 * runners require a test executor, a platform, and a reporter.
 */
final case class TestRunner[R0 <: Has[_], R1 <: Has[_], E](
  executor: TestExecutor[R0, R1, E],
  platform: Platform = Platform.makeDefault().withReportFailure(_ => ()),
  reporter: TestReporter[E] = DefaultTestReporter(TestAnnotationRenderer.default),
  bootstrap: Layer[Nothing, TestLogger with Clock] = (Console.live >>> TestLogger.fromConsole) ++ Clock.live
) { self =>

  lazy val runtime: Runtime[Unit] = Runtime((), platform)

  /**
   * Runs the spec, producing the execution results.
   */
  def run(spec: ZSpec[R0 with R1, E]): URIO[R1 with TestLogger with Clock, ExecutedSpec[E]] =
    executor.run(spec, ExecutionStrategy.ParallelN(4)).timed.flatMap { case (duration, results) =>
      reporter(duration, results).as(results)
    }

  // TODO: BZ: how to test all the unsafeRun* methods?
  /**
   * An unsafe, synchronous run of the specified spec.
   */
  def unsafeRun(
    spec: ZSpec[R0, E]
  ): ExecutedSpec[E] =
    runtime.unsafeRun(runSpecR0(spec))

  /**
   * An unsafe, synchronous run of the specified spec.
   */
  def unsafeRun(
    spec: ZSpec[R0 with R1, E],
    environment: URLayer[ZEnv, R1]
  ): ExecutedSpec[E] =
    runtime.unsafeRun(runSpecR0WithR1(spec, environment))

  /**
   * An unsafe, asynchronous run of the specified spec.
   */
  def unsafeRunAsync(
    spec: ZSpec[R0, E]
  )(
    k: ExecutedSpec[E] => Unit
  ): Unit =
    runtime.unsafeRunAsync(runSpecR0(spec)) {
      case Exit.Success(v) => k(v)
      case Exit.Failure(c) => throw FiberFailure(c)
    }

  /**
   * An unsafe, asynchronous run of the specified spec.
   */
  def unsafeRunAsync(
    spec: ZSpec[R0 with R1, E],
    environment: URLayer[ZEnv, R1]
  )(
    k: ExecutedSpec[E] => Unit
  ): Unit =
    runtime.unsafeRunAsync(runSpecR0WithR1(spec, environment)) {
      case Exit.Success(v) => k(v)
      case Exit.Failure(c) => throw FiberFailure(c)
    }

  /**
   * An unsafe, synchronous run of the specified spec.
   */
  def unsafeRunSync(
    spec: ZSpec[R0, E]
  ): Exit[Nothing, ExecutedSpec[E]] =
    runtime.unsafeRunSync(runSpecR0(spec))

  /**
   * An unsafe, synchronous run of the specified spec.
   */
  def unsafeRunSync(
    spec: ZSpec[R0 with R1, E],
    environment: URLayer[ZEnv, R1]
  ): Exit[Nothing, ExecutedSpec[E]] =
    runtime.unsafeRunSync(runSpecR0WithR1(spec, environment))

  /**
   * Creates a copy of this runner replacing the reporter.
   */
  def withReporter[E1 >: E](reporter: TestReporter[E1]): TestRunner[R0, R1, E] =
    copy(reporter = reporter)

  /**
   * Creates a copy of this runner replacing the platform
   */
  def withPlatform(f: Platform => Platform): TestRunner[R0, R1, E] =
    copy(platform = f(platform))

  private[test] def buildRuntime: Managed[Nothing, Runtime[TestLogger with Clock]] =
    bootstrap.toRuntime(platform)

  private def runSpecR0(spec: ZSpec[R0, E]): UIO[ExecutedSpec[E]] =
    run(spec)
      .provideLayer(
        ZLayer.succeed(()).asInstanceOf[ULayer[R1]] ++
          bootstrap
      )

  private def runSpecR0WithR1(
    spec: ZSpec[R0 with R1, E],
    environment: URLayer[ZEnv, R1]
  ): UIO[ExecutedSpec[E]] =
    run(spec).provideLayer(
      (ZEnv.live >>> environment) ++ bootstrap
    )
}

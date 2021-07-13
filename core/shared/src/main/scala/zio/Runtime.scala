/*
 * Copyright 2017-2021 John A. De Goes and the ZIO Contributors
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

package zio

import zio.internal.tracing.{TracingConfig, ZIOFn}
import zio.internal.{Executor, FiberContext, Platform, PlatformConstants, Tracing}

import scala.concurrent.Future

/**
 * A `Runtime[R]` is capable of executing tasks within an environment `R`.
 */
trait Runtime[+R] {

  /**
   * The environment of the runtime.
   */
  def environment: R

  /**
   * The platform of the runtime, which provides the essential capabilities
   * necessary to bootstrap execution of tasks.
   */
  def platform: Platform

  /**
   * Constructs a new `Runtime` by mapping the environment.
   */
  def map[R1](f: R => R1): Runtime[R1] = Runtime(f(environment), platform)

  /**
   * Constructs a new `Runtime` by mapping the platform.
   */
  def mapPlatform(f: Platform => Platform): Runtime[R] = Runtime(environment, f(platform))

  /**
   * Executes the effect synchronously, failing
   * with [[zio.FiberFailure]] if there are any errors. May fail on
   * Scala.js if the effect cannot be entirely run synchronously.
   *
   * This method is effectful and should only be done at the edges of your program.
   */
  final def unsafeRun[E, A](zio: => ZIO[R, E, A]): A =
    unsafeRunSync(zio).getOrElse(c => throw FiberFailure(c))

  /**
   * Executes the Task/RIO effect synchronously, failing
   * with the original `Throwable` on both [[Cause.Fail]] and [[Cause.Die]].
   * In addition, appends a new element to the `Throwable`s "caused by" chain,
   * with this `Cause` "pretty printed" (in stackless mode) as the message.
   * May fail on Scala.js if the effect cannot be entirely run synchronously.
   *
   * This method is effectful and should only be done at the edges of your program.
   */
  final def unsafeRunTask[A](task: => RIO[R, A]): A =
    unsafeRunSync(task).fold(cause => throw cause.squashTrace, identity)

  /**
   * Executes the effect synchronously. May
   * fail on Scala.js if the effect cannot be entirely run synchronously.
   *
   * This method is effectful and should only be invoked at the edges of your program.
   */
  final def unsafeRunSync[E, A](zio: => ZIO[R, E, A]): Exit[E, A] = {
    val result = internal.OneShot.make[Exit[E, A]]

    unsafeRunWith(zio)(result.set)

    result.get()
  }

  /**
   * Executes the effect asynchronously,
   * eventually passing the exit value to the specified callback.
   *
   * This method is effectful and should only be invoked at the edges of your program.
   */
  final def unsafeRunAsync[E, A](zio: => ZIO[R, E, A])(k: Exit[E, A] => Any): Unit = {
    unsafeRunAsyncCancelable(zio)(k)
    ()
  }

  /**
   * Executes the effect asynchronously,
   * eventually passing the exit value to the specified callback.
   * It returns a callback, which can be used to interrupt the running execution.
   *
   * This method is effectful and should only be invoked at the edges of your program.
   */
  final def unsafeRunAsyncCancelable[E, A](zio: => ZIO[R, E, A])(k: Exit[E, A] => Any): Fiber.Id => Exit[E, A] = {
    lazy val curZio = if (Platform.isJVM) ZIO.yieldNow *> zio else zio
    val canceler    = unsafeRunWith(curZio)(k)
    fiberId => {
      val result = internal.OneShot.make[Exit[E, A]]
      canceler(fiberId)(result.set)
      result.get()
    }
  }

  /**
   * Executes the effect asynchronously, discarding the result of execution.
   *
   * This method is effectful and should only be invoked at the edges of your program.
   */
  final def unsafeRunAsync_[E, A](zio: ZIO[R, E, A]): Unit =
    unsafeRunAsync(zio)(_ => ())

  /**
   * Runs the IO, returning a Future that will be completed when the effect has been executed.
   *
   * This method is effectful and should only be used at the edges of your program.
   */
  final def unsafeRunToFuture[E <: Throwable, A](zio: ZIO[R, E, A]): CancelableFuture[A] = {
    val p: concurrent.Promise[A] = scala.concurrent.Promise[A]()

    val canceler = unsafeRunWith(zio)(_.fold(cause => p.failure(cause.squashTraceWith(identity)), p.success))

    new CancelableFuture[A](p.future) {
      def cancel(): Future[Exit[Throwable, A]] = {
        val p: concurrent.Promise[Exit[Throwable, A]] = scala.concurrent.Promise[Exit[Throwable, A]]()
        canceler(Fiber.Id.None)(p.success)
        p.future
      }
    }
  }

  /**
   * Constructs a new `Runtime` with the specified new environment.
   */
  def as[R1](r1: R1): Runtime[R1] = map(_ => r1)

  /**
   * Constructs a new `Runtime` with the specified executor.
   */
  def withExecutor(e: Executor): Runtime[R] = mapPlatform(_.withExecutor(e))

  /**
   * Constructs a new `Runtime` with the specified fatal predicate.
   */
  def withFatal(f: Throwable => Boolean): Runtime[R] = mapPlatform(_.withFatal(f))

  /**
   * Constructs a new `Runtime` with the fatal error reporter.
   */
  def withReportFatal(f: Throwable => Nothing): Runtime[R] = mapPlatform(_.withReportFatal(f))

  /**
   * Constructs a new `Runtime` with the specified error reporter.
   */
  def withReportFailure(f: Cause[Any] => Unit): Runtime[R] = mapPlatform(_.withReportFailure(f))

  /**
   * Constructs a new `Runtime` with the specified tracer and tracing configuration.
   */
  def withTracing(t: Tracing): Runtime[R] = mapPlatform(_.withTracing(t))

  /**
   * Constructs a new `Runtime` with the specified tracing configuration.
   */
  def withTracingConfig(config: TracingConfig): Runtime[R] = mapPlatform(_.withTracingConfig(config))

  private final def unsafeRunWith[E, A](
    zio: => ZIO[R, E, A]
  )(k: Exit[E, A] => Any): Fiber.Id => (Exit[E, A] => Any) => Unit = {
    val InitialInterruptStatus = InterruptStatus.Interruptible

    val fiberId = Fiber.newFiberId()

    val scope = ZScope.unsafeMake[Exit[E, A]]()

    val supervisor = platform.supervisor

    lazy val context: FiberContext[E, A] = new FiberContext[E, A](
      fiberId,
      platform,
      environment.asInstanceOf[AnyRef],
      platform.executor,
      InitialInterruptStatus,
      None,
      PlatformConstants.tracingSupported,
      Platform.newWeakHashMap(),
      supervisor,
      scope,
      platform.reportFailure
    )

    if (supervisor ne Supervisor.none) {
      supervisor.unsafeOnStart(environment, zio, None, context)

      context.onDone(exit => supervisor.unsafeOnEnd(exit.flatten, context))
    }

    context.evaluateNow(ZIOFn.recordStackTrace(() => zio)(zio.asInstanceOf[IO[E, A]]))
    context.runAsync(k)

    fiberId => k => unsafeRunAsync(context.interruptAs(fiberId))((exit: Exit[Nothing, Exit[E, A]]) => k(exit.flatten))
  }
}

object Runtime {

  /**
   * A runtime that can be shutdown to release resources allocated to it.
   */
  abstract class Managed[+R] extends Runtime[R] {

    /**
     * Shuts down this runtime and releases resources allocated to it. Once
     * this runtime has been shut down the behavior of methods on it is
     * undefined and it should be discarded.
     */
    def shutdown(): Unit

    override final def as[R1](r1: R1): Runtime.Managed[R1] =
      map(_ => r1)

    override final def map[R1](f: R => R1): Runtime.Managed[R1] =
      Managed(f(environment), platform, () => shutdown())

    override final def mapPlatform(f: Platform => Platform): Runtime.Managed[R] =
      Managed(environment, f(platform), () => shutdown())

    override final def withExecutor(e: Executor): Runtime.Managed[R] =
      mapPlatform(_.withExecutor(e))

    override final def withFatal(f: Throwable => Boolean): Runtime.Managed[R] =
      mapPlatform(_.withFatal(f))

    override final def withReportFatal(f: Throwable => Nothing): Runtime.Managed[R] =
      mapPlatform(_.withReportFatal(f))

    override final def withReportFailure(f: Cause[Any] => Unit): Runtime.Managed[R] =
      mapPlatform(_.withReportFailure(f))

    override final def withTracing(t: Tracing): Runtime.Managed[R] =
      mapPlatform(_.withTracing(t))

    override final def withTracingConfig(config: TracingConfig): Runtime.Managed[R] =
      mapPlatform(_.withTracingConfig(config))
  }

  object Managed {

    /**
     * Builds a new managed runtime given an environment `R`, a
     * [[zio.internal.Platform]], and a shut down action.
     */
    def apply[R](r: R, platform0: Platform, shutdown0: () => Unit): Runtime.Managed[R] =
      new Runtime.Managed[R] {
        val environment = r
        val platform    = platform0
        def shutdown()  = shutdown0()
      }
  }

  /**
   * Builds a new runtime given an environment `R` and a [[zio.internal.Platform]].
   */
  def apply[R](r: R, platform0: Platform): Runtime[R] = new Runtime[R] {
    val environment = r
    val platform    = platform0
  }

  lazy val default: Runtime[ZEnv] = Runtime(ZEnv.Services.live, Platform.default)

  lazy val global: Runtime[ZEnv] = Runtime(ZEnv.Services.live, Platform.global)

  /**
   * A version of the default runtime that tracks all fibers forked by tasks
   * it executes.
   */
  lazy val supervised: Runtime[ZEnv] =
    default.mapPlatform(_.withSupervisor(Supervisor.unsafeTrack(true)))

  /**
   * Unsafely creates a `Runtime` from a `ZLayer` whose resources will be
   * allocated immediately, and not released until the `Runtime` is shut down
   * or the end of the application.
   *
   * This method is useful for small applications and integrating ZIO with
   * legacy code, but other applications should investigate using
   * [[ZIO.provideLayer]] directly in their application entry points.
   */
  def unsafeFromLayer[R <: Has[_]](
    layer: Layer[Any, R],
    platform: Platform = Platform.default
  ): Runtime.Managed[R] = {
    val runtime = Runtime((), platform)
    val (environment, shutdown) = runtime.unsafeRun {
      ZManaged.ReleaseMap.make.flatMap { releaseMap =>
        layer.build.zio.provide(((), releaseMap)).flatMap { case (_, acquire) =>
          val finalizer = () =>
            runtime.unsafeRun {
              releaseMap.releaseAll(Exit.unit, ExecutionStrategy.Sequential).uninterruptible.unit
            }

          UIO.effectTotal(Platform.addShutdownHook(finalizer)).as((acquire, finalizer))
        }
      }
    }

    Runtime.Managed(environment, platform, shutdown)
  }
}

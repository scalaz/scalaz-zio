package zio.test

import zio.test.environment.{TestEnvironment, testEnvironment}
import zio.{ExecutionStrategy, Has, Tag, UIO, URLayer, ZEnv}
import zio.ZLayer

object TestUtils {

  def execute[E](spec: ZSpec[TestEnvironment, E]): UIO[ExecutedSpec[E]] =
    execute[Has[Any], E](spec, ZLayer.succeed((): Any))

  def execute[R <: Has[_]: Tag, E](
    spec: ZSpec[TestEnvironment with R, E],
    environment: URLayer[ZEnv, R]
  ): UIO[ExecutedSpec[E]] =
    TestExecutor
      .default[TestEnvironment, R, E](testEnvironment)
      .run(spec, ExecutionStrategy.Sequential)
      .provideLayer(ZEnv.live >>> environment)

  def forAllTests[E](
    execSpec: ExecutedSpec[E]
  )(f: Either[TestFailure[E], TestSuccess] => Boolean): Boolean =
    execSpec.forall {
      case ExecutedSpec.TestCase(_, test, _) => f(test)
      case _                                 => true
    }

  def isIgnored[E](spec: ZSpec[environment.TestEnvironment, E]): UIO[Boolean] =
    execute(spec).map { executedSpec =>
      forAllTests(executedSpec) {
        case Right(TestSuccess.Ignored) => true
        case _                          => false
      }
    }

  def succeeded[E](spec: ZSpec[environment.TestEnvironment, E]): UIO[Boolean] =
    execute(spec).map { executedSpec =>
      forAllTests(executedSpec) {
        case Right(TestSuccess.Succeeded(_)) => true
        case _                               => false
      }
    }
}

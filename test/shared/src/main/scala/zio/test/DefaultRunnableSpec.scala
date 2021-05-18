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
import zio.{URIO, ZIO}

import scala.language.implicitConversions

/**
 * A default runnable spec that provides testable versions of all of the
 * modules in ZIO (Clock, Random, etc).
 */
abstract class DefaultRunnableSpec extends RunnableSpec[TestEnvironment, Any] {

  override def aspects: List[TestAspect[Nothing, TestEnvironment, Nothing, Any]] =
    List(TestAspect.timeoutWarning(60.seconds))

  override def runner: TestRunner[TestEnvironment, Any] =
    defaultTestRunner

  /**
   * Returns an effect that executes a given spec, producing the results of the execution.
   */
  private[zio] override def runSpec(
    spec: ZSpec[Environment, Failure]
  ): URIO[TestLogger with Clock, ExecutedSpec[Failure]] =
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
  def test(label: String)(assertion: => TestReturnValue)(implicit loc: SourceLocation): ZSpec[Any, Nothing] =
    zio.test.test(label)(assertion)

  /**
   * Builds a spec with a single effectful test.
   */
  def testM[R, E](label: String)(assertion: => ZIO[R, E, TestReturnValue])(implicit loc: SourceLocation): ZSpec[R, E] =
    zio.test.testM(label)(assertion)

  implicit def any2AssertionOps[A](a: A): AssertionSyntax.AssertionOps[A] = AssertionSyntax.AssertionOps(a)
}

object AssertionSyntax {
  def assertionError(name: String): Nothing =
    throw new Error(s"`$name` may only be called within the body of `assert`")

  implicit final class EitherAssertionOps[A, B](private val self: Either[A, B]) extends AnyVal {
    def $asLeft: A = assertionError("$asLeft")

    def $asRight: B = assertionError("$asRight")
  }

  implicit final class AssertionOps[A](private val self: A) extends AnyVal {
    def withAssertion(assertion: Assertion[A]): Boolean =
      assertion.test(self)

    def $is[Case <: A]: Case = assertionError("$is")

    def $as[Case <: A]: Case = assertionError("$as")

    def $throwsA[E <: Throwable]: Boolean = assertionError("$throwsA")

    def $throws: Throwable = assertionError("$throws")
  }
}

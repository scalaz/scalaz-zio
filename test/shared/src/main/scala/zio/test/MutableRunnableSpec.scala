/*
 * Copyright 2021 John A. De Goes and the ZIO Contributors
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

import izumi.reflect.Tag
import zio.clock.Clock
import zio.duration._
import zio.test.environment.TestEnvironment
import zio.{Chunk, Has, URIO, ZIO, ZLayer, ULayer}

import scala.util.control.NoStackTrace

/**
 * Syntax for writing test like
 * {{{
 * object MySpec extends MutableRunnableSpec(layer, aspect) {
 *   suite("foo") {
 *     testM("name") {
 *     } @@ ignore
 *
 *     test("name 2")
 *   }
 *   suite("another suite") {
 *     test("name 3")
 *   }
 * }
 * }}}
 */
class MutableRunnableSpec[R <: Has[_]: Tag](
  layer: ZLayer[TestEnvironment, Throwable, R],
  aspect: TestAspect[R with TestEnvironment, R with TestEnvironment, Any, Any] = TestAspect.identity
) extends RunnableSpec[TestEnvironment, Has[Any], Any] {
  self =>

    override def sharedLayer: ULayer[Has[Any]] =
      DefaultRunnableSpec.none

  private class InAnotherTestException(`type`: String, label: String)
      extends Exception(s"${`type`} `${label}` is in another test")
      with NoStackTrace

  sealed trait SpecBuilder {
    def toSpec: ZSpec[R with TestEnvironment, Any]
    def label: String
  }

  sealed case class SuiteBuilder(label: String) extends SpecBuilder {

    private[test] var nested: Chunk[SpecBuilder] = Chunk.empty
    private var aspects: Chunk[TestAspect[R with TestEnvironment, R with TestEnvironment, Failure, Failure]] =
      Chunk.empty

    /**
     * Syntax for adding aspects.
     * {{{
     * test("foo") { assert(42, equalTo(42)) } @@ ignore
     * }}}
     */
    final def @@(
      aspect: TestAspect[R with TestEnvironment, R with TestEnvironment, Failure, Failure]
    ): SuiteBuilder = {
      aspects = aspects :+ aspect
      this
    }

    def toSpec: ZSpec[R with TestEnvironment, Any] =
      aspects.foldLeft(
        zio.test.suite(label)(
          nested.map(_.toSpec): _*
        )
      )((spec, aspect) => spec @@ aspect)
  }

  sealed case class TestBuilder(label: String, var toSpec: ZSpec[R with TestEnvironment, Any]) extends SpecBuilder {

    /**
     * Syntax for adding aspects.
     * {{{
     * test("foo") { assert(42, equalTo(42)) } @@ ignore
     * }}}
     */
    final def @@(
      aspect: TestAspect[R with TestEnvironment, R with TestEnvironment, Failure, Failure]
    ): TestBuilder = {
      toSpec = toSpec @@ aspect
      this
    }
  }

  // init SpecBuilder for this test class
  private var stack: List[SuiteBuilder] = SuiteBuilder(self.getClass.getSimpleName.stripSuffix("$")) :: Nil
  // to prevent calling tests inside tests
  // there should be no tests constructions after spec was built
  private var specBuilt = false

  /**
   * Builds a suite containing a number of other specs.
   */
  final def suite(label: String)(specs: => SpecBuilder): SuiteBuilder = {
    if (specBuilt)
      throw new InAnotherTestException("Suite", label)
    val _oldStack = stack
    val builder   = SuiteBuilder(label)
    stack.head.nested = stack.head.nested :+ builder
    stack = builder :: stack
    specs
    stack = _oldStack
    builder
  }

  /**
   * Builds a spec with a single pure test.
   */
  final def test(label: String)(assertion: => TestResult)(implicit loc: SourceLocation): TestBuilder = {
    if (specBuilt)
      throw new InAnotherTestException("Test", label)
    val test    = zio.test.test(label)(assertion)
    val builder = TestBuilder(label, test)
    stack.head.nested = stack.head.nested :+ builder
    builder
  }

  /**
   * Builds a spec with a single effectful test.
   */
  final def testM(
    label: String
  )(assertion: => ZIO[R with TestEnvironment, Failure, TestResult])(implicit loc: SourceLocation): TestBuilder = {
    if (specBuilt)
      throw new InAnotherTestException("Test", label)
    val test    = zio.test.testM(label)(assertion)
    val builder = TestBuilder(label, test)
    stack.head.nested = stack.head.nested :+ builder
    builder
  }

  final override def spec: ZSpec[Environment, Failure] = {
    specBuilt = true
    (stack.head @@ aspect).toSpec.provideCustomLayerShared(layer.mapError(TestFailure.fail))
  }

  override def aspects: List[TestAspect[Nothing, TestEnvironment, Nothing, Any]] =
    List(TestAspect.timeoutWarning(60.seconds))

  override def runner: TestRunner[TestEnvironment, SharedEnvironment, Any] =
    defaultTestRunner

  /**
   * Returns an effect that executes a given spec, producing the results of the execution.
   */
  private[zio] override def runSpec(
    spec: ZSpec[Environment with SharedEnvironment, Failure]
  ): URIO[SharedEnvironment with TestLogger with Clock, ExecutedSpec[Failure]] =
    runner.run(aspects.foldLeft(spec)(_ @@ _) @@ TestAspect.fibers)
}

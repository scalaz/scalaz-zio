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

import zio.{ExecutionStrategy, Has, Layer, Tag, URIO, ZIO}

/**
 * A `TestExecutor[R0, R1, E]` is capable of executing specs that require an
 * environment of `R0 with R1` and may fail with an `E`. The environment is
 * split into two components:
 *  - `R0` is provided to the `Spec` by this `TestExecutor` using the layer
 *    returned by the implementation of the `environment` method. As a result,
 *    this part of the environment is never shared between tests.
 *  - The other part of the environment (`R1`) must be provided by code outside
 *    of this `TestExecutor`, which makes it possible to create a shared version
 *    of that dependency, and use it to run multiple independent specs.
 */
abstract class TestExecutor[+R0 <: Has[_], R1 <: Has[_], E] {
  def run(spec: ZSpec[R0 with R1, E], defExec: ExecutionStrategy): URIO[R1, ExecutedSpec[E]]
  def environment: Layer[Nothing, R0]
}

object TestExecutor {
  def default[R0 <: Annotations: Tag, R1 <: Has[_]: Tag, E](
    env: Layer[Nothing, R0]
  ): TestExecutor[R0, R1, E] =
    new TestExecutor[R0, R1, E] {
      def run(spec: ZSpec[R0 with R1, E], defExec: ExecutionStrategy): URIO[R1, ExecutedSpec[E]] =
        spec.annotated
          .provideSomeLayer[R1](environment)
          .foreachExec(defExec)(
            e =>
              e.failureOrCause.fold(
                { case (failure, annotations) => ZIO.succeedNow((Left(failure), annotations)) },
                cause => ZIO.succeedNow((Left(TestFailure.Runtime(cause)), TestAnnotationMap.empty))
              ),
            { case (success, annotations) =>
              ZIO.succeedNow((Right(success), annotations))
            }
          )
          .use(_.foldM[R1, Nothing, ExecutedSpec[E]](defExec) {
            case Spec.SuiteCase(label, specs, _) =>
              specs.map(specs => ExecutedSpec.suite(label, specs))
            case Spec.TestCase(label, test, staticAnnotations) =>
              test.map { case (result, dynamicAnnotations) =>
                ExecutedSpec.test(label, result, staticAnnotations ++ dynamicAnnotations)
              }.toManaged_
          }.useNow)
      val environment = env
    }
}

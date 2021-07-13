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

import zio.Chunk
import zio.test.ExecutedSpec._

/**
 * An `ExecutedSpec` is a spec that has been run to produce test results.
 */
final case class ExecutedSpec[+E](caseValue: SpecCase[E, ExecutedSpec[E]]) { self =>

  /**
   * Determines if any node in the spec is satisfied by the given predicate.
   */
  def exists(f: SpecCase[E, Boolean] => Boolean): Boolean =
    fold[Boolean] { c =>
      (c: @unchecked) match {
        case c @ MultipleCase(specs) => specs.exists(identity) || f(c)
        case c @ TestCase(_, _)      => f(c)
      }
    }

  /**
   * Folds over all nodes to produce a final result.
   */
  def fold[Z](f: SpecCase[E, Z] => Z): Z =
    (caseValue: @unchecked) match {
      case MultipleCase(specs) => f(MultipleCase(specs.map(_.fold(f))))
      case t @ TestCase(_, _)  => f(t)
    }

  /**
   * Determines if all nodes in the spec are satisfied by the given predicate.
   */
  def forall(f: SpecCase[E, Boolean] => Boolean): Boolean =
    fold[Boolean] { c =>
      (c: @unchecked) match {
        case c @ MultipleCase(specs) => specs.forall(identity) && f(c)
        case c @ TestCase(_, _)      => f(c)
      }
    }

  /**
   * Computes the size of the spec, i.e. the number of tests in the spec.
   */
  def size: Int =
    fold[Int] { c =>
      (c: @unchecked) match {
        case MultipleCase(counts) => counts.sum
        case TestCase(_, _)       => 1
      }
    }

  /**
   * Transforms the spec one layer at a time.
   */
  def transform[E1](f: SpecCase[E, ExecutedSpec[E1]] => SpecCase[E1, ExecutedSpec[E1]]): ExecutedSpec[E1] =
    (caseValue: @unchecked) match {
      case MultipleCase(specs) => ExecutedSpec(f(MultipleCase(specs.map(_.transform(f)))))
      case t @ TestCase(_, _)  => ExecutedSpec(f(t))
    }

  /**
   * Transforms the spec statefully, one layer at a time.
   */
  def transformAccum[E1, Z](
    z0: Z
  )(f: (Z, SpecCase[E, ExecutedSpec[E1]]) => (Z, SpecCase[E1, ExecutedSpec[E1]])): (Z, ExecutedSpec[E1]) =
    (caseValue: @unchecked) match {
      case MultipleCase(specs) =>
        val (z, specs1) =
          specs.foldLeft[(Z, Chunk[ExecutedSpec[E1]])](z0 -> Chunk.empty) { case ((z, vector), spec) =>
            val (z1, spec1) = spec.transformAccum(z)(f)

            z1 -> (vector :+ spec1)
          }

        val (z1, caseValue) = f(z, MultipleCase(specs1))

        z1 -> ExecutedSpec(caseValue)
      case t @ TestCase(_, _) =>
        val (z, caseValue) = f(z0, t)
        z -> ExecutedSpec(caseValue)
    }
}

object ExecutedSpec {

  sealed trait SpecCase[+E, +A] { self =>
    def map[B](f: A => B): SpecCase[E, B] =
      self match {
        case LabeledCase(label, spec)    => LabeledCase(label, f(spec))
        case MultipleCase(specs)         => MultipleCase(specs.map(f))
        case TestCase(test, annotations) => TestCase(test, annotations)
      }
  }

  final case class LabeledCase[+A](label: String, spec: A) extends SpecCase[Nothing, A]

  final case class MultipleCase[+A](specs: Chunk[A]) extends SpecCase[Nothing, A]

  final case class TestCase[+E](
    test: Either[TestFailure[E], TestSuccess],
    annotations: TestAnnotationMap
  ) extends SpecCase[E, Nothing]

  def labeled[E](label: String, spec: ExecutedSpec[E]): ExecutedSpec[E] =
    ExecutedSpec(LabeledCase(label, spec))

  def multiple[E](specs: Chunk[ExecutedSpec[E]]): ExecutedSpec[E] =
    ExecutedSpec(MultipleCase(specs))

  def test[E](
    test: Either[TestFailure[E], TestSuccess],
    annotations: TestAnnotationMap
  ): ExecutedSpec[E] =
    ExecutedSpec(TestCase(test, annotations))
}

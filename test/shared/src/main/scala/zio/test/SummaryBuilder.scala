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

import zio.test.render.ConsoleRenderer

object SummaryBuilder {
  def buildSummary[E](executedSpec: ExecutedSpec[E]): Summary = {
    val success = countTestResults(executedSpec) {
      case Right(TestSuccess.Succeeded(_)) => true
      case _                               => false
    }
    val fail = countTestResults(executedSpec)(_.isLeft)
    val ignore = countTestResults(executedSpec) {
      case Right(TestSuccess.Ignored) => true
      case _                          => false
    }
    val failures = extractFailures(executedSpec)
    val rendered = ConsoleRenderer
      .render(failures.flatMap(DefaultTestReporter.render(_, false)), TestAnnotationRenderer.silent)
      .mkString("\n")
    Summary(success, fail, ignore, rendered)
  }

  private def countTestResults[E](
    executedSpec: ExecutedSpec[E]
  )(pred: Either[TestFailure[E], TestSuccess] => Boolean): Int =
    executedSpec.fold[Int] { c =>
      (c: @unchecked) match {
        case ExecutedSpec.SuiteCase(_, counts) => counts.sum
        case ExecutedSpec.TestCase(_, test, _) => if (pred(test)) 1 else 0
      }
    }

  private def extractFailures[E](executedSpec: ExecutedSpec[E]): Seq[ExecutedSpec[E]] =
    executedSpec.fold[Seq[ExecutedSpec[E]]] { c =>
      (c: @unchecked) match {
        case ExecutedSpec.SuiteCase(label, specs) =>
          val newSpecs = specs.flatten
          if (newSpecs.nonEmpty) Seq(ExecutedSpec(ExecutedSpec.SuiteCase(label, newSpecs))) else Seq.empty
        case c @ ExecutedSpec.TestCase(_, test, _) => if (test.isLeft) Seq(ExecutedSpec(c)) else Seq.empty
      }
    }
}

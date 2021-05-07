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

import zio.test.mock.Expectation
import zio.test.mock.internal.{InvalidCall, MockException}
import zio.test.render.ExecutionResult.ResultType.{Suite, Test}
import zio.test.render.ExecutionResult.Status.{Failed, Ignored, Passed}
import zio.test.render.ExecutionResult.{ResultType, Status}
import zio.test.render.LogLine.{Fragment, Line, Message}
import zio.test.render._
import zio.{Cause, Has, _}

import java.util.regex.Pattern
import scala.annotation.tailrec
import scala.util.Try

object DefaultTestReporter {
  def render[E](
    executedSpec: ExecutedSpec[E],
    includeCause: Boolean
  ): Seq[ExecutionResult] = {
    def loop(
      executedSpec: ExecutedSpec[E],
      depth: Int,
      ancestors: List[TestAnnotationMap]
    ): Seq[ExecutionResult] =
      (executedSpec.caseValue: @unchecked) match {
        case ExecutedSpec.SuiteCase(label, specs) =>
          val hasFailures = executedSpec.exists {
            case ExecutedSpec.TestCase(_, test, _) => test.isLeft
            case _                                 => false
          }

          val annotations = executedSpec.fold[TestAnnotationMap] { es =>
            (es: @unchecked) match {
              case ExecutedSpec.SuiteCase(_, annotations)   => annotations.foldLeft(TestAnnotationMap.empty)(_ ++ _)
              case ExecutedSpec.TestCase(_, _, annotations) => annotations
            }
          }

          val (status, renderedLabel) =
            if (specs.isEmpty) (Ignored, Seq(renderSuiteIgnored(label, depth)))
            else if (hasFailures) (Failed, Seq(renderSuiteFailed(label, depth)))
            else (Passed, Seq(renderSuiteSucceeded(label, depth)))

          val allAnnotations = annotations :: ancestors
          val rest           = specs.flatMap(loop(_, depth + 1, allAnnotations))

          rendered(Suite, label, status, depth, renderedLabel.flatMap(_.lines): _*)
            .withAnnotations(allAnnotations) +: rest

        case ExecutedSpec.TestCase(label, result, annotations) =>
          val renderedResult = result match {
            case Right(TestSuccess.Succeeded(_)) =>
              Some(rendered(Test, label, Passed, depth, fr(label).toLine))
            case Right(TestSuccess.Ignored) =>
              Some(rendered(Test, label, Ignored, depth, warn(label).toLine))
            case Left(TestFailure.Assertion(result)) =>
              result
                .fold[Option[TestResult]] {
                  case result: AssertionResult.FailureDetailsResult => Some(BoolAlgebra.success(result))
                  case AssertionResult.TraceResult(trace, genFailureDetails) =>
                    Trace
                      .prune(trace, false)
                      .map(a => BoolAlgebra.success(AssertionResult.TraceResult(a, genFailureDetails)))
                }(
                  {
                    case (Some(a), Some(b)) => Some(a && b)
                    case (Some(a), None)    => Some(a)
                    case (None, Some(b))    => Some(b)
                    case _                  => None
                  },
                  {
                    case (Some(a), Some(b)) => Some(a || b)
                    case (Some(a), None)    => Some(a)
                    case (None, Some(b))    => Some(b)
                    case _                  => None
                  },
                  _.map(!_)
                )
                .map {
                  _.fold(details =>
                    rendered(Test, label, Failed, depth, renderFailure(label, depth, details).lines: _*)
                  )(
                    _ && _,
                    _ || _,
                    !_
                  )
                }

            case Left(TestFailure.Runtime(cause)) =>
              Some(renderRuntimeCause(cause, label, depth, includeCause))
          }
          renderedResult.map(r => Seq(r.withAnnotations(annotations :: ancestors))).getOrElse(Seq.empty)
      }

    loop(executedSpec, 0, List.empty)
  }

  def apply[E](testRenderer: TestRenderer, testAnnotationRenderer: TestAnnotationRenderer): TestReporter[E] = {
    (duration: Duration, executedSpec: ExecutedSpec[E]) =>
      val rendered = testRenderer.render(render(executedSpec, true), testAnnotationRenderer)
      val stats    = testRenderer.render(logStats(duration, executedSpec) :: Nil, testAnnotationRenderer)
      TestLogger.logLine((rendered ++ stats).mkString("\n"))
  }

  private def logStats[E](duration: Duration, executedSpec: ExecutedSpec[E]): ExecutionResult = {
    val (success, ignore, failure) = executedSpec.fold[(Int, Int, Int)] { es =>
      (es: @unchecked) match {
        case ExecutedSpec.SuiteCase(_, stats) =>
          stats.foldLeft((0, 0, 0)) { case ((x1, x2, x3), (y1, y2, y3)) =>
            (x1 + y1, x2 + y2, x3 + y3)
          }
        case ExecutedSpec.TestCase(_, result, _) =>
          result match {
            case Left(_)                         => (0, 0, 1)
            case Right(TestSuccess.Succeeded(_)) => (1, 0, 0)
            case Right(TestSuccess.Ignored)      => (0, 1, 0)
          }
      }
    }
    val total = success + ignore + failure
    val stats = detail(
      s"Ran $total test${if (total == 1) "" else "s"} in ${duration.render}: $success succeeded, $ignore ignored, $failure failed"
    )

    rendered(ResultType.Other, "", Status.Passed, 0, stats.toLine)
  }

  private def renderSuiteIgnored(label: String, offset: Int) =
    rendered(Suite, label, Ignored, offset, warn(s"- $label").toLine)

  private def renderSuiteFailed(label: String, offset: Int) =
    rendered(Suite, label, Failed, offset, error(s"- $label").toLine)

  private def renderSuiteSucceeded(label: String, offset: Int) =
    rendered(Suite, label, Passed, offset, fr(label).toLine)

  def renderAssertFailure(result: TestResult, label: String, depth: Int): ExecutionResult =
    result.fold(details => rendered(Test, label, Failed, depth, renderFailure(label, depth, details).lines: _*))(
      _ && _,
      _ || _,
      !_
    )

  private def renderRuntimeCause[E](cause: Cause[E], label: String, depth: Int, includeCause: Boolean) = {
    val failureDetails =
      Seq(renderFailureLabel(label, depth)) ++ Seq(renderCause(cause, depth)).filter(_ => includeCause).flatMap(_.lines)

    rendered(Test, label, Failed, depth, failureDetails: _*)
  }

  def renderAssertionResult(assertionResult: AssertionResult, offset: Int): Message =
    assertionResult match {
      case AssertionResult.TraceResult(trace, genFailureDetails) =>
        val failures = FailureCase.fromTrace(trace)
        failures
          .map(fc =>
            renderGenFailureDetails(genFailureDetails, offset) ++
              Message(renderFailureCase(fc, offset))
          )
          .foldLeft(Message.empty)(_ ++ _)

      case AssertionResult.FailureDetailsResult(failureDetails, genFailureDetails) =>
        renderGenFailureDetails(genFailureDetails, offset) ++
          renderFailureDetails(failureDetails, offset)
    }

  def renderFailureCase(failureCase: FailureCase, offset: Int, isNested: Boolean = false): Chunk[Line] =
    failureCase match {
      case FailureCase(errorMessage, _, _, path, _, _, _) if isNested =>
        val errorMessageLines =
          Chunk.fromIterable(errorMessage.lines) match {
            case head +: tail => (error("• ") +: head) +: tail.map(error("  ") +: _)
            case _            => Chunk.empty
          }

        errorMessageLines ++
          Chunk.fromIterable(path.drop(path.length - 1).map { case (label, value) =>
            dim(s"$label = ") + primary(value.toString)
          })

      case FailureCase(errorMessage, codeString, location, path, _, nested, _) =>
        val errorMessageLines =
          Chunk.fromIterable(errorMessage.lines) match {
            case head +: tail => (error("• ") +: head) +: tail.map(error("  ") +: _)
            case _            => Chunk.empty
          }

        errorMessageLines ++ Chunk(Line.fromString(codeString, offset)) ++ (nested
          .flatMap(renderFailureCase(_, offset, true))
          .map(_.withOffset(offset + tabSize)) ++
          Chunk.fromIterable(path.map { case (label, value) => dim(s"$label = ") + primary(value.toString) }) ++
          Chunk(detail(s"☛ $location").toLine))
    }

  private def renderAssertionFailureDetails(failureDetails: ::[AssertionValue], offset: Int): Message = {
    @tailrec
    def loop(failureDetails: List[AssertionValue], rendered: Message): Message =
      failureDetails match {
        case fragment :: whole :: failureDetails =>
          loop(whole :: failureDetails, rendered :+ renderWhole(fragment, whole, offset))
        case _ =>
          rendered
      }

    renderFragment(failureDetails.head, offset).toMessage ++ loop(
      failureDetails,
      Message.empty
    ) ++ renderAssertionLocation(failureDetails.last, offset)
  }

  private def renderAssertionLocation(av: AssertionValue, offset: Int) = av.sourceLocation.fold(Message()) { location =>
    detail(s"☛ $location").toLine
      .withOffset(offset + 2)
      .toMessage
  }

  private def renderSatisfied(assertionValue: AssertionValue): Fragment =
    if (assertionValue.result.isSuccess) Fragment(" satisfied ")
    else Fragment(" did not satisfy ")

  def renderCause(cause: Cause[Any], offset: Int): Message = {
    val defects = cause.defects
    val timeouts = defects.collect { case TestTimeoutException(message) =>
      Message(message)
    }
    val mockExceptions = defects.collect { case exception: MockException =>
        renderMockException(exception).map(withOffset(offset + 1))
    }
    val remaining =
      cause.stripSomeDefects {
        case TestTimeoutException(_) => true
        case _: MockException        => true
      }
    val prefix = if (timeouts.nonEmpty) {
      // In case of timeout we don't show the mock exceptions
      timeouts.foldLeft(Message.empty)(_ ++ _)
    } else {
      mockExceptions.foldLeft(Message.empty)(_ ++ _)
    }

    remaining match {
      case Some(remainingCause) =>
        prefix ++ Message(
          remainingCause.prettyPrint
            .split("\n")
            .map(s => withOffset(offset + 1)(Line.fromString(s)))
            .toVector
        )
      case None =>
        prefix
    }
  }

  private def renderMockException(exception: MockException): Message =
    exception match {
      case MockException.InvalidCallException(failures) =>
        val header = error(s"- could not find a matching expectation").toLine
        header +: renderUnmatchedExpectations(failures)

      case MockException.UnsatisfiedExpectationsException(expectation) =>
        val header = error(s"- unsatisfied expectations").toLine
        header +: renderUnsatisfiedExpectations(expectation)

      case MockException.UnexpectedCallException(method, args) =>
        Message(
          Seq(
            error(s"- unexpected call to $method with arguments").toLine,
            withOffset(1)(detail(args.toString).toLine)
          )
        )

      case MockException.InvalidRangeException(range) =>
        Message(
          Seq(
            error(s"- invalid repetition range ${range.start} to ${range.end} by ${range.step}").toLine
          )
        )
    }

  private def renderUnmatchedExpectations(failedMatches: List[InvalidCall]): Message =
    failedMatches.map {
      case InvalidCall.InvalidArguments(invoked, args, assertion) =>
        val header = error(s"- $invoked called with invalid arguments").toLine
        (header +: renderTestFailure("", assertImpl(args)(assertion)).drop(1)).withOffset(1)

      case InvalidCall.InvalidCapability(invoked, expected, assertion) =>
        Message(
          Seq(
            withOffset(1)(error(s"- invalid call to $invoked").toLine),
            withOffset(2)(
              Fragment(s"expected $expected with arguments ") + detail(assertion.toString)
            )
          )
        )

      case InvalidCall.InvalidPolyType(invoked, args, expected, assertion) =>
        Message(
          Seq(
            withOffset(1)(error(s"- $invoked called with arguments $args and invalid polymorphic type").toLine),
            withOffset(2)(
              Fragment(s"expected $expected with arguments ") + detail(assertion.toString)
            )
          )
        )
    }.reverse.foldLeft(Message.empty)(_ ++ _)

  private def renderUnsatisfiedExpectations[R <: Has[_]](expectation: Expectation[R]): Message = {

    @tailrec
    def loop(stack: List[(Int, Expectation[R])], lines: Vector[Line]): Vector[Line] =
      stack match {
        case Nil =>
          lines

        case (ident, Expectation.And(children, state, _, _)) :: tail if state.isFailed =>
          val title       = Line.fromString("in any order", ident)
          val unsatisfied = children.filter(_.state.isFailed).map(ident + 1 -> _)
          loop(unsatisfied ++ tail, lines :+ title)

        case (ident, Expectation.Call(method, assertion, _, state, _)) :: tail if state.isFailed =>
          val rendered =
            withOffset(ident)(Fragment(s"$method with arguments ") + detail(assertion.toString))
          loop(tail, lines :+ rendered)

        case (ident, Expectation.Chain(children, state, _, _)) :: tail if state.isFailed =>
          val title       = Line.fromString("in sequential order", ident)
          val unsatisfied = children.filter(_.state.isFailed).map(ident + 1 -> _)
          loop(unsatisfied ++ tail, lines :+ title)

        case (ident, Expectation.Or(children, state, _, _)) :: tail if state.isFailed =>
          val title       = Line.fromString("one of", ident)
          val unsatisfied = children.map(ident + 1 -> _)
          loop(unsatisfied ++ tail, lines :+ title)

        case (ident, Expectation.Repeated(child, range, state, _, _, completed)) :: tail if state.isFailed =>
          val min = Try(range.min.toString).getOrElse("0")
          val max = Try(range.max.toString).getOrElse("∞")
          val title =
            Line.fromString(
              s"repeated $completed times not in range $min to $max by ${range.step}",
              ident
            )
          val unsatisfied = ident + 1 -> child
          loop(unsatisfied :: tail, lines :+ title)

        case _ :: tail =>
          loop(tail, lines)
      }

    val lines = loop(List(1 -> expectation), Vector.empty)
    Message(lines)
  }

  def renderTestFailure(label: String, testResult: TestResult): Message =
    testResult.failures.fold(Message.empty) { details =>
      Message {
        details
          .fold(assertionResult =>
            rendered(Test, label, Failed, 0, renderFailure(label, 0, assertionResult).lines: _*)
          )(
            _ && _,
            _ || _,
            !_
          )
          .lines
      }
    }

  private def renderFailure(label: String, offset: Int, details: AssertionResult): Message =
    renderFailureLabel(label, offset) +: renderAssertionResult(details, offset)

  def renderFailureLabel(label: String, offset: Int): Line =
    withOffset(offset)(error("- " + label).toLine)

  def renderFailureDetails(failureDetails: FailureDetails, offset: Int): Message =
    renderAssertionFailureDetails(failureDetails.assertion, offset)

  private def renderGenFailureDetails[A](failureDetails: Option[GenFailureDetails], offset: Int): Message =
    failureDetails match {
      case Some(details) =>
        val shrunken = details.shrunkenInput.toString
        val initial  = details.initialInput.toString
        val renderShrunken = withOffset(offset + 1)(
          Fragment(
            s"Test failed after ${details.iterations + 1} iteration${if (details.iterations > 0) "s" else ""} with input: "
          ) +
            error(shrunken)
        )
        if (initial == shrunken) renderShrunken.toMessage
        else
          renderShrunken + withOffset(offset + 1)(
            Fragment(s"Original input before shrinking was: ") + error(initial)
          )
      case None => Message.empty
    }

  private def renderFragment(fragment: AssertionValue, offset: Int): Line =
    withOffset(offset + 1) {
      primary(renderValue(fragment)) +
        renderSatisfied(fragment) +
        detail(fragment.printAssertion)
    }

  private def renderWhole(fragment: AssertionValue, whole: AssertionValue, offset: Int): Line =
    withOffset(offset + 1) {
      primary(renderValue(whole)) +
        renderSatisfied(whole) ++
        highlight(detail(whole.printAssertion), fragment.printAssertion)
    }

  private def highlight(fragment: Fragment, substring: String, style: Fragment.Style = Fragment.Style.Warning): Line = {
    val parts = fragment.text.split(Pattern.quote(substring))
    if (parts.size == 1) fragment.toLine
    else
      parts.foldLeft(Line.empty) { (line, part) =>
        if (line.fragments.size < parts.size * 2 - 2)
          line + Fragment(part, fragment.style) + Fragment(substring, style)
        else line + Fragment(part, fragment.style)
      }
  }

  private def renderValue(av: AssertionValue) = (av.value, av.expression) match {
    case (v, Some(expression)) if !expressionRedundant(v.toString, expression) => s"`$expression` = $v"
    case (v, _)                                                                => v.toString
  }

  private def expressionRedundant(valueStr: String, expression: String) = {
    // toString drops double quotes, and for tuples and collections doesn't include spaces after the comma
    def strip(s: String) = s
      .replace("\"", "")
      .replace(" ", "")
      .replace("\n", "")
      .replace("\\n", "")
    strip(valueStr) == strip(expression)
  }

  def rendered(
    caseType: ResultType,
    label: String,
    result: Status,
    offset: Int,
    lines: Line*
  ): ExecutionResult =
    ExecutionResult(caseType, label, result, offset, Nil, lines.toList)
}

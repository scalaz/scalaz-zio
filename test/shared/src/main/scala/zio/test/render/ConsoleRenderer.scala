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

package zio.test.render

import zio.test.render.ExecutionResult.{ResultType, Status}
import zio.test.render.LogLine.Fragment.Style
import zio.test.render.LogLine.{Fragment, Line, Message}
import zio.test.{ConsoleUtils, TestAnnotation, TestAnnotationMap, TestAnnotationRenderer}

trait ConsoleRenderer extends TestRenderer {
  private val tabSize = 2

  override def render(result: ExecutionResult, testAnnotationRenderer: TestAnnotationRenderer): String = {
    val message = Message(result.lines).intersperse(Line.fromString("\n"))

    val output = result.resultType match {
      case ResultType.Suite =>
        renderSuite(result.status, result.offset, message)
      case ResultType.Test =>
        renderTest(result.status, result.offset, message)
      case ResultType.Other =>
        Message(result.lines)
    }

    val renderedAnnotations = renderAnnotations(result.annotations, testAnnotationRenderer)
    renderToStringLines(output ++ renderedAnnotations).mkString
  }

  private def renderSuite(status: Status, offset: Int, message: Message): Message =
    status match {
      case Status.Passed => withOffset(offset)(info("+") + sp) +: message
      case Status.Failed => withOffset(offset)(Line.empty) +: message
      case Status.Ignored =>
        withOffset(offset)(Line.empty) +: message :+ fr(" - " + TestAnnotation.ignored.identifier + " suite").toLine
    }

  private def renderTest(status: Status, offset: Int, message: Message) =
    status match {
      case Status.Passed  => withOffset(offset)(info("+") + sp) +: message
      case Status.Ignored => withOffset(offset)(warn("-") + sp) +: message
      case Status.Failed  => message
    }

  def renderToStringLines(message: Message): Seq[String] = {
    def renderFragment(f: Fragment): String =
      f.style match {
        case Style.Default             => f.text
        case Style.Primary             => ConsoleUtils.blue(f.text)
        case Style.Warning             => ConsoleUtils.yellow(f.text)
        case Style.Error               => ConsoleUtils.red(f.text)
        case Style.Info                => ConsoleUtils.green(f.text)
        case Style.Detail              => ConsoleUtils.cyan(f.text)
        case Style.Dimmed              => ConsoleUtils.dim(f.text)
        case Style.Bold(fr)            => ConsoleUtils.bold(renderFragment(fr))
        case Style.Underlined(fr)      => ConsoleUtils.underlined(renderFragment(fr))
        case Style.Ansi(fr, ansiColor) => ConsoleUtils.ansi(ansiColor, renderFragment(fr))
      }

    message.lines.map { line =>
      renderOffset(line.offset)(line.optimized.fragments.foldLeft("")((str, f) => str + renderFragment(f)))
    }
  }

  private def renderAnnotations(
    annotations: List[TestAnnotationMap],
    annotationRenderer: TestAnnotationRenderer
  ): Message =
    annotations match {
      case annotations :: ancestors =>
        val rendered = annotationRenderer.run(ancestors, annotations)
        if (rendered.isEmpty) Message.empty
        else Message(rendered.mkString(" - ", ", ", ""))
      case Nil => Message.empty
    }

  private def renderOffset(n: Int)(s: String) =
    " " * (n * tabSize) + s
}
object ConsoleRenderer extends ConsoleRenderer

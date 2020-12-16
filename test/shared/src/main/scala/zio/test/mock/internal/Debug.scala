/*
 * Copyright 2017-2020 John A. De Goes and the ZIO Contributors
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

package zio.test.mock.internal

import zio.Has
import zio.test.mock.Expectation

private[mock] object Debug {

  /**
   * To see mock debug output during test execution, flip this flag to `true`.
   */
  final val enabled = false

  def debug(message: => String): Unit =
    if (enabled) println(message)

  def prettify[R <: Has[_]](expectation: Expectation[R], identSize: Int = 1): String = {
    val ident   = " " * 4 * identSize
    val state   = s"state = ${expectation.state}"
    val invoked = s"""invocations = [${expectation.invocations.mkString(", ")}]"""

    def renderRoot(name: String, children: List[Expectation[R]]): String = {
      val header    = (s"$name(" :: s"$state," :: s"$invoked," :: Nil).mkString(s"\n$ident")
      val content   = renderChildren(children).mkString("\n")
      val prevIdent = " " * 4 * (identSize - 1)
      s"$header,\n$content\n$prevIdent)"
    }

    def renderChildren(list: List[Expectation[R]]): List[String] =
      list.map { child =>
        val rendered = prettify(child, identSize + 1)
        s"$ident$rendered"
      }

    expectation match {
      case Expectation.Call(capability, assertion, _, _, _) =>
        s"Call($state, $invoked, $capability, $assertion)"
      case Expectation.And(children, _, _, _) =>
        renderRoot("And", children)
      case Expectation.Chain(children, _, _, _) =>
        renderRoot("Chain", children)
      case Expectation.Or(children, _, _, _) =>
        renderRoot("Or", children)
      case Expectation.NoCalls(_) => renderRoot("NoCalls", Nil)
      case Expectation.Repeated(child, range, _, _, started, completed) =>
        val progress = s"progress = $started out of $completed,"
        ("Repeated(" :: state :: s"range = $range," :: progress :: invoked :: prettify(child, 1) :: ")" :: Nil)
          .mkString(s"\n$ident")
    }
  }

  def prettify[R <: Has[_]](scopes: List[Scope[R]]): String =
    scopes.map { case Scope(expectation, id, _) =>
      val rendered = prettify(expectation)
      s">>>\nInvocation ID: $id\n$rendered"
    } match {
      case Nil         => ""
      case head :: Nil => s"[Head]:\n$head"
      case head :: tail =>
        val renderedTail = tail.mkString("\n")
        s"[Head]:\n$head\n[Tail]:\n$renderedTail"
    }
}

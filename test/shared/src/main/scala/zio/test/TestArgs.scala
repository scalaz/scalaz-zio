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

final case class TestArgs(
  testSearchTerms: List[String],
  tagSearchTerms: List[String],
  testTaskPolicy: Option[String],
  testRenderer: Option[String],
  printSummary: Boolean
)

object TestArgs {
  def empty: TestArgs = TestArgs(List.empty[String], List.empty[String], None, None, printSummary = true)

  def parse(args: Array[String]): TestArgs = {
    // TODO: Add a proper command-line parser
    val parsedArgs = args
      .sliding(2, 2)
      .collect {
        case Array("-t", term)        => ("testSearchTerm", term)
        case Array("-tags", term)     => ("tagSearchTerm", term)
        case Array("-policy", name)   => ("policy", name)
        case Array("-renderer", name) => ("renderer", name)
        case Array("-summary", flag)  => ("summary", flag)
      }
      .toList
      .groupBy(_._1)
      .map { case (k, v) =>
        (k, v.map(_._2))
      }

    val terms          = parsedArgs.getOrElse("testSearchTerm", Nil)
    val tags           = parsedArgs.getOrElse("tagSearchTerm", Nil)
    val testTaskPolicy = parsedArgs.getOrElse("policy", Nil).headOption
    val testRenderer   = parsedArgs.getOrElse("renderer", Nil).headOption.map(_.toLowerCase)
    val printSummary   = parsedArgs.getOrElse("summary", Nil).headOption.forall(_.toBoolean)
    TestArgs(terms, tags, testTaskPolicy, testRenderer, printSummary)
  }
}

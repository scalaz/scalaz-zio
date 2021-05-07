package zio.test.render
import zio.test.TestAnnotationRenderer.LeafRenderer
import zio.test.render.ExecutionResult.{ResultType, Status}
import zio.test.render.LogLine.Message
import zio.test.{TestAnnotation, TestAnnotationRenderer}

import scala.annotation.tailrec
import scala.util.Try

trait IntelliJRenderer extends TestRenderer {
  import IntelliJRenderer._

  override def render(results: Seq[ExecutionResult], testAnnotationRenderer: TestAnnotationRenderer): Seq[String] =
    mkTree(results).flatMap(renderTree)

  private def renderTree(t: Node[ExecutionResult]): List[String] =
    t.value.resultType match {
      case ResultType.Suite =>
        onSuiteStarted(t.value) +: t.children.flatMap(renderTree) :+ onSuiteFinished(t.value)
      case ResultType.Test =>
        onTestStarted(t.value) +: t.children.flatMap(renderTree) :+
          (t.value.status match {
            case Status.Passed  => onTestFinished(t.value, None)
            case Status.Failed  => onTestFailed(t.value)
            case Status.Ignored => onTestIgnored(t.value)
          })

      case ResultType.Other => Nil
    }

  private def onSuiteStarted(result: ExecutionResult) =
    tc(s"testSuiteStarted name='${escape(result.label)}'")

  private def onSuiteFinished(result: ExecutionResult) =
    tc(s"testSuiteFinished name='${escape(result.label)}'")

  private def onTestStarted(result: ExecutionResult) =
    tc(s"testStarted name='${escape(result.label)}' locationHint='${escape(location(result))}'")

  private def onTestFinished(result: ExecutionResult, timing: Option[String]) =
    tc(s"testFinished name='${escape(result.label)}' duration='${timing.getOrElse("")}'")

  private def onTestIgnored(result: ExecutionResult) =
    tc(s"testIgnored name='${escape(result.label)}'")

  private def onTestFailed(result: ExecutionResult) = {
    val message = Message(result.lines.drop(1)).withOffset(-result.offset)
    val error   = ConsoleRenderer.renderToStringLines(message).mkString("\n")

    tc(s"testFailed name='${escape(result.label)}' message='Assertion failed:' details='${escape(error)}'")
  }

  private def tc(message: String): String =
    s"##teamcity[$message]"

  def escape(str: String): String =
    str
      .replaceAll("[|]", "||")
      .replaceAll("[']", "|'")
      .replaceAll("[\n]", "|n")
      .replaceAll("[\r]", "|r")
      .replaceAll("]", "|]")
      .replaceAll("\\[", "|[")

  private def location(result: ExecutionResult) =
    result.annotations match {
      case annotations :: _ => locationRenderer.run(Nil, annotations).mkString
      case Nil              => ""
    }
}
object IntelliJRenderer extends IntelliJRenderer {
  private type Graph[A] = Map[Int, List[Node[A]]]

  val locationRenderer: TestAnnotationRenderer =
    LeafRenderer(TestAnnotation.location) { case child :: _ =>
      if (child.isEmpty) None
      else child.headOption.map(s => s"file://${s.path}:${s.line}")
    }

  private case class Node[A](value: A, children: List[Node[A]]) {
    def find(value: A): Option[Node[A]] = children.find(_.value == value)

    def replace(n: Node[A], r: Node[A]): List[Node[A]] = children.updated(children.indexOf(n), r)
  }

  private def mkTree(results: Seq[ExecutionResult]): List[Node[ExecutionResult]] = {
    def add(result: ExecutionResult)(graph: Graph[ExecutionResult]): Graph[ExecutionResult] = {
      @tailrec
      def buildGraph(id: Int, node: Node[ExecutionResult], graph: Graph[ExecutionResult]): Graph[ExecutionResult] =
        graph.get(id) match {
          case None => graph
          case Some(nodes) =>
            nodes match {
              case Nil => throw new Exception("Unexpected empty node")
              case _ =>
                val last  = nodes.last
                val child = last.find(node.value)
                val newNode = last.copy(children = child match {
                  case None    => last.children :+ node
                  case Some(c) => last.replace(c, node)
                })
                buildGraph(id - 1, newNode, graph.updated(id, nodes.init :+ newNode))
            }
        }

      val id = result.offset match {
        case 0                         => Try(graph.keys.max).getOrElse(0)
        case size if size > graph.size => graph.size
        case size                      => size
      }
      val node = Node(result, Nil)

      val g = graph.get(id) match {
        case None        => graph + (id -> List(node))
        case Some(nodes) => graph.updated(id, nodes :+ node)
      }
      buildGraph(id - 1, node, g)
    }

    results
      .foldLeft[Graph[ExecutionResult]](Map.empty) { case (g, res) =>
        add(res)(g)
      }
      .getOrElse(0, Nil)
  }
}

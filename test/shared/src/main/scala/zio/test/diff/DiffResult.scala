package zio.test.diff

import zio.test.PrettyPrint
import zio.test.diff.Diff.{dim, green, red}

sealed trait DiffResult {
  self =>

  import DiffResult._

  def noDiff: Boolean = !hasDiff

  def hasDiff: Boolean =
    self match {
      case Recursive(_, fields) => fields.exists(_._2.hasDiff)
      case Different(_, _, _)   => true
      case Removed(_)           => true
      case Added(_)             => true
      case Identical(_)         => false
    }

  def render: String = self match {
    case Recursive(label, fields) =>
      scala.Console.RESET +
        s"""
$label(
  ${fields
          .filter(_._2.hasDiff)
          .map {
            case (Some(label), diff) => dim(label + " = ") + DiffResult.indent(diff.render)
            case (None, diff)        => DiffResult.indent(diff.render)
          }
          .mkString(",\n  ")}
)
         """.trim
    case Different(_, _, Some(custom)) =>
      custom
    case Different(oldValue, newValue, None) =>
      s"${red(PrettyPrint(oldValue))} → ${green(PrettyPrint(newValue))}"
    case Removed(oldValue) =>
      red(PrettyPrint(oldValue))
    case Added(newValue) =>
      green(PrettyPrint(newValue))
    case Identical(value) =>
      PrettyPrint(value)
  }
}

object DiffResult {
  case class Recursive(label: String, fields: Seq[(Option[String], DiffResult)]) extends DiffResult

  case class Different(oldValue: Any, newValue: Any, customRender: Option[String] = None) extends DiffResult

  case class Removed(oldValue: Any) extends DiffResult

  case class Added(newValue: Any) extends DiffResult

  case class Identical(value: Any) extends DiffResult

  def indent(string: String): String =
    string.split("\n").toList match {
      case head :: tail =>
        (head +: tail.map("  " + _)).mkString("\n")
      case other => other.mkString("\n")
    }
}

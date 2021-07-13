package zio.test.diff

import zio.test.internal.myers.{Action, MyersDiff}
import zio.{Chunk, NonEmptyChunk}

import scala.collection.mutable.{ArrayBuffer, ListBuffer}

trait Diff[-A] {
  def diff(x: A, y: A): DiffResult

  def isLowPriority: Boolean = false
}

object Diff extends LowPriDiff {

  def render[A: Diff](oldValue: A, newValue: A): String =
    (oldValue diffed newValue).render

  implicit final class DiffOps[A](private val self: A)(implicit diff: Diff[A]) {
    def diffed(that: A): DiffResult = diff.diff(self, that)
  }

  def red[A](string: A): String   = scala.Console.RED + string + scala.Console.RESET
  def dim[A](string: A): String   = "\u001b[2m" + string + scala.Console.RESET
  def green[A](string: A): String = scala.Console.GREEN + string + scala.Console.RESET

  implicit def mapDiff[K, V: Diff]: Diff[Map[K, V]] = (x: Map[K, V], y: Map[K, V]) => {
    val fields =
      (x.keySet ++ y.keySet).toList.map { key =>
        (x.get(key), y.get(key)) match {
          case (Some(oldValue), Some(newValue)) =>
            Some(key.toString) -> (oldValue diffed newValue)
          case (Some(oldValue), None) =>
            Some(key.toString) -> DiffResult.Removed(oldValue)
          case (None, Some(newValue)) =>
            Some(key.toString) -> DiffResult.Added(newValue)
          case (None, None) =>
            throw new Error("THIS IS IMPOSSIBLE.")
        }
      }

    DiffResult.Recursive("Map", fields)
  }

  implicit def listDiff[A: Diff]: Diff[List[A]]               = mkSeqDiff("List")(identity(_))
  implicit def vectorDiff[A: Diff]: Diff[Vector[A]]           = mkSeqDiff("Vector")(identity(_))
  implicit def chunkDiff[A: Diff]: Diff[Chunk[A]]             = mkSeqDiff("Chunk")(identity(_))
  implicit def nonEmptyChunk[A: Diff]: Diff[NonEmptyChunk[A]] = mkSeqDiff("NonEmptyChunk")(identity(_))
  implicit def arrayDiff[A: Diff]: Diff[Array[A]]             = mkSeqDiff("Array")((_: Array[A]).toIndexedSeq)
  implicit def arrayBufferDiff[A: Diff]: Diff[ArrayBuffer[A]] =
    mkSeqDiff("ArrayBuffer")((_: ArrayBuffer[A]).toIndexedSeq)
  implicit def listBufferDiff[A: Diff]: Diff[ListBuffer[A]] = mkSeqDiff("ListBuffer")((_: ListBuffer[A]).toIndexedSeq)

  def mkSeqDiff[F[_], A: Diff](name: String)(f: F[A] => Seq[A]): Diff[F[A]] = (x0: F[A], y0: F[A]) => {
    val x                                          = f(x0)
    val y                                          = f(y0)
    def safeGet(list: Seq[A], idx: Int): Option[A] = list.lift(idx)

    val fields =
      (0 until (x.length max y.length)).map { idx =>
        (safeGet(x, idx), safeGet(y, idx)) match {
          case (Some(oldValue), Some(newValue)) =>
            Some(idx.toString) -> (oldValue diffed newValue)
          case (Some(oldValue), None) =>
            Some(idx.toString) -> DiffResult.Removed(oldValue)
          case (None, Some(newValue)) =>
            Some(idx.toString) -> DiffResult.Added(newValue)
          case (None, None) =>
            throw new Error("THIS IS IMPOSSIBLE.")
        }
      }

    DiffResult.Recursive(name, fields)
  }

  implicit def setDiff[A: Diff]: Diff[Set[A]] = (x: Set[A], y: Set[A]) => {
    val removed = x -- y
    val added   = y -- x

    val fields: List[(Option[String], DiffResult)] =
      (removed.map(None -> DiffResult.Removed(_)) ++ added.map(None -> DiffResult.Added(_))).toList

    DiffResult.Recursive("Set", fields)
  }

  implicit val stringDiff: Diff[String] = (x: String, y: String) => {
    val actions0 = MyersDiff.diff(x, y).actions

    val actions = actions0
      .foldLeft[Chunk[Action]](Chunk.empty) {
        case (Action.Delete(s1) +: tail, Action.Delete(s2)) =>
          Action.Delete(s1 + s2) +: tail
        case (Action.Insert(s1) +: tail, Action.Insert(s2)) =>
          Action.Insert(s1 + s2) +: tail
        case (Action.Keep(s1) +: tail, Action.Keep(s2)) =>
          Action.Keep(s1 + s2) +: tail
        case (acc, action) => action +: acc
      }
      .reverse

    val isIdentical = actions.count {
      case Action.Keep(_) => false
      case _              => true
    } == 0

    if (isIdentical) {
      DiffResult.Identical(x)
    } else if (
      actions0.count {
        case Action.Keep(_) => true
        case _              => false
      } < 30
    ) {
      DiffResult.Different(x, y)
    } else {
      val custom = scala.Console.RESET + actions.map {
        case Action.Delete(s) => red("-" + s)
        case Action.Insert(s) => green("+" + s)
        case Action.Keep(s)   => s
      }.mkString("")

      DiffResult.Different(x, y, Some(custom))
    }
  }

}

trait LowPriDiff {
  implicit val anyValDiff: Diff[AnyVal] = (x: AnyVal, y: AnyVal) =>
    if (x == y) DiffResult.Identical(x)
    else DiffResult.Different(x, y)
}

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

package zio.test.internal.myers

import zio.Chunk

sealed trait Action

object Action {

  final case class Delete(s: String) extends Action {
    override def toString: String = s"-$s"
  }

  final case class Insert(s: String) extends Action {
    override def toString: String = s"+$s"
  }

  final case class Keep(s: String) extends Action {
    override def toString: String = s
  }
}

final case class DiffResult(actions: Chunk[Action]) {
  def applyChanges(original: String): String =
    actions
      .foldLeft((new StringBuilder(original), 0)) { case ((s, index), action) =>
        action match {
          case Action.Delete(_) => (s.deleteCharAt(index), index)
          case Action.Keep(_)   => (s, index + 1)
          case Action.Insert(i) => (s.insert(index, i), index + 1)
        }
      }
      ._1
      .toString()

  def invert: DiffResult =
    DiffResult(
      actions.map {
        case Action.Delete(s) => Action.Insert(s)
        case Action.Insert(s) => Action.Delete(s)
        case k: Action.Keep   => k
      }
    )

  override def toString: String =
    actions
      .foldLeft(new StringBuilder()) { case (s, a) =>
        s.append(a)
      }
      .toString()

}

object MyersDiff {
  def diff(original: String, modified: String): DiffResult = {

    var varOriginal                    = original
    var varModified                    = modified
    var longestCommonSubstring: String = getLongestCommonSubsequence(original, modified)

    var actions: Chunk[Action] = Chunk.empty

    while (longestCommonSubstring.size > 0) {
      val headOfLongestCommonSubstring = longestCommonSubstring(0)
      longestCommonSubstring = longestCommonSubstring.drop(1)

      var headOfModified = varModified(0)
      var loop           = true

      while (loop) {
        headOfModified = varModified(0)
        varModified = varModified.drop(1)
        if (headOfModified != headOfLongestCommonSubstring)
          actions = actions :+ Action.Insert(headOfModified.toString)

        loop = varModified.size > 0 && headOfModified != headOfLongestCommonSubstring
      }

      var headOfOriginal = varOriginal(0)
      loop = true

      while (loop) {
        headOfOriginal = varOriginal(0)
        varOriginal = varOriginal.drop(1)
        if (headOfOriginal != headOfLongestCommonSubstring)
          actions = actions :+ Action.Delete(headOfOriginal.toString)

        loop = varOriginal.size > 0 && headOfOriginal != headOfLongestCommonSubstring
      }

      actions = actions :+ Action.Keep(headOfLongestCommonSubstring.toString)
    }

    while (varModified.size > 0) {
      val headOfModified = varModified(0)
      varModified = varModified.drop(1)
      actions = actions :+ Action.Insert(headOfModified.toString)
    }

    while (varOriginal.size > 0) {
      val headOfOriginal = varOriginal(0)
      varOriginal = varOriginal.drop(1)
      actions = actions :+ Action.Delete(headOfOriginal.toString)
    }

    DiffResult(actions)
  }

  def getLongestCommonSubsequence(original: String, modified: String): String =
    if (original == null || original.length() == 0 || modified == null || modified.length() == 0) ""
    else if (original == modified) original
    else {

      val myersMatrix: Array[Array[Int]] = initializeMyersMatrix(original, modified)
      val longestCommonSubsequence       = new StringBuilder()

      var originalPosition = original.length()
      var modifiedPosition = modified.length()

      var loop = true

      while (loop) {
        if (myersMatrix(originalPosition)(modifiedPosition) == myersMatrix(originalPosition - 1)(modifiedPosition)) {
          originalPosition -= 1
        } else if (
          myersMatrix(originalPosition)(modifiedPosition) == myersMatrix(originalPosition)(modifiedPosition - 1)
        ) {
          modifiedPosition -= 1
        } else {
          longestCommonSubsequence += original.charAt(originalPosition - 1)
          originalPosition -= 1
          modifiedPosition -= 1
        }

        loop = originalPosition > 0 && modifiedPosition > 0
      }

      longestCommonSubsequence.toString.reverse
    }

  private def initializeMyersMatrix(original: String, modified: String): Array[Array[Int]] = {
    val originalLength = original.length()
    val modifiedLength = modified.length()

    val myersMatrix = Array.fill[Int](originalLength + 1, modifiedLength + 1)(0)

    for (i <- 0 until originalLength) {
      for (j <- 0 until modifiedLength) {
        if (original.charAt(i) == modified.charAt(j)) {
          myersMatrix(i + 1)(j + 1) = myersMatrix(i)(j) + 1
        } else {
          if (myersMatrix(i)(j + 1) >= myersMatrix(i + 1)(j)) {
            myersMatrix(i + 1)(j + 1) = myersMatrix(i)(j + 1)
          } else {
            myersMatrix(i + 1)(j + 1) = myersMatrix(i + 1)(j)
          }
        }
      }
    }

    myersMatrix
  }
}

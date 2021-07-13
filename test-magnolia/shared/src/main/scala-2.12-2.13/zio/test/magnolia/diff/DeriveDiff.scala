package zio.test.magnolia.diff

import magnolia._
import zio.test.diff.{Diff, DiffResult}

trait DeriveDiff {
  type Typeclass[A] = Diff[A]

  def combine[T](caseClass: CaseClass[Typeclass, T]): Typeclass[T] =
    (x: T, y: T) => {
      val fields = caseClass.parameters.map { param =>
        Some(param.label) -> param.typeclass.diff(param.dereference(x), param.dereference(y))
      }

      DiffResult.Recursive(caseClass.typeName.short, fields)
    }

  implicit def gen[T]: Typeclass[T] = macro Magnolia.gen[T]
}

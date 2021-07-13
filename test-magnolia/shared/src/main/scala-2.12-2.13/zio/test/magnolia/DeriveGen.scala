/*
 * Copyright 2020-2021 John A. De Goes and the ZIO Contributors
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

package zio.test.magnolia

import _root_.zio.test.{Gen, Sized}
import magnolia._
import zio.random.Random

import java.time.{Instant, LocalDate, LocalDateTime}
import java.util.UUID

/**
 * A `DeriveGen[A]` can derive a generator of `A` values. Implicit instances of
 * DeriveGen are automatically provided for standard types and algebraic data
 * types made up of standard types. For example, here is how you can
 * automatically derive generators for case classes and sealed traits:
 *
 * {{{
 * final case class Point(x: Double, y: Double)
 *
 * val genPoint: Gen[Random with Sized, Point] = DeriveGen[Point]
 *
 * sealed trait Color
 * case object Red   extends Color
 * case object Green extends Color
 * case object Blue  extends Color
 *
 * val genColor: Gen[Random with Sized, Color] = DeriveGen[Color]
 * }}}
 *
 * You can derive generators that include your own custom types by providing an
 * implicit `DeriveGen` instance for your type using a generator and the
 * `instance` method.
 */
trait DeriveGen[A] {
  def derive: Gen[Random with Sized, A]
}

object DeriveGen {

  /**
   * Derives a generator of `A` values given an implicit `DeriveGen` instance.
   */
  def apply[A](implicit ev: DeriveGen[A]): Gen[Random with Sized, A] = ev.derive

  /**
   * Constructs a `DeriveGen` instance given a generator of `A` values.
   */
  def instance[A](gen: Gen[Random with Sized, A]): DeriveGen[A] =
    new DeriveGen[A] {
      val derive: Gen[Random with Sized, A] = gen
    }

  implicit val genBoolean: DeriveGen[Boolean]             = instance(Gen.boolean)
  implicit val genByte: DeriveGen[Byte]                   = instance(Gen.anyByte)
  implicit val genChar: DeriveGen[Char]                   = instance(Gen.anyChar)
  implicit val genDouble: DeriveGen[Double]               = instance(Gen.anyDouble)
  implicit val genFloat: DeriveGen[Float]                 = instance(Gen.anyFloat)
  implicit val genInt: DeriveGen[Int]                     = instance(Gen.anyInt)
  implicit val genLong: DeriveGen[Long]                   = instance(Gen.anyLong)
  implicit val genShort: DeriveGen[Short]                 = instance(Gen.anyShort)
  implicit val genString: DeriveGen[String]               = instance(Gen.anyString)
  implicit val genUnit: DeriveGen[Unit]                   = instance(Gen.unit)
  implicit val genUUID: DeriveGen[UUID]                   = instance(Gen.anyUUID)
  implicit val genInstant: DeriveGen[Instant]             = instance(Gen.anyInstant)
  implicit val genLocalDateTime: DeriveGen[LocalDateTime] = instance(Gen.anyLocalDateTime)
  implicit val genLocalDate: DeriveGen[LocalDate]         = instance(Gen.anyLocalDateTime.map(_.toLocalDate()))
  implicit val genBigDecimal: DeriveGen[BigDecimal] = instance(
    Gen.bigDecimal(
      BigDecimal(Double.MinValue) * BigDecimal(Double.MaxValue),
      BigDecimal(Double.MaxValue) * BigDecimal(Double.MaxValue)
    )
  )

  implicit def genEither[A, B](implicit ev1: DeriveGen[A], ev2: DeriveGen[B]): DeriveGen[Either[A, B]] =
    instance(Gen.either(ev1.derive, ev2.derive))

  implicit def genFunction[A, B](implicit ev: DeriveGen[B]): DeriveGen[A => B] =
    instance(Gen.function(ev.derive))

  implicit def genIterable[A](implicit ev: DeriveGen[A]): DeriveGen[Iterable[A]] =
    instance(Gen.oneOf(Gen.listOf(ev.derive), Gen.vectorOf(ev.derive), Gen.setOf(ev.derive)))

  implicit def genList[A](implicit ev: DeriveGen[A]): DeriveGen[List[A]] =
    instance(Gen.listOf(ev.derive))

  implicit def genMap[A, B](implicit ev1: DeriveGen[A], ev2: DeriveGen[B]): DeriveGen[Map[A, B]] =
    instance(Gen.mapOf(ev1.derive, ev2.derive))

  implicit def genOption[A](implicit ev: DeriveGen[A]): DeriveGen[Option[A]] =
    instance(Gen.option(ev.derive))

  implicit def genSeq[A](implicit ev: DeriveGen[A]): DeriveGen[Seq[A]] =
    instance(Gen.oneOf(Gen.listOf(ev.derive), Gen.vectorOf(ev.derive)))

  implicit def genPartialFunction[A, B](implicit ev: DeriveGen[B]): DeriveGen[PartialFunction[A, B]] =
    instance(Gen.partialFunction(ev.derive))

  implicit def genSet[A](implicit ev: DeriveGen[A]): DeriveGen[Set[A]] =
    instance(Gen.setOf(ev.derive))

  implicit def genTuple2[A, B](implicit ev1: DeriveGen[A], ev2: DeriveGen[B]): DeriveGen[(A, B)] =
    instance(Gen.zipN(ev1.derive, ev2.derive)((_, _)))

  implicit def genTuple3[A, B, C](implicit
    ev1: DeriveGen[A],
    ev2: DeriveGen[B],
    ev3: DeriveGen[C]
  ): DeriveGen[(A, B, C)] =
    instance(Gen.zipN(ev1.derive, ev2.derive, ev3.derive)((_, _, _)))

  implicit def genTuple4[A, B, C, D](implicit
    ev1: DeriveGen[A],
    ev2: DeriveGen[B],
    ev3: DeriveGen[C],
    ev4: DeriveGen[D]
  ): DeriveGen[(A, B, C, D)] =
    instance(Gen.zipN(ev1.derive, ev2.derive, ev3.derive, ev4.derive)((_, _, _, _)))

  implicit def genTuple5[A, B, C, D, F](implicit
    ev1: DeriveGen[A],
    ev2: DeriveGen[B],
    ev3: DeriveGen[C],
    ev4: DeriveGen[D],
    ev5: DeriveGen[F]
  ): DeriveGen[(A, B, C, D, F)] =
    instance(Gen.zipN(ev1.derive, ev2.derive, ev3.derive, ev4.derive, ev5.derive)((_, _, _, _, _)))

  implicit def genTuple6[A, B, C, D, F, G](implicit
    ev1: DeriveGen[A],
    ev2: DeriveGen[B],
    ev3: DeriveGen[C],
    ev4: DeriveGen[D],
    ev5: DeriveGen[F],
    ev6: DeriveGen[G]
  ): DeriveGen[(A, B, C, D, F, G)] =
    instance(Gen.zipN(ev1.derive, ev2.derive, ev3.derive, ev4.derive, ev5.derive, ev6.derive)((_, _, _, _, _, _)))

  implicit def genTuple7[A, B, C, D, F, G, H](implicit
    ev1: DeriveGen[A],
    ev2: DeriveGen[B],
    ev3: DeriveGen[C],
    ev4: DeriveGen[D],
    ev5: DeriveGen[F],
    ev6: DeriveGen[G],
    ev7: DeriveGen[H]
  ): DeriveGen[(A, B, C, D, F, G, H)] =
    instance(
      Gen.zipN(ev1.derive, ev2.derive, ev3.derive, ev4.derive, ev5.derive, ev6.derive, ev7.derive)(
        (_, _, _, _, _, _, _)
      )
    )

  implicit def genTuple8[A, B, C, D, F, G, H, I](implicit
    ev1: DeriveGen[A],
    ev2: DeriveGen[B],
    ev3: DeriveGen[C],
    ev4: DeriveGen[D],
    ev5: DeriveGen[F],
    ev6: DeriveGen[G],
    ev7: DeriveGen[H],
    ev8: DeriveGen[I]
  ): DeriveGen[(A, B, C, D, F, G, H, I)] =
    instance(
      Gen.zipN(ev1.derive, ev2.derive, ev3.derive, ev4.derive, ev5.derive, ev6.derive, ev7.derive, ev8.derive)(
        (_, _, _, _, _, _, _, _)
      )
    )

  implicit def genTuple9[A, B, C, D, F, G, H, I, J](implicit
    ev1: DeriveGen[A],
    ev2: DeriveGen[B],
    ev3: DeriveGen[C],
    ev4: DeriveGen[D],
    ev5: DeriveGen[F],
    ev6: DeriveGen[G],
    ev7: DeriveGen[H],
    ev8: DeriveGen[I],
    ev9: DeriveGen[J]
  ): DeriveGen[(A, B, C, D, F, G, H, I, J)] =
    instance(
      Gen.zipN(
        ev1.derive,
        ev2.derive,
        ev3.derive,
        ev4.derive,
        ev5.derive,
        ev6.derive,
        ev7.derive,
        ev8.derive,
        ev9.derive
      )((_, _, _, _, _, _, _, _, _))
    )

  implicit def genTuple10[A, B, C, D, F, G, H, I, J, K](implicit
    ev1: DeriveGen[A],
    ev2: DeriveGen[B],
    ev3: DeriveGen[C],
    ev4: DeriveGen[D],
    ev5: DeriveGen[F],
    ev6: DeriveGen[G],
    ev7: DeriveGen[H],
    ev8: DeriveGen[I],
    ev9: DeriveGen[J],
    ev10: DeriveGen[K]
  ): DeriveGen[(A, B, C, D, F, G, H, I, J, K)] =
    instance(
      Gen.zipN(
        ev1.derive,
        ev2.derive,
        ev3.derive,
        ev4.derive,
        ev5.derive,
        ev6.derive,
        ev7.derive,
        ev8.derive,
        ev9.derive,
        ev10.derive
      )((_, _, _, _, _, _, _, _, _, _))
    )

  implicit def genVector[A](implicit ev: DeriveGen[A]): DeriveGen[Vector[A]] =
    instance(Gen.vectorOf(ev.derive))

  type Typeclass[T] = DeriveGen[T]

  def combine[T](caseClass: CaseClass[Typeclass, T]): Typeclass[T] =
    instance(Gen.suspend(Gen.zipAll(caseClass.parameters.map(_.typeclass.derive)).map(caseClass.rawConstruct)))

  def dispatch[T](sealedTrait: SealedTrait[Typeclass, T]): Typeclass[T] =
    instance(Gen.suspend(Gen.oneOf(sealedTrait.subtypes.map(_.typeclass.derive): _*)))

  implicit def gen[T]: Typeclass[T] = macro Magnolia.gen[T]
}

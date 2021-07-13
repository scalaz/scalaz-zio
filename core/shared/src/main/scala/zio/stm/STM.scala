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

package zio.stm

import zio.{BuildFrom, CanFail, Fiber, IO, NonEmptyChunk}

import scala.util.Try

object STM {

  /**
   * @see See [[zio.stm.ZSTM.absolve]]
   */
  def absolve[E, A](e: STM[E, Either[E, A]]): STM[E, A] =
    ZSTM.absolve(e)

  /**
   * @see See [[zio.stm.ZSTM.atomically]]
   */
  def atomically[E, A](stm: STM[E, A]): IO[E, A] =
    ZSTM.atomically(stm)

  /**
   * @see See [[zio.stm.ZSTM.check]]
   */
  def check(p: => Boolean): USTM[Unit] = ZSTM.check(p)

  /**
   * @see See [[[zio.stm.ZSTM.collectAll[R,E,A,Collection[+Element]<:Iterable[Element]]*]]]
   */
  def collectAll[E, A, Collection[+Element] <: Iterable[Element]](
    in: Collection[STM[E, A]]
  )(implicit bf: BuildFrom[Collection[STM[E, A]], A, Collection[A]]): STM[E, Collection[A]] =
    ZSTM.collectAll(in)

  /**
   * @see See [[[zio.stm.ZSTM.collectAll[R,E,A](in:Set*]]]
   */
  def collectAll[E, A](in: Set[STM[E, A]]): STM[E, Set[A]] =
    ZSTM.collectAll(in)

  /**
   * @see See [[zio.stm.ZSTM.collectAll_]]
   */
  def collectAll_[E, A](in: Iterable[STM[E, A]]): STM[E, Unit] =
    ZSTM.collectAll_(in)

  /**
   * @see See [[zio.stm.ZSTM.collectFirst]]
   */
  def collectFirst[E, A, B](as: Iterable[A])(f: A => STM[E, Option[B]]): STM[E, Option[B]] =
    ZSTM.collectFirst(as)(f)

  /**
   * @see See [[zio.stm.ZSTM.cond]]
   */
  def cond[E, A](predicate: Boolean, result: => A, error: => E): STM[E, A] =
    ZSTM.cond(predicate, result, error)

  /**
   * @see See [[zio.stm.ZSTM.die]]
   */
  def die(t: => Throwable): USTM[Nothing] =
    ZSTM.die(t)

  /**
   * @see See [[zio.stm.ZSTM.dieMessage]]
   */
  def dieMessage(m: => String): USTM[Nothing] =
    ZSTM.dieMessage(m)

  /**
   * @see See [[zio.stm.ZSTM.done]]
   */
  def done[E, A](exit: => ZSTM.internal.TExit[E, A]): STM[E, A] =
    ZSTM.done(exit)

  /**
   * @see See [[zio.stm.ZSTM.done]]
   */
  def exists[E, A](as: Iterable[A])(f: A => STM[E, Boolean]): STM[E, Boolean] =
    ZSTM.exists(as)(f)

  /**
   * @see See [[zio.stm.ZSTM.fail]]
   */
  def fail[E](e: => E): STM[E, Nothing] =
    ZSTM.fail(e)

  /**
   * @see See [[zio.stm.ZSTM.fiberId]]
   */
  val fiberId: USTM[Fiber.Id] =
    ZSTM.fiberId

  /**
   * @see [[zio.stm.ZSTM.filter[R,E,A,Collection*]]
   */
  def filter[E, A, Collection[+Element] <: Iterable[Element]](
    as: Collection[A]
  )(f: A => STM[E, Boolean])(implicit bf: BuildFrom[Collection[A], A, Collection[A]]): STM[E, Collection[A]] =
    ZSTM.filter(as)(f)

  /**
   * @see [[[zio.stm.ZSTM.filter[R,E,A](as:Set*]]]
   */
  def filter[E, A](as: Set[A])(f: A => STM[E, Boolean]): STM[E, Set[A]] =
    ZSTM.filter(as)(f)

  /**
   * @see [[zio.stm.ZSTM.filterNot[R,E,A,Collection*]]
   */
  def filterNot[E, A, Collection[+Element] <: Iterable[Element]](
    as: Collection[A]
  )(f: A => STM[E, Boolean])(implicit bf: BuildFrom[Collection[A], A, Collection[A]]): STM[E, Collection[A]] =
    ZSTM.filterNot(as)(f)

  /**
   * @see [[[zio.stm.ZSTM.filterNot[R,E,A](as:Set*]]]
   */
  def filterNot[E, A](as: Set[A])(f: A => STM[E, Boolean]): STM[E, Set[A]] =
    ZSTM.filterNot(as)(f)

  /**
   * @see See [[zio.stm.ZSTM.flatten]]
   */
  def flatten[E, A](task: STM[E, STM[E, A]]): STM[E, A] =
    ZSTM.flatten(task)

  /**
   * @see See [[zio.stm.ZSTM.foldLeft]]
   */
  def foldLeft[E, S, A](in: Iterable[A])(zero: S)(f: (S, A) => STM[E, S]): STM[E, S] =
    ZSTM.foldLeft(in)(zero)(f)

  /**
   * @see See [[zio.stm.ZSTM.foldRight]]
   */
  def foldRight[E, S, A](in: Iterable[A])(zero: S)(f: (A, S) => STM[E, S]): STM[E, S] =
    ZSTM.foldRight(in)(zero)(f)

  /**
   * @see See [[zio.stm.ZSTM.forall]]
   */
  def forall[R, E, A](as: Iterable[A])(f: A => ZSTM[R, E, Boolean]): ZSTM[R, E, Boolean] =
    ZSTM.forall(as)(f)

  /**
   * @see See [[[zio.stm.ZSTM.foreach[R,E,A,B,Collection[+Element]<:Iterable[Element]]*]]]
   */
  def foreach[E, A, B, Collection[+Element] <: Iterable[Element]](
    in: Collection[A]
  )(f: A => STM[E, B])(implicit bf: BuildFrom[Collection[A], B, Collection[B]]): STM[E, Collection[B]] =
    ZSTM.foreach(in)(f)

  /**
   * @see See [[[zio.stm.ZSTM.foreach[R,E,A,B](in:Set*]]]
   */
  def foreach[E, A, B](in: Set[A])(f: A => STM[E, B]): STM[E, Set[B]] =
    ZSTM.foreach(in)(f)

  /**
   * @see See [[zio.stm.ZSTM.foreach_]]
   */
  def foreach_[E, A](in: Iterable[A])(f: A => STM[E, Any]): STM[E, Unit] =
    ZSTM.foreach_(in)(f)

  /**
   * @see See [[zio.stm.ZSTM.fromEither]]
   */
  def fromEither[E, A](e: => Either[E, A]): STM[E, A] =
    ZSTM.fromEither(e)

  /**
   * @see See [[zio.stm.ZSTM.fromFunction]]
   */
  def fromFunction[A](f: Any => A): USTM[A] =
    ZSTM.fromFunction(f)

  /**
   * @see See [[zio.stm.ZSTM.fromFunctionM]]
   */
  def fromFunctionM[E, A](f: Any => STM[E, A]): STM[E, A] =
    ZSTM.fromFunctionM(f)

  /**
   * @see See [[zio.stm.ZSTM.fromOption]]
   */
  def fromOption[A](v: => Option[A]): STM[Option[Nothing], A] =
    ZSTM.fromOption(v)

  /**
   * @see See [[zio.stm.ZSTM.fromTry]]
   */
  def fromTry[A](a: => Try[A]): TaskSTM[A] =
    ZSTM.fromTry(a)

  /**
   * @see See [[zio.stm.ZSTM.identity]]
   */
  def identity: USTM[Any] = ZSTM.identity

  /**
   * @see See [[zio.stm.ZSTM.ifM]]
   */
  def ifM[E](b: STM[E, Boolean]): ZSTM.IfM[Any, E] =
    ZSTM.ifM(b)

  /**
   * @see See [[zio.stm.ZSTM.ifF]]
   */
  def ifF[E](b: STM[E, Boolean]): ZSTM.IfF[Any, E] =
    ZSTM.ifF(b)

  /**
   * @see See [[zio.stm.ZSTM.iterate]]
   */
  def iterate[E, S](initial: S)(cont: S => Boolean)(body: S => STM[E, S]): STM[E, S] =
    ZSTM.iterate(initial)(cont)(body)

  /**
   * @see See [[zio.stm.ZSTM.left]]
   */
  def left[A](a: => A): USTM[Either[A, Nothing]] =
    ZSTM.left(a)

  /**
   * @see See [[zio.stm.ZSTM.loop]]
   */
  def loop[E, A, S](initial: S)(cont: S => Boolean, inc: S => S)(body: S => STM[E, A]): STM[E, List[A]] =
    ZSTM.loop(initial)(cont, inc)(body)

  /**
   * @see See [[zio.stm.ZSTM.loop_]]
   */
  def loop_[E, S](initial: S)(cont: S => Boolean, inc: S => S)(body: S => STM[E, Any]): STM[E, Unit] =
    ZSTM.loop_(initial)(cont, inc)(body)

  /**
   * @see See [[zio.stm.ZSTM.mapN[R,E,A,B,C]*]]
   */
  def mapN[E, A, B, C](tx1: STM[E, A], tx2: STM[E, B])(f: (A, B) => C): STM[E, C] =
    ZSTM.mapN(tx1, tx2)(f)

  /**
   * @see See [[zio.stm.ZSTM.mapN[R,E,A,B,C,D]*]]
   */
  def mapN[E, A, B, C, D](tx1: STM[E, A], tx2: STM[E, B], tx3: STM[E, C])(
    f: (A, B, C) => D
  ): STM[E, D] =
    ZSTM.mapN(tx1, tx2, tx3)(f)

  /**
   * @see See [[zio.stm.ZSTM.mapN[R,E,A,B,C,D,F]*]]
   */
  def mapN[E, A, B, C, D, F](tx1: STM[E, A], tx2: STM[E, B], tx3: STM[E, C], tx4: STM[E, D])(
    f: (A, B, C, D) => F
  ): STM[E, F] =
    ZSTM.mapN(tx1, tx2, tx3, tx4)(f)

  /**
   * @see See [[zio.stm.ZSTM.mergeAll]]
   */
  def mergeAll[E, A, B](
    in: Iterable[STM[E, A]]
  )(zero: B)(f: (B, A) => B): STM[E, B] = ZSTM.mergeAll(in)(zero)(f)

  /**
   * @see See [[zio.stm.ZSTM.none]]
   */
  val none: USTM[Option[Nothing]] = ZSTM.none

  /**
   * @see See [[zio.stm.ZSTM.partition]]
   */
  def partial[A](a: => A): STM[Throwable, A] =
    ZSTM.partial(a)

  /**
   * @see See [[zio.stm.ZSTM.partition]]
   */
  def partition[E, A, B](
    in: Iterable[A]
  )(f: A => STM[E, B])(implicit ev: CanFail[E]): STM[Nothing, (Iterable[E], Iterable[B])] =
    ZSTM.partition(in)(f)

  /**
   * @see See [[zio.stm.ZSTM.reduceAll]]
   */
  def reduceAll[E, A](a: STM[E, A], as: Iterable[STM[E, A]])(
    f: (A, A) => A
  ): STM[E, A] = ZSTM.reduceAll(a, as)(f)

  /**
   * @see See [[zio.stm.ZSTM.replicate]]
   */
  def replicate[E, A](n: Int)(tx: STM[E, A]): Iterable[STM[E, A]] =
    ZSTM.replicate(n)(tx)

  /**
   * @see See [[zio.stm.ZSTM.replicate]]
   */
  def replicateM[E, A](n: Int)(transaction: STM[E, A]): STM[E, Iterable[A]] =
    ZSTM.replicateM(n)(transaction)

  /**
   * @see See [[zio.stm.ZSTM.replicate]]
   */
  def replicateM_[E, A](n: Int)(transaction: STM[E, A]): STM[E, Unit] =
    ZSTM.replicateM_(n)(transaction)

  /**
   * @see See [[zio.stm.ZSTM.require]]
   */
  def require[E, A](error: => E): STM[E, Option[A]] => STM[E, A] =
    ZSTM.require[Any, E, A](error)

  /**
   * @see See [[zio.stm.ZSTM.retry]]
   */
  val retry: USTM[Nothing] =
    ZSTM.retry

  /**
   * @see See [[zio.stm.ZSTM.right]]
   */
  def right[A](a: => A): USTM[Either[Nothing, A]] =
    ZSTM.right(a)

  /**
   * @see See [[zio.stm.ZSTM.some]]
   */
  def some[A](a: => A): USTM[Option[A]] =
    ZSTM.some(a)

  /**
   * @see See [[zio.stm.ZSTM.succeed]]
   */
  def succeed[A](a: => A): USTM[A] =
    ZSTM.succeed(a)

  /**
   * @see See [[zio.stm.ZSTM.suspend]]
   */
  def suspend[E, A](stm: => STM[E, A]): STM[E, A] =
    ZSTM.suspend(stm)

  /**
   * @see See [[zio.stm.ZSTM.unit]]
   */
  val unit: USTM[Unit] =
    ZSTM.unit

  /**
   * @see See [[zio.stm.ZSTM.unless]]
   */
  def unless[E](b: => Boolean)(stm: => STM[E, Any]): STM[E, Unit] =
    ZSTM.unless(b)(stm)

  /**
   * @see See [[zio.stm.ZSTM.unlessM]]
   */
  def unlessM[E](b: STM[E, Boolean]): ZSTM.UnlessM[Any, E] =
    ZSTM.unlessM(b)

  /**
   * @see See [[[zio.stm.ZSTM.validate[R,E,A,B,Collection[+Element]<:Iterable[Element]]*]]]
   */
  def validate[E, A, B, Collection[+Element] <: Iterable[Element]](in: Collection[A])(
    f: A => STM[E, B]
  )(implicit bf: BuildFrom[Collection[A], B, Collection[B]], ev: CanFail[E]): STM[::[E], Collection[B]] =
    ZSTM.validate(in)(f)

  /**
   * @see See [[[zio.stm.ZSTM.validate[R,E,A,B](in:zio\.NonEmptyChunk*]]]
   */
  def validate[E, A, B](in: NonEmptyChunk[A])(
    f: A => STM[E, B]
  )(implicit ev: CanFail[E]): STM[::[E], NonEmptyChunk[B]] =
    ZSTM.validate(in)(f)

  /**
   * @see See [[zio.stm.ZSTM.validateFirst]]
   */
  def validateFirst[E, A, B, Collection[+Element] <: Iterable[Element]](
    in: Collection[A]
  )(f: A => STM[E, B])(implicit bf: BuildFrom[Collection[A], E, Collection[E]], ev: CanFail[E]): STM[Collection[E], B] =
    ZSTM.validateFirst(in)(f)

  /**
   * @see See [[zio.stm.ZSTM.when]]
   */
  def when[E](b: => Boolean)(stm: => STM[E, Any]): STM[E, Unit] = ZSTM.when(b)(stm)

  /**
   * @see See [[zio.stm.ZSTM.whenCase]]
   */
  def whenCase[E, A](a: => A)(pf: PartialFunction[A, STM[E, Any]]): STM[E, Unit] =
    ZSTM.whenCase(a)(pf)

  /**
   * @see See [[zio.stm.ZSTM.whenCaseM]]
   */
  def whenCaseM[E, A](a: STM[E, A])(pf: PartialFunction[A, STM[E, Any]]): STM[E, Unit] =
    ZSTM.whenCaseM(a)(pf)

  /**
   * @see See [[zio.stm.ZSTM.whenM]]
   */
  def whenM[E](b: STM[E, Boolean]): ZSTM.WhenM[Any, E] =
    ZSTM.whenM(b)

  private[zio] def succeedNow[A](a: A): USTM[A] =
    ZSTM.succeedNow(a)
}

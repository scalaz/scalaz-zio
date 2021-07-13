/*
 * Copyright 2017-2021 John A. De Goes and the ZIO Contributors
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

package zio

import zio.clock.Clock
import zio.duration.Duration
import zio.internal.{Executor, Platform}

import scala.reflect.ClassTag

object URIO {

  /**
   * @see [[zio.ZIO.absolve]]
   */
  def absolve[R, A](v: URIO[R, Either[Nothing, A]]): URIO[R, A] =
    ZIO.absolve(v)

  /**
   * @see [[zio.ZIO.access]]
   */
  def access[R]: ZIO.AccessPartiallyApplied[R] = ZIO.access[R]

  /**
   * @see [[zio.ZIO.accessM]]
   */
  def accessM[R]: ZIO.AccessMPartiallyApplied[R] = ZIO.accessM[R]

  /**
   * @see [[zio.ZIO.allowInterrupt]]
   */
  def allowInterrupt: UIO[Unit] = ZIO.allowInterrupt

  /**
   * @see [[zio.ZIO.apply]]
   */
  def apply[A](a: => A): UIO[A] = ZIO.effectTotal(a)

  /**
   * @see bracket in [[zio.ZIO]]
   */
  def bracket[R, A](acquire: URIO[R, A]): ZIO.BracketAcquire[R, Nothing, A] =
    ZIO.bracket(acquire)

  /**
   * @see bracket in [[zio.ZIO]]
   */
  def bracket[R, A, B](
    acquire: URIO[R, A],
    release: A => URIO[R, Any],
    use: A => URIO[R, B]
  ): URIO[R, B] = ZIO.bracket(acquire, release, use)

  /**
   * @see bracketExit in [[zio.ZIO]]
   */
  def bracketExit[R, A](acquire: URIO[R, A]): ZIO.BracketExitAcquire[R, Nothing, A] =
    ZIO.bracketExit(acquire)

  /**
   * @see bracketExit in [[zio.ZIO]]
   */
  def bracketExit[R, A, B](
    acquire: URIO[R, A],
    release: (A, Exit[Nothing, B]) => URIO[R, Any],
    use: A => URIO[R, B]
  ): URIO[R, B] = ZIO.bracketExit(acquire, release, use)

  /**
   * @see [[zio.ZIO.checkInterruptible]]
   */
  def checkInterruptible[R, A](f: InterruptStatus => URIO[R, A]): URIO[R, A] =
    ZIO.checkInterruptible(f)

  /**
   * @see [[zio.ZIO.checkTraced]]
   */
  def checkTraced[R, A](f: TracingStatus => URIO[R, A]): URIO[R, A] =
    ZIO.checkTraced(f)

  /**
   * @see See [[zio.ZIO.collect]]
   */
  def collect[R, A, B, Collection[+Element] <: Iterable[Element]](in: Collection[A])(
    f: A => ZIO[R, Option[Nothing], B]
  )(implicit bf: BuildFrom[Collection[A], B, Collection[B]]): URIO[R, Collection[B]] =
    ZIO.collect(in)(f)

  /**
   * @see See [[[zio.ZIO.collectAll[R,E,A,Collection[+Element]<:Iterable[Element]]*]]]
   */
  def collectAll[R, A, Collection[+Element] <: Iterable[Element]](
    in: Collection[URIO[R, A]]
  )(implicit bf: BuildFrom[Collection[URIO[R, A]], A, Collection[A]]): URIO[R, Collection[A]] =
    ZIO.collectAll(in)

  /**
   * @see See [[[zio.ZIO.collectAll[R,E,A](in:Set*]]]
   */
  def collectAll[R, A](in: Set[URIO[R, A]]): URIO[R, Set[A]] =
    ZIO.collectAll(in)

  /**
   * @see See [[[zio.ZIO.collectAll[R,E,A](in:Array*]]]
   */
  def collectAll[R, A: ClassTag](in: Array[URIO[R, A]]): URIO[R, Array[A]] =
    ZIO.collectAll(in)

  /**
   * @see See [[[zio.ZIO.collectAll[R,E,A](in:Option*]]]
   */
  def collectAll[R, A](in: Option[URIO[R, A]]): URIO[R, Option[A]] =
    ZIO.collectAll(in)

  /**
   * @see See [[[zio.ZIO.collectAll[R,E,A](in:zio\.NonEmptyChunk*]]]
   */
  def collectAll[R, A](in: NonEmptyChunk[URIO[R, A]]): URIO[R, NonEmptyChunk[A]] =
    ZIO.collectAll(in)

  /**
   * @see See [[[zio.ZIO.collectAll_[R,E,A](in:Iterable*]]]
   */
  def collectAll_[R, A](in: Iterable[URIO[R, A]]): URIO[R, Unit] =
    ZIO.collectAll_(in)

  /**
   * @see See [[[zio.ZIO.collectAllPar[R,E,A,Collection[+Element]<:Iterable[Element]]*]]]
   */
  def collectAllPar[R, A, Collection[+Element] <: Iterable[Element]](
    in: Collection[URIO[R, A]]
  )(implicit bf: BuildFrom[Collection[URIO[R, A]], A, Collection[A]]): URIO[R, Collection[A]] =
    ZIO.collectAllPar(in)

  /**
   * @see See [[[zio.ZIO.collectAllPar[R,E,A](as:Set*]]]
   */
  def collectAllPar[R, A](as: Set[URIO[R, A]]): URIO[R, Set[A]] =
    ZIO.collectAllPar(as)

  /**
   * @see See [[[zio.ZIO.collectAllPar[R,E,A](as:Array*]]]
   */
  def collectAllPar[R, A: ClassTag](as: Array[URIO[R, A]]): URIO[R, Array[A]] =
    ZIO.collectAllPar(as)

  /**
   * @see See [[[zio.ZIO.collectAllPar[R,E,A](as:zio\.NonEmptyChunk*]]]
   */
  def collectAllPar[R, A](as: NonEmptyChunk[URIO[R, A]]): URIO[R, NonEmptyChunk[A]] =
    ZIO.collectAllPar(as)

  /**
   * @see See [[[zio.ZIO.collectAllPar_[R,E,A](as:Iterable*]]]
   */
  def collectAllPar_[R, A](in: Iterable[URIO[R, A]]): URIO[R, Unit] =
    ZIO.collectAllPar_(in)

  /**
   * @see See [[zio.ZIO.collectAllParN]]
   */
  def collectAllParN[R, A, Collection[+Element] <: Iterable[Element]](n: Int)(
    as: Collection[URIO[R, A]]
  )(implicit bf: BuildFrom[Collection[URIO[R, A]], A, Collection[A]]): URIO[R, Collection[A]] =
    ZIO.collectAllParN(n)(as)

  /**
   * @see See [[zio.ZIO.collectAllParN_]]
   */
  def collectAllParN_[R, A](n: Int)(as: Iterable[URIO[R, A]]): URIO[R, Unit] =
    ZIO.collectAllParN_(n)(as)

  /**
   * @see [[zio.ZIO.collectAllSuccesses]]
   */
  def collectAllSuccesses[R, A, Collection[+Element] <: Iterable[Element]](
    in: Collection[URIO[R, A]]
  )(implicit bf: BuildFrom[Collection[URIO[R, A]], A, Collection[A]]): URIO[R, Collection[A]] =
    ZIO.collectAllSuccesses(in)

  /**
   * @see [[zio.ZIO.collectAllSuccessesPar]]
   */
  def collectAllSuccessesPar[R, A, Collection[+Element] <: Iterable[Element]](
    as: Collection[URIO[R, A]]
  )(implicit bf: BuildFrom[Collection[URIO[R, A]], A, Collection[A]]): URIO[R, Collection[A]] =
    ZIO.collectAllSuccessesPar(as)

  /**
   * @see [[zio.ZIO.collectAllSuccessesParN]]
   */
  def collectAllSuccessesParN[R, A, Collection[+Element] <: Iterable[Element]](n: Int)(
    as: Collection[URIO[R, A]]
  )(implicit bf: BuildFrom[Collection[URIO[R, A]], A, Collection[A]]): URIO[R, Collection[A]] =
    ZIO.collectAllSuccessesParN(n)(as)

  /**
   * @see [[zio.ZIO.collectAllWith]]
   */
  def collectAllWith[R, A, B, Collection[+Element] <: Iterable[Element]](in: Collection[URIO[R, A]])(
    f: PartialFunction[A, B]
  )(implicit bf: BuildFrom[Collection[URIO[R, A]], B, Collection[B]]): URIO[R, Collection[B]] =
    ZIO.collectAllWith(in)(f)

  /**
   * @see [[zio.ZIO.collectAllWithPar]]
   */
  def collectAllWithPar[R, A, B, Collection[+Element] <: Iterable[Element]](as: Collection[URIO[R, A]])(
    f: PartialFunction[A, B]
  )(implicit bf: BuildFrom[Collection[URIO[R, A]], B, Collection[B]]): URIO[R, Collection[B]] =
    ZIO.collectAllWithPar(as)(f)

  /**
   * @see [[zio.ZIO.collectAllWithParN]]
   */
  def collectAllWithParN[R, A, B, Collection[+Element] <: Iterable[Element]](n: Int)(as: Collection[URIO[R, A]])(
    f: PartialFunction[A, B]
  )(implicit bf: BuildFrom[Collection[URIO[R, A]], B, Collection[B]]): URIO[R, Collection[B]] =
    ZIO.collectAllWithParN(n)(as)(f)

  /**
   * @see See [[zio.ZIO.collectFirst]]
   */
  def collectFirst[R, A, B](as: Iterable[A])(f: A => URIO[R, Option[B]]): URIO[R, Option[B]] =
    ZIO.collectFirst(as)(f)

  /**
   * @see See [[zio.ZIO.collectPar]]
   */
  def collectPar[R, A, B, Collection[+Element] <: Iterable[Element]](in: Collection[A])(
    f: A => ZIO[R, Option[Nothing], B]
  )(implicit bf: BuildFrom[Collection[A], B, Collection[B]]): URIO[R, Collection[B]] =
    ZIO.collectPar(in)(f)

  /**
   * @see See [[zio.ZIO.collectParN]]
   */
  def collectParN[R, A, B, Collection[+Element] <: Iterable[Element]](n: Int)(in: Collection[A])(
    f: A => ZIO[R, Option[Nothing], B]
  )(implicit bf: BuildFrom[Collection[A], B, Collection[B]]): URIO[R, Collection[B]] =
    ZIO.collectParN(n)(in)(f)

  /**
   * @see See [[zio.ZIO.debug]]
   */
  def debug(value: Any): UIO[Unit] =
    ZIO.debug(value)

  /**
   * @see [[zio.ZIO.descriptor]]
   */
  def descriptor: UIO[Fiber.Descriptor] = ZIO.descriptor

  /**
   * @see [[zio.ZIO.descriptorWith]]
   */
  def descriptorWith[R, A](f: Fiber.Descriptor => URIO[R, A]): URIO[R, A] =
    ZIO.descriptorWith(f)

  /**
   * @see [[zio.ZIO.die]]
   */
  def die(t: => Throwable): UIO[Nothing] = ZIO.die(t)

  /**
   * @see [[zio.ZIO.dieMessage]]
   */
  def dieMessage(message: => String): UIO[Nothing] = ZIO.dieMessage(message)

  /**
   * @see [[zio.ZIO.done]]
   */
  def done[A](r: => Exit[Nothing, A]): UIO[A] = ZIO.done(r)

  /**
   * @see [[zio.ZIO.effectAsync]]
   */
  def effectAsync[R, A](register: (URIO[R, A] => Unit) => Any, blockingOn: List[Fiber.Id] = Nil): URIO[R, A] =
    ZIO.effectAsync(register, blockingOn)

  /**
   * @see [[zio.ZIO.effectAsyncMaybe]]
   */
  def effectAsyncMaybe[R, A](
    register: (URIO[R, A] => Unit) => Option[URIO[R, A]],
    blockingOn: List[Fiber.Id] = Nil
  ): URIO[R, A] =
    ZIO.effectAsyncMaybe(register, blockingOn)

  /**
   * @see [[zio.ZIO.effectAsyncM]]
   */
  def effectAsyncM[R, A](register: (URIO[R, A] => Unit) => URIO[R, Any]): URIO[R, A] =
    ZIO.effectAsyncM(register)

  /**
   * @see [[zio.ZIO.effectAsyncInterrupt]]
   */
  def effectAsyncInterrupt[R, A](
    register: (URIO[R, A] => Unit) => Either[Canceler[R], URIO[R, A]],
    blockingOn: List[Fiber.Id] = Nil
  ): URIO[R, A] =
    ZIO.effectAsyncInterrupt(register, blockingOn)

  /**
   * @see [[zio.ZIO.effectSuspendTotal]]
   */
  def effectSuspendTotal[R, A](rio: => URIO[R, A]): URIO[R, A] = ZIO.effectSuspendTotal(rio)

  /**
   * @see [[zio.ZIO.effectSuspendTotalWith]]
   */
  def effectSuspendTotalWith[R, A](p: (Platform, Fiber.Id) => URIO[R, A]): URIO[R, A] =
    ZIO.effectSuspendTotalWith(p)

  /**
   * @see [[zio.ZIO.effectTotal]]
   */
  def effectTotal[A](effect: => A): UIO[A] = ZIO.effectTotal(effect)

  /**
   * @see [[zio.ZIO.environment]]
   */
  def environment[R]: URIO[R, R] = ZIO.environment

  /**
   * @see See [[zio.ZIO.executor]]
   */
  def executor: UIO[Executor] =
    ZIO.executor

  /**
   * @see See [[zio.ZIO.exists]]
   */
  def exists[R, A](as: Iterable[A])(f: A => URIO[R, Boolean]): URIO[R, Boolean] =
    ZIO.exists(as)(f)

  /**
   * @see [[zio.ZIO.fiberId]]
   */
  val fiberId: UIO[Fiber.Id] = ZIO.fiberId

  /**
   * @see [[zio.ZIO.filter[R,E,A,Collection*]]
   */
  def filter[R, A, Collection[+Element] <: Iterable[Element]](
    as: Collection[A]
  )(f: A => URIO[R, Boolean])(implicit bf: BuildFrom[Collection[A], A, Collection[A]]): URIO[R, Collection[A]] =
    ZIO.filter(as)(f)

  /**
   * @see [[[zio.ZIO.filter[R,E,A](as:Set*]]]
   */
  def filter[R, A](as: Set[A])(f: A => URIO[R, Boolean]): URIO[R, Set[A]] =
    ZIO.filter(as)(f)

  /**
   * @see [[zio.ZIO.filterPar[R,E,A,Collection*]]
   */
  def filterPar[R, A, Collection[+Element] <: Iterable[Element]](
    as: Collection[A]
  )(f: A => URIO[R, Boolean])(implicit bf: BuildFrom[Collection[A], A, Collection[A]]): URIO[R, Collection[A]] =
    ZIO.filterPar(as)(f)

  /**
   * @see [[[zio.ZIO.filterPar[R,E,A](as:Set*]]]
   */
  def filterPar[R, A](as: Set[A])(f: A => URIO[R, Boolean]): URIO[R, Set[A]] =
    ZIO.filterPar(as)(f)

  /**
   * @see [[zio.ZIO.filterNot[R,E,A,Collection*]]
   */
  def filterNot[R, A, Collection[+Element] <: Iterable[Element]](
    as: Collection[A]
  )(f: A => URIO[R, Boolean])(implicit bf: BuildFrom[Collection[A], A, Collection[A]]): URIO[R, Collection[A]] =
    ZIO.filterNot(as)(f)

  /**
   * @see [[[zio.ZIO.filterNot[R,E,A](as:Set*]]]
   */
  def filterNot[R, A](as: Set[A])(f: A => URIO[R, Boolean]): URIO[R, Set[A]] =
    ZIO.filterNot(as)(f)

  /**
   * @see [[zio.ZIO.filterNotPar[R,E,A,Collection*]]
   */
  def filterNotPar[R, A, Collection[+Element] <: Iterable[Element]](
    as: Collection[A]
  )(f: A => URIO[R, Boolean])(implicit bf: BuildFrom[Collection[A], A, Collection[A]]): URIO[R, Collection[A]] =
    ZIO.filterNotPar(as)(f)

  /**
   * @see [[[zio.ZIO.filterNotPar[R,E,A](as:Set*]]]
   */
  def filterNotPar[R, A](as: Set[A])(f: A => URIO[R, Boolean]): URIO[R, Set[A]] =
    ZIO.filterNotPar(as)(f)

  /**
   * @see [[zio.ZIO.first]]
   */
  def first[A]: URIO[(A, Any), A] =
    ZIO.first

  /**
   * @see [[zio.ZIO.firstSuccessOf]]
   */
  def firstSuccessOf[R, A](
    rio: URIO[R, A],
    rest: Iterable[URIO[R, A]]
  ): URIO[R, A] = ZIO.firstSuccessOf(rio, rest)

  /**
   * @see [[zio.ZIO.flatten]]
   */
  def flatten[R, A](taskr: URIO[R, URIO[R, A]]): URIO[R, A] =
    ZIO.flatten(taskr)

  /**
   * @see [[zio.ZIO.foldLeft]]
   */
  def foldLeft[R, S, A](in: Iterable[A])(zero: S)(f: (S, A) => URIO[R, S]): URIO[R, S] =
    ZIO.foldLeft(in)(zero)(f)

  /**
   * @see [[zio.ZIO.foldRight]]
   */
  def foldRight[R, S, A](in: Iterable[A])(zero: S)(f: (A, S) => URIO[R, S]): URIO[R, S] =
    ZIO.foldRight(in)(zero)(f)

  /**
   * @see See [[zio.ZIO.forall]]
   */
  def forall[R, A](as: Iterable[A])(f: A => URIO[R, Boolean]): URIO[R, Boolean] =
    ZIO.forall(as)(f)

  /**
   * @see See [[[zio.ZIO.foreach[R,E,A,B,Collection[+Element]<:Iterable[Element]]*]]]
   */
  def foreach[R, A, B, Collection[+Element] <: Iterable[Element]](
    in: Collection[A]
  )(f: A => URIO[R, B])(implicit bf: BuildFrom[Collection[A], B, Collection[B]]): URIO[R, Collection[B]] =
    ZIO.foreach(in)(f)

  /**
   * @see [[[zio.ZIO.foreach[R,E,A,B](in:Set*]]]
   */
  final def foreach[R, A, B](in: Set[A])(f: A => URIO[R, B]): URIO[R, Set[B]] =
    ZIO.foreach(in)(f)

  /**
   * @see [[[zio.ZIO.foreach[R,E,A,B](in:Array*]]]
   */
  final def foreach[R, A, B: ClassTag](in: Array[A])(f: A => URIO[R, B]): URIO[R, Array[B]] =
    ZIO.foreach(in)(f)

  /**
   * @see See [[[zio.ZIO.foreach[R,E,Key,Key2,Value,Value2](map:Map*]]]
   */
  def foreach[R, Key, Key2, Value, Value2](
    map: Map[Key, Value]
  )(f: (Key, Value) => URIO[R, (Key2, Value2)]): URIO[R, Map[Key2, Value2]] =
    ZIO.foreach(map)(f)

  /**
   * @see [[[zio.ZIO.foreach[R,E,A,B](in:Option*]]]
   */
  def foreach[R, A, B](in: Option[A])(f: A => URIO[R, B]): URIO[R, Option[B]] =
    ZIO.foreach(in)(f)

  /**
   * @see [[[zio.ZIO.foreach[R,E,A,B](in:zio\.NonEmptyChunk*]]]
   */
  def foreach[R, A, B](in: NonEmptyChunk[A])(f: A => URIO[R, B]): URIO[R, NonEmptyChunk[B]] =
    ZIO.foreach(in)(f)

  /**
   * @see See [[zio.ZIO.foreachExec]]
   */
  final def foreachExec[R, A, B, Collection[+Element] <: Iterable[Element]](as: Collection[A])(
    exec: ExecutionStrategy
  )(f: A => URIO[R, B])(implicit bf: BuildFrom[Collection[A], B, Collection[B]]): URIO[R, Collection[B]] =
    ZIO.foreachExec(as)(exec)(f)

  /**
   * @see See [[[zio.ZIO.foreachPar[R,E,A,B,Collection[+Element]<:Iterable[Element]]*]]]
   */
  def foreachPar[R, A, B, Collection[+Element] <: Iterable[Element]](
    as: Collection[A]
  )(fn: A => URIO[R, B])(implicit bf: BuildFrom[Collection[A], B, Collection[B]]): URIO[R, Collection[B]] =
    ZIO.foreachPar(as)(fn)

  /**
   * @see [[[zio.ZIO.foreachPar[R,E,A,B](as:Set*]]]
   */
  final def foreachPar[R, A, B](as: Set[A])(fn: A => URIO[R, B]): URIO[R, Set[B]] =
    ZIO.foreachPar(as)(fn)

  /**
   * @see [[[zio.ZIO.foreachPar[R,E,A,B](as:Array*]]]
   */
  final def foreachPar[R, A, B: ClassTag](as: Array[A])(fn: A => URIO[R, B]): URIO[R, Array[B]] =
    ZIO.foreachPar(as)(fn)

  /**
   * @see See [[[zio.ZIO.foreachPar[R,E,Key,Key2,Value,Value2](map:Map*]]]
   */
  def foreachPar[R, Key, Key2, Value, Value2](
    map: Map[Key, Value]
  )(f: (Key, Value) => URIO[R, (Key2, Value2)]): URIO[R, Map[Key2, Value2]] =
    ZIO.foreachPar(map)(f)

  /**
   * @see [[[zio.ZIO.foreachPar[R,E,A,B](as:zio\.NonEmptyChunk*]]]
   */
  def foreachPar[R, A, B](as: NonEmptyChunk[A])(fn: A => URIO[R, B]): URIO[R, NonEmptyChunk[B]] =
    ZIO.foreachPar(as)(fn)

  /**
   * @see [[zio.ZIO.foreachParN]]
   */
  def foreachParN[R, A, B, Collection[+Element] <: Iterable[Element]](n: Int)(
    as: Collection[A]
  )(fn: A => URIO[R, B])(implicit bf: BuildFrom[Collection[A], B, Collection[B]]): URIO[R, Collection[B]] =
    ZIO.foreachParN(n)(as)(fn)

  /**
   * @see [[[zio.ZIO.foreach_[R,E,A](as:Iterable*]]]
   */
  def foreach_[R, A](as: Iterable[A])(f: A => URIO[R, Any]): URIO[R, Unit] =
    ZIO.foreach_(as)(f)

  /**
   * @see [[[zio.ZIO.foreachPar_[R,E,A](as:Iterable*]]]
   */
  def foreachPar_[R, A, B](as: Iterable[A])(f: A => URIO[R, Any]): URIO[R, Unit] =
    ZIO.foreachPar_(as)(f)

  /**
   * @see [[zio.ZIO.foreachParN_]]
   */
  def foreachParN_[R, A, B](n: Int)(as: Iterable[A])(f: A => URIO[R, Any]): URIO[R, Unit] =
    ZIO.foreachParN_(n)(as)(f)

  /**
   * @see [[zio.ZIO.forkAll]]
   */
  def forkAll[R, A, Collection[+Element] <: Iterable[Element]](
    as: Collection[URIO[R, A]]
  )(implicit bf: BuildFrom[Collection[URIO[R, A]], A, Collection[A]]): URIO[R, Fiber[Nothing, Collection[A]]] =
    ZIO.forkAll(as)

  /**
   * @see [[zio.ZIO.forkAll_]]
   */
  def forkAll_[R, A](as: Iterable[URIO[R, A]]): URIO[R, Unit] =
    ZIO.forkAll_(as)

  /**
   * @see [[zio.ZIO.fromEither]]
   */
  def fromEither[A](v: => Either[Nothing, A]): UIO[A] =
    ZIO.fromEither(v)

  /**
   * @see [[zio.ZIO.fromFiber]]
   */
  def fromFiber[A](fiber: => Fiber[Nothing, A]): UIO[A] =
    ZIO.fromFiber(fiber)

  /**
   * @see [[zio.ZIO.fromFiberM]]
   */
  def fromFiberM[A](fiber: UIO[Fiber[Nothing, A]]): UIO[A] =
    ZIO.fromFiberM(fiber)

  /**
   * @see [[zio.ZIO.fromFunction]]
   */
  def fromFunction[R, A](f: R => A): URIO[R, A] =
    ZIO.fromFunction(f)

  /**
   * @see [[zio.ZIO.fromFunctionM]]
   */
  def fromFunctionM[R, A](f: R => UIO[A]): URIO[R, A] =
    ZIO.fromFunctionM(f)

  /**
   * @see [[zio.ZIO.halt]]
   */
  def halt(cause: => Cause[Nothing]): UIO[Nothing] = ZIO.halt(cause)

  /**
   * @see [[zio.ZIO.haltWith]]
   */
  def haltWith[R](function: (() => ZTrace) => Cause[Nothing]): URIO[R, Nothing] =
    ZIO.haltWith(function)

  /**
   * @see [[zio.ZIO.identity]]
   */
  def identity[R]: URIO[R, R] = ZIO.identity

  /**
   * @see [[zio.ZIO.ifM]]
   */
  def ifM[R](b: URIO[R, Boolean]): ZIO.IfM[R, Nothing] =
    ZIO.ifM(b)

  /**
   * @see [[zio.ZIO.ifM]]
   */
  def ifF[R](b: URIO[R, Boolean]): ZIO.IfF[R, Nothing] =
    ZIO.ifF(b)

  /**
   * @see [[zio.ZIO.infinity]]
   */
  val infinity: URIO[Clock, Nothing] = ZIO.sleep(Duration.fromNanos(Long.MaxValue)) *> ZIO.never

  /**
   * @see [[zio.ZIO.interrupt]]
   */
  val interrupt: UIO[Nothing] = ZIO.interrupt

  /**
   * @see See [[zio.ZIO.interruptAs]]
   */
  def interruptAs(fiberId: => Fiber.Id): UIO[Nothing] = ZIO.interruptAs(fiberId)

  /**
   * @see [[zio.ZIO.interruptible]]
   */
  def interruptible[R, A](taskr: URIO[R, A]): URIO[R, A] =
    ZIO.interruptible(taskr)

  /**
   * @see [[zio.ZIO.interruptibleMask]]
   */
  def interruptibleMask[R, A](k: ZIO.InterruptStatusRestore => URIO[R, A]): URIO[R, A] =
    ZIO.interruptibleMask(k)

  /**
   * @see See [[zio.ZIO.iterate]]
   */
  def iterate[R, S](initial: S)(cont: S => Boolean)(body: S => URIO[R, S]): URIO[R, S] =
    ZIO.iterate(initial)(cont)(body)

  /**
   * @see [[zio.ZIO.lock]]
   */
  def lock[R, A](executor: => Executor)(taskr: URIO[R, A]): URIO[R, A] =
    ZIO.lock(executor)(taskr)

  /**
   *  @see See [[zio.ZIO.loop]]
   */
  def loop[R, A, S](initial: S)(cont: S => Boolean, inc: S => S)(body: S => URIO[R, A]): URIO[R, List[A]] =
    ZIO.loop(initial)(cont, inc)(body)

  /**
   *  @see See [[zio.ZIO.loop_]]
   */
  def loop_[R, S](initial: S)(cont: S => Boolean, inc: S => S)(body: S => URIO[R, Any]): URIO[R, Unit] =
    ZIO.loop_(initial)(cont, inc)(body)

  /**
   *  @see [[zio.ZIO.left]]
   */
  def left[R, A](a: => A): URIO[R, Either[A, Nothing]] = ZIO.left(a)

  /**
   *  @see [[zio.ZIO.mapN[R,E,A,B,C]*]]
   */
  def mapN[R, A, B, C](urio1: URIO[R, A], urio2: URIO[R, B])(f: (A, B) => C): URIO[R, C] =
    ZIO.mapN(urio1, urio2)(f)

  /**
   *  @see [[zio.ZIO.mapN[R,E,A,B,C,D]*]]
   */
  def mapN[R, A, B, C, D](urio1: URIO[R, A], urio2: URIO[R, B], urio3: URIO[R, C])(
    f: (A, B, C) => D
  ): URIO[R, D] =
    ZIO.mapN(urio1, urio2, urio3)(f)

  /**
   *  @see [[zio.ZIO.mapN[R,E,A,B,C,D,F]*]]
   */
  def mapN[R, A, B, C, D, F](urio1: URIO[R, A], urio2: URIO[R, B], urio3: URIO[R, C], urio4: URIO[R, D])(
    f: (A, B, C, D) => F
  ): URIO[R, F] =
    ZIO.mapN(urio1, urio2, urio3, urio4)(f)

  /**
   *  @see [[zio.ZIO.mapParN[R,E,A,B,C]*]]
   */
  def mapParN[R, A, B, C](urio1: URIO[R, A], urio2: URIO[R, B])(f: (A, B) => C): URIO[R, C] =
    ZIO.mapParN(urio1, urio2)(f)

  /**
   *  @see [[zio.ZIO.mapParN[R,E,A,B,C,D]*]]
   */
  def mapParN[R, A, B, C, D](urio1: URIO[R, A], urio2: URIO[R, B], urio3: URIO[R, C])(
    f: (A, B, C) => D
  ): URIO[R, D] =
    ZIO.mapParN(urio1, urio2, urio3)(f)

  /**
   *  @see [[zio.ZIO.mapParN[R,E,A,B,C,D,F]*]]
   */
  def mapParN[R, A, B, C, D, F](urio1: URIO[R, A], urio2: URIO[R, B], urio3: URIO[R, C], urio4: URIO[R, D])(
    f: (A, B, C, D) => F
  ): URIO[R, F] =
    ZIO.mapParN(urio1, urio2, urio3, urio4)(f)

  /**
   * @see See [[zio.ZIO.memoize]]
   */
  def memoize[R, A, B](f: A => URIO[R, B]): UIO[A => URIO[R, B]] =
    ZIO.memoize(f)

  /**
   * @see [[zio.ZIO.mergeAll]]
   */
  def mergeAll[R, A, B](in: Iterable[URIO[R, A]])(zero: B)(f: (B, A) => B): URIO[R, B] =
    ZIO.mergeAll(in)(zero)(f)

  /**
   * @see [[zio.ZIO.mergeAllPar]]
   */
  def mergeAllPar[R, A, B](in: Iterable[URIO[R, A]])(zero: B)(f: (B, A) => B): URIO[R, B] =
    ZIO.mergeAllPar(in)(zero)(f)

  /**
   * @see [[zio.ZIO.never]]
   */
  val never: UIO[Nothing] = ZIO.never

  /**
   * @see [[zio.ZIO.none]]
   */
  val none: UIO[Option[Nothing]] = ZIO.none

  /**
   *  @see See [[zio.ZIO.not]]
   */
  def not[R](effect: URIO[R, Boolean]): URIO[R, Boolean] =
    ZIO.not(effect)

  /**
   * @see [[zio.ZIO.provide]]
   */
  def provide[R, A](r: => R): URIO[R, A] => UIO[A] =
    ZIO.provide(r)

  /**
   * @see [[zio.ZIO.raceAll]]
   */
  def raceAll[R, R1 <: R, A](taskr: URIO[R, A], taskrs: Iterable[URIO[R1, A]]): URIO[R1, A] =
    ZIO.raceAll(taskr, taskrs)

  /**
   * @see [[zio.ZIO.reduceAll]]
   */
  def reduceAll[R, R1 <: R, A](a: URIO[R, A], as: Iterable[URIO[R1, A]])(f: (A, A) => A): URIO[R1, A] =
    ZIO.reduceAll(a, as)(f)

  /**
   * @see [[zio.ZIO.reduceAllPar]]
   */
  def reduceAllPar[R, R1 <: R, A](a: URIO[R, A], as: Iterable[URIO[R1, A]])(f: (A, A) => A): URIO[R1, A] =
    ZIO.reduceAllPar(a, as)(f)

  /**
   * @see [[zio.ZIO.replicate]]
   */
  def replicate[R, A](n: Int)(effect: URIO[R, A]): Iterable[URIO[R, A]] =
    ZIO.replicate(n)(effect)

  /**
   * @see See [[zio.ZIO.replicateM]]
   */
  def replicateM[R, A](n: Int)(effect: URIO[R, A]): URIO[R, Iterable[A]] =
    ZIO.replicateM(n)(effect)

  /**
   * @see See [[zio.ZIO.replicateM_]]
   */
  def replicateM_[R, A](n: Int)(effect: URIO[R, A]): URIO[R, Unit] =
    ZIO.replicateM_(n)(effect)

  /**
   * @see [[zio.ZIO.reserve]]
   */
  def reserve[R, A, B](reservation: URIO[R, Reservation[R, Nothing, A]])(use: A => URIO[R, B]): URIO[R, B] =
    ZIO.reserve(reservation)(use)

  /**
   *  @see [[zio.ZIO.right]]
   */
  def right[R, B](b: => B): RIO[R, Either[Nothing, B]] = ZIO.right(b)

  /**
   * @see [[zio.ZIO.runtime]]
   */
  def runtime[R]: URIO[R, Runtime[R]] = ZIO.runtime

  /**
   * @see [[zio.ZIO.second]]
   */
  def second[A]: URIO[(Any, A), A] =
    ZIO.second

  /**
   * @see See [[zio.ZIO.service]]
   */
  def service[A: Tag]: URIO[Has[A], A] =
    ZIO.service[A]

  /**
   * @see See [[zio.ZIO.services[A,B]*]]
   */
  def services[A: Tag, B: Tag]: URIO[Has[A] with Has[B], (A, B)] =
    ZIO.services[A, B]

  /**
   * @see See [[zio.ZIO.services[A,B,C]*]]
   */
  def services[A: Tag, B: Tag, C: Tag]: URIO[Has[A] with Has[B] with Has[C], (A, B, C)] =
    ZIO.services[A, B, C]

  /**
   * @see See [[zio.ZIO.services[A,B,C,D]*]]
   */
  def services[A: Tag, B: Tag, C: Tag, D: Tag]: URIO[Has[A] with Has[B] with Has[C] with Has[D], (A, B, C, D)] =
    ZIO.services[A, B, C, D]

  /**
   * @see See [[zio.ZIO.serviceWith]]
   */
  def serviceWith[Service]: ZIO.ServiceWithPartiallyApplied[Service] =
    new ZIO.ServiceWithPartiallyApplied[Service]

  /**
   * @see [[zio.ZIO.sleep]]
   */
  def sleep(duration: => Duration): URIO[Clock, Unit] = ZIO.sleep(duration)

  /**
   *  @see [[zio.ZIO.some]]
   */
  def some[R, A](a: => A): URIO[R, Option[A]] = ZIO.some(a)

  /**
   * @see [[zio.ZIO.succeed]]
   */
  def succeed[A](a: => A): UIO[A] = ZIO.succeed(a)

  /**
   * @see [[zio.ZIO.swap]]
   */
  def swap[A, B]: URIO[(A, B), (B, A)] = ZIO.swap

  /**
   * @see [[zio.ZIO.trace]]
   */
  def trace: UIO[ZTrace] = ZIO.trace

  /**
   * @see [[zio.ZIO.traced]]
   */
  def traced[R, A](zio: URIO[R, A]): URIO[R, A] = ZIO.traced(zio)

  /**
   * @see [[zio.ZIO.unit]]
   */
  val unit: UIO[Unit] = ZIO.unit

  /**
   * @see [[zio.ZIO.uninterruptible]]
   */
  def uninterruptible[R, A](taskr: URIO[R, A]): URIO[R, A] = ZIO.uninterruptible(taskr)

  /**
   * @see [[zio.ZIO.uninterruptibleMask]]
   */
  def uninterruptibleMask[R, A](k: ZIO.InterruptStatusRestore => URIO[R, A]): URIO[R, A] =
    ZIO.uninterruptibleMask(k)

  /**
   * @see See [[zio.ZIO.unless]]
   */
  def unless[R](b: => Boolean)(zio: => URIO[R, Any]): URIO[R, Unit] =
    ZIO.unless(b)(zio)

  /**
   * @see See [[zio.ZIO.unlessM]]
   */
  def unlessM[R](b: URIO[R, Boolean]): ZIO.UnlessM[R, Nothing] =
    ZIO.unlessM(b)

  /**
   * @see [[zio.ZIO.unsandbox]]
   */
  def unsandbox[R, A](v: IO[Cause[Nothing], A]): URIO[R, A] = ZIO.unsandbox(v)

  /**
   * @see [[zio.ZIO.untraced]]
   */
  def untraced[R, A](zio: URIO[R, A]): URIO[R, A] = ZIO.untraced(zio)

  /**
   * @see [[zio.ZIO.when]]
   */
  def when[R](b: => Boolean)(rio: => URIO[R, Any]): URIO[R, Unit] = ZIO.when(b)(rio)

  /**
   * @see [[zio.ZIO.whenCase]]
   */
  def whenCase[R, A](a: => A)(pf: PartialFunction[A, URIO[R, Any]]): URIO[R, Unit] =
    ZIO.whenCase(a)(pf)

  /**
   * @see [[zio.ZIO.whenCaseM]]
   */
  def whenCaseM[R, A](a: URIO[R, A])(pf: PartialFunction[A, URIO[R, Any]]): URIO[R, Unit] =
    ZIO.whenCaseM(a)(pf)

  /**
   * @see [[zio.ZIO.whenM]]
   */
  def whenM[R](b: URIO[R, Boolean]): ZIO.WhenM[R, Nothing] =
    ZIO.whenM(b)

  /**
   * @see [[zio.ZIO.yieldNow]]
   */
  val yieldNow: UIO[Unit] = ZIO.yieldNow

  private[zio] def succeedNow[A](a: A): UIO[A] = ZIO.succeedNow(a)
}

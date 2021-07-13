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

import scala.concurrent.ExecutionContext
import scala.reflect.ClassTag

object RIO {

  /**
   * @see See [[zio.ZIO.absolve]]
   */
  def absolve[R, A](v: RIO[R, Either[Throwable, A]]): RIO[R, A] =
    ZIO.absolve(v)

  /**
   * @see See [[zio.ZIO.allowInterrupt]]
   */
  def allowInterrupt: UIO[Unit] =
    ZIO.allowInterrupt

  /**
   * @see See [[zio.ZIO.apply]]
   */
  def apply[A](a: => A): Task[A] = ZIO.apply(a)

  /**
   * @see See [[zio.ZIO.access]]
   */
  def access[R]: ZIO.AccessPartiallyApplied[R] =
    ZIO.access

  /**
   * @see See [[zio.ZIO.accessM]]
   */
  def accessM[R]: ZIO.AccessMPartiallyApplied[R] =
    ZIO.accessM

  /**
   * @see See bracket [[zio.ZIO]]
   */
  def bracket[R, A](acquire: RIO[R, A]): ZIO.BracketAcquire[R, Throwable, A] =
    ZIO.bracket(acquire)

  /**
   * @see See bracket [[zio.ZIO]]
   */
  def bracket[R, A, B](
    acquire: RIO[R, A],
    release: A => URIO[R, Any],
    use: A => RIO[R, B]
  ): RIO[R, B] = ZIO.bracket(acquire, release, use)

  /**
   * @see See bracketExit [[zio.ZIO]]
   */
  def bracketExit[R, A](acquire: RIO[R, A]): ZIO.BracketExitAcquire[R, Throwable, A] =
    ZIO.bracketExit(acquire)

  /**
   * @see See bracketExit [[zio.ZIO]]
   */
  def bracketExit[R, A, B](
    acquire: RIO[R, A],
    release: (A, Exit[Throwable, B]) => URIO[R, Any],
    use: A => RIO[R, B]
  ): RIO[R, B] =
    ZIO.bracketExit(acquire, release, use)

  /**
   * @see See [[zio.ZIO.checkInterruptible]]
   */
  def checkInterruptible[R, A](f: InterruptStatus => RIO[R, A]): RIO[R, A] =
    ZIO.checkInterruptible(f)

  /**
   * @see See [[zio.ZIO.checkTraced]]
   */
  def checkTraced[R, A](f: TracingStatus => RIO[R, A]): RIO[R, A] =
    ZIO.checkTraced(f)

  /**
   * @see See [[zio.ZIO.collect]]
   */
  def collect[R, A, B, Collection[+Element] <: Iterable[Element]](in: Collection[A])(
    f: A => ZIO[R, Option[Throwable], B]
  )(implicit bf: BuildFrom[Collection[A], B, Collection[B]]): RIO[R, Collection[B]] =
    ZIO.collect(in)(f)

  /**
   * @see See [[[zio.ZIO.collectAll[R,E,A,Collection[+Element]<:Iterable[Element]]*]]]
   */
  def collectAll[R, A, Collection[+Element] <: Iterable[Element]](
    in: Collection[RIO[R, A]]
  )(implicit bf: BuildFrom[Collection[RIO[R, A]], A, Collection[A]]): RIO[R, Collection[A]] =
    ZIO.collectAll(in)

  /**
   * @see See [[[zio.ZIO.collectAll[R,E,A](in:Set*]]]
   */
  def collectAll[R, A](in: Set[RIO[R, A]]): RIO[R, Set[A]] =
    ZIO.collectAll(in)

  /**
   * @see See [[[zio.ZIO.collectAll[R,E,A](in:Array*]]]
   */
  def collectAll[R, A: ClassTag](in: Array[RIO[R, A]]): RIO[R, Array[A]] =
    ZIO.collectAll(in)

  /**
   * @see See [[[zio.ZIO.collectAll[R,E,A](in:Option*]]]
   */
  def collectAll[R, A](in: Option[RIO[R, A]]): RIO[R, Option[A]] =
    ZIO.collectAll(in)

  /**
   * @see See [[[zio.ZIO.collectAll[R,E,A](in:zio\.NonEmptyChunk*]]]
   */
  def collectAll[R, A](in: NonEmptyChunk[RIO[R, A]]): RIO[R, NonEmptyChunk[A]] =
    ZIO.collectAll(in)

  /**
   * @see See [[[zio.ZIO.collectAll_[R,E,A](in:Iterable*]]]
   */
  def collectAll_[R, A](in: Iterable[RIO[R, A]]): RIO[R, Unit] =
    ZIO.collectAll_(in)

  /**
   * @see See [[[zio.ZIO.collectAllPar[R,E,A,Collection[+Element]<:Iterable[Element]]*]]]
   */
  def collectAllPar[R, A, Collection[+Element] <: Iterable[Element]](
    as: Collection[RIO[R, A]]
  )(implicit bf: BuildFrom[Collection[RIO[R, A]], A, Collection[A]]): RIO[R, Collection[A]] =
    ZIO.collectAllPar(as)

  /**
   * @see See [[[zio.ZIO.collectAllPar[R,E,A](as:Set*]]]
   */
  def collectAllPar[R, A](as: Set[RIO[R, A]]): RIO[R, Set[A]] =
    ZIO.collectAllPar(as)

  /**
   * @see See [[[zio.ZIO.collectAllPar[R,E,A](as:Array*]]]
   */
  def collectAllPar[R, A: ClassTag](as: Array[RIO[R, A]]): RIO[R, Array[A]] =
    ZIO.collectAllPar(as)

  /**
   * @see See [[[zio.ZIO.collectAllPar[R,E,A](as:zio\.NonEmptyChunk*]]]
   */
  def collectAllPar[R, A](as: NonEmptyChunk[RIO[R, A]]): RIO[R, NonEmptyChunk[A]] =
    ZIO.collectAllPar(as)

  /**
   * @see See [[[zio.ZIO.collectAllPar_[R,E,A](as:Iterable*]]]
   */
  def collectAllPar_[R, A](in: Iterable[RIO[R, A]]): RIO[R, Unit] =
    ZIO.collectAllPar_(in)

  /**
   * @see See [[zio.ZIO.collectAllParN]]
   */
  def collectAllParN[R, A, Collection[+Element] <: Iterable[Element]](
    n: Int
  )(as: Collection[RIO[R, A]])(implicit bf: BuildFrom[Collection[RIO[R, A]], A, Collection[A]]): RIO[R, Collection[A]] =
    ZIO.collectAllParN(n)(as)

  /**
   * @see See [[zio.ZIO.collectAllParN_]]
   */
  def collectAllParN_[R, A](n: Int)(as: Iterable[RIO[R, A]]): RIO[R, Unit] =
    ZIO.collectAllParN_(n)(as)

  /**
   * @see See [[zio.ZIO.collectAllSuccesses]]
   */
  def collectAllSuccesses[R, A, Collection[+Element] <: Iterable[Element]](
    in: Collection[RIO[R, A]]
  )(implicit bf: BuildFrom[Collection[RIO[R, A]], A, Collection[A]]): RIO[R, Collection[A]] =
    ZIO.collectAllSuccesses(in)

  /**
   * @see See [[zio.ZIO.collectAllSuccessesPar]]
   */
  def collectAllSuccessesPar[R, A, Collection[+Element] <: Iterable[Element]](
    as: Collection[RIO[R, A]]
  )(implicit bf: BuildFrom[Collection[RIO[R, A]], A, Collection[A]]): URIO[R, Collection[A]] =
    ZIO.collectAllSuccessesPar(as)

  /**
   * @see See [[zio.ZIO.collectAllSuccessesParN]]
   */
  def collectAllSuccessesParN[R, A, Collection[+Element] <: Iterable[Element]](n: Int)(
    as: Collection[RIO[R, A]]
  )(implicit bf: BuildFrom[Collection[RIO[R, A]], A, Collection[A]]): URIO[R, Collection[A]] =
    ZIO.collectAllSuccessesParN(n)(as)

  /**
   * @see See [[zio.ZIO.collectAllWith]]
   */
  def collectAllWith[R, A, B, Collection[+Element] <: Iterable[Element]](
    in: Collection[RIO[R, A]]
  )(f: PartialFunction[A, B])(implicit bf: BuildFrom[Collection[RIO[R, A]], B, Collection[B]]): RIO[R, Collection[B]] =
    ZIO.collectAllWith(in)(f)

  /**
   * @see See [[zio.ZIO.collectAllWithPar]]
   */
  def collectAllWithPar[R, A, B, Collection[+Element] <: Iterable[Element]](
    as: Collection[RIO[R, A]]
  )(f: PartialFunction[A, B])(implicit bf: BuildFrom[Collection[RIO[R, A]], B, Collection[B]]): RIO[R, Collection[B]] =
    ZIO.collectAllWithPar(as)(f)

  /**
   * @see See [[zio.ZIO.collectAllWithParN]]
   */
  def collectAllWithParN[R, A, B, Collection[+Element] <: Iterable[Element]](n: Int)(
    as: Collection[RIO[R, A]]
  )(f: PartialFunction[A, B])(implicit bf: BuildFrom[Collection[RIO[R, A]], B, Collection[B]]): RIO[R, Collection[B]] =
    ZIO.collectAllWithParN(n)(as)(f)

  /**
   * @see See [[zio.ZIO.collectFirst]]
   */
  def collectFirst[R, A, B](as: Iterable[A])(f: A => RIO[R, Option[B]]): RIO[R, Option[B]] =
    ZIO.collectFirst(as)(f)

  /**
   * @see See [[zio.ZIO.collectPar]]
   */
  def collectPar[R, A, B, Collection[+Element] <: Iterable[Element]](in: Collection[A])(
    f: A => ZIO[R, Option[Throwable], B]
  )(implicit bf: BuildFrom[Collection[A], B, Collection[B]]): RIO[R, Collection[B]] =
    ZIO.collectPar(in)(f)

  /**
   * @see See [[zio.ZIO.collectParN]]
   */
  def collectParN[R, A, B, Collection[+Element] <: Iterable[Element]](n: Int)(in: Collection[A])(
    f: A => ZIO[R, Option[Throwable], B]
  )(implicit bf: BuildFrom[Collection[A], B, Collection[B]]): RIO[R, Collection[B]] =
    ZIO.collectParN(n)(in)(f)

  /**
   * @see See [[zio.ZIO.cond]]
   */
  def cond[A](predicate: Boolean, result: => A, error: => Throwable): Task[A] =
    ZIO.cond(predicate, result, error)

  /**
   * @see See [[zio.ZIO.debug]]
   */
  def debug(value: Any): UIO[Unit] =
    ZIO.debug(value)

  /**
   * @see See [[zio.ZIO.descriptor]]
   */
  def descriptor: UIO[Fiber.Descriptor] = ZIO.descriptor

  /**
   * @see See [[zio.ZIO.descriptorWith]]
   */
  def descriptorWith[R, A](f: Fiber.Descriptor => RIO[R, A]): RIO[R, A] =
    ZIO.descriptorWith(f)

  /**
   * @see See [[zio.ZIO.die]]
   */
  def die(t: => Throwable): UIO[Nothing] = ZIO.die(t)

  /**
   * @see See [[zio.ZIO.dieMessage]]
   */
  def dieMessage(message: => String): UIO[Nothing] = ZIO.dieMessage(message)

  /**
   * @see See [[zio.ZIO.done]]
   */
  def done[A](r: => Exit[Throwable, A]): Task[A] = ZIO.done(r)

  /**
   * @see See [[zio.ZIO.effect]]
   */
  def effect[A](effect: => A): Task[A] = ZIO.effect(effect)

  /**
   * @see See [[zio.ZIO.effectAsync]]
   */
  def effectAsync[R, A](register: (RIO[R, A] => Unit) => Any, blockingOn: List[Fiber.Id] = Nil): RIO[R, A] =
    ZIO.effectAsync(register, blockingOn)

  /**
   * @see See [[zio.ZIO.effectAsyncMaybe]]
   */
  def effectAsyncMaybe[R, A](
    register: (RIO[R, A] => Unit) => Option[RIO[R, A]],
    blockingOn: List[Fiber.Id] = Nil
  ): RIO[R, A] =
    ZIO.effectAsyncMaybe(register, blockingOn)

  /**
   * @see See [[zio.ZIO.effectAsyncM]]
   */
  def effectAsyncM[R, A](register: (RIO[R, A] => Unit) => RIO[R, Any]): RIO[R, A] =
    ZIO.effectAsyncM(register)

  /**
   * @see See [[zio.ZIO.effectAsyncInterrupt]]
   */
  def effectAsyncInterrupt[R, A](
    register: (RIO[R, A] => Unit) => Either[Canceler[R], RIO[R, A]],
    blockingOn: List[Fiber.Id] = Nil
  ): RIO[R, A] =
    ZIO.effectAsyncInterrupt(register, blockingOn)

  /**
   * Returns a lazily constructed effect, whose construction may itself require effects.
   * When no environment is required (i.e., when R == Any) it is conceptually equivalent to `flatten(effect(io))`.
   */
  def effectSuspend[R, A](rio: => RIO[R, A]): RIO[R, A] = ZIO.effectSuspend(rio)

  /**
   * @see See [[zio.ZIO.effectSuspendTotal]]
   */
  def effectSuspendTotal[R, A](rio: => RIO[R, A]): RIO[R, A] = ZIO.effectSuspendTotal(rio)

  /**
   * @see See [[zio.ZIO.effectSuspendTotalWith]]
   */
  def effectSuspendTotalWith[R, A](p: (Platform, Fiber.Id) => RIO[R, A]): RIO[R, A] =
    ZIO.effectSuspendTotalWith(p)

  /**
   * Returns a lazily constructed effect, whose construction may itself require effects.
   * When no environment is required (i.e., when R == Any) it is conceptually equivalent to `flatten(effect(io))`.
   */
  def effectSuspendWith[R, A](p: (Platform, Fiber.Id) => RIO[R, A]): RIO[R, A] = ZIO.effectSuspendWith(p)

  /**
   * @see See [[zio.ZIO.effectTotal]]
   */
  def effectTotal[A](effect: => A): UIO[A] = ZIO.effectTotal(effect)

  /**
   * @see See [[zio.ZIO.environment]]
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
  def exists[R, A](as: Iterable[A])(f: A => RIO[R, Boolean]): RIO[R, Boolean] =
    ZIO.exists(as)(f)

  /**
   * @see See [[zio.ZIO.fail]]
   */
  def fail(error: => Throwable): Task[Nothing] = ZIO.fail(error)

  /**
   * @see [[zio.ZIO.fiberId]]
   */
  val fiberId: UIO[Fiber.Id] = ZIO.fiberId

  /**
   * @see [[zio.ZIO.filter[R,E,A,Collection*]]
   */
  def filter[R, A, Collection[+Element] <: Iterable[Element]](
    as: Collection[A]
  )(f: A => RIO[R, Boolean])(implicit bf: BuildFrom[Collection[A], A, Collection[A]]): RIO[R, Collection[A]] =
    ZIO.filter(as)(f)

  /**
   * @see [[[zio.ZIO.filter[R,E,A](as:Set*]]]
   */
  def filter[R, A](as: Set[A])(f: A => RIO[R, Boolean]): RIO[R, Set[A]] =
    ZIO.filter(as)(f)

  /**
   * @see [[zio.ZIO.filterPar[R,E,A,Collection*]]
   */
  def filterPar[R, A, Collection[+Element] <: Iterable[Element]](
    as: Collection[A]
  )(f: A => RIO[R, Boolean])(implicit bf: BuildFrom[Collection[A], A, Collection[A]]): RIO[R, Collection[A]] =
    ZIO.filterPar(as)(f)

  /**
   * @see [[[zio.ZIO.filterPar[R,E,A](as:Set*]]]
   */
  def filterPar[R, A](as: Set[A])(f: A => RIO[R, Boolean]): RIO[R, Set[A]] =
    ZIO.filterPar(as)(f)

  /**
   * @see [[zio.ZIO.filterNot[R,E,A,Collection*]]
   */
  def filterNot[R, A, Collection[+Element] <: Iterable[Element]](
    as: Collection[A]
  )(f: A => RIO[R, Boolean])(implicit bf: BuildFrom[Collection[A], A, Collection[A]]): RIO[R, Collection[A]] =
    ZIO.filterNot(as)(f)

  /**
   * @see [[[zio.ZIO.filterNot[R,E,A](as:Set*]]]
   */
  def filterNot[R, A](as: Set[A])(f: A => RIO[R, Boolean]): RIO[R, Set[A]] =
    ZIO.filterNot(as)(f)

  /**
   * @see [[zio.ZIO.filterNotPar[R,E,A,Collection*]]
   */
  def filterNotPar[R, A, Collection[+Element] <: Iterable[Element]](
    as: Collection[A]
  )(f: A => RIO[R, Boolean])(implicit bf: BuildFrom[Collection[A], A, Collection[A]]): RIO[R, Collection[A]] =
    ZIO.filterNotPar(as)(f)

  /**
   * @see [[[zio.ZIO.filterNotPar[R,E,A](as:Set*]]]
   */
  def filterNotPar[R, A](as: Set[A])(f: A => RIO[R, Boolean]): RIO[R, Set[A]] =
    ZIO.filterNotPar(as)(f)

  /**
   * @see See [[zio.ZIO.first]]
   */
  def first[A]: RIO[(A, Any), A] =
    ZIO.first

  /**
   * @see See [[zio.ZIO.firstSuccessOf]]
   */
  def firstSuccessOf[R, A](
    rio: RIO[R, A],
    rest: Iterable[RIO[R, A]]
  ): RIO[R, A] = ZIO.firstSuccessOf(rio, rest)

  /**
   * @see See [[zio.ZIO.flatten]]
   */
  def flatten[R, A](taskr: RIO[R, RIO[R, A]]): RIO[R, A] =
    ZIO.flatten(taskr)

  /**
   * @see See [[zio.ZIO.foldLeft]]
   */
  def foldLeft[R, S, A](in: Iterable[A])(zero: S)(f: (S, A) => RIO[R, S]): RIO[R, S] =
    ZIO.foldLeft(in)(zero)(f)

  /**
   * @see See [[zio.ZIO.foldRight]]
   */
  def foldRight[R, S, A](in: Iterable[A])(zero: S)(f: (A, S) => RIO[R, S]): RIO[R, S] =
    ZIO.foldRight(in)(zero)(f)

  /**
   * @see See [[zio.ZIO.forall]]
   */
  def forall[R, A](as: Iterable[A])(f: A => RIO[R, Boolean]): RIO[R, Boolean] =
    ZIO.forall(as)(f)

  /**
   * @see See [[[zio.ZIO.foreach[R,E,A,B,Collection[+Element]<:Iterable[Element]]*]]]
   */
  def foreach[R, A, B, Collection[+Element] <: Iterable[Element]](
    in: Collection[A]
  )(f: A => RIO[R, B])(implicit bf: BuildFrom[Collection[A], B, Collection[B]]): RIO[R, Collection[B]] =
    ZIO.foreach(in)(f)

  /**
   * @see See [[[zio.ZIO.foreach[R,E,A,B](in:Set*]]]
   */
  def foreach[R, A, B](in: Set[A])(f: A => RIO[R, B]): RIO[R, Set[B]] =
    ZIO.foreach(in)(f)

  /**
   * @see See [[[zio.ZIO.foreach[R,E,A,B](in:Array*]]]
   */
  def foreach[R, A, B: ClassTag](in: Array[A])(f: A => RIO[R, B]): RIO[R, Array[B]] =
    ZIO.foreach(in)(f)

  /**
   * @see See [[[zio.ZIO.foreach[R,E,Key,Key2,Value,Value2](map:Map*]]]
   */
  def foreach[R, Key, Key2, Value, Value2](
    map: Map[Key, Value]
  )(f: (Key, Value) => RIO[R, (Key2, Value2)]): RIO[R, Map[Key2, Value2]] =
    ZIO.foreach(map)(f)

  /**
   * @see See [[[zio.ZIO.foreach[R,E,A,B](in:Option*]]]
   */
  def foreach[R, A, B](in: Option[A])(f: A => RIO[R, B]): RIO[R, Option[B]] =
    ZIO.foreach(in)(f)

  /**
   * @see See [[[zio.ZIO.foreach[R,E,A,B](in:zio\.NonEmptyChunk*]]]
   */
  def foreach[R, A, B](in: NonEmptyChunk[A])(f: A => RIO[R, B]): RIO[R, NonEmptyChunk[B]] =
    ZIO.foreach(in)(f)

  /**
   * @see See [[zio.ZIO.foreachExec]]
   */
  final def foreachExec[R, A, B, Collection[+Element] <: Iterable[Element]](as: Collection[A])(
    exec: ExecutionStrategy
  )(f: A => RIO[R, B])(implicit bf: BuildFrom[Collection[A], B, Collection[B]]): RIO[R, Collection[B]] =
    ZIO.foreachExec(as)(exec)(f)

  /**
   * @see See [[[zio.ZIO.foreachPar[R,E,A,B,Collection[+Element]<:Iterable[Element]]*]]]
   */
  def foreachPar[R, A, B, Collection[+Element] <: Iterable[Element]](
    as: Collection[A]
  )(fn: A => RIO[R, B])(implicit bf: BuildFrom[Collection[A], B, Collection[B]]): RIO[R, Collection[B]] =
    ZIO.foreachPar(as)(fn)

  /**
   * @see See [[[zio.ZIO.foreachPar[R,E,A,B](as:Set*]]]
   */
  def foreachPar[R, A, B](as: Set[A])(fn: A => RIO[R, B]): RIO[R, Set[B]] =
    ZIO.foreachPar(as)(fn)

  /**
   * @see See [[[zio.ZIO.foreachPar[R,E,A,B](as:Array*]]]
   */
  def foreachPar[R, A, B: ClassTag](as: Array[A])(fn: A => RIO[R, B]): RIO[R, Array[B]] =
    ZIO.foreachPar(as)(fn)

  /**
   * @see See [[[zio.ZIO.foreachPar[R,E,Key,Key2,Value,Value2](map:Map*]]]
   */
  def foreachPar[R, Key, Key2, Value, Value2](
    map: Map[Key, Value]
  )(f: (Key, Value) => RIO[R, (Key2, Value2)]): RIO[R, Map[Key2, Value2]] =
    ZIO.foreachPar(map)(f)

  /**
   * @see See [[[zio.ZIO.foreachPar[R,E,A,B](as:zio\.NonEmptyChunk*]]]
   */
  def foreachPar[R, A, B](as: NonEmptyChunk[A])(fn: A => RIO[R, B]): RIO[R, NonEmptyChunk[B]] =
    ZIO.foreachPar(as)(fn)

  /**
   * @see See [[zio.ZIO.foreachParN]]
   */
  def foreachParN[R, A, B, Collection[+Element] <: Iterable[Element]](n: Int)(
    as: Collection[A]
  )(fn: A => RIO[R, B])(implicit bf: BuildFrom[Collection[A], B, Collection[B]]): RIO[R, Collection[B]] =
    ZIO.foreachParN(n)(as)(fn)

  /**
   * @see See [[[zio.ZIO.foreach_[R,E,A](as:Iterable*]]]
   */
  def foreach_[R, A](as: Iterable[A])(f: A => RIO[R, Any]): RIO[R, Unit] =
    ZIO.foreach_(as)(f)

  /**
   * @see See [[[zio.ZIO.foreachPar_[R,E,A](as:Iterable*]]]
   */
  def foreachPar_[R, A, B](as: Iterable[A])(f: A => RIO[R, Any]): RIO[R, Unit] =
    ZIO.foreachPar_(as)(f)

  /**
   * @see See [[zio.ZIO.foreachParN_]]
   */
  def foreachParN_[R, A, B](n: Int)(as: Iterable[A])(f: A => RIO[R, Any]): RIO[R, Unit] =
    ZIO.foreachParN_(n)(as)(f)

  /**
   * @see See [[zio.ZIO.forkAll]]
   */
  def forkAll[R, A, Collection[+Element] <: Iterable[Element]](
    as: Collection[RIO[R, A]]
  )(implicit bf: BuildFrom[Collection[RIO[R, A]], A, Collection[A]]): URIO[R, Fiber[Throwable, Collection[A]]] =
    ZIO.forkAll(as)

  /**
   * @see See [[zio.ZIO.forkAll_]]
   */
  def forkAll_[R, A](as: Iterable[RIO[R, A]]): URIO[R, Unit] =
    ZIO.forkAll_(as)

  /**
   * @see See [[zio.ZIO.fromEither]]
   */
  def fromEither[A](v: => Either[Throwable, A]): Task[A] =
    ZIO.fromEither(v)

  /**
   * @see See [[zio.ZIO.fromFiber]]
   */
  def fromFiber[A](fiber: => Fiber[Throwable, A]): Task[A] =
    ZIO.fromFiber(fiber)

  /**
   * @see See [[zio.ZIO.fromFiberM]]
   */
  def fromFiberM[A](fiber: Task[Fiber[Throwable, A]]): Task[A] =
    ZIO.fromFiberM(fiber)

  /**
   * @see See [[zio.ZIO.fromFunction]]
   */
  def fromFunction[R, A](f: R => A): URIO[R, A] =
    ZIO.fromFunction(f)

  /**
   * @see See [[zio.ZIO.fromFunctionFuture]]
   */
  def fromFunctionFuture[R, A](f: R => scala.concurrent.Future[A]): RIO[R, A] =
    ZIO.fromFunctionFuture(f)

  /**
   * @see See [[zio.ZIO.fromFunctionM]]
   */
  def fromFunctionM[R, A](f: R => Task[A]): RIO[R, A] =
    ZIO.fromFunctionM(f)

  /**
   * @see See [[zio.ZIO.fromFuture]]
   */
  def fromFuture[A](make: ExecutionContext => scala.concurrent.Future[A]): Task[A] =
    ZIO.fromFuture(make)

  /**
   * @see See [[zio.ZIO.fromFutureInterrupt]]
   */
  def fromFutureInterrupt[A](make: ExecutionContext => scala.concurrent.Future[A]): Task[A] =
    ZIO.fromFutureInterrupt(make)

  /**
   * @see See [[zio.ZIO.fromTry]]
   */
  def fromTry[A](value: => scala.util.Try[A]): Task[A] =
    ZIO.fromTry(value)

  /**
   * @see See [[zio.ZIO.getOrFail]]
   */
  def getOrFail[A](v: => Option[A]): Task[A] = ZIO.getOrFail(v)

  /**
   * @see See [[zio.ZIO.halt]]
   */
  def halt(cause: => Cause[Throwable]): Task[Nothing] = ZIO.halt(cause)

  /**
   * @see See [[zio.ZIO.haltWith]]
   */
  def haltWith[R](function: (() => ZTrace) => Cause[Throwable]): RIO[R, Nothing] =
    ZIO.haltWith(function)

  /**
   * @see See [[zio.ZIO.identity]]
   */
  def identity[R]: RIO[R, R] = ZIO.identity

  /**
   * @see [[zio.ZIO.ifM]]
   */
  def ifM[R](b: RIO[R, Boolean]): ZIO.IfM[R, Throwable] =
    ZIO.ifM(b)

  /**
   * @see [[zio.ZIO.ifM]]
   */
  def ifF[R](b: RIO[R, Boolean]): ZIO.IfF[R, Throwable] =
    ZIO.ifF(b)

  /**
   * @see [[zio.ZIO.infinity]]
   */
  val infinity: URIO[Clock, Nothing] = ZIO.sleep(Duration.fromNanos(Long.MaxValue)) *> ZIO.never

  /**
   * @see See [[zio.ZIO.interrupt]]
   */
  val interrupt: UIO[Nothing] = ZIO.interrupt

  /**
   * @see See [[zio.ZIO.interruptAs]]
   */
  def interruptAs(fiberId: => Fiber.Id): UIO[Nothing] = ZIO.interruptAs(fiberId)

  /**
   * @see See [[zio.ZIO.interruptible]]
   */
  def interruptible[R, A](taskr: RIO[R, A]): RIO[R, A] =
    ZIO.interruptible(taskr)

  /**
   * @see See [[zio.ZIO.interruptibleMask]]
   */
  def interruptibleMask[R, A](k: ZIO.InterruptStatusRestore => RIO[R, A]): RIO[R, A] =
    ZIO.interruptibleMask(k)

  /**
   * @see See [[zio.ZIO.iterate]]
   */
  def iterate[R, S](initial: S)(cont: S => Boolean)(body: S => RIO[R, S]): RIO[R, S] =
    ZIO.iterate(initial)(cont)(body)

  /**
   * @see See [[zio.ZIO.lock]]
   */
  def lock[R, A](executor: => Executor)(taskr: RIO[R, A]): RIO[R, A] =
    ZIO.lock(executor)(taskr)

  /**
   *  @see See [[zio.ZIO.loop]]
   */
  def loop[R, A, S](initial: S)(cont: S => Boolean, inc: S => S)(body: S => RIO[R, A]): RIO[R, List[A]] =
    ZIO.loop(initial)(cont, inc)(body)

  /**
   *  @see See [[zio.ZIO.loop_]]
   */
  def loop_[R, S](initial: S)(cont: S => Boolean, inc: S => S)(body: S => RIO[R, Any]): RIO[R, Unit] =
    ZIO.loop_(initial)(cont, inc)(body)

  /**
   *  @see See [[zio.ZIO.left]]
   */
  def left[R, A](a: => A): RIO[R, Either[A, Nothing]] = ZIO.left(a)

  /**
   *  @see [[zio.ZIO.mapN[R,E,A,B,C]*]]
   */
  def mapN[R, A, B, C](rio1: RIO[R, A], rio2: RIO[R, B])(f: (A, B) => C): RIO[R, C] =
    ZIO.mapN(rio1, rio2)(f)

  /**
   *  @see [[zio.ZIO.mapN[R,E,A,B,C,D]*]]
   */
  def mapN[R, A, B, C, D](rio1: RIO[R, A], rio2: RIO[R, B], rio3: RIO[R, C])(
    f: (A, B, C) => D
  ): RIO[R, D] =
    ZIO.mapN(rio1, rio2, rio3)(f)

  /**
   *  @see [[zio.ZIO.mapN[R,E,A,B,C,D,F]*]]
   */
  def mapN[R, A, B, C, D, F](rio1: RIO[R, A], rio2: RIO[R, B], rio3: RIO[R, C], rio4: RIO[R, D])(
    f: (A, B, C, D) => F
  ): RIO[R, F] =
    ZIO.mapN(rio1, rio2, rio3, rio4)(f)

  /**
   *  @see [[zio.ZIO.mapParN[R,E,A,B,C]*]]
   */
  def mapParN[R, A, B, C](rio1: RIO[R, A], rio2: RIO[R, B])(f: (A, B) => C): RIO[R, C] =
    ZIO.mapParN(rio1, rio2)(f)

  /**
   *  @see [[zio.ZIO.mapParN[R,E,A,B,C,D]*]]
   */
  def mapParN[R, A, B, C, D](rio1: RIO[R, A], rio2: RIO[R, B], rio3: RIO[R, C])(f: (A, B, C) => D): RIO[R, D] =
    ZIO.mapParN(rio1, rio2, rio3)(f)

  /**
   *  @see [[zio.ZIO.mapParN[R,E,A,B,C,D,F]*]]
   */
  def mapParN[R, A, B, C, D, F](rio1: RIO[R, A], rio2: RIO[R, B], rio3: RIO[R, C], rio4: RIO[R, D])(
    f: (A, B, C, D) => F
  ): RIO[R, F] =
    ZIO.mapParN(rio1, rio2, rio3, rio4)(f)

  /**
   * @see See [[zio.ZIO.memoize]]
   */
  def memoize[R, A, B](f: A => RIO[R, B]): UIO[A => RIO[R, B]] =
    ZIO.memoize(f)

  /**
   * @see See [[zio.ZIO.mergeAll]]
   */
  def mergeAll[R, A, B](in: Iterable[RIO[R, A]])(zero: B)(f: (B, A) => B): RIO[R, B] =
    ZIO.mergeAll(in)(zero)(f)

  /**
   * @see See [[zio.ZIO.mergeAllPar]]
   */
  def mergeAllPar[R, A, B](in: Iterable[RIO[R, A]])(zero: B)(f: (B, A) => B): RIO[R, B] =
    ZIO.mergeAllPar(in)(zero)(f)

  /**
   * @see See [[zio.ZIO.never]]
   */
  val never: UIO[Nothing] = ZIO.never

  /**
   * @see See [[zio.ZIO.none]]
   */
  val none: UIO[Option[Nothing]] = ZIO.none

  /**
   * @see See [[zio.ZIO.noneOrFail]]
   */
  def noneOrFail(o: Option[Throwable]): RIO[Nothing, Unit] =
    ZIO.noneOrFail(o)

  /**
   * @see See [[zio.ZIO.noneOrFailWith]]
   */
  def noneOrFailWith[O](o: Option[O])(f: O => Throwable): RIO[Nothing, Unit] =
    ZIO.noneOrFailWith(o)(f)

  /**
   *  @see See [[zio.ZIO.not]]
   */
  def not[R](effect: RIO[R, Boolean]): RIO[R, Boolean] =
    ZIO.not(effect)

  /**
   * @see See [[zio.ZIO.partition]]
   */
  def partition[R, A, B](in: Iterable[A])(f: A => RIO[R, B]): RIO[R, (Iterable[Throwable], Iterable[B])] =
    ZIO.partition(in)(f)

  /**
   * @see See [[zio.ZIO.partitionPar]]
   */
  def partitionPar[R, A, B](in: Iterable[A])(f: A => RIO[R, B]): RIO[R, (Iterable[Throwable], Iterable[B])] =
    ZIO.partitionPar(in)(f)

  /**
   * @see See [[zio.ZIO.partitionParN]]
   */
  def partitionParN[R, A, B](n: Int)(in: Iterable[A])(f: A => RIO[R, B]): RIO[R, (Iterable[Throwable], Iterable[B])] =
    ZIO.partitionParN(n)(in)(f)

  /**
   * @see See [[zio.ZIO.provide]]
   */
  def provide[R, A](r: => R): RIO[R, A] => Task[A] =
    ZIO.provide(r)

  /**
   * @see See [[zio.ZIO.raceAll]]
   */
  def raceAll[R, R1 <: R, A](taskr: RIO[R, A], taskrs: Iterable[RIO[R1, A]]): RIO[R1, A] =
    ZIO.raceAll(taskr, taskrs)

  /**
   * @see See [[zio.ZIO.reduceAll]]
   */
  def reduceAll[R, R1 <: R, A](a: RIO[R, A], as: Iterable[RIO[R1, A]])(f: (A, A) => A): RIO[R1, A] =
    ZIO.reduceAll(a, as)(f)

  /**
   * @see See [[zio.ZIO.reduceAllPar]]
   */
  def reduceAllPar[R, R1 <: R, A](a: RIO[R, A], as: Iterable[RIO[R1, A]])(f: (A, A) => A): RIO[R1, A] =
    ZIO.reduceAllPar(a, as)(f)

  /**
   * @see See [[zio.ZIO.replicate]]
   */
  def replicate[R, A](n: Int)(effect: RIO[R, A]): Iterable[RIO[R, A]] =
    ZIO.replicate(n)(effect)

  /**
   * @see See [[zio.ZIO.replicateM]]
   */
  def replicateM[R, A](n: Int)(effect: RIO[R, A]): RIO[R, Iterable[A]] =
    ZIO.replicateM(n)(effect)

  /**
   * @see See [[zio.ZIO.replicateM_]]
   */
  def replicateM_[R, A](n: Int)(effect: RIO[R, A]): RIO[R, Unit] =
    ZIO.replicateM_(n)(effect)

  /**
   * @see See [[zio.ZIO.require]]
   */
  def require[A](error: => Throwable): IO[Throwable, Option[A]] => IO[Throwable, A] =
    ZIO.require[Any, Throwable, A](error)

  /**
   * @see See [[zio.ZIO.reserve]]
   */
  def reserve[R, A, B](reservation: RIO[R, Reservation[R, Throwable, A]])(use: A => RIO[R, B]): RIO[R, B] =
    ZIO.reserve(reservation)(use)

  /**
   *  @see [[zio.ZIO.right]]
   */
  def right[R, B](b: => B): RIO[R, Either[Nothing, B]] = ZIO.right(b)

  /**
   * @see See [[zio.ZIO.runtime]]
   */
  def runtime[R]: ZIO[R, Nothing, Runtime[R]] = ZIO.runtime

  /**
   * @see See [[zio.ZIO.second]]
   */
  def second[A]: RIO[(Any, A), A] =
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
    ZIO.serviceWith[Service]

  /**
   * @see See [[zio.ZIO.sleep]]
   */
  def sleep(duration: => Duration): RIO[Clock, Unit] =
    ZIO.sleep(duration)

  /**
   *  @see [[zio.ZIO.some]]
   */
  def some[R, A](a: => A): RIO[R, Option[A]] = ZIO.some(a)

  /**
   * @see See [[zio.ZIO.succeed]]
   */
  def succeed[A](a: => A): UIO[A] = ZIO.succeed(a)

  /**
   * @see See [[zio.ZIO.swap]]
   */
  def swap[A, B]: RIO[(A, B), (B, A)] =
    ZIO.swap

  /**
   * @see See [[zio.ZIO.trace]]
   */
  def trace: UIO[ZTrace] = ZIO.trace

  /**
   * @see See [[zio.ZIO.traced]]
   */
  def traced[R, A](zio: RIO[R, A]): RIO[R, A] = ZIO.traced(zio)

  /**
   * @see See [[zio.ZIO.unit]]
   */
  val unit: UIO[Unit] = ZIO.unit

  /**
   * @see See [[zio.ZIO.uninterruptible]]
   */
  def uninterruptible[R, A](taskr: RIO[R, A]): RIO[R, A] =
    ZIO.uninterruptible(taskr)

  /**
   * @see See [[zio.ZIO.uninterruptibleMask]]
   */
  def uninterruptibleMask[R, A](k: ZIO.InterruptStatusRestore => RIO[R, A]): RIO[R, A] =
    ZIO.uninterruptibleMask(k)

  /**
   * @see See [[zio.ZIO.unless]]
   */
  def unless[R](b: => Boolean)(zio: => RIO[R, Any]): RIO[R, Unit] =
    ZIO.unless(b)(zio)

  /**
   * @see See [[zio.ZIO.unlessM]]
   */
  def unlessM[R](b: RIO[R, Boolean]): ZIO.UnlessM[R, Throwable] =
    ZIO.unlessM(b)

  /**
   * @see See [[zio.ZIO.unsandbox]]
   */
  def unsandbox[R, A](v: IO[Cause[Throwable], A]): RIO[R, A] = ZIO.unsandbox(v)

  /**
   * @see See [[zio.ZIO.untraced]]
   */
  def untraced[R, A](zio: RIO[R, A]): RIO[R, A] = ZIO.untraced(zio)

  /**
   * @see See [[zio.ZIO.when]]
   */
  def when[R](b: => Boolean)(rio: => RIO[R, Any]): RIO[R, Unit] =
    ZIO.when(b)(rio)

  /**
   * @see See [[zio.ZIO.whenCase]]
   */
  def whenCase[R, A](a: => A)(pf: PartialFunction[A, RIO[R, Any]]): RIO[R, Unit] =
    ZIO.whenCase(a)(pf)

  /**
   * @see See [[zio.ZIO.whenCaseM]]
   */
  def whenCaseM[R, A](a: RIO[R, A])(pf: PartialFunction[A, RIO[R, Any]]): RIO[R, Unit] =
    ZIO.whenCaseM(a)(pf)

  /**
   * @see See [[zio.ZIO.whenM]]
   */
  def whenM[R](b: RIO[R, Boolean]): ZIO.WhenM[R, Throwable] =
    ZIO.whenM(b)

  /**
   * @see See [[zio.ZIO.yieldNow]]
   */
  val yieldNow: UIO[Unit] = ZIO.yieldNow

  private[zio] def succeedNow[A](a: A): UIO[A] = ZIO.succeedNow(a)
}

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

import zio.internal.{Executor, Platform}

import scala.concurrent.ExecutionContext
import scala.reflect.ClassTag

object Task extends TaskPlatformSpecific {

  /**
   * @see See [[zio.ZIO.absolve]]
   */
  def absolve[A](v: Task[Either[Throwable, A]]): Task[A] =
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
   * @see See bracket [[zio.ZIO]]
   */
  def bracket[A](acquire: Task[A]): ZIO.BracketAcquire[Any, Throwable, A] =
    ZIO.bracket(acquire)

  /**
   * @see See bracket [[zio.ZIO]]
   */
  def bracket[A, B](acquire: Task[A], release: A => UIO[Any], use: A => Task[B]): Task[B] =
    ZIO.bracket(acquire, release, use)

  /**
   * @see See bracketExit [[zio.ZIO]]
   */
  def bracketExit[A](acquire: Task[A]): ZIO.BracketExitAcquire[Any, Throwable, A] =
    ZIO.bracketExit(acquire)

  /**
   * @see See bracketExit [[zio.ZIO]]
   */
  def bracketExit[A, B](
    acquire: Task[A],
    release: (A, Exit[Throwable, B]) => UIO[Any],
    use: A => Task[B]
  ): Task[B] =
    ZIO.bracketExit(acquire, release, use)

  /**
   * @see See [[zio.ZIO.checkInterruptible]]
   */
  def checkInterruptible[A](f: InterruptStatus => Task[A]): Task[A] =
    ZIO.checkInterruptible(f)

  /**
   * @see See [[zio.ZIO.checkTraced]]
   */
  def checkTraced[A](f: TracingStatus => Task[A]): Task[A] =
    ZIO.checkTraced(f)

  /**
   * @see See [[zio.ZIO.collect]]
   */
  def collect[A, B, Collection[+Element] <: Iterable[Element]](
    in: Collection[A]
  )(f: A => IO[Option[Throwable], B])(implicit bf: BuildFrom[Collection[A], B, Collection[B]]): Task[Collection[B]] =
    ZIO.collect(in)(f)

  /**
   * @see See [[[zio.ZIO.collectAll[R,E,A,Collection[+Element]<:Iterable[Element]]*]]]
   */
  def collectAll[A, Collection[+Element] <: Iterable[Element]](
    in: Collection[Task[A]]
  )(implicit bf: BuildFrom[Collection[Task[A]], A, Collection[A]]): Task[Collection[A]] =
    ZIO.collectAll(in)

  /**
   * @see See [[[zio.ZIO.collectAll[R,E,A](in:Set*]]]
   */
  def collectAll[A](in: Set[Task[A]]): Task[Set[A]] =
    ZIO.collectAll(in)

  /**
   * @see See [[[zio.ZIO.collectAll[R,E,A](in:Array*]]]
   */
  def collectAll[A: ClassTag](in: Array[Task[A]]): Task[Array[A]] =
    ZIO.collectAll(in)

  /**
   * @see See [[[zio.ZIO.collectAll[R,E,A](in:Option*]]]
   */
  def collectAll[A](in: Option[Task[A]]): Task[Option[A]] =
    ZIO.collectAll(in)

  /**
   * @see See [[[zio.ZIO.collectAll[R,E,A](in:zio\.NonEmptyChunk*]]]
   */
  def collectAll[A](in: NonEmptyChunk[Task[A]]): Task[NonEmptyChunk[A]] =
    ZIO.collectAll(in)

  /**
   * @see See [[[zio.ZIO.collectAll_[R,E,A](in:Iterable*]]]
   */
  def collectAll_[A](in: Iterable[Task[A]]): Task[Unit] =
    ZIO.collectAll_(in)

  /**
   * @see See [[[zio.ZIO.collectAllPar[R,E,A,Collection[+Element]<:Iterable[Element]]*]]]
   */
  def collectAllPar[A, Collection[+Element] <: Iterable[Element]](
    as: Collection[Task[A]]
  )(implicit bf: BuildFrom[Collection[Task[A]], A, Collection[A]]): Task[Collection[A]] =
    ZIO.collectAllPar(as)

  /**
   * @see See [[[zio.ZIO.collectAllPar[R,E,A](as:Set*]]]
   */
  def collectAllPar[A](as: Set[Task[A]]): Task[Set[A]] =
    ZIO.collectAllPar(as)

  /**
   * @see See [[[zio.ZIO.collectAllPar[R,E,A](as:Array*]]]
   */
  def collectAllPar[A: ClassTag](as: Array[Task[A]]): Task[Array[A]] =
    ZIO.collectAllPar(as)

  /**
   * @see See [[[zio.ZIO.collectAllPar[R,E,A](as:zio\.NonEmptyChunk*]]]
   */
  def collectAllPar[A](as: NonEmptyChunk[Task[A]]): Task[NonEmptyChunk[A]] =
    ZIO.collectAllPar(as)

  /**
   * @see See [[[zio.ZIO.collectAllPar_[R,E,A](as:Iterable*]]]
   */
  def collectAllPar_[A](in: Iterable[Task[A]]): Task[Unit] =
    ZIO.collectAllPar_(in)

  /**
   * @see See [[zio.ZIO.collectAllParN]]
   */
  def collectAllParN[A, Collection[+Element] <: Iterable[Element]](
    n: Int
  )(as: Collection[Task[A]])(implicit bf: BuildFrom[Collection[Task[A]], A, Collection[A]]): Task[Collection[A]] =
    ZIO.collectAllParN(n)(as)

  /**
   * @see See [[zio.ZIO.collectAllParN_]]
   */
  def collectAllParN_[A](n: Int)(as: Iterable[Task[A]]): Task[Unit] =
    ZIO.collectAllParN_(n)(as)

  /**
   * @see See [[zio.ZIO.collectAllSuccesses]]
   */
  def collectAllSuccesses[A, Collection[+Element] <: Iterable[Element]](
    in: Collection[Task[A]]
  )(implicit bf: BuildFrom[Collection[Task[A]], A, Collection[A]]): UIO[Collection[A]] =
    ZIO.collectAllSuccesses(in)

  /**
   * @see See [[zio.ZIO.collectAllSuccessesPar]]
   */
  def collectAllSuccessesPar[A, Collection[+Element] <: Iterable[Element]](
    as: Collection[Task[A]]
  )(implicit bf: BuildFrom[Collection[Task[A]], A, Collection[A]]): UIO[Collection[A]] =
    ZIO.collectAllSuccessesPar(as)

  /**
   * @see See [[zio.ZIO.collectAllSuccessesParN]]
   */
  def collectAllSuccessesParN[A, Collection[+Element] <: Iterable[Element]](
    n: Int
  )(as: Collection[Task[A]])(implicit bf: BuildFrom[Collection[Task[A]], A, Collection[A]]): UIO[Collection[A]] =
    ZIO.collectAllSuccessesParN(n)(as)

  /**
   * @see See [[zio.ZIO.collectAllWith]]
   */
  def collectAllWith[A, B, Collection[+Element] <: Iterable[Element]](
    as: Collection[Task[A]]
  )(f: PartialFunction[A, B])(implicit bf: BuildFrom[Collection[Task[A]], B, Collection[B]]): Task[Collection[B]] =
    ZIO.collectAllWith(as)(f)

  /**
   * @see See [[zio.ZIO.collectAllWithPar]]
   */
  def collectAllWithPar[A, B, Collection[+Element] <: Iterable[Element]](
    as: Collection[Task[A]]
  )(f: PartialFunction[A, B])(implicit bf: BuildFrom[Collection[Task[A]], B, Collection[B]]): Task[Collection[B]] =
    ZIO.collectAllWithPar(as)(f)

  /**
   * @see See [[zio.ZIO.collectAllWithParN]]
   */
  def collectAllWithParN[A, B, Collection[+Element] <: Iterable[Element]](n: Int)(
    as: Collection[Task[A]]
  )(f: PartialFunction[A, B])(implicit bf: BuildFrom[Collection[Task[A]], B, Collection[B]]): Task[Collection[B]] =
    ZIO.collectAllWithParN(n)(as)(f)

  /**
   * @see See [[zio.ZIO.collectFirst]]
   */
  def collectFirst[A, B](as: Iterable[A])(f: A => Task[Option[B]]): Task[Option[B]] =
    ZIO.collectFirst(as)(f)

  /**
   * @see See [[zio.ZIO.collectPar]]
   */
  def collectPar[A, B, Collection[+Element] <: Iterable[Element]](
    in: Collection[A]
  )(f: A => IO[Option[Throwable], B])(implicit bf: BuildFrom[Collection[A], B, Collection[B]]): Task[Collection[B]] =
    ZIO.collectPar(in)(f)

  /**
   * @see See [[zio.ZIO.collectParN]]
   */
  def collectParN[A, B, Collection[+Element] <: Iterable[Element]](n: Int)(
    in: Collection[A]
  )(f: A => IO[Option[Throwable], B])(implicit bf: BuildFrom[Collection[A], B, Collection[B]]): Task[Collection[B]] =
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
   * @see See [[zio.ZIO.descriptor]]
   */
  def descriptor: UIO[Fiber.Descriptor] = ZIO.descriptor

  /**
   * @see See [[zio.ZIO.descriptorWith]]
   */
  def descriptorWith[A](f: Fiber.Descriptor => Task[A]): Task[A] =
    ZIO.descriptorWith(f)

  /**
   * @see See [[zio.ZIO.effect]]
   */
  def effect[A](effect: => A): Task[A] = ZIO.effect(effect)

  /**
   * @see See [[zio.ZIO.effectAsync]]
   */
  def effectAsync[A](register: (Task[A] => Unit) => Any, blockingOn: List[Fiber.Id] = Nil): Task[A] =
    ZIO.effectAsync(register, blockingOn)

  /**
   * @see See [[zio.ZIO.effectAsyncMaybe]]
   */
  def effectAsyncMaybe[A](
    register: (Task[A] => Unit) => Option[Task[A]],
    blockingOn: List[Fiber.Id] = Nil
  ): Task[A] =
    ZIO.effectAsyncMaybe(register, blockingOn)

  /**
   * @see See [[zio.ZIO.effectAsyncM]]
   */
  def effectAsyncM[A](register: (Task[A] => Unit) => Task[Any]): Task[A] =
    ZIO.effectAsyncM(register)

  /**
   * @see See [[zio.ZIO.effectAsyncInterrupt]]
   */
  def effectAsyncInterrupt[A](
    register: (Task[A] => Unit) => Either[Canceler[Any], Task[A]],
    blockingOn: List[Fiber.Id] = Nil
  ): Task[A] =
    ZIO.effectAsyncInterrupt(register, blockingOn)

  /**
   * @see See [[zio.RIO.effectSuspend]]
   */
  def effectSuspend[A](task: => Task[A]): Task[A] = ZIO.effectSuspend(task)

  /**
   * @see See [[zio.ZIO.effectSuspendTotal]]
   */
  def effectSuspendTotal[A](task: => Task[A]): Task[A] = ZIO.effectSuspendTotal(task)

  /**
   * @see See [[zio.ZIO.effectSuspendTotalWith]]
   */
  def effectSuspendTotalWith[A](p: (Platform, Fiber.Id) => Task[A]): Task[A] = ZIO.effectSuspendTotalWith(p)

  /**
   * @see See [[zio.RIO.effectSuspendWith]]
   */
  def effectSuspendWith[A](p: (Platform, Fiber.Id) => Task[A]): Task[A] = ZIO.effectSuspendWith(p)

  /**
   * @see See [[zio.ZIO.effectTotal]]
   */
  def effectTotal[A](effect: => A): UIO[A] = ZIO.effectTotal(effect)

  /**
   * @see See [[zio.ZIO.executor]]
   */
  def executor: UIO[Executor] =
    ZIO.executor

  /**
   * @see See [[zio.ZIO.exists]]
   */
  def exists[A](as: Iterable[A])(f: A => Task[Boolean]): Task[Boolean] =
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
  def filter[A, Collection[+Element] <: Iterable[Element]](
    as: Collection[A]
  )(f: A => Task[Boolean])(implicit bf: BuildFrom[Collection[A], A, Collection[A]]): Task[Collection[A]] =
    ZIO.filter(as)(f)

  /**
   * @see [[zio.ZIO.filter[R,E,A](as:Set*]]
   */
  def filter[A](as: Set[A])(f: A => Task[Boolean]): Task[Set[A]] =
    ZIO.filter(as)(f)

  /**
   * @see [[zio.ZIO.filterPar[R,E,A,Collection*]]
   */
  def filterPar[A, Collection[+Element] <: Iterable[Element]](
    as: Collection[A]
  )(f: A => Task[Boolean])(implicit bf: BuildFrom[Collection[A], A, Collection[A]]): Task[Collection[A]] =
    ZIO.filterPar(as)(f)

  /**
   * @see [[[zio.ZIO.filterPar[R,E,A](as:Set*]]]
   */
  def filterPar[A](as: Set[A])(f: A => Task[Boolean]): Task[Set[A]] =
    ZIO.filterPar(as)(f)

  /**
   * @see [[zio.ZIO.filterNot[R,E,A,Collection*]]
   */
  def filterNot[A, Collection[+Element] <: Iterable[Element]](
    as: Collection[A]
  )(f: A => Task[Boolean])(implicit bf: BuildFrom[Collection[A], A, Collection[A]]): Task[Collection[A]] =
    ZIO.filterNot(as)(f)

  /**
   * @see [[[zio.ZIO.filterNot[R,E,A](as:Set*]]]
   */
  def filterNot[A](as: Set[A])(f: A => Task[Boolean]): Task[Set[A]] =
    ZIO.filterNot(as)(f)

  /**
   * @see [[zio.ZIO.filterNotPar[R,E,A,Collection*]]
   */
  def filterNotPar[A, Collection[+Element] <: Iterable[Element]](
    as: Collection[A]
  )(f: A => Task[Boolean])(implicit bf: BuildFrom[Collection[A], A, Collection[A]]): Task[Collection[A]] =
    ZIO.filterNotPar(as)(f)

  /**
   * @see [[[zio.ZIO.filterNotPar[R,E,A](as:Set*]]]
   */
  def filterNotPar[A](as: Set[A])(f: A => Task[Boolean]): Task[Set[A]] =
    ZIO.filterNotPar(as)(f)

  /**
   * @see See [[zio.ZIO.firstSuccessOf]]
   */
  def firstSuccessOf[A](
    task: Task[A],
    rest: Iterable[Task[A]]
  ): Task[A] =
    ZIO.firstSuccessOf(task, rest)

  /**
   * @see See [[zio.ZIO.flatten]]
   */
  def flatten[A](task: Task[Task[A]]): Task[A] =
    ZIO.flatten(task)

  /**
   * @see See [[zio.ZIO.foldLeft]]
   */
  def foldLeft[S, A](in: Iterable[A])(zero: S)(f: (S, A) => Task[S]): Task[S] =
    ZIO.foldLeft(in)(zero)(f)

  /**
   * @see See [[zio.ZIO.foldRight]]
   */
  def foldRight[S, A](in: Iterable[A])(zero: S)(f: (A, S) => Task[S]): Task[S] =
    ZIO.foldRight(in)(zero)(f)

  /**
   * @see See [[zio.ZIO.forall]]
   */
  def forall[A](as: Iterable[A])(f: A => Task[Boolean]): Task[Boolean] =
    ZIO.forall(as)(f)

  /**
   * @see See [[[zio.ZIO.foreach[R,E,A,B,Collection[+Element]<:Iterable[Element]]*]]]
   */
  def foreach[A, B, Collection[+Element] <: Iterable[Element]](
    in: Collection[A]
  )(f: A => Task[B])(implicit bf: BuildFrom[Collection[A], B, Collection[B]]): Task[Collection[B]] =
    ZIO.foreach(in)(f)

  /**
   * @see See [[[zio.ZIO.foreach[R,E,A,B](in:Set*]]]
   */
  def foreach[A, B](in: Set[A])(f: A => Task[B]): Task[Set[B]] =
    ZIO.foreach(in)(f)

  /**
   * @see See [[[zio.ZIO.foreach[R,E,A,B](in:Array*]]]
   */
  def foreach[A, B: ClassTag](in: Array[A])(f: A => Task[B]): Task[Array[B]] =
    ZIO.foreach(in)(f)

  /**
   * @see See [[[zio.ZIO.foreach[R,E,Key,Key2,Value,Value2](map:Map*]]]
   */
  def foreach[Key, Key2, Value, Value2](
    map: Map[Key, Value]
  )(f: (Key, Value) => Task[(Key2, Value2)]): Task[Map[Key2, Value2]] =
    ZIO.foreach(map)(f)

  /**
   * @see See [[[zio.ZIO.foreach[R,E,A,B](in:Option*]]]
   */
  def foreach[A, B](in: Option[A])(f: A => Task[B]): Task[Option[B]] =
    ZIO.foreach(in)(f)

  /**
   * @see See [[[zio.ZIO.foreach[R,E,A,B](in:zio\.NonEmptyChunk*]]]
   */
  def foreach[A, B](in: NonEmptyChunk[A])(f: A => Task[B]): Task[NonEmptyChunk[B]] =
    ZIO.foreach(in)(f)

  /**
   * @see See [[zio.ZIO.foreachExec]]
   */
  final def foreachExec[A, B, Collection[+Element] <: Iterable[Element]](as: Collection[A])(
    exec: ExecutionStrategy
  )(f: A => Task[B])(implicit bf: BuildFrom[Collection[A], B, Collection[B]]): Task[Collection[B]] =
    ZIO.foreachExec(as)(exec)(f)

  /**
   * @see See [[[zio.ZIO.foreachPar[R,E,A,B,Collection[+Element]<:Iterable[Element]]*]]]
   */
  def foreachPar[A, B, Collection[+Element] <: Iterable[Element]](
    as: Collection[A]
  )(fn: A => Task[B])(implicit bf: BuildFrom[Collection[A], B, Collection[B]]): Task[Collection[B]] =
    ZIO.foreachPar(as)(fn)

  /**
   * @see See [[[zio.ZIO.foreachPar[R,E,A,B](as:Set*]]]
   */
  def foreachPar[A, B](as: Set[A])(fn: A => Task[B]): Task[Set[B]] =
    ZIO.foreachPar(as)(fn)

  /**
   * @see See [[[zio.ZIO.foreachPar[R,E,A,B](as:Array*]]]
   */
  def foreachPar[A, B: ClassTag](as: Array[A])(fn: A => Task[B]): Task[Array[B]] =
    ZIO.foreachPar(as)(fn)

  /**
   * @see See [[[zio.ZIO.foreachPar[R,E,Key,Key2,Value,Value2](map:Map*]]]
   */
  def foreachPar[Key, Key2, Value, Value2](
    map: Map[Key, Value]
  )(f: (Key, Value) => Task[(Key2, Value2)]): Task[Map[Key2, Value2]] =
    ZIO.foreachPar(map)(f)

  /**
   * @see See [[[zio.ZIO.foreachPar[R,E,A,B](as:zio\.NonEmptyChunk*]]]
   */
  def foreachPar[A, B](as: NonEmptyChunk[A])(fn: A => Task[B]): Task[NonEmptyChunk[B]] =
    ZIO.foreachPar(as)(fn)

  /**
   * @see See [[zio.ZIO.foreachParN]]
   */
  def foreachParN[A, B, Collection[+Element] <: Iterable[Element]](
    n: Int
  )(as: Collection[A])(fn: A => Task[B])(implicit bf: BuildFrom[Collection[A], B, Collection[B]]): Task[Collection[B]] =
    ZIO.foreachParN(n)(as)(fn)

  /**
   * @see See [[[zio.ZIO.foreach_[R,E,A](as:Iterable*]]]
   */
  def foreach_[A](as: Iterable[A])(f: A => Task[Any]): Task[Unit] =
    ZIO.foreach_(as)(f)

  /**
   * @see See [[[zio.ZIO.foreachPar_[R,E,A](as:Iterable*]]]
   */
  def foreachPar_[A, B](as: Iterable[A])(f: A => Task[Any]): Task[Unit] =
    ZIO.foreachPar_(as)(f)

  /**
   * @see See [[zio.ZIO.foreachParN_]]
   */
  def foreachParN_[A, B](n: Int)(as: Iterable[A])(f: A => Task[Any]): Task[Unit] =
    ZIO.foreachParN_(n)(as)(f)

  /**
   * @see See [[zio.ZIO.forkAll]]
   */
  def forkAll[A, Collection[+Element] <: Iterable[Element]](
    as: Collection[Task[A]]
  )(implicit bf: BuildFrom[Collection[Task[A]], A, Collection[A]]): UIO[Fiber[Throwable, Collection[A]]] =
    ZIO.forkAll(as)

  /**
   * @see See [[zio.ZIO.forkAll_]]
   */
  def forkAll_[A](as: Iterable[Task[A]]): UIO[Unit] =
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
   * @see [[zio.ZIO.fromFunction]]
   */
  def fromFunction[A](f: Any => A): Task[A] = ZIO.fromFunction(f)

  /**
   * @see See [[zio.ZIO.fromFutureInterrupt]]
   */
  def fromFutureInterrupt[A](make: ExecutionContext => scala.concurrent.Future[A]): Task[A] =
    ZIO.fromFutureInterrupt(make)

  /**
   * @see [[zio.ZIO.fromFunctionFuture]]
   */
  def fromFunctionFuture[A](f: Any => scala.concurrent.Future[A]): Task[A] =
    ZIO.fromFunctionFuture(f)

  /**
   * @see [[zio.ZIO.fromFunctionM]]
   */
  def fromFunctionM[A](f: Any => Task[A]): Task[A] = ZIO.fromFunctionM(f)

  /**
   * @see See [[zio.ZIO.fromFuture]]
   */
  def fromFuture[A](make: ExecutionContext => scala.concurrent.Future[A]): Task[A] =
    ZIO.fromFuture(make)

  /**
   * @see See [[zio.ZIO.fromTry]]
   */
  def fromTry[A](value: => scala.util.Try[A]): Task[A] =
    ZIO.fromTry(value)

  /**
   * @see See [[zio.ZIO.getOrFail]]
   */
  final def getOrFail[A](v: => Option[A]): Task[A] = ZIO.getOrFail(v)

  /**
   * @see See [[zio.ZIO.halt]]
   */
  def halt(cause: => Cause[Throwable]): Task[Nothing] = ZIO.halt(cause)

  /**
   * @see See [[zio.ZIO.haltWith]]
   */
  def haltWith[E <: Throwable](function: (() => ZTrace) => Cause[E]): Task[Nothing] =
    ZIO.haltWith(function)

  /**
   * @see [[zio.ZIO.identity]]
   */
  def identity: Task[Any] = ZIO.identity

  /**
   * @see [[zio.ZIO.ifM]]
   */
  def ifM(b: Task[Boolean]): ZIO.IfM[Any, Throwable] =
    ZIO.ifM(b)

  /**
   * @see [[zio.ZIO.ifM]]
   */
  def ifF(b: Task[Boolean]): ZIO.IfF[Any, Throwable] =
    ZIO.ifF(b)

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
  def interruptible[A](task: Task[A]): Task[A] =
    ZIO.interruptible(task)

  /**
   * @see See [[zio.ZIO.interruptibleMask]]
   */
  def interruptibleMask[A](k: ZIO.InterruptStatusRestore => Task[A]): Task[A] =
    ZIO.interruptibleMask(k)

  /**
   * @see See [[zio.ZIO.iterate]]
   */
  def iterate[S](initial: S)(cont: S => Boolean)(body: S => Task[S]): Task[S] =
    ZIO.iterate(initial)(cont)(body)

  /**
   *  @see See [[zio.ZIO.left]]
   */
  def left[A](a: => A): Task[Either[A, Nothing]] = ZIO.left(a)

  /**
   * @see See [[zio.ZIO.lock]]
   */
  def lock[A](executor: => Executor)(task: Task[A]): Task[A] =
    ZIO.lock(executor)(task)

  /**
   *  @see See [[zio.ZIO.loop]]
   */
  def loop[A, S](initial: S)(cont: S => Boolean, inc: S => S)(body: S => Task[A]): Task[List[A]] =
    ZIO.loop(initial)(cont, inc)(body)

  /**
   *  @see See [[zio.ZIO.loop_]]
   */
  def loop_[S](initial: S)(cont: S => Boolean, inc: S => S)(body: S => Task[Any]): Task[Unit] =
    ZIO.loop_(initial)(cont, inc)(body)

  /**
   *  @see [[zio.ZIO.mapN[R,E,A,B,C]*]]
   */
  def mapN[A, B, C](task1: Task[A], task2: Task[B])(f: (A, B) => C): Task[C] =
    ZIO.mapN(task1, task2)(f)

  /**
   *  @see [[zio.ZIO.mapN[R,E,A,B,C,D]*]]
   */
  def mapN[A, B, C, D](task1: Task[A], task2: Task[B], task3: Task[C])(f: (A, B, C) => D): Task[D] =
    ZIO.mapN(task1, task2, task3)(f)

  /**
   *  @see [[zio.ZIO.mapN[R,E,A,B,C,D,F]*]]
   */
  def mapN[A, B, C, D, F](task1: Task[A], task2: Task[B], task3: Task[C], task4: Task[D])(
    f: (A, B, C, D) => F
  ): Task[F] =
    ZIO.mapN(task1, task2, task3, task4)(f)

  /**
   *  @see [[zio.ZIO.mapParN[R,E,A,B,C]*]]
   */
  def mapParN[A, B, C](task1: Task[A], task2: Task[B])(f: (A, B) => C): Task[C] =
    ZIO.mapParN(task1, task2)(f)

  /**
   *  @see [[zio.ZIO.mapParN[R,E,A,B,C,D]*]]
   */
  def mapParN[A, B, C, D](task1: Task[A], task2: Task[B], task3: Task[C])(f: (A, B, C) => D): Task[D] =
    ZIO.mapParN(task1, task2, task3)(f)

  /**
   *  @see [[zio.ZIO.mapParN[R,E,A,B,C,D,F]*]]
   */
  def mapParN[A, B, C, D, F](task1: Task[A], task2: Task[B], task3: Task[C], task4: Task[D])(
    f: (A, B, C, D) => F
  ): Task[F] =
    ZIO.mapParN(task1, task2, task3, task4)(f)

  /**
   * @see See [[zio.ZIO.memoize]]
   */
  def memoize[A, B](f: A => Task[B]): UIO[A => Task[B]] =
    ZIO.memoize(f)

  /**
   * @see See [[zio.ZIO.mergeAll]]
   */
  def mergeAll[A, B](in: Iterable[Task[A]])(zero: B)(f: (B, A) => B): Task[B] =
    ZIO.mergeAll(in)(zero)(f)

  /**
   * @see See [[zio.ZIO.mergeAllPar]]
   */
  def mergeAllPar[A, B](in: Iterable[Task[A]])(zero: B)(f: (B, A) => B): Task[B] =
    ZIO.mergeAllPar(in)(zero)(f)

  /**
   * @see See [[zio.ZIO.never]]
   */
  val never: UIO[Nothing] = ZIO.never

  /**
   * @see See [[zio.ZIO.none]]
   */
  val none: Task[Option[Nothing]] = ZIO.none

  /**
   * @see See [[zio.ZIO.noneOrFail]]
   */
  def noneOrFail(o: Option[Throwable]): Task[Unit] =
    ZIO.noneOrFail(o)

  /**
   * @see See [[zio.ZIO.noneOrFailWith]]
   */
  def noneOrFailWith[O](o: Option[O])(f: O => Throwable): Task[Unit] =
    ZIO.noneOrFailWith(o)(f)

  /**
   *  @see See [[zio.ZIO.not]]
   */
  def not(effect: Task[Boolean]): Task[Boolean] =
    ZIO.not(effect)

  /**
   * @see See [[zio.ZIO.partition]]
   */
  def partition[A, B](in: Iterable[A])(f: A => Task[B]): Task[(Iterable[Throwable], Iterable[B])] =
    ZIO.partition(in)(f)

  /**
   * @see See [[zio.ZIO.partitionPar]]
   */
  def partitionPar[A, B](in: Iterable[A])(f: A => Task[B]): Task[(Iterable[Throwable], Iterable[B])] =
    ZIO.partitionPar(in)(f)

  /**
   * @see See [[zio.ZIO.partitionParN]]
   */
  def partitionParN[A, B](n: Int)(in: Iterable[A])(f: A => Task[B]): Task[(Iterable[Throwable], Iterable[B])] =
    ZIO.partitionParN(n)(in)(f)

  /**
   * @see See [[zio.ZIO.raceAll]]
   */
  def raceAll[A](task: Task[A], ios: Iterable[Task[A]]): Task[A] =
    ZIO.raceAll(task, ios)

  /**
   * @see See [[zio.ZIO.reduceAll]]
   */
  def reduceAll[A](a: Task[A], as: Iterable[Task[A]])(f: (A, A) => A): Task[A] =
    ZIO.reduceAll(a, as)(f)

  /**
   * @see See [[zio.ZIO.reduceAllPar]]
   */
  def reduceAllPar[A](a: Task[A], as: Iterable[Task[A]])(f: (A, A) => A): Task[A] =
    ZIO.reduceAllPar(a, as)(f)

  /**
   * @see See [[zio.ZIO.replicate]]
   */
  def replicate[A](n: Int)(effect: Task[A]): Iterable[Task[A]] =
    ZIO.replicate(n)(effect)

  /**
   * @see See [[zio.ZIO.replicateM]]
   */
  def replicateM[A](n: Int)(effect: Task[A]): Task[Iterable[A]] =
    ZIO.replicateM(n)(effect)

  /**
   * @see See [[zio.ZIO.replicateM_]]
   */
  def replicateM_[A](n: Int)(effect: Task[A]): Task[Unit] =
    ZIO.replicateM_(n)(effect)

  /**
   * @see See [[zio.ZIO.require]]
   */
  def require[A](error: => Throwable): Task[Option[A]] => Task[A] =
    ZIO.require[Any, Throwable, A](error)

  /**
   * @see See [[zio.ZIO.reserve]]
   */
  def reserve[A, B](reservation: Task[Reservation[Any, Throwable, A]])(use: A => Task[B]): Task[B] =
    ZIO.reserve(reservation)(use)

  /**
   *  @see [[zio.ZIO.right]]
   */
  def right[B](b: => B): Task[Either[Nothing, B]] = ZIO.right(b)

  /**
   * @see See [[zio.ZIO.runtime]]
   */
  def runtime: UIO[Runtime[Any]] = ZIO.runtime

  /**
   * @see See [[zio.ZIO.succeed]]
   */
  def succeed[A](a: => A): UIO[A] = ZIO.succeed(a)

  /**
   *  @see [[zio.ZIO.some]]
   */
  def some[A](a: => A): Task[Option[A]] = ZIO.some(a)

  /**
   * @see See [[zio.ZIO.trace]]
   */
  def trace: UIO[ZTrace] = ZIO.trace

  /**
   * @see See [[zio.ZIO.traced]]
   */
  def traced[A](task: Task[A]): Task[A] = ZIO.traced(task)

  /**
   * @see See [[zio.ZIO.unit]]
   */
  val unit: UIO[Unit] = ZIO.unit

  /**
   * @see See [[zio.ZIO.uninterruptible]]
   */
  def uninterruptible[A](task: Task[A]): Task[A] =
    ZIO.uninterruptible(task)

  /**
   * @see See [[zio.ZIO.uninterruptibleMask]]
   */
  def uninterruptibleMask[A](k: ZIO.InterruptStatusRestore => Task[A]): Task[A] =
    ZIO.uninterruptibleMask(k)

  /**
   * @see See [[zio.ZIO.unless]]
   */
  def unless(b: => Boolean)(zio: => Task[Any]): Task[Unit] =
    ZIO.unless(b)(zio)

  /**
   * @see See [[zio.ZIO.unlessM]]
   */
  def unlessM(b: Task[Boolean]): ZIO.UnlessM[Any, Throwable] =
    ZIO.unlessM(b)

  /**
   * @see [[zio.ZIO.unsandbox]]
   */
  def unsandbox[A](v: IO[Cause[Throwable], A]): Task[A] = ZIO.unsandbox(v)

  /**
   * @see See [[zio.ZIO.untraced]]
   */
  def untraced[A](task: Task[A]): Task[A] = ZIO.untraced(task)

  /**
   * @see See [[zio.ZIO.when]]
   */
  def when(b: => Boolean)(task: => Task[Any]): Task[Unit] =
    ZIO.when(b)(task)

  /**
   * @see See [[zio.ZIO.whenCase]]
   */
  def whenCase[A](a: => A)(pf: PartialFunction[A, Task[Any]]): Task[Unit] =
    ZIO.whenCase(a)(pf)

  /**
   * @see See [[zio.ZIO.whenCaseM]]
   */
  def whenCaseM[A](a: Task[A])(pf: PartialFunction[A, Task[Any]]): Task[Unit] =
    ZIO.whenCaseM(a)(pf)

  /**
   * @see See [[zio.ZIO.whenM]]
   */
  def whenM(b: Task[Boolean]): ZIO.WhenM[Any, Throwable] =
    ZIO.whenM(b)

  /**
   * @see See [[zio.ZIO.yieldNow]]
   */
  val yieldNow: UIO[Unit] = ZIO.yieldNow

  private[zio] def succeedNow[A](a: A): UIO[A] = ZIO.succeedNow(a)
}

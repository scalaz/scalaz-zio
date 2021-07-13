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

import com.github.ghik.silencer.silent
import zio._
import zio.internal.{Platform, Stack, Sync}

import java.util.concurrent.atomic.{AtomicBoolean, AtomicLong}
import java.util.{HashMap => MutableMap}
import scala.annotation.tailrec
import scala.collection.mutable.Builder
import scala.util.{Failure, Success, Try}

/**
 * `STM[E, A]` represents an effect that can be performed transactionally,
 * resulting in a failure `E` or a value `A`.
 *
 * {{{
 * def transfer(receiver: TRef[Int],
 *              sender: TRef[Int], much: Int): UIO[Int] =
 *   STM.atomically {
 *     for {
 *       balance <- sender.get
 *       _       <- STM.check(balance >= much)
 *       _       <- receiver.update(_ + much)
 *       _       <- sender.update(_ - much)
 *       newAmnt <- receiver.get
 *     } yield newAmnt
 *   }
 *
 *   val action: UIO[Int] =
 *     for {
 *       t <- STM.atomically(TRef.make(0).zip(TRef.make(20000)))
 *       (receiver, sender) = t
 *       balance <- transfer(receiver, sender, 1000)
 *     } yield balance
 * }}}
 *
 * Software Transactional Memory is a technique which allows composition
 *  of arbitrary atomic operations. It is the software analog of transactions in database systems.
 *
 * The API is lifted directly from the Haskell package Control.Concurrent.STM although the implementation does not
 *  resemble the Haskell one at all.
 *  [[http://hackage.haskell.org/package/stm-2.5.0.0/docs/Control-Concurrent-STM.html]]
 *
 *  STM in Haskell was introduced in:
 *  Composable memory transactions, by Tim Harris, Simon Marlow, Simon Peyton Jones, and Maurice Herlihy, in ACM
 *  Conference on Principles and Practice of Parallel Programming 2005.
 * [[https://www.microsoft.com/en-us/research/publication/composable-memory-transactions/]]
 *
 * See also:
 * Lock Free Data Structures using STMs in Haskell, by Anthony Discolo, Tim Harris, Simon Marlow, Simon Peyton Jones,
 * Satnam Singh) FLOPS 2006: Eighth International Symposium on Functional and Logic Programming, Fuji Susono, JAPAN,
 *  April 2006
 *  [[https://www.microsoft.com/en-us/research/publication/lock-free-data-structures-using-stms-in-haskell/]]
 */
final class ZSTM[-R, +E, +A] private[stm] (
  private val exec: (ZSTM.internal.Journal, Fiber.Id, AtomicLong, R) => ZSTM.internal.TExit[E, A]
) extends AnyVal { self =>
  import ZSTM.internal.{prepareResetJournal, TExit}

  /**
   * A symbolic alias for `orDie`.
   */
  final def !(implicit ev: E <:< Throwable, ev2: CanFail[E]): ZSTM[R, Nothing, A] =
    self.orDie

  /**
   * Alias for `<*>` and `zip`.
   */
  def &&&[R1 <: R, E1 >: E, B](that: ZSTM[R1, E1, B]): ZSTM[R1, E1, (A, B)] =
    self <*> that

  /**
   * Splits the environment, providing the first part to this effect and the
   * second part to that effect.
   */
  def ***[R1, E1 >: E, B](that: ZSTM[R1, E1, B]): ZSTM[(R, R1), E1, (A, B)] =
    (ZSTM.first >>> self) &&& (ZSTM.second >>> that)

  /**
   * Sequentially zips this value with the specified one, discarding the
   * first element of the tuple.
   */
  def *>[R1 <: R, E1 >: E, B](that: => ZSTM[R1, E1, B]): ZSTM[R1, E1, B] =
    self zipRight that

  /**
   * Depending on provided environment, returns either this one or the other
   * effect lifted in `Left` or `Right`, respectively.
   */
  def +++[R1, B, E1 >: E](that: ZSTM[R1, E1, B]): ZSTM[Either[R, R1], E1, Either[A, B]] =
    ZSTM.accessM[Either[R, R1]](_.fold(self.provide(_).map(Left(_)), that.provide(_).map(Right(_))))

  /**
   * Sequentially zips this value with the specified one, discarding the
   * second element of the tuple.
   */
  def <*[R1 <: R, E1 >: E, B](that: => ZSTM[R1, E1, B]): ZSTM[R1, E1, A] =
    self zipLeft that

  /**
   * Sequentially zips this value with the specified one.
   */
  def <*>[R1 <: R, E1 >: E, B](that: => ZSTM[R1, E1, B]): ZSTM[R1, E1, (A, B)] =
    self zip that

  /**
   * A symbolic alias for `orElseEither`.
   */
  def <+>[R1 <: R, E1, B](that: => ZSTM[R1, E1, B]): ZSTM[R1, E1, Either[A, B]] =
    self.orElseEither(that)

  /**
   * Propagates self environment to that.
   */
  def <<<[R1, E1 >: E](that: ZSTM[R1, E1, R]): ZSTM[R1, E1, A] =
    that >>> self

  /**
   * Tries this effect first, and if it fails or retries, tries the other effect.
   */
  def <>[R1 <: R, E1, A1 >: A](that: => ZSTM[R1, E1, A1]): ZSTM[R1, E1, A1] =
    orElse(that)

  /**
   * Tries this effect first, and if it enters retry, then it tries the other effect. This is
   * an equivalent of haskell's orElse.
   */
  def <|>[R1 <: R, E1 >: E, A1 >: A](that: => ZSTM[R1, E1, A1]): ZSTM[R1, E1, A1] =
    orTry(that)

  /**
   * Feeds the value produced by this effect to the specified function,
   * and then runs the returned effect as well to produce its results.
   */
  def >>=[R1 <: R, E1 >: E, B](f: A => ZSTM[R1, E1, B]): ZSTM[R1, E1, B] =
    self flatMap f

  /**
   * Propagates the given environment to self.
   */
  def >>>[E1 >: E, B](that: ZSTM[A, E1, B]): ZSTM[R, E1, B] =
    flatMap(that.provide)

  /**
   * Depending on provided environment returns either this one or the other effect.
   */
  def |||[R1, E1 >: E, A1 >: A](that: ZSTM[R1, E1, A1]): ZSTM[Either[R, R1], E1, A1] =
    ZSTM.accessM[Either[R, R1]](_.fold(self.provide, that.provide))

  /**
   * Returns an effect that submerges the error case of an `Either` into the
   * `STM`. The inverse operation of `STM.either`.
   */
  def absolve[E1 >: E, B](implicit ev: A <:< Either[E1, B]): ZSTM[R, E1, B] =
    ZSTM.absolve(self.map(ev))

  /**
   * Named alias for `>>>`.
   */
  def andThen[E1 >: E, B](that: ZSTM[A, E1, B]): ZSTM[R, E1, B] =
    self >>> that

  /**
   * Maps the success value of this effect to the specified constant value.
   */
  def as[B](b: => B): ZSTM[R, E, B] = self map (_ => b)

  /**
   * Maps the success value of this effect to an optional value.
   */
  def asSome: ZSTM[R, E, Option[A]] =
    map(Some(_))

  /**
   * Maps the error value of this effect to an optional value.
   */
  def asSomeError: ZSTM[R, Option[E], A] =
    mapError(Some(_))

  /**
   * Returns an `STM` effect whose failure and success channels have been mapped by
   * the specified pair of functions, `f` and `g`.
   */
  @deprecated("use mapBoth", "2.0.0")
  def bimap[E2, B](f: E => E2, g: A => B)(implicit ev: CanFail[E]): ZSTM[R, E2, B] =
    mapBoth(f, g)

  /**
   * Recovers from all errors.
   */
  def catchAll[R1 <: R, E2, A1 >: A](h: E => ZSTM[R1, E2, A1])(implicit ev: CanFail[E]): ZSTM[R1, E2, A1] =
    foldM[R1, E2, A1](h, ZSTM.succeedNow)

  /**
   * Recovers from some or all of the error cases.
   */
  def catchSome[R1 <: R, E1 >: E, A1 >: A](
    pf: PartialFunction[E, ZSTM[R1, E1, A1]]
  )(implicit ev: CanFail[E]): ZSTM[R1, E1, A1] =
    catchAll(pf.applyOrElse(_, (e: E) => ZSTM.fail(e)))

  /**
   * Simultaneously filters and maps the value produced by this effect.
   */
  def collect[B](pf: PartialFunction[A, B]): ZSTM[R, E, B] =
    collectM(pf.andThen(ZSTM.succeedNow(_)))

  /**
   * Simultaneously filters and flatMaps the value produced by this effect.
   * Continues on the effect returned from pf.
   */
  def collectM[R1 <: R, E1 >: E, B](pf: PartialFunction[A, ZSTM[R1, E1, B]]): ZSTM[R1, E1, B] =
    self.continueWithM {
      case TExit.Fail(e)    => ZSTM.fail(e)
      case TExit.Die(t)     => ZSTM.die(t)
      case TExit.Succeed(a) => if (pf.isDefinedAt(a)) pf(a) else ZSTM.retry
      case TExit.Retry      => ZSTM.retry
    }

  /**
   * Named alias for `<<<`.
   */
  def compose[R1, E1 >: E](that: ZSTM[R1, E1, R]): ZSTM[R1, E1, A] =
    self <<< that

  /**
   * Commits this transaction atomically.
   */
  def commit: ZIO[R, E, A] = ZSTM.atomically(self)

  /**
   * Commits this transaction atomically, regardless of whether the transaction
   * is a success or a failure.
   */
  def commitEither: ZIO[R, E, A] =
    either.commit.absolve

  /**
   * Repeats this `STM` effect until its result satisfies the specified predicate.
   * '''WARNING''': `repeatUntil` uses a busy loop to repeat the effect and will consume a
   * thread until it completes (it cannot yield). This is because STM describes a single atomic transaction
   * which must either complete, retry or fail a transaction before yielding back to the ZIO Runtime.
   * - Use [[retryUntil]] instead if you don't need to maintain transaction state for repeats.
   * - Ensure repeating the STM effect will eventually satisfy the predicate.
   * - Consider using the Blocking thread pool for execution of the transaction.
   */
  def repeatUntil(f: A => Boolean): ZSTM[R, E, A] =
    flatMap(a => if (f(a)) ZSTM.succeedNow(a) else repeatUntil(f))

  /**
   * Repeats this `STM` effect while its result satisfies the specified predicate.
   * '''WARNING''': `repeatWhile` uses a busy loop to repeat the effect and will consume a
   * thread until it completes (it cannot yield). This is because STM describes a single atomic transaction
   * which must either complete, retry or fail a transaction before yielding back to the ZIO Runtime.
   * - Use [[retryWhile]] instead if you don't need to maintain transaction state for repeats.
   * - Ensure repeating the STM effect will eventually not satisfy the predicate.
   * - Consider using the Blocking thread pool for execution of the transaction.
   */
  def repeatWhile(f: A => Boolean): ZSTM[R, E, A] =
    flatMap(a => if (f(a)) repeatWhile(f) else ZSTM.succeedNow(a))

  /**
   * Converts the failure channel into an `Either`.
   */
  def either(implicit ev: CanFail[E]): URSTM[R, Either[E, A]] =
    fold(Left(_), Right(_))

  /**
   * Executes the specified finalization transaction whether or
   * not this effect succeeds. Note that as with all STM transactions,
   * if the full transaction fails, everything will be rolled back.
   */
  def ensuring[R1 <: R](finalizer: ZSTM[R1, Nothing, Any]): ZSTM[R1, E, A] =
    foldM(e => finalizer *> ZSTM.fail(e), a => finalizer *> ZSTM.succeedNow(a))

  /**
   * Returns an effect that ignores errors and runs repeatedly until it eventually succeeds.
   */
  def eventually(implicit ev: CanFail[E]): URSTM[R, A] =
    foldM(_ => eventually, ZSTM.succeedNow)

  /**
   * Dies with specified `Throwable` if the predicate fails.
   */
  def filterOrDie(p: A => Boolean)(t: => Throwable): ZSTM[R, E, A] =
    filterOrElse_(p)(ZSTM.die(t))

  /**
   * Dies with a [[java.lang.RuntimeException]] having the specified text message
   * if the predicate fails.
   */
  def filterOrDieMessage(p: A => Boolean)(msg: => String): ZSTM[R, E, A] =
    filterOrElse_(p)(ZSTM.dieMessage(msg))

  /**
   * Applies `f` if the predicate fails.
   */
  def filterOrElse[R1 <: R, E1 >: E, A1 >: A](p: A => Boolean)(f: A => ZSTM[R1, E1, A1]): ZSTM[R1, E1, A1] =
    flatMap(v => if (!p(v)) f(v) else ZSTM.succeedNow(v))

  /**
   * Supplies `zstm` if the predicate fails.
   */
  def filterOrElse_[R1 <: R, E1 >: E, A1 >: A](p: A => Boolean)(zstm: => ZSTM[R1, E1, A1]): ZSTM[R1, E1, A1] =
    filterOrElse[R1, E1, A1](p)(_ => zstm)

  /**
   * Fails with `e` if the predicate fails.
   */
  def filterOrFail[E1 >: E](p: A => Boolean)(e: => E1): ZSTM[R, E1, A] =
    filterOrElse_[R, E1, A](p)(ZSTM.fail(e))

  /**
   * Feeds the value produced by this effect to the specified function,
   * and then runs the returned effect as well to produce its results.
   */
  def flatMap[R1 <: R, E1 >: E, B](f: A => ZSTM[R1, E1, B]): ZSTM[R1, E1, B] =
    self.continueWithM {
      case TExit.Succeed(a) => f(a)
      case TExit.Fail(e)    => ZSTM.fail(e)
      case TExit.Die(t)     => ZSTM.die(t)
      case TExit.Retry      => ZSTM.retry
    }

  /**
   * Creates a composite effect that represents this effect followed by another
   * one that may depend on the error produced by this one.
   */
  def flatMapError[R1 <: R, E2](f: E => ZSTM[R1, Nothing, E2])(implicit ev: CanFail[E]): ZSTM[R1, E2, A] =
    foldM(e => f(e).flip, ZSTM.succeedNow)

  /**
   * Flattens out a nested `STM` effect.
   */
  def flatten[R1 <: R, E1 >: E, B](implicit ev: A <:< ZSTM[R1, E1, B]): ZSTM[R1, E1, B] =
    self flatMap ev

  /**
   * Unwraps the optional error, defaulting to the provided value.
   */
  def flattenErrorOption[E1, E2 <: E1](default: => E2)(implicit ev: E <:< Option[E1]): ZSTM[R, E1, A] =
    mapError(e => ev(e).getOrElse(default))

  /**
   * Flips the success and failure channels of this transactional effect. This
   * allows you to use all methods on the error channel, possibly before
   * flipping back.
   */
  def flip(implicit ev: CanFail[E]): ZSTM[R, A, E] =
    foldM(ZSTM.succeedNow, ZSTM.fail(_))

  /**
   *  Swaps the error/value parameters, applies the function `f` and flips the parameters back
   */
  def flipWith[R1, A1, E1](f: ZSTM[R, A, E] => ZSTM[R1, A1, E1]): ZSTM[R1, E1, A1] =
    f(flip).flip

  /**
   * Folds over the `STM` effect, handling both failure and success, but not
   * retry.
   */
  def fold[B](f: E => B, g: A => B)(implicit ev: CanFail[E]): URSTM[R, B] =
    self.continueWithM {
      case TExit.Fail(e)    => ZSTM.succeedNow(f(e))
      case TExit.Die(t)     => ZSTM.die(t)
      case TExit.Succeed(a) => ZSTM.succeedNow(g(a))
      case TExit.Retry      => ZSTM.retry
    }

  /**
   * Effectfully folds over the `STM` effect, handling both failure and
   * success.
   */
  def foldM[R1 <: R, E1, B](f: E => ZSTM[R1, E1, B], g: A => ZSTM[R1, E1, B])(implicit
    ev: CanFail[E]
  ): ZSTM[R1, E1, B] =
    self.continueWithM {
      case TExit.Fail(e)    => f(e)
      case TExit.Die(t)     => ZSTM.die(t)
      case TExit.Succeed(a) => g(a)
      case TExit.Retry      => ZSTM.retry
    }

  /**
   * Repeats this effect forever (until the first error).
   * '''WARNING''': `forever` uses a busy loop to repeat the effect and will consume a
   * thread until it fails. This is because STM describes a single atomic transaction, and so must either complete,
   * retry or fail a transaction before yielding back to the ZIO Runtime.
   * - Ensure repeating the STM effect will eventually fail.
   * - Consider using the Blocking thread pool for execution of the transaction.
   */
  @deprecated(
    "Repeating until failure doesn't make sense in the context of STM because it will always roll back the transaction",
    "1.0.2"
  )
  def forever: ZSTM[R, E, Nothing] = self *> self.forever

  /**
   * Unwraps the optional success of this effect, but can fail with None value.
   */
  def get[B](implicit ev1: E <:< Nothing, ev2: A <:< Option[B]): ZSTM[R, Option[Nothing], B] =
    foldM(
      ev1,
      ev2(_).fold[ZSTM[R, Option[Nothing], B]](ZSTM.fail(None))(ZSTM.succeedNow(_))
    )(CanFail)

  /**
   * Returns a successful effect with the head of the list if the list is
   * non-empty or fails with the error `None` if the list is empty.
   */
  def head[B](implicit ev: A <:< List[B]): ZSTM[R, Option[E], B] =
    foldM(
      e => ZSTM.fail(Some(e)),
      ev(_).headOption.fold[ZSTM[R, Option[E], B]](ZSTM.fail(None))(ZSTM.succeedNow)
    )

  /**
   * Returns a new effect that ignores the success or failure of this effect.
   */
  def ignore: URSTM[R, Unit] = self.fold(ZIO.unitFn, ZIO.unitFn)

  /**
   * Returns whether this transactional effect is a failure.
   */
  def isFailure: ZSTM[R, Nothing, Boolean] =
    fold(_ => true, _ => false)

  /**
   * Returns whether this transactional effect is a success.
   */
  def isSuccess: ZSTM[R, Nothing, Boolean] =
    fold(_ => false, _ => true)

  /**
   * Returns a successful effect if the value is `Left`, or fails with the error `None`.
   */
  def left[B, C](implicit ev: A <:< Either[B, C]): ZSTM[R, Option[E], B] =
    foldM(
      e => ZSTM.fail(Some(e)),
      a => ev(a).fold(ZSTM.succeedNow, _ => ZSTM.fail(None))
    )

  /**
   * Returns a successful effect if the value is `Left`, or fails with the error e.
   */
  def leftOrFail[B, C, E1 >: E](e: => E1)(implicit ev: A <:< Either[B, C]): ZSTM[R, E1, B] =
    flatMap(ev(_) match {
      case Right(_)    => ZSTM.fail(e)
      case Left(value) => ZSTM.succeedNow(value)
    })

  /**
   * Returns a successful effect if the value is `Left`, or fails with
   * a [[java.util.NoSuchElementException]].
   */
  def leftOrFailException[B, C, E1 >: NoSuchElementException](implicit
    ev: A <:< Either[B, C],
    ev2: E <:< E1
  ): ZSTM[R, E1, B] =
    foldM(
      e => ZSTM.fail(ev2(e)),
      a => ev(a).fold(ZSTM.succeedNow(_), _ => ZSTM.fail(new NoSuchElementException("Either.left.get on Right")))
    )

  /**
   * Maps the value produced by the effect.
   */
  def map[B](f: A => B): ZSTM[R, E, B] =
    self.continueWithM {
      case TExit.Succeed(a) => ZSTM.succeedNow(f(a))
      case TExit.Fail(e)    => ZSTM.fail(e)
      case TExit.Die(t)     => ZSTM.die(t)
      case TExit.Retry      => ZSTM.retry
    }

  /**
   * Returns an `STM` effect whose failure and success channels have been mapped by
   * the specified pair of functions, `f` and `g`.
   */
  def mapBoth[E2, B](f: E => E2, g: A => B)(implicit ev: CanFail[E]): ZSTM[R, E2, B] =
    foldM(e => ZSTM.fail(f(e)), a => ZSTM.succeedNow(g(a)))

  /**
   * Maps from one error type to another.
   */
  def mapError[E1](f: E => E1)(implicit ev: CanFail[E]): ZSTM[R, E1, A] =
    self.continueWithM {
      case TExit.Succeed(a) => ZSTM.succeedNow(a)
      case TExit.Fail(e)    => ZSTM.fail(f(e))
      case TExit.Die(t)     => ZSTM.die(t)
      case TExit.Retry      => ZSTM.retry
    }

  /**
   * Maps the value produced by the effect with the specified function that may
   * throw exceptions but is otherwise pure, translating any thrown exceptions
   * into typed failed effects.
   */
  def mapPartial[B](f: A => B)(implicit ev: E <:< Throwable): ZSTM[R, Throwable, B] =
    foldM(e => ZSTM.fail(ev(e)), a => ZSTM.partial(f(a)))

  /**
   * Returns a new effect where the error channel has been merged into the
   * success channel to their common combined type.
   */
  def merge[A1 >: A](implicit ev1: E <:< A1, ev2: CanFail[E]): URSTM[R, A1] =
    foldM(e => ZSTM.succeedNow(ev1(e)), ZSTM.succeedNow)

  /**
   * Requires the option produced by this value to be `None`.
   */
  def none[B](implicit ev: A <:< Option[B]): ZSTM[R, Option[E], Unit] =
    self.foldM(
      e => ZSTM.fail(Some(e)),
      _.fold[ZSTM[R, Option[E], Unit]](ZSTM.unit)(_ => ZSTM.fail(None))
    )

  /**
   * Propagates the success value to the first element of a tuple, but
   * passes the effect input `R` along unmodified as the second element
   * of the tuple.
   */
  def onFirst[R1 <: R]: ZSTM[R1, E, (A, R1)] =
    self <*> ZSTM.identity[R1]

  /**
   * Returns this effect if environment is on the left, otherwise returns
   * whatever is on the right unmodified. Note that the result is lifted
   * in either.
   */
  def onLeft[C]: ZSTM[Either[R, C], E, Either[A, C]] =
    self +++ ZSTM.identity[C]

  /**
   * Returns this effect if environment is on the right, otherwise returns
   * whatever is on the left unmodified. Note that the result is lifted
   * in either.
   */
  def onRight[C]: ZSTM[Either[C, R], E, Either[C, A]] =
    ZSTM.identity[C] +++ self

  /**
   * Propagates the success value to the second element of a tuple, but
   * passes the effect input `R` along unmodified as the first element
   * of the tuple.
   */
  def onSecond[R1 <: R]: ZSTM[R1, E, (R1, A)] =
    ZSTM.identity[R1] <*> self

  /**
   * Converts the failure channel into an `Option`.
   */
  def option(implicit ev: CanFail[E]): URSTM[R, Option[A]] =
    fold(_ => None, Some(_))

  /**
   * Converts an option on errors into an option on values.
   */
  def optional[E1](implicit ev: E <:< Option[E1]): ZSTM[R, E1, Option[A]] =
    foldM(
      _.fold[ZSTM[R, E1, Option[A]]](ZSTM.succeedNow(Option.empty[A]))(ZSTM.fail(_)),
      a => ZSTM.succeedNow(Some(a))
    )

  /**
   * Translates `STM` effect failure into death of the fiber, making all failures unchecked and
   * not a part of the type of the effect.
   */
  def orDie(implicit ev1: E <:< Throwable, ev2: CanFail[E]): URSTM[R, A] =
    orDieWith(ev1)

  /**
   * Keeps none of the errors, and terminates the fiber running the `STM`
   * effect with them, using the specified function to convert the `E`
   * into a `Throwable`.
   */
  def orDieWith(f: E => Throwable)(implicit ev: CanFail[E]): URSTM[R, A] =
    mapError(f).catchAll(ZSTM.die(_))

  /**
   * Named alias for `<>`.
   */
  def orElse[R1 <: R, E1, A1 >: A](that: => ZSTM[R1, E1, A1]): ZSTM[R1, E1, A1] =
    new ZSTM((journal, fiberId, stackSize, r) => {
      val reset = prepareResetJournal(journal)

      val continueM: TExit[E, A] => STM[E1, A1] = {
        case TExit.Fail(_)    => { reset(); that.provide(r) }
        case TExit.Die(t)     => ZSTM.die(t)
        case TExit.Succeed(a) => ZSTM.succeedNow(a)
        case TExit.Retry      => { reset(); that.provide(r) }
      }

      val framesCount = stackSize.incrementAndGet()

      if (framesCount > ZSTM.MaxFrames) {
        throw new ZSTM.Resumable(self.provide(r), Stack(continueM))
      } else {
        val continued =
          try {
            continueM(self.exec(journal, fiberId, stackSize, r))
          } catch {
            case res: ZSTM.Resumable[e, e1, a, b] =>
              res.ks.push(continueM.asInstanceOf[TExit[e, a] => STM[e1, b]])
              throw res
          }

        continued.exec(journal, fiberId, stackSize, r)
      }
    })

  /**
   * Returns a transactional effect that will produce the value of this effect
   * in left side, unless it fails or retries, in which case, it will produce the value
   * of the specified effect in right side.
   */
  def orElseEither[R1 <: R, E1, B](
    that: => ZSTM[R1, E1, B]
  ): ZSTM[R1, E1, Either[A, B]] =
    (self map (Left[A, B](_))) orElse (that map (Right[A, B](_)))

  /**
   * Tries this effect first, and if it fails or retries, fails with the specified error.
   */
  def orElseFail[E1](e1: => E1): ZSTM[R, E1, A] =
    orElse(ZSTM.fail(e1))

  /**
   * Returns an effect that will produce the value of this effect, unless it
   * fails with the `None` value, in which case it will produce the value of
   * the specified effect.
   */
  def orElseOptional[R1 <: R, E1, A1 >: A](
    that: => ZSTM[R1, Option[E1], A1]
  )(implicit ev: E <:< Option[E1]): ZSTM[R1, Option[E1], A1] =
    catchAll(ev(_).fold(that)(e => ZSTM.fail(Some(e))))

  /**
   * Tries this effect first, and if it fails or retries, succeeds with the specified
   * value.
   */
  def orElseSucceed[A1 >: A](a1: => A1): URSTM[R, A1] =
    orElse(ZSTM.succeedNow(a1))

  /**
   * Named alias for `<|>`.
   */
  def orTry[R1 <: R, E1 >: E, A1 >: A](that: => ZSTM[R1, E1, A1]): ZSTM[R1, E1, A1] =
    continueWithM {
      case TExit.Fail(e)    => ZSTM.fail(e)
      case TExit.Die(t)     => ZSTM.die(t)
      case TExit.Succeed(a) => ZSTM.succeedNow(a)
      case TExit.Retry      => that
    }

  /**
   * Provides the transaction its required environment, which eliminates
   * its dependency on `R`.
   */
  def provide(r: R): STM[E, A] =
    provideSome(_ => r)

  /**
   * Provides some of the environment required to run this effect,
   * leaving the remainder `R0`.
   */
  def provideSome[R0](f: R0 => R): ZSTM[R0, E, A] =
    new ZSTM((journal, fiberId, stackSize, r0) => {

      val framesCount = stackSize.incrementAndGet()

      if (framesCount > ZSTM.MaxFrames) {
        throw new ZSTM.Resumable(
          new ZSTM((journal, fiberId, stackSize, _) => self.exec(journal, fiberId, stackSize, f(r0))),
          Stack[TExit[E, A] => STM[E, A]]()
        )
      } else {
        // no need to catch resumable here
        self.exec(journal, fiberId, stackSize, f(r0))
      }
    })

  /**
   * Keeps some of the errors, and terminates the fiber with the rest.
   */
  def refineOrDie[E1](
    pf: PartialFunction[E, E1]
  )(implicit ev1: E <:< Throwable, ev2: CanFail[E]): ZSTM[R, E1, A] =
    refineOrDieWith(pf)(ev1)

  /**
   * Keeps some of the errors, and terminates the fiber with the rest, using
   * the specified function to convert the `E` into a `Throwable`.
   */
  def refineOrDieWith[E1](pf: PartialFunction[E, E1])(f: E => Throwable)(implicit ev: CanFail[E]): ZSTM[R, E1, A] =
    self.catchAll(err => (pf.lift(err)).fold[ZSTM[R, E1, A]](ZSTM.die(f(err)))(ZSTM.fail(_)))

  /**
   * Fail with the returned value if the `PartialFunction` matches, otherwise
   * continue with our held value.
   */
  def reject[E1 >: E](pf: PartialFunction[A, E1]): ZSTM[R, E1, A] =
    rejectM(pf.andThen(ZSTM.fail(_)))

  /**
   * Continue with the returned computation if the `PartialFunction` matches,
   * translating the successful match into a failure, otherwise continue with
   * our held value.
   */
  def rejectM[R1 <: R, E1 >: E](pf: PartialFunction[A, ZSTM[R1, E1, E1]]): ZSTM[R1, E1, A] =
    self.flatMap { v =>
      pf.andThen[ZSTM[R1, E1, A]](_.flatMap(ZSTM.fail(_)))
        .applyOrElse[A, ZSTM[R1, E1, A]](v, ZSTM.succeedNow)
    }

  /**
   * Filters the value produced by this effect, retrying the transaction until
   * the predicate returns true for the value.
   */
  def retryUntil(f: A => Boolean): ZSTM[R, E, A] =
    collect {
      case a if f(a) => a
    }

  /**
   * Filters the value produced by this effect, retrying the transaction while
   * the predicate returns true for the value.
   */
  def retryWhile(f: A => Boolean): ZSTM[R, E, A] =
    collect {
      case a if !f(a) => a
    }

  /**
   * Returns a successful effect if the value is `Right`, or fails with the error `None`.
   */
  def right[B, C](implicit ev: A <:< Either[B, C]): ZSTM[R, Option[E], C] =
    self.foldM(
      e => ZSTM.fail(Some(e)),
      a => ev(a).fold(_ => ZSTM.fail(None), ZSTM.succeedNow)
    )

  /**
   * Returns a successful effect if the value is `Right`, or fails with the
   * given error 'e'.
   */
  def rightOrFail[B, C, E1 >: E](e: => E1)(implicit ev: A <:< Either[B, C]): ZSTM[R, E1, C] =
    self.flatMap(ev(_) match {
      case Right(value) => ZSTM.succeedNow(value)
      case Left(_)      => ZSTM.fail(e)
    })

  /**
   * Returns a successful effect if the value is `Right`, or fails with
   * a [[java.util.NoSuchElementException]].
   */
  def rightOrFailException[B, C, E1 >: NoSuchElementException](implicit
    ev: A <:< Either[B, C],
    ev2: E <:< E1
  ): ZSTM[R, E1, C] =
    self.foldM(
      e => ZSTM.fail(ev2(e)),
      a => ev(a).fold(_ => ZSTM.fail(new NoSuchElementException("Either.right.get on Left")), ZSTM.succeedNow(_))
    )

  /**
   * Converts an option on values into an option on errors.
   */
  def some[B](implicit ev: A <:< Option[B]): ZSTM[R, Option[E], B] =
    self.foldM(
      e => ZSTM.fail(Some(e)),
      _.fold[ZSTM[R, Option[E], B]](ZSTM.fail(Option.empty[E]))(ZSTM.succeedNow)
    )

  /**
   * Extracts the optional value, or returns the given 'default'.
   */
  def someOrElse[B](default: => B)(implicit ev: A <:< Option[B]): ZSTM[R, E, B] =
    map(_.getOrElse(default))

  /**
   * Extracts the optional value, or executes the effect 'default'.
   */
  def someOrElseM[B, R1 <: R, E1 >: E](default: ZSTM[R1, E1, B])(implicit ev: A <:< Option[B]): ZSTM[R1, E1, B] =
    self.flatMap(ev(_) match {
      case Some(value) => ZSTM.succeedNow(value)
      case None        => default
    })

  /**
   * Extracts the optional value, or fails with the given error 'e'.
   */
  def someOrFail[B, E1 >: E](e: => E1)(implicit ev: A <:< Option[B]): ZSTM[R, E1, B] =
    flatMap(_.fold[ZSTM[R, E1, B]](ZSTM.fail(e))(ZSTM.succeedNow))

  /**
   * Extracts the optional value, or fails with a [[java.util.NoSuchElementException]]
   */
  def someOrFailException[B, E1 >: E](implicit
    ev: A <:< Option[B],
    ev2: NoSuchElementException <:< E1
  ): ZSTM[R, E1, B] =
    foldM(
      ZSTM.fail(_),
      _.fold[ZSTM[R, E1, B]](ZSTM.fail(ev2(new NoSuchElementException("None.get"))))(ZSTM.succeedNow)
    )

  /**
   * Summarizes a `STM` effect by computing a provided value before and after execution, and
   * then combining the values to produce a summary, together with the result of
   * execution.
   */
  def summarized[R1 <: R, E1 >: E, B, C](summary: ZSTM[R1, E1, B])(f: (B, B) => C): ZSTM[R1, E1, (C, A)] =
    for {
      start <- summary
      value <- self
      end   <- summary
    } yield (f(start, end), value)

  /**
   * "Peeks" at the success of transactional effect.
   */
  def tap[R1 <: R, E1 >: E](f: A => ZSTM[R1, E1, Any]): ZSTM[R1, E1, A] =
    flatMap(a => f(a).as(a))

  /**
   * "Peeks" at both sides of an transactional effect.
   */
  def tapBoth[R1 <: R, E1 >: E](f: E => ZSTM[R1, E1, Any], g: A => ZSTM[R1, E1, Any])(implicit
    ev: CanFail[E]
  ): ZSTM[R1, E1, A] =
    foldM(e => f(e) *> ZSTM.fail(e), a => g(a) as a)

  /**
   * "Peeks" at the error of the transactional effect.
   */
  def tapError[R1 <: R, E1 >: E](f: E => ZSTM[R1, E1, Any])(implicit ev: CanFail[E]): ZSTM[R1, E1, A] =
    foldM(e => f(e) *> ZSTM.fail(e), ZSTM.succeedNow)

  /**
   * Maps the success value of this effect to unit.
   */
  def unit: ZSTM[R, E, Unit] = as(())

  /**
   * The moral equivalent of `if (!p) exp`
   */
  def unless(b: => Boolean): ZSTM[R, E, Unit] =
    ZSTM.unless(b)(self)

  /**
   * The moral equivalent of `if (!p) exp` when `p` has side-effects
   */
  def unlessM[R1 <: R, E1 >: E](b: ZSTM[R1, E1, Boolean]): ZSTM[R1, E1, Unit] =
    ZSTM.unlessM(b)(self)

  /**
   * Updates a service in the environment of this effect.
   */
  final def updateService[M] =
    new ZSTM.UpdateService[R, E, A, M](self)

  /**
   * The moral equivalent of `if (p) exp`
   */
  def when(b: => Boolean): ZSTM[R, E, Unit] = ZSTM.when(b)(self)

  /**
   * The moral equivalent of `if (p) exp` when `p` has side-effects
   */
  def whenM[R1 <: R, E1 >: E](b: ZSTM[R1, E1, Boolean]): ZSTM[R1, E1, Unit] = ZSTM.whenM(b)(self)

  /**
   * Same as [[retryUntil]].
   */
  def withFilter(f: A => Boolean): ZSTM[R, E, A] = retryUntil(f)

  /**
   * Named alias for `<*>`.
   */
  def zip[R1 <: R, E1 >: E, B](that: => ZSTM[R1, E1, B]): ZSTM[R1, E1, (A, B)] =
    (self zipWith that)((a, b) => a -> b)

  /**
   * Named alias for `<*`.
   */
  def zipLeft[R1 <: R, E1 >: E, B](that: => ZSTM[R1, E1, B]): ZSTM[R1, E1, A] =
    (self zip that) map (_._1)

  /**
   * Named alias for `*>`.
   */
  def zipRight[R1 <: R, E1 >: E, B](that: => ZSTM[R1, E1, B]): ZSTM[R1, E1, B] =
    (self zip that) map (_._2)

  /**
   * Sequentially zips this value with the specified one, combining the values
   * using the specified combiner function.
   */
  def zipWith[R1 <: R, E1 >: E, B, C](that: => ZSTM[R1, E1, B])(f: (A, B) => C): ZSTM[R1, E1, C] =
    self flatMap (a => that map (b => f(a, b)))

  private def continueWithM[R1 <: R, E1, B](continueM: TExit[E, A] => ZSTM[R1, E1, B]): ZSTM[R1, E1, B] =
    new ZSTM((journal, fiberId, stackSize, r) => {
      val framesCount = stackSize.incrementAndGet()

      if (framesCount > ZSTM.MaxFrames) {
        throw new ZSTM.Resumable(self.provide(r), Stack(continueM.andThen(_.provide(r))))
      } else {
        val continued =
          try {
            continueM(self.exec(journal, fiberId, stackSize, r))
          } catch {
            case res: ZSTM.Resumable[e, e1, a, b] =>
              res.ks.push(continueM.asInstanceOf[TExit[e, a] => STM[e1, b]])
              throw res
          }

        continued.exec(journal, fiberId, stackSize, r)
      }
    })

  private def run(journal: ZSTM.internal.Journal, fiberId: Fiber.Id, r: R): TExit[E, A] = {
    type Cont = ZSTM.internal.TExit[Any, Any] => STM[Any, Any]

    val stackSize = new AtomicLong()
    val stack     = new Stack[Cont]()
    var current   = self.asInstanceOf[ZSTM[R, Any, Any]]
    var result    = null: TExit[Any, Any]

    while (result eq null) {
      try {
        val v = current.exec(journal, fiberId, stackSize, r)

        if (stack.isEmpty)
          result = v
        else {
          val next = stack.pop()
          current = next(v)
        }
      } catch {
        case cont: ZSTM.Resumable[_, _, _, _] =>
          current = cont.stm
          while (!cont.ks.isEmpty) stack.push(cont.ks.pop().asInstanceOf[Cont])
          stackSize.set(0)
      }
    }
    result.asInstanceOf[TExit[E, A]]
  }
}

object ZSTM {
  import internal._

  /**
   * Submerges the error case of an `Either` into the `STM`. The inverse
   * operation of `STM.either`.
   */
  def absolve[R, E, A](z: ZSTM[R, E, Either[E, A]]): ZSTM[R, E, A] =
    z.flatMap(fromEither(_))

  /**
   * Accesses the environment of the transaction.
   */
  def access[R]: AccessPartiallyApplied[R] =
    new AccessPartiallyApplied

  /**
   * Accesses the environment of the transaction to perform a transaction.
   */
  def accessM[R]: AccessMPartiallyApplied[R] =
    new AccessMPartiallyApplied

  /**
   * Atomically performs a batch of operations in a single transaction.
   */
  def atomically[R, E, A](stm: ZSTM[R, E, A]): ZIO[R, E, A] =
    ZIO.accessM[R] { r =>
      ZIO.effectSuspendTotalWith { (platform, fiberId) =>
        tryCommit(platform, fiberId, stm, r) match {
          case TryCommit.Done(io) => io // TODO: Interruptible in Suspend
          case TryCommit.Suspend(journal) =>
            val txnId     = makeTxnId()
            val done      = new AtomicBoolean(false)
            val interrupt = UIO(Sync(done)(done.set(true)))
            val async     = ZIO.effectAsync(tryCommitAsync(journal, platform, fiberId, stm, txnId, done, r))

            async ensuring interrupt
        }
      }
    }

  /**
   * Checks the condition, and if it's true, returns unit, otherwise, retries.
   */
  def check[R](p: => Boolean): URSTM[R, Unit] =
    suspend(if (p) STM.unit else retry)

  /**
   * Evaluate each effect in the structure from left to right, collecting the
   * the successful values and discarding the empty cases.
   */
  def collect[R, E, A, B, Collection[+Element] <: Iterable[Element]](
    in: Collection[A]
  )(f: A => ZSTM[R, Option[E], B])(implicit bf: BuildFrom[Collection[A], B, Collection[B]]): ZSTM[R, E, Collection[B]] =
    foreach[R, E, A, Option[B], Iterable](in)(a => f(a).optional).map(_.flatten).map(bf.fromSpecific(in))

  /**
   * Collects all the transactional effects in a collection, returning a single
   * transactional effect that produces a collection of values.
   */
  def collectAll[R, E, A, Collection[+Element] <: Iterable[Element]](
    in: Collection[ZSTM[R, E, A]]
  )(implicit bf: BuildFrom[Collection[ZSTM[R, E, A]], A, Collection[A]]): ZSTM[R, E, Collection[A]] =
    foreach(in)(ZIO.identityFn)

  /**
   * Collects all the transactional effects in a set, returning a single
   * transactional effect that produces a set of values.
   */
  def collectAll[R, E, A](in: Set[ZSTM[R, E, A]]): ZSTM[R, E, Set[A]] =
    foreach(in)(ZIO.identityFn)

  /**
   * Collects all the transactional effects, returning a single transactional
   * effect that produces `Unit`.
   *
   * Equivalent to `collectAll(i).unit`, but without the cost of building the
   * list of results.
   */
  def collectAll_[R, E, A](in: Iterable[ZSTM[R, E, A]]): ZSTM[R, E, Unit] =
    foreach_(in)(ZIO.identityFn)

  /**
   * Collects the first element of the `Iterable[A]` for which the effectual
   * function `f` returns `Some`.
   */
  def collectFirst[R, E, A, B](as: Iterable[A])(f: A => ZSTM[R, E, Option[B]]): ZSTM[R, E, Option[B]] =
    succeedNow(as.iterator).flatMap { iterator =>
      def loop: ZSTM[R, E, Option[B]] =
        if (iterator.hasNext) f(iterator.next()).flatMap(_.fold(loop)(some(_)))
        else none
      loop
    }

  /**
   * Similar to Either.cond, evaluate the predicate,
   * return the given A as success if predicate returns true, and the given E as error otherwise
   */
  def cond[E, A](predicate: Boolean, result: => A, error: => E): STM[E, A] =
    if (predicate) succeed(result) else fail(error)

  /**
   * Kills the fiber running the effect.
   */
  def die(t: => Throwable): USTM[Nothing] =
    new ZSTM((_, _, _, _) => TExit.Die(t))

  /**
   * Kills the fiber running the effect with a `RuntimeException` that contains
   * the specified message.
   */
  def dieMessage(m: => String): USTM[Nothing] =
    die(new RuntimeException(m))

  /**
   * Returns a value modelled on provided exit status.
   */
  def done[R, E, A](exit: => TExit[E, A]): ZSTM[R, E, A] =
    suspend(done(exit))

  /**
   * Retrieves the environment inside an stm.
   */
  def environment[R]: URSTM[R, R] =
    new ZSTM((_, _, _, r) => TExit.Succeed(r))

  /**
   * Determines whether any element of the `Iterable[A]` satisfies the
   * effectual predicate `f`.
   */
  def exists[R, E, A](as: Iterable[A])(f: A => ZSTM[R, E, Boolean]): ZSTM[R, E, Boolean] =
    succeedNow(as.iterator).flatMap { iterator =>
      def loop: ZSTM[R, E, Boolean] =
        if (iterator.hasNext) f(iterator.next()).flatMap(b => if (b) succeedNow(b) else loop)
        else succeedNow(false)
      loop
    }

  /**
   * Returns a value that models failure in the transaction.
   */
  def fail[E](e: => E): STM[E, Nothing] =
    new ZSTM((_, _, _, _) => TExit.Fail(e))

  /**
   * Returns the fiber id of the fiber committing the transaction.
   */
  val fiberId: USTM[Fiber.Id] = new ZSTM((_, fiberId, _, _) => TExit.Succeed(fiberId))

  /**
   * Filters the collection using the specified effectual predicate.
   */
  def filter[R, E, A, Collection[+Element] <: Iterable[Element]](
    as: Collection[A]
  )(f: A => ZSTM[R, E, Boolean])(implicit bf: BuildFrom[Collection[A], A, Collection[A]]): ZSTM[R, E, Collection[A]] =
    as.foldLeft[ZSTM[R, E, Builder[A, Collection[A]]]](ZSTM.succeed(bf.newBuilder(as))) { (zio, a) =>
      zio.zipWith(f(a))((as, p) => if (p) as += a else as)
    }.map(_.result())

  /**
   * Filters the set using the specified effectual predicate.
   */
  def filter[R, E, A](as: Set[A])(f: A => ZSTM[R, E, Boolean]): ZSTM[R, E, Set[A]] =
    filter[R, E, A, Iterable](as)(f).map(_.toSet)

  /**
   * Filters the collection using the specified effectual predicate, removing
   * all elements that satisfy the predicate.
   */
  def filterNot[R, E, A, Collection[+Element] <: Iterable[Element]](
    as: Collection[A]
  )(f: A => ZSTM[R, E, Boolean])(implicit bf: BuildFrom[Collection[A], A, Collection[A]]): ZSTM[R, E, Collection[A]] =
    filter(as)(f(_).map(!_))

  /**
   * Filters the set using the specified effectual predicate, removing all
   * elements that satisfy the predicate.
   */
  def filterNot[R, E, A](as: Set[A])(f: A => ZSTM[R, E, Boolean]): ZSTM[R, E, Set[A]] =
    filterNot[R, E, A, Iterable](as)(f).map(_.toSet)

  /**
   * Returns an effectful function that extracts out the first element of a
   * tuple.
   */
  def first[A]: ZSTM[(A, Any), Nothing, A] =
    fromFunction(_._1)

  /**
   * Returns an effect that first executes the outer effect, and then executes
   * the inner effect, returning the value from the inner effect, and effectively
   * flattening a nested effect.
   */
  def flatten[R, E, A](tx: ZSTM[R, E, ZSTM[R, E, A]]): ZSTM[R, E, A] =
    tx.flatMap(ZIO.identityFn)

  /**
   * Folds an Iterable[A] using an effectual function f, working sequentially from left to right.
   */
  def foldLeft[R, E, S, A](
    in: Iterable[A]
  )(zero: S)(f: (S, A) => ZSTM[R, E, S]): ZSTM[R, E, S] =
    in.foldLeft(ZSTM.succeedNow(zero): ZSTM[R, E, S])((acc, el) => acc.flatMap(f(_, el)))

  /**
   * Folds an Iterable[A] using an effectual function f, working sequentially from right to left.
   */
  def foldRight[R, E, S, A](
    in: Iterable[A]
  )(zero: S)(f: (A, S) => ZSTM[R, E, S]): ZSTM[R, E, S] =
    in.foldRight(ZSTM.succeedNow(zero): ZSTM[R, E, S])((el, acc) => acc.flatMap(f(el, _)))

  /**
   * Determines whether all elements of the `Iterable[A]` satisfy the effectual
   * predicate `f`.
   */
  def forall[R, E, A](as: Iterable[A])(f: A => ZSTM[R, E, Boolean]): ZSTM[R, E, Boolean] =
    succeedNow(as.iterator).flatMap { iterator =>
      def loop: ZSTM[R, E, Boolean] =
        if (iterator.hasNext) f(iterator.next()).flatMap(b => if (b) loop else succeedNow(b))
        else succeedNow(true)
      loop
    }

  /**
   * Applies the function `f` to each element of the `Collection[A]` and
   * returns a transactional effect that produces a new `Collection[B]`.
   */
  def foreach[R, E, A, B, Collection[+Element] <: Iterable[Element]](
    in: Collection[A]
  )(f: A => ZSTM[R, E, B])(implicit bf: BuildFrom[Collection[A], B, Collection[B]]): ZSTM[R, E, Collection[B]] =
    in.foldLeft[ZSTM[R, E, Builder[B, Collection[B]]]](ZSTM.succeed(bf.newBuilder(in))) { (tx, a) =>
      tx.zipWith(f(a))(_ += _)
    }.map(_.result())

  /**
   * Applies the function `f` to each element of the `Set[A]` and returns a
   * transactional effect that produces a new `Set[B]`.
   */
  def foreach[R, E, A, B](in: Set[A])(f: A => ZSTM[R, E, B]): ZSTM[R, E, Set[B]] =
    foreach[R, E, A, B, Iterable](in)(f).map(_.toSet)

  /**
   * Applies the function `f` to each element of the `Iterable[A]` and
   * returns a transactional effect that produces `Unit`.
   *
   * Equivalent to `foreach(as)(f).unit`, but without the cost of building
   * the list of results.
   */
  def foreach_[R, E, A](in: Iterable[A])(f: A => ZSTM[R, E, Any]): ZSTM[R, E, Unit] =
    ZSTM.succeedNow(in.iterator).flatMap[R, E, Unit] { it =>
      def loop: ZSTM[R, E, Unit] =
        if (it.hasNext) f(it.next()) *> loop
        else ZSTM.unit
      loop
    }

  /**
   * Lifts an `Either` into a `STM`.
   */
  def fromEither[E, A](e: => Either[E, A]): STM[E, A] =
    STM.suspend {
      e match {
        case Left(t)  => STM.fail(t)
        case Right(a) => STM.succeedNow(a)
      }
    }

  /**
   * Lifts a function `R => A` into a `URSTM[R, A]`.
   */
  def fromFunction[R, A](f: R => A): URSTM[R, A] =
    access(f)

  /**
   * Lifts an effectful function whose effect requires no environment into
   * an effect that requires the input to the function.
   */
  def fromFunctionM[R, E, A](f: R => STM[E, A]): ZSTM[R, E, A] =
    accessM(f)

  /**
   * Lifts an `Option` into a `STM`.
   */
  def fromOption[A](v: => Option[A]): STM[Option[Nothing], A] =
    STM.suspend(v.fold[STM[Option[Nothing], A]](STM.fail(None))(STM.succeedNow))

  /**
   * Lifts a `Try` into a `STM`.
   */
  def fromTry[A](a: => Try[A]): TaskSTM[A] =
    STM.suspend {
      a match {
        case Failure(t) => STM.fail(t)
        case Success(a) => STM.succeedNow(a)
      }
    }

  /**
   * Returns the identity effectful function, which performs no effects
   */
  def identity[R]: URSTM[R, R] = fromFunction[R, R](ZIO.identityFn)

  /**
   * Runs `onTrue` if the result of `b` is `true` and `onFalse` otherwise.
   */
  def ifM[R, E](b: ZSTM[R, E, Boolean]): ZSTM.IfM[R, E] =
    new ZSTM.IfM(b)

  /**
   * Returns `onTrue` if the result of `b` is `true` and `onFalse` otherwise.
   */
  def ifF[R, E](b: ZSTM[R, E, Boolean]): ZSTM.IfF[R, E] =
    new ZSTM.IfF(b)

  /**
   * Iterates with the specified transactional function. The moral equivalent
   * of:
   *
   * {{{
   * var s = initial
   *
   * while (cont(s)) {
   *   s = body(s)
   * }
   *
   * s
   * }}}
   */
  def iterate[R, E, S](initial: S)(cont: S => Boolean)(body: S => ZSTM[R, E, S]): ZSTM[R, E, S] =
    if (cont(initial)) body(initial).flatMap(iterate(_)(cont)(body))
    else ZSTM.succeedNow(initial)

  /**
   * Returns an effect with the value on the left part.
   */
  def left[A](a: => A): USTM[Either[A, Nothing]] =
    succeed(Left(a))

  /**
   * Loops with the specified transactional function, collecting the results
   * into a list. The moral equivalent of:
   *
   * {{{
   * var s  = initial
   * var as = List.empty[A]
   *
   * while (cont(s)) {
   *   as = body(s) :: as
   *   s  = inc(s)
   * }
   *
   * as.reverse
   * }}}
   */
  def loop[R, E, A, S](initial: S)(cont: S => Boolean, inc: S => S)(body: S => ZSTM[R, E, A]): ZSTM[R, E, List[A]] =
    if (cont(initial))
      body(initial).flatMap(a => loop(inc(initial))(cont, inc)(body).map(as => a :: as))
    else
      ZSTM.succeedNow(List.empty[A])

  /**
   * Loops with the specified transactional function purely for its
   * transactional effects. The moral equivalent of:
   *
   * {{{
   * var s = initial
   *
   * while (cont(s)) {
   *   body(s)
   *   s = inc(s)
   * }
   * }}}
   */
  def loop_[R, E, S](initial: S)(cont: S => Boolean, inc: S => S)(body: S => ZSTM[R, E, Any]): ZSTM[R, E, Unit] =
    if (cont(initial)) body(initial) *> loop_(inc(initial))(cont, inc)(body)
    else ZSTM.unit

  /**
   * Sequentially zips the specified effects using the specified combiner
   * function.
   */
  def mapN[R, E, A, B, C](tx1: ZSTM[R, E, A], tx2: ZSTM[R, E, B])(f: (A, B) => C): ZSTM[R, E, C] =
    tx1.zipWith(tx2)(f)

  /**
   * Sequentially zips the specified effects using the specified combiner
   * function.
   */
  def mapN[R, E, A, B, C, D](tx1: ZSTM[R, E, A], tx2: ZSTM[R, E, B], tx3: ZSTM[R, E, C])(
    f: (A, B, C) => D
  ): ZSTM[R, E, D] =
    for {
      a <- tx1
      b <- tx2
      c <- tx3
    } yield f(a, b, c)

  /**
   * Sequentially zips the specified effects using the specified combiner
   * function.
   */
  def mapN[R, E, A, B, C, D, F](tx1: ZSTM[R, E, A], tx2: ZSTM[R, E, B], tx3: ZSTM[R, E, C], tx4: ZSTM[R, E, D])(
    f: (A, B, C, D) => F
  ): ZSTM[R, E, F] =
    for {
      a <- tx1
      b <- tx2
      c <- tx3
      d <- tx4
    } yield f(a, b, c, d)

  /**
   * Merges an `Iterable[ZSTM]` to a single ZSTM, working sequentially.
   */
  def mergeAll[R, E, A, B](
    in: Iterable[ZSTM[R, E, A]]
  )(zero: B)(f: (B, A) => B): ZSTM[R, E, B] =
    in.foldLeft[ZSTM[R, E, B]](succeedNow(zero))(_.zipWith(_)(f))

  /**
   * Returns an effect with the empty value.
   */
  val none: USTM[Option[Nothing]] = succeedNow(None)

  /**
   * Creates an `STM` value from a partial (but pure) function.
   */
  def partial[A](a: => A): STM[Throwable, A] =
    fromTry(Try(a))

  /**
   * Feeds elements of type `A` to a function `f` that returns an effect.
   * Collects all successes and failures in a tupled fashion.
   */
  def partition[R, E, A, B](
    in: Iterable[A]
  )(f: A => ZSTM[R, E, B])(implicit ev: CanFail[E]): ZSTM[R, Nothing, (Iterable[E], Iterable[B])] =
    ZSTM.foreach(in)(f(_).either).map(ZIO.partitionMap(_)(ZIO.identityFn))

  /**
   * Reduces an `Iterable[ZSTM]` to a single `ZSTM`, working sequentially.
   */
  def reduceAll[R, R1 <: R, E, A](a: ZSTM[R, E, A], as: Iterable[ZSTM[R1, E, A]])(
    f: (A, A) => A
  ): ZSTM[R1, E, A] =
    as.foldLeft[ZSTM[R1, E, A]](a)(_.zipWith(_)(f))

  /**
   * Replicates the given effect n times.
   * If 0 or negative numbers are given, an empty `Iterable` will return.
   */
  def replicate[R, E, A](n: Int)(tx: ZSTM[R, E, A]): Iterable[ZSTM[R, E, A]] =
    new Iterable[ZSTM[R, E, A]] {
      override def iterator: Iterator[ZSTM[R, E, A]] = Iterator.range(0, n).map(_ => tx)
    }

  /**
   * Performs this transaction the specified number of times and collects the
   * results.
   */
  def replicateM[R, E, A](n: Int)(transaction: ZSTM[R, E, A]): ZSTM[R, E, Iterable[A]] =
    ZSTM.collectAll(ZSTM.replicate(n)(transaction))

  /**
   * Performs this transaction the specified number of times, discarding the
   * results.
   */
  def replicateM_[R, E, A](n: Int)(transaction: ZSTM[R, E, A]): ZSTM[R, E, Unit] =
    ZSTM.collectAll_(ZSTM.replicate(n)(transaction))

  /**
   * Requires that the given `ZSTM[R, E, Option[A]]` contain a value. If there is no
   * value, then the specified error will be raised.
   */
  def require[R, E, A](error: => E): ZSTM[R, E, Option[A]] => ZSTM[R, E, A] =
    _.flatMap(_.fold[ZSTM[R, E, A]](fail(error))(succeedNow))

  /**
   * Abort and retry the whole transaction when any of the underlying
   * transactional variables have changed.
   */
  val retry: USTM[Nothing] = new ZSTM((_, _, _, _) => TExit.Retry)

  /**
   * Returns an effect with the value on the right part.
   */
  def right[A](a: => A): USTM[Either[Nothing, A]] =
    succeed(Right(a))

  /**
   * Returns an effectful function that extracts out the second element of a
   * tuple.
   */
  def second[A]: URSTM[(Any, A), A] =
    fromFunction(_._2)

  /**
   * Accesses the specified service in the environment of the effect.
   */
  def service[A: Tag]: ZSTM[Has[A], Nothing, A] =
    ZSTM.access(_.get[A])

  /**
   * Accesses the specified services in the environment of the effect.
   */
  def services[A: Tag, B: Tag]: ZSTM[Has[A] with Has[B], Nothing, (A, B)] =
    ZSTM.access(r => (r.get[A], r.get[B]))

  /**
   * Accesses the specified services in the environment of the effect.
   */
  def services[A: Tag, B: Tag, C: Tag]: ZSTM[Has[A] with Has[B] with Has[C], Nothing, (A, B, C)] =
    ZSTM.access(r => (r.get[A], r.get[B], r.get[C]))

  /**
   * Accesses the specified services in the environment of the effect.
   */
  def services[A: Tag, B: Tag, C: Tag, D: Tag]
    : ZSTM[Has[A] with Has[B] with Has[C] with Has[D], Nothing, (A, B, C, D)] =
    ZSTM.access(r => (r.get[A], r.get[B], r.get[C], r.get[D]))

  /**
   * Effectfully accesses the specified service in the environment of the effect.
   */
  def serviceWith[Service]: ServiceWithPartiallyApplied[Service] =
    new ServiceWithPartiallyApplied[Service]

  /**
   * Returns an effect with the optional value.
   */
  def some[A](a: => A): USTM[Option[A]] =
    succeed(Some(a))

  /**
   * Returns an `STM` effect that succeeds with the specified value.
   */
  def succeed[A](a: => A): USTM[A] =
    new ZSTM((_, _, _, _) => TExit.Succeed(a))

  /**
   * Suspends creation of the specified transaction lazily.
   */
  def suspend[R, E, A](stm: => ZSTM[R, E, A]): ZSTM[R, E, A] =
    STM.succeed(stm).flatten

  /**
   * Returns an effectful function that merely swaps the elements in a `Tuple2`.
   */
  def swap[A, B]: URSTM[(A, B), (B, A)] =
    fromFunction[(A, B), (B, A)](_.swap)

  /**
   * Returns an `STM` effect that succeeds with `Unit`.
   */
  val unit: USTM[Unit] = succeedNow(())

  /**
   * The moral equivalent of `if (!p) exp`
   */
  def unless[R, E](b: => Boolean)(stm: => ZSTM[R, E, Any]): ZSTM[R, E, Unit] =
    suspend(if (b) unit else stm.unit)

  /**
   * The moral equivalent of `if (!p) exp` when `p` has side-effects
   */
  def unlessM[R, E](b: ZSTM[R, E, Boolean]): ZSTM.UnlessM[R, E] =
    new ZSTM.UnlessM(b)

  /**
   * Feeds elements of type `A` to `f` and accumulates all errors in error
   * channel or successes in success channel.
   *
   * This combinator is lossy meaning that if there are errors all successes
   * will be lost. To retain all information please use [[partition]].
   */
  def validate[R, E, A, B, Collection[+Element] <: Iterable[Element]](in: Collection[A])(
    f: A => ZSTM[R, E, B]
  )(implicit bf: BuildFrom[Collection[A], B, Collection[B]], ev: CanFail[E]): ZSTM[R, ::[E], Collection[B]] =
    partition(in)(f).flatMap {
      case (e :: es, _) => fail(::(e, es))
      case (_, bs)      => succeedNow(bf.fromSpecific(in)(bs))
    }

  /**
   * Feeds elements of type `A` to `f` and accumulates all errors in error
   * channel or successes in success channel.
   *
   * This combinator is lossy meaning that if there are errors all successes
   * will be lost. To retain all information please use [[partition]].
   */
  def validate[R, E, A, B](in: NonEmptyChunk[A])(
    f: A => ZSTM[R, E, B]
  )(implicit ev: CanFail[E]): ZSTM[R, ::[E], NonEmptyChunk[B]] =
    partition(in)(f).flatMap {
      case (e :: es, _) => fail(::(e, es))
      case (_, bs)      => succeedNow(NonEmptyChunk.nonEmpty(Chunk.fromIterable(bs)))
    }

  /**
   * Feeds elements of type `A` to `f` until it succeeds. Returns first success
   * or the accumulation of all errors.
   */
  def validateFirst[R, E, A, B, Collection[+Element] <: Iterable[Element]](in: Collection[A])(
    f: A => ZSTM[R, E, B]
  )(implicit bf: BuildFrom[Collection[A], E, Collection[E]], ev: CanFail[E]): ZSTM[R, Collection[E], B] =
    ZSTM.foreach(in)(f(_).flip).flip

  /**
   * The moral equivalent of `if (p) exp`
   */
  def when[R, E](b: => Boolean)(stm: => ZSTM[R, E, Any]): ZSTM[R, E, Unit] =
    suspend(if (b) stm.unit else unit)

  /**
   * Runs an effect when the supplied `PartialFunction` matches for the given value, otherwise does nothing.
   */
  def whenCase[R, E, A](a: => A)(pf: PartialFunction[A, ZSTM[R, E, Any]]): ZSTM[R, E, Unit] =
    suspend(pf.applyOrElse(a, (_: A) => unit).unit)

  /**
   * Runs an effect when the supplied `PartialFunction` matches for the given effectful value, otherwise does nothing.
   */
  def whenCaseM[R, E, A](a: ZSTM[R, E, A])(pf: PartialFunction[A, ZSTM[R, E, Any]]): ZSTM[R, E, Unit] =
    a.flatMap(whenCase(_)(pf))

  /**
   * The moral equivalent of `if (p) exp` when `p` has side-effects
   */
  def whenM[R, E](b: ZSTM[R, E, Boolean]): ZSTM.WhenM[R, E] =
    new ZSTM.WhenM(b)

  private[zio] def succeedNow[A](a: A): USTM[A] =
    succeed(a)

  final class AccessPartiallyApplied[R](private val dummy: Boolean = true) extends AnyVal {
    def apply[A](f: R => A): ZSTM[R, Nothing, A] =
      ZSTM.environment.map(f)
  }

  final class AccessMPartiallyApplied[R](private val dummy: Boolean = true) extends AnyVal {
    def apply[E, A](f: R => ZSTM[R, E, A]): ZSTM[R, E, A] =
      ZSTM.environment.flatMap(f)
  }

  final class ServiceWithPartiallyApplied[Service](private val dummy: Boolean = true) extends AnyVal {
    def apply[E, A](f: Service => ZSTM[Has[Service], E, A])(implicit tag: Tag[Service]): ZSTM[Has[Service], E, A] =
      ZSTM.service[Service].flatMap(f)
  }

  final class IfM[R, E](private val b: ZSTM[R, E, Boolean]) {
    def apply[R1 <: R, E1 >: E, A](onTrue: => ZSTM[R1, E1, A], onFalse: => ZSTM[R1, E1, A]): ZSTM[R1, E1, A] =
      b.flatMap(b => if (b) onTrue else onFalse)
  }

  final class IfF[R, E](private val b: ZSTM[R, E, Boolean]) {
    def apply[A](onTrue: => A, onFalse: => A): ZSTM[R, E, A] =
      b.map(b => if (b) onTrue else onFalse)
  }

  final class UnlessM[R, E](private val b: ZSTM[R, E, Boolean]) {
    def apply[R1 <: R, E1 >: E](stm: => ZSTM[R1, E1, Any]): ZSTM[R1, E1, Unit] =
      b.flatMap(b => if (b) unit else stm.unit)
  }

  final class UpdateService[-R, +E, +A, M](private val self: ZSTM[R, E, A]) {
    def apply[R1 <: R with Has[M]](f: M => M)(implicit ev: Has.IsHas[R1], tag: Tag[M]): ZSTM[R1, E, A] =
      self.provideSome(ev.update(_, f))
  }

  final class WhenM[R, E](private val b: ZSTM[R, E, Boolean]) {
    def apply[R1 <: R, E1 >: E](stm: => ZSTM[R1, E1, Any]): ZSTM[R1, E1, Unit] =
      b.flatMap(b => if (b) stm.unit else unit)
  }

  private final class Resumable[E, E1, A, B](
    val stm: STM[E, A],
    val ks: Stack[internal.TExit[E, A] => STM[E1, B]]
  ) extends Throwable(null, null, false, false)

  private val MaxFrames = 200

  private[stm] object internal {
    val DefaultJournalSize = 4
    val MaxRetries         = 10

    class Versioned[A](val value: A)

    type TxnId = Long

    type Journal = MutableMap[ZTRef.Atomic[_], Entry]

    type Todo = () => Any

    /**
     * Creates a function that can reset the journal.
     */
    def prepareResetJournal(journal: Journal): () => Any = {
      val saved = new MutableMap[ZTRef.Atomic[_], Entry](journal.size)

      val it = journal.entrySet.iterator
      while (it.hasNext) {
        val entry = it.next
        saved.put(entry.getKey, entry.getValue.copy())
      }

      () => { journal.clear(); journal.putAll(saved); () }
    }

    /**
     * Commits the journal.
     */
    def commitJournal(journal: Journal): Unit = {
      val it = journal.entrySet.iterator
      while (it.hasNext) it.next.getValue.commit()
    }

    /**
     * Allocates memory for the journal, if it is null, otherwise just clears it.
     */
    def allocJournal(journal: Journal): Journal =
      if (journal eq null) new MutableMap[ZTRef.Atomic[_], Entry](DefaultJournalSize)
      else {
        journal.clear()
        journal
      }

    /**
     * Determines if the journal is valid.
     */
    def isValid(journal: Journal): Boolean = {
      var valid = true
      val it    = journal.entrySet.iterator
      while (valid && it.hasNext) valid = it.next.getValue.isValid
      valid
    }

    /**
     * Analyzes the journal, determining whether it is valid and whether it is
     * read only in a single pass. Note that information on whether the
     * journal is read only will only be accurate if the journal is valid, due
     * to short-circuiting that occurs on an invalid journal.
     */
    def analyzeJournal(journal: Journal): JournalAnalysis = {
      var result = JournalAnalysis.ReadOnly: JournalAnalysis
      val it     = journal.entrySet.iterator
      while ((result ne JournalAnalysis.Invalid) && it.hasNext) {
        val value = it.next.getValue
        if (value.isInvalid) result = JournalAnalysis.Invalid
        else if (value.isChanged) result = JournalAnalysis.ReadWrite
      }
      result
    }

    sealed abstract class JournalAnalysis extends Serializable with Product
    object JournalAnalysis {
      case object Invalid   extends JournalAnalysis
      case object ReadOnly  extends JournalAnalysis
      case object ReadWrite extends JournalAnalysis
    }

    /**
     * Determines if the journal is invalid.
     */
    def isInvalid(journal: Journal): Boolean = !isValid(journal)

    /**
     * Atomically collects and clears all the todos from any `TRef` that
     * participated in the transaction.
     */
    def collectTodos(journal: Journal): MutableMap[TxnId, Todo] = {
      import collection.JavaConverters._

      val allTodos  = new MutableMap[TxnId, Todo](DefaultJournalSize)
      val emptyTodo = Map.empty[TxnId, Todo]

      val it = journal.entrySet.iterator
      while (it.hasNext) {
        val tref = it.next.getValue.tref
        val todo = tref.todo

        var loop = true
        while (loop) {
          val oldTodo = todo.get

          loop = !todo.compareAndSet(oldTodo, emptyTodo)

          if (!loop) allTodos.putAll(oldTodo.asJava): @silent("JavaConverters")
        }
      }

      allTodos
    }

    /**
     * Executes the todos in the current thread, sequentially.
     */
    def execTodos(todos: MutableMap[TxnId, Todo]): Unit = {
      val it = todos.entrySet.iterator
      while (it.hasNext) it.next.getValue.apply()
    }

    /**
     * For the given transaction id, adds the specified todo effect to all
     * `TRef` values.
     */
    def addTodo(txnId: TxnId, journal: Journal, todoEffect: Todo): Boolean = {
      var added = false

      val it = journal.entrySet.iterator
      while (it.hasNext) {
        val tref = it.next.getValue.tref

        var loop = true
        while (loop) {
          val oldTodo = tref.todo.get

          if (!oldTodo.contains(txnId)) {
            val newTodo = oldTodo.updated(txnId, todoEffect)

            loop = !tref.todo.compareAndSet(oldTodo, newTodo)

            if (!loop) added = true
          } else loop = false
        }
      }

      added
    }

    /**
     * Runs all the todos.
     */
    def completeTodos[E, A](io: IO[E, A], journal: Journal, platform: Platform): TryCommit[E, A] = {
      val todos = collectTodos(journal)

      if (todos.size > 0) platform.executor.submitOrThrow(() => execTodos(todos))

      TryCommit.Done(io)
    }

    /**
     * Finds all the new todo targets that are not already tracked in the `oldJournal`.
     */
    def untrackedTodoTargets(oldJournal: Journal, newJournal: Journal): Journal = {
      val untracked = new MutableMap[ZTRef.Atomic[_], Entry](newJournal.size)

      untracked.putAll(newJournal)

      val it = newJournal.entrySet.iterator
      while (it.hasNext) {
        val entry = it.next
        val key   = entry.getKey
        val value = entry.getValue
        if (oldJournal.containsKey(key)) {
          // We already tracked this one, remove it:
          untracked.remove(key)
        } else if (value.isNew) {
          // This `TRef` was created in the current transaction, so no need to
          // add any todos to it, because it cannot be modified from the outside
          // until the transaction succeeds; so any todo added to it would never
          // succeed.
          untracked.remove(key)
        }
      }

      untracked
    }

    def tryCommitAsync[R, E, A](
      journal: Journal,
      platform: Platform,
      fiberId: Fiber.Id,
      stm: ZSTM[R, E, A],
      txnId: TxnId,
      done: AtomicBoolean,
      r: R
    )(
      k: ZIO[R, E, A] => Any
    ): Unit = {
      def complete(io: IO[E, A]): Unit = { done.set(true); k(io); () }

      @tailrec
      def suspend(accum: Journal, journal: Journal): Unit = {
        addTodo(txnId, journal, () => tryCommitAsync(null, platform, fiberId, stm, txnId, done, r)(k))

        if (isInvalid(journal)) tryCommit(platform, fiberId, stm, r) match {
          case TryCommit.Done(io) => complete(io)
          case TryCommit.Suspend(journal2) =>
            val untracked = untrackedTodoTargets(accum, journal2)

            if (untracked.size > 0) {
              accum.putAll(untracked)

              suspend(accum, untracked)
            }
        }
      }

      Sync(done) {
        if (!done.get) {
          if (journal ne null) suspend(journal, journal)
          else
            tryCommit(platform, fiberId, stm, r) match {
              case TryCommit.Done(io)         => complete(io)
              case TryCommit.Suspend(journal) => suspend(journal, journal)
            }
        }
      }
    }

    def tryCommit[R, E, A](platform: Platform, fiberId: Fiber.Id, stm: ZSTM[R, E, A], r: R): TryCommit[E, A] = {
      var journal = null.asInstanceOf[MutableMap[ZTRef.Atomic[_], Entry]]
      var value   = null.asInstanceOf[TExit[E, A]]

      var loop    = true
      var retries = 0

      while (loop) {
        journal = allocJournal(journal)

        if (retries > MaxRetries) {
          Sync(globalLock) {
            value = stm.run(journal, fiberId, r)
            commitJournal(journal)
            loop = false
          }
        } else {
          value = stm.run(journal, fiberId, r)

          val analysis = analyzeJournal(journal)

          if (analysis ne JournalAnalysis.Invalid) {
            loop = false

            value match {
              case _: TExit.Succeed[_] =>
                if (analysis eq JournalAnalysis.ReadWrite) {
                  Sync(globalLock) {
                    if (isValid(journal)) commitJournal(journal) else loop = true
                  }
                } else {
                  Sync(globalLock) {
                    if (isInvalid(journal)) loop = true
                  }
                }

              case _ =>
            }
          }
        }

        retries += 1
      }

      value match {
        case TExit.Succeed(a) => completeTodos(IO.succeedNow(a), journal, platform)
        case TExit.Fail(e)    => completeTodos(IO.fail(e), journal, platform)
        case TExit.Die(t)     => completeTodos(IO.die(t), journal, platform)
        case TExit.Retry      => TryCommit.Suspend(journal)
      }
    }

    def makeTxnId(): Long = txnCounter.incrementAndGet()

    private[this] val txnCounter: AtomicLong = new AtomicLong()

    val globalLock: AnyRef = new AnyRef {}

    sealed abstract class TExit[+A, +B] extends Serializable with Product
    object TExit {
      val unit: TExit[Nothing, Unit] = Succeed(())

      final case class Fail[+A](value: A)    extends TExit[A, Nothing]
      final case class Die(error: Throwable) extends TExit[Nothing, Nothing]
      final case class Succeed[+B](value: B) extends TExit[Nothing, B]
      case object Retry                      extends TExit[Nothing, Nothing]
    }

    abstract class Entry { self =>
      type S

      val tref: ZTRef.Atomic[S]

      private[stm] val expected: Versioned[S]

      protected[this] var newValue: S

      val isNew: Boolean

      private[this] var _isChanged = false

      def unsafeSet(value: Any): Unit = {
        _isChanged = true
        newValue = value.asInstanceOf[S]
      }

      def unsafeGet[B]: B = newValue.asInstanceOf[B]

      /**
       * Commits the new value to the `TRef`.
       */
      def commit(): Unit = tref.versioned = new Versioned(newValue)

      /**
       * Creates a copy of the Entry.
       */
      def copy(): Entry = new Entry {
        type S = self.S
        val tref     = self.tref
        val expected = self.expected
        val isNew    = self.isNew
        var newValue = self.newValue
        _isChanged = self.isChanged
      }

      /**
       * Determines if the entry is invalid. This is the negated version of
       * `isValid`.
       */
      def isInvalid: Boolean = !isValid

      /**
       * Determines if the entry is valid. That is, if the version of the
       * `TRef` is equal to the expected version.
       */
      def isValid: Boolean = tref.versioned eq expected

      /**
       * Determines if the variable has been set in a transaction.
       */
      def isChanged: Boolean = _isChanged

      override def toString: String =
        s"Entry(expected.value = ${expected.value}, newValue = $newValue, tref = $tref, isChanged = $isChanged)"
    }

    object Entry {

      /**
       * Creates an entry for the journal, given the `TRef` being untracked, the
       * new value of the `TRef`, and the expected version of the `TRef`.
       */
      private[stm] def apply[A0](tref0: ZTRef.Atomic[A0], isNew0: Boolean): Entry = {
        val versioned = tref0.versioned

        new Entry {
          type S = A0
          val tref     = tref0
          val isNew    = isNew0
          val expected = versioned
          var newValue = versioned.value
        }
      }
    }

    sealed abstract class TryCommit[+E, +A]
    object TryCommit {
      final case class Done[+E, +A](io: IO[E, A]) extends TryCommit[E, A]
      final case class Suspend(journal: Journal)  extends TryCommit[Nothing, Nothing]
    }
  }
}

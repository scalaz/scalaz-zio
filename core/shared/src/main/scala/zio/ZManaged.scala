/*
 * Copyright 2018-2021 John A. De Goes and the ZIO Contributors
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

import zio.ZManaged.ReleaseMap
import zio.clock.Clock
import zio.duration.Duration
import zio.internal.Executor

import scala.collection.immutable.LongMap
import scala.concurrent.ExecutionContext
import scala.reflect.ClassTag

/**
 * A `Reservation[-R, +E, +A]` encapsulates resource acquisition and disposal
 * without specifying when or how that resource might be used.
 *
 * See [[ZManaged#reserve]] and [[ZIO#reserve]] for details of usage.
 */
final case class Reservation[-R, +E, +A](acquire: ZIO[R, E, A], release: Exit[Any, Any] => URIO[R, Any])

/**
 * A `ZManaged[R, E, A]` is a managed resource of type `A`, which may be used by
 * invoking the `use` method of the resource. The resource will be automatically
 * acquired before the resource is used, and automatically released after the
 * resource is used.
 *
 * Resources do not survive the scope of `use`, meaning that if you attempt to
 * capture the resource, leak it from `use`, and then use it after the resource
 * has been consumed, the resource will not be valid anymore and may fail with
 * some checked error, as per the type of the functions provided by the resource.
 */
sealed abstract class ZManaged[-R, +E, +A] extends Serializable { self =>

  /**
   * The ZIO value that underlies this ZManaged value. To evaluate it, a ReleaseMap is
   * required. The ZIO value will return a tuple of the resource allocated by this ZManaged
   * and a finalizer that will release the resource.
   *
   * Note that this method is a low-level interface, not intended for regular usage. As such,
   * it offers no guarantees on interruption or resource safety - those are up to the caller
   * to enforce!
   */
  def zio: ZIO[(R, ZManaged.ReleaseMap), E, (ZManaged.Finalizer, A)]

  /**
   * A symbolic alias for `orDie`.
   */
  final def !(implicit ev1: E <:< Throwable, ev2: CanFail[E]): ZManaged[R, Nothing, A] =
    self.orDie

  /**
   * Symbolic alias for zip.
   */
  def &&&[R1 <: R, E1 >: E, B](that: ZManaged[R1, E1, B]): ZManaged[R1, E1, (A, B)] =
    zipWith(that)((a, b) => (a, b))

  /**
   * Symbolic alias for zipParRight
   */
  def &>[R1 <: R, E1 >: E, A1](that: ZManaged[R1, E1, A1]): ZManaged[R1, E1, A1] =
    zipPar(that).map(_._2)

  /**
   * Splits the environment, providing the first part to this effect and the
   * second part to that effect.
   */
  def ***[R1, E1 >: E, B](that: ZManaged[R1, E1, B]): ZManaged[(R, R1), E1, (A, B)] =
    (ZManaged.first >>> self) &&& (ZManaged.second >>> that)

  /**
   * Symbolic alias for zipRight
   */
  def *>[R1 <: R, E1 >: E, A1](that: ZManaged[R1, E1, A1]): ZManaged[R1, E1, A1] =
    flatMap(_ => that)

  def +++[R1, B, E1 >: E](that: ZManaged[R1, E1, B]): ZManaged[Either[R, R1], E1, Either[A, B]] =
    for {
      e <- ZManaged.environment[Either[R, R1]]
      r <- e.fold(map(Left(_)).provide(_), that.map(Right(_)).provide(_))
    } yield r

  /**
   * Symbolic alias for zipParLeft
   */
  def <&[R1 <: R, E1 >: E, A1](that: ZManaged[R1, E1, A1]): ZManaged[R1, E1, A] = zipPar(that).map(_._1)

  /**
   * Symbolic alias for zipPar
   */
  def <&>[R1 <: R, E1 >: E, A1](that: ZManaged[R1, E1, A1]): ZManaged[R1, E1, (A, A1)] =
    zipWithPar(that)((_, _))

  /**
   * Symbolic alias for zipLeft.
   */
  def <*[R1 <: R, E1 >: E, A1](that: ZManaged[R1, E1, A1]): ZManaged[R1, E1, A] =
    flatMap(r => that.map(_ => r))

  /**
   * Symbolic alias for zip.
   */
  def <*>[R1 <: R, E1 >: E, A1](that: ZManaged[R1, E1, A1]): ZManaged[R1, E1, (A, A1)] =
    zipWith(that)((_, _))

  /**
   * Symbolic alias for compose
   */
  def <<<[R1, E1 >: E](that: ZManaged[R1, E1, R]): ZManaged[R1, E1, A] =
    for {
      r1 <- ZManaged.environment[R1]
      r  <- that.provide(r1)
      a  <- provide(r)
    } yield a

  /**
   * Operator alias for `orElse`.
   */
  def <>[R1 <: R, E2, A1 >: A](that: => ZManaged[R1, E2, A1])(implicit ev: CanFail[E]): ZManaged[R1, E2, A1] =
    orElse(that)

  /**
   * Symbolic alias for flatMap
   */
  def >>=[R1 <: R, E1 >: E, B](k: A => ZManaged[R1, E1, B]): ZManaged[R1, E1, B] =
    flatMap(k)

  /**
   * Symbolic alias for andThen
   */
  def >>>[E1 >: E, B](that: ZManaged[A, E1, B]): ZManaged[R, E1, B] =
    self.flatMap(that.provide)

  /**
   * Symbolic alias for join
   */
  def |||[R1, E1 >: E, A1 >: A](that: ZManaged[R1, E1, A1]): ZManaged[Either[R, R1], E1, A1] =
    for {
      either <- ZManaged.environment[Either[R, R1]]
      a1     <- either.fold(provide, that.provide)
    } yield a1

  /**
   * Submerges the error case of an `Either` into the `ZManaged`. The inverse
   * operation of `ZManaged.either`.
   */
  def absolve[E1 >: E, B](implicit ev: A <:< Either[E1, B]): ZManaged[R, E1, B] =
    ZManaged.absolve(self.map(ev))

  /**
   * Attempts to convert defects into a failure, throwing away all information
   * about the cause of the failure.
   */
  def absorb(implicit ev: E <:< Throwable): ZManaged[R, Throwable, A] =
    absorbWith(ev)

  /**
   * Attempts to convert defects into a failure, throwing away all information
   * about the cause of the failure.
   */
  def absorbWith(f: E => Throwable): ZManaged[R, Throwable, A] =
    self.sandbox
      .foldM(
        cause => ZManaged.fail(cause.squashWith(f)),
        ZManaged.succeedNow
      )

  /**
   * Maps this effect to the specified constant while preserving the
   * effects of this effect.
   */
  def as[B](b: => B): ZManaged[R, E, B] =
    map(_ => b)

  /**
   * Maps the success value of this effect to a service.
   */
  def asService[A1 >: A: Tag]: ZManaged[R, E, Has[A1]] =
    map(Has(_))

  /**
   * Maps the success value of this effect to an optional value.
   */
  final def asSome: ZManaged[R, E, Option[A]] =
    map(Some(_))

  /**
   * Maps the error value of this effect to an optional value.
   */
  final def asSomeError: ZManaged[R, Option[E], A] =
    mapError(Some(_))

  /**
   * Executes the this effect and then provides its output as an environment to the second effect
   */
  def andThen[E1 >: E, B](that: ZManaged[A, E1, B]): ZManaged[R, E1, B] = self >>> that

  /**
   * Returns an effect whose failure and success channels have been mapped by
   * the specified pair of functions, `f` and `g`.
   */
  @deprecated("use mapBoth", "2.0.0")
  def bimap[E1, A1](f: E => E1, g: A => A1)(implicit ev: CanFail[E]): ZManaged[R, E1, A1] =
    mapBoth(f, g)

  /**
   * Recovers from all errors.
   */
  def catchAll[R1 <: R, E2, A1 >: A](
    h: E => ZManaged[R1, E2, A1]
  )(implicit ev: CanFail[E]): ZManaged[R1, E2, A1] =
    foldM(h, ZManaged.succeedNow)

  /**
   * Recovers from all errors with provided Cause.
   *
   * {{{
   * managed.catchAllCause(_ => ZManaged.succeed(defaultConfig))
   * }}}
   *
   * @see [[absorb]], [[sandbox]], [[mapErrorCause]] - other functions that can recover from defects
   */
  def catchAllCause[R1 <: R, E2, A1 >: A](h: Cause[E] => ZManaged[R1, E2, A1]): ZManaged[R1, E2, A1] =
    self.foldCauseM[R1, E2, A1](h, ZManaged.succeedNow)

  /**
   * Recovers from some or all of the error cases.
   */
  def catchSome[R1 <: R, E1 >: E, A1 >: A](
    pf: PartialFunction[E, ZManaged[R1, E1, A1]]
  )(implicit ev: CanFail[E]): ZManaged[R1, E1, A1] =
    foldM(pf.applyOrElse[E, ZManaged[R1, E1, A1]](_, ZManaged.fail(_)), ZManaged.succeedNow)

  /**
   * Recovers from some or all of the error Causes.
   */
  def catchSomeCause[R1 <: R, E1 >: E, A1 >: A](
    pf: PartialFunction[Cause[E], ZManaged[R1, E1, A1]]
  ): ZManaged[R1, E1, A1] =
    foldCauseM(pf.applyOrElse[Cause[E], ZManaged[R1, E1, A1]](_, ZManaged.halt(_)), ZManaged.succeedNow)

  /**
   * Fail with `e` if the supplied `PartialFunction` does not match, otherwise
   * succeed with the returned value.
   */
  def collect[E1 >: E, B](e: => E1)(pf: PartialFunction[A, B]): ZManaged[R, E1, B] =
    collectM(e)(pf.andThen(ZManaged.succeedNow(_)))

  /**
   * Fail with `e` if the supplied `PartialFunction` does not match, otherwise
   * continue with the returned value.
   */
  def collectM[R1 <: R, E1 >: E, B](e: => E1)(pf: PartialFunction[A, ZManaged[R1, E1, B]]): ZManaged[R1, E1, B] =
    self.flatMap(v => pf.applyOrElse[A, ZManaged[R1, E1, B]](v, _ => ZManaged.fail(e)))

  /**
   * Executes the second effect and then provides its output as an environment to this effect
   */
  def compose[R1, E1 >: E](that: ZManaged[R1, E1, R]): ZManaged[R1, E1, A] =
    self <<< that

  /**
   * Returns an effect whose failure and success have been lifted into an
   * `Either`.The resulting effect cannot fail
   */
  def either(implicit ev: CanFail[E]): ZManaged[R, Nothing, Either[E, A]] =
    fold(Left[E, A], Right[E, A])

  /**
   * Ensures that `f` is executed when this ZManaged is finalized, after
   * the existing finalizer.
   *
   * For usecases that need access to the ZManaged's result, see [[ZManaged#onExit]].
   */
  def ensuring[R1 <: R](f: ZIO[R1, Nothing, Any]): ZManaged[R1, E, A] =
    onExit(_ => f)

  /**
   * Ensures that `f` is executed when this ZManaged is finalized, before
   * the existing finalizer.
   *
   * For usecases that need access to the ZManaged's result, see [[ZManaged#onExitFirst]].
   */
  def ensuringFirst[R1 <: R](f: ZIO[R1, Nothing, Any]): ZManaged[R1, E, A] =
    onExitFirst(_ => f)

  /**
   * Returns a ZManaged that ignores errors raised by the acquire effect and
   * runs it repeatedly until it eventually succeeds.
   */
  def eventually(implicit ev: CanFail[E]): ZManaged[R, Nothing, A] =
    ZManaged(zio.eventually)

  /**
   * Zips this effect with its environment
   */
  def first[R1 <: R, A1 >: A]: ZManaged[R1, E, (A1, R1)] = self &&& ZManaged.identity

  /**
   * Returns a managed resource that attempts to acquire this managed resource
   * and in case of failure, attempts to acquire each of the specified managed
   * resources in order until one of them is successfully acquired, ensuring
   * that the acquired resource is properly released after being used.
   */
  final def firstSuccessOf[R1 <: R, E1 >: E, A1 >: A](rest: Iterable[ZManaged[R1, E1, A1]]): ZManaged[R1, E1, A1] =
    ZManaged.firstSuccessOf(self, rest)

  /**
   * Returns an effect that models the execution of this effect, followed by
   * the passing of its value to the specified continuation function `k`,
   * followed by the effect that it returns.
   */
  def flatMap[R1 <: R, E1 >: E, B](f: A => ZManaged[R1, E1, B]): ZManaged[R1, E1, B] =
    ZManaged {
      self.zio.flatMap { case (releaseSelf, a) =>
        f(a).zio.map { case (releaseThat, b) =>
          (
            e =>
              releaseThat(e).run
                .flatMap(e1 =>
                  releaseSelf(e).run
                    .flatMap(e2 => ZIO.done(e1 *> e2))
                ),
            b
          )
        }
      }
    }

  /**
   * Effectfully map the error channel
   */
  def flatMapError[R1 <: R, E2](f: E => ZManaged[R1, Nothing, E2])(implicit ev: CanFail[E]): ZManaged[R1, E2, A] =
    flipWith(_.flatMap(f))

  /**
   * Returns an effect that performs the outer effect first, followed by the
   * inner effect, yielding the value of the inner effect.
   *
   * This method can be used to "flatten" nested effects.
   */
  def flatten[R1 <: R, E1 >: E, B](implicit ev: A <:< ZManaged[R1, E1, B]): ZManaged[R1, E1, B] =
    flatMap(ev)

  /**
   * Returns an effect that performs the outer effect first, followed by the
   * inner effect, yielding the value of the inner effect.
   *
   * This method can be used to "flatten" nested effects.
   */
  def flattenM[R1 <: R, E1 >: E, B](implicit ev: A <:< ZIO[R1, E1, B]): ZManaged[R1, E1, B] =
    mapM(ev)

  /**
   * Flip the error and result
   */
  def flip: ZManaged[R, A, E] =
    foldM(ZManaged.succeedNow, ZManaged.fail(_))

  /**
   * Flip the error and result, then apply an effectful function to the effect
   */
  def flipWith[R1, A1, E1](f: ZManaged[R, A, E] => ZManaged[R1, A1, E1]): ZManaged[R1, E1, A1] =
    f(flip).flip

  /**
   * Folds over the failure value or the success value to yield an effect that
   * does not fail, but succeeds with the value returned by the left or right
   * function passed to `fold`.
   */
  def fold[B](failure: E => B, success: A => B)(implicit ev: CanFail[E]): ZManaged[R, Nothing, B] =
    foldM(failure.andThen(ZManaged.succeedNow), success.andThen(ZManaged.succeedNow))

  /**
   * A more powerful version of `fold` that allows recovering from any kind of failure except interruptions.
   */
  def foldCause[B](failure: Cause[E] => B, success: A => B): ZManaged[R, Nothing, B] =
    sandbox.fold(failure, success)

  /**
   * A more powerful version of `foldM` that allows recovering from any kind of failure except interruptions.
   */
  def foldCauseM[R1 <: R, E1, A1](
    failure: Cause[E] => ZManaged[R1, E1, A1],
    success: A => ZManaged[R1, E1, A1]
  ): ZManaged[R1, E1, A1] =
    ZManaged(self.zio.foldCauseM(failure(_).zio, { case (_, a) => success(a).zio }))

  /**
   * Recovers from errors by accepting one effect to execute for the case of an
   * error, and one effect to execute for the case of success.
   */
  def foldM[R1 <: R, E2, B](
    failure: E => ZManaged[R1, E2, B],
    success: A => ZManaged[R1, E2, B]
  )(implicit ev: CanFail[E]): ZManaged[R1, E2, B] =
    foldCauseM(_.failureOrCause.fold(failure, ZManaged.halt(_)), success)

  /**
   * Creates a `ZManaged` value that acquires the original resource in a fiber,
   * and provides that fiber. The finalizer for this value will interrupt the fiber
   * and run the original finalizer.
   */
  def fork: ZManaged[R, Nothing, Fiber.Runtime[E, A]] =
    ZManaged {
      ZIO.uninterruptibleMask { restore =>
        for {
          tp                  <- ZIO.environment[(R, ReleaseMap)]
          (r, outerReleaseMap) = tp
          innerReleaseMap     <- ReleaseMap.make
          fiber               <- restore(zio.map(_._2).forkDaemon.provide(r -> innerReleaseMap))
          releaseMapEntry <-
            outerReleaseMap.add(e => fiber.interrupt *> innerReleaseMap.releaseAll(e, ExecutionStrategy.Sequential))
        } yield (releaseMapEntry, fiber)
      }
    }

  /**
   * Unwraps the optional success of this effect, but can fail with None value.
   */
  def get[B](implicit ev1: E <:< Nothing, ev2: A <:< Option[B]): ZManaged[R, Option[Nothing], B] =
    ZManaged.absolve(mapError(ev1)(CanFail).map(ev2(_).toRight(None)))

  /**
   * Returns a new effect that ignores the success or failure of this effect.
   */
  def ignore: ZManaged[R, Nothing, Unit] =
    fold(_ => (), _ => ())

  /**
   * Returns whether this managed effect is a failure.
   */
  def isFailure: ZManaged[R, Nothing, Boolean] =
    fold(_ => true, _ => false)

  /**
   * Returns whether this managed effect is a success.
   */
  def isSuccess: ZManaged[R, Nothing, Boolean] =
    fold(_ => false, _ => true)

  /**
   * Depending on the environment execute this or the other effect
   */
  def join[R1, E1 >: E, A1 >: A](that: ZManaged[R1, E1, A1]): ZManaged[Either[R, R1], E1, A1] = self ||| that

  def left[R1 <: R, C]: ZManaged[Either[R1, C], E, Either[A, C]] = self +++ ZManaged.identity

  /**
   * Locks this managed effect to the specified executor, guaranteeing that
   * this managed effect as well as managed effects that are composed
   * sequentially after it will be run on the specified executor.
   */
  final def lock(executor: Executor): ZManaged[R, E, A] =
    ZManaged.lock(executor) *> self

  /**
   * Returns an effect whose success is mapped by the specified `f` function.
   */
  def map[B](f: A => B): ZManaged[R, E, B] =
    ZManaged(zio.map { case (fin, a) => (fin, f(a)) })

  /**
   * Returns an effect whose failure and success channels have been mapped by
   * the specified pair of functions, `f` and `g`.
   */
  def mapBoth[E1, A1](f: E => E1, g: A => A1)(implicit ev: CanFail[E]): ZManaged[R, E1, A1] =
    mapError(f).map(g)

  /**
   * Returns an effect whose success is mapped by the specified side effecting
   * `f` function, translating any thrown exceptions into typed failed effects.
   */
  final def mapEffect[B](f: A => B)(implicit ev: E <:< Throwable): ZManaged[R, Throwable, B] =
    foldM(e => ZManaged.fail(ev(e)), a => ZManaged.effect(f(a)))

  /**
   * Returns an effect whose failure is mapped by the specified `f` function.
   */
  def mapError[E1](f: E => E1)(implicit ev: CanFail[E]): ZManaged[R, E1, A] =
    ZManaged(zio.mapError(f))

  /**
   * Returns an effect whose full failure is mapped by the specified `f` function.
   */
  def mapErrorCause[E1](f: Cause[E] => Cause[E1]): ZManaged[R, E1, A] =
    ZManaged(zio.mapErrorCause(f))

  /**
   * Effectfully maps the resource acquired by this value.
   */
  def mapM[R1 <: R, E1 >: E, B](f: A => ZIO[R1, E1, B]): ZManaged[R1, E1, B] =
    ZManaged(zio.flatMap { case (fin, a) => f(a).map((fin, _)).provideSome[(R1, ZManaged.ReleaseMap)](_._1) })

  def memoize: ZManaged[Any, Nothing, ZManaged[R, E, A]] =
    ZManaged.releaseMap.mapM { finalizers =>
      for {
        promise <- Promise.make[E, A]
        complete <- ZIO
                      .accessM[R] { r =>
                        self.zio
                          .provide((r, finalizers))
                          .map(_._2)
                          .to(promise)
                      }
                      .once
      } yield (complete *> promise.await).toManaged_
    }

  /**
   * Returns a new effect where the error channel has been merged into the
   * success channel to their common combined type.
   */
  def merge[A1 >: A](implicit ev1: E <:< A1, ev2: CanFail[E]): ZManaged[R, Nothing, A1] =
    self.foldM(e => ZManaged.succeedNow(ev1(e)), ZManaged.succeedNow)

  /**
   * Requires the option produced by this value to be `None`.
   */
  final def none[B](implicit ev: A <:< Option[B]): ZManaged[R, Option[E], Unit] =
    self.foldM(
      e => ZManaged.fail(Some(e)),
      a => a.fold[ZManaged[R, Option[E], Unit]](ZManaged.succeedNow(()))(_ => ZManaged.fail(None))
    )

  /**
   * Runs this managed effect, as well as any managed effects that are composed
   * sequentially after it, using the specified `ExecutionContext`.
   */
  final def on(ec: ExecutionContext): ZManaged[R, E, A] =
    self.lock(Executor.fromExecutionContext(Int.MaxValue)(ec))

  /**
   * Ensures that a cleanup function runs when this ZManaged is finalized, after
   * the existing finalizers.
   */
  def onExit[R1 <: R](cleanup: Exit[E, A] => ZIO[R1, Nothing, Any]): ZManaged[R1, E, A] =
    ZManaged {
      ZIO.uninterruptibleMask { restore =>
        for {
          tp                   <- ZIO.environment[(R1, ReleaseMap)]
          (r1, outerReleaseMap) = tp
          innerReleaseMap      <- ReleaseMap.make
          exitEA               <- restore(zio.map(_._2)).run.provide(r1 -> innerReleaseMap)
          releaseMapEntry <- outerReleaseMap.add { e =>
                               innerReleaseMap
                                 .releaseAll(e, ExecutionStrategy.Sequential)
                                 .run
                                 .zipWith(cleanup(exitEA).provide(r1).run)((l, r) => ZIO.done(l *> r))
                                 .flatten
                             }
          a <- ZIO.done(exitEA)
        } yield (releaseMapEntry, a)
      }
    }

  /**
   * Ensures that a cleanup function runs when this ZManaged is finalized, before
   * the existing finalizers.
   */
  def onExitFirst[R1 <: R](cleanup: Exit[E, A] => ZIO[R1, Nothing, Any]): ZManaged[R1, E, A] =
    ZManaged {
      ZIO.uninterruptibleMask { restore =>
        for {
          tp                   <- ZIO.environment[(R1, ReleaseMap)]
          (r1, outerReleaseMap) = tp
          innerReleaseMap      <- ReleaseMap.make
          exitEA               <- restore(zio.map(_._2)).run.provide(r1 -> innerReleaseMap)
          releaseMapEntry <- outerReleaseMap.add { e =>
                               cleanup(exitEA)
                                 .provide(r1)
                                 .run
                                 .zipWith(innerReleaseMap.releaseAll(e, ExecutionStrategy.Sequential).run)((l, r) =>
                                   ZIO.done(l *> r)
                                 )
                                 .flatten
                             }
          a <- ZIO.done(exitEA)
        } yield (releaseMapEntry, a)
      }
    }

  /**
   * Executes this effect, skipping the error but returning optionally the success.
   */
  def option(implicit ev: CanFail[E]): ZManaged[R, Nothing, Option[A]] =
    fold(_ => None, Some(_))

  /**
   * Converts an option on errors into an option on values.
   */
  final def optional[E1](implicit ev: E <:< Option[E1]): ZManaged[R, E1, Option[A]] =
    self.foldM(
      e => e.fold[ZManaged[R, E1, Option[A]]](ZManaged.succeedNow(Option.empty[A]))(ZManaged.fail(_)),
      a => ZManaged.succeedNow(Some(a))
    )

  /**
   * Translates effect failure into death of the fiber, making all failures unchecked and
   * not a part of the type of the effect.
   */
  def orDie(implicit ev1: E <:< Throwable, ev2: CanFail[E]): ZManaged[R, Nothing, A] =
    orDieWith(ev1)

  /**
   * Keeps none of the errors, and terminates the fiber with them, using
   * the specified function to convert the `E` into a `Throwable`.
   */
  def orDieWith(f: E => Throwable)(implicit ev: CanFail[E]): ZManaged[R, Nothing, A] =
    mapError(f).catchAll(ZManaged.die(_))

  /**
   * Executes this effect and returns its value, if it succeeds, but
   * otherwise executes the specified effect.
   */
  def orElse[R1 <: R, E2, A1 >: A](that: => ZManaged[R1, E2, A1])(implicit ev: CanFail[E]): ZManaged[R1, E2, A1] =
    foldM(_ => that, ZManaged.succeedNow)

  /**
   * Returns an effect that will produce the value of this effect, unless it
   * fails, in which case, it will produce the value of the specified effect.
   */
  def orElseEither[R1 <: R, E2, B](
    that: => ZManaged[R1, E2, B]
  )(implicit ev: CanFail[E]): ZManaged[R1, E2, Either[A, B]] =
    foldM(_ => that.map(Right[A, B]), a => ZManaged.succeedNow(Left[A, B](a)))

  /**
   * Executes this effect and returns its value, if it succeeds, but
   * otherwise fails with the specified error.
   */
  final def orElseFail[E1](e1: => E1)(implicit ev: CanFail[E]): ZManaged[R, E1, A] =
    orElse(ZManaged.fail(e1))

  /**
   * Returns an effect that will produce the value of this effect, unless it
   * fails with the `None` value, in which case it will produce the value of
   * the specified effect.
   */
  final def orElseOptional[R1 <: R, E1, A1 >: A](
    that: => ZManaged[R1, Option[E1], A1]
  )(implicit ev: E <:< Option[E1]): ZManaged[R1, Option[E1], A1] =
    catchAll(ev(_).fold(that)(e => ZManaged.fail(Some(e))))

  /**
   * Executes this effect and returns its value, if it succeeds, but
   * otherwise succeeds with the specified value.
   */
  final def orElseSucceed[A1 >: A](a1: => A1)(implicit ev: CanFail[E]): ZManaged[R, Nothing, A1] =
    orElse(ZManaged.succeedNow(a1))

  /**
   * Preallocates the managed resource, resulting in a ZManaged that reserves
   * and acquires immediately and cannot fail. You should take care that you
   * are not interrupted between running preallocate and actually acquiring
   * the resource as you might leak otherwise.
   */
  def preallocate: ZIO[R, E, Managed[Nothing, A]] =
    ZIO.uninterruptibleMask { restore =>
      for {
        releaseMap <- ReleaseMap.make
        tp         <- restore(self.zio.provideSome[R]((_, releaseMap))).run
        preallocated <- tp.foldM(
                          c =>
                            releaseMap
                              .releaseAll(Exit.fail(c), ExecutionStrategy.Sequential) *>
                              ZIO.halt(c),
                          { case (release, a) =>
                            UIO.succeed(
                              ZManaged {
                                ZIO.accessM[(Any, ReleaseMap)] { case (_, releaseMap) =>
                                  releaseMap.add(release).map((_, a))
                                }
                              }
                            )
                          }
                        )
      } yield preallocated
    }

  /**
   * Preallocates the managed resource inside an outer managed, resulting in a
   * ZManaged that reserves and acquires immediately and cannot fail.
   */
  def preallocateManaged: ZManaged[R, E, Managed[Nothing, A]] =
    ZManaged {
      self.zio.map { case (release, a) =>
        (
          release,
          ZManaged {
            ZIO.accessM[(Any, ReleaseMap)] { case (_, releaseMap) =>
              releaseMap.add(release).map((_, a))
            }
          }
        )
      }
    }

  /**
   * Provides the `ZManaged` effect with its required environment, which eliminates
   * its dependency on `R`.
   */
  def provide(r: R)(implicit ev: NeedsEnv[R]): Managed[E, A] =
    provideSome(_ => r)

  /**
   * Provides the part of the environment that is not part of the `ZEnv`,
   * leaving a managed effect that only depends on the `ZEnv`.
   *
   * {{{
   * val loggingLayer: ZLayer[Any, Nothing, Logging] = ???
   *
   * val managed: ZManaged[ZEnv with Logging, Nothing, Unit] = ???
   *
   * val managed2 = managed.provideCustomLayer(loggingLayer)
   * }}}
   */
  def provideCustomLayer[E1 >: E, R1 <: Has[_]](
    layer: ZLayer[ZEnv, E1, R1]
  )(implicit ev: ZEnv with R1 <:< R, tagged: Tag[R1]): ZManaged[ZEnv, E1, A] =
    provideSomeLayer[ZEnv](layer)

  /**
   * Provides a layer to the `ZManaged`, which translates it to another level.
   */
  def provideLayer[E1 >: E, R0, R1](
    layer: ZLayer[R0, E1, R1]
  )(implicit ev1: R1 <:< R, ev2: NeedsEnv[R]): ZManaged[R0, E1, A] =
    layer.build.map(ev1).flatMap(self.provide)

  /**
   * Provides some of the environment required to run this effect,
   * leaving the remainder `R0`.
   *
   * {{{
   * val managed: ZManaged[Console with Logging, Nothing, Unit] = ???
   *
   * managed.provideSome[Console](env =>
   *   new Console with Logging {
   *     val console = env.console
   *     val logging = new Logging.Service[Any] {
   *       def log(line: String) = console.putStrLn(line)
   *     }
   *   }
   * )
   * }}}
   */
  def provideSome[R0](f: R0 => R)(implicit ev: NeedsEnv[R]): ZManaged[R0, E, A] =
    ZManaged(zio.provideSome[(R0, ZManaged.ReleaseMap)](tp => (f(tp._1), tp._2)))

  /**
   * Splits the environment into two parts, providing one part using the
   * specified layer and leaving the remainder `R0`.
   *
   * {{{
   * val clockLayer: ZLayer[Any, Nothing, Clock] = ???
   *
   * val managed: ZManaged[Clock with Random, Nothing, Unit] = ???
   *
   * val managed2 = managed.provideSomeLayer[Random](clockLayer)
   * }}}
   */
  final def provideSomeLayer[R0 <: Has[_]]: ZManaged.ProvideSomeLayer[R0, R, E, A] =
    new ZManaged.ProvideSomeLayer[R0, R, E, A](self)

  /**
   * Keeps some of the errors, and terminates the fiber with the rest.
   */
  def refineOrDie[E1](
    pf: PartialFunction[E, E1]
  )(implicit ev1: E <:< Throwable, ev2: CanFail[E]): ZManaged[R, E1, A] =
    refineOrDieWith(pf)(ev1)

  /**
   * Keeps some of the errors, and terminates the fiber with the rest, using
   * the specified function to convert the `E` into a `Throwable`.
   */
  def refineOrDieWith[E1](
    pf: PartialFunction[E, E1]
  )(f: E => Throwable)(implicit ev: CanFail[E]): ZManaged[R, E1, A] =
    catchAll(e => pf.lift(e).fold[ZManaged[R, E1, A]](ZManaged.die(f(e)))(ZManaged.fail(_)))

  /**
   * Fail with the returned value if the `PartialFunction` matches, otherwise
   * continue with our held value.
   */
  def reject[E1 >: E](pf: PartialFunction[A, E1]): ZManaged[R, E1, A] =
    rejectM(pf.andThen(ZManaged.fail(_)))

  /**
   * Continue with the returned computation if the `PartialFunction` matches,
   * translating the successful match into a failure, otherwise continue with
   * our held value.
   */
  def rejectM[R1 <: R, E1 >: E](pf: PartialFunction[A, ZManaged[R1, E1, E1]]): ZManaged[R1, E1, A] =
    self.flatMap { v =>
      pf.andThen[ZManaged[R1, E1, A]](_.flatMap(ZManaged.fail(_)))
        .applyOrElse[A, ZManaged[R1, E1, A]](v, ZManaged.succeedNow)
    }

  /**
   * Runs all the finalizers associated with this scope. This is useful to
   * conceptually "close" a scope when composing multiple managed effects.
   * Note that this is only safe if the result of this managed effect is valid
   * outside its scope.
   */
  def release: ZManaged[R, E, A] =
    ZManaged.fromEffect(useNow)

  /**
   * Returns a `Reservation` that allows separately accessing effects
   * describing resource acquisition and release.
   */
  def reserve: UIO[Reservation[R, E, A]] =
    ReleaseMap.make.map { releaseMap =>
      Reservation(
        zio.provideSome[R]((_, releaseMap)).map(_._2),
        releaseMap.releaseAll(_, ExecutionStrategy.Sequential)
      )
    }

  /**
   * Retries with the specified retry policy.
   * Retries are done following the failure of the original `io` (up to a fixed maximum with
   * `once` or `recurs` for example), so that that `io.retry(Schedule.once)` means
   * "execute `io` and in case of failure, try again once".
   */
  def retry[R1 <: R, S](policy: Schedule[R1, E, S])(implicit ev: CanFail[E]): ZManaged[R1 with Clock, E, A] =
    ZManaged(ZIO.accessM[(R1 with Clock, ZManaged.ReleaseMap)] { case (env, releaseMap) =>
      zio.provideSome[R1 with Clock](env => (env, releaseMap)).retry(policy).provide(env)
    })

  def right[R1 <: R, C]: ZManaged[Either[C, R1], E, Either[C, A]] = ZManaged.identity +++ self

  /**
   * Returns an effect that semantically runs the effect on a fiber,
   * producing an [[zio.Exit]] for the completion value of the fiber.
   */
  def run: ZManaged[R, Nothing, Exit[E, A]] =
    foldCauseM(
      cause => ZManaged.succeedNow(Exit.halt(cause)),
      succ => ZManaged.succeedNow(Exit.succeed(succ))
    )

  /**
   * Exposes the full cause of failure of this effect.
   */
  def sandbox: ZManaged[R, Cause[E], A] =
    ZManaged(zio.sandbox)

  /**
   * Companion helper to `sandbox`. Allows recovery, and partial recovery, from
   * errors and defects alike.
   */
  def sandboxWith[R1 <: R, E2, B](
    f: ZManaged[R1, Cause[E], A] => ZManaged[R1, Cause[E2], B]
  ): ZManaged[R1, E2, B] =
    ZManaged.unsandbox(f(self.sandbox))

  /**
   * Zips this effect with its environment
   */
  def second[R1 <: R, A1 >: A]: ZManaged[R1, E, (R1, A1)] = ZManaged.identity[R1] &&& self

  /**
   * Converts an option on values into an option on errors.
   */
  final def some[B](implicit ev: A <:< Option[B]): ZManaged[R, Option[E], B] =
    self.foldM(
      e => ZManaged.fail(Some(e)),
      a => a.fold[ZManaged[R, Option[E], B]](ZManaged.fail(Option.empty[E]))(ZManaged.succeedNow)
    )

  /**
   * Extracts the optional value, or returns the given 'default'.
   */
  final def someOrElse[B](default: => B)(implicit ev: A <:< Option[B]): ZManaged[R, E, B] =
    map(_.getOrElse(default))

  /**
   * Extracts the optional value, or executes the effect 'default'.
   */
  final def someOrElseM[B, R1 <: R, E1 >: E](
    default: ZManaged[R1, E1, B]
  )(implicit ev: A <:< Option[B]): ZManaged[R1, E1, B] =
    self.flatMap(ev(_) match {
      case Some(value) => ZManaged.succeedNow(value)
      case None        => default
    })

  /**
   * Extracts the optional value, or fails with the given error 'e'.
   */
  final def someOrFail[B, E1 >: E](e: => E1)(implicit ev: A <:< Option[B]): ZManaged[R, E1, B] =
    self.flatMap(ev(_) match {
      case Some(value) => ZManaged.succeedNow(value)
      case None        => ZManaged.fail(e)
    })

  /**
   * Extracts the optional value, or fails with a [[java.util.NoSuchElementException]]
   */
  final def someOrFailException[B, E1 >: E](implicit
    ev: A <:< Option[B],
    ev2: NoSuchElementException <:< E1
  ): ZManaged[R, E1, B] =
    self.foldM(
      e => ZManaged.fail(e),
      ev(_) match {
        case Some(value) => ZManaged.succeedNow(value)
        case None        => ZManaged.fail(ev2(new NoSuchElementException("None.get")))
      }
    )

  /**
   * Returns an effect that effectfully peeks at the acquired resource.
   */
  def tap[R1 <: R, E1 >: E](f: A => ZManaged[R1, E1, Any]): ZManaged[R1, E1, A] =
    flatMap(a => f(a).as(a))

  /**
   * Returns an effect that effectfully peeks at the failure or success of the acquired resource.
   */
  def tapBoth[R1 <: R, E1 >: E](f: E => ZManaged[R1, E1, Any], g: A => ZManaged[R1, E1, Any])(implicit
    ev: CanFail[E]
  ): ZManaged[R1, E1, A] =
    foldM(
      e => f(e) *> ZManaged.fail(e),
      a => g(a).as(a)
    )

  /**
   * Returns an effect that effectually peeks at the cause of the failure of
   * the acquired resource.
   */
  final def tapCause[R1 <: R, E1 >: E](f: Cause[E] => ZManaged[R1, E1, Any]): ZManaged[R1, E1, A] =
    catchAllCause(c => f(c) *> ZManaged.halt(c))

  /**
   * Returns an effect that effectually "peeks" at the defect of the acquired
   * resource.
   */
  final def tapDefect[R1 <: R, E1 >: E](f: Cause[Nothing] => ZManaged[R1, E1, Any]): ZManaged[R1, E1, A] =
    catchAllCause(c => f(c.stripFailures) *> ZManaged.halt(c))

  /**
   * Returns an effect that effectfully peeks at the failure of the acquired resource.
   */
  def tapError[R1 <: R, E1 >: E](f: E => ZManaged[R1, E1, Any])(implicit ev: CanFail[E]): ZManaged[R1, E1, A] =
    tapBoth(f, ZManaged.succeedNow)

  /**
   * Like [[ZManaged#tap]], but uses a function that returns a ZIO value rather than a
   * ZManaged value.
   */
  def tapM[R1 <: R, E1 >: E](f: A => ZIO[R1, E1, Any]): ZManaged[R1, E1, A] =
    mapM(a => f(a).as(a))

  /**
   * Returns a new effect that executes this one and times the acquisition of the resource.
   */
  def timed: ZManaged[R with Clock, E, (Duration, A)] =
    ZManaged {
      ZIO.environment[(R, ReleaseMap)].flatMap { case (r, releaseMap) =>
        self.zio
          .provide((r, releaseMap))
          .timed
          .map { case (duration, (fin, a)) =>
            (fin, (duration, a))
          }
          .provideSome[(R with Clock, ReleaseMap)](_._1)
      }
    }

  /**
   * Returns an effect that will timeout this resource, returning `None` if the
   * timeout elapses before the resource was reserved and acquired.
   * If the reservation completes successfully (even after the timeout) the release action will be run on a new fiber.
   * `Some` will be returned if acquisition and reservation complete in time
   */
  def timeout(d: Duration): ZManaged[R with Clock, E, Option[A]] =
    ZManaged {
      ZIO.uninterruptibleMask { restore =>
        for {
          env                 <- ZIO.environment[(R with Clock, ReleaseMap)]
          (r, outerReleaseMap) = env
          innerReleaseMap     <- ZManaged.ReleaseMap.make
          earlyRelease        <- outerReleaseMap.add(innerReleaseMap.releaseAll(_, ExecutionStrategy.Sequential))
          raceResult <- restore {
                          zio
                            .provide((r, innerReleaseMap))
                            .raceWith(ZIO.sleep(d).as(None))(
                              (result, sleeper) => sleeper.interrupt *> ZIO.done(result.map(tp => Right(tp._2))),
                              (_, resultFiber) => UIO.succeed(Left(resultFiber))
                            )
                            .provide(r)
                        }
          a <- raceResult match {
                 case Right(value) => UIO.succeed(Some(value))
                 case Left(fiber) =>
                   ZIO.fiberId.flatMap { id =>
                     fiber.interrupt
                       .ensuring(innerReleaseMap.releaseAll(Exit.interrupt(id), ExecutionStrategy.Sequential))
                       .forkDaemon
                   }.as(None)
               }
        } yield (earlyRelease, a)
      }
    }

  /**
   * Constructs a layer from this managed resource.
   */
  def toLayer[A1 >: A: Tag]: ZLayer[R, E, Has[A1]] =
    ZLayer.fromManaged(self)

  /**
   * Constructs a layer from this managed resource, which must return one or
   * more services.
   */
  def toLayerMany[A1 <: Has[_]](implicit ev: A <:< A1): ZLayer[R, E, A1] =
    ZLayer(self.map(ev))

  /**
   * Return unit while running the effect
   */
  lazy val unit: ZManaged[R, E, Unit] = as(())

  /**
   * The moral equivalent of `if (!p) exp`
   */
  final def unless(b: => Boolean): ZManaged[R, E, Unit] =
    ZManaged.unless(b)(self)

  /**
   * The moral equivalent of `if (!p) exp` when `p` has side-effects
   */
  final def unlessM[R1 <: R, E1 >: E](b: ZManaged[R1, E1, Boolean]): ZManaged[R1, E1, Unit] =
    ZManaged.unlessM(b)(self)

  /**
   * The inverse operation `ZManaged.sandboxed`
   */
  def unsandbox[E1](implicit ev: E <:< Cause[E1]): ZManaged[R, E1, A] =
    ZManaged.unsandbox(mapError(ev))

  /**
   * Updates a service in the environment of this effect.
   */
  final def updateService[M] =
    new ZManaged.UpdateService[R, E, A, M](self)

  /**
   * Run an effect while acquiring the resource before and releasing it after
   */
  def use[R1 <: R, E1 >: E, B](f: A => ZIO[R1, E1, B]): ZIO[R1, E1, B] =
    ZManaged.ReleaseMap.make.bracketExit(
      (relMap, exit: Exit[E1, B]) => relMap.releaseAll(exit, ExecutionStrategy.Sequential),
      relMap => zio.provideSome[R]((_, relMap)).flatMap { case (_, a) => f(a) }
    )

  /**
   *  Run an effect while acquiring the resource before and releasing it after.
   *  This does not provide the resource to the function
   */
  def use_[R1 <: R, E1 >: E, B](f: ZIO[R1, E1, B]): ZIO[R1, E1, B] =
    use(_ => f)

  /**
   * Use the resource until interruption.
   * Useful for resources that you want to acquire and use as long as the application is running, like a HTTP server.
   */
  val useForever: ZIO[R, E, Nothing] = use(_ => ZIO.never)

  /**
   * Runs the acquire and release actions and returns the result of this
   * managed effect. Note that this is only safe if the result of this managed
   * effect is valid outside its scope.
   */
  def useNow: ZIO[R, E, A] =
    use(ZIO.succeedNow)

  /**
   * The moral equivalent of `if (p) exp`
   */
  def when(b: => Boolean): ZManaged[R, E, Unit] =
    ZManaged.when(b)(self)

  /**
   * The moral equivalent of `if (p) exp` when `p` has side-effects
   */
  def whenM[R1 <: R, E1 >: E](b: ZManaged[R1, E1, Boolean]): ZManaged[R1, E1, Unit] =
    ZManaged.whenM(b)(self)

  /**
   * Modifies this `ZManaged` to provide a canceler that can be used to eagerly
   * execute the finalizer of this `ZManaged`. The canceler will run
   * uninterruptibly with an exit value indicating that the effect was
   * interrupted, and if completed will cause the regular finalizer to not run.
   */
  def withEarlyRelease: ZManaged[R, E, (UIO[Any], A)] =
    ZManaged.fiberId.flatMap(fiberId => withEarlyReleaseExit(Exit.interrupt(fiberId)))

  /**
   * A more powerful version of [[withEarlyRelease]] that allows specifying an
   * exit value in the event of early release.
   */
  def withEarlyReleaseExit(e: Exit[Any, Any]): ZManaged[R, E, (UIO[Any], A)] =
    ZManaged(zio.map(tp => (tp._1, (tp._1(e).uninterruptible, tp._2))))

  /**
   * Named alias for `<*>`.
   */
  def zip[R1 <: R, E1 >: E, A1](that: ZManaged[R1, E1, A1]): ZManaged[R1, E1, (A, A1)] =
    self <*> that

  /**
   * Named alias for `<*`.
   */
  def zipLeft[R1 <: R, E1 >: E, A1](that: ZManaged[R1, E1, A1]): ZManaged[R1, E1, A] =
    self <* that

  /**
   * Named alias for `<&>`.
   */
  def zipPar[R1 <: R, E1 >: E, A1](that: ZManaged[R1, E1, A1]): ZManaged[R1, E1, (A, A1)] =
    self <&> that

  /**
   * Named alias for `<&`.
   */
  def zipParLeft[R1 <: R, E1 >: E, A1](that: ZManaged[R1, E1, A1]): ZManaged[R1, E1, A] =
    self <& that

  /**
   * Named alias for `&>`.
   */
  def zipParRight[R1 <: R, E1 >: E, A1](that: ZManaged[R1, E1, A1]): ZManaged[R1, E1, A1] =
    self &> that

  /**
   * Named alias for `*>`.
   */
  def zipRight[R1 <: R, E1 >: E, A1](that: ZManaged[R1, E1, A1]): ZManaged[R1, E1, A1] =
    self *> that

  /**
   * Returns an effect that executes both this effect and the specified effect,
   * in sequence, combining their results with the specified `f` function.
   */
  def zipWith[R1 <: R, E1 >: E, A1, A2](that: ZManaged[R1, E1, A1])(f: (A, A1) => A2): ZManaged[R1, E1, A2] =
    flatMap(r => that.map(r1 => f(r, r1)))

  /**
   * Returns an effect that executes both this effect and the specified effect,
   * in parallel, combining their results with the specified `f` function. If
   * either side fails, then the other side will be interrupted.
   */
  def zipWithPar[R1 <: R, E1 >: E, A1, A2](that: ZManaged[R1, E1, A1])(f: (A, A1) => A2): ZManaged[R1, E1, A2] =
    ZManaged.ReleaseMap.makeManaged(ExecutionStrategy.Parallel).mapM { parallelReleaseMap =>
      val innerMap =
        ZManaged.ReleaseMap.makeManaged(ExecutionStrategy.Sequential).zio.provideSome[R1]((_, parallelReleaseMap))

      (innerMap zip innerMap) flatMap { case ((_, l), (_, r)) =>
        self.zio
          .provideSome[R1]((_, l))
          .zipWithPar(that.zio.provideSome[R1]((_, r))) {
            // We can safely discard the finalizers here because the resulting ZManaged's early
            // release will trigger the ReleaseMap, which would release both finalizers in
            // parallel.
            case ((_, a), (_, b)) =>
              f(a, b)
          }

      }
    }
}

object ZManaged extends ZManagedPlatformSpecific {

  private sealed abstract class State
  private final case class Exited(nextKey: Long, exit: Exit[Any, Any])            extends State
  private final case class Running(nextKey: Long, finalizers: LongMap[Finalizer]) extends State

  final class AccessPartiallyApplied[R](private val dummy: Boolean = true) extends AnyVal {
    def apply[A](f: R => A): ZManaged[R, Nothing, A] =
      ZManaged.environment.map(f)
  }

  final class AccessMPartiallyApplied[R](private val dummy: Boolean = true) extends AnyVal {
    def apply[E, A](f: R => ZIO[R, E, A]): ZManaged[R, E, A] =
      ZManaged.environment.mapM(f)
  }

  final class AccessManagedPartiallyApplied[R](private val dummy: Boolean = true) extends AnyVal {
    def apply[E, A](f: R => ZManaged[R, E, A]): ZManaged[R, E, A] =
      ZManaged.environment.flatMap(f)
  }

  final class ServiceWithPartiallyApplied[Service](private val dummy: Boolean = true) extends AnyVal {
    def apply[E, A](f: Service => ZIO[Has[Service], E, A])(implicit
      tag: Tag[Service]
    ): ZManaged[Has[Service], E, A] =
      ZManaged.fromEffect(ZIO.serviceWith[Service](f))
  }

  /**
   * A finalizer used in a [[ReleaseMap]]. The [[Exit]] value passed to
   * it is the result of executing [[ZManaged#use]] or an arbitrary value
   * passed into [[ReleaseMap#release]].
   */
  type Finalizer = Exit[Any, Any] => UIO[Any]
  object Finalizer {
    val noop: Finalizer = _ => UIO.unit
  }

  final class IfM[R, E](private val b: ZManaged[R, E, Boolean]) extends AnyVal {
    def apply[R1 <: R, E1 >: E, A](
      onTrue: => ZManaged[R1, E1, A],
      onFalse: => ZManaged[R1, E1, A]
    ): ZManaged[R1, E1, A] =
      b.flatMap(b => if (b) onTrue else onFalse)
  }

  final class ProvideSomeLayer[R0 <: Has[_], -R, +E, +A](private val self: ZManaged[R, E, A]) extends AnyVal {
    def apply[E1 >: E, R1 <: Has[_]](
      layer: ZLayer[R0, E1, R1]
    )(implicit ev1: R0 with R1 <:< R, ev2: NeedsEnv[R], tagged: Tag[R1]): ZManaged[R0, E1, A] =
      self.provideLayer[E1, R0, R0 with R1](ZLayer.identity[R0] ++ layer)
  }

  final class UnlessM[R, E](private val b: ZManaged[R, E, Boolean]) extends AnyVal {
    def apply[R1 <: R, E1 >: E](managed: => ZManaged[R1, E1, Any]): ZManaged[R1, E1, Unit] =
      b.flatMap(b => if (b) unit else managed.unit)
  }

  final class UpdateService[-R, +E, +A, M](private val self: ZManaged[R, E, A]) extends AnyVal {
    def apply[R1 <: R with Has[M]](f: M => M)(implicit ev: Has.IsHas[R1], tag: Tag[M]): ZManaged[R1, E, A] =
      self.provideSome(ev.update(_, f))
  }

  final class WhenM[R, E](private val b: ZManaged[R, E, Boolean]) extends AnyVal {
    def apply[R1 <: R, E1 >: E](managed: => ZManaged[R1, E1, Any]): ZManaged[R1, E1, Unit] =
      b.flatMap(b => if (b) managed.unit else unit)
  }

  /**
   * A `ReleaseMap` represents the finalizers associated with a scope.
   *
   * The design of `ReleaseMap` is inspired by ResourceT, written by Michael Snoyman @snoyberg.
   * (https://github.com/snoyberg/conduit/blob/master/resourcet/Control/Monad/Trans/Resource/Internal.hs)
   */
  abstract class ReleaseMap extends Serializable {

    /**
     * An opaque identifier for a finalizer stored in the map.
     */
    type Key

    /**
     * Adds a finalizer to the finalizers associated with this scope. If the
     * finalizers associated with this scope have already been run this
     * finalizer will be run immediately.
     *
     * The finalizer returned from this method will remove the original finalizer
     * from the map and run it.
     */
    def add(finalizer: Finalizer): UIO[Finalizer]

    /**
     * Adds a finalizer to the finalizers associated with this scope. If the
     * scope is still open, a [[Key]] will be returned. This is an opaque identifier
     * that can be used to activate this finalizer and remove it from the map.
     * from the map. If the scope has been closed, the finalizer will be executed
     * immediately (with the [[Exit]] value with which the scope has ended) and
     * no Key will be returned.
     */
    def addIfOpen(finalizer: Finalizer): UIO[Option[Key]]

    /**
     * Retrieves the finalizer associated with this key.
     */
    def get(key: Key): UIO[Option[Finalizer]]

    /**
     * Runs the specified finalizer and removes it from the finalizers
     * associated with this scope.
     */
    def release(key: Key, exit: Exit[Any, Any]): UIO[Any]

    /**
     * Runs the finalizers associated with this scope using the specified
     * execution strategy. After this action finishes, any finalizers added
     * to this scope will be run immediately.
     */
    def releaseAll(exit: Exit[Any, Any], execStrategy: ExecutionStrategy): UIO[Any]

    /**
     * Removes the finalizer associated with this key and returns it.
     */
    def remove(key: Key): UIO[Option[Finalizer]]

    /**
     * Replaces the finalizer associated with this key and returns it.
     * If the finalizers associated with this scope have already been run this
     * finalizer will be run immediately.
     */
    def replace(key: Key, finalizer: Finalizer): UIO[Option[Finalizer]]
  }

  object ReleaseMap {

    /**
     * Construct a [[ReleaseMap]] wrapped in a [[ZManaged]]. The `ReleaseMap` will
     * be released with the specified [[ExecutionStrategy]] as the release action
     * for the resulting `ZManaged`.
     */
    def makeManaged(executionStrategy: ExecutionStrategy): ZManaged[Any, Nothing, ReleaseMap] =
      ZManaged.makeExit(make)((m, e) => m.releaseAll(e, executionStrategy))

    /**
     * Creates a new ReleaseMap.
     */
    def make: UIO[ReleaseMap] = {
      // The sorting order of the LongMap uses bit ordering (000, 001, ... 111 but with 64 bits). This
      // works out to be `0 ... Long.MaxValue, Long.MinValue, ... -1`. The order of the map is mainly
      // important for the finalization, in which we want to walk it in reverse order. So we insert
      // into the map using keys that will build it in reverse. That way, when we do the final iteration,
      // the finalizers are already in correct order.
      val initialKey: Long = -1L
      def next(l: Long) =
        if (l == 0L) throw new RuntimeException("ReleaseMap wrapped around")
        else if (l == Long.MinValue) Long.MaxValue
        else l - 1

      Ref.make[State](Running(initialKey, LongMap.empty)).map { ref =>
        new ReleaseMap {
          type Key = Long

          def add(finalizer: Finalizer): UIO[Finalizer] =
            addIfOpen(finalizer).map {
              case Some(key) => release(key, _)
              case None      => _ => UIO.unit
            }

          def addIfOpen(finalizer: Finalizer): UIO[Option[Key]] =
            ref.modify {
              case Exited(nextKey, exit) =>
                finalizer(exit).as(None) -> Exited(next(nextKey), exit)
              case Running(nextKey, fins) =>
                UIO.succeed(Some(nextKey)) -> Running(next(nextKey), fins + (nextKey -> finalizer))
            }.flatten

          def release(key: Key, exit: Exit[Any, Any]): UIO[Any] =
            ref.modify {
              case s @ Exited(_, _) => (UIO.unit, s)
              case s @ Running(_, fins) =>
                (fins.get(key).fold(UIO.unit: UIO[Any])(_(exit)), s.copy(finalizers = fins - key))
            }.flatten

          def releaseAll(exit: Exit[Any, Any], execStrategy: ExecutionStrategy): UIO[Any] =
            ref.modify {
              case s @ Exited(_, _) => (UIO.unit, s)
              case Running(nextKey, fins) =>
                execStrategy match {
                  case ExecutionStrategy.Sequential =>
                    (
                      ZIO
                        .foreach(fins: Iterable[(Long, Finalizer)]) { case (_, fin) =>
                          fin.apply(exit).run
                        }
                        .flatMap(results => ZIO.done(Exit.collectAll(results) getOrElse Exit.unit)),
                      Exited(nextKey, exit)
                    )

                  case ExecutionStrategy.Parallel =>
                    (
                      ZIO
                        .foreachPar(fins: Iterable[(Long, Finalizer)]) { case (_, finalizer) =>
                          finalizer(exit).run
                        }
                        .flatMap(results => ZIO.done(Exit.collectAllPar(results) getOrElse Exit.unit)),
                      Exited(nextKey, exit)
                    )

                  case ExecutionStrategy.ParallelN(n) =>
                    (
                      ZIO
                        .foreachParN(n)(fins: Iterable[(Long, Finalizer)]) { case (_, finalizer) =>
                          finalizer(exit).run
                        }
                        .flatMap(results => ZIO.done(Exit.collectAllPar(results) getOrElse Exit.unit)),
                      Exited(nextKey, exit)
                    )

                }
            }.flatten

          def remove(key: Key): UIO[Option[Finalizer]] =
            ref.modify {
              case Exited(nk, exit)  => (None, Exited(nk, exit))
              case Running(nk, fins) => (fins get key, Running(nk, fins - key))
            }

          def replace(key: Key, finalizer: Finalizer): UIO[Option[Finalizer]] =
            ref.modify {
              case Exited(nk, exit)  => (finalizer(exit).as(None), Exited(nk, exit))
              case Running(nk, fins) => (UIO.succeed(fins get key), Running(nk, fins + (key -> finalizer)))
            }.flatten

          def get(key: Key): UIO[Option[Finalizer]] =
            ref.get.map {
              case Exited(_, _)     => None
              case Running(_, fins) => fins get key
            }

        }
      }
    }
  }

  /**
   * Submerges the error case of an `Either` into the `ZManaged`. The inverse
   * operation of `ZManaged.either`.
   */
  def absolve[R, E, A](v: ZManaged[R, E, Either[E, A]]): ZManaged[R, E, A] =
    v.flatMap(fromEither(_))

  /**
   * Create a managed that accesses the environment.
   */
  def access[R]: AccessPartiallyApplied[R] =
    new AccessPartiallyApplied

  /**
   * Create a managed that accesses the environment.
   */
  def accessM[R]: AccessMPartiallyApplied[R] =
    new AccessMPartiallyApplied

  /**
   * Create a managed that accesses the environment.
   */
  def accessManaged[R]: AccessManagedPartiallyApplied[R] =
    new AccessManagedPartiallyApplied

  /**
   * Creates new [[ZManaged]] from a ZIO value that uses a [[ReleaseMap]] and returns
   * a resource and a finalizer.
   *
   * The correct usage of this constructor consists of:
   * - Properly registering a finalizer in the ReleaseMap as part of the ZIO value;
   * - Managing interruption safety - take care to use [[ZIO.uninterruptible]] or
   *   [[ZIO.uninterruptibleMask]] to verify that the finalizer is registered in the
   *   ReleaseMap after acquiring the value;
   * - Returning the finalizer returned from [[ReleaseMap#add]]. This is important
   *   to prevent double-finalization.
   */
  def apply[R, E, A](run0: ZIO[(R, ReleaseMap), E, (Finalizer, A)]): ZManaged[R, E, A] =
    new ZManaged[R, E, A] {
      def zio = run0
    }

  /**
   * Evaluate each effect in the structure from left to right, collecting the
   * the successful values and discarding the empty cases. For a parallel version, see `collectPar`.
   */
  def collect[R, E, A, B, Collection[+Element] <: Iterable[Element]](in: Collection[A])(
    f: A => ZManaged[R, Option[E], B]
  )(implicit bf: BuildFrom[Collection[A], B, Collection[B]]): ZManaged[R, E, Collection[B]] =
    foreach[R, E, A, Option[B], Iterable](in)(a => f(a).optional).map(_.flatten).map(bf.fromSpecific(in))

  /**
   * Evaluate each effect in the structure from left to right, and collect the
   * results. For a parallel version, see `collectAllPar`.
   */
  def collectAll[R, E, A, Collection[+Element] <: Iterable[Element]](
    ms: Collection[ZManaged[R, E, A]]
  )(implicit bf: BuildFrom[Collection[ZManaged[R, E, A]], A, Collection[A]]): ZManaged[R, E, Collection[A]] =
    foreach(ms)(ZIO.identityFn)

  /**
   * Evaluate each effect in the structure from left to right, and discard the
   * results. For a parallel version, see `collectAllPar_`.
   */
  def collectAll_[R, E, A](ms: Iterable[ZManaged[R, E, A]]): ZManaged[R, E, Unit] =
    foreach_(ms)(ZIO.identityFn)

  /**
   * Evaluate each effect in the structure in parallel, and collect the
   * results. For a sequential version, see `collectAll`.
   */
  def collectAllPar[R, E, A, Collection[+Element] <: Iterable[Element]](
    as: Collection[ZManaged[R, E, A]]
  )(implicit bf: BuildFrom[Collection[ZManaged[R, E, A]], A, Collection[A]]): ZManaged[R, E, Collection[A]] =
    foreachPar(as)(ZIO.identityFn)

  /**
   * Evaluate each effect in the structure in parallel, and discard the
   * results. For a sequential version, see `collectAll_`.
   */
  def collectAllPar_[R, E, A](as: Iterable[ZManaged[R, E, A]]): ZManaged[R, E, Unit] =
    foreachPar_(as)(ZIO.identityFn)

  /**
   * Evaluate each effect in the structure in parallel, and collect the
   * results. For a sequential version, see `collectAll`.
   *
   * Unlike `CollectAllPar`, this method will use at most `n` fibers.
   */
  def collectAllParN[R, E, A, Collection[+Element] <: Iterable[Element]](n: Int)(
    as: Collection[ZManaged[R, E, A]]
  )(implicit bf: BuildFrom[Collection[ZManaged[R, E, A]], A, Collection[A]]): ZManaged[R, E, Collection[A]] =
    foreachParN(n)(as)(ZIO.identityFn)

  /**
   * Evaluate each effect in the structure in parallel, and discard the
   * results. For a sequential version, see `collectAll_`.
   *
   * Unlike `CollectAllPar_`, this method will use at most `n` fibers.
   */
  def collectAllParN_[R, E, A](n: Int)(as: Iterable[ZManaged[R, E, A]]): ZManaged[R, E, Unit] =
    foreachParN_(n)(as)(ZIO.identityFn)

  /**
   * Collects the first element of the `Iterable[A]` for which the effectual
   * function `f` returns `Some`.
   */
  def collectFirst[R, E, A, B](as: Iterable[A])(f: A => ZManaged[R, E, Option[B]]): ZManaged[R, E, Option[B]] =
    effectTotal(as.iterator).flatMap { iterator =>
      def loop: ZManaged[R, E, Option[B]] =
        if (iterator.hasNext) f(iterator.next()).flatMap(_.fold(loop)(some(_)))
        else none
      loop
    }

  /**
   * Evaluate each effect in the structure in parallel, collecting the
   * the successful values and discarding the empty cases.
   */
  def collectPar[R, E, A, B, Collection[+Element] <: Iterable[Element]](in: Collection[A])(
    f: A => ZManaged[R, Option[E], B]
  )(implicit bf: BuildFrom[Collection[A], B, Collection[B]]): ZManaged[R, E, Collection[B]] =
    foreachPar[R, E, A, Option[B], Iterable](in)(a => f(a).optional).map(_.flatten).map(bf.fromSpecific(in))

  /**
   * Evaluate each effect in the structure in parallel, collecting the
   * the successful values and discarding the empty cases.
   *
   * Unlike `collectPar`, this method will use at most up to `n` fibers.
   */
  def collectParN[R, E, A, B, Collection[+Element] <: Iterable[Element]](n: Int)(in: Collection[A])(
    f: A => ZManaged[R, Option[E], B]
  )(implicit bf: BuildFrom[Collection[A], B, Collection[B]]): ZManaged[R, E, Collection[B]] =
    foreachParN[R, E, A, Option[B], Iterable](n)(in)(a => f(a).optional).map(_.flatten).map(bf.fromSpecific(in))

  /**
   * Similar to Either.cond, evaluate the predicate,
   * return the given A as success if predicate returns true, and the given E as error otherwise
   */
  def cond[E, A](predicate: Boolean, result: => A, error: => E): Managed[E, A] =
    if (predicate) succeed(result) else fail(error)

  /**
   * Returns an effect that dies with the specified `Throwable`.
   * This method can be used for terminating a fiber because a defect has been
   * detected in the code.
   */
  def die(t: => Throwable): ZManaged[Any, Nothing, Nothing] =
    halt(Cause.die(t))

  /**
   * Returns an effect that dies with a [[java.lang.RuntimeException]] having the
   * specified text message. This method can be used for terminating a fiber
   * because a defect has been detected in the code.
   */
  def dieMessage(message: => String): ZManaged[Any, Nothing, Nothing] =
    die(new RuntimeException(message))

  /**
   * Returns an effect from a lazily evaluated [[zio.Exit]] value.
   */
  def done[E, A](r: => Exit[E, A]): ZManaged[Any, E, A] =
    ZManaged.fromEffect(ZIO.done(r))

  /**
   * Lifts a synchronous side-effect into a `ZManaged[R, Throwable, A]`,
   * translating any thrown exceptions into typed failed effects.
   */
  def effect[A](r: => A): ZManaged[Any, Throwable, A] =
    ZManaged.fromEffect(ZIO.effect(r))

  /**
   * Lifts a by-name, pure value into a Managed.
   */
  def effectTotal[A](r: => A): ZManaged[Any, Nothing, A] =
    ZManaged.fromEffect(ZIO.effectTotal(r))

  /**
   * Accesses the whole environment of the effect.
   */
  def environment[R]: ZManaged[R, Nothing, R] =
    ZManaged.fromEffect(ZIO.environment)

  /**
   * Determines whether any element of the `Iterable[A]` satisfies the
   * effectual predicate `f`.
   */
  def exists[R, E, A](as: Iterable[A])(f: A => ZManaged[R, E, Boolean]): ZManaged[R, E, Boolean] =
    effectTotal(as.iterator).flatMap { iterator =>
      def loop: ZManaged[R, E, Boolean] =
        if (iterator.hasNext) f(iterator.next()).flatMap(b => if (b) succeedNow(b) else loop)
        else succeedNow(false)
      loop
    }

  /**
   * Returns an effect that models failure with the specified  error.
   * The moral equivalent of `throw` for pure code.
   */
  def fail[E](error: => E): ZManaged[Any, E, Nothing] =
    halt(Cause.fail(error))

  /**
   * Returns an effect that succeeds with the `Fiber.Id` of the caller.
   */
  lazy val fiberId: ZManaged[Any, Nothing, Fiber.Id] = ZManaged.fromEffect(ZIO.fiberId)

  /**
   * Creates an effect that only executes the provided finalizer as its
   * release action.
   */
  def finalizer[R](f: URIO[R, Any]): ZManaged[R, Nothing, Unit] =
    finalizerExit(_ => f)

  /**
   * Creates an effect that only executes the provided function as its
   * release action.
   */
  def finalizerExit[R](f: Exit[Any, Any] => URIO[R, Any]): ZManaged[R, Nothing, Unit] =
    makeExit(ZIO.unit)((_, e) => f(e))

  /**
   * Creates an effect that executes a finalizer stored in a [[Ref]].
   * The `Ref` is yielded as the result of the effect, allowing for
   * control flows that require mutating finalizers.
   */
  def finalizerRef[R](initial: Finalizer): ZManaged[R, Nothing, Ref[Finalizer]] =
    ZManaged.makeExit(Ref.make(initial))((ref, exit) => ref.get.flatMap(_.apply(exit)))

  /**
   * Returns an effectful function that extracts out the first element of a
   * tuple.
   */
  def first[A]: ZManaged[(A, Any), Nothing, A] =
    fromFunction(_._1)

  /**
   * Returns a managed resource that attempts to acquire the first managed
   * resource and in case of failure, attempts to acquire each of the specified
   * managed resources in order until one of them is successfully acquired,
   * ensuring that the acquired resource is properly released after being used.
   */
  def firstSuccessOf[R, E, A](
    first: ZManaged[R, E, A],
    rest: Iterable[ZManaged[R, E, A]]
  ): ZManaged[R, E, A] =
    rest.foldLeft(first)(_ <> _)

  /**
   * Returns an effect that performs the outer effect first, followed by the
   * inner effect, yielding the value of the inner effect.
   *
   * This method can be used to "flatten" nested effects.
   */
  def flatten[R, E, A](zManaged: ZManaged[R, E, ZManaged[R, E, A]]): ZManaged[R, E, A] =
    zManaged.flatMap(scala.Predef.identity)

  /**
   * Returns an effect that performs the outer effect first, followed by the
   * inner effect, yielding the value of the inner effect.
   *
   * This method can be used to "flatten" nested effects.
   */
  def flattenM[R, E, A](zManaged: ZManaged[R, E, ZIO[R, E, A]]): ZManaged[R, E, A] =
    zManaged.mapM(scala.Predef.identity)

  /**
   * Folds an Iterable[A] using an effectual function f, working sequentially from left to right.
   */
  def foldLeft[R, E, S, A](
    in: Iterable[A]
  )(zero: S)(f: (S, A) => ZManaged[R, E, S]): ZManaged[R, E, S] =
    in.foldLeft(ZManaged.succeedNow(zero): ZManaged[R, E, S])((acc, el) => acc.flatMap(f(_, el)))

  /**
   * Determines whether all elements of the `Iterable[A]` satisfy the effectual
   * predicate `f`.
   */
  def forall[R, E, A](as: Iterable[A])(f: A => ZManaged[R, E, Boolean]): ZManaged[R, E, Boolean] =
    effectTotal(as.iterator).flatMap { iterator =>
      def loop: ZManaged[R, E, Boolean] =
        if (iterator.hasNext) f(iterator.next()).flatMap(b => if (b) loop else succeedNow(b))
        else succeedNow(true)
      loop
    }

  /**
   * Applies the function `f` to each element of the `Collection[A]` and
   * returns the results in a new `Collection[B]`.
   *
   * For a parallel version of this method, see `foreachPar`.
   * If you do not need the results, see `foreach_` for a more efficient implementation.
   */
  def foreach[R, E, A1, A2, Collection[+Element] <: Iterable[Element]](in: Collection[A1])(
    f: A1 => ZManaged[R, E, A2]
  )(implicit bf: BuildFrom[Collection[A1], A2, Collection[A2]]): ZManaged[R, E, Collection[A2]] =
    ZManaged(ZIO.foreach(in.toList)(f(_).zio).map { result =>
      val (fins, as) = result.unzip
      (e => ZIO.foreach(fins.reverse)(_.apply(e)), bf.fromSpecific(in)(as))
    })

  /**
   * Applies the function `f` if the argument is non-empty and
   * returns the results in a new `Option[A2]`.
   */
  final def foreach[R, E, A1, A2](in: Option[A1])(f: A1 => ZManaged[R, E, A2]): ZManaged[R, E, Option[A2]] =
    in.fold[ZManaged[R, E, Option[A2]]](succeed(None))(f(_).map(Some(_)))

  /**
   * Applies the function `f` to each element of the `Collection[A]` and returns
   * the result in a new `Collection[B]` using the specified execution strategy.
   */
  final def foreachExec[R, E, A, B, Collection[+Element] <: Iterable[Element]](as: Collection[A])(
    exec: ExecutionStrategy
  )(f: A => ZManaged[R, E, B])(implicit bf: BuildFrom[Collection[A], B, Collection[B]]): ZManaged[R, E, Collection[B]] =
    exec match {
      case ExecutionStrategy.Parallel     => ZManaged.foreachPar(as)(f)
      case ExecutionStrategy.ParallelN(n) => ZManaged.foreachParN(n)(as)(f)
      case ExecutionStrategy.Sequential   => ZManaged.foreach(as)(f)
    }

  /**
   * Applies the function `f` to each element of the `Collection[A]` in parallel,
   * and returns the results in a new `Collection[B]`.
   *
   * For a sequential version of this method, see `foreach`.
   */
  def foreachPar[R, E, A1, A2, Collection[+Element] <: Iterable[Element]](as: Collection[A1])(
    f: A1 => ZManaged[R, E, A2]
  )(implicit bf: BuildFrom[Collection[A1], A2, Collection[A2]]): ZManaged[R, E, Collection[A2]] =
    ReleaseMap
      .makeManaged(ExecutionStrategy.Parallel)
      .mapM { parallelReleaseMap =>
        val makeInnerMap =
          ReleaseMap.makeManaged(ExecutionStrategy.Sequential).zio.map(_._2).provideSome[Any]((_, parallelReleaseMap))

        ZIO
          .foreachPar(as.toIterable)(a =>
            makeInnerMap.flatMap(innerMap => f(a).zio.map(_._2).provideSome[R]((_, innerMap)))
          )
          .map(bf.fromSpecific(as))
      }

  /**
   * Applies the function `f` to each element of the `Collection[A]` in parallel,
   * and returns the results in a new `Collection[B]`.
   *
   * Unlike `foreachPar`, this method will use at most up to `n` fibers.
   */
  def foreachParN[R, E, A1, A2, Collection[+Element] <: Iterable[Element]](n: Int)(as: Collection[A1])(
    f: A1 => ZManaged[R, E, A2]
  )(implicit bf: BuildFrom[Collection[A1], A2, Collection[A2]]): ZManaged[R, E, Collection[A2]] =
    ReleaseMap
      .makeManaged(ExecutionStrategy.ParallelN(n))
      .mapM { parallelReleaseMap =>
        val makeInnerMap =
          ReleaseMap.makeManaged(ExecutionStrategy.Sequential).zio.map(_._2).provideSome[Any]((_, parallelReleaseMap))

        ZIO
          .foreachParN(n)(as.toIterable)(a =>
            makeInnerMap.flatMap(innerMap => f(a).zio.map(_._2).provideSome[R]((_, innerMap)))
          )
          .map(bf.fromSpecific(as))
      }

  /**
   * Applies the function `f` to each element of the `Iterable[A]` and runs
   * produced effects sequentially.
   *
   * Equivalent to `foreach(as)(f).unit`, but without the cost of building
   * the list of results.
   */
  def foreach_[R, E, A](as: Iterable[A])(f: A => ZManaged[R, E, Any]): ZManaged[R, E, Unit] =
    ZManaged(ZIO.foreach(as)(f(_).zio).map { result =>
      val (fins, _) = result.unzip
      (e => ZIO.foreach(fins.toList.reverse)(_.apply(e)), ())
    })

  /**
   * Applies the function `f` to each element of the `Iterable[A]` and runs
   * produced effects in parallel, discarding the results.
   *
   * For a sequential version of this method, see `foreach_`.
   */
  def foreachPar_[R, E, A](as: Iterable[A])(f: A => ZManaged[R, E, Any]): ZManaged[R, E, Unit] =
    ReleaseMap.makeManaged(ExecutionStrategy.Parallel).mapM { parallelReleaseMap =>
      val makeInnerMap =
        ReleaseMap.makeManaged(ExecutionStrategy.Sequential).zio.map(_._2).provideSome[Any]((_, parallelReleaseMap))

      ZIO.foreachPar_(as)(a => makeInnerMap.flatMap(innerMap => f(a).zio.map(_._2).provideSome[R]((_, innerMap))))
    }

  /**
   * Applies the function `f` to each element of the `Iterable[A]` and runs
   * produced effects in parallel, discarding the results.
   *
   * Unlike `foreachPar_`, this method will use at most up to `n` fibers.
   */
  def foreachParN_[R, E, A](
    n: Int
  )(
    as: Iterable[A]
  )(
    f: A => ZManaged[R, E, Any]
  ): ZManaged[R, E, Unit] =
    ReleaseMap.makeManaged(ExecutionStrategy.ParallelN(n)).mapM { parallelReleaseMap =>
      val makeInnerMap =
        ReleaseMap.makeManaged(ExecutionStrategy.Sequential).zio.map(_._2).provideSome[Any]((_, parallelReleaseMap))

      ZIO.foreachParN_(n)(as)(a => makeInnerMap.flatMap(innerMap => f(a).zio.map(_._2).provideSome[R]((_, innerMap))))
    }

  /**
   * Creates a [[ZManaged]] from an `AutoCloseable` resource. The resource's `close`
   * method will be used as the release action.
   */
  def fromAutoCloseable[R, E, A <: AutoCloseable](fa: ZIO[R, E, A]): ZManaged[R, E, A] =
    make(fa)(a => UIO(a.close()))

  /**
   * Lifts a ZIO[R, E, A] into ZManaged[R, E, A] with no release action. The
   * effect will be performed interruptibly.
   */
  def fromEffect[R, E, A](fa: ZIO[R, E, A]): ZManaged[R, E, A] =
    ZManaged(fa.provideSome[(R, ReleaseMap)](_._1).map((Finalizer.noop, _)))

  /**
   * Lifts a ZIO[R, E, A] into ZManaged[R, E, A] with no release action. The
   * effect will be performed uninterruptibly. You usually want the [[ZManaged.fromEffect]]
   * variant.
   */
  def fromEffectUninterruptible[R, E, A](fa: ZIO[R, E, A]): ZManaged[R, E, A] =
    fromEffect(fa.uninterruptible)

  /**
   * Lifts an `Either` into a `ZManaged` value.
   */
  def fromEither[E, A](v: => Either[E, A]): ZManaged[Any, E, A] =
    effectTotal(v).flatMap(_.fold(fail(_), succeedNow))

  /**
   * Lifts a function `R => A` into a `ZManaged[R, Nothing, A]`.
   */
  def fromFunction[R, A](f: R => A): ZManaged[R, Nothing, A] = ZManaged.fromEffect(ZIO.environment[R]).map(f)

  /**
   * Lifts an effectful function whose effect requires no environment into
   * an effect that requires the input to the function.
   */
  def fromFunctionM[R, E, A](f: R => ZManaged[Any, E, A]): ZManaged[R, E, A] = flatten(fromFunction(f))

  /**
   * Lifts an `Option` into a `ZManaged` but preserves the error as an option in the error channel, making it easier to compose
   * in some scenarios.
   */
  def fromOption[A](v: => Option[A]): ZManaged[Any, Option[Nothing], A] =
    effectTotal(v).flatMap(_.fold[Managed[Option[Nothing], A]](fail(None))(succeedNow))

  /**
   * Lifts a `Try` into a `ZManaged`.
   */
  def fromTry[A](value: => scala.util.Try[A]): TaskManaged[A] =
    effect(value).flatMap {
      case scala.util.Success(v) => succeedNow(v)
      case scala.util.Failure(t) => fail(t)
    }

  /**
   * Returns an effect that models failure with the specified `Cause`.
   */
  def halt[E](cause: => Cause[E]): ZManaged[Any, E, Nothing] =
    ZManaged.fromEffect(ZIO.halt(cause))

  /**
   * Returns the identity effectful function, which performs no effects
   */
  def identity[R]: ZManaged[R, Nothing, R] = fromFunction(scala.Predef.identity)

  /**
   * Runs `onTrue` if the result of `b` is `true` and `onFalse` otherwise.
   */
  def ifM[R, E](b: ZManaged[R, E, Boolean]): ZManaged.IfM[R, E] =
    new ZManaged.IfM(b)

  /**
   * Returns an effect that is interrupted as if by the fiber calling this
   * method.
   */
  lazy val interrupt: ZManaged[Any, Nothing, Nothing] =
    ZManaged.fromEffect(ZIO.descriptor).flatMap(d => halt(Cause.interrupt(d.id)))

  /**
   * Returns an effect that is interrupted as if by the specified fiber.
   */
  def interruptAs(fiberId: => Fiber.Id): ZManaged[Any, Nothing, Nothing] =
    halt(Cause.interrupt(fiberId))

  /**
   * Iterates with the specified effectual function. The moral equivalent of:
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
  def iterate[R, E, S](initial: S)(cont: S => Boolean)(body: S => ZManaged[R, E, S]): ZManaged[R, E, S] =
    if (cont(initial)) body(initial).flatMap(iterate(_)(cont)(body))
    else ZManaged.succeedNow(initial)

  /**
   * Returns a managed effect that describes shifting to the specified executor
   * as the `acquire` action and shifting back to the original executor as the
   * `release` action.
   */
  def lock(executor: => Executor): ZManaged[Any, Nothing, Unit] =
    ZManaged.make(ZIO.descriptorWith(descriptor => ZIO.shift(executor).as(descriptor.executor)))(ZIO.shift).unit

  /**
   * Loops with the specified effectual function, collecting the results into a
   * list. The moral equivalent of:
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
  def loop[R, E, A, S](
    initial: S
  )(cont: S => Boolean, inc: S => S)(body: S => ZManaged[R, E, A]): ZManaged[R, E, List[A]] =
    if (cont(initial))
      body(initial).flatMap(a => loop(inc(initial))(cont, inc)(body).map(as => a :: as))
    else
      ZManaged.succeedNow(List.empty[A])

  /**
   * Loops with the specified effectual function purely for its effects. The
   * moral equivalent of:
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
  def loop_[R, E, S](
    initial: S
  )(cont: S => Boolean, inc: S => S)(body: S => ZManaged[R, E, Any]): ZManaged[R, E, Unit] =
    if (cont(initial)) body(initial) *> loop_(inc(initial))(cont, inc)(body)
    else ZManaged.unit

  /**
   * Lifts a `ZIO[R, E, A]` into `ZManaged[R, E, A]` with a release action.
   * The acquire and release actions will be performed uninterruptibly.
   */
  def make[R, R1 <: R, E, A](acquire: ZIO[R, E, A])(release: A => ZIO[R1, Nothing, Any]): ZManaged[R1, E, A] =
    makeExit(acquire)((a, _) => release(a))

  /**
   * Lifts a `ZIO[R, E, A]` into `ZManaged[R, E, A]` with a release action
   * that does not need access to the resource. The acquire and release actions
   * will be performed uninterruptibly.
   */
  def make_[R, R1 <: R, E, A](acquire: ZIO[R, E, A])(release: ZIO[R1, Nothing, Any]): ZManaged[R1, E, A] =
    make(acquire)(_ => release)

  /**
   * Lifts a synchronous effect into `ZManaged[R, Throwable, A]` with a release action.
   * The acquire and release actions will be performed uninterruptibly.
   */
  def makeEffect[A](acquire: => A)(release: A => Any): ZManaged[Any, Throwable, A] =
    make(Task(acquire))(a => Task(release(a)).orDie)

  /**
   * Lifts a synchronous effect into `ZManaged[R, Throwable, A]` with a release
   * action that does not need access to the resource. The acquire and release
   * actions will be performed uninterruptibly.
   */
  def makeEffect_[A](acquire: => A)(release: => Any): ZManaged[Any, Throwable, A] =
    makeEffect(acquire)(_ => release)

  /**
   * Lifts a synchronous effect that does not throw exceptions into a
   * `ZManaged[Any, Nothing, A]` with a release action. The acquire and
   * release actions will be performed uninterruptibly.
   */
  def makeEffectTotal[A](acquire: => A)(release: A => Any): ZManaged[Any, Nothing, A] =
    make(ZIO.effectTotal(acquire))(a => ZIO.effectTotal(release(a)))

  /**
   * Lifts a synchronous effect that does not throw exceptions into a
   * `ZManaged[Any, Nothing, A]` with a release action that does not need
   * access to the resource. The acquire and release actions will be performed
   * uninterruptibly.
   */
  def makeEffectTotal_[A](acquire: => A)(release: => A): ZManaged[Any, Nothing, A] =
    makeEffectTotal(acquire)(_ => release)

  /**
   * Lifts a `ZIO[R, E, A]` into `ZManaged[R, E, A]` with a release action that handles `Exit`.
   * The acquire and release actions will be performed uninterruptibly.
   */
  def makeExit[R, R1 <: R, E, A](
    acquire: ZIO[R, E, A]
  )(release: (A, Exit[Any, Any]) => ZIO[R1, Nothing, Any]): ZManaged[R1, E, A] =
    ZManaged {
      (for {
        r               <- ZIO.environment[(R1, ReleaseMap)]
        a               <- acquire.provide(r._1)
        releaseMapEntry <- r._2.add(release(a, _).provide(r._1))
      } yield (releaseMapEntry, a)).uninterruptible
    }

  /**
   * Lifts a `ZIO[R, E, A]` into `ZManaged[R, E, A]` with a release action that
   * does not need access to the resource but handles `Exit`. The acquire and
   * release actions will be performed uninterruptibly.
   */
  def makeExit_[R, R1 <: R, E, A](acquire: ZIO[R, E, A])(
    release: Exit[Any, Any] => ZIO[R1, Nothing, Any]
  ): ZManaged[R1, E, A] =
    makeExit(acquire)((_, exit) => release(exit))

  /**
   * Lifts a ZIO[R, E, A] into ZManaged[R, E, A] with a release action.
   * The acquire action will be performed interruptibly, while release
   * will be performed uninterruptibly.
   */
  def makeInterruptible[R, E, A](
    acquire: ZIO[R, E, A]
  )(release: A => URIO[R, Any]): ZManaged[R, E, A] =
    ZManaged.fromEffect(acquire).onExitFirst(_.foreach(release))

  /**
   * Lifts a ZIO[R, E, A] into ZManaged[R, E, A] with a release action that
   * does not require access to the resource. The acquire action will be
   * performed interruptibly, while release will be performed uninterruptibly.
   */
  def makeInterruptible_[R, R1 <: R, E, A](
    acquire: ZIO[R, E, A]
  )(release: ZIO[R1, Nothing, Any]): ZManaged[R1, E, A] =
    makeInterruptible[R1, E, A](acquire)(_ => release)

  /**
   * Creates a ZManaged from a [[Reservation]] produced by an effect. Evaluating
   * the effect that produces the reservation will be performed *uninterruptibly*,
   * while the acquisition step of the reservation will be performed *interruptibly*.
   * The release step will be performed uninterruptibly as usual.
   *
   * This two-phase acquisition allows for resource acquisition flows that can be
   * safely interrupted and released. For an example, see [[Semaphore#withPermitsManaged]].
   */
  def makeReserve[R, E, A](reservation: ZIO[R, E, Reservation[R, E, A]]): ZManaged[R, E, A] =
    ZManaged {
      ZIO.uninterruptibleMask { restore =>
        for {
          tp             <- ZIO.environment[(R, ReleaseMap)]
          (r, releaseMap) = tp
          reserved       <- reservation.provide(r)
          releaseKey     <- releaseMap.addIfOpen(reserved.release(_).provide(r))
          finalizerAndA <- releaseKey match {
                             case Some(key) =>
                               restore(reserved.acquire.provideSome[(R, ReleaseMap)](_._1))
                                 .map((releaseMap.release(key, (_: Exit[Any, Any])), _))
                             case None => ZIO.interrupt
                           }
        } yield finalizerAndA
      }
    }

  /**
   * Sequentially zips the specified effects using the specified combiner
   * function.
   */
  def mapN[R, E, A, B, C](zManaged1: ZManaged[R, E, A], zManaged2: ZManaged[R, E, B])(
    f: (A, B) => C
  ): ZManaged[R, E, C] =
    zManaged1.zipWith(zManaged2)(f)

  /**
   * Sequentially zips the specified effects using the specified combiner
   * function.
   */
  def mapN[R, E, A, B, C, D](
    zManaged1: ZManaged[R, E, A],
    zManaged2: ZManaged[R, E, B],
    zManaged3: ZManaged[R, E, C]
  )(f: (A, B, C) => D): ZManaged[R, E, D] =
    for {
      a <- zManaged1
      b <- zManaged2
      c <- zManaged3
    } yield f(a, b, c)

  /**
   * Sequentially zips the specified effects using the specified combiner
   * function.
   */
  def mapN[R, E, A, B, C, D, F](
    zManaged1: ZManaged[R, E, A],
    zManaged2: ZManaged[R, E, B],
    zManaged3: ZManaged[R, E, C],
    zManaged4: ZManaged[R, E, D]
  )(f: (A, B, C, D) => F): ZManaged[R, E, F] =
    for {
      a <- zManaged1
      b <- zManaged2
      c <- zManaged3
      d <- zManaged4
    } yield f(a, b, c, d)

  /**
   * Returns an effect that executes the specified effects in parallel,
   * combining their results with the specified `f` function. If any effect
   * fails, then the other effects will be interrupted.
   */
  def mapParN[R, E, A, B, C](zManaged1: ZManaged[R, E, A], zManaged2: ZManaged[R, E, B])(
    f: (A, B) => C
  ): ZManaged[R, E, C] =
    zManaged1.zipWithPar(zManaged2)(f)

  /**
   * Returns an effect that executes the specified effects in parallel,
   * combining their results with the specified `f` function. If any effect
   * fails, then the other effects will be interrupted.
   */
  def mapParN[R, E, A, B, C, D](
    zManaged1: ZManaged[R, E, A],
    zManaged2: ZManaged[R, E, B],
    zManaged3: ZManaged[R, E, C]
  )(f: (A, B, C) => D): ZManaged[R, E, D] =
    (zManaged1 <&> zManaged2 <&> zManaged3).map { case ((a, b), c) =>
      f(a, b, c)
    }

  /**
   * Returns an effect that executes the specified effects in parallel,
   * combining their results with the specified `f` function. If any effect
   * fails, then the other effects will be interrupted.
   */
  def mapParN[R, E, A, B, C, D, F](
    zManaged1: ZManaged[R, E, A],
    zManaged2: ZManaged[R, E, B],
    zManaged3: ZManaged[R, E, C],
    zManaged4: ZManaged[R, E, D]
  )(f: (A, B, C, D) => F): ZManaged[R, E, F] =
    (zManaged1 <&> zManaged2 <&> zManaged3 <&> zManaged4).map { case (((a, b), c), d) =>
      f(a, b, c, d)
    }

  /**
   * Merges an `Iterable[ZManaged]` to a single `ZManaged`, working sequentially.
   */
  def mergeAll[R, E, A, B](in: Iterable[ZManaged[R, E, A]])(zero: B)(f: (B, A) => B): ZManaged[R, E, B] =
    in.foldLeft[ZManaged[R, E, B]](succeedNow(zero))(_.zipWith(_)(f))

  /**
   * Merges an `Iterable[ZManaged]` to a single `ZManaged`, working in parallel.
   *
   * Due to the parallel nature of this combinator, `f` must be both:
   * - commutative: `f(a, b) == f(b, a)`
   * - associative: `f(a, f(b, c)) == f(f(a, b), c)`
   */
  def mergeAllPar[R, E, A, B](in: Iterable[ZManaged[R, E, A]])(zero: B)(f: (B, A) => B): ZManaged[R, E, B] =
    ReleaseMap.makeManaged(ExecutionStrategy.Parallel).mapM { parallelReleaseMap =>
      ZIO.mergeAllPar(in.map(_.zio.map(_._2)))(zero)(f).provideSome[R]((_, parallelReleaseMap))
    }

  /**
   * Merges an `Iterable[ZManaged]` to a single `ZManaged`, working in parallel.
   *
   * Due to the parallel nature of this combinator, `f` must be both:
   * - commutative: `f(a, b) == f(b, a)`
   * - associative: `f(a, f(b, c)) == f(f(a, b), c)`
   *
   * Unlike `mergeAllPar`, this method will use at most up to `n` fibers.
   */
  def mergeAllParN[R, E, A, B](
    n: Int
  )(
    in: Iterable[ZManaged[R, E, A]]
  )(
    zero: B
  )(
    f: (B, A) => B
  ): ZManaged[R, E, B] =
    ReleaseMap.makeManaged(ExecutionStrategy.ParallelN(n)).mapM { parallelReleaseMap =>
      ZIO.mergeAllParN(n)(in.map(_.zio.map(_._2)))(zero)(f).provideSome[R]((_, parallelReleaseMap))
    }

  /**
   * Returns a `ZManaged` that never acquires a resource.
   */
  lazy val never: ZManaged[Any, Nothing, Nothing] = ZManaged.fromEffect(ZIO.never)

  /**
   * Returns a `ZManaged` with the empty value.
   */
  val none: Managed[Nothing, Option[Nothing]] =
    succeedNow(None)

  /**
   * A scope in which resources can be safely preallocated. Passing a [[ZManaged]]
   * to the `apply` method will create (inside an effect) a managed resource which
   * is already acquired and cannot fail.
   */
  abstract class PreallocationScope {
    def apply[R, E, A](managed: ZManaged[R, E, A]): ZIO[R, E, Managed[Nothing, A]]
  }

  /**
   * Creates a scope in which resources can be safely preallocated.
   */
  lazy val preallocationScope: Managed[Nothing, PreallocationScope] =
    scope.map { allocate =>
      new PreallocationScope {
        def apply[R, E, A](managed: ZManaged[R, E, A]) =
          allocate(managed).map { case (release, res) =>
            ZManaged.makeExit(ZIO.succeedNow(res))((_, exit) => release(exit))
          }
      }
    }

  /**
   * Reduces an `Iterable[IO]` to a single `IO`, working sequentially.
   */
  def reduceAll[R, E, A](a: ZManaged[R, E, A], as: Iterable[ZManaged[R, E, A]])(
    f: (A, A) => A
  ): ZManaged[R, E, A] =
    as.foldLeft[ZManaged[R, E, A]](a)(_.zipWith(_)(f))

  /**
   * Reduces an `Iterable[IO]` to a single `IO`, working in parallel.
   */
  def reduceAllPar[R, E, A](a: ZManaged[R, E, A], as: Iterable[ZManaged[R, E, A]])(
    f: (A, A) => A
  ): ZManaged[R, E, A] =
    ReleaseMap.makeManaged(ExecutionStrategy.Parallel).mapM { parallelReleaseMap =>
      ZIO.reduceAllPar(a.zio.map(_._2), as.map(_.zio.map(_._2)))(f).provideSome[R]((_, parallelReleaseMap))
    }

  /**
   * Reduces an `Iterable[IO]` to a single `IO`, working in parallel.
   *
   * Unlike `mergeAllPar`, this method will use at most up to `n` fibers.
   *
   * This is not implemented in terms of ZIO.foreach / ZManaged.zipWithPar as otherwise all reservation phases would always run, causing unnecessary work
   */
  def reduceAllParN[R, E, A](
    n: Int
  )(
    a: ZManaged[R, E, A],
    as: Iterable[ZManaged[R, E, A]]
  )(
    f: (A, A) => A
  ): ZManaged[R, E, A] =
    ReleaseMap.makeManaged(ExecutionStrategy.ParallelN(n)).mapM { parallelReleaseMap =>
      ZIO
        .reduceAllParN[(R, ReleaseMap), (R, ReleaseMap), E, A](n)(a.zio.map(_._2), as.map(_.zio.map(_._2)))(f)
        .provideSome[R]((_, parallelReleaseMap))
    }

  /**
   * Requires that the given `ZManaged[E, Option[A]]` contain a value. If there is no
   * value, then the specified error will be raised.
   */
  def require[R, E, A](error: => E): ZManaged[R, E, Option[A]] => ZManaged[R, E, A] =
    (zManaged: ZManaged[R, E, Option[A]]) => zManaged.flatMap(_.fold[ZManaged[R, E, A]](fail(error))(succeedNow))

  /**
   * Lifts a pure `Reservation[R, E, A]` into `ZManaged[R, E, A]`. The acquisition step
   * is performed interruptibly.
   */
  def reserve[R, E, A](reservation: Reservation[R, E, A]): ZManaged[R, E, A] =
    makeReserve(ZIO.succeed(reservation))

  /**
   * Provides access to the entire map of resources allocated by this ZManaged.
   */
  lazy val releaseMap: ZManaged[Any, Nothing, ReleaseMap] =
    apply(ZIO.environment[(Any, ReleaseMap)].map(tp => (Finalizer.noop, tp._2)))

  /**
   * Returns an ZManaged that accesses the runtime, which can be used to
   * (unsafely) execute tasks. This is useful for integration with legacy
   * code that must call back into ZIO code.
   */
  def runtime[R]: ZManaged[R, Nothing, Runtime[R]] =
    ZManaged.fromEffect(ZIO.runtime[R])

  def sandbox[R, E, A](v: ZManaged[R, E, A]): ZManaged[R, Cause[E], A] =
    v.sandbox

  /**
   * Returns a managed effect that describes shifting to the specified executor
   * as the `acquire` action with no release action.
   */
  def shift(executor: => Executor): ZManaged[Any, Nothing, Unit] =
    ZManaged.fromEffect(ZIO.shift(executor))

  /**
   * A scope in which [[ZManaged]] values can be safely allocated. Passing a managed
   * resource to the `apply` method will return an effect that allocates the resource
   * and returns it with an early-release handle.
   */
  abstract class Scope {
    def apply[R, E, A](managed: ZManaged[R, E, A]): ZIO[R, E, (ZManaged.Finalizer, A)]
  }

  /**
   * Creates a scope in which resources can be safely allocated into together with a release action.
   */
  lazy val scope: Managed[Nothing, Scope] =
    ZManaged.releaseMap.map { finalizers =>
      new Scope {
        override def apply[R, E, A](managed: ZManaged[R, E, A]) =
          for {
            r  <- ZIO.environment[R]
            tp <- managed.zio.provide((r, finalizers))
          } yield tp
      }
    }

  /**
   * Returns an effectful function that extracts out the second element of a
   * tuple.
   */
  def second[A]: ZManaged[(Any, A), Nothing, A] =
    fromFunction(_._2)

  /**
   * Accesses the specified service in the environment of the effect.
   */
  def service[A: Tag]: ZManaged[Has[A], Nothing, A] =
    ZManaged.access(_.get[A])

  /**
   * Accesses the specified services in the environment of the effect.
   */
  def services[A: Tag, B: Tag]: ZManaged[Has[A] with Has[B], Nothing, (A, B)] =
    ZManaged.access(r => (r.get[A], r.get[B]))

  /**
   * Accesses the specified services in the environment of the effect.
   */
  def services[A: Tag, B: Tag, C: Tag]: ZManaged[Has[A] with Has[B] with Has[C], Nothing, (A, B, C)] =
    ZManaged.access(r => (r.get[A], r.get[B], r.get[C]))

  /**
   * Accesses the specified services in the environment of the effect.
   */
  def services[A: Tag, B: Tag, C: Tag, D: Tag]
    : ZManaged[Has[A] with Has[B] with Has[C] with Has[D], Nothing, (A, B, C, D)] =
    ZManaged.access(r => (r.get[A], r.get[B], r.get[C], r.get[D]))

  /**
   * Effectfully accesses the specified service in the environment of the effect.
   *
   * {{{
   * def foo(int: Int) = ZManaged.serviceWith[Foo](_.foo(int))
   * }}}
   */
  def serviceWith[Service]: ServiceWithPartiallyApplied[Service] =
    new ServiceWithPartiallyApplied[Service]

  /**
   *  Returns an effect with the optional value.
   */
  def some[A](a: => A): UManaged[Option[A]] =
    succeed(Some(a))

  /**
   * Lifts a lazy, pure value into a Managed.
   */
  def succeed[A](r: => A): ZManaged[Any, Nothing, A] =
    ZManaged(ZIO.succeed((Finalizer.noop, r)))

  /**
   * Returns a lazily constructed Managed.
   */
  def suspend[R, E, A](zManaged: => ZManaged[R, E, A]): ZManaged[R, E, A] =
    flatten(effectTotal(zManaged))

  /**
   * Returns an effectful function that merely swaps the elements in a `Tuple2`.
   */
  def swap[A, B]: ZManaged[(A, B), Nothing, (B, A)] = fromFunction(_.swap)

  /**
   * Returns a ZManaged value that represents a managed resource that can be safely
   * swapped within the scope of the ZManaged. The function provided inside the
   * ZManaged can be used to switch the resource currently in use.
   *
   * When the resource is switched, the finalizer for the previous finalizer will
   * be executed uninterruptibly. If the effect executing inside the [[ZManaged#use]]
   * is interrupted, the finalizer for the resource currently in use is guaranteed
   * to execute.
   *
   * This constructor can be used to create an expressive control flow that uses
   * several instances of a managed resource. For example:
   * {{{
   * def makeWriter: Task[FileWriter]
   * trait FileWriter {
   *   def write(data: Int): Task[Unit]
   *   def close: UIO[Unit]
   * }
   *
   * val elements = List(1, 2, 3, 4)
   * val writingProgram =
   *   ZManaged.switchable[Any, Throwable, FileWriter].use { switchWriter =>
   *     ZIO.foreach_(elements) { element =>
   *       for {
   *         writer <- switchWriter(makeWriter.toManaged(_.close))
   *         _      <- writer.write(element)
   *       } yield ()
   *     }
   *   }
   * }}}
   */
  def switchable[R, E, A]: ZManaged[R, Nothing, ZManaged[R, E, A] => ZIO[R, E, A]] =
    for {
      releaseMap <- ZManaged.releaseMap
      key <- releaseMap
               .addIfOpen(_ => UIO.unit)
               .flatMap {
                 case Some(key) => UIO.succeed(key)
                 case None      => ZIO.interrupt
               }
               .toManaged_
      switch = (newResource: ZManaged[R, E, A]) =>
                 ZIO.uninterruptibleMask { restore =>
                   for {
                     _ <- releaseMap
                            .replace(key, _ => UIO.unit)
                            .flatMap(_.map(_.apply(Exit.unit)).getOrElse(ZIO.unit))
                     r     <- ZIO.environment[R]
                     inner <- ReleaseMap.make
                     a     <- restore(newResource.zio.provide((r, inner)))
                     _ <- releaseMap
                            .replace(key, inner.releaseAll(_, ExecutionStrategy.Sequential))
                   } yield a._2
                 }
    } yield switch

  /**
   * Returns the effect resulting from mapping the success of this effect to unit.
   */
  lazy val unit: ZManaged[Any, Nothing, Unit] = ZManaged.succeedNow(())

  /**
   * The moral equivalent of `if (!p) exp`
   */
  def unless[R, E](b: => Boolean)(zio: => ZManaged[R, E, Any]): ZManaged[R, E, Unit] =
    suspend(if (b) unit else zio.unit)

  /**
   * The moral equivalent of `if (!p) exp` when `p` has side-effects
   */
  def unlessM[R, E](b: ZManaged[R, E, Boolean]): ZManaged.UnlessM[R, E] =
    new ZManaged.UnlessM(b)

  /**
   * The inverse operation to `sandbox`. Submerges the full cause of failure.
   */
  def unsandbox[R, E, A](v: ZManaged[R, Cause[E], A]): ZManaged[R, E, A] =
    v.mapErrorCause(_.flatten)

  /**
   * Unwraps a `ZManaged` that is inside a `ZIO`.
   */
  def unwrap[R, E, A](fa: ZIO[R, E, ZManaged[R, E, A]]): ZManaged[R, E, A] =
    ZManaged.fromEffect(fa).flatten

  /**
   * The moral equivalent of `if (p) exp`
   */
  def when[R, E](b: => Boolean)(zManaged: => ZManaged[R, E, Any]): ZManaged[R, E, Unit] =
    ZManaged.suspend(if (b) zManaged.unit else unit)

  /**
   * Runs an effect when the supplied `PartialFunction` matches for the given value, otherwise does nothing.
   */
  def whenCase[R, E, A](a: => A)(pf: PartialFunction[A, ZManaged[R, E, Any]]): ZManaged[R, E, Unit] =
    ZManaged.suspend(pf.applyOrElse(a, (_: A) => unit).unit)

  /**
   * Runs an effect when the supplied `PartialFunction` matches for the given effectful value, otherwise does nothing.
   */
  def whenCaseM[R, E, A](
    a: ZManaged[R, E, A]
  )(pf: PartialFunction[A, ZManaged[R, E, Any]]): ZManaged[R, E, Unit] =
    a.flatMap(whenCase(_)(pf))

  /**
   * The moral equivalent of `if (p) exp` when `p` has side-effects
   */
  def whenM[R, E](b: ZManaged[R, E, Boolean]): ZManaged.WhenM[R, E] =
    new ZManaged.WhenM(b)

  /**
   * Locally installs a supervisor and an effect that succeeds with all the
   * children that have been forked in the returned effect.
   */
  def withChildren[R, E, A](get: UIO[Chunk[Fiber.Runtime[Any, Any]]] => ZManaged[R, E, A]): ZManaged[R, E, A] =
    ZManaged.unwrap(Supervisor.track(true).map { supervisor =>
      // Filter out the fiber id of whoever is calling this:
      ZManaged(
        get(supervisor.value.flatMap(children => ZIO.descriptor.map(d => children.filter(_.id != d.id)))).zio
          .supervised(supervisor)
      )
    })

  private[zio] def succeedNow[A](r: A): ZManaged[Any, Nothing, A] =
    ZManaged(IO.succeedNow((Finalizer.noop, r)))

  implicit final class RefineToOrDieOps[R, E <: Throwable, A](private val self: ZManaged[R, E, A]) extends AnyVal {

    /**
     * Keeps some of the errors, and terminates the fiber with the rest.
     */
    def refineToOrDie[E1 <: E: ClassTag](implicit ev: CanFail[E]): ZManaged[R, E1, A] =
      self.refineOrDie { case e: E1 => e }
  }

}

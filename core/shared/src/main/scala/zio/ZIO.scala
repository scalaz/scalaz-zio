/*
 * Copyright 2017-2020 John A. De Goes and the ZIO Contributors
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

import java.util.concurrent.atomic.AtomicReferenceArray

import scala.concurrent.ExecutionContext
import scala.reflect.ClassTag
import scala.util.{ Failure, Success }

import zio.clock.Clock
import zio.duration._
import zio.internal.tracing.{ ZIOFn, ZIOFn1, ZIOFn2 }
import zio.internal.{ Executor, Platform }
import zio.{ TracingStatus => TracingS }

/**
 * A `ZIO[R, E, A]` ("Zee-Oh of Are Eeh Aye") is an immutable data structure
 * that models an effectful program. The effect requires an environment `R`,
 * and the effect may fail with an error `E` or produce a single `A`.
 *
 * Conceptually, this structure is equivalent to `ReaderT[R, EitherT[UIO, E, ?]]`
 * for some infallible effect monad `UIO`, but because monad transformers
 * perform poorly in Scala, this data structure bakes in the reader effect of
 * `ReaderT` with the recoverable error effect of `EitherT` without runtime
 * overhead.
 *
 * `ZIO` values are ordinary immutable values, and may be used like any other
 * value in purely functional code. Because `ZIO` values just *model* effects
 * (like input / output), which must be interpreted by a separate runtime system,
 * `ZIO` values are entirely pure and do not violate referential transparency.
 *
 * `ZIO` values can efficiently describe the following classes of effects:
 *
 *  - '''Pure Values''' — `ZIO.succeed`
 *  - '''Error Effects''' — `ZIO.fail`
 *  - '''Synchronous Effects''' — `IO.effect`
 *  - '''Asynchronous Effects''' — `IO.effectAsync`
 *  - '''Concurrent Effects''' — `IO#fork`
 *  - '''Resource Effects''' — `IO#bracket`
 *  - '''Contextual Effects''' — `ZIO.access`
 *
 * The concurrency model is based on ''fibers'', a user-land lightweight thread,
 * which permit cooperative multitasking, fine-grained interruption, and very
 * high performance with large numbers of concurrently executing fibers.
 *
 * `ZIO` values compose with other `ZIO` values in a variety of ways to build
 * complex, rich, interactive applications. See the methods on `ZIO` for more
 * details about how to compose `ZIO` values.
 *
 * In order to integrate with Scala, `ZIO` values must be interpreted into the
 * Scala runtime. This process of interpretation executes the effects described
 * by a given immutable `ZIO` value. For more information on interpreting `ZIO`
 * values, see the default interpreter in `BootstrapRuntime` or the safe main
 * function in `App`.
 */
sealed trait ZIO[-R, +E, +A] extends Serializable with ZIOPlatformSpecific[R, E, A] { self =>

  /**
   * Sequentially zips this effect with the specified effect, combining the
   * results into a tuple.
   */
  final def &&&[R1 <: R, E1 >: E, B](that: ZIO[R1, E1, B]): ZIO[R1, E1, (A, B)] =
    self.zipWith(that)((a, b) => (a, b))

  /**
   * Returns an effect that executes both this effect and the specified effect,
   * in parallel, returning result of provided effect. If either side fails,
   * then the other side will be interrupted, interrupted the result.
   */
  final def &>[R1 <: R, E1 >: E, B](that: ZIO[R1, E1, B]): ZIO[R1, E1, B] =
    self.zipWithPar(that)((_, b) => b)

  /**
   * Splits the environment, providing the first part to this effect and the
   * second part to that effect.
   */
  final def ***[R1, E1 >: E, B](that: ZIO[R1, E1, B]): ZIO[(R, R1), E1, (A, B)] =
    (ZIO.first[E1, R, R1] >>> self) &&& (ZIO.second[E1, R, R1] >>> that)

  /**
   * A variant of `flatMap` that ignores the value produced by this effect.
   */
  final def *>[R1 <: R, E1 >: E, B](that: => ZIO[R1, E1, B]): ZIO[R1, E1, B] =
    self.flatMap(new ZIO.ZipRightFn(() => that))

  /**
   * Depending on provided environment, returns either this one or the other
   * effect lifted in `Left` or `Right`, respectively.
   */
  final def +++[R1, B, E1 >: E](that: ZIO[R1, E1, B]): ZIO[Either[R, R1], E1, Either[A, B]] =
    ZIO.accessM[Either[R, R1]](_.fold(self.provide(_).map(Left(_)), that.provide(_).map(Right(_))))

  /**
   * Returns an effect that executes both this effect and the specified effect,
   * in parallel, this effect result returned. If either side fails,
   * then the other side will be interrupted, interrupted the result.
   */
  final def <&[R1 <: R, E1 >: E, B](that: ZIO[R1, E1, B]): ZIO[R1, E1, A] =
    self.zipWithPar(that)((a, _) => a)

  /**
   * Returns an effect that executes both this effect and the specified effect,
   * in parallel, combining their results into a tuple. If either side fails,
   * then the other side will be interrupted, interrupted the result.
   */
  final def <&>[R1 <: R, E1 >: E, B](that: ZIO[R1, E1, B]): ZIO[R1, E1, (A, B)] =
    self.zipWithPar(that)((a, b) => (a, b))

  /**
   * Sequences the specified effect after this effect, but ignores the
   * value produced by the effect.
   */
  final def <*[R1 <: R, E1 >: E, B](that: => ZIO[R1, E1, B]): ZIO[R1, E1, A] =
    self.flatMap(new ZIO.ZipLeftFn(() => that))

  /**
   * Alias for `&&&`.
   */
  final def <*>[R1 <: R, E1 >: E, B](that: ZIO[R1, E1, B]): ZIO[R1, E1, (A, B)] =
    self &&& that

  /**
   * Operator alias for `compose`.
   */
  final def <<<[R1, E1 >: E](that: ZIO[R1, E1, R]): ZIO[R1, E1, A] =
    that >>> self

  /**
   * Operator alias for `orElse`.
   */
  final def <>[R1 <: R, E2, A1 >: A](that: => ZIO[R1, E2, A1])(implicit ev: CanFail[E]): ZIO[R1, E2, A1] =
    orElse(that)

  /**
   * Alias for `flatMap`.
   *
   * {{{
   * val parsed = readFile("foo.txt") >>= parseFile
   * }}}
   */
  final def >>=[R1 <: R, E1 >: E, B](k: A => ZIO[R1, E1, B]): ZIO[R1, E1, B] = flatMap(k)

  /**
   * Operator alias for `andThen`.
   */
  final def >>>[R1 >: A, E1 >: E, B](that: ZIO[R1, E1, B]): ZIO[R, E1, B] =
    self.flatMap(that.provide)

  /**
   * Depending on provided environment returns either this one or the other effect.
   */
  final def |||[R1, E1 >: E, A1 >: A](that: ZIO[R1, E1, A1]): ZIO[Either[R, R1], E1, A1] =
    ZIO.accessM(_.fold(self.provide, that.provide))

  /**
   * Returns an effect that submerges the error case of an `Either` into the
   * `ZIO`. The inverse operation of `ZIO.either`.
   */
  final def absolve[R1 <: R, E1, B](implicit ev1: ZIO[R, E, A] <:< ZIO[R1, E1, Either[E1, B]]): ZIO[R1, E1, B] =
    ZIO.absolve[R1, E1, B](ev1(self))

  /**
   * Attempts to convert defects into a failure, throwing away all information
   * about the cause of the failure.
   */
  final def absorb(implicit ev: E <:< Throwable): ZIO[R, Throwable, A] =
    absorbWith(ev)

  /**
   * Attempts to convert defects into a failure, throwing away all information
   * about the cause of the failure.
   */
  final def absorbWith(f: E => Throwable): ZIO[R, Throwable, A] =
    self.sandbox
      .foldM(
        cause => ZIO.failNow(cause.squashWith(f)),
        ZIO.succeedNow
      )

  final def andThen[R1 >: A, E1 >: E, B](that: ZIO[R1, E1, B]): ZIO[R, E1, B] =
    self >>> that

  /**
   * Maps the success value of this effect to the specified constant value.
   */
  final def as[B](b: => B): ZIO[R, E, B] = self.flatMap(new ZIO.ConstZIOFn(() => b))

  /**
   * Maps the error value of this effect to the specified constant value.
   */
  @deprecated("Use ZIO#orElseFail", "1.0.0")
  final def asError[E1](e1: => E1)(implicit ev: CanFail[E]): ZIO[R, E1, A] =
    mapError(new ZIO.ConstFn(() => e1))

  /**
   * Maps the success value of this effect to a service.
   */
  final def asService[A1 >: A](implicit tagged: Tagged[A1]): ZIO[R, E, Has[A1]] =
    map(Has(_))

  /**
   * Maps the success value of this effect to an optional value.
   */
  final def asSome: ZIO[R, E, Option[A]] =
    map(Some(_))

  /**
   * Maps the error value of this effect to an optional value.
   */
  final def asSomeError: ZIO[R, Option[E], A] =
    mapError(Some(_))

  /**
   * Returns an effect whose failure and success channels have been mapped by
   * the specified pair of functions, `f` and `g`.
   */
  final def bimap[E2, B](f: E => E2, g: A => B)(implicit ev: CanFail[E]): ZIO[R, E2, B] =
    foldM(e => ZIO.failNow(f(e)), a => ZIO.succeedNow(g(a)))

  /**
   * Shorthand for the uncurried version of `ZIO.bracket`.
   */
  final def bracket[R1 <: R, E1 >: E, B](
    release: A => URIO[R1, Any],
    use: A => ZIO[R1, E1, B]
  ): ZIO[R1, E1, B] = ZIO.bracket(self, release, use)

  /**
   * Shorthand for the curried version of `ZIO.bracket`.
   */
  final def bracket: ZIO.BracketAcquire[R, E, A] = ZIO.bracket(self)

  /**
   * A less powerful variant of `bracket` where the resource acquired by this
   * effect is not needed.
   */
  final def bracket_[R1 <: R, E1 >: E]: ZIO.BracketAcquire_[R1, E1] =
    new ZIO.BracketAcquire_(self)

  /**
   * Uncurried version. Doesn't offer curried syntax and has worse
   * type-inference characteristics, but it doesn't allocate intermediate
   * [[zio.ZIO.BracketAcquire_]] and [[zio.ZIO.BracketRelease_]] objects.
   */
  final def bracket_[R1 <: R, E1 >: E, B](
    release: URIO[R1, Any],
    use: ZIO[R1, E1, B]
  ): ZIO[R1, E1, B] =
    ZIO.bracket(self, (_: A) => release, (_: A) => use)

  /**
   * Shorthand for the uncurried version of `ZIO.bracketExit`.
   */
  final def bracketExit[R1 <: R, E1 >: E, B](
    release: (A, Exit[E1, B]) => URIO[R1, Any],
    use: A => ZIO[R1, E1, B]
  ): ZIO[R1, E1, B] = ZIO.bracketExit(self, release, use)

  /**
   * Shorthand for the curried version of `ZIO.bracketExit`.
   */
  final def bracketExit[R1 <: R, E1 >: E, A1 >: A]: ZIO.BracketExitAcquire[R1, E1, A1] = ZIO.bracketExit(self)

  /**
   * Executes the release effect only if there was an error.
   */
  final def bracketOnError[R1 <: R, E1 >: E, B](
    release: A => URIO[R1, Any]
  )(use: A => ZIO[R1, E1, B]): ZIO[R1, E1, B] =
    ZIO.bracketExit(self)((a: A, eb: Exit[E1, B]) =>
      eb match {
        case Exit.Failure(_) => release(a)
        case _               => ZIO.unit
      }
    )(use)

  /**
   * Shorthand for the uncurried version of `ZIO.bracketFork`.
   */
  @deprecated("Use ZIO#disconnect on an ordinary bracket", "1.0.0")
  final def bracketFork[R1 <: R, E1 >: E, B](
    release: A => URIO[R1, Any],
    use: A => ZIO[R1, E1, B]
  ): ZIO[R1, E1, B] = ZIO.bracketFork(self, release, use)

  /**
   * Shorthand for the curried version of `ZIO.bracketFork`.
   */
  @deprecated("Use ZIO#disconnect on an ordinary bracket", "1.0.0")
  final def bracketFork: ZIO.BracketForkAcquire[R, E, A] = ZIO.bracketFork(self)

  /**
   * A less powerful variant of `bracketFork` where the resource acquired by this
   * effect is not needed.
   */
  @deprecated("Use ZIO#disconnect on an ordinary bracket", "1.0.0")
  final def bracketFork_[R1 <: R, E1 >: E]: ZIO.BracketForkAcquire_[R1, E1] =
    new ZIO.BracketForkAcquire_(self)

  /**
   * Uncurried version of `bracketFork_` Doesn't offer curried syntax and has worse
   * type-inference characteristics, but it doesn't allocate intermediate
   * [[zio.ZIO.BracketForkAcquire_]] and [[zio.ZIO.BracketForkRelease_]] objects.
   */
  @deprecated("Use ZIO#disconnect on an ordinary bracket", "1.0.0")
  final def bracketFork_[R1 <: R, E1 >: E, B](
    release: URIO[R1, Any],
    use: ZIO[R1, E1, B]
  ): ZIO[R1, E1, B] =
    ZIO.bracketFork(self, (_: A) => release, (_: A) => use)

  /**
   * Shorthand for the uncurried version of `ZIO.bracketForkExit`.
   */
  @deprecated("Use ZIO#disconnect on an ordinary bracket", "1.0.0")
  final def bracketForkExit[R1 <: R, E1 >: E, B](
    release: (A, Exit[E1, B]) => URIO[R1, Any],
    use: A => ZIO[R1, E1, B]
  ): ZIO[R1, E1, B] = ZIO.bracketForkExit(self, release, use)

  /**
   * Shorthand for the curried version of `ZIO.bracketForkExit`.
   */
  @deprecated("Use ZIO#disconnect on an ordinary bracket", "1.0.0")
  final def bracketForkExit[R1 <: R, E1 >: E, A1 >: A]: ZIO.BracketForkExitAcquire[R1, E1, A1] =
    ZIO.bracketForkExit(self)

  /**
   * Executes the release effect only if there was an error.
   */
  @deprecated("Use ZIO#disconnect on an ordinary bracket", "1.0.0")
  final def bracketForkOnError[R1 <: R, E1 >: E, B](
    release: A => URIO[R1, Any]
  )(use: A => ZIO[R1, E1, B]): ZIO[R1, E1, B] =
    ZIO.bracketForkExit(self)((a: A, eb: Exit[E1, B]) =>
      eb match {
        case Exit.Failure(_) => release(a)
        case _               => ZIO.unit
      }
    )(use)

  /**
   * Returns an effect that, if evaluated, will return the cached result of
   * this effect. Cached results will expire after `timeToLive` duration.
   */
  final def cached(timeToLive: Duration): ZIO[R with Clock, Nothing, IO[E, A]] = {

    def compute(start: Long): ZIO[R with Clock, Nothing, Option[(Long, Promise[E, A])]] =
      for {
        p <- Promise.make[E, A]
        _ <- self.to(p)
      } yield Some((start + timeToLive.toNanos, p))

    def get(cache: RefM[Option[(Long, Promise[E, A])]]): ZIO[R with Clock, E, A] =
      ZIO.uninterruptibleMask { restore =>
        clock.nanoTime.flatMap { time =>
          cache.updateSomeAndGet {
            case None                              => compute(time)
            case Some((end, _)) if end - time <= 0 => compute(time)
          }.flatMap(a => restore(a.get._2.await))
        }
      }

    for {
      r     <- ZIO.environment[R with Clock]
      cache <- RefM.make[Option[(Long, Promise[E, A])]](None)
    } yield get(cache).provide(r)
  }

  /**
   * Recovers from all errors.
   *
   * {{{
   * openFile("config.json").catchAll(_ => IO.succeed(defaultConfig))
   * }}}
   */
  final def catchAll[R1 <: R, E2, A1 >: A](h: E => ZIO[R1, E2, A1])(implicit ev: CanFail[E]): ZIO[R1, E2, A1] =
    self.foldM[R1, E2, A1](h, new ZIO.SucceedFn(h))

  /**
   * Recovers from all errors with provided Cause.
   *
   * {{{
   * openFile("config.json").catchAllCause(_ => IO.succeed(defaultConfig))
   * }}}
   *
   * @see [[absorb]], [[sandbox]], [[mapErrorCause]] - other functions that can recover from defects
   */
  final def catchAllCause[R1 <: R, E2, A1 >: A](h: Cause[E] => ZIO[R1, E2, A1]): ZIO[R1, E2, A1] =
    self.foldCauseM[R1, E2, A1](h, new ZIO.SucceedFn(h))

  /**
   * Recovers from some or all of the error cases.
   *
   * {{{
   * openFile("data.json").catchSome {
   *   case FileNotFoundException(_) => openFile("backup.json")
   * }
   * }}}
   */
  final def catchSome[R1 <: R, E1 >: E, A1 >: A](
    pf: PartialFunction[E, ZIO[R1, E1, A1]]
  )(implicit ev: CanFail[E]): ZIO[R1, E1, A1] = {
    def tryRescue(c: Cause[E]): ZIO[R1, E1, A1] =
      c.failureOrCause.fold(t => pf.applyOrElse(t, (_: E) => ZIO.haltNow(c)), ZIO.haltNow)

    self.foldCauseM[R1, E1, A1](ZIOFn(pf)(tryRescue), new ZIO.SucceedFn(pf))
  }

  /**
   * Recovers from some or all of the error cases with provided cause.
   *
   * {{{
   * openFile("data.json").catchSomeCause {
   *   case c if (c.interrupted) => openFile("backup.json")
   * }
   * }}}
   */
  final def catchSomeCause[R1 <: R, E1 >: E, A1 >: A](
    pf: PartialFunction[Cause[E], ZIO[R1, E1, A1]]
  ): ZIO[R1, E1, A1] = {
    def tryRescue(c: Cause[E]): ZIO[R1, E1, A1] =
      pf.applyOrElse(c, (_: Cause[E]) => ZIO.haltNow(c))

    self.foldCauseM[R1, E1, A1](ZIOFn(pf)(tryRescue), new ZIO.SucceedFn(pf))
  }

  /**
   * Fail with `e` if the supplied `PartialFunction` does not match, otherwise
   * succeed with the returned value.
   */
  final def collect[E1 >: E, B](e: => E1)(pf: PartialFunction[A, B]): ZIO[R, E1, B] =
    collectM(e)(pf.andThen(ZIO.succeedNow(_)))

  /**
   * Fail with `e` if the supplied `PartialFunction` does not match, otherwise
   * continue with the returned value.
   */
  final def collectM[R1 <: R, E1 >: E, B](e: => E1)(pf: PartialFunction[A, ZIO[R1, E1, B]]): ZIO[R1, E1, B] =
    self.flatMap { v =>
      pf.applyOrElse[A, ZIO[R1, E1, B]](v, _ => ZIO.failNow(e))
    }

  final def compose[R1, E1 >: E](that: ZIO[R1, E1, R]): ZIO[R1, E1, A] = self <<< that

  /**
   * Returns an effect that is delayed from this effect by the specified
   * [[zio.duration.Duration]].
   */
  final def delay(duration: Duration): ZIO[R with Clock, E, A] =
    clock.sleep(duration) *> self

  /**
   * Returns an effect whose interruption will be disconnected from the
   * fiber's own interruption.
   */
  final def disconnect: ZIO[R, E, A] = self.fork(SuperviseMode.InterruptFork).flatMap(_.join)

  /**
   * Repeats this effect until its result satisfies the specified predicate.
   */
  final def doUntil(f: A => Boolean): ZIO[R, E, A] =
    repeat(Schedule.doUntil(f))

  /**
   * Repeats this effect until its result is equal to the predicate.
   */
  final def doUntilEquals[A1 >: A](a: => A1): ZIO[R, E, A1] =
    repeat(Schedule.doUntilEquals(a))

  /**
   * Repeats this effect until its result satisfies the specified effectful predicate.
   */
  final def doUntilM(f: A => UIO[Boolean]): ZIO[R, E, A] =
    repeat(Schedule.doUntilM(f))

  /**
   * Repeats this effect while its result satisfies the specified predicate.
   */
  final def doWhile(f: A => Boolean): ZIO[R, E, A] =
    repeat(Schedule.doWhile(f))

  /**
   * Repeats this effect for as long as the predicate is equal to its result.
   */
  final def doWhileEquals[A1 >: A](a: => A1): ZIO[R, E, A1] =
    repeat(Schedule.doWhileEquals(a))

  /**
   * Repeats this effect while its result satisfies the specified effectful predicate.
   */
  final def doWhileM(f: A => UIO[Boolean]): ZIO[R, E, A] =
    repeat(Schedule.doWhileM(f))

  /**
   * Returns an effect whose failure and success have been lifted into an
   * `Either`.The resulting effect cannot fail, because the failure case has
   * been exposed as part of the `Either` success case.
   *
   * This method is useful for recovering from `ZIO` effects that may fail.
   *
   * The error parameter of the returned `ZIO` is `Nothing`, since it is
   * guaranteed the `ZIO` effect does not model failure.
   */
  final def either(implicit ev: CanFail[E]): URIO[R, Either[E, A]] =
    self.foldM(ZIO.succeedLeft, ZIO.succeedRight)

  /**
   * Returns an effect that, if this effect _starts_ execution, then the
   * specified `finalizer` is guaranteed to begin execution, whether this effect
   * succeeds, fails, or is interrupted.
   *
   * For use cases that need access to the effect's result, see [[ZIO#onExit]].
   *
   * Finalizers offer very powerful guarantees, but they are low-level, and
   * should generally not be used for releasing resources. For higher-level
   * logic built on `ensuring`, see `ZIO#bracket`.
   */
  final def ensuring[R1 <: R](finalizer: URIO[R1, Any]): ZIO[R1, E, A] =
    ZIO.uninterruptibleMask(restore =>
      restore(self)
        .foldCauseM(
          cause1 =>
            finalizer.foldCauseM[R1, E, Nothing](
              cause2 => ZIO.haltNow(cause1 ++ cause2),
              _ => ZIO.haltNow(cause1)
            ),
          value =>
            finalizer.foldCauseM[R1, E, A](
              cause1 => ZIO.haltNow(cause1),
              _ => ZIO.succeedNow(value)
            )
        )
    )

  /**
   * Acts on the children of this fiber (collected into a single fiber),
   * guaranteeing the specified callback will be invoked, whether or not
   * this effect succeeds.
   */
  final def ensuringChild[R1 <: R](f: Fiber[Any, List[Any]] => ZIO[R1, Nothing, Any]): ZIO[R1, E, A] =
    ensuringChildren(children => f(Fiber.collectAll(children)))

  /**
   * Acts on the children of this fiber, guaranteeing the specified callback
   * will be invoked, whether or not this effect succeeds.
   */
  final def ensuringChildren[R1 <: R](f: Iterable[Fiber[Any, Any]] => ZIO[R1, Nothing, Any]): ZIO[R1, E, A] =
    self.ensuring(ZIO.children.flatMap(f))

  /**
   * Returns an effect that ignores errors and runs repeatedly until it eventually succeeds.
   */
  final def eventually(implicit ev: CanFail[E]): URIO[R, A] =
    self orElse eventually

  /**
   * Executes this effect and returns its value, if it succeeds, but otherwise
   * returns the specified value.
   */
  @deprecated("Use ZIO#orElseSucceed", "1.0.0")
  final def fallback[A1 >: A](a: => A1)(implicit ev: CanFail[E]): ZIO[R, Nothing, A1] =
    fold(_ => a, identity)

  /**
   * Dies with specified `Throwable` if the predicate fails.
   */
  final def filterOrDie(p: A => Boolean)(t: => Throwable): ZIO[R, E, A] =
    self.filterOrElse_(p)(ZIO.dieNow(t))

  /**
   * Dies with a [[java.lang.RuntimeException]] having the specified text message
   * if the predicate fails.
   */
  final def filterOrDieMessage(p: A => Boolean)(message: => String): ZIO[R, E, A] =
    self.filterOrElse_(p)(ZIO.dieMessage(message))

  /**
   * Applies `f` if the predicate fails.
   */
  final def filterOrElse[R1 <: R, E1 >: E, A1 >: A](p: A => Boolean)(f: A => ZIO[R1, E1, A1]): ZIO[R1, E1, A1] =
    self.flatMap {
      case v if !p(v) => f(v)
      case v          => ZIO.succeedNow(v)
    }

  /**
   * Supplies `zio` if the predicate fails.
   */
  final def filterOrElse_[R1 <: R, E1 >: E, A1 >: A](p: A => Boolean)(zio: => ZIO[R1, E1, A1]): ZIO[R1, E1, A1] =
    filterOrElse[R1, E1, A1](p)(_ => zio)

  /**
   * Fails with `e` if the predicate fails.
   */
  final def filterOrFail[E1 >: E](p: A => Boolean)(e: => E1): ZIO[R, E1, A] =
    filterOrElse_[R, E1, A](p)(ZIO.failNow(e))

  /**
   * Returns an effect that races this effect with all the specified effects,
   * yielding the value of the first effect to succeed with a value.
   * Losers of the race will be interrupted immediately
   */
  final def firstSuccessOf[R1 <: R, E1 >: E, A1 >: A](rest: Iterable[ZIO[R1, E1, A1]]): ZIO[R1, E1, A1] =
    ZIO.firstSuccessOf(self, rest)

  /**
   * Returns an effect that models the execution of this effect, followed by
   * the passing of its value to the specified continuation function `k`,
   * followed by the effect that it returns.
   *
   * {{{
   * val parsed = readFile("foo.txt").flatMap(file => parseFile(file))
   * }}}
   */
  def flatMap[R1 <: R, E1 >: E, B](k: A => ZIO[R1, E1, B]): ZIO[R1, E1, B] =
    new ZIO.FlatMap(self, k)

  /**
   * Creates a composite effect that represents this effect followed by another
   * one that may depend on the error produced by this one.
   *
   * {{{
   * val parsed = readFile("foo.txt").flatMapError(error => logErrorToFile(error))
   * }}}
   */
  final def flatMapError[R1 <: R, E2](f: E => URIO[R1, E2])(implicit ev: CanFail[E]): ZIO[R1, E2, A] =
    flipWith(_ flatMap f)

  /**
   * Returns an effect that performs the outer effect first, followed by the
   * inner effect, yielding the value of the inner effect.
   *
   * This method can be used to "flatten" nested effects.
   **/
  final def flatten[R1 <: R, E1 >: E, B](implicit ev1: A <:< ZIO[R1, E1, B]): ZIO[R1, E1, B] =
    self.flatMap(a => ev1(a))

  /**
   * Returns an effect that swaps the error/success cases. This allows you to
   * use all methods on the error channel, possibly before flipping back.
   */
  final def flip: ZIO[R, A, E] =
    self.foldM(ZIO.succeedNow, ZIO.failNow)

  /**
   *  Swaps the error/value parameters, applies the function `f` and flips the parameters back
   */
  final def flipWith[R1, A1, E1](f: ZIO[R, A, E] => ZIO[R1, A1, E1]): ZIO[R1, E1, A1] = f(self.flip).flip

  /**
   * Folds over the failure value or the success value to yield an effect that
   * does not fail, but succeeds with the value returned by the left or right
   * function passed to `fold`.
   */
  final def fold[B](failure: E => B, success: A => B)(implicit ev: CanFail[E]): URIO[R, B] =
    foldM(new ZIO.MapFn(failure), new ZIO.MapFn(success))

  /**
   * A more powerful version of `fold` that allows recovering from any kind of failure except interruptions.
   */
  final def foldCause[B](failure: Cause[E] => B, success: A => B): URIO[R, B] =
    foldCauseM(new ZIO.MapFn(failure), new ZIO.MapFn(success))

  /**
   * A more powerful version of `foldM` that allows recovering from any kind of failure except interruptions.
   */
  final def foldCauseM[R1 <: R, E2, B](
    failure: Cause[E] => ZIO[R1, E2, B],
    success: A => ZIO[R1, E2, B]
  ): ZIO[R1, E2, B] =
    new ZIO.Fold(self, failure, success)

  /**
   * Recovers from errors by accepting one effect to execute for the case of an
   * error, and one effect to execute for the case of success.
   *
   * This method has better performance than `either` since no intermediate
   * value is allocated and does not require subsequent calls to `flatMap` to
   * define the next effect.
   *
   * The error parameter of the returned `IO` may be chosen arbitrarily, since
   * it will depend on the `IO`s returned by the given continuations.
   */
  final def foldM[R1 <: R, E2, B](failure: E => ZIO[R1, E2, B], success: A => ZIO[R1, E2, B])(
    implicit ev: CanFail[E]
  ): ZIO[R1, E2, B] =
    foldCauseM(new ZIO.FoldCauseMFailureFn(failure), success)

  /**
   * Repeats this effect forever (until the first error). For more sophisticated
   * schedules, see the `repeat` method.
   */
  final def forever: ZIO[R, E, Nothing] = self *> self.forever

  /**
   * Returns an effect that forks this effect into its own separate fiber,
   * returning the fiber immediately, without waiting for it to begin
   * executing the effect.
   *
   * The returned fiber can be used to interrupt the forked fiber, await its
   * result, or join the fiber. See [[zio.Fiber]] for more information.
   *
   * The fiber is forked with interrupt supervision mode, meaning that when the
   * fiber that forks the child exits, the child will be interrupted.
   *
   * {{{
   * for {
   *   fiber <- subtask.fork
   *   // Do stuff...
   *   a <- fiber.join
   * } yield a
   * }}}
   */
  final def fork: URIO[R, Fiber.Runtime[E, A]] = fork(SuperviseMode.Interrupt)

  /**
   * Returns an effect that forks this effect into its own separate fiber,
   * returning the fiber immediately, without waiting for it to begin
   * executing the effect.
   *
   * The returned fiber can be used to interrupt the forked fiber, await its
   * result, or join the fiber. See [[zio.Fiber]] for more information.
   *
   * The specified supervision mode dictates what happens when the fiber that
   * forks the effect exits. Available supervision modes allow for the parent
   * fiber to interrupt, await, or disown the child fiber. See
   * [[zio.SuperviseMode]] for more details.
   *
   * {{{
   * for {
   *   fiber <- subtask.fork
   *   // Do stuff...
   *   a <- fiber.join
   * } yield a
   * }}}
   */
  final def fork(superviseMode: SuperviseMode): URIO[R, Fiber.Runtime[E, A]] = new ZIO.Fork(self, superviseMode)

  /**
   * Forks the effect into a new fiber, but immediately disowns the fiber, so
   * that when the fiber executing this effect exits, the forked fiber will not
   * automatically be terminated. Disowned fibers become new root fibers.
   */
  final def forkDaemon: URIO[R, Fiber.Runtime[E, A]] =
    ZIO.uninterruptibleMask(restore => restore(self).fork.tap(ZIO.disown))

  /**
   * Forks the fiber in a [[ZManaged]]. Using the [[ZManaged]] value will
   * execute the effect in the fiber, while ensuring its interruption when
   * the effect supplied to [[ZManaged#use]] completes.
   */
  final def forkManaged(
    superviseMode: SuperviseMode = SuperviseMode.Interrupt
  ): ZManaged[R, Nothing, Fiber.Runtime[E, A]] = toManaged_.fork(superviseMode)

  final def forkManaged: ZManaged[R, Nothing, Fiber.Runtime[E, A]] = forkManaged(SuperviseMode.Interrupt)

  /**
   * Forks an effect that will be executed on the specified `ExecutionContext`.
   */
  final def forkOn(
    ec: ExecutionContext,
    superviseMode: SuperviseMode = SuperviseMode.Interrupt
  ): ZIO[R, E, Fiber.Runtime[E, A]] =
    self.on(ec).fork(superviseMode)

  /**
   * Like fork but handles an error with the provided handler.
   */
  final def forkWithErrorHandler(handler: E => UIO[Unit]): URIO[R, Fiber.Runtime[E, A]] =
    onError(new ZIO.FoldCauseMFailureFn(handler)).fork

  /**
   * Unwraps the optional error, defaulting to the provided value.
   */
  final def flattenErrorOption[E1, E2 <: E1](default: E2)(implicit ev: E <:< Option[E1]): ZIO[R, E1, A] =
    self.mapError(e => ev(e).getOrElse(default))

  /**
   * Unwraps the optional success of this effect, but can fail with unit value.
   */
  final def get[E1 >: E, B](implicit ev1: E1 =:= Nothing, ev2: A <:< Option[B]): ZIO[R, Unit, B] =
    ZIO.absolve(self.mapError(ev1).map(ev2(_).toRight(())))

  /**
   * Returns a new effect that, on exit of this effect, invokes the specified
   * handler with all forked (non-daemon) children of this effect.
   */
  final def handleChildrenWith[R1 <: R, E1 >: E](f: Iterable[Fiber[Any, Any]] => URIO[R1, Any]): ZIO[R1, E1, A] =
    self.ensuring(ZIO.children.flatMap(f))

  /**
   * Returns a successful effect with the head of the list if the list is
   * non-empty or fails with the error `None` if the list is empty.
   */
  final def head[B](implicit ev: A <:< List[B]): ZIO[R, Option[E], B] =
    self.foldM(
      e => ZIO.failNow(Some(e)),
      a => ev(a).headOption.fold[ZIO[R, Option[E], B]](ZIO.failNow(None))(ZIO.succeedNow)
    )

  /**
   * Returns a new effect that ignores the success or failure of this effect.
   */
  final def ignore: URIO[R, Unit] = self.fold(ZIO.unitFn, ZIO.unitFn)

  /**
   * Returns a new effect that performs the same operations as this effect, but
   * interruptibly, even if composed inside of an uninterruptible region.
   *
   * Note that effects are interruptible by default, so this function only has
   * meaning if used within an uninterruptible region.
   *
   * WARNING: This operator "punches holes" into effects, allowing them to be
   * interrupted in unexpected places. Do not use this operator unless you know
   * exactly what you are doing. Instead, you should use [[ZIO.uninterruptibleMask]].
   */
  final def interruptible: ZIO[R, E, A] = interruptStatus(InterruptStatus.Interruptible)

  @deprecated("Use ZIO#interruptible.disconnect", "1.0.0")
  final def interruptibleFork: ZIO[R, E, A] = self.interruptible.disconnect

  /**
   * Switches the interrupt status for this effect. If `true` is used, then the
   * effect becomes interruptible (the default), while if `false` is used, then
   * the effect becomes uninterruptible. These changes are compositional, so
   * they only affect regions of the effect.
   */
  final def interruptStatus(flag: InterruptStatus): ZIO[R, E, A] = new ZIO.InterruptStatus(self, flag)

  /**
   * Joins this effect with the specified effect.
   */
  final def join[R1, E1 >: E, A1 >: A](that: ZIO[R1, E1, A1]): ZIO[Either[R, R1], E1, A1] = self ||| that

  /**
   * Returns an effect which is guaranteed to be executed on the specified
   * executor. The specified effect will always run on the specified executor,
   * even in the presence of asynchronous boundaries.
   *
   * This is useful when an effect must be executued somewhere, for example:
   * on a UI thread, inside a client library's thread pool, inside a blocking
   * thread pool, inside a low-latency thread pool, or elsewhere.
   *
   * The `lock` function composes with the innermost `lock` taking priority.
   * Use of this method does not alter the execution semantics of other effects
   * composed with this one, making it easy to compositionally reason about
   * where effects are running.
   */
  final def lock(executor: Executor): ZIO[R, E, A] =
    ZIO.lock(executor)(self)

  /**
   * Returns an effect whose success is mapped by the specified `f` function.
   */
  def map[B](f: A => B): ZIO[R, E, B] = new ZIO.FlatMap(self, new ZIO.MapFn(f))

  /**
   * Returns an effect with its error channel mapped using the specified
   * function. This can be used to lift a "smaller" error into a "larger"
   * error.
   */
  final def mapError[E2](f: E => E2)(implicit ev: CanFail[E]): ZIO[R, E2, A] =
    self.foldCauseM(new ZIO.MapErrorFn(f), new ZIO.SucceedFn(f))

  /**
   * Returns an effect with its full cause of failure mapped using the
   * specified function. This can be used to transform errors while
   * preserving the original structure of `Cause`.
   *
   * @see [[absorb]], [[sandbox]], [[catchAllCause]] - other functions for dealing with defects
   */
  final def mapErrorCause[E2](h: Cause[E] => Cause[E2]): ZIO[R, E2, A] =
    self.foldCauseM(new ZIO.MapErrorCauseFn(h), new ZIO.SucceedFn(h))

  /**
   * Returns an effect that, if evaluated, will return the lazily computed result
   * of this effect.
   */
  final def memoize: URIO[R, IO[E, A]] =
    for {
      r <- ZIO.environment[R]
      p <- Promise.make[E, A]
      l <- Promise.make[Nothing, Unit]
      _ <- (l.await *> ((self provide r) to p)).fork
    } yield l.succeed(()) *> p.await

  /**
   * Returns a new effect where the error channel has been merged into the
   * success channel to their common combined type.
   */
  final def merge[A1 >: A](implicit ev1: E <:< A1, ev2: CanFail[E]): URIO[R, A1] =
    self.foldM(e => ZIO.succeedNow(ev1(e)), ZIO.succeedNow)

  /**
   * Requires the option produced by this value to be `None`.
   */
  final def none[B](implicit ev: A <:< Option[B]): ZIO[R, Option[E], Unit] =
    self.foldM(
      e => ZIO.failNow(Some(e)),
      a => a.fold[ZIO[R, Option[E], Unit]](ZIO.succeedNow(()))(_ => ZIO.failNow(None))
    )

  /**
   * Executes the effect on the specified `ExecutionContext` and then shifts back
   * to the default one.
   */
  final def on(ec: ExecutionContext): ZIO[R, E, A] =
    self.lock(Executor.fromExecutionContext(Int.MaxValue)(ec))

  /**
   * Returns an effect that will be executed at most once, even if it is
   * evaluated multiple times.
   */
  final def once: UIO[ZIO[R, E, Unit]] =
    Ref.make(true).map(ref => self.whenM(ref.getAndSet(false)))

  /**
   * Runs the specified effect if this effect fails, providing the error to the
   * effect if it exists. The provided effect will not be interrupted.
   */
  final def onError[R1 <: R](cleanup: Cause[E] => URIO[R1, Any]): ZIO[R1, E, A] =
    ZIO.bracketExit(ZIO.unit)((_, eb: Exit[E, A]) =>
      eb match {
        case Exit.Success(_)     => ZIO.unit
        case Exit.Failure(cause) => cleanup(cause)
      }
    )(_ => self)

  /**
   * Ensures that a cleanup functions runs, whether this effect succeeds,
   * fails, or is interrupted.
   */
  final def onExit[R1 <: R](cleanup: Exit[E, A] => URIO[R1, Any]): ZIO[R1, E, A] =
    ZIO.bracketExit(ZIO.unit)((_, exit: Exit[E, A]) => cleanup(exit))(_ => self)

  /**
   * Propagates the success value to the first element of a tuple, but
   * passes the effect input `R` along unmodified as the second element
   * of the tuple.
   */
  final def onFirst[R1 <: R]: ZIO[R1, E, (A, R1)] =
    self &&& ZIO.identity[R1]

  /**
   * Runs the specified effect if this effect is interrupted.
   */
  final def onInterrupt[R1 <: R](cleanup: URIO[R1, Any]): ZIO[R1, E, A] =
    onInterrupt(_ => cleanup)

  /**
   * Calls the specified function, and runs the effect it returns, if this
   * effect is interrupted.
   */
  final def onInterrupt[R1 <: R](cleanup: Set[Fiber.Id] => URIO[R1, Any]): ZIO[R1, E, A] =
    ZIO.uninterruptibleMask { restore =>
      restore(self).foldCauseM(
        cause => if (cause.interrupted) cleanup(cause.interruptors) *> ZIO.halt(cause) else ZIO.halt(cause),
        a => ZIO.succeed(a)
      )
    }

  /**
   * Returns this effect if environment is on the left, otherwise returns
   * whatever is on the right unmodified. Note that the result is lifted
   * in either.
   */
  final def onLeft[C]: ZIO[Either[R, C], E, Either[A, C]] =
    self +++ ZIO.identity[C]

  /**
   * Returns this effect if environment is on the right, otherwise returns
   * whatever is on the left unmodified. Note that the result is lifted
   * in either.
   */
  final def onRight[C]: ZIO[Either[C, R], E, Either[C, A]] =
    ZIO.identity[C] +++ self

  /**
   * Propagates the success value to the second element of a tuple, but
   * passes the effect input `R` along unmodified as the first element
   * of the tuple.
   */
  final def onSecond[R1 <: R]: ZIO[R1, E, (R1, A)] =
    ZIO.identity[R1] &&& self

  /**
   * Runs the specified effect if this effect is terminated, either because of
   * a defect or because of interruption.
   */
  final def onTermination[R1 <: R](cleanup: Cause[Nothing] => URIO[R1, Any]): ZIO[R1, E, A] =
    ZIO.bracketExit(ZIO.unit)((_, eb: Exit[E, A]) =>
      eb match {
        case Exit.Failure(cause) => cause.failureOrCause.fold(_ => ZIO.unit, cleanup)
        case _                   => ZIO.unit
      }
    )(_ => self)

  /**
   * Executes this effect, skipping the error but returning optionally the success.
   */
  final def option(implicit ev: CanFail[E]): URIO[R, Option[A]] =
    self.foldM(_ => IO.succeedNow(None), a => IO.succeedNow(Some(a)))

  /**
   * Converts an option on errors into an option on values.
   */
  final def optional[E1](implicit ev: E <:< Option[E1]): ZIO[R, E1, Option[A]] =
    self.foldM(
      e => e.fold[ZIO[R, E1, Option[A]]](ZIO.succeedNow(Option.empty[A]))(ZIO.failNow(_)),
      a => ZIO.succeedNow(Some(a))
    )

  /**
   * Translates effect failure into death of the fiber, making all failures unchecked and
   * not a part of the type of the effect.
   */
  final def orDie[E1 >: E](implicit ev1: E1 <:< Throwable, ev2: CanFail[E]): URIO[R, A] =
    orDieWith(ev1)

  /**
   * Keeps none of the errors, and terminates the fiber with them, using
   * the specified function to convert the `E` into a `Throwable`.
   */
  final def orDieWith(f: E => Throwable)(implicit ev: CanFail[E]): URIO[R, A] =
    (self mapError f) catchAll (IO.dieNow)

  /**
   * Executes this effect and returns its value, if it succeeds, but
   * otherwise executes the specified effect.
   */
  final def orElse[R1 <: R, E2, A1 >: A](that: => ZIO[R1, E2, A1])(implicit ev: CanFail[E]): ZIO[R1, E2, A1] =
    tryOrElse(that, new ZIO.SucceedFn(() => that))

  /**
   * Returns an effect that will produce the value of this effect, unless it
   * fails, in which case, it will produce the value of the specified effect.
   */
  final def orElseEither[R1 <: R, E2, B](that: => ZIO[R1, E2, B])(implicit ev: CanFail[E]): ZIO[R1, E2, Either[A, B]] =
    tryOrElse(that.map(Right(_)), ZIO.succeedLeft)

  /**
   * Executes this effect and returns its value, if it succeeds, but
   * otherwise fails with the specified error.
   */
  final def orElseFail[E1](e1: => E1)(implicit ev: CanFail[E]): ZIO[R, E1, A] =
    orElse(ZIO.failNow(e1))

  /**
   * Executes this effect and returns its value, if it succeeds, but
   * otherwise suceeds with the specified value.
   */
  final def orElseSucceed[A1 >: A](a1: => A1)(implicit ev: CanFail[E]): URIO[R, A1] =
    orElse(ZIO.succeedNow(a1))

  /**
   * Exposes all parallel errors in a single call
   *
   */
  final def parallelErrors[E1 >: E]: ZIO[R, ::[E1], A] =
    self.foldCauseM(
      cause =>
        cause.failures match {
          case Nil            => ZIO.haltNow(cause.asInstanceOf[Cause[Nothing]])
          case ::(head, tail) => ZIO.failNow(::(head, tail))
        },
      ZIO.succeedNow
    )

  /**
   * Provides the `ZIO` effect with its required environment, which eliminates
   * its dependency on `R`.
   */
  final def provide(r: R)(implicit ev: NeedsEnv[R]): IO[E, A] = ZIO.provide(r)(self)

  /**
   * Provides the part of the environment that is not part of the `ZEnv`,
   * leaving an effect that only depends on the `ZEnv`.
   *
   * {{{
   * val loggingLayer: ZLayer[Any, Nothing, Logging] = ???
   *
   * val zio: ZIO[ZEnv with Logging, Nothing, Unit] = ???
   *
   * val zio2 = zio.provideCustomLayer(loggingLayer)
   * }}}
   */
  final def provideCustomLayer[E1 >: E, R1 <: Has[_]](
    layer: ZLayer[ZEnv, E1, R1]
  )(implicit ev: ZEnv with R1 <:< R, tagged: Tagged[R1]): ZIO[ZEnv, E1, A] =
    provideSomeLayer[ZEnv](layer)

  /**
   * Provides a layer to the ZIO effect, which translates it to another level.
   */
  final def provideLayer[E1 >: E, R0, R1 <: Has[_]](
    layer: ZLayer[R0, E1, R1]
  )(implicit ev1: R1 <:< R, ev2: NeedsEnv[R]): ZIO[R0, E1, A] =
    layer.build.map(ev1).use(self.provide)

  /**
   * An effectual version of `provide`, useful when the act of provision
   * requires an effect.
   */
  @deprecated("Use ZIO#provideLayer", "1.0.0")
  final def provideM[E1 >: E](r: ZIO[Any, E1, R])(implicit ev: NeedsEnv[R]): ZIO[Any, E1, A] =
    r.flatMap(self.provide)

  /**
   * Uses the given Managed[E1, R] to the environment required to run this effect,
   * leaving no outstanding environments and returning IO[E1, A]
   */
  @deprecated("Use ZIO#provideLayer", "1.0.0")
  final def provideManaged[E1 >: E](r0: Managed[E1, R])(implicit ev: NeedsEnv[R]): IO[E1, A] = provideSomeManaged(r0)

  /**
   * Provides some of the environment required to run this effect,
   * leaving the remainder `R0`.
   *
   * {{{
   * val effect: ZIO[Console with Logging, Nothing, Unit] = ???
   *
   * effect.provideSome[Console](env =>
   *   new Console with Logging {
   *     val console = env.console
   *     val logging = new Logging.Service[Any] {
   *       def log(line: String) = console.putStrLn(line)
   *     }
   *   }
   * )
   * }}}
   */
  final def provideSome[R0](f: R0 => R)(implicit ev: NeedsEnv[R]): ZIO[R0, E, A] =
    ZIO.accessM(r0 => self.provide(f(r0)))

  /**
   * Splits the environment into two parts, providing one part using the
   * specified layer and leaving the remainder `R0`.
   *
   * {{{
   * val clockLayer: ZLayer[Any, Nothing, Clock] = ???
   *
   * val zio: ZIO[Clock with Random, Nothing, Unit] = ???
   *
   * val zio2 = zio.provideSomeLayer[Random](clockLayer)
   * }}}
   */
  final def provideSomeLayer[R0 <: Has[_]]: ZIO.ProvideSomeLayer[R0, R, E, A] =
    new ZIO.ProvideSomeLayer[R0, R, E, A](self)

  /**
   * An effectful version of `provideSome`, useful when the act of partial
   * provision requires an effect.
   *
   * {{{
   * val effect: ZIO[Console with Logging, Nothing, Unit] = ???
   *
   * val r0: URIO[Console, Console with Logging] = ???
   *
   * effect.provideSomeM(r0)
   * }}}
   */
  @deprecated("Use ZIO#provideLayer", "1.0.0")
  final def provideSomeM[R0, E1 >: E](r0: ZIO[R0, E1, R])(implicit ev: NeedsEnv[R]): ZIO[R0, E1, A] =
    r0.flatMap(self.provide)

  /**
   * Uses the given ZManaged[R0, E1, R] to provide some of the environment required to run this effect,
   * leaving the remainder `R0`.
   */
  @deprecated("Use ZIO#provideLayer", "1.0.0")
  final def provideSomeManaged[R0, E1 >: E](r0: ZManaged[R0, E1, R])(implicit ev: NeedsEnv[R]): ZIO[R0, E1, A] =
    r0.use(self.provide)

  /**
   * Returns a successful effect if the value is `Left`, or fails with the error `None`.
   */
  final def left[B, C](implicit ev: A <:< Either[B, C]): ZIO[R, Option[E], B] =
    self.foldM(
      e => ZIO.failNow(Some(e)),
      a => ev(a).fold(ZIO.succeedNow, _ => ZIO.failNow(None))
    )

  /**
   * Returns a successful effect if the value is `Left`, or fails with the error e.
   */
  final def leftOrFail[B, C, E1 >: E](e: => E1)(implicit ev: A <:< Either[B, C]): ZIO[R, E1, B] =
    self.flatMap(ev(_) match {
      case Right(_)    => ZIO.failNow(e)
      case Left(value) => ZIO.succeedNow(value)
    })

  /**
   * Returns a successful effect if the value is `Left`, or fails with a [[java.util.NoSuchElementException]].
   */
  final def leftOrFailException[B, C, E1 >: NoSuchElementException](
    implicit ev: A <:< Either[B, C],
    ev2: E <:< E1
  ): ZIO[R, E1, B] =
    self.foldM(
      e => ZIO.failNow(ev2(e)),
      a => ev(a).fold(ZIO.succeedNow(_), _ => ZIO.failNow(new NoSuchElementException("Either.left.get on Right")))
    )

  /**
   * Returns a succesful effect if the value is `Right`, or fails with the error `None`.
   */
  final def right[B, C](implicit ev: A <:< Either[B, C]): ZIO[R, Option[E], C] =
    self.foldM(
      e => ZIO.failNow(Some(e)),
      a => ev(a).fold(_ => ZIO.failNow(None), ZIO.succeedNow)
    )

  /**
   * Returns a successful effect if the value is `Right`, or fails with the given error 'e'.
   */
  final def rightOrFail[B, C, E1 >: E](e: => E1)(implicit ev: A <:< Either[B, C]): ZIO[R, E1, C] =
    self.flatMap(ev(_) match {
      case Right(value) => ZIO.succeedNow(value)
      case Left(_)      => ZIO.failNow(e)
    })

  /**
   * Returns a successful effect if the value is `Right`, or fails with a [[java.util.NoSuchElementException]].
   */
  final def rightOrFailException[B, C, E1 >: NoSuchElementException](
    implicit ev: A <:< Either[B, C],
    ev2: E <:< E1
  ): ZIO[R, E1, C] =
    self.foldM(
      e => ZIO.failNow(ev2(e)),
      a => ev(a).fold(_ => ZIO.failNow(new NoSuchElementException("Either.right.get on Left")), ZIO.succeedNow(_))
    )

  /**
   * Returns an effect that races this effect with the specified effect,
   * returning the first successful `A` from the faster side. If one effect
   * succeeds, the other will be interrupted. If neither succeeds, then the
   * effect will fail with some error.
   *
   * WARNING: The raced effect will safely interrupt the "loser", but will not
   * resume until the loser has been cleanly terminated. If early return is
   * desired, then instead of performing `l race r`, perform
   * `l.disconnect race r.disconnect`, which disconnects left and right
   * interrupt signal, allowing the earliest possible return.
   */
  final def race[R1 <: R, E1 >: E, A1 >: A](that: ZIO[R1, E1, A1]): ZIO[R1, E1, A1] =
    ZIO.fiberId
      .flatMap(parentFiberId =>
        (self raceWith that)(
          (exit, right) =>
            exit.foldM[Any, E1, A1](
              cause => right.join mapErrorCause (cause && _), // TODO: Preserve error?
              a => (right interruptAs parentFiberId) as a
            ),
          (exit, left) =>
            exit.foldM[Any, E1, A1](
              cause => left.join mapErrorCause (_ && cause), // TODO: Preserve error?
              a => (left interruptAs parentFiberId) as a
            )
        )
      )
      .refailWithTrace

  /**
   * Returns an effect that races this effect with all the specified effects,
   * yielding the value of the first effect to succeed with a value.
   * Losers of the race will be interrupted immediately
   */
  final def raceAll[R1 <: R, E1 >: E, A1 >: A](ios: Iterable[ZIO[R1, E1, A1]]): ZIO[R1, E1, A1] = {
    def arbiter[E1, A1](
      fibers: List[Fiber[E1, A1]],
      winner: Fiber[E1, A1],
      promise: Promise[E1, (A1, Fiber[E1, A1])],
      fails: Ref[Int]
    )(res: Exit[E1, A1]): URIO[R1, Any] =
      res.foldM[R1, Nothing, Unit](
        e => ZIO.flatten(fails.modify((c: Int) => (if (c == 0) promise.halt(e).unit else ZIO.unit) -> (c - 1))),
        a =>
          promise
            .succeed(a -> winner)
            .flatMap(set =>
              if (set) fibers.foldLeft(IO.unit)((io, f) => if (f eq winner) io else io <* f.interrupt)
              else ZIO.unit
            )
      )

    (for {
      done  <- Promise.make[E1, (A1, Fiber[E1, A1])]
      fails <- Ref.make[Int](ios.size)
      c <- ZIO.uninterruptibleMask { restore =>
            for {
              head <- ZIO.interruptible(self).fork
              tail <- ZIO.foreach(ios)(io => ZIO.interruptible(io).fork)
              fs   = head :: tail
              _ <- fs.foldLeft[ZIO[R1, E1, Any]](ZIO.unit) {
                    case (io, f) =>
                      io *> f.await.flatMap(arbiter(fs, f, done, fails)).fork
                  }

              inheritRefs = { (res: (A1, Fiber[E1, A1])) =>
                res._2.inheritRefs.as(res._1)
              }

              c <- restore(done.await >>= inheritRefs)
                    .onInterrupt(fs.foldLeft(IO.unit)((io, f) => io <* f.interrupt))
            } yield c
          }
    } yield c).refailWithTrace
  }

  @deprecated("Use ZIO#raceFirst", "1.0.0")
  final def raceAttempt[R1 <: R, E1 >: E, A1 >: A](that: ZIO[R1, E1, A1]): ZIO[R1, E1, A1] =
    self raceFirst that

  /**
   * Returns an effect that races this effect with the specified effect,
   * yielding the first result to complete, whether by success or failure. If
   * neither effect completes, then the composed effect will not complete.
   *
   * WARNING: The raced effect will safely interrupt the "loser", but will not
   * resume until the loser has been cleanly terminated. If early return is
   * desired, then instead of performing `l raceFirst r`, perform
   * `l.disconnect raceFirst r.disconnect`, which disconnects left and right
   * interrupt signal, allowing the earliest possible return.
   */
  final def raceFirst[R1 <: R, E1 >: E, A1 >: A](that: ZIO[R1, E1, A1]): ZIO[R1, E1, A1] =
    (self.run race that.run).flatMap(ZIO.done(_)).refailWithTrace

  /**
   * Returns an effect that races this effect with the specified effect,
   * yielding the first result to succeed. If neither effect succeeds, then the
   * composed effect will fail with some error.
   *
   * WARNING: The raced effect will safely interrupt the "loser", but will not
   * resume until the loser has been cleanly terminated. If early return is
   * desired, then instead of performing `l raceEither r`, perform
   * `l.disconnect raceEither r.disconnect`, which disconnects left and right
   * interrupt signal, allowing the earliest possible return.
   */
  final def raceEither[R1 <: R, E1 >: E, B](that: ZIO[R1, E1, B]): ZIO[R1, E1, Either[A, B]] =
    (self.map(Left(_)) race that.map(Right(_)))

  /**
   * Returns an effect that races this effect with the specified effect, calling
   * the specified finisher as soon as one result or the other has been computed.
   */
  final def raceWith[R1 <: R, E1, E2, B, C](that: ZIO[R1, E1, B])(
    leftDone: (Exit[E, A], Fiber[E1, B]) => ZIO[R1, E2, C],
    rightDone: (Exit[E1, B], Fiber[E, A]) => ZIO[R1, E2, C],
    leftSuperviseMode: SuperviseMode = SuperviseMode.Interrupt,
    rightSuperviseMode: SuperviseMode = SuperviseMode.Interrupt
  ): ZIO[R1, E2, C] =
    new ZIO.RaceWith[R1, E, E1, E2, A, B, C](
      self,
      that,
      (exit, fiber) => leftDone(exit, fiber),
      (exit, fiber) => rightDone(exit, fiber),
      leftSuperviseMode,
      rightSuperviseMode
    )

  /**
   * Attach a wrapping trace pointing to this location in case of error.
   *
   * Useful when joining fibers to make the resulting trace mention
   * the `join` point, otherwise only the traces of joined fibers are
   * included.
   *
   * {{{
   *   for {
   *     badFiber <- UIO(1 / 0).fork
   *     _ <- badFiber.join.refailWithTrace
   *   } yield ()
   * }}}
   * */
  final def refailWithTrace: ZIO[R, E, A] =
    foldCauseM(c => ZIO.haltWith(trace => Cause.traced(c, trace())), ZIO.succeedNow)

  /**
   * Keeps some of the errors, and terminates the fiber with the rest
   */
  final def refineOrDie[E1](pf: PartialFunction[E, E1])(implicit ev1: E <:< Throwable, ev2: CanFail[E]): ZIO[R, E1, A] =
    refineOrDieWith(pf)(ev1)

  /**
   * Keeps some of the errors, and terminates the fiber with the rest, using
   * the specified function to convert the `E` into a `Throwable`.
   */
  final def refineOrDieWith[E1](pf: PartialFunction[E, E1])(f: E => Throwable)(implicit ev: CanFail[E]): ZIO[R, E1, A] =
    self catchAll (err => (pf lift err).fold[ZIO[R, E1, A]](ZIO.dieNow(f(err)))(ZIO.failNow(_)))

  /**
   * Fail with the returned value if the `PartialFunction` matches, otherwise
   * continue with our held value.
   */
  final def reject[R1 <: R, E1 >: E](pf: PartialFunction[A, E1]): ZIO[R1, E1, A] =
    rejectM(pf.andThen(ZIO.failNow(_)))

  /**
   * Continue with the returned computation if the `PartialFunction` matches,
   * translating the successful match into a failure, otherwise continue with
   * our held value.
   */
  final def rejectM[R1 <: R, E1 >: E](pf: PartialFunction[A, ZIO[R1, E1, E1]]): ZIO[R1, E1, A] =
    self.flatMap { v =>
      pf.andThen[ZIO[R1, E1, A]](_.flatMap(ZIO.failNow))
        .applyOrElse[A, ZIO[R1, E1, A]](v, ZIO.succeedNow)
    }

  /**
   * Returns a new effect that repeats this effect according to the specified
   * schedule or until the first failure. Scheduled recurrences are in addition
   * to the first execution, so that `io.repeat(Schedule.once)` yields an
   * effect that executes `io`, and then if that succeeds, executes `io` an
   * additional time.
   */
  final def repeat[R1 <: R, B](schedule: Schedule[R1, A, B]): ZIO[R1, E, B] =
    repeatOrElse[R1, E, B](schedule, (e, _) => ZIO.failNow(e))

  /**
   * Returns a new effect that repeats this effect according to the specified
   * schedule or until the first failure, at which point, the failure value
   * and schedule output are passed to the specified handler.
   *
   * Scheduled recurrences are in addition to the first execution, so that
   * `io.repeat(Schedule.once)` yields an effect that executes `io`, and then
   * if that succeeds, executes `io` an additional time.
   */
  final def repeatOrElse[R1 <: R, E2, B](
    schedule: Schedule[R1, A, B],
    orElse: (E, Option[B]) => ZIO[R1, E2, B]
  ): ZIO[R1, E2, B] =
    repeatOrElseEither[R1, B, E2, B](schedule, orElse).map(_.merge)

  /**
   * Returns a new effect that repeats this effect according to the specified
   * schedule or until the first failure, at which point, the failure value
   * and schedule output are passed to the specified handler.
   *
   * Scheduled recurrences are in addition to the first execution, so that
   * `io.repeat(Schedule.once)` yields an effect that executes `io`, and then
   * if that succeeds, executes `io` an additional time.
   */
  final def repeatOrElseEither[R1 <: R, B, E2, C](
    schedule: Schedule[R1, A, B],
    orElse: (E, Option[B]) => ZIO[R1, E2, C]
  ): ZIO[R1, E2, Either[C, B]] = {
    def loop(last: A, state: schedule.State): ZIO[R1, E2, Either[C, B]] =
      schedule
        .update(last, state)
        .foldM(
          _ => ZIO.succeedRight(schedule.extract(last, state)),
          s =>
            self.foldM(
              e => orElse(e, Some(schedule.extract(last, state))).map(Left(_)),
              a => loop(a, s)
            )
        )
    self.foldM(
      orElse(_, None).map(Left(_)),
      a => schedule.initial.flatMap(loop(a, _))
    )
  }

  /**
   * Retries with the specified retry policy.
   * Retries are done following the failure of the original `io` (up to a fixed maximum with
   * `once` or `recurs` for example), so that that `io.retry(Schedule.once)` means
   * "execute `io` and in case of failure, try again once".
   */
  final def retry[R1 <: R, E1 >: E, S](policy: Schedule[R1, E1, S])(implicit ev: CanFail[E]): ZIO[R1, E, A] =
    retryOrElse(policy, (e: E, _: S) => ZIO.failNow(e))

  /**
   * Retries with the specified schedule, until it fails, and then both the
   * value produced by the schedule together with the last error are passed to
   * the recovery function.
   */
  final def retryOrElse[R1 <: R, A2 >: A, E1 >: E, S, E2](
    policy: Schedule[R1, E1, S],
    orElse: (E, S) => ZIO[R1, E2, A2]
  )(implicit ev: CanFail[E]): ZIO[R1, E2, A2] =
    retryOrElseEither(policy, orElse).map(_.merge)

  /**
   * Retries with the specified schedule, until it fails, and then both the
   * value produced by the schedule together with the last error are passed to
   * the recovery function.
   */
  final def retryOrElseEither[R1 <: R, E1 >: E, S, E2, B](
    policy: Schedule[R1, E1, S],
    orElse: (E, S) => ZIO[R1, E2, B]
  )(implicit ev: CanFail[E]): ZIO[R1, E2, Either[B, A]] = {
    def loop(state: policy.State): ZIO[R1, E2, Either[B, A]] =
      self.foldM(
        err =>
          policy
            .update(err, state)
            .foldM(
              _ => orElse(err, policy.extract(err, state)).map(Left(_)),
              loop
            ),
        ZIO.succeedRight
      )

    policy.initial.flatMap(loop)
  }

  /**
   * Retries this effect until its error satisfies the specified predicate.
   */
  final def retryUntil(f: E => Boolean)(implicit ev: CanFail[E]): ZIO[R, E, A] =
    retry(Schedule.doUntil(f))

  /**
   * Retries this effect until its error equals the predicate.
   */
  final def retryUntilEquals[E1 >: E](e: => E1)(implicit ev: CanFail[E1]): ZIO[R, E1, A] =
    retry(Schedule.doUntilEquals(e))

  /**
   * Retries this effect until its error satisfies the specified effectful predicate.
   */
  final def retryUntilM(f: E => UIO[Boolean])(implicit ev: CanFail[E]): ZIO[R, E, A] =
    retry(Schedule.doUntilM(f))

  /**
   * Retries this effect while its error satisfies the specified predicate.
   */
  final def retryWhile(f: E => Boolean)(implicit ev: CanFail[E]): ZIO[R, E, A] =
    retry(Schedule.doWhile(f))

  /**
   * Repeats this effect for as long as the error equals the predicate.
   */
  final def retryWhileEquals[E1 >: E](e: => E1)(implicit ev: CanFail[E1]): ZIO[R, E1, A] =
    retry(Schedule.doWhileEquals(e))

  /**
   * Retries this effect while its error satisfies the specified effectful predicate.
   */
  final def retryWhileM(f: E => UIO[Boolean])(implicit ev: CanFail[E]): ZIO[R, E, A] =
    retry(Schedule.doWhileM(f))

  /**
   * Returns an effect that semantically runs the effect on a fiber,
   * producing an [[zio.Exit]] for the completion value of the fiber.
   */
  final def run: URIO[R, Exit[E, A]] =
    new ZIO.Fold[R, E, Nothing, A, Exit[E, A]](
      self,
      cause => ZIO.succeedNow(Exit.halt(cause)),
      succ => ZIO.succeedNow(Exit.succeed(succ))
    )

  /**
   * Exposes the full cause of failure of this effect.
   *
   * {{{
   * case class DomainError()
   *
   * val veryBadIO: IO[DomainError, Unit] =
   *   IO.effectTotal(5 / 0) *> IO.fail(DomainError())
   *
   * val caught: UIO[Unit] =
   *   veryBadIO.sandbox.catchAll {
   *     case Cause.Die(_: ArithmeticException) =>
   *       // Caught defect: divided by zero!
   *       IO.succeed(0)
   *     case Cause.Fail(e) =>
   *       // Caught error: DomainError!
   *       IO.succeed(0)
   *     case cause =>
   *       // Caught unknown defects, shouldn't recover!
   *       IO.halt(cause)
   *    *
   *   }
   * }}}
   */
  final def sandbox: ZIO[R, Cause[E], A] = foldCauseM(ZIO.failNow, ZIO.succeedNow)

  /**
   * Converts an option on values into an option on errors.
   */
  final def some[B](implicit ev: A <:< Option[B]): ZIO[R, Option[E], B] =
    self.foldM(
      e => ZIO.failNow(Some(e)),
      a => a.fold[ZIO[R, Option[E], B]](ZIO.failNow(Option.empty[E]))(ZIO.succeedNow)
    )

  /**
   * Extracts the optional value, or fails with the given error 'e'.
   */
  final def someOrFail[B, E1 >: E](e: => E1)(implicit ev: A <:< Option[B]): ZIO[R, E1, B] =
    self.flatMap(ev(_) match {
      case Some(value) => ZIO.succeedNow(value)
      case None        => ZIO.failNow(e)
    })

  /**
   * Extracts the optional value, or fails with a [[java.util.NoSuchElementException]]
   */
  final def someOrFailException[B, E1 >: E](
    implicit ev: A <:< Option[B],
    ev2: NoSuchElementException <:< E1
  ): ZIO[R, E1, B] =
    self.foldM(e => ZIO.failNow(e), ev(_) match {
      case Some(value) => ZIO.succeedNow(value)
      case None        => ZIO.failNow(ev2(new NoSuchElementException("None.get")))
    })

  /**
   * Companion helper to `sandbox`. Allows recovery, and partial recovery, from
   * errors and defects alike, as in:
   *
   * {{{
   * case class DomainError()
   *
   * val veryBadIO: IO[DomainError, Unit] =
   *   IO.effectTotal(5 / 0) *> IO.fail(DomainError())
   *
   * val caught: IO[DomainError, Unit] =
   *   veryBadIO.sandboxWith(_.catchSome {
   *     case Cause.Die(_: ArithmeticException)=>
   *       // Caught defect: divided by zero!
   *       IO.succeed(0)
   *   })
   * }}}
   *
   * Using `sandboxWith` with `catchSome` is better than using
   * `io.sandbox.catchAll` with a partial match, because in
   * the latter, if the match fails, the original defects will
   * be lost and replaced by a `MatchError`
   */
  final def sandboxWith[R1 <: R, E2, B](f: ZIO[R1, Cause[E], A] => ZIO[R1, Cause[E2], B]): ZIO[R1, E2, B] =
    ZIO.unsandbox(f(self.sandbox))

  /**
   * Summarizes a effect by computing some value before and after execution, and
   * then combining the values to produce a summary, together with the result of
   * execution.
   */
  final def summarized[R1 <: R, E1 >: E, B, C](summary: ZIO[R1, E1, B])(f: (B, B) => C): ZIO[R1, E1, (C, A)] =
    for {
      start <- summary
      value <- self
      end   <- summary
    } yield (f(start, end), value)

  /**
   * An integer that identifies the term in the `ZIO` sum type to which this
   * instance belongs (e.g. `IO.Tags.Succeed`).
   */
  def tag: Int

  /**
   * Returns an effect that effectfully "peeks" at the success of this effect.
   *
   * {{{
   * readFile("data.json").tap(putStrLn)
   * }}}
   */
  final def tap[R1 <: R, E1 >: E](f: A => ZIO[R1, E1, Any]): ZIO[R1, E1, A] = self.flatMap(new ZIO.TapFn(f))

  /**
   * Returns an effect that effectfully "peeks" at the failure or success of
   * this effect.
   * {{{
   * readFile("data.json").tapBoth(logError(_), logData(_))
   * }}}
   */
  final def tapBoth[R1 <: R, E1 >: E](f: E => ZIO[R1, E1, Any], g: A => ZIO[R1, E1, Any])(
    implicit ev: CanFail[E]
  ): ZIO[R1, E1, A] =
    self.foldCauseM(new ZIO.TapErrorRefailFn(f), new ZIO.TapFn(g))

  /**
   * Returns an effect that effectually "peeks" at the cause of the failure of
   * this effect.
   * {{{
   * readFile("data.json").tapCause(logCause(_))
   * }}}
   */
  final def tapCause[R1 <: R, E1 >: E](f: Cause[E] => ZIO[R1, E1, Any]): ZIO[R1, E1, A] =
    self.foldCauseM(new ZIO.TapCauseRefailFn(f), ZIO.succeedNow)

  /**
   * Returns an effect that effectfully "peeks" at the failure of this effect.
   * {{{
   * readFile("data.json").tapError(logError(_))
   * }}}
   */
  final def tapError[R1 <: R, E1 >: E](f: E => ZIO[R1, E1, Any])(implicit ev: CanFail[E]): ZIO[R1, E1, A] =
    self.foldCauseM(new ZIO.TapErrorRefailFn(f), ZIO.succeedNow)

  /**
   * Returns a new effect that executes this one and times the execution.
   */
  final def timed: ZIO[R with Clock, E, (Duration, A)] = timedWith(clock.nanoTime)

  /**
   * A more powerful variation of `timed` that allows specifying the clock.
   */
  final def timedWith[R1 <: R, E1 >: E](nanoTime: ZIO[R1, E1, Long]): ZIO[R1, E1, (Duration, A)] =
    summarized(nanoTime)((start, end) => Duration.fromNanos(end - start))

  /**
   * Returns an effect that will timeout this effect, returning `None` if the
   * timeout elapses before the effect has produced a value; and returning
   * `Some` of the produced value otherwise.
   *
   * If the timeout elapses without producing a value, the running effect
   * will be safely interrupted.
   *
   * WARNING: The effect returned by this method will not itself return until
   * the underlying effect is actually interrupted. This leads to more
   * predictable resource utilization. If early return is desired, then
   * instead of using `effect.timeout(d)`, use `effect.disconnect.timeout(d)`,
   * which first disconnects the effect's interruption signal before performing
   * the timeout, resulting in earliest possible return, before an underlying
   * effect has been successfully interrupted.
   */
  final def timeout(d: Duration): ZIO[R with Clock, E, Option[A]] = timeoutTo(None)(Some(_))(d)

  /**
   * The same as [[timeout]], but instead of producing a `None` in the event
   * of timeout, it will produce the specified error.
   */
  final def timeoutFail[E1 >: E](e: E1)(d: Duration): ZIO[R with Clock, E1, A] =
    ZIO.flatten(timeoutTo(ZIO.failNow(e))(ZIO.succeedNow)(d))

  /**
   * Returns an effect that will attempt to timeout this effect, but will not
   * wait for the running effect to terminate if the timeout elapses without
   * producing a value. Returns `Right` with the produced value if the effect
   * completes before the timeout or `Left` with the interrupting fiber
   * otherwise.
   */
  @deprecated("Use ZIO#disconnect.timeout(d)", "1.0.0")
  final def timeoutFork(d: Duration): ZIO[R with Clock, E, Option[A]] =
    self.disconnect.timeout(d)

  /**
   * Returns an effect that will timeout this effect, returning either the
   * default value if the timeout elapses before the effect has produced a
   * value; and or returning the result of applying the function `f` to the
   * success value of the effect.
   *
   * If the timeout elapses without producing a value, the running effect
   * will be safely interrupted
   *
   * {{{
   * IO.succeed(1).timeoutTo(None)(Some(_))(1.second)
   * }}}
   */
  final def timeoutTo[R1 <: R, E1 >: E, A1 >: A, B](b: B): ZIO.TimeoutTo[R1, E1, A1, B] =
    new ZIO.TimeoutTo(self, b)

  /**
   * Returns an effect that keeps or breaks a promise based on the result of
   * this effect. Synchronizes interruption, so if this effect is interrupted,
   * the specified promise will be interrupted, too.
   */
  final def to[E1 >: E, A1 >: A](p: Promise[E1, A1]): URIO[R, Boolean] =
    ZIO.uninterruptibleMask(restore => restore(self).run.flatMap(p.done(_)))

  /**
   * Converts the effect into a [[scala.concurrent.Future]].
   */
  final def toFuture(implicit ev2: E <:< Throwable): URIO[R, CancelableFuture[A]] =
    self toFutureWith ev2

  /**
   * Converts the effect into a [[scala.concurrent.Future]].
   */
  final def toFutureWith(f: E => Throwable): URIO[R, CancelableFuture[A]] =
    self.fork >>= (_.toFutureWith(f))

  /**
   * Constructs a layer from this effect.
   */
  final def toLayer[A1 >: A](implicit ev: Tagged[A1]): ZLayer[R, E, Has[A1]] =
    ZLayer.fromEffect(self)

  /**
   * Constructs a layer from this effect, which must return one or more
   * services.
   */
  final def toLayerMany[A1 <: Has[_]](implicit ev: A <:< A1): ZLayer[R, E, A1] =
    ZLayer(ZManaged.fromEffect(self.map(ev)))

  /**
   * Converts this ZIO to [[zio.Managed]]. This ZIO and the provided release action
   * will be performed uninterruptibly.
   */
  final def toManaged[R1 <: R](release: A => URIO[R1, Any]): ZManaged[R1, E, A] =
    ZManaged.make(this)(release)

  /**
   * Converts this ZIO to [[zio.ZManaged]] with no release action. It will be performed
   * interruptibly.
   */
  final def toManaged_ : ZManaged[R, E, A] =
    ZManaged.fromEffect[R, E, A](this)

  /**
   * Enables ZIO tracing for this effect. Because this is the default, this
   * operation only has an additional meaning if the effect is located within
   * an `untraced` section, or the current fiber has been spawned by a parent
   * inside an `untraced` section.
   */
  final def traced: ZIO[R, E, A] = tracingStatus(TracingStatus.Traced)

  /**
   * Toggles ZIO tracing support for this effect. If `true` is used, then the
   * effect will accumulate traces, while if `false` is used, then tracing
   * is disabled. These changes are compositional, so they only affect regions
   * of the effect.
   */
  final def tracingStatus(flag: TracingStatus): ZIO[R, E, A] = new ZIO.TracingStatus(self, flag)

  private[this] final def tryOrElse[R1 <: R, E2, B](
    that: => ZIO[R1, E2, B],
    succ: A => ZIO[R1, E2, B]
  ): ZIO[R1, E2, B] =
    new ZIO.Fold[R1, E, E2, A, B](
      self,
      ZIOFn(() => that) { cause =>
        cause.stripFailures match {
          case None    => that.catchAllCause(cause2 => ZIO.haltNow(Cause.die(FiberFailure(cause)) ++ cause2))
          case Some(c) => ZIO.haltNow(c)
        }
      },
      succ
    )

  /**
   * Performs this effect uninterruptibly. This will prevent the effect from
   * being terminated externally, but the effect may fail for internal reasons
   * (e.g. an uncaught error) or terminate due to defect.
   *
   * Uninterruptible effects may recover from all failure causes (including
   * interruption of an inner effect that has been made interruptible).
   */
  final def uninterruptible: ZIO[R, E, A] = interruptStatus(InterruptStatus.Uninterruptible)

  /**
   * Returns the effect resulting from mapping the success of this effect to unit.
   */
  final def unit: ZIO[R, E, Unit] = as(())

  /**
   * The inverse operation to `sandbox`. Submerges the full cause of failure.
   */
  final def unsandbox[R1 <: R, E1, A1 >: A](implicit ev1: ZIO[R, E, A] <:< ZIO[R1, Cause[E1], A1]): ZIO[R1, E1, A1] =
    ZIO.unsandbox(ev1(self))

  /**
   * Disables ZIO tracing facilities for the duration of the effect.
   *
   * Note: ZIO tracing is cached, as such after the first iteration
   * it has a negligible effect on performance of hot-spots (Additional
   * hash map lookup per flatMap). As such, using `untraced` sections
   * is not guaranteed to result in a noticeable performance increase.
   */
  final def untraced: ZIO[R, E, A] = tracingStatus(TracingStatus.Untraced)

  /**
   * The moral equivalent of `if (p) exp`
   */
  final def when(b: => Boolean): ZIO[R, E, Unit] =
    ZIO.when(b)(self)

  /**
   * The moral equivalent of `if (p) exp` when `p` has side-effects
   */
  final def whenM[R1 <: R, E1 >: E](
    b: URIO[R1, Boolean]
  ): ZIO[R1, E1, Unit] =
    ZIO.whenM(b)(self)

  /**
   * A named alias for `&&&` or `<*>`.
   */
  final def zip[R1 <: R, E1 >: E, B](that: ZIO[R1, E1, B]): ZIO[R1, E1, (A, B)] =
    self &&& that

  /**
   * A named alias for `<*`.
   */
  final def zipLeft[R1 <: R, E1 >: E, B](that: => ZIO[R1, E1, B]): ZIO[R1, E1, A] =
    self <* that

  /**
   * A named alias for `<&>`.
   */
  final def zipPar[R1 <: R, E1 >: E, B](that: ZIO[R1, E1, B]): ZIO[R1, E1, (A, B)] =
    self <&> that

  /**
   * A named alias for `<&`.
   */
  final def zipParLeft[R1 <: R, E1 >: E, B](that: ZIO[R1, E1, B]): ZIO[R1, E1, A] =
    self <& that

  /**
   * A named alias for `&>`.
   */
  final def zipParRight[R1 <: R, E1 >: E, B](that: ZIO[R1, E1, B]): ZIO[R1, E1, B] =
    self &> that

  /**
   * A named alias for `*>`.
   */
  final def zipRight[R1 <: R, E1 >: E, B](that: => ZIO[R1, E1, B]): ZIO[R1, E1, B] =
    self *> that

  /**
   * Sequentially zips this effect with the specified effect using the
   * specified combiner function.
   */
  final def zipWith[R1 <: R, E1 >: E, B, C](that: => ZIO[R1, E1, B])(f: (A, B) => C): ZIO[R1, E1, C] =
    self.flatMap(a => that.map(ZIOFn(f)(b => f(a, b))))

  /**
   * Returns an effect that executes both this effect and the specified effect,
   * in parallel, combining their results with the specified `f` function. If
   * either side fails, then the other side will be interrupted.
   */
  final def zipWithPar[R1 <: R, E1 >: E, B, C](
    that: ZIO[R1, E1, B]
  )(f: (A, B) => C): ZIO[R1, E1, C] = {
    def coordinate[A, B](
      fiberId: Fiber.Id,
      f: (A, B) => C,
      leftWinner: Boolean
    )(winner: Exit[E1, A], loser: Fiber[E1, B]): ZIO[R1, E1, C] =
      winner match {
        case Exit.Success(a) => loser.join.map(f(a, _))
        case Exit.Failure(cause) =>
          loser.interruptAs(fiberId).flatMap {
            case Exit.Success(_) => ZIO.haltNow(cause)
            case Exit.Failure(loserCause) =>
              if (leftWinner) ZIO.haltNow(cause && loserCause)
              else ZIO.haltNow(loserCause && cause)
          }
      }

    val g = (b: B, a: A) => f(a, b)
    ZIO.fiberId.flatMap(parentFiberId =>
      (self raceWith that)(coordinate(parentFiberId, f, true), coordinate(parentFiberId, g, false)).fork.flatMap { f =>
        f.await.flatMap { exit =>
          if (exit.succeeded) f.inheritRefs *> ZIO.doneNow(exit)
          else ZIO.doneNow(exit)
        }
      }
    )
  }
}

object ZIO extends ZIOCompanionPlatformSpecific {

  /**
   * Submerges the error case of an `Either` into the `ZIO`. The inverse
   * operation of `IO.either`.
   */
  def absolve[R, E, A](v: ZIO[R, E, Either[E, A]]): ZIO[R, E, A] =
    v.flatMap(fromEither(_))

  /**
   * Accesses the environment of the effect.
   * {{{
   * val portNumber = effect.access(_.config.portNumber)
   * }}}
   */
  def access[R]: ZIO.AccessPartiallyApplied[R] =
    new ZIO.AccessPartiallyApplied[R]

  /**
   * Effectfully accesses the environment of the effect.
   */
  def accessM[R]: ZIO.AccessMPartiallyApplied[R] =
    new ZIO.AccessMPartiallyApplied[R]

  /**
   * Makes an explicit check to see if the fiber has been interrupted, and if
   * so, performs self-interruption
   */
  def allowInterrupt: UIO[Unit] =
    descriptorWith(d => if (d.interruptors.nonEmpty) interrupt else ZIO.unit)

  /**
   * Awaits all child fibers of the fiber executing the effect.
   */
  def awaitAllChildren: UIO[Unit] = ZIO.children.flatMap(Fiber.awaitAll(_))

  /**
   * When this effect represents acquisition of a resource (for example,
   * opening a file, launching a thread, etc.), `bracket` can be used to ensure
   * the acquisition is not interrupted and the resource is always released.
   *
   * The function does two things:
   *
   * 1. Ensures this effect, which acquires the resource, will not be
   * interrupted. Of course, acquisition may fail for internal reasons (an
   * uncaught exception).
   * 2. Ensures the `release` effect will not be interrupted, and will be
   * executed so long as this effect successfully acquires the resource.
   *
   * In between acquisition and release of the resource, the `use` effect is
   * executed.
   *
   * If the `release` effect fails, then the entire effect will fail even
   * if the `use` effect succeeds. If this fail-fast behavior is not desired,
   * errors produced by the `release` effect can be caught and ignored.
   *
   * {{{
   * openFile("data.json").bracket(closeFile) { file =>
   *   for {
   *     header <- readHeader(file)
   *     ...
   *   } yield result
   * }
   * }}}
   */
  def bracket[R, E, A](acquire: ZIO[R, E, A]): ZIO.BracketAcquire[R, E, A] =
    new ZIO.BracketAcquire[R, E, A](acquire)

  /**
   * Uncurried version. Doesn't offer curried syntax and have worse type-inference
   * characteristics, but guarantees no extra allocations of intermediate
   * [[zio.ZIO.BracketAcquire]] and [[zio.ZIO.BracketRelease]] objects.
   */
  def bracket[R, E, A, B](
    acquire: ZIO[R, E, A],
    release: A => URIO[R, Any],
    use: A => ZIO[R, E, B]
  ): ZIO[R, E, B] =
    bracketExit(acquire, new ZIO.BracketReleaseFn(release): (A, Exit[E, B]) => URIO[R, Any], use)

  /**
   * Acquires a resource, uses the resource, and then releases the resource.
   * Neither the acquisition nor the release will be interrupted, and the
   * resource is guaranteed to be released, so long as the `acquire` effect
   * succeeds. If `use` fails, then after release, the returned effect will fail
   * with the same error.
   */
  def bracketExit[R, E, A](acquire: ZIO[R, E, A]): ZIO.BracketExitAcquire[R, E, A] =
    new ZIO.BracketExitAcquire(acquire)

  /**
   * Uncurried version. Doesn't offer curried syntax and has worse type-inference
   * characteristics, but guarantees no extra allocations of intermediate
   * [[zio.ZIO.BracketExitAcquire]] and [[zio.ZIO.BracketExitRelease]] objects.
   */
  def bracketExit[R, E, A, B](
    acquire: ZIO[R, E, A],
    release: (A, Exit[E, B]) => URIO[R, Any],
    use: A => ZIO[R, E, B]
  ): ZIO[R, E, B] =
    ZIO.uninterruptibleMask[R, E, B](restore =>
      acquire.flatMap(ZIOFn(traceAs = use) { a =>
        restore(use(a)).run.flatMap(ZIOFn(traceAs = release) { e =>
          release(a, e).foldCauseM(
            cause2 => ZIO.haltNow(e.fold(_ ++ cause2, _ => cause2)),
            _ => ZIO.doneNow(e)
          )
        })
      })
    )

  /**
   * A variant of `bracket` which returns immediately on interruption.
   * however, it does not actually interrupt the underlying acquisition, but rather, in a separate fiber,
   * awaits the acquisition and then gracefully and immediately releases the resource after acquisition.
   * Thus the fiber executing bracketFork is able to be interrupted right away even if in the middle
   * of a lengthy acquisition operation.
   */
  @deprecated("Use ZIO#disconnect on an ordinary bracket", "1.0.0")
  def bracketFork[R, E, A](acquire: ZIO[R, E, A]): ZIO.BracketForkAcquire[R, E, A] =
    new ZIO.BracketForkAcquire[R, E, A](acquire)

  /**
   * Uncurried version of `bracketFork`. Doesn't offer curried syntax and has worse type-inference
   * characteristics, but guarantees no extra allocations of intermediate
   * [[zio.ZIO.BracketForkAcquire]] and [[zio.ZIO.BracketForkRelease]] objects.
   */
  @deprecated("Use ZIO#disconnect on an ordinary bracket", "1.0.0")
  def bracketFork[R, E, A, B](
    acquire: ZIO[R, E, A],
    release: A => URIO[R, Any],
    use: A => ZIO[R, E, B]
  ): ZIO[R, E, B] =
    bracketForkExit(acquire, new ZIO.BracketReleaseFn(release): (A, Exit[E, B]) => URIO[R, Any], use)

  /**
   * A variant of `bracketExit` which returns immediately on interruption.
   * However, it does not actually interrupt the underlying acquisition, but rather, in a separate fiber,
   * awaits the acquisition and then gracefully and immediately releases the resource after acquisition.
   */
  @deprecated("Use ZIO#disconnect on an ordinary bracket", "1.0.0")
  def bracketForkExit[R, E, A](acquire: ZIO[R, E, A]): ZIO.BracketForkExitAcquire[R, E, A] =
    new ZIO.BracketForkExitAcquire(acquire)

  /**
   * Uncurried version of `bracketForkExit`. Doesn't offer curried syntax and has worse type-inference
   * characteristics, but guarantees no extra allocations of intermediate
   * [[zio.ZIO.BracketForkExitAcquire]] and [[zio.ZIO.BracketForkExitRelease]] objects.
   */
  @deprecated("Use ZIO#disconnect on an ordinary bracket", "1.0.0")
  def bracketForkExit[R, E, A, B](
    acquire: ZIO[R, E, A],
    release: (A, Exit[E, B]) => URIO[R, Any],
    use: A => ZIO[R, E, B]
  ): ZIO[R, E, B] =
    ZIO.bracketExit(acquire, release, use).interruptibleFork

  /**
   * Checks the interrupt status, and produces the effect returned by the
   * specified callback.
   */
  def checkInterruptible[R, E, A](f: zio.InterruptStatus => ZIO[R, E, A]): ZIO[R, E, A] =
    new ZIO.CheckInterrupt(f)

  /**
   * Checks the ZIO Tracing status, and produces the effect returned by the
   * specified callback.
   */
  def checkTraced[R, E, A](f: TracingS => ZIO[R, E, A]): ZIO[R, E, A] =
    new ZIO.CheckTracing(f)

  /**
   * Provides access to the list of child fibers supervised by this fiber.
   */
  def children: UIO[Iterable[Fiber[Any, Any]]] = descriptor.flatMap(_.children)

  /**
   * Evaluate each effect in the structure from left to right, and collect
   * the results. For a parallel version, see `collectAllPar`.
   */
  def collectAll[R, E, A](in: Iterable[ZIO[R, E, A]]): ZIO[R, E, List[A]] =
    foreach[R, E, ZIO[R, E, A], A](in)(ZIO.identityFn)

  /**
   * Evaluate each effect in the structure in parallel, and collect
   * the results. For a sequential version, see `collectAll`.
   */
  def collectAllPar[R, E, A](as: Iterable[ZIO[R, E, A]]): ZIO[R, E, List[A]] =
    foreachPar[R, E, ZIO[R, E, A], A](as)(ZIO.identityFn)

  /**
   * Evaluate each effect in the structure in parallel, and collect
   * the results. For a sequential version, see `collectAll`.
   *
   * Unlike `collectAllPar`, this method will use at most `n` fibers.
   */
  def collectAllParN[R, E, A](n: Int)(as: Iterable[ZIO[R, E, A]]): ZIO[R, E, List[A]] =
    foreachParN[R, E, ZIO[R, E, A], A](n)(as)(ZIO.identityFn)

  /**
   * Evaluate and run each effect in the structure and collect discarding failed ones.
   */
  def collectAllSuccesses[R, E, A](in: Iterable[ZIO[R, E, A]]): URIO[R, List[A]] =
    collectAllWith[R, Nothing, Exit[E, A], A](in.map(_.run)) { case zio.Exit.Success(a) => a }

  /**
   * Evaluate and run each effect in the structure in parallel, and collect discarding failed ones.
   */
  def collectAllSuccessesPar[R, E, A](in: Iterable[ZIO[R, E, A]]): URIO[R, List[A]] =
    collectAllWithPar[R, Nothing, Exit[E, A], A](in.map(_.run)) { case zio.Exit.Success(a) => a }

  /**
   * Evaluate and run each effect in the structure in parallel, and collect discarding failed ones.
   *
   * Unlike `collectAllSuccessesPar`, this method will use at most up to `n` fibers.
   */
  def collectAllSuccessesParN[R, E, A](
    n: Int
  )(in: Iterable[ZIO[R, E, A]]): URIO[R, List[A]] =
    collectAllWithParN[R, Nothing, Exit[E, A], A](n)(in.map(_.run)) { case zio.Exit.Success(a) => a }

  /**
   * Evaluate each effect in the structure with `collectAll`, and collect
   * the results with given partial function.
   */
  def collectAllWith[R, E, A, U](
    in: Iterable[ZIO[R, E, A]]
  )(f: PartialFunction[A, U]): ZIO[R, E, List[U]] =
    ZIO.collectAll(in).map(_.collect(f))

  /**
   * Evaluate each effect in the structure with `collectAllPar`, and collect
   * the results with given partial function.
   */
  def collectAllWithPar[R, E, A, U](
    in: Iterable[ZIO[R, E, A]]
  )(f: PartialFunction[A, U]): ZIO[R, E, List[U]] =
    ZIO.collectAllPar(in).map(_.collect(f))

  /**
   * Evaluate each effect in the structure with `collectAllPar`, and collect
   * the results with given partial function.
   *
   * Unlike `collectAllWithPar`, this method will use at most up to `n` fibers.
   */
  def collectAllWithParN[R, E, A, U](n: Int)(
    in: Iterable[ZIO[R, E, A]]
  )(f: PartialFunction[A, U]): ZIO[R, E, List[U]] =
    ZIO.collectAllParN(n)(in).map(_.collect(f))

  /**
   * Returns information about the current fiber, such as its identity.
   */
  def descriptor: UIO[Fiber.Descriptor] = descriptorWith(succeedNow)

  /**
   * Constructs an effect based on information about the current fiber, such as
   * its identity.
   */
  def descriptorWith[R, E, A](f: Fiber.Descriptor => ZIO[R, E, A]): ZIO[R, E, A] =
    new ZIO.Descriptor(f)

  /**
   * Returns an effect that dies with the specified `Throwable`.
   * This method can be used for terminating a fiber because a defect has been
   * detected in the code.
   */
  def die(t: => Throwable): UIO[Nothing] =
    haltWith(trace => Cause.Traced(Cause.Die(t), trace()))

  /**
   * Returns an effect that dies with a [[java.lang.RuntimeException]] having the
   * specified text message. This method can be used for terminating a fiber
   * because a defect has been detected in the code.
   */
  def dieMessage(message: => String): UIO[Nothing] =
    die(new RuntimeException(message))

  /**
   * Disowns the specified fiber, which means that when this fiber exits, the
   * specified fiber will not be interrupted. Disowned fibers become new root
   * fibers, and are not terminated automatically when any other fibers ends.
   */
  def disown(fiber: Fiber[Any, Any]): UIO[Boolean] = new ZIO.Disown(fiber)

  /**
   * Disowns all children.
   */
  val disownChildren: UIO[Boolean] =
    ZIO.children.flatMap(children => ZIO.foreach(children)(disown(_)).map(_.exists(a => a)))

  /**
   * Returns an effect from a [[zio.Exit]] value.
   */
  def done[E, A](r: => Exit[E, A]): IO[E, A] =
    ZIO.effectSuspendTotal {
      r match {
        case Exit.Success(b)     => succeedNow(b)
        case Exit.Failure(cause) => haltNow(cause)
      }
    }

  /**
   *
   * Imports a synchronous side-effect into a pure `ZIO` value, translating any
   * thrown exceptions into typed failed effects creating with `ZIO.fail`.
   *
   * {{{
   * def putStrLn(line: String): Task[Unit] = Task.effect(println(line))
   * }}}
   */
  def effect[A](effect: => A): Task[A] = new ZIO.EffectPartial(() => effect)

  /**
   * Imports an asynchronous side-effect into a pure `ZIO` value. See
   * `effectAsyncMaybe` for the more expressive variant of this function that
   * can return a value synchronously.
   *
   * The callback function `ZIO[R, E, A] => Unit` must be called at most once.
   */
  def effectAsync[R, E, A](
    register: (ZIO[R, E, A] => Unit) => Unit,
    blockingOn: List[Fiber.Id] = Nil
  ): ZIO[R, E, A] =
    effectAsyncMaybe(ZIOFn(register) { (callback: ZIO[R, E, A] => Unit) =>
      register(callback)

      None
    }, blockingOn)

  /**
   * Imports an asynchronous side-effect into a ZIO effect. The side-effect
   * has the option of returning the value synchronously, which is useful in
   * cases where it cannot be determined if the effect is synchronous or
   * asynchronous until the side-effect is actually executed. The effect also
   * has the option of returning a canceler, which will be used by the runtime
   * to cancel the asynchronous effect if the fiber executing the effect is
   * interrupted.
   *
   * If the register function returns a value synchronously, then the callback
   * function `ZIO[R, E, A] => Unit` must not be called. Otherwise the callback
   * function must be called at most once.
   */
  def effectAsyncInterrupt[R, E, A](
    register: (ZIO[R, E, A] => Unit) => Either[Canceler[R], ZIO[R, E, A]],
    blockingOn: List[Fiber.Id] = Nil
  ): ZIO[R, E, A] = {
    import java.util.concurrent.atomic.AtomicBoolean

    import internal.OneShot

    effectTotal((new AtomicBoolean(false), OneShot.make[Canceler[R]])).flatMap {
      case (started, cancel) =>
        flatten {
          effectAsyncMaybe(
            ZIOFn(register) { (k: UIO[ZIO[R, E, A]] => Unit) =>
              started.set(true)

              try register(io => k(ZIO.succeedNow(io))) match {
                case Left(canceler) =>
                  cancel.set(canceler)
                  None
                case Right(io) => Some(ZIO.succeedNow(io))
              } finally if (!cancel.isSet) cancel.set(ZIO.unit)
            },
            blockingOn
          )
        }.onInterrupt(effectSuspendTotal(if (started.get) cancel.get() else ZIO.unit))
    }
  }

  /**
   * Imports an asynchronous effect into a pure `ZIO` value. This formulation is
   * necessary when the effect is itself expressed in terms of `ZIO`.
   */
  def effectAsyncM[R, E, A](
    register: (ZIO[R, E, A] => Unit) => ZIO[R, E, Any]
  ): ZIO[R, E, A] =
    for {
      p <- Promise.make[E, A]
      r <- ZIO.runtime[R]
      a <- ZIO.uninterruptibleMask { restore =>
            val f = register(k => r.unsafeRunAsync_(k.to(p)))

            restore(f.catchAllCause(p.halt)).fork *> restore(p.await)
          }
    } yield a

  /**
   * Imports an asynchronous effect into a pure `ZIO` value, possibly returning
   * the value synchronously.
   */
  def effectAsyncMaybe[R, E, A](
    register: (ZIO[R, E, A] => Unit) => Option[ZIO[R, E, A]],
    blockingOn: List[Fiber.Id] = Nil
  ): ZIO[R, E, A] =
    new ZIO.EffectAsync(register, blockingOn)

  /**
   * Returns a lazily constructed effect, whose construction may itself require effects.
   * When no environment is required (i.e., when R == Any) it is conceptually equivalent to `flatten(effect(io))`.
   */
  def effectSuspend[R, A](rio: => RIO[R, A]): RIO[R, A] = new ZIO.EffectSuspendPartialWith((_, _) => rio)

  /**
   * Returns a lazily constructed effect, whose construction may itself require
   * effects. The effect must not throw any exceptions. When no environment is required (i.e., when R == Any)
   * it is conceptually equivalent to `flatten(effectTotal(zio))`. If you wonder if the effect throws exceptions,
   * do not use this method, use [[Task.effectSuspend]] or [[ZIO.effectSuspend]].
   */
  def effectSuspendTotal[R, E, A](zio: => ZIO[R, E, A]): ZIO[R, E, A] =
    new ZIO.EffectSuspendTotalWith((_, _) => zio)

  /**
   * Returns a lazily constructed effect, whose construction may itself require effects.
   * The effect must not throw any exceptions. When no environment is required (i.e., when R == Any)
   * it is conceptually equivalent to `flatten(effectTotal(zio))`. If you wonder if the effect throws exceptions,
   * do not use this method, use [[Task.effectSuspend]] or [[ZIO.effectSuspend]].
   */
  def effectSuspendTotalWith[R, E, A](f: (Platform, Fiber.Id) => ZIO[R, E, A]): ZIO[R, E, A] =
    new ZIO.EffectSuspendTotalWith(f)

  /**
   * Returns a lazily constructed effect, whose construction may itself require effects.
   * When no environment is required (i.e., when R == Any) it is conceptually equivalent to `flatten(effect(io))`.
   */
  def effectSuspendWith[R, A](f: (Platform, Fiber.Id) => RIO[R, A]): RIO[R, A] =
    new ZIO.EffectSuspendPartialWith(f)

  /**
   * Imports a total synchronous effect into a pure `ZIO` value.
   * The effect must not throw any exceptions. If you wonder if the effect
   * throws exceptions, then do not use this method, use [[Task.effect]],
   * [[IO.effect]], or [[ZIO.effect]].
   *
   * {{{
   * val nanoTime: UIO[Long] = IO.effectTotal(System.nanoTime())
   * }}}
   */
  def effectTotal[A](effect: => A): UIO[A] = new ZIO.EffectTotal(() => effect)

  /**
   * Accesses the whole environment of the effect.
   */
  def environment[R]: URIO[R, R] = access(r => r)

  /**
   * Returns an effect that models failure with the specified error.
   * The moral equivalent of `throw` for pure code.
   */
  def fail[E](error: => E): IO[E, Nothing] =
    haltWith(trace => Cause.Traced(Cause.Fail(error), trace()))

  /**
   * Returns the `Fiber.Id` of the fiber executing the effect that calls this method.
   */
  val fiberId: UIO[Fiber.Id] = ZIO.descriptor.map(_.id)

  /**
   * Filters the collection using the specified effectual predicate.
   */
  def filter[R, E, A](as: Iterable[A])(f: A => ZIO[R, E, Boolean]): ZIO[R, E, List[A]] =
    as.foldRight[ZIO[R, E, List[A]]](ZIO.succeedNow(Nil)) { (a, zio) =>
      f(a).zipWith(zio)((p, as) => if (p) a :: as else as)
    }

  /**
   * Returns an effectful function that extracts out the first element of a
   * tuple.
   */
  def first[E, A, B]: ZIO[(A, B), E, A] = fromFunction[(A, B), A](_._1)

  /**
   * Returns an effect that races this effect with all the specified effects,
   * yielding the value of the first effect to succeed with a value.
   * Losers of the race will be interrupted immediately
   */
  def firstSuccessOf[R, R1 <: R, E, A](
    zio: ZIO[R, E, A],
    rest: Iterable[ZIO[R1, E, A]]
  ): ZIO[R1, E, A] =
    rest.foldLeft[ZIO[R1, E, A]](zio)(_ orElse _).refailWithTrace

  /**
   * Returns an effect that first executes the outer effect, and then executes
   * the inner effect, returning the value from the inner effect, and effectively
   * flattening a nested effect.
   */
  def flatten[R, E, A](zio: ZIO[R, E, ZIO[R, E, A]]): ZIO[R, E, A] =
    zio.flatMap(ZIO.identityFn)

  /**
   * Folds an Iterable[A] using an effectual function f, working sequentially from left to right.
   */
  def foldLeft[R, E, S, A](
    in: Iterable[A]
  )(zero: S)(f: (S, A) => ZIO[R, E, S]): ZIO[R, E, S] =
    in.foldLeft(IO.succeedNow(zero): ZIO[R, E, S]) { (acc, el) =>
      acc.flatMap(f(_, el))
    }

  /**
   * Folds an Iterable[A] using an effectual function f, working sequentially from right to left.
   */
  def foldRight[R, E, S, A](
    in: Iterable[A]
  )(zero: S)(f: (A, S) => ZIO[R, E, S]): ZIO[R, E, S] =
    in.foldRight(IO.succeedNow(zero): ZIO[R, E, S]) { (el, acc) =>
      acc.flatMap(f(el, _))
    }

  /**
   * Applies the function `f` to each element of the `Iterable[A]` and
   * returns the results in a new `List[B]`.
   *
   * For a parallel version of this method, see `foreachPar`.
   */
  def foreach[R, E, A, B](in: Iterable[A])(f: A => ZIO[R, E, B]): ZIO[R, E, List[B]] =
    in.foldRight[ZIO[R, E, List[B]]](effectTotal(Nil)) { (a, io) =>
      f(a).zipWith(io)((b, bs) => b :: bs)
    }

  /**
   * Applies the function `f` if the argument is non-empty and
   * returns the results in a new `Option[B]`.
   */
  final def foreach[R, E, A, B](in: Option[A])(f: A => ZIO[R, E, B]): ZIO[R, E, Option[B]] =
    in.fold[ZIO[R, E, Option[B]]](none)(f(_).map(Some(_)))

  /**
   * Applies the function `f` to each element of the `Chunk[A]` and
   * returns the results in a new `Chunk[B]`.
   *
   * For a parallel version of this method, see `foreachPar`.
   */
  final def foreach[R, E, A, B](in: Chunk[A])(f: A => ZIO[R, E, B]): ZIO[R, E, Chunk[B]] =
    in.mapM(f)

  /**
   * Applies the function `f` to each element of the `Iterable[A]` and runs
   * produced effects sequentially.
   *
   * Equivalent to `foreach(as)(f).unit`, but without the cost of building
   * the list of results.
   */
  def foreach_[R, E, A](as: Iterable[A])(f: A => ZIO[R, E, Any]): ZIO[R, E, Unit] =
    ZIO.effectTotal(as.iterator).flatMap { i =>
      def loop: ZIO[R, E, Unit] =
        if (i.hasNext) f(i.next) *> loop
        else ZIO.unit
      loop
    }

  /**
   * Applies the function `f` to each element of the `Chunk[A]` and
   * returns the results in a new `Chunk[B]`.
   *
   * For a parallel version of this method, see `foreachPar`.
   */
  final def foreach_[R, E, A](as: Chunk[A])(f: A => ZIO[R, E, Any]): ZIO[R, E, Unit] =
    as.mapM_(f)

  /**
   * Applies the function `f` to each element of the `Iterable[A]` in parallel,
   * and returns the results in a new `List[B]`.
   *
   * For a sequential version of this method, see `foreach`.
   */
  def foreachPar[R, E, A, B](as: Iterable[A])(fn: A => ZIO[R, E, B]): ZIO[R, E, List[B]] = {
    val size      = as.size
    val resultArr = new AtomicReferenceArray[B](size)

    val wrappedFn: ZIOFn1[(A, Int), ZIO[R, E, Any]] = ZIOFn(fn) {
      case (a, i) => fn(a).tap(b => ZIO.effectTotal(resultArr.set(i, b)))
    }

    foreachPar_(as.zipWithIndex)(wrappedFn).as(
      (0 until size).reverse.foldLeft[List[B]](Nil) { (acc, i) =>
        resultArr.get(i) :: acc
      }
    )
  }

  /**
   * Applies the function `f` to each element of the `Chunk[A]` in parallel,
   * and returns the results in a new `Chunk[B]`.
   *
   * For a sequential version of this method, see `foreach`.
   */
  final def foreachPar[R, E, A, B](as: Chunk[A])(fn: A => ZIO[R, E, B]): ZIO[R, E, Chunk[B]] =
    as.mapMPar(fn)

  /**
   * Applies the function `f` to each element of the `Iterable[A]` and runs
   * produced effects in parallel, discarding the results.
   *
   * For a sequential version of this method, see `foreach_`.
   *
   * Optimized to avoid keeping full tree of effects, so that method could be
   * able to handle large input sequences.
   * Behaves almost like this code:
   *
   * {{{
   * as.foldLeft(ZIO.unit) { (acc, a) => acc.zipParLeft(f(a)) }
   * }}}
   *
   * Additionally, interrupts all effects on any failure.
   */
  def foreachPar_[R, E, A](as: Iterable[A])(f: A => ZIO[R, E, Any]): ZIO[R, E, Unit] =
    if (as.isEmpty) ZIO.unit
    else {
      val size = as.size
      for {
        parentId       <- ZIO.fiberId
        causes         <- Ref.make[Cause[E]](Cause.empty)
        result         <- Promise.make[Nothing, Boolean]
        failureTrigger <- Promise.make[Unit, Unit]
        status         <- Ref.make((0, 0, false))

        startTask = status.modify {
          case (started, done, failing) =>
            if (failing) {
              (false, (started, done, failing))
            } else {
              (true, (started + 1, done, failing))
            }
        }

        startFailure = status.update {
          case (started, done, _) => (started, done, true)
        } *> failureTrigger.fail(())

        task = ZIOFn(f)((a: A) =>
          ZIO
            .whenM[R, E](startTask) {
              f(a).interruptible
                .tapCause(c => causes.update(_ && c) *> startFailure)
                .ensuring {
                  val isComplete = status.modify {
                    case (started, done, failing) =>
                      val newDone = done + 1
                      ((if (failing) started else size) == newDone, (started, newDone, failing))
                  }
                  ZIO.whenM(isComplete) {
                    result.complete(failureTrigger.succeed(()))
                  }
                }
            }
            .uninterruptible
        )

        fibers <- ZIO.foreach(as)(a => task(a).fork)
        interrupter = failureTrigger.await
          .catchAll(_ => ZIO.foreach(fibers)(_.interruptAs(parentId).fork) >>= Fiber.joinAll)
          .forkManaged
        _ <- interrupter.use_ {
              ZIO
                .whenM(result.await.map(!_)) {
                  causes.get >>= ZIO.haltNow
                }
                .refailWithTrace
            }
      } yield ()
    }

  /**
   * Applies the function `f` to each element of the `Iterable[A]` and runs
   * produced effects in parallel, discarding the results.
   *
   * For a sequential version of this method, see `foreach_`.
   */
  final def foreachPar_[R, E, A](as: Chunk[A])(f: A => ZIO[R, E, Any]): ZIO[R, E, Unit] =
    as.mapMPar_(f)

  /**
   * Applies the function `f` to each element of the `Iterable[A]` in parallel,
   * and returns the results in a new `List[B]`.
   *
   * Unlike `foreachPar`, this method will use at most up to `n` fibers.
   */
  def foreachParN[R, E, A, B](
    n: Int
  )(as: Iterable[A])(fn: A => ZIO[R, E, B]): ZIO[R, E, List[B]] =
    Queue
      .bounded[(Promise[E, B], A)](n.toInt)
      .bracket(_.shutdown) { q =>
        for {
          pairs <- ZIO.foreach(as)(a => Promise.make[E, B].map(p => (p, a)))
          _     <- ZIO.foreach(pairs)(pair => q.offer(pair)).fork
          _ <- ZIO.collectAll(List.fill(n.toInt)(q.take.flatMap {
                case (p, a) => fn(a).foldCauseM(c => ZIO.foreach(pairs)(_._1.halt(c)), b => p.succeed(b))
              }.forever.fork))
          res <- ZIO.foreach(pairs)(_._1.await)
        } yield res
      }
      .refailWithTrace

  /**
   * Applies the function `f` to each element of the `Iterable[A]` and runs
   * produced effects in parallel, discarding the results.
   *
   * Unlike `foreachPar_`, this method will use at most up to `n` fibers.
   */
  def foreachParN_[R, E, A](
    n: Int
  )(as: Iterable[A])(f: A => ZIO[R, E, Any]): ZIO[R, E, Unit] =
    Semaphore
      .make(n.toLong)
      .flatMap { semaphore =>
        ZIO.foreachPar_(as) { a =>
          semaphore.withPermit(f(a))
        }
      }
      .refailWithTrace

  /**
   * Returns an effect that forks all of the specified values, and returns a
   * composite fiber that produces a list of their results, in order.
   */
  def forkAll[R, E, A](as: Iterable[ZIO[R, E, A]]): URIO[R, Fiber[E, List[A]]] =
    ZIO.foreach(as)(_.map(List(_)).fork).map { fibers =>
      fibers
        .reduceRightOption[Fiber[E, List[A]]]((a, as) => a.zipWith(as)((a, as) => a ::: as))
        .getOrElse(Fiber.succeed(Nil))
    }

  /**
   * Returns an effect that forks all of the specified values, and returns a
   * composite fiber that produces unit. This version is faster than [[forkAll]]
   * in cases where the results of the forked fibers are not needed.
   */
  def forkAll_[R, E, A](as: Iterable[ZIO[R, E, A]]): URIO[R, Unit] =
    as.foldRight[URIO[R, Unit]](ZIO.unit)(_.fork *> _)

  /**
   * Lifts an `Either` into a `ZIO` value.
   */
  def fromEither[E, A](v: => Either[E, A]): IO[E, A] =
    effectTotal(v).flatMap(_.fold(failNow, succeedNow))

  /**
   * Creates a `ZIO` value that represents the exit value of the specified
   * fiber.
   */
  def fromFiber[E, A](fiber: => Fiber[E, A]): IO[E, A] =
    effectTotal(fiber).flatMap(_.join)

  /**
   * Creates a `ZIO` value that represents the exit value of the specified
   * fiber.
   */
  def fromFiberM[E, A](fiber: IO[E, Fiber[E, A]]): IO[E, A] =
    fiber.flatMap(_.join)

  /**
   * Lifts a function `R => A` into a `URIO[R, A]`.
   */
  def fromFunction[R, A](f: R => A): URIO[R, A] =
    access(f)

  /**
   * Lifts a function returning Future into an effect that requires the input to the function.
   */
  def fromFunctionFuture[R, A](f: R => scala.concurrent.Future[A]): RIO[R, A] =
    fromFunction(f).flatMap(a => fromFuture(_ => a))

  /**
   * Lifts an effectful function whose effect requires no environment into
   * an effect that requires the input to the function.
   */
  def fromFunctionM[R, E, A](f: R => IO[E, A]): ZIO[R, E, A] =
    accessM(f)

  /**
   * Imports a function that creates a [[scala.concurrent.Future]] from an
   * [[scala.concurrent.ExecutionContext]] into a `ZIO`.
   */
  def fromFuture[A](make: ExecutionContext => scala.concurrent.Future[A]): Task[A] =
    Task.descriptorWith { d =>
      val ec = d.executor.asEC
      ZIO.effect(make(ec)).flatMap { f =>
        f.value
          .fold(
            Task.effectAsync { (k: Task[A] => Unit) =>
              f.onComplete {
                case Success(a) => k(Task.succeedNow(a))
                case Failure(t) => k(Task.failNow(t))
              }(ec)
            }
          )(Task.fromTry(_))
      }
    }

  /**
   * Imports a function that creates a [[scala.concurrent.Future]] from an
   * [[scala.concurrent.ExecutionContext]] into a `ZIO`. The provided
   * `ExecutionContext` will interrupt the `Future` between asynchronous
   * operations such as `map` and `flatMap` if this effect is interrupted. Note
   * that no attempt will be made to interrupt a `Future` blocking on a
   * synchronous operation and that the `Future` must be created using the
   * provided `ExecutionContext`.
   */
  def fromFutureInterrupt[A](make: ExecutionContext => scala.concurrent.Future[A]): Task[A] =
    Task.descriptorWith { d =>
      val ec          = d.executor.asEC
      val interrupted = new java.util.concurrent.atomic.AtomicBoolean(false)
      val latch       = scala.concurrent.Promise[Unit]()
      val interruptibleEC = new scala.concurrent.ExecutionContext {
        def execute(runnable: Runnable): Unit =
          if (!interrupted.get) ec.execute(runnable)
          else {
            val _ = latch.success(())
          }
        def reportFailure(cause: Throwable): Unit =
          ec.reportFailure(cause)
      }
      effect(make(interruptibleEC)).flatMap { f =>
        f.value
          .fold(
            Task.effectAsync { (cb: Task[A] => Unit) =>
              f.onComplete {
                case Success(a) => latch.success(()); cb(Task.succeed(a))
                case Failure(t) => latch.success(()); cb(Task.failNow(t))
              }(interruptibleEC)
            }
          )(Task.fromTry(_))
      }.onInterrupt(
        Task.effectTotal(interrupted.set(true)) *> Task.fromFuture(_ => latch.future).orDie
      )
    }

  /**
   * Lifts an `Option` into a `ZIO`.
   */
  def fromOption[A](v: => Option[A]): IO[Unit, A] =
    effectTotal(v).flatMap(_.fold[IO[Unit, A]](failNow(()))(succeedNow))

  /**
   * Lifts a `Try` into a `ZIO`.
   */
  def fromTry[A](value: => scala.util.Try[A]): Task[A] =
    effect(value).flatMap {
      case scala.util.Success(v) => ZIO.succeedNow(v)
      case scala.util.Failure(t) => ZIO.failNow(t)
    }

  /**
   * Returns an effect that models failure with the specified `Cause`.
   */
  def halt[E](cause: => Cause[E]): IO[E, Nothing] =
    new ZIO.Fail(_ => cause)

  /**
   * Returns an effect that models failure with the specified `Cause`.
   *
   * This version takes in a lazily-evaluated trace that can be attached to the `Cause`
   * via `Cause.Traced`.
   */
  def haltWith[E](function: (() => ZTrace) => Cause[E]): IO[E, Nothing] = new ZIO.Fail(function)

  /**
   * Returns the identity effectful function, which performs no effects
   */
  def identity[R]: URIO[R, R] = fromFunction[R, R](ZIO.identityFn[R])

  /**
   * Runs `onTrue` if the result of `b` is `true` and `onFalse` otherwise.
   */
  def ifM[R, E](b: ZIO[R, E, Boolean]): ZIO.IfM[R, E] =
    new ZIO.IfM(b)

  /**
   * Like [[never]], but fibers that running this effect won't be garbage
   * collected unless interrupted.
   */
  val infinity: URIO[Clock, Nothing] = ZIO.sleep(Duration.fromNanos(Long.MaxValue)) *> ZIO.never

  /**
   * Returns an effect that is interrupted as if by the fiber calling this
   * method.
   */
  val interrupt: UIO[Nothing] = ZIO.fiberId.flatMap(fiberId => interruptAs(fiberId))

  /**
   * Interrupts all child fibers of the fiber executing the effect.
   */
  def interruptAllChildren: UIO[Unit] = ZIO.children.flatMap(Fiber.interruptAll(_))

  /**
   * Returns an effect that is interrupted as if by the specified fiber.
   */
  def interruptAs(fiberId: => Fiber.Id): UIO[Nothing] =
    haltWith(trace => Cause.Traced(Cause.interrupt(fiberId), trace()))

  /**
   * Prefix form of `ZIO#interruptible`.
   */
  def interruptible[R, E, A](zio: ZIO[R, E, A]): ZIO[R, E, A] =
    zio.interruptible

  /**
   * Makes the effect interruptible, but passes it a restore function that
   * can be used to restore the inherited interruptibility from whatever region
   * the effect is composed into.
   */
  def interruptibleMask[R, E, A](
    k: ZIO.InterruptStatusRestore => ZIO[R, E, A]
  ): ZIO[R, E, A] =
    checkInterruptible(flag => k(new ZIO.InterruptStatusRestore(flag)).interruptible)

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
  def iterate[R, E, S](initial: S)(cont: S => Boolean)(body: S => ZIO[R, E, S]): ZIO[R, E, S] =
    if (cont(initial)) body(initial).flatMap(iterate(_)(cont)(body))
    else ZIO.succeedNow(initial)

  /**
   *  Returns an effect with the value on the left part.
   */
  def left[A](a: => A): UIO[Either[A, Nothing]] =
    succeed(Left(a))

  /**
   * Returns an effect that will execute the specified effect fully on the
   * provided executor, before returning to the default executor. See
   * [[ZIO!.lock]].
   */
  def lock[R, E, A](executor: => Executor)(zio: ZIO[R, E, A]): ZIO[R, E, A] =
    ZIO.effectSuspendTotal(new ZIO.Lock(executor, zio))

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
  def loop[R, E, A, S](initial: S)(cont: S => Boolean, inc: S => S)(body: S => ZIO[R, E, A]): ZIO[R, E, List[A]] =
    if (cont(initial))
      body(initial).flatMap(a => loop(inc(initial))(cont, inc)(body).map(as => a :: as))
    else
      ZIO.succeedNow(List.empty[A])

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
  def loop_[R, E, S](initial: S)(cont: S => Boolean, inc: S => S)(body: S => ZIO[R, E, Any]): ZIO[R, E, Unit] =
    if (cont(initial)) body(initial) *> loop_(inc(initial))(cont, inc)(body)
    else ZIO.unit

  /**
   * Sequentially zips the specified effects using the specified combiner
   * function.
   */
  def mapN[R, E, A, B, C](zio1: ZIO[R, E, A], zio2: ZIO[R, E, B])(f: (A, B) => C): ZIO[R, E, C] =
    zio1.zipWith(zio2)(f)

  /**
   * Sequentially zips the specified effects using the specified combiner
   * function.
   */
  def mapN[R, E, A, B, C, D](zio1: ZIO[R, E, A], zio2: ZIO[R, E, B], zio3: ZIO[R, E, C])(
    f: (A, B, C) => D
  ): ZIO[R, E, D] =
    for {
      a <- zio1
      b <- zio2
      c <- zio3
    } yield f(a, b, c)

  /**
   * Sequentially zips the specified effects using the specified combiner
   * function.
   */
  def mapN[R, E, A, B, C, D, F](zio1: ZIO[R, E, A], zio2: ZIO[R, E, B], zio3: ZIO[R, E, C], zio4: ZIO[R, E, D])(
    f: (A, B, C, D) => F
  ): ZIO[R, E, F] =
    for {
      a <- zio1
      b <- zio2
      c <- zio3
      d <- zio4
    } yield f(a, b, c, d)

  /**
   * Returns an effect that executes the specified effects in parallel,
   * combining their results with the specified `f` function. If any effect
   * fails, then the other effects will be interrupted.
   */
  def mapParN[R, E, A, B, C](zio1: ZIO[R, E, A], zio2: ZIO[R, E, B])(f: (A, B) => C): ZIO[R, E, C] =
    zio1.zipWithPar(zio2)(f)

  /**
   * Returns an effect that executes the specified effects in parallel,
   * combining their results with the specified `f` function. If any effect
   * fails, then the other effects will be interrupted.
   */
  def mapParN[R, E, A, B, C, D](zio1: ZIO[R, E, A], zio2: ZIO[R, E, B], zio3: ZIO[R, E, C])(
    f: (A, B, C) => D
  ): ZIO[R, E, D] =
    (zio1 <&> zio2 <&> zio3).map {
      case ((a, b), c) => f(a, b, c)
    }

  /**
   * Returns an effect that executes the specified effects in parallel,
   * combining their results with the specified `f` function. If any effect
   * fails, then the other effects will be interrupted.
   */
  def mapParN[R, E, A, B, C, D, F](
    zio1: ZIO[R, E, A],
    zio2: ZIO[R, E, B],
    zio3: ZIO[R, E, C],
    zio4: ZIO[R, E, D]
  )(f: (A, B, C, D) => F): ZIO[R, E, F] =
    (zio1 <&> zio2 <&> zio3 <&> zio4).map {
      case (((a, b), c), d) => f(a, b, c, d)
    }

  /**
   * Merges an `Iterable[IO]` to a single IO, working sequentially.
   */
  def mergeAll[R, E, A, B](
    in: Iterable[ZIO[R, E, A]]
  )(zero: B)(f: (B, A) => B): ZIO[R, E, B] =
    in.foldLeft[ZIO[R, E, B]](succeedNow(zero))(_.zipWith(_)(f))

  /**
   * Merges an `Iterable[IO]` to a single IO, working in parallel.
   *
   * It's unsafe to execute side effects inside `f`, as `f` may be executed
   * more than once for some of `in` elements during effect execution.
   */
  def mergeAllPar[R, E, A, B](
    in: Iterable[ZIO[R, E, A]]
  )(zero: B)(f: (B, A) => B): ZIO[R, E, B] =
    Ref.make(zero) >>= { acc =>
      foreachPar_(in) {
        Predef.identity(_) >>= { a =>
          acc.update(f(_, a))
        }
      } *> acc.get
    }

  /**
   * Returns an effect with the empty value.
   */
  val none: UIO[Option[Nothing]] = succeedNow(None)

  /**
   * Feeds elements of type `A` to a function `f` that returns an effect.
   * Collects all successes and failures in a tupled fashion.
   */
  def partition[R, E, A, B](
    in: Iterable[A]
  )(f: A => ZIO[R, E, B])(implicit ev: CanFail[E]): ZIO[R, Nothing, (List[E], List[B])] =
    ZIO.foreach(in)(f(_).either).map(partitionMap(_)(ZIO.identityFn))

  /**
   * Feeds elements of type `A` to a function `f` that returns an effect.
   * Collects all successes and failures in parallel and returns the result as
   * a tuple.
   */
  def partitionPar[R, E, A, B](
    in: Iterable[A]
  )(f: A => ZIO[R, E, B])(implicit ev: CanFail[E]): ZIO[R, Nothing, (List[E], List[B])] =
    ZIO.foreachPar(in)(f(_).either).map(ZIO.partitionMap(_)(ZIO.identityFn))

  /**
   * Feeds elements of type `A` to a function `f` that returns an effect.
   * Collects all successes and failures in parallel and returns the result as
   * a tuple.
   *
   * Unlike [[partitionPar]], this method will use at most up to `n` fibers.
   */
  def partitionParN[R, E, A, B](n: Int)(
    in: Iterable[A]
  )(f: A => ZIO[R, E, B])(implicit ev: CanFail[E]): ZIO[R, Nothing, (List[E], List[B])] =
    ZIO.foreachParN(n)(in)(f(_).either).map(ZIO.partitionMap(_)(ZIO.identityFn))

  /**
   * Given an environment `R`, returns a function that can supply the
   * environment to programs that require it, removing their need for any
   * specific environment.
   *
   * This is similar to dependency injection, and the `provide` function can be
   * thought of as `inject`.
   */
  def provide[R, E, A](r: => R): ZIO[R, E, A] => IO[E, A] =
    (zio: ZIO[R, E, A]) => new ZIO.Provide(r, zio)

  /**
   * Returns a effect that will never produce anything. The moral
   * equivalent of `while(true) {}`, only without the wasted CPU cycles.
   */
  val never: UIO[Nothing] = effectAsync[Any, Nothing, Nothing](_ => ())

  /**
   * Races an `IO[E, A]` against zero or more other effects. Yields either the
   * first success or the last failure.
   */
  def raceAll[R, R1 <: R, E, A](
    zio: ZIO[R, E, A],
    ios: Iterable[ZIO[R1, E, A]]
  ): ZIO[R1, E, A] =
    zio.raceAll(ios)

  /**
   * Reduces an `Iterable[IO]` to a single `IO`, working sequentially.
   */
  def reduceAll[R, R1 <: R, E, A](a: ZIO[R, E, A], as: Iterable[ZIO[R1, E, A]])(
    f: (A, A) => A
  ): ZIO[R1, E, A] =
    as.foldLeft[ZIO[R1, E, A]](a)(_.zipWith(_)(f))

  /**
   * Reduces an `Iterable[IO]` to a single `IO`, working in parallel.
   */
  def reduceAllPar[R, R1 <: R, E, A](a: ZIO[R, E, A], as: Iterable[ZIO[R1, E, A]])(
    f: (A, A) => A
  ): ZIO[R1, E, A] = {
    def prepend[Z](z: Z, zs: Iterable[Z]): Iterable[Z] =
      new Iterable[Z] {
        override def iterator: Iterator[Z] = Iterator(z) ++ zs.iterator
      }

    val all = prepend(a, as)
    mergeAllPar(all)(Option.empty[A]) { (acc, elem) =>
      Some(acc.fold(elem)(f(_, elem)))
    }.map(_.get)
  }

  /**
   * Replicates the given effect n times.
   * If 0 or negative numbers are given, an empty `Iterable` will return.
   */
  def replicate[R, E, A](n: Int)(effect: ZIO[R, E, A]): Iterable[ZIO[R, E, A]] =
    new Iterable[ZIO[R, E, A]] {
      override def iterator: Iterator[ZIO[R, E, A]] = Iterator.range(0, n).map(_ => effect)
    }

  /**
   * Requires that the given `ZIO[R, E, Option[A]]` contain a value. If there is no
   * value, then the specified error will be raised.
   */
  def require[R, E, A](error: => E): ZIO[R, E, Option[A]] => ZIO[R, E, A] =
    (io: ZIO[R, E, Option[A]]) => io.flatMap(_.fold[ZIO[R, E, A]](failNow[E](error))(succeedNow))

  /**
   * Acquires a resource, uses the resource, and then releases the resource.
   * However, unlike `bracket`, the separation of these phases allows
   * the acquisition to be interruptible.
   *
   * Useful for concurrent data structures and other cases where the
   * 'deallocator' can tell if the allocation succeeded or not just by
   * inspecting internal / external state.
   */
  def reserve[R, E, A, B](reservation: ZIO[R, E, Reservation[R, E, A]])(use: A => ZIO[R, E, B]): ZIO[R, E, B] =
    ZManaged(reservation).use(use)

  /**
   *  Returns an effect with the value on the right part.
   */
  def right[B](b: => B): UIO[Either[Nothing, B]] =
    succeed(Right(b))

  /**
   * Returns an effect that accesses the runtime, which can be used to
   * (unsafely) execute tasks. This is useful for integration with
   * non-functional code that must call back into functional code.
   */
  def runtime[R]: URIO[R, Runtime[R]] =
    for {
      environment <- environment[R]
      platform    <- effectSuspendTotalWith((p, _) => ZIO.succeedNow(p))
    } yield Runtime(environment, platform)

  /**
   * Returns an effectful function that extracts out the second element of a
   * tuple.
   */
  def second[E, A, B]: ZIO[(A, B), E, B] = fromFunction[(A, B), B](_._2)

  /**
   *  Alias for [[ZIO.collectAll]]
   */
  @deprecated("Use ZIO.collectAll", "1.0.0")
  def sequence[R, E, A](in: Iterable[ZIO[R, E, A]]): ZIO[R, E, List[A]] =
    collectAll[R, E, A](in)

  /**
   *  Alias for [[ZIO.collectAllPar]]
   */
  @deprecated("Use ZIO.collectAllPar", "1.0.0")
  def sequencePar[R, E, A](as: Iterable[ZIO[R, E, A]]): ZIO[R, E, List[A]] =
    collectAllPar[R, E, A](as)

  /**
   *  Alias for [[ZIO.collectAllParN]]
   */
  @deprecated("Use ZIO.collectAllParN", "1.0.0")
  def sequenceParN[R, E, A](n: Int)(as: Iterable[ZIO[R, E, A]]): ZIO[R, E, List[A]] =
    collectAllParN[R, E, A](n)(as)

  /**
   * Sleeps for the specified duration. This method is asynchronous, and does
   * not actually block the fiber.
   */
  def sleep(duration: => Duration): URIO[Clock, Unit] =
    clock.sleep(duration)

  /**
   *  Returns an effect with the optional value.
   */
  def some[A](a: => A): UIO[Option[A]] =
    succeed(Some(a))

  /**
   * Returns an effect that models success with the specified value.
   */
  def succeed[A](a: => A): UIO[A] =
    effectTotal(a)

  /**
   * Returns an effectful function that merely swaps the elements in a `Tuple2`.
   */
  def swap[E, A, B]: ZIO[(A, B), E, (B, A)] =
    fromFunction[(A, B), (B, A)](_.swap)

  /**
   * Capture ZIO trace at the current point
   * */
  def trace: UIO[ZTrace] = ZIO.Trace

  /**
   * Prefix form of `ZIO#traced`.
   */
  def traced[R, E, A](zio: ZIO[R, E, A]): ZIO[R, E, A] =
    zio.traced

  /**
   * Alias for [[[ZIO.foreach[R,E,A,B](in:Iterable*]]]
   */
  @deprecated("Use ZIO.foreach", "1.0.0")
  def traverse[R, E, A, B](in: Iterable[A])(f: A => ZIO[R, E, B]): ZIO[R, E, List[B]] =
    foreach[R, E, A, B](in)(f)

  /**
   * Alias for [[[ZIO.foreach_[R,E,A](as:Iterable*]]]
   */
  @deprecated("Use ZIO.foreach_", "1.0.0")
  def traverse_[R, E, A](as: Iterable[A])(f: A => ZIO[R, E, Any]): ZIO[R, E, Unit] =
    foreach_[R, E, A](as)(f)

  /**
   * Alias for [[[ZIO.foreachPar[R,E,A,B](as:Iterable*]]]
   */
  @deprecated("Use ZIO.foreachPar", "1.0.0")
  def traversePar[R, E, A, B](as: Iterable[A])(fn: A => ZIO[R, E, B]): ZIO[R, E, List[B]] =
    foreachPar[R, E, A, B](as)(fn)

  /**
   * Alias for [[[ZIO.foreachPar_[R,E,A](as:Iterable*]]]
   */
  @deprecated("Use ZIO.foreachPar_", "1.0.0")
  def traversePar_[R, E, A](as: Iterable[A])(f: A => ZIO[R, E, Any]): ZIO[R, E, Unit] =
    foreachPar_[R, E, A](as)(f)

  /**
   * Alias for [[ZIO.foreachParN]]
   */
  @deprecated("Use ZIO.foreachParN", "1.0.0")
  def traverseParN[R, E, A, B](
    n: Int
  )(as: Iterable[A])(fn: A => ZIO[R, E, B]): ZIO[R, E, List[B]] =
    foreachParN[R, E, A, B](n)(as)(fn)

  /**
   * Alias for [[ZIO.foreachParN_]]
   */
  @deprecated("Use ZIO.foreachParN_", "1.0.0")
  def traverseParN_[R, E, A](
    n: Int
  )(as: Iterable[A])(f: A => ZIO[R, E, Any]): ZIO[R, E, Unit] =
    foreachParN_[R, E, A](n)(as)(f)

  /**
   * Strictly-evaluated unit lifted into the `ZIO` monad.
   */
  val unit: UIO[Unit] = succeedNow(())

  /**
   * Prefix form of `ZIO#uninterruptible`.
   */
  def uninterruptible[R, E, A](zio: ZIO[R, E, A]): ZIO[R, E, A] =
    zio.uninterruptible

  /**
   * Makes the effect uninterruptible, but passes it a restore function that
   * can be used to restore the inherited interruptibility from whatever region
   * the effect is composed into.
   */
  def uninterruptibleMask[R, E, A](
    k: ZIO.InterruptStatusRestore => ZIO[R, E, A]
  ): ZIO[R, E, A] =
    checkInterruptible(flag => k(new ZIO.InterruptStatusRestore(flag)).uninterruptible)

  /**
   * The inverse operation `IO.sandboxed`
   *
   * Terminates with exceptions on the `Left` side of the `Either` error, if it
   * exists. Otherwise extracts the contained `IO[E, A]`
   */
  def unsandbox[R, E, A](v: ZIO[R, Cause[E], A]): ZIO[R, E, A] =
    v.mapErrorCause(_.flatten)

  /**
   * Prefix form of `ZIO#untraced`.
   */
  def untraced[R, E, A](zio: ZIO[R, E, A]): ZIO[R, E, A] =
    zio.untraced

  /**
   * Feeds elements of type `A` to `f` and accumulates all errors in error
   * channel or successes in success channel.
   *
   * This combinator is lossy meaning that if there are errors all successes
   * will be lost. To retain all information please use [[partition]].
   */
  def validate[R, E, A, B](
    in: Iterable[A]
  )(f: A => ZIO[R, E, B])(implicit ev: CanFail[E]): ZIO[R, ::[E], List[B]] =
    partition(in)(f).flatMap {
      case (e :: es, _) => ZIO.failNow(::(e, es))
      case (_, bs)      => ZIO.succeedNow(bs)
    }

  /**
   * Feeds elements of type `A` to `f `and accumulates, in parallel, all errors
   * in error channel or successes in success channel.
   *
   * This combinator is lossy meaning that if there are errors all successes
   * will be lost. To retain all information please use [[partitionPar]].
   */
  def validatePar[R, E, A, B](
    in: Iterable[A]
  )(f: A => ZIO[R, E, B])(implicit ev: CanFail[E]): ZIO[R, ::[E], List[B]] =
    partitionPar(in)(f).flatMap {
      case (e :: es, _) => ZIO.failNow(::(e, es))
      case (_, bs)      => ZIO.succeedNow(bs)
    }

  /**
   * Feeds elements of type `A` to `f` until it succeeds. Returns first success
   * or the accumulation of all errors.
   */
  def validateFirst[R, E, A, B](
    in: Iterable[A]
  )(f: A => ZIO[R, E, B])(implicit ev: CanFail[E]): ZIO[R, List[E], B] =
    ZIO.foreach(in)(f(_).flip).flip

  /**
   * Feeds elements of type `A` to `f`, in parallel, until it succeeds. Returns
   * first success or the accumulation of all errors.
   *
   * In case of success all other running fibers are terminated.
   */
  def validateFirstPar[R, E, A, B](
    in: Iterable[A]
  )(f: A => ZIO[R, E, B])(implicit ev: CanFail[E]): ZIO[R, List[E], B] =
    ZIO.foreachPar(in)(f(_).flip).flip

  /**
   * The moral equivalent of `if (p) exp`
   */
  def when[R, E](b: => Boolean)(zio: ZIO[R, E, Any]): ZIO[R, E, Unit] =
    if (b) zio.unit else unit

  /**
   * Runs an effect when the supplied `PartialFunction` matches for the given value, otherwise does nothing.
   */
  def whenCase[R, E, A](a: => A)(pf: PartialFunction[A, ZIO[R, E, Any]]): ZIO[R, E, Unit] =
    effectSuspendTotal(pf.applyOrElse(a, (_: A) => unit).unit)

  /**
   * Runs an effect when the supplied `PartialFunction` matches for the given effectful value, otherwise does nothing.
   */
  def whenCaseM[R, E, A](a: ZIO[R, E, A])(pf: PartialFunction[A, ZIO[R, E, Any]]): ZIO[R, E, Unit] =
    a.flatMap(whenCase(_)(pf))

  /**
   * The moral equivalent of `if (p) exp` when `p` has side-effects
   */
  def whenM[R, E](b: ZIO[R, E, Boolean])(zio: ZIO[R, E, Any]): ZIO[R, E, Unit] =
    b.flatMap(b => if (b) zio.unit else unit)

  /**
   * Returns an effect that yields to the runtime system, starting on a fresh
   * stack. Manual use of this method can improve fairness, at the cost of
   * overhead.
   */
  val yieldNow: UIO[Unit] = ZIO.Yield

  def apply[A](a: => A): Task[A] = effect(a)

  private val _IdentityFn: Any => Any = (a: Any) => a

  private[zio] def identityFn[A]: A => A = _IdentityFn.asInstanceOf[A => A]

  private[zio] def partitionMap[A, A1, A2](iterable: Iterable[A])(f: A => Either[A1, A2]): (List[A1], List[A2]) =
    iterable.foldRight((List.empty[A1], List.empty[A2])) {
      case (a, (es, bs)) =>
        f(a).fold(
          e => (e :: es, bs),
          b => (es, b :: bs)
        )
    }

  private[zio] val unitFn: Any => Unit = (_: Any) => ()

  implicit final class ZIOAutocloseableOps[R, E, A <: AutoCloseable](private val io: ZIO[R, E, A]) extends AnyVal {

    /**
     * Like `bracket`, safely wraps a use and release of a resource.
     * This resource will get automatically closed, because it implements `AutoCloseable`.
     */
    def bracketAuto[R1 <: R, E1 >: E, B](use: A => ZIO[R1, E1, B]): ZIO[R1, E1, B] =
      // TODO: Dotty doesn't infer this properly: io.bracket[R1, E1](a => UIO(a.close()))(use)
      bracket(io)(a => UIO(a.close()))(use)

    /**
     * Converts this ZIO value to a ZManaged value. See [[ZManaged.fromAutoCloseable]].
     */
    def toManaged: ZManaged[R, E, A] = ZManaged.fromAutoCloseable(io)
  }

  implicit final class ZioRefineToOrDieOps[R, E <: Throwable, A](private val self: ZIO[R, E, A]) extends AnyVal {

    /**
     * Keeps some of the errors, and terminates the fiber with the rest.
     */
    def refineToOrDie[E1 <: E: ClassTag](implicit ev: CanFail[E]): ZIO[R, E1, A] =
      self.refineOrDie { case e: E1 => e }
  }

  final class ProvideSomeLayer[R0 <: Has[_], -R, +E, +A](private val self: ZIO[R, E, A]) extends AnyVal {
    def apply[E1 >: E, R1 <: Has[_]](
      layer: ZLayer[R0, E1, R1]
    )(implicit ev1: R0 with R1 <:< R, ev2: NeedsEnv[R], tagged: Tagged[R1]): ZIO[R0, E1, A] =
      self.provideLayer[E1, R0, R0 with R1](ZLayer.identity[R0] ++ layer)
  }

  implicit final class ZIOWithFilterOps[R, E, A](private val self: ZIO[R, E, A]) extends AnyVal {

    /**
     * Enables to check conditions in the value produced by ZIO
     * If the condition is not satisfied, it fails with NoSuchElementException
     * this provide the syntax sugar in for-comprehension:
     * for {
     *   (i, j) <- io1
     *   positive <- io2 if positive > 0
     *  } yield ()
     */
    def withFilter(predicate: A => Boolean)(implicit ev: NoSuchElementException <:< E): ZIO[R, E, A] =
      self.flatMap { a =>
        if (predicate(a)) ZIO.succeedNow(a)
        else ZIO.failNow(new NoSuchElementException("The value doesn't satisfy the predicate"))
      }
  }

  final class InterruptStatusRestore(private val flag: zio.InterruptStatus) extends AnyVal {
    def apply[R, E, A](zio: ZIO[R, E, A]): ZIO[R, E, A] =
      zio.interruptStatus(flag)
  }

  final class IfM[R, E](private val b: ZIO[R, E, Boolean]) extends AnyVal {
    def apply[R1 <: R, E1 >: E, A](onTrue: ZIO[R1, E1, A], onFalse: ZIO[R1, E1, A]): ZIO[R1, E1, A] =
      b.flatMap(b => if (b) onTrue else onFalse)
  }

  final class TimeoutTo[R, E, A, B](self: ZIO[R, E, A], b: B) {
    def apply[B1 >: B](f: A => B1)(duration: Duration): ZIO[R with Clock, E, B1] =
      (self map f) raceFirst (ZIO.sleep(duration) as b)
  }

  final class BracketAcquire_[-R, +E](private val acquire: ZIO[R, E, Any]) extends AnyVal {
    def apply[R1 <: R](release: URIO[R1, Any]): BracketRelease_[R1, E] =
      new BracketRelease_(acquire, release)
  }
  final class BracketRelease_[-R, +E](acquire: ZIO[R, E, Any], release: URIO[R, Any]) {
    def apply[R1 <: R, E1 >: E, B](use: ZIO[R1, E1, B]): ZIO[R1, E1, B] =
      ZIO.bracket(acquire, (_: Any) => release, (_: Any) => use)
  }

  final class BracketAcquire[-R, +E, +A](private val acquire: ZIO[R, E, A]) extends AnyVal {
    def apply[R1](release: A => URIO[R1, Any]): BracketRelease[R with R1, E, A] =
      new BracketRelease[R with R1, E, A](acquire, release)
  }
  final class BracketRelease[-R, +E, +A](acquire: ZIO[R, E, A], release: A => URIO[R, Any]) {
    def apply[R1 <: R, E1 >: E, B](use: A => ZIO[R1, E1, B]): ZIO[R1, E1, B] =
      ZIO.bracket(acquire, release, use)
  }

  final class BracketExitAcquire[-R, +E, +A](private val acquire: ZIO[R, E, A]) extends AnyVal {
    def apply[R1 <: R, E1 >: E, B](
      release: (A, Exit[E1, B]) => URIO[R1, Any]
    ): BracketExitRelease[R1, E, E1, A, B] =
      new BracketExitRelease(acquire, release)
  }
  final class BracketExitRelease[-R, +E, E1, +A, B](
    acquire: ZIO[R, E, A],
    release: (A, Exit[E1, B]) => URIO[R, Any]
  ) {
    def apply[R1 <: R, E2 >: E <: E1, B1 <: B](use: A => ZIO[R1, E2, B1]): ZIO[R1, E2, B1] =
      ZIO.bracketExit(acquire, release, use)
  }

  final class BracketForkAcquire_[-R, +E](private val acquire: ZIO[R, E, Any]) extends AnyVal {
    @deprecated("Use ZIO#disconnect on an ordinary bracket", "1.0.0")
    def apply[R1 <: R](release: URIO[R1, Any]): BracketForkRelease_[R1, E] =
      new BracketForkRelease_(acquire, release)
  }
  final class BracketForkRelease_[-R, +E](acquire: ZIO[R, E, Any], release: URIO[R, Any]) {
    @deprecated("Use ZIO#disconnect on an ordinary bracket", "1.0.0")
    def apply[R1 <: R, E1 >: E, B](use: ZIO[R1, E1, B]): ZIO[R1, E1, B] =
      ZIO.bracketFork(acquire, (_: Any) => release, (_: Any) => use)
  }
  final class BracketForkAcquire[-R, +E, +A](private val acquire: ZIO[R, E, A]) extends AnyVal {
    @deprecated("Use ZIO#disconnect on an ordinary bracket", "1.0.0")
    def apply[R1](release: A => URIO[R1, Any]): BracketForkRelease[R with R1, E, A] =
      new BracketForkRelease[R with R1, E, A](acquire, release)
  }
  final class BracketForkRelease[-R, +E, +A](acquire: ZIO[R, E, A], release: A => URIO[R, Any]) {
    @deprecated("Use ZIO#disconnect on an ordinary bracket", "1.0.0")
    def apply[R1 <: R, E1 >: E, B](use: A => ZIO[R1, E1, B]): ZIO[R1, E1, B] =
      ZIO.bracketFork(acquire, release, use)
  }
  final class BracketForkExitAcquire[-R, +E, +A](private val acquire: ZIO[R, E, A]) extends AnyVal {
    @deprecated("Use ZIO#disconnect on an ordinary bracket", "1.0.0")
    def apply[R1 <: R, E1 >: E, B](
      release: (A, Exit[E1, B]) => URIO[R1, Any]
    ): BracketForkExitRelease[R1, E, E1, A, B] =
      new BracketForkExitRelease(acquire, release)
  }
  final class BracketForkExitRelease[-R, +E, E1, +A, B](
    acquire: ZIO[R, E, A],
    release: (A, Exit[E1, B]) => URIO[R, Any]
  ) {
    @deprecated("Use ZIO#disconnect on an ordinary bracket", "1.0.0")
    def apply[R1 <: R, E2 >: E <: E1, B1 <: B](use: A => ZIO[R1, E2, B1]): ZIO[R1, E2, B1] =
      ZIO.bracketForkExit(acquire, release, use)
  }

  final class AccessPartiallyApplied[R](private val dummy: Boolean = true) extends AnyVal {
    def apply[A](f: R => A): URIO[R, A] =
      new ZIO.Read(r => succeedNow(f(r)))
  }

  final class AccessMPartiallyApplied[R](private val dummy: Boolean = true) extends AnyVal {
    def apply[E, A](f: R => ZIO[R, E, A]): ZIO[R, E, A] =
      new ZIO.Read(f)
  }

  @inline
  private def succeedLeft[E, A]: E => UIO[Either[E, A]] =
    _succeedLeft.asInstanceOf[E => UIO[Either[E, A]]]

  private val _succeedLeft: Any => IO[Any, Either[Any, Any]] =
    e2 => succeedNow[Either[Any, Any]](Left(e2))

  @inline
  private def succeedRight[E, A]: A => UIO[Either[E, A]] =
    _succeedRight.asInstanceOf[A => UIO[Either[E, A]]]

  private val _succeedRight: Any => IO[Any, Either[Any, Any]] =
    a => succeedNow[Either[Any, Any]](Right(a))

  final class ZipLeftFn[R, E, A, B](override val underlying: () => ZIO[R, E, A]) extends ZIOFn1[B, ZIO[R, E, B]] {
    def apply(a: B): ZIO[R, E, B] =
      underlying().as(a)
  }

  final class ZipRightFn[R, E, A, B](override val underlying: () => ZIO[R, E, B]) extends ZIOFn1[A, ZIO[R, E, B]] {
    def apply(a: A): ZIO[R, E, B] = {
      val _ = a
      underlying()
    }
  }

  final class TapFn[R, E, A](override val underlying: A => ZIO[R, E, Any]) extends ZIOFn1[A, ZIO[R, E, A]] {
    def apply(a: A): ZIO[R, E, A] =
      underlying(a).as(a)
  }

  final class MapFn[R, E, A, B](override val underlying: A => B) extends ZIOFn1[A, ZIO[R, E, B]] {
    def apply(a: A): ZIO[R, E, B] =
      new ZIO.Succeed(underlying(a))
  }

  final class ConstZIOFn[R, E, A, B](override val underlying: () => B) extends ZIOFn1[A, ZIO[R, E, B]] {
    def apply(a: A): ZIO[R, E, B] = {
      val _ = a
      new ZIO.Succeed(underlying())
    }
  }

  final class ConstFn[A, B](override val underlying: () => B) extends ZIOFn1[A, B] {
    def apply(a: A): B = {
      val _ = a
      underlying()
    }
  }

  final class BracketReleaseFn[R, E, A, B](override val underlying: A => URIO[R, Any])
      extends ZIOFn2[A, Exit[E, B], URIO[R, Any]] {
    override def apply(a: A, exit: Exit[E, B]): URIO[R, Any] = {
      val _ = exit
      underlying(a)
    }
  }

  final class SucceedFn[R, E, A](override val underlying: AnyRef) extends ZIOFn1[A, ZIO[R, E, A]] {
    def apply(a: A): ZIO[R, E, A] = new ZIO.Succeed(a)
  }

  final class MapErrorFn[R, E, E2, A](override val underlying: E => E2) extends ZIOFn1[Cause[E], ZIO[R, E2, Nothing]] {
    def apply(a: Cause[E]): ZIO[R, E2, Nothing] =
      ZIO.haltNow(a.map(underlying))
  }

  final class MapErrorCauseFn[R, E, E2, A](override val underlying: Cause[E] => Cause[E2])
      extends ZIOFn1[Cause[E], ZIO[R, E2, Nothing]] {
    def apply(a: Cause[E]): ZIO[R, E2, Nothing] =
      ZIO.haltNow(underlying(a))
  }

  final class FoldCauseMFailureFn[R, E, E2, A](override val underlying: E => ZIO[R, E2, A])
      extends ZIOFn1[Cause[E], ZIO[R, E2, A]] {
    def apply(c: Cause[E]): ZIO[R, E2, A] =
      c.failureOrCause.fold(underlying, ZIO.haltNow)
  }

  final class TapCauseRefailFn[R, E, E1 >: E, A](override val underlying: Cause[E] => ZIO[R, E1, Any])
      extends ZIOFn1[Cause[E], ZIO[R, E1, Nothing]] {
    def apply(c: Cause[E]): ZIO[R, E1, Nothing] =
      underlying(c) *> ZIO.haltNow(c)
  }

  final class TapErrorRefailFn[R, E, E1 >: E, A](override val underlying: E => ZIO[R, E1, Any])
      extends ZIOFn1[Cause[E], ZIO[R, E1, Nothing]] {
    def apply(c: Cause[E]): ZIO[R, E1, Nothing] =
      c.failureOrCause.fold(underlying(_) *> ZIO.haltNow(c), _ => ZIO.haltNow(c))
  }

  private[zio] object Tags {
    final val FlatMap                  = 0
    final val Succeed                  = 1
    final val EffectTotal              = 2
    final val Fail                     = 3
    final val Fold                     = 4
    final val InterruptStatus          = 5
    final val CheckInterrupt           = 6
    final val EffectPartial            = 7
    final val EffectAsync              = 8
    final val Fork                     = 9
    final val Descriptor               = 10
    final val Lock                     = 11
    final val Yield                    = 12
    final val Access                   = 13
    final val Provide                  = 14
    final val EffectSuspendPartialWith = 15
    final val FiberRefNew              = 16
    final val FiberRefModify           = 17
    final val Trace                    = 18
    final val TracingStatus            = 19
    final val CheckTracing             = 20
    final val EffectSuspendTotalWith   = 21
    final val RaceWith                 = 22
    final val Disown                   = 23
  }
  private[zio] final class FlatMap[R, E, A0, A](val zio: ZIO[R, E, A0], val k: A0 => ZIO[R, E, A])
      extends ZIO[R, E, A] {
    override def tag = Tags.FlatMap
  }

  private[zio] final class Succeed[A](val value: A) extends UIO[A] {
    override def tag = Tags.Succeed
  }

  private[zio] final class EffectTotal[A](val effect: () => A) extends UIO[A] {
    override def tag = Tags.EffectTotal
  }

  private[zio] final class EffectPartial[A](val effect: () => A) extends Task[A] {
    override def tag = Tags.EffectPartial
  }

  private[zio] final class EffectAsync[R, E, A](
    val register: (ZIO[R, E, A] => Unit) => Option[ZIO[R, E, A]],
    val blockingOn: List[Fiber.Id]
  ) extends ZIO[R, E, A] {
    override def tag = Tags.EffectAsync
  }

  private[zio] final class Fold[R, E, E2, A, B](
    val value: ZIO[R, E, A],
    val failure: Cause[E] => ZIO[R, E2, B],
    val success: A => ZIO[R, E2, B]
  ) extends ZIOFn1[A, ZIO[R, E2, B]]
      with ZIO[R, E2, B]
      with Function[A, ZIO[R, E2, B]] {

    override def tag = Tags.Fold

    override def underlying = success

    def apply(v: A): ZIO[R, E2, B] = success(v)
  }

  private[zio] final class Fork[R, E, A](val value: ZIO[R, E, A], val superviseMode: SuperviseMode)
      extends URIO[R, Fiber.Runtime[E, A]] {
    override def tag = Tags.Fork
  }

  private[zio] final class InterruptStatus[R, E, A](val zio: ZIO[R, E, A], val flag: _root_.zio.InterruptStatus)
      extends ZIO[R, E, A] {
    override def tag = Tags.InterruptStatus
  }

  private[zio] final class CheckInterrupt[R, E, A](val k: zio.InterruptStatus => ZIO[R, E, A]) extends ZIO[R, E, A] {
    override def tag = Tags.CheckInterrupt
  }

  private[zio] final class Fail[E, A](val fill: (() => ZTrace) => Cause[E]) extends IO[E, A] { self =>
    override def tag = Tags.Fail

    override def map[B](f: A => B): IO[E, B] =
      self.asInstanceOf[IO[E, B]]

    override def flatMap[R1 <: Any, E1 >: E, B](k: A => ZIO[R1, E1, B]): ZIO[R1, E1, B] =
      self.asInstanceOf[ZIO[R1, E1, B]]
  }

  private[zio] final class Descriptor[R, E, A](val k: Fiber.Descriptor => ZIO[R, E, A]) extends ZIO[R, E, A] {
    override def tag = Tags.Descriptor
  }

  private[zio] final class Lock[R, E, A](val executor: Executor, val zio: ZIO[R, E, A]) extends ZIO[R, E, A] {
    override def tag = Tags.Lock
  }

  private[zio] object Yield extends UIO[Unit] {
    override def tag = Tags.Yield
  }

  private[zio] final class Read[R, E, A](val k: R => ZIO[R, E, A]) extends ZIO[R, E, A] {
    override def tag = Tags.Access
  }

  private[zio] final class Provide[R, E, A](val r: R, val next: ZIO[R, E, A]) extends IO[E, A] {
    override def tag = Tags.Provide
  }

  private[zio] final class EffectSuspendPartialWith[R, A](val f: (Platform, Fiber.Id) => RIO[R, A]) extends RIO[R, A] {
    override def tag = Tags.EffectSuspendPartialWith
  }

  private[zio] final class EffectSuspendTotalWith[R, E, A](val f: (Platform, Fiber.Id) => ZIO[R, E, A])
      extends ZIO[R, E, A] {
    override def tag = Tags.EffectSuspendTotalWith
  }

  private[zio] final class FiberRefNew[A](val initialValue: A, val combine: (A, A) => A) extends UIO[FiberRef[A]] {
    override def tag = Tags.FiberRefNew
  }

  private[zio] final class FiberRefModify[A, B](val fiberRef: FiberRef[A], val f: A => (B, A)) extends UIO[B] {
    override def tag = Tags.FiberRefModify
  }

  private[zio] object Trace extends UIO[ZTrace] {
    override def tag = Tags.Trace
  }

  private[zio] final class TracingStatus[R, E, A](val zio: ZIO[R, E, A], val flag: TracingS) extends ZIO[R, E, A] {
    override def tag = Tags.TracingStatus
  }

  private[zio] final class CheckTracing[R, E, A](val k: TracingS => ZIO[R, E, A]) extends ZIO[R, E, A] {
    override def tag = Tags.CheckTracing
  }

  private[zio] final class RaceWith[R, EL, ER, E, A, B, C](
    val left: ZIO[R, EL, A],
    val right: ZIO[R, ER, B],
    val leftWins: (Exit[EL, A], Fiber[ER, B]) => ZIO[R, E, C],
    val rightWins: (Exit[ER, B], Fiber[EL, A]) => ZIO[R, E, C],
    val leftSuperviseMode: SuperviseMode,
    val rightSuperviseMode: SuperviseMode
  ) extends ZIO[R, E, C] {
    override def tag: Int = Tags.RaceWith
  }

  private[zio] final class Disown(val fiber: Fiber[Any, Any]) extends UIO[Boolean] {
    override def tag = Tags.Disown
  }

  private val debug = new java.util.concurrent.atomic.AtomicReference[Set[String]](Set())

  private[zio] def isDebug(name: String): Boolean = debug.get.contains(name)

  private[zio] def withDebug[R, E, A](name: String)(zio: => ZIO[R, E, A]): ZIO[R, E, A] = {
    val before = UIO(debug.updateAndGet(_ + name))
    val after  = UIO(debug.updateAndGet(_ - name))

    (before *> zio).ensuring(after)
  }

  private[zio] def dieNow(t: Throwable): UIO[Nothing] = die(t)

  private[zio] def doneNow[E, A](r: Exit[E, A]): IO[E, A] = r match {
    case Exit.Success(b)     => succeedNow(b)
    case Exit.Failure(cause) => haltNow(cause)
  }

  private[zio] def failNow[E](error: E): IO[E, Nothing] = fail(error)

  private[zio] def haltNow[E](cause: Cause[E]): IO[E, Nothing] = new ZIO.Fail(_ => cause)

  private[zio] def succeedNow[A](a: A): UIO[A] = new ZIO.Succeed(a)
}

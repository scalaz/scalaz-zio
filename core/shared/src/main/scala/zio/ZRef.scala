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

import com.github.ghik.silencer.silent
import zio.stm.STM

import java.util.concurrent.atomic.AtomicReference

/**
 * A `ZRef[RA, RB, EA, EB, A, B]` is a polymorphic, purely functional
 * description of a mutable reference. The fundamental operations of a `ZRef`
 * are `set` and `get`. `set` takes a value of type `A` and sets the reference
 * to a new value, requiring an environment of type `RA` and potentially
 * failing with an error of type `EA`. `get` gets the current value of the
 * reference and returns a value of type `B`, requiring an environment of type
 * `RB` and potentially failing with an error of type `EB`.
 *
 * When the error and value types of the `ZRef` are unified, that is, it is a
 * `ZRef[R, R, E, E, A, A]`, the `ZRef` also supports atomic `modify` and
 * `update` operations. All operations are guaranteed to be safe for concurrent
 * access.
 *
 * By default, `ZRef` is implemented in terms of compare and swap operations
 * for maximum performance and does not support performing effects within
 * update operations. If you need to perform effects within update operations
 * you can create a `ZRefM`, a specialized type of `ZRef` that supports
 * performing effects within update operations at some cost to performance. In
 * this case writes will semantically block other writers, while multiple
 * readers can read simultaneously.
 *
 * `ZRefM` also supports composing multiple `ZRefM` values together to form a
 * single `ZRefM` value that can be atomically updated using the `zip`
 * operator. In this case reads and writes will semantically block other
 * readers and writers.
 *
 * NOTE: While `ZRef` provides the functional equivalent of a mutable
 * reference, the value inside the `ZRef` should normally be immutable since
 * compare and swap operations are not safe for mutable values that do not
 * support concurrent access. If you do need to use a mutable value `ZRefM`
 * will guarantee that access to the value is properly synchronized.
 */
sealed abstract class ZRef[-RA, -RB, +EA, +EB, -A, +B] extends Serializable { self =>

  /**
   * Folds over the error and value types of the `ZRef`. This is a highly
   * polymorphic method that is capable of arbitrarily transforming the error
   * and value types of the `ZRef`. For most use cases one of the more specific
   * combinators implemented in terms of `fold` will be more ergonomic but this
   * method is extremely useful for implementing new combinators.
   */
  def fold[EC, ED, C, D](
    ea: EA => EC,
    eb: EB => ED,
    ca: C => Either[EC, A],
    bd: B => Either[ED, D]
  ): ZRef[RA, RB, EC, ED, C, D]

  /**
   * Folds over the error and value types of the `ZRef`, allowing access to
   * the state in transforming the `set` value. This is a more powerful version
   * of `fold` but requires unifying the error types.
   */
  def foldAll[EC, ED, C, D](
    ea: EA => EC,
    eb: EB => ED,
    ec: EB => EC,
    ca: C => B => Either[EC, A],
    bd: B => Either[ED, D]
  ): ZRef[RA with RB, RB, EC, ED, C, D]

  /**
   * Reads the value from the `ZRef`.
   */
  def get: ZIO[RB, EB, B]

  /**
   * Writes a new value to the `ZRef`, with a guarantee of immediate
   * consistency (at some cost to performance).
   */
  def set(a: A): ZIO[RA, EA, Unit]

  /**
   * Writes a new value to the `ZRef` without providing a guarantee of
   * immediate consistency.
   */
  def setAsync(a: A): ZIO[RA, EA, Unit]

  /**
   * Maps and filters the `get` value of the `ZRef` with the specified partial
   * function, returning a `ZRef` with a `get` value that succeeds with the
   * result of the partial function if it is defined or else fails with `None`.
   */
  def collect[C](pf: PartialFunction[B, C]): ZRef[RA, RB, EA, Option[EB], A, C] =
    fold(identity, Some(_), Right(_), pf.lift(_).toRight(None))

  /**
   * Transforms the `set` value of the `ZRef` with the specified function.
   */
  def contramap[C](f: C => A): ZRef[RA, RB, EA, EB, C, B] =
    contramapEither(c => Right(f(c)))

  /**
   * Transforms the `set` value of the `ZRef` with the specified fallible
   * function.
   */
  def contramapEither[EC >: EA, C](f: C => Either[EC, A]): ZRef[RA, RB, EC, EB, C, B] =
    dimapEither(f, Right(_))

  /**
   * Transforms both the `set` and `get` values of the `ZRef` with the
   * specified functions.
   */
  def dimap[C, D](f: C => A, g: B => D): ZRef[RA, RB, EA, EB, C, D] =
    dimapEither(c => Right(f(c)), b => Right(g(b)))

  /**
   * Transforms both the `set` and `get` values of the `ZRef` with the
   * specified fallible functions.
   */
  def dimapEither[EC >: EA, ED >: EB, C, D](
    f: C => Either[EC, A],
    g: B => Either[ED, D]
  ): ZRef[RA, RB, EC, ED, C, D] =
    fold(identity, identity, f, g)

  /**
   * Transforms both the `set` and `get` errors of the `ZRef` with the
   * specified functions.
   */
  def dimapError[EC, ED](f: EA => EC, g: EB => ED): ZRef[RA, RB, EC, ED, A, B] =
    fold(f, g, Right(_), Right(_))

  /**
   * Filters the `set` value of the `ZRef` with the specified predicate,
   * returning a `ZRef` with a `set` value that succeeds if the predicate is
   * satisfied or else fails with `None`.
   */
  def filterInput[A1 <: A](f: A1 => Boolean): ZRef[RA, RB, Option[EA], EB, A1, B] =
    fold(Some(_), identity, a => if (f(a)) Right(a) else Left(None), Right(_))

  /**
   * Filters the `get` value of the `ZRef` with the specified predicate,
   * returning a `ZRef` with a `get` value that succeeds if the predicate is
   * satisfied or else fails with `None`.
   */
  def filterOutput(f: B => Boolean): ZRef[RA, RB, EA, Option[EB], A, B] =
    fold(identity, Some(_), Right(_), b => if (f(b)) Right(b) else Left(None))

  /**
   * Transforms the `get` value of the `ZRef` with the specified function.
   */
  def map[C](f: B => C): ZRef[RA, RB, EA, EB, A, C] =
    mapEither(b => Right(f(b)))

  /**
   * Transforms the `get` value of the `ZRef` with the specified fallible
   * function.
   */
  def mapEither[EC >: EB, C](f: B => Either[EC, C]): ZRef[RA, RB, EA, EC, A, C] =
    dimapEither(Right(_), f)

  /**
   * Returns a read only view of the `ZRef`.
   */
  def readOnly: ZRef[RA, RB, EA, EB, Nothing, B] =
    self

  /**
   * Returns a write only view of the `ZRef`.
   */
  def writeOnly: ZRef[RA, RB, EA, Unit, A, Nothing] =
    fold(identity, _ => (), Right(_), _ => Left(()))
}

object ZRef extends Serializable {

  /**
   * Creates a new `ZRef` with the specified value.
   */
  def make[A](a: A): UIO[Ref[A]] =
    UIO.effectTotal(unsafeMake(a))

  private[zio] def unsafeMake[A](a: A): Ref.Atomic[A] =
    Atomic(new AtomicReference(a))

  /**
   * Creates a new managed `ZRef` with the specified value
   */
  def makeManaged[A](a: A): Managed[Nothing, Ref[A]] =
    make(a).toManaged_

  implicit class UnifiedSyntax[-R, +E, A](private val self: ZRef[R, R, E, E, A, A]) extends AnyVal {

    /**
     * Atomically writes the specified value to the `ZRef`, returning the value
     * immediately before modification.
     */
    def getAndSet(a: A): ZIO[R, E, A] =
      self match {
        case atomic: Atomic[A] => atomic.getAndSet(a)
        case derived           => derived.modify(v => (v, a))
      }

    /**
     * Atomically modifies the `ZRef` with the specified function, returning
     * the value immediately before modification.
     */
    def getAndUpdate(f: A => A): ZIO[R, E, A] =
      self match {
        case atomic: Atomic[A] => atomic.getAndUpdate(f)
        case derived           => derived.modify(v => (v, f(v)))
      }

    /**
     * Atomically modifies the `ZRef` with the specified partial function,
     * returning the value immediately before modification. If the function is
     * undefined on the current value it doesn't change it.
     */
    def getAndUpdateSome(pf: PartialFunction[A, A]): ZIO[R, E, A] =
      self match {
        case atomic: Atomic[A] => atomic.getAndUpdateSome(pf)
        case derived =>
          derived.modify { v =>
            val result = pf.applyOrElse[A, A](v, identity)
            (v, result)
          }
      }

    /**
     * Atomically modifies the `ZRef` with the specified function, which
     * computes a return value for the modification. This is a more powerful
     * version of `update`.
     */
    @silent("unreachable code")
    def modify[B](f: A => (B, A)): ZIO[R, E, B] =
      self match {
        case atomic: Atomic[A] => atomic.modify(f)
        case derived: Derived[E, E, A, A] =>
          derived.value.modify { s =>
            derived.getEither(s) match {
              case Left(e) => (Left(e), s)
              case Right(a1) => {
                val (b, a2) = f(a1)
                derived.setEither(a2) match {
                  case Left(e)  => (Left(e), s)
                  case Right(s) => (Right(b), s)
                }
              }
            }
          }.absolve
        case derivedAll: DerivedAll[E, E, A, A] =>
          derivedAll.value.modify { s =>
            derivedAll.getEither(s) match {
              case Left(e) => (Left(e), s)
              case Right(a1) => {
                val (b, a2) = f(a1)
                derivedAll.setEither(a2)(s) match {
                  case Left(e)  => (Left(e), s)
                  case Right(s) => (Right(b), s)
                }
              }
            }
          }.absolve
        case zRefM: ZRefM[R, R, E, E, A, A] =>
          zRefM.modifyM(a => ZIO.succeedNow(f(a)))
      }

    /**
     * Atomically modifies the `ZRef` with the specified partial function,
     * which computes a return value for the modification if the function is
     * defined on the current value otherwise it returns a default value. This
     * is a more powerful version of `updateSome`.
     */
    def modifySome[B](default: B)(pf: PartialFunction[A, (B, A)]): ZIO[R, E, B] =
      self match {
        case atomic: Atomic[A] => atomic.modifySome(default)(pf)
        case derived =>
          derived.modify(v => pf.applyOrElse[A, (B, A)](v, _ => (default, v)))
      }

    /**
     * Atomically modifies the `ZRef` with the specified function.
     */
    def update(f: A => A): ZIO[R, E, Unit] =
      self match {
        case atomic: Atomic[A] => atomic.update(f)
        case derived           => derived.modify(v => ((), f(v)))
      }

    /**
     * Atomically modifies the `ZRef` with the specified function and returns
     * the updated value.
     */
    def updateAndGet(f: A => A): ZIO[R, E, A] =
      self match {
        case atomic: Atomic[A] => atomic.updateAndGet(f)
        case derived =>
          derived.modify { v =>
            val result = f(v)
            (result, result)
          }
      }

    /**
     * Atomically modifies the `ZRef` with the specified partial function. If
     * the function is undefined on the current value it doesn't change it.
     */
    def updateSome(pf: PartialFunction[A, A]): ZIO[R, E, Unit] =
      self match {
        case atomic: Atomic[A] => atomic.updateSome(pf)
        case derived =>
          derived.modify { v =>
            val result = pf.applyOrElse[A, A](v, identity)
            ((), result)
          }
      }

    /**
     * Atomically modifies the `ZRef` with the specified partial function. If
     * the function is undefined on the current value it returns the old value
     * without changing it.
     */
    def updateSomeAndGet(pf: PartialFunction[A, A]): ZIO[R, E, A] =
      self match {
        case atomic: Atomic[A] => atomic.updateSomeAndGet(pf)
        case derived =>
          derived.modify { v =>
            val result = pf.applyOrElse[A, A](v, identity)
            (result, result)
          }
      }
  }

  /**
   * A `ZRefM[RA, RB, EA, EB, A, B]` is a polymorphic, purely functional
   * description of a mutable reference. The fundamental operations of a `ZRefM`
   * are `set` and `get`. `set` takes a value of type `A` and sets the reference
   * to a new value, requiring an environment of type `RA` and potentially
   * failing with an error of type `EA`. `get` gets the current value of the
   * reference and returns a value of type `B`, requiring an environment of type
   * `RB` and potentially failing with an error of type `EB`.
   *
   * When the error and value types of the `ZRefM` are unified, that is, it is a
   * `ZRefM[R, R, E, E, A, A]`, the `ZRefM` also supports atomic `modify` and
   * `update` operations.
   *
   * Unlike an ordinary `ZRef`, a `ZRefM` allows performing effects within update
   * operations, at some cost to performance. Writes will semantically block
   * other writers, while multiple readers can read simultaneously.
   *
   * `ZRefM` also supports composing multiple `ZRefM` values together to form a
   * single `ZRefM` value that can be atomically updated using the `zip`
   * operator. In this case reads and writes will semantically block other
   * readers and writers.
   */
  sealed abstract class ZRefM[-RA, -RB, +EA, +EB, -A, +B] extends ZRef[RA, RB, EA, EB, A, B] { self =>

    protected def semaphores: Set[Semaphore]

    protected def unsafeGet: ZIO[RB, EB, B]

    protected def unsafeSet(a: A): ZIO[RA, EA, Unit]

    protected def unsafeSetAsync(a: A): ZIO[RA, EA, Unit]

    /**
     * A symbolic alias for `zip`.
     */
    final def <*>[RA1 <: RA, RB1 <: RB, EA1 >: EA, EB1 >: EB, A2, B2](
      that: ZRefM[RA1, RB1, EA1, EB1, A2, B2]
    ): ZRef[RA1, RB1, EA1, EB1, (A, A2), (B, B2)] =
      self zip that

    /**
     * Maps and filters the `get` value of the `ZRefM` with the specified partial
     * function, returning a `ZRefM` with a `get` value that succeeds with the
     * result of the partial function if it is defined or else fails with `None`.
     */
    override final def collect[C](pf: PartialFunction[B, C]): ZRefM[RA, RB, EA, Option[EB], A, C] =
      collectM(pf.andThen(ZIO.succeedNow(_)))

    /**
     * Maps and filters the `get` value of the `ZRefM` with the specified
     * effectual partial function, returning a `ZRefM` with a `get` value that
     * succeeds with the result of the partial function if it is defined or else
     * fails with `None`.
     */
    final def collectM[RC <: RB, EC >: EB, C](
      pf: PartialFunction[B, ZIO[RC, EC, C]]
    ): ZRefM[RA, RC, EA, Option[EC], A, C] =
      foldM(
        identity,
        Some(_),
        ZIO.succeedNow,
        b => pf.andThen(_.asSomeError).applyOrElse[B, ZIO[RC, Option[EC], C]](b, _ => ZIO.fail(None))
      )

    /**
     * Transforms the `set` value of the `ZRefM` with the specified function.
     */
    override final def contramap[C](f: C => A): ZRefM[RA, RB, EA, EB, C, B] =
      contramapM(c => ZIO.succeedNow(f(c)))

    /**
     * Transforms the `set` value of the `ZRef` with the specified fallible
     * function.
     */
    override final def contramapEither[EC >: EA, C](f: C => Either[EC, A]): ZRefM[RA, RB, EC, EB, C, B] =
      dimapEither(f, Right(_))

    /**
     * Transforms the `set` value of the `ZRefM` with the specified effectual
     * function.
     */
    final def contramapM[RC <: RA, EC >: EA, C](f: C => ZIO[RC, EC, A]): ZRefM[RC, RB, EC, EB, C, B] =
      dimapM(f, ZIO.succeedNow)

    /**
     * Transforms both the `set` and `get` values of the `ZRefM` with the
     * specified functions.
     */
    override final def dimap[C, D](f: C => A, g: B => D): ZRefM[RA, RB, EA, EB, C, D] =
      dimapM(c => ZIO.succeedNow(f(c)), b => ZIO.succeedNow(g(b)))

    /**
     * Transforms both the `set` and `get` values of the `ZRef` with the
     * specified fallible functions.
     */
    override final def dimapEither[EC >: EA, ED >: EB, C, D](
      f: C => Either[EC, A],
      g: B => Either[ED, D]
    ): ZRefM[RA, RB, EC, ED, C, D] =
      fold(identity, identity, f, g)

    /**
     * Transforms both the `set` and `get` values of the `ZRefM` with the
     * specified effectual functions.
     */
    final def dimapM[RC <: RA, RD <: RB, EC >: EA, ED >: EB, C, D](
      f: C => ZIO[RC, EC, A],
      g: B => ZIO[RD, ED, D]
    ): ZRefM[RC, RD, EC, ED, C, D] =
      foldM(identity, identity, f, g)

    /**
     * Transforms both the `set` and `get` errors of the `ZRefM` with the
     * specified functions.
     */
    override final def dimapError[EC, ED](f: EA => EC, g: EB => ED): ZRefM[RA, RB, EC, ED, A, B] =
      fold(f, g, Right(_), Right(_))

    /**
     * Filters the `set` value of the `ZRefM` with the specified predicate,
     * returning a `ZRefM` with a `set` value that succeeds if the predicate is
     * satisfied or else fails with `None`.
     */
    override final def filterInput[A1 <: A](f: A1 => Boolean): ZRefM[RA, RB, Option[EA], EB, A1, B] =
      filterInputM(a => ZIO.succeedNow(f(a)))

    /**
     * Filters the `set` value of the `ZRefM` with the specified effectual
     * predicate, returning a `ZRefM` with a `set` value that succeeds if the
     * predicate is satisfied or else fails with `None`.
     */
    final def filterInputM[RC <: RA, EC >: EA, A1 <: A](
      f: A1 => ZIO[RC, EC, Boolean]
    ): ZRefM[RC, RB, Option[EC], EB, A1, B] =
      foldM(Some(_), identity, a => ZIO.ifM(f(a).asSomeError)(ZIO.succeedNow(a), ZIO.fail(None)), ZIO.succeedNow)

    /**
     * Filters the `get` value of the `ZRefM` with the specified predicate,
     * returning a `ZRefM` with a `get` value that succeeds if the predicate is
     * satisfied or else fails with `None`.
     */
    override final def filterOutput(f: B => Boolean): ZRefM[RA, RB, EA, Option[EB], A, B] =
      filterOutputM(a => ZIO.succeedNow(f(a)))

    /**
     * Filters the `get` value of the `ZRefM` with the specified effectual predicate,
     * returning a `ZRefM` with a `get` value that succeeds if the predicate is
     * satisfied or else fails with `None`.
     */
    final def filterOutputM[RC <: RB, EC >: EB](f: B => ZIO[RC, EC, Boolean]): ZRefM[RA, RC, EA, Option[EC], A, B] =
      foldM(identity, Some(_), ZIO.succeedNow, b => ZIO.ifM(f(b).asSomeError)(ZIO.succeedNow(b), ZIO.fail(None)))

    /**
     * Folds over the error and value types of the `ZRefM`.
     */
    final def fold[EC, ED, C, D](
      ea: EA => EC,
      eb: EB => ED,
      ca: C => Either[EC, A],
      bd: B => Either[ED, D]
    ): ZRefM[RA, RB, EC, ED, C, D] =
      foldM(ea, eb, c => ZIO.fromEither(ca(c)), b => ZIO.fromEither(bd(b)))

    /**
     * Folds over the error and value types of the `ZRefM`, allowing access to
     * the state in transforming the `set` value but requiring unifying the error
     * type.
     */
    final def foldAll[EC, ED, C, D](
      ea: EA => EC,
      eb: EB => ED,
      ec: EB => EC,
      ca: C => B => Either[EC, A],
      bd: B => Either[ED, D]
    ): ZRefM[RA with RB, RB, EC, ED, C, D] =
      foldAllM(ea, eb, ec, c => b => ZIO.fromEither(ca(c)(b)), b => ZIO.fromEither(bd(b)))

    /**
     * Folds over the error and value types of the `ZRefM`, allowing access to
     * the state in transforming the `set` value. This is a more powerful version
     * of `foldM` but requires unifying the environment and error types.
     */
    final def foldAllM[RC <: RA with RB, RD <: RB, EC, ED, C, D](
      ea: EA => EC,
      eb: EB => ED,
      ec: EB => EC,
      ca: C => B => ZIO[RC, EC, A],
      bd: B => ZIO[RD, ED, D]
    ): ZRefM[RC, RD, EC, ED, C, D] =
      new ZRefM[RC, RD, EC, ED, C, D] {
        def semaphores =
          self.semaphores
        def unsafeGet: ZIO[RD, ED, D] =
          self.get.foldM(e => ZIO.fail(eb(e)), bd)
        def unsafeSet(c: C): ZIO[RC, EC, Unit] =
          self.get.foldM(
            e => ZIO.fail(ec(e)),
            b => ca(c)(b).flatMap(a => self.unsafeSet(a).mapError(ea))
          )
        def unsafeSetAsync(c: C): ZIO[RC, EC, Unit] =
          self.get.foldM(
            e => ZIO.fail(ec(e)),
            b => ca(c)(b).flatMap(a => self.unsafeSetAsync(a).mapError(ea))
          )
      }

    /**
     * Folds over the error and value types of the `ZRefM`. This is a highly
     * polymorphic method that is capable of arbitrarily transforming the error
     * and value types of the `ZRefM`. For most use cases one of the more
     * specific combinators implemented in terms of `foldM` will be more
     * ergonomic but this method is extremely useful for implementing new
     * combinators.
     */
    final def foldM[RC <: RA, RD <: RB, EC, ED, C, D](
      ea: EA => EC,
      eb: EB => ED,
      ca: C => ZIO[RC, EC, A],
      bd: B => ZIO[RD, ED, D]
    ): ZRefM[RC, RD, EC, ED, C, D] =
      new ZRefM[RC, RD, EC, ED, C, D] {
        def semaphores: Set[Semaphore] =
          self.semaphores
        def unsafeGet: ZIO[RD, ED, D] =
          self.unsafeGet.foldM(e => ZIO.fail(eb(e)), bd)
        def unsafeSetAsync(c: C): ZIO[RC, EC, Unit] =
          ca(c).flatMap(self.unsafeSetAsync(_).mapError(ea))
        def unsafeSet(c: C): ZIO[RC, EC, Unit] =
          ca(c).flatMap(self.unsafeSet(_).mapError(ea))
      }

    /**
     * Reads the value from the `ZRef`.
     */
    final def get: ZIO[RB, EB, B] =
      if (semaphores.size == 1) unsafeGet else withPermit(unsafeGet)

    /**
     * Transforms the `get` value of the `ZRefM` with the specified function.
     */
    override final def map[C](f: B => C): ZRefM[RA, RB, EA, EB, A, C] =
      mapM(b => ZIO.succeedNow(f(b)))

    /**
     * Transforms the `get` value of the `ZRef` with the specified fallible
     * function.
     */
    override final def mapEither[EC >: EB, C](f: B => Either[EC, C]): ZRefM[RA, RB, EA, EC, A, C] =
      dimapEither(Right(_), f)

    /**
     * Transforms the `get` value of the `ZRefM` with the specified effectual
     * function.
     */
    final def mapM[RC <: RB, EC >: EB, C](f: B => ZIO[RC, EC, C]): ZRefM[RA, RC, EA, EC, A, C] =
      dimapM(ZIO.succeedNow, f)

    /**
     * Returns a read only view of the `ZRefM`.
     */
    override final def readOnly: ZRefM[RA, RB, EA, EB, Nothing, B] =
      self

    /**
     * Writes a new value to the `ZRef`, with a guarantee of immediate
     * consistency (at some cost to performance).
     */
    final def set(a: A): ZIO[RA, EA, Unit] =
      withPermit(unsafeSet(a))

    /**
     * Writes a new value to the `ZRef` without providing a guarantee of
     * immediate consistency.
     */
    final def setAsync(a: A): ZIO[RA, EA, Unit] =
      withPermit(unsafeSetAsync(a))

    /**
     * Performs the specified effect every time a value is written to this
     * `ZRefM`.
     */
    final def tapInput[RC <: RA, EC >: EA, A1 <: A](f: A1 => ZIO[RC, EC, Any]): ZRefM[RC, RB, EC, EB, A1, B] =
      contramapM(a => f(a).as(a))

    /**
     * Performs the specified effect very time a value is read from this
     * `ZRefM`.
     */
    final def tapOutput[RC <: RB, EC >: EB](f: B => ZIO[RC, EC, Any]): ZRefM[RA, RC, EA, EC, A, B] =
      mapM(b => f(b).as(b))

    /**
     * Returns a write only view of the `ZRefM`.
     */
    override final def writeOnly: ZRefM[RA, RB, EA, Unit, A, Nothing] =
      fold(identity, _ => (), Right(_), _ => Left(()))

    /**
     * Combines this `ZRefM` with the specified `ZRefM` to create a new
     * `ZRefM` with the `get` and `set` values of both. The new `ZRefM` value
     * supports atomically modifying both of the underlying `ZRefM` values.
     */
    final def zip[RA1 <: RA, RB1 <: RB, EA1 >: EA, EB1 >: EB, A2, B2](
      that: ZRefM[RA1, RB1, EA1, EB1, A2, B2]
    ): ZRef[RA1, RB1, EA1, EB1, (A, A2), (B, B2)] =
      new ZRefM[RA1, RB1, EA1, EB1, (A, A2), (B, B2)] {
        val semaphores: Set[Semaphore] =
          self.semaphores | that.semaphores
        def unsafeGet: ZIO[RB1, EB1, (B, B2)] =
          self.get <*> that.get
        def unsafeSetAsync(a: (A, A2)): ZIO[RA1, EA1, Unit] =
          self.unsafeSetAsync(a._1) *> that.unsafeSetAsync(a._2)
        def unsafeSet(a: (A, A2)): ZIO[RA1, EA1, Unit] =
          self.unsafeSet(a._1) *> that.unsafeSet(a._2)
      }

    private final def withPermit[R, E, A](zio: ZIO[R, E, A]): ZIO[R, E, A] =
      ZIO.uninterruptibleMask { restore =>
        restore(STM.foreach(semaphores)(_.acquire).commit) *>
          restore(zio).ensuring(STM.foreach(semaphores)(_.release).commit)
      }
  }

  object ZRefM {

    /**
     * Creates a new `RefM` and a `Dequeue` that will emit every change to the
     * `RefM`.
     */
    @deprecated("use SubscriptionRef", "2.0.0")
    def dequeueRef[A](a: A): UIO[(RefM[A], Dequeue[A])] =
      for {
        ref   <- make(a)
        queue <- Queue.unbounded[A]
      } yield (ref.tapInput(queue.offer), queue)

    /**
     * Creates a new `ZRefM` with the specified value.
     */
    def make[A](a: A): UIO[RefM[A]] =
      for {
        ref       <- Ref.make(a)
        semaphore <- Semaphore.make(1)
      } yield new RefM[A] {
        val semaphores: Set[Semaphore] =
          Set(semaphore)
        def unsafeGet: ZIO[Any, Nothing, A] =
          ref.get
        def unsafeSet(a: A): ZIO[Any, Nothing, Unit] =
          ref.set(a)
        def unsafeSetAsync(a: A): ZIO[Any, Nothing, Unit] =
          ref.setAsync(a)
      }

    /**
     * Creates a new `ZRefM` with the specified value in the context of a
     * `Managed.`
     */
    def makeManaged[A](a: A): UManaged[RefM[A]] =
      make(a).toManaged_

    implicit class UnifiedSyntax[-R, +E, A](private val self: ZRefM[R, R, E, E, A, A]) extends AnyVal {

      /**
       * Atomically modifies the `RefM` with the specified function, returning the
       * value immediately before modification.
       */
      def getAndUpdateM[R1 <: R, E1 >: E](f: A => ZIO[R1, E1, A]): ZIO[R1, E1, A] =
        modifyM(v => f(v).map(result => (v, result)))

      /**
       * Atomically modifies the `RefM` with the specified partial function,
       * returning the value immediately before modification.
       * If the function is undefined on the current value it doesn't change it.
       */
      def getAndUpdateSomeM[R1 <: R, E1 >: E](pf: PartialFunction[A, ZIO[R1, E1, A]]): ZIO[R1, E1, A] =
        modifyM(v => pf.applyOrElse[A, ZIO[R1, E1, A]](v, ZIO.succeedNow).map(result => (v, result)))

      /**
       * Atomically modifies the `RefM` with the specified function, which computes
       * a return value for the modification. This is a more powerful version of
       * `update`.
       */
      def modifyM[R1 <: R, E1 >: E, B](f: A => ZIO[R1, E1, (B, A)]): ZIO[R1, E1, B] =
        self.withPermit(self.unsafeGet.flatMap(f).flatMap { case (b, a) => self.unsafeSet(a).as(b) })

      /**
       * Atomically modifies the `RefM` with the specified function, which computes
       * a return value for the modification if the function is defined in the current value
       * otherwise it returns a default value.
       * This is a more powerful version of `updateSome`.
       */
      def modifySomeM[R1 <: R, E1 >: E, B](default: B)(pf: PartialFunction[A, ZIO[R1, E1, (B, A)]]): ZIO[R1, E1, B] =
        modifyM(v => pf.applyOrElse[A, ZIO[R1, E1, (B, A)]](v, _ => ZIO.succeedNow((default, v))))

      /**
       * Atomically modifies the `RefM` with the specified function.
       */
      def updateM[R1 <: R, E1 >: E](f: A => ZIO[R1, E1, A]): ZIO[R1, E1, Unit] =
        modifyM(v => f(v).map(result => ((), result)))

      /**
       * Atomically modifies the `RefM` with the specified function, returning the
       * value immediately after modification.
       */
      def updateAndGetM[R1 <: R, E1 >: E](f: A => ZIO[R1, E1, A]): ZIO[R1, E1, A] =
        modifyM(v => f(v).map(result => (result, result)))

      /**
       * Atomically modifies the `RefM` with the specified partial function.
       * If the function is undefined on the current value it doesn't change it.
       */
      def updateSomeM[R1 <: R, E1 >: E](pf: PartialFunction[A, ZIO[R1, E1, A]]): ZIO[R1, E1, Unit] =
        modifyM(v => pf.applyOrElse[A, ZIO[R1, E1, A]](v, ZIO.succeedNow).map(result => ((), result)))

      /**
       * Atomically modifies the `RefM` with the specified partial function.
       * If the function is undefined on the current value it returns the old value
       * without changing it.
       */
      def updateSomeAndGetM[R1 <: R, E1 >: E](pf: PartialFunction[A, ZIO[R1, E1, A]]): ZIO[R1, E1, A] =
        modifyM(v => pf.applyOrElse[A, ZIO[R1, E1, A]](v, ZIO.succeedNow).map(result => (result, result)))
    }
  }

  private[zio] final case class Atomic[A](value: AtomicReference[A]) extends Ref[A] { self =>

    def fold[EC, ED, C, D](
      ea: Nothing => EC,
      eb: Nothing => ED,
      ca: C => Either[EC, A],
      bd: A => Either[ED, D]
    ): ZRef[Any, Any, EC, ED, C, D] =
      new Derived[EC, ED, C, D] {
        type S = A
        def getEither(s: S): Either[ED, D] =
          bd(s)
        def setEither(c: C): Either[EC, S] =
          ca(c)
        val value: Atomic[S] =
          self
      }

    def foldAll[EC, ED, C, D](
      ea: Nothing => EC,
      eb: Nothing => ED,
      ec: Nothing => EC,
      ca: C => A => Either[EC, A],
      bd: A => Either[ED, D]
    ): ZRef[Any, Any, EC, ED, C, D] =
      new DerivedAll[EC, ED, C, D] {
        type S = A
        def getEither(s: S): Either[ED, D] =
          bd(s)
        def setEither(c: C)(s: S): Either[EC, S] =
          ca(c)(s)
        val value: Atomic[S] =
          self
      }

    def get: UIO[A] =
      ZIO.effectTotal(unsafeGet)

    def getAndSet(a: A): UIO[A] =
      ZIO.effectTotal(unsafeGetAndSet(a))

    def getAndUpdate(f: A => A): UIO[A] =
      ZIO.effectTotal(unsafeGetAndUpdate(f))

    def getAndUpdateSome(pf: PartialFunction[A, A]): UIO[A] =
      ZIO.effectTotal(unsafeGetAndUpdateSome(pf))

    def modify[B](f: A => (B, A)): UIO[B] =
      ZIO.effectTotal(unsafeModify(f))

    def modifySome[B](default: B)(pf: PartialFunction[A, (B, A)]): UIO[B] =
      ZIO.effectTotal(unsafeModifySome(default)(pf))

    def set(a: A): UIO[Unit] =
      ZIO.effectTotal(unsafeSet(a))

    def setAsync(a: A): UIO[Unit] =
      ZIO.effectTotal(unsafeSetAsync(a))

    override def toString: String =
      s"Ref(${value.get})"

    def unsafeGet: A =
      value.get

    def unsafeGetAndSet(a: A): A = {
      var loop       = true
      var current: A = null.asInstanceOf[A]
      while (loop) {
        current = value.get
        loop = !value.compareAndSet(current, a)
      }
      current
    }

    def unsafeGetAndUpdate(f: A => A): A = {
      var loop       = true
      var current: A = null.asInstanceOf[A]
      while (loop) {
        current = value.get
        val next = f(current)
        loop = !value.compareAndSet(current, next)
      }
      current
    }

    def unsafeGetAndUpdateSome(pf: PartialFunction[A, A]): A = {
      var loop       = true
      var current: A = null.asInstanceOf[A]
      while (loop) {
        current = value.get
        val next = pf.applyOrElse(current, (_: A) => current)
        loop = !value.compareAndSet(current, next)
      }
      current
    }

    def unsafeModify[B](f: A => (B, A)): B = {
      var loop = true
      var b: B = null.asInstanceOf[B]
      while (loop) {
        val current = value.get
        val tuple   = f(current)
        b = tuple._1
        loop = !value.compareAndSet(current, tuple._2)
      }
      b
    }

    def unsafeModifySome[B](default: B)(pf: PartialFunction[A, (B, A)]): B = {
      var loop = true
      var b: B = null.asInstanceOf[B]
      while (loop) {
        val current = value.get
        val tuple   = pf.applyOrElse(current, (_: A) => (default, current))
        b = tuple._1
        loop = !value.compareAndSet(current, tuple._2)
      }
      b
    }

    def unsafeSet(a: A): Unit =
      value.set(a)

    def unsafeSetAsync(a: A): Unit =
      value.lazySet(a)

    def unsafeUpdate(f: A => A): Unit = {
      var loop    = true
      var next: A = null.asInstanceOf[A]
      while (loop) {
        val current = value.get
        next = f(current)
        loop = !value.compareAndSet(current, next)
      }
      ()
    }

    def unsafeUpdateAndGet(f: A => A): A = {
      var loop    = true
      var next: A = null.asInstanceOf[A]
      while (loop) {
        val current = value.get
        next = f(current)
        loop = !value.compareAndSet(current, next)
      }
      next
    }

    def unsafeUpdateSome(pf: PartialFunction[A, A]): Unit = {
      var loop    = true
      var next: A = null.asInstanceOf[A]
      while (loop) {
        val current = value.get
        next = pf.applyOrElse(current, (_: A) => current)
        loop = !value.compareAndSet(current, next)
      }
      ()
    }

    def unsafeUpdateSomeAndGet(pf: PartialFunction[A, A]): A = {
      var loop    = true
      var next: A = null.asInstanceOf[A]
      while (loop) {
        val current = value.get
        next = pf.applyOrElse(current, (_: A) => current)
        loop = !value.compareAndSet(current, next)
      }
      next
    }

    def update(f: A => A): UIO[Unit] =
      ZIO.effectTotal(unsafeUpdate(f))

    def updateAndGet(f: A => A): UIO[A] =
      ZIO.effectTotal(unsafeUpdateAndGet(f))

    def updateSome(pf: PartialFunction[A, A]): UIO[Unit] =
      ZIO.effectTotal(unsafeUpdateSome(pf))

    def updateSomeAndGet(pf: PartialFunction[A, A]): UIO[A] =
      ZIO.effectTotal(unsafeUpdateSomeAndGet(pf))
  }

  private abstract class Derived[+EA, +EB, -A, +B] extends ZRef[Any, Any, EA, EB, A, B] { self =>
    type S

    def getEither(s: S): Either[EB, B]

    def setEither(a: A): Either[EA, S]

    val value: Atomic[S]

    final def fold[EC, ED, C, D](
      ea: EA => EC,
      eb: EB => ED,
      ca: C => Either[EC, A],
      bd: B => Either[ED, D]
    ): ZRef[Any, Any, EC, ED, C, D] =
      new Derived[EC, ED, C, D] {
        type S = self.S
        def getEither(s: S): Either[ED, D] =
          self.getEither(s).fold(e => Left(eb(e)), bd)
        def setEither(c: C): Either[EC, S] =
          ca(c).flatMap(a => self.setEither(a).fold(e => Left(ea(e)), Right(_)))
        val value: Atomic[S] =
          self.value
      }

    final def foldAll[EC, ED, C, D](
      ea: EA => EC,
      eb: EB => ED,
      ec: EB => EC,
      ca: C => B => Either[EC, A],
      bd: B => Either[ED, D]
    ): ZRef[Any, Any, EC, ED, C, D] =
      new DerivedAll[EC, ED, C, D] {
        type S = self.S
        def getEither(s: S): Either[ED, D] =
          self.getEither(s).fold(e => Left(eb(e)), bd)
        def setEither(c: C)(s: S): Either[EC, S] =
          self
            .getEither(s)
            .fold(e => Left(ec(e)), ca(c))
            .flatMap(a => self.setEither(a).fold(e => Left(ea(e)), Right(_)))
        val value: Atomic[S] =
          self.value
      }

    final def get: IO[EB, B] =
      value.get.flatMap(getEither(_).fold(ZIO.fail(_), ZIO.succeedNow))

    final def set(a: A): IO[EA, Unit] =
      setEither(a).fold(ZIO.fail(_), value.set)

    final def setAsync(a: A): IO[EA, Unit] =
      setEither(a).fold(ZIO.fail(_), value.setAsync)
  }

  private abstract class DerivedAll[+EA, +EB, -A, +B] extends ZRef[Any, Any, EA, EB, A, B] { self =>
    type S

    def getEither(s: S): Either[EB, B]

    def setEither(a: A)(s: S): Either[EA, S]

    val value: Atomic[S]

    final def fold[EC, ED, C, D](
      ea: EA => EC,
      eb: EB => ED,
      ca: C => Either[EC, A],
      bd: B => Either[ED, D]
    ): ZRef[Any, Any, EC, ED, C, D] =
      new DerivedAll[EC, ED, C, D] {
        type S = self.S
        def getEither(s: S): Either[ED, D] =
          self.getEither(s).fold(e => Left(eb(e)), bd)
        def setEither(c: C)(s: S): Either[EC, S] =
          ca(c).flatMap(a => self.setEither(a)(s).fold(e => Left(ea(e)), Right(_)))
        val value: Atomic[S] =
          self.value
      }

    final def foldAll[EC, ED, C, D](
      ea: EA => EC,
      eb: EB => ED,
      ec: EB => EC,
      ca: C => B => Either[EC, A],
      bd: B => Either[ED, D]
    ): ZRef[Any, Any, EC, ED, C, D] =
      new DerivedAll[EC, ED, C, D] {
        type S = self.S
        def getEither(s: S): Either[ED, D] =
          self.getEither(s).fold(e => Left(eb(e)), bd)
        def setEither(c: C)(s: S): Either[EC, S] =
          self
            .getEither(s)
            .fold(e => Left(ec(e)), ca(c))
            .flatMap(a => self.setEither(a)(s).fold(e => Left(ea(e)), Right(_)))
        val value: Atomic[S] =
          self.value
      }

    final def get: IO[EB, B] =
      value.get.flatMap(getEither(_).fold(ZIO.fail(_), ZIO.succeedNow))

    final def set(a: A): IO[EA, Unit] =
      value.modify { s =>
        setEither(a)(s) match {
          case Left(e)  => (Left(e), s)
          case Right(s) => (Right(()), s)
        }
      }.absolve

    final def setAsync(a: A): IO[EA, Unit] =
      value.modify { s =>
        setEither(a)(s) match {
          case Left(e)  => (Left(e), s)
          case Right(s) => (Right(()), s)
        }
      }.absolve
  }
}

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
import zio.UIO
import zio.stm.ZSTM.internal._

import java.util.concurrent.atomic.AtomicReference

/**
 * A `ZTRef[EA, EB, A, B]` is a polymorphic, purely functional description of a
 * mutable reference that can be modified as part of a transactional effect. The
 * fundamental operations of a `ZTRef` are `set` and `get`. `set` takes a value
 * of type `A` and transactionally sets the reference to a new value, potentially
 * failing with an error of type `EA`. `get` gets the current value of the reference
 * and returns a value of type `B`, potentially failing with an error of type `EB`.
 *
 * When the error and value types of the `ZTRef` are unified, that is, it is a
 * `ZTRef[E, E, A, A]`, the `ZTRef` also supports atomic `modify` and `update`
 * operations. All operations are guaranteed to be executed transactionally.
 *
 * NOTE: While `ZTRef` provides the transactional equivalent of a mutable reference,
 * the value inside the `ZTRef` should be immutable. For performance reasons `ZTRef`
 * is implemented in terms of compare and swap operations rather than synchronization.
 * These operations are not safe for mutable values that do not support concurrent
 * access.
 */
sealed abstract class ZTRef[+EA, +EB, -A, +B] extends Serializable { self =>

  protected def atomic: ZTRef.Atomic[_]

  /**
   * Retrieves the value of the `ZTRef`.
   */
  def get: STM[EB, B]

  /**
   * Sets the value of the `ZTRef`.
   */
  def set(a: A): STM[EA, Unit]

  /**
   * Folds over the error and value types of the `ZTRef`. This is a highly
   * polymorphic method that is capable of arbitrarily transforming the error
   * and value types of the `ZTRef`. For most use cases one of the more
   * specific combinators implemented in terms of `fold` will be more ergonomic
   * but this method is extremely useful for implementing new combinators.
   */
  def fold[EC, ED, C, D](
    ea: EA => EC,
    eb: EB => ED,
    ca: C => Either[EC, A],
    bd: B => Either[ED, D]
  ): ZTRef[EC, ED, C, D]

  /**
   * Folds over the error and value types of the `ZTRef`, allowing access to
   * the state in transforming the `set` value. This is a more powerful version
   * of `fold` but requires unifying the error types.
   */
  def foldAll[EC, ED, C, D](
    ea: EA => EC,
    eb: EB => ED,
    ec: EB => EC,
    ca: C => B => Either[EC, A],
    bd: B => Either[ED, D]
  ): ZTRef[EC, ED, C, D]

  /**
   * Maps and filters the `get` value of the `ZTRef` with the specified partial
   * function, returning a `ZTRef` with a `get` value that succeeds with the
   * result of the partial function if it is defined or else fails with `None`.
   */
  final def collect[C](pf: PartialFunction[B, C]): ZTRef[EA, Option[EB], A, C] =
    fold(identity, Some(_), Right(_), pf.lift(_).toRight(None))

  /**
   * Transforms the `set` value of the `ZTRef` with the specified function.
   */
  final def contramap[C](f: C => A): ZTRef[EA, EB, C, B] =
    contramapEither(c => Right(f(c)))

  /**
   * Transforms the `set` value of the `ZTRef` with the specified fallible
   * function.
   */
  final def contramapEither[EC >: EA, C](f: C => Either[EC, A]): ZTRef[EC, EB, C, B] =
    dimapEither(f, Right(_))

  /**
   * Transforms both the `set` and `get` values of the `ZTRef` with the
   * specified functions.
   */
  final def dimap[C, D](f: C => A, g: B => D): ZTRef[EA, EB, C, D] =
    dimapEither(c => Right(f(c)), b => Right(g(b)))

  /**
   * Transforms both the `set` and `get` values of the `ZTRef` with the
   * specified fallible functions.
   */
  final def dimapEither[EC >: EA, ED >: EB, C, D](f: C => Either[EC, A], g: B => Either[ED, D]): ZTRef[EC, ED, C, D] =
    fold(identity, identity, f, g)

  /**
   * Transforms both the `set` and `get` errors of the `ZTRef` with the
   * specified functions.
   */
  final def dimapError[EC, ED](f: EA => EC, g: EB => ED): ZTRef[EC, ED, A, B] =
    fold(f, g, Right(_), Right(_))

  /**
   * Filters the `set` value of the `ZTRef` with the specified predicate,
   * returning a `ZTRef` with a `set` value that succeeds if the predicate is
   * satisfied or else fails with `None`.
   */
  final def filterInput[A1 <: A](f: A1 => Boolean): ZTRef[Option[EA], EB, A1, B] =
    fold(Some(_), identity, a => if (f(a)) Right(a) else Left(None), Right(_))

  /**
   * Filters the `get` value of the `ZTRef` with the specified predicate,
   * returning a `ZTRef` with a `get` value that succeeds if the predicate is
   * satisfied or else fails with `None`.
   */
  final def filterOutput(f: B => Boolean): ZTRef[EA, Option[EB], A, B] =
    fold(identity, Some(_), Right(_), b => if (f(b)) Right(b) else Left(None))

  /**
   * Folds over the error and value types of the `ZTRef`, allowing access to
   * the state in transforming the `set` value. This is a more powerful version
   * of `fold` but requires unifying the error types.
   */
  def foldAllSTM[EC, ED, C, D](
    ea: EA => EC,
    eb: EB => ED,
    ec: EB => EC,
    ca: C => B => STM[EC, A],
    bd: B => STM[ED, D]
  ): ZTRef[EC, ED, C, D] =
    new ZTRef.ZTRefSTM[EC, ED, C, D] {
      def atomic: ZTRef.Atomic[_] =
        self.atomic
      def get: STM[ED, D] =
        self.get.foldSTM(e => STM.fail(eb(e)), bd)
      def set(c: C): STM[EC, Unit] =
        self.get.mapError(ec).flatMap(b => ca(c)(b)).flatMap(a => self.set(a).mapError(ea))
    }

  /**
   * Folds over the error and value types of the `ZTRef`. This is a highly
   * polymorphic method that is capable of arbitrarily transforming the error
   * and value types of the `ZTRef`. For most use cases one of the more
   * specific combinators implemented in terms of `fold` will be more ergonomic
   * but this method is extremely useful for implementing new combinators.
   */
  def foldSTM[EC, ED, C, D](
    ea: EA => EC,
    eb: EB => ED,
    ca: C => STM[EC, A],
    bd: B => STM[ED, D]
  ): ZTRef[EC, ED, C, D] =
    new ZTRef.ZTRefSTM[EC, ED, C, D] {
      def atomic: ZTRef.Atomic[_] =
        self.atomic
      def get: STM[ED, D] =
        self.get.foldSTM(e => STM.fail(eb(e)), bd)
      def set(c: C): STM[EC, Unit] =
        ca(c).flatMap(a => self.set(a).mapError(ea))
    }

  /**
   * Transforms the `get` value of the `ZTRef` with the specified function.
   */
  final def map[C](f: B => C): ZTRef[EA, EB, A, C] =
    mapEither(b => Right(f(b)))

  /**
   * Transforms the `get` value of the `ZTRef` with the specified fallible
   * function.
   */
  final def mapEither[EC >: EB, C](f: B => Either[EC, C]): ZTRef[EA, EC, A, C] =
    dimapEither(Right(_), f)

  /**
   * Returns a read only view of the `ZTRef`.
   */
  final def readOnly: ZTRef[EA, EB, Nothing, B] =
    self

  /**
   * Returns a write only view of the `ZTRef`.
   */
  final def writeOnly: ZTRef[EA, Unit, A, Nothing] =
    fold(identity, _ => (), Right(_), _ => Left(()))

  private[stm] def unsafeGet(journal: Journal): B =
    atomic.getOrMakeEntry(journal).unsafeGet

  private[stm] def unsafeSet(journal: Journal, a: A): Unit =
    atomic.getOrMakeEntry(journal).unsafeSet(a)
}

object ZTRef {

  private[stm] final class Atomic[A](
    @volatile private[stm] var versioned: Versioned[A],
    private[stm] val todo: AtomicReference[Map[TxnId, Todo]]
  ) extends ZTRef[Nothing, Nothing, A, A] { self =>
    def fold[EC, ED, C, D](
      ea: Nothing => EC,
      eb: Nothing => ED,
      ca: C => Either[EC, A],
      bd: A => Either[ED, D]
    ): ZTRef[EC, ED, C, D] =
      new Derived[EC, ED, C, D] {
        type S = A
        def getEither(s: S): Either[ED, D] = bd(s)
        def setEither(c: C): Either[EC, S] = ca(c)
        val value: Atomic[S]               = self
        val atomic: ZTRef.Atomic[_]        = self.atomic
      }

    def foldAll[EC, ED, C, D](
      ea: Nothing => EC,
      eb: Nothing => ED,
      ec: Nothing => EC,
      ca: C => A => Either[EC, A],
      bd: A => Either[ED, D]
    ): ZTRef[EC, ED, C, D] =
      new DerivedAll[EC, ED, C, D] {
        type S = A
        def getEither(s: S): Either[ED, D]       = bd(s)
        def setEither(c: C)(s: S): Either[EC, S] = ca(c)(s)
        val value: Atomic[S]                     = self
        val atomic: ZTRef.Atomic[_]              = self.atomic
      }

    def get: USTM[A] =
      ZSTM.Effect((journal, _, _) => getOrMakeEntry(journal).unsafeGet[A])

    def set(a: A): USTM[Unit] =
      ZSTM.Effect { (journal, _, _) =>
        val entry = getOrMakeEntry(journal)
        entry.unsafeSet(a)
        ()
      }

    /**
     * Sets the value of the `ZTRef` and returns the old value.
     */
    def getAndSet(a: A): USTM[A] =
      ZSTM.Effect { (journal, _, _) =>
        val entry    = getOrMakeEntry(journal)
        val oldValue = entry.unsafeGet[A]
        entry.unsafeSet(a)
        oldValue
      }

    /**
     * Updates the value of the variable and returns the old value.
     */
    def getAndUpdate(f: A => A): USTM[A] =
      ZSTM.Effect { (journal, _, _) =>
        val entry    = getOrMakeEntry(journal)
        val oldValue = entry.unsafeGet[A]
        entry.unsafeSet(f(oldValue))
        oldValue
      }

    /**
     * Updates some values of the variable but leaves others alone, returning the
     * old value.
     */
    def getAndUpdateSome(f: PartialFunction[A, A]): USTM[A] =
      getAndUpdate(f orElse { case a => a })

    /**
     * Updates the value of the variable, returning a function of the specified
     * value.
     */
    def modify[B](f: A => (B, A)): USTM[B] =
      ZSTM.Effect { (journal, _, _) =>
        val entry                = getOrMakeEntry(journal)
        val (retValue, newValue) = f(entry.unsafeGet[A])
        entry.unsafeSet(newValue)
        retValue
      }

    /**
     * Updates the value of the variable, returning a function of the specified
     * value.
     */
    def modifySome[B](default: B)(f: PartialFunction[A, (B, A)]): USTM[B] =
      modify(a => f.lift(a).getOrElse((default, a)))

    override def toString: String =
      s"ZTRef.Atomic(id = ${self.hashCode()}, versioned.value = ${versioned.value}, todo = ${todo.get})"

    /**
     * Updates the value of the variable.
     */
    def update(f: A => A): USTM[Unit] =
      ZSTM.Effect { (journal, _, _) =>
        val entry    = getOrMakeEntry(journal)
        val newValue = f(entry.unsafeGet[A])
        entry.unsafeSet(newValue)
        ()
      }

    /**
     * Updates the value of the variable and returns the new value.
     */
    def updateAndGet(f: A => A): USTM[A] =
      ZSTM.Effect { (journal, _, _) =>
        val entry    = getOrMakeEntry(journal)
        val newValue = f(entry.unsafeGet[A])
        entry.unsafeSet(newValue)
        newValue
      }

    /**
     * Updates some values of the variable but leaves others alone.
     */
    def updateSome(f: PartialFunction[A, A]): USTM[Unit] =
      update(f orElse { case a => a })

    /**
     * Updates some values of the variable but leaves others alone, returning the
     * new value.
     */
    def updateSomeAndGet(f: PartialFunction[A, A]): USTM[A] =
      updateAndGet(f orElse { case a => a })

    protected val atomic: Atomic[_] = self

    private[stm] def getOrMakeEntry(journal: Journal): Entry =
      if (journal.containsKey(self)) journal.get(self)
      else {
        val entry = Entry(self, false)
        journal.put(self, entry)
        entry
      }
  }

  private abstract class Derived[+EA, +EB, -A, +B] extends ZTRef[EA, EB, A, B] { self =>
    type S

    def getEither(s: S): Either[EB, B]

    def setEither(a: A): Either[EA, S]

    val value: Atomic[S]

    final def fold[EC, ED, C, D](
      ea: EA => EC,
      eb: EB => ED,
      ca: C => Either[EC, A],
      bd: B => Either[ED, D]
    ): ZTRef[EC, ED, C, D] =
      new Derived[EC, ED, C, D] {
        type S = self.S
        def getEither(s: S): Either[ED, D] =
          self.getEither(s).fold(e => Left(eb(e)), bd)
        def setEither(c: C): Either[EC, S] =
          ca(c).flatMap(a => self.setEither(a).fold(e => Left(ea(e)), Right(_)))
        val value: Atomic[S] =
          self.value
        val atomic: Atomic[_] =
          self.atomic
      }

    final def foldAll[EC, ED, C, D](
      ea: EA => EC,
      eb: EB => ED,
      ec: EB => EC,
      ca: C => B => Either[EC, A],
      bd: B => Either[ED, D]
    ): ZTRef[EC, ED, C, D] =
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
        val atomic: Atomic[_] =
          self.atomic
      }

    final def get: STM[EB, B] =
      value.get.flatMap(getEither(_).fold(STM.fail(_), STM.succeedNow))

    final def set(a: A): STM[EA, Unit] =
      setEither(a).fold(STM.fail(_), value.set)
  }

  private abstract class DerivedAll[+EA, +EB, -A, +B] extends ZTRef[EA, EB, A, B] { self =>
    type S

    def getEither(s: S): Either[EB, B]

    def setEither(a: A)(s: S): Either[EA, S]

    val value: Atomic[S]

    final def fold[EC, ED, C, D](
      ea: EA => EC,
      eb: EB => ED,
      ca: C => Either[EC, A],
      bd: B => Either[ED, D]
    ): ZTRef[EC, ED, C, D] =
      new DerivedAll[EC, ED, C, D] {
        type S = self.S
        def getEither(s: S): Either[ED, D] =
          self.getEither(s).fold(e => Left(eb(e)), bd)
        def setEither(c: C)(s: S): Either[EC, S] =
          ca(c).flatMap(a => self.setEither(a)(s).fold(e => Left(ea(e)), Right(_)))
        val value: Atomic[S] =
          self.value
        val atomic: Atomic[_] =
          self.atomic
      }

    final def foldAll[EC, ED, C, D](
      ea: EA => EC,
      eb: EB => ED,
      ec: EB => EC,
      ca: C => B => Either[EC, A],
      bd: B => Either[ED, D]
    ): ZTRef[EC, ED, C, D] =
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
        val atomic: Atomic[_] =
          self.atomic
      }

    final def get: STM[EB, B] =
      value.get.flatMap(getEither(_).fold(STM.fail(_), STM.succeedNow))

    final def set(a: A): STM[EA, Unit] =
      value.modify { s =>
        setEither(a)(s) match {
          case Left(e)  => (Left(e), s)
          case Right(s) => (Right(()), s)
        }
      }.absolve
  }

  private abstract class ZTRefSTM[+EA, +EB, -A, +B] extends ZTRef[EA, EB, A, B] {

    protected def atomic: Atomic[_]

    def get: STM[EB, B]

    def set(a: A): STM[EA, Unit]

    final def fold[EC, ED, C, D](
      ea: EA => EC,
      eb: EB => ED,
      ca: C => Either[EC, A],
      bd: B => Either[ED, D]
    ): ZTRef[EC, ED, C, D] =
      foldSTM(ea, eb, c => STM.fromEither(ca(c)), b => STM.fromEither(bd(b)))

    final def foldAll[EC, ED, C, D](
      ea: EA => EC,
      eb: EB => ED,
      ec: EB => EC,
      ca: C => (B => Either[EC, A]),
      bd: B => Either[ED, D]
    ): ZTRef[EC, ED, C, D] =
      foldAllSTM(ea, eb, ec, c => b => STM.fromEither(ca(c)(b)), b => STM.fromEither(bd(b)))
  }

  implicit class UnifiedSyntax[E, A](private val self: ETRef[E, A]) extends AnyVal {
    def getAndSet(a: A): STM[E, A] =
      self match {
        case atomic: Atomic[A] => atomic.getAndSet(a)
        case derived           => derived.modify((_, a))
      }

    def getAndUpdate(f: A => A): STM[E, A] =
      self match {
        case atomic: Atomic[A] => atomic.getAndUpdate(f)
        case derived           => derived.modify(v => (v, f(v)))
      }

    def getAndUpdateSome(pf: PartialFunction[A, A]): STM[E, A] =
      self match {
        case atomic: Atomic[A] => atomic.getAndUpdateSome(pf)
        case derived =>
          derived.modify { v =>
            val result = pf.applyOrElse[A, A](v, identity)
            (v, result)
          }
      }

    @silent("unreachable code")
    def modify[B](f: A => (B, A)): STM[E, B] =
      self match {
        case atomic: Atomic[A] => atomic.modify(f)
        case derived: Derived[E, E, A, A] =>
          derived.value.modify { s =>
            derived.getEither(s) match {
              case Left(e) => (Left(e), s)
              case Right(a1) =>
                val (b, a2) = f(a1)
                derived.setEither(a2) match {
                  case Left(e)  => (Left(e), s)
                  case Right(s) => (Right(b), s)
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
        case zTRefSTM: ZTRefSTM[E, E, A, A] =>
          zTRefSTM.get.flatMap(a => f(a) match { case (b, a) => zTRefSTM.set(a).as(b) })
      }

    def modifySome[B](default: B)(pf: PartialFunction[A, (B, A)]): STM[E, B] =
      self match {
        case atomic: Atomic[A] => atomic.modifySome(default)(pf)
        case derived =>
          derived.modify(v => pf.applyOrElse[A, (B, A)](v, _ => (default, v)))
      }

    def update(f: A => A): STM[E, Unit] =
      self match {
        case atomic: Atomic[A] => atomic.update(f)
        case derived           => derived.modify(v => ((), f(v)))
      }

    def updateAndGet(f: A => A): STM[E, A] =
      self match {
        case atomic: Atomic[A] => atomic.updateAndGet(f)
        case derived =>
          derived.modify { v =>
            val result = f(v)
            (result, result)
          }
      }

    def updateSome(pf: PartialFunction[A, A]): STM[E, Unit] =
      self match {
        case atomic: Atomic[A] => atomic.updateSome(pf)
        case derived =>
          derived.modify { v =>
            val result = pf.applyOrElse[A, A](v, identity)
            ((), result)
          }
      }

    def updateSomeAndGet(pf: PartialFunction[A, A]): STM[E, A] =
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
   * Makes a new `ZTRef` that is initialized to the specified value.
   */
  def make[A](a: => A): USTM[TRef[A]] =
    ZSTM.Effect { (journal, _, _) =>
      val value     = a
      val versioned = new Versioned(value)
      val todo      = new AtomicReference[Map[TxnId, Todo]](Map())
      val tref      = new Atomic(versioned, todo)
      journal.put(tref, Entry(tref, true))
      tref
    }

  /**
   * A convenience method that makes a `ZTRef` and immediately commits the
   * transaction to extract the value out.
   */
  def makeCommit[A](a: => A): UIO[TRef[A]] =
    STM.atomically(make(a))

  private[stm] def unsafeMake[A](a: A): TRef[A] = {
    val value     = a
    val versioned = new Versioned(value)
    val todo      = new AtomicReference[Map[TxnId, Todo]](Map())
    new Atomic(versioned, todo)
  }
}

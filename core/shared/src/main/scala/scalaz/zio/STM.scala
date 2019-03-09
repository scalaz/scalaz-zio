package scalaz.zio

import java.util.concurrent.atomic.AtomicLong

import scala.collection.mutable.{ Map => MutableMap }

/**
 * `STM[E, A]` represents a computation that can be performed transactional
 * resulting in a failure `E` or a value `A`.
 *
 * {{{
 * def transfer(receiver: TVar[Int],
 *              sender: TVar[Int], much: Int): UIO[Int] =
 *   STM.atomically {
 *     for {
 *       balance <- sender.get
 *       _       <- check(balance >= much)
 *       _       <- receiver.update(_ + much)
 *       _       <- sender.update(_ - much)
 *       newAmnt <- receiver.get
 *     } yield newAmnt
 *   }
 *
 *   val action: UIO[Int] =
 *     for {
 *       t <- STM.atomically(TVar.make(0).zip(TVar.make(20000)))
 *       (receiver, sender) = t
 *       balance <- transfer(receiver, sender, 1000)
 *     } yield balance
 * }}}
 */
final class STM[+E, +A] private (
  val run: STM.internal.Journal => Either[E, A]
) extends AnyVal { self =>

  /**
   * Maps from one error type to another.
   */
  final def mapError[E1](f: E => E1): STM[E1, A] =
    new STM(journal => self.run(journal) match {
      case Left(e) => Left(f(e))
      case r => r.asInstanceOf[Either[E1, A]]
    })

  /**
   * Maps the value produced by the computation.
   */
  final def map[B](f: A => B): STM[E, B] =
    new STM(journal => self.run(journal) match {
      case Right(a) => Right(f(a))
      case l => l.asInstanceOf[Either[E, B]]
    })

  /**
   * Feeds the value produced by this computation to the specified computation,
   * and then runs that computation as well to produce its failure or value.
   */
  final def flatMap[E1 >: E, B](f: A => STM[E1, B]): STM[E1, B] =
    new STM(journal =>
        self.run(journal) match {
          case Right(a) => f(a).run(journal)
          case l        => l.asInstanceOf[Either[E1, B]]
      }
    )

  /**
   * Tries this computation first, and if it fails, tries the other computation.
   */
  final def orElse[E2, A1 >: A](that: => STM[E2, A1]): STM[E2, A1] =
    self.either.flatMap(_.fold(_ => that, STM.succeed(_)))

  /**
   * Surfaces any errors at the value level, where they can be recovered from.
   */
  def either: STM[Nothing, Either[E, A]] =
    new STM(
      journal =>
        self.run(journal) match {
          case value => Right(value)
      }
    )

  /**
   * Sequentially zips this value with the specified one.
   */
  def zip[E1 >: E, B](that: => STM[E1, B]): STM[E1, (A, B)] =
    self.zipWith(that)((a, b) => a -> b)

  def zipWith[E1 >: E, B, C](that: => STM[E1, B])(f: (A, B) => C): STM[E1, C] =
    self.flatMap(a => that.map(b => f(a, b)))
}



object STM {

  private[STM] object internal {
    type Journal = 
      MutableMap[Long, STM.internal.Entry]

    final def rightUnit[A]: Either[A, Unit] = 
      _RightUnit.asInstanceOf[Either[A, Unit]]
     
    private[this] val _RightUnit: Either[Any, Unit] = Right(())

    final def freshIdentity(): Long = counter.incrementAndGet()

    private[this] val counter: AtomicLong = new AtomicLong()

    val semaphore = new java.util.concurrent.Semaphore(1)

    abstract class Entry {
      type A
      val tVar: TVar[A]
      var newValue: A
      val expectedVersion: Long

      final def unsafeSet(value: Any): Unit = 
        newValue = value.asInstanceOf[A]

      final def unsafeGet[B]: B = 
        newValue.asInstanceOf[B]

      /**
       * Determines if the entry is valid. That is, if the version of the 
       * `TVar` is equal to the expected version.
       */
      final def isValid: Boolean = tVar.version == expectedVersion

      /**
       * Commits the new value to the `TVar`.
       */
      final def commit(): Unit = {
        tVar.value   = newValue
        tVar.version = expectedVersion + 1
      }
    }

    object Entry {
      /**
       * Creates an entry for the journal, given the `TVar` being updated, the 
       * new value of the `TVar`, and the expected version of the `TVar`.
       */
      def apply[A0](tVar0: TVar[A0], newValue0: A0, expectedVersion0: Long): Entry = 
        new Entry {
          type A = A0
          val tVar            = tVar0
          var newValue        = newValue0
          val expectedVersion = expectedVersion0
        }
    }
  }

  import internal._

  /**
   * A variable that can be modified as part of a transactional computation.
   */
  class TVar[A] private (id: Long, @volatile var version: Long, @volatile var value: A) {
    self =>

    /**
     * Retrieves the value of the `TVar`.
     */
    final val get: STM[Nothing, A] =
      new STM(journal => {
        val entry = getOrMakeEntry(journal)

        Right(entry.unsafeGet[A])
      })

    /**
     * Sets the value of the `TVar`.
     */
    final def set(newValue: A): STM[Nothing, Unit] =
      new STM(journal => {
        val entry = getOrMakeEntry(journal)

        entry.unsafeSet(newValue)
        
        rightUnit
      })

    /**
     * Updates the value of the variable.
     */
    final def update(f: A => A): STM[Nothing, A] =
      new STM(journal => {
        val entry = getOrMakeEntry(journal)

        val newValue = f(entry.unsafeGet[A])

        entry.unsafeSet(newValue)
        
        Right(newValue)
      })

    private def getOrMakeEntry(journal: Journal): Entry = 
      if (journal.contains(id)) journal(id)
      else {
        val entry = Entry(self, value, version)
        journal.update(id, entry)
        entry
      }
  }

  object TVar {
    /**
     * Makes a new `TVar`.
     */
    final def make[A](initialValue: => A): STM[Nothing, TVar[A]] =
      new STM(journal => {
        val id = freshIdentity()

        val tVar = new TVar(id, 0, initialValue)

        journal.update(id, Entry(tVar, initialValue, 0))
        
        Right(tVar)
      })
  }

  /**
   * Returns an `STM` effect that succeeds with the specified value.
   */
  final def succeed[A](a: A): STM[Nothing, A] =
    new STM(_ => Right(a))

  /**
   * Returns an `STM` effect that succeeds with the specified (lazily 
   * evaluated) value.
   */
  final def succeedLazy[A](a: => A): STM[Nothing, A] =
    new STM(_ => Right(a))

  /**
   * Returns an `STM` effect that succeeds with `Unit`.
   */
  final val unit: STM[Nothing, Unit] = succeed(())

  /**
   * Atomically performs a batch of operations in a single transaction.
   */
  final def atomically[E, A](stm: STM[E, A]): IO[E, A] =
    IO.absolve(IO.effectTotal {
      import internal.semaphore

      var retry = true
      var value = null.asInstanceOf[Either[E, A]]

      while (retry) {
        val journal = MutableMap.empty[Long, Entry]
        
        value = stm.run(journal)

        try {
          semaphore.acquire()

          if (journal.values.forall(_.isValid)) {
            journal.values.foreach(_.commit())

            retry = false
          }
        } finally semaphore.release()
      }

      value
    })

  /**
   * Abort and retry the whole transaction when any of the underlying
   * variables have changed.
   */
  final val retry: STM[Nothing, Nothing] = ???

  /**
   * Checks the condition, and if it's true, returns unit, otherwise, retries.
   */
  final def check(p: Boolean): STM[Nothing, Unit] = 
    if (p) STM.unit else retry

  /**
   * Returns a value that models failure in the transaction.
   */
  final def fail[E](e: E): STM[E, Nothing] = new STM(_ => Left(e))
}
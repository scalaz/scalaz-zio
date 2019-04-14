/*
 * Copyright 2017-2019 John A. De Goes and the ZIO Contributors
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

package scalaz.zio.internal

import java.util.concurrent.atomic.{ AtomicLong, AtomicReference }

import scalaz.zio._

import scala.annotation.{ switch, tailrec }

/**
 * An implementation of Fiber that maintains context necessary for evaluation.
 */
private[zio] final class FiberContext[E, A](
  platform: Platform
) extends Fiber[E, A] {
  import java.util.{ Collections, Set }

  import FiberContext._
  import FiberState._

  // Accessed from multiple threads:
  private[this] val state = new AtomicReference[FiberState[E, A]](FiberState.Initial[E, A])

  // Accessed from within a single thread (not necessarily the same):
  @volatile private[this] var supervised  = List.empty[Set[FiberContext[_, _]]]
  @volatile private[this] var supervising = 0
  @volatile private[this] var locked      = List.empty[Executor]
  @volatile private[this] var environment = List[Any](())

  private[this] val interruptStatus = StackBool()
  private[this] val fiberId         = FiberContext.fiberCounter.getAndIncrement()
  private[this] val stack           = new Stack[Any => IO[Any, Any]]()

  final def runAsync(k: Callback[E, A]): Unit =
    register0(xx => k(Exit.flatten(xx))) match {
      case null =>
      case v    => k(v)
    }

  private object InterruptExit extends Function[Any, IO[E, Any]] {
    final def apply(v: Any): IO[E, Any] = {
      interruptStatus.popDrop(())

      ZIO.succeed(v)
    }
  }

  /**
   * Unwinds the stack, collecting all finalizers and coalescing them into a
   * `UIO` that produces an option of a cause of finalizer failures. If needed,
   * catch exceptions and push error handler on the stack.
   */
  final def unwindStack(): Unit = {
    var unwinding = true

    // Unwind the stack, looking for exception handlers and coalescing
    // finalizers.
    while (unwinding && !stack.isEmpty) {
      stack.pop() match {
        case InterruptExit => interruptStatus.popDrop(())

        case a: ZIO.Fold[_, _, _, _, _] if allowRecovery =>
          // Push error handler onto the stack and abort iteration:
          stack.push(a.err.asInstanceOf[Any => IO[Any, Any]])
          unwinding = false
        case _ =>
      }
    }
  }

  private[this] final def executor: Executor =
    locked.headOption.getOrElse(platform.executor)

  /**
   * The main interpreter loop for `IO` actions. For purely synchronous actions,
   * this will run to completion unless required to yield to other fibers.
   * For mixed actions, the loop will proceed no further than the first
   * asynchronous boundary.
   *
   * @param io0 The `IO` to evaluate on the fiber.
   */
  final def evaluateNow(io0: IO[E, _]): Unit = {
    // Do NOT accidentally capture any of local variables in a closure,
    // or Scala will wrap them in ObjectRef and performance will plummet.
    var curIo: IO[E, Any] = io0

    while (curIo ne null) {
      try {
        // Put the maximum operation count on the stack for fast access:
        val maxopcount = executor.yieldOpCount

        var opcount: Int = 0

        while (curIo ne null) {
          // Check to see if the fiber should continue executing or not:
          if (!shouldInterrupt) {
            // Fiber does not need to be interrupted, but might need to yield:
            if (opcount == maxopcount) {
              // Cannot capture `curIo` since it will be boxed into `ObjectRef`,
              // which destroys performance. So put `curIo` into a temp val:
              val tmpIo = curIo

              curIo = IO.yieldNow *> tmpIo
            } else {
              // Fiber is neither being interrupted nor needs to yield. Execute
              // the next instruction in the program:
              (curIo.tag: @switch) match {
                case ZIO.Tags.FlatMap =>
                  val io = curIo.asInstanceOf[ZIO.FlatMap[Any, E, Any, Any]]

                  val nested = io.zio

                  // A mini interpreter for the left side of FlatMap that evaluates
                  // anything that is 1-hop away. This eliminates heap usage for the
                  // happy path.
                  (nested.tag: @switch) match {
                    case ZIO.Tags.Succeed =>
                      val io2 = nested.asInstanceOf[ZIO.Succeed[Any]]

                      curIo = io.k(io2.value)

                    case ZIO.Tags.Effect =>
                      val io2 = nested.asInstanceOf[ZIO.Effect[Any]]

                      curIo = io.k(io2.effect(platform))

                    case ZIO.Tags.Descriptor =>
                      val value = getDescriptor

                      curIo = io.k(value)

                    case _ =>
                      // Fallback case. We couldn't evaluate the LHS so we have to
                      // use the stack:
                      curIo = nested

                      stack.push(io.k)
                  }

                case ZIO.Tags.Succeed =>
                  val io = curIo.asInstanceOf[ZIO.Succeed[Any]]

                  val value = io.value

                  curIo = nextInstr(value)

                case ZIO.Tags.Effect =>
                  val io = curIo.asInstanceOf[ZIO.Effect[Any]]

                  val value = io.effect(platform)

                  curIo = nextInstr(value)

                case ZIO.Tags.EffectAsync =>
                  val io = curIo.asInstanceOf[ZIO.EffectAsync[E, Any]]

                  // Enter suspended state:
                  curIo = if (enterAsync()) {
                    io.register(resumeAsync) match {
                      case Some(io) => if (exitAsync()) io else null
                      case None     => null
                    }
                  } else IO.interrupt

                case ZIO.Tags.Fold =>
                  val io = curIo.asInstanceOf[ZIO.Fold[Any, E, Any, Any, Any]]

                  curIo = io.value

                  stack.push(io)

                case ZIO.Tags.Fork =>
                  val io = curIo.asInstanceOf[ZIO.Fork[_, Any]]

                  val value: FiberContext[_, Any] = fork(io.value)

                  supervise(value)

                  curIo = nextInstr(value)

                case ZIO.Tags.InterruptStatus =>
                  val io = curIo.asInstanceOf[ZIO.InterruptStatus[Any, E, Any]]

                  curIo = changeInterrupt(io.zio, io.flag)

                case ZIO.Tags.CheckInterrupt =>
                  val io = curIo.asInstanceOf[ZIO.CheckInterrupt[Any, E, Any]]

                  curIo = io.f(interruptible)

                case ZIO.Tags.Supervised =>
                  val io = curIo.asInstanceOf[ZIO.Supervised[Any, E, Any]]

                  // TODO: Use bracket?
                  curIo = enterSupervision *> io.value.ensuring(exitSupervision)

                case ZIO.Tags.Fail =>
                  val io = curIo.asInstanceOf[ZIO.Fail[E, Any]]

                  unwindStack()

                  if (stack.isEmpty) {
                    // Error not caught, stack is empty:
                    curIo = null

                    done(Exit.halt(io.cause))
                  } else {
                    // Error caught, next continuation on the stack will deal
                    // with it, so we just have to compute it here:
                    curIo = nextInstr(io.cause)
                  }

                case ZIO.Tags.Descriptor =>
                  curIo = nextInstr(getDescriptor)

                case ZIO.Tags.Lock =>
                  val io = curIo.asInstanceOf[ZIO.Lock[Any, E, Any]]

                  // TODO: Use bracket?
                  curIo = (lock(io.executor) *> io.zio).ensuring(unlock)

                case ZIO.Tags.Yield =>
                  evaluateLater(IO.unit)

                  curIo = null

                case ZIO.Tags.Access =>
                  val io = curIo.asInstanceOf[ZIO.Read[Any, E, Any]]

                  curIo = io.k(environment.head)

                case ZIO.Tags.Provide =>
                  val io = curIo.asInstanceOf[ZIO.Provide[Any, E, Any]]

                  environment = io.r :: environment

                  curIo = io.next.ensuring(ZIO.effectTotal { environment = environment.drop(1) })
              }
            }
          } else {
            // Fiber was interrupted
            curIo = terminate(IO.interrupt)
          }

          opcount = opcount + 1
        }
      } catch {
        case _: InterruptedException =>
          Thread.interrupted
          curIo = terminate(IO.interrupt)

        // Catastrophic error handler. Any error thrown inside the interpreter is
        // either a bug in the interpreter or a bug in the user's code. Let the
        // fiber die but attempt finalization & report errors.
        case t: Throwable if (platform.nonFatal(t)) =>
          curIo = terminate(IO.die(t))
      }
    }
  }

  private[this] final def lock(executor: Executor): UIO[Unit] =
    IO.effectTotal { locked = executor :: locked } *> IO.yieldNow

  private[this] final def unlock: UIO[Unit] =
    IO.effectTotal { locked = locked.drop(1) } *> IO.yieldNow

  private[this] final def getDescriptor: Fiber.Descriptor =
    Fiber.Descriptor(fiberId, state.get.interrupted, interruptible, executor, getFibers)

  // We make a copy of the supervised fibers set as an array
  // to prevent mutations of the set from propagating to the caller.
  private[this] final def getFibers: UIO[IndexedSeq[Fiber[_, _]]] =
    UIO {
      supervised match {
        case set :: _ =>
          val arr = Array.ofDim[Fiber[_, _]](set.size)
          set.toArray[Fiber[_, _]](arr)
        case Nil => Array.empty[Fiber[_, _]]
      }
    }

  /**
   * Forks an `IO` with the specified failure handler.
   */
  final def fork[E, A](io: IO[E, A]): FiberContext[E, A] = {
    val context = new FiberContext[E, A](platform)

    platform.executor.submitOrThrow(() => context.evaluateNow(io))

    context
  }

  private[this] final def evaluateLater(io: IO[E, Any]): Unit =
    executor.submitOrThrow(() => evaluateNow(io))

  /**
   * Resumes an asynchronous computation.
   *
   * @param value The value produced by the asynchronous computation.
   */
  private[this] final val resumeAsync: IO[E, Any] => Unit =
    io => if (exitAsync()) evaluateLater(io)

  final def interrupt: UIO[Exit[E, A]] = IO.effectAsyncMaybe[Nothing, Exit[E, A]] { k =>
    kill0(x => k(IO.done(x)))
  }

  final def await: UIO[Exit[E, A]] = IO.effectAsyncMaybe[Nothing, Exit[E, A]] { k =>
    observe0(x => k(IO.done(x)))
  }

  final def poll: UIO[Option[Exit[E, A]]] = IO.effectTotal(poll0)

  private[this] final def enterSupervision: IO[E, Unit] = IO.effectTotal {
    supervising += 1

    def newWeakSet[A]: Set[A] = Collections.newSetFromMap[A](platform.newWeakHashMap[A, java.lang.Boolean]())

    val set = newWeakSet[FiberContext[_, _]]

    supervised = set :: supervised
  }

  private[this] final def supervise(child: FiberContext[_, _]): Unit =
    if (supervising > 0) {
      supervised match {
        case Nil =>
        case set :: _ =>
          set.add(child)

          ()
      }
    }

  @tailrec
  private[this] final def enterAsync(): Boolean = {
    val oldState = state.get

    oldState match {
      case Executing(interrupted, terminating, _, observers) =>
        val newState = Executing(interrupted, terminating, FiberStatus.Suspended, observers)

        if (!state.compareAndSet(oldState, newState)) enterAsync()
        else if (shouldInterrupt) {
          // Fiber interrupted, so go back into running state:
          exitAsync()
          false
        } else true

      case _ => false
    }
  }

  @tailrec
  private[this] final def exitAsync(): Boolean = {
    val oldState = state.get

    oldState match {
      case Executing(interrupted, terminating, FiberStatus.Suspended, observers) =>
        if (!state.compareAndSet(oldState, Executing(interrupted, terminating, FiberStatus.Running, observers)))
          exitAsync()
        else true

      case _ => false
    }
  }

  private[this] final def exitSupervision: UIO[_] =
    IO.effectTotal {
      supervising -= 1
      supervised = supervised drop 1
    }

  @inline
  private[this] final def interruptible: Boolean =
    interruptStatus.peekOrElse(true)

  @inline
  private[this] final def shouldInterrupt: Boolean =
    state.get.interrupted && interruptible && !state.get.terminating

  @inline
  private[this] final def allowRecovery: Boolean =
    !(state.get.interrupted && interruptible)

  @inline
  private[this] final def nextInstr(value: Any): IO[E, Any] =
    if (!stack.isEmpty) stack.pop()(value).asInstanceOf[IO[E, Any]]
    else {
      done(Exit.succeed(value.asInstanceOf[A]))

      null
    }

  private[this] final def changeInterrupt[E, A](io: IO[E, A], flag: Boolean): IO[E, A] = {
    interruptStatus.push(flag)
    stack.push(InterruptExit)
    io
  }

  @tailrec
  private[this] final def terminate(io: UIO[Nothing]): UIO[Nothing] = {

    val oldState = state.get
    oldState match {
      case Executing(interrupted, _, status, observers) =>
        if (!state.compareAndSet(oldState, Executing(interrupted, true, status, observers)))
          terminate(io)
        else io

      case _ => null
    }
  }

  @tailrec
  private[this] final def done(v: Exit[E, A]): Unit = {
    val oldState = state.get

    oldState match {
      case Executing(_, _, _, observers: List[Callback[Nothing, Exit[E, A]]]) => // TODO: Dotty doesn't infer this properly
        if (!state.compareAndSet(oldState, Done(v))) done(v)
        else {
          notifyObservers(v, observers)
          reportUnhandled(v)
        }

      case Done(_) => // Huh?
    }
  }

  private[this] final def reportUnhandled(v: Exit[E, A]): Unit = v match {
    case Exit.Failure(cause) => platform.reportFailure(cause)

    case _ =>
  }

  @tailrec
  private[this] final def kill0(
    k: Callback[Nothing, Exit[E, A]]
  ): Option[IO[Nothing, Exit[E, A]]] = {

    val oldState = state.get

    oldState match {
      case Executing(_, terminating, FiberStatus.Suspended, observers0) if interruptible =>
        val observers = k :: observers0

        if (!state.compareAndSet(oldState, Executing(true, terminating, FiberStatus.Running, observers))) kill0(k)
        else {
          evaluateLater(IO.interrupt)

          None
        }

      case Executing(_, terminating, status, observers0) =>
        val observers = k :: observers0

        if (!state.compareAndSet(oldState, Executing(true, terminating, status, observers))) kill0(k)
        else None

      case Done(e) => Some(IO.succeed(e))
    }
  }

  private[this] final def observe0(
    k: Callback[Nothing, Exit[E, A]]
  ): Option[IO[Nothing, Exit[E, A]]] =
    register0(k) match {
      case null => None
      case x    => Some(IO.succeed(x))
    }

  @tailrec
  private[this] final def register0(k: Callback[Nothing, Exit[E, A]]): Exit[E, A] = {
    val oldState = state.get

    oldState match {
      case Executing(interrupted, terminating, status, observers0) =>
        val observers = k :: observers0

        if (!state.compareAndSet(oldState, Executing(interrupted, terminating, status, observers))) register0(k)
        else null

      case Done(v) => v
    }
  }

  private[this] final def poll0: Option[Exit[E, A]] =
    state.get match {
      case Done(r) => Some(r)
      case _       => None
    }

  private[this] final def notifyObservers(
    v: Exit[E, A],
    observers: List[Callback[Nothing, Exit[E, A]]]
  ): Unit = {
    val result = Exit.succeed(v)

    // To preserve fair scheduling, we submit all resumptions on the thread
    // pool in order of their submission.
    observers.reverse.foreach(
      k =>
        platform.executor
          .submitOrThrow(() => k(result))
    )
  }
}
private[zio] object FiberContext {
  val fiberCounter = new AtomicLong(0)

  sealed trait FiberStatus extends Serializable with Product
  object FiberStatus {
    case object Running   extends FiberStatus
    case object Suspended extends FiberStatus
  }

  sealed trait FiberState[+E, +A] extends Serializable with Product {

    /** indicates if the fiber was interrupted */
    def interrupted: Boolean

    /** indicates if the fiber is terminating */
    def terminating: Boolean

  }
  object FiberState extends Serializable {
    final case class Executing[E, A](
      interrupted: Boolean,
      terminating: Boolean,
      status: FiberStatus,
      observers: List[Callback[Nothing, Exit[E, A]]]
    ) extends FiberState[E, A]
    final case class Done[E, A](value: Exit[E, A]) extends FiberState[E, A] {
      def interrupted: Boolean = value.interrupted
      def terminating: Boolean = false
    }

    def Initial[E, A] = Executing[E, A](false, false, FiberStatus.Running, Nil)
  }
}

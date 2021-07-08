/*
 * Copyright 2021 John A. De Goes and the ZIO Contributors
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

import zio.internal.{MutableConcurrentQueue, Platform}

import java.util.Set
import java.util.concurrent.atomic.AtomicBoolean

/**
 * A `ZHub[RA, RB, EA, EB, A, B]` is an asynchronous message hub. Publishers
 * can publish messages of type `A` to the hub and subscribers can subscribe to
 * take messages of type `B` from the hub. Publishing messages can require an
 * environment of type `RA` and fail with an error of type `EA`. Taking
 * messages can require an environment of type `RB` and fail with an error of
 * type `EB`.
 */
sealed abstract class ZHub[-RA, -RB, +EA, +EB, -A, +B] extends Serializable { self =>

  /**
   * Waits for the hub to be shut down.
   */
  def awaitShutdown: UIO[Unit]

  /**
   * The maximum capacity of the hub.
   */
  def capacity: Int

  /**
   * Checks whether the hub is shut down.
   */
  def isShutdown: UIO[Boolean]

  /**
   * Publishes a message to the hub, returning whether the message was
   * published to the hub.
   */
  def publish(a: A): ZIO[RA, EA, Boolean]

  /**
   * Publishes all of the specified messages to the hub, returning whether
   * they were published to the hub.
   */
  def publishAll(as: Iterable[A]): ZIO[RA, EA, Boolean]

  /**
   * Shuts down the hub.
   */
  def shutdown: UIO[Unit]

  /**
   * The current number of messages in the hub.
   */
  def size: UIO[Int]

  /**
   * Subscribes to receive messages from the hub. The resulting subscription
   * can be evaluated multiple times within the scope of the managed to take a
   * message from the hub each time.
   */
  def subscribe: ZManaged[Any, Nothing, ZDequeue[RB, EB, B]]

  /**
   * Transforms messages published to the hub using the specified function.
   */
  final def contramap[C](f: C => A): ZHub[RA, RB, EA, EB, C, B] =
    contramapM(c => ZIO.succeedNow(f(c)))

  /**
   * Transforms messages published to the hub using the specified effectual
   * function.
   */
  final def contramapM[RC <: RA, EC >: EA, C](f: C => ZIO[RC, EC, A]): ZHub[RC, RB, EC, EB, C, B] =
    dimapM(f, ZIO.succeedNow)

  /**
   * Transforms messages published to and taken from the hub using the
   * specified functions.
   */
  final def dimap[C, D](f: C => A, g: B => D): ZHub[RA, RB, EA, EB, C, D] =
    dimapM(c => ZIO.succeedNow(f(c)), b => ZIO.succeedNow(g(b)))

  /**
   * Transforms messages published to and taken from the hub using the
   * specified effectual functions.
   */
  final def dimapM[RC <: RA, RD <: RB, EC >: EA, ED >: EB, C, D](
    f: C => ZIO[RC, EC, A],
    g: B => ZIO[RD, ED, D]
  ): ZHub[RC, RD, EC, ED, C, D] =
    new ZHub[RC, RD, EC, ED, C, D] {
      def awaitShutdown: UIO[Unit] =
        self.awaitShutdown
      def capacity: Int =
        self.capacity
      def isShutdown: UIO[Boolean] =
        self.isShutdown
      def publish(c: C): ZIO[RC, EC, Boolean] =
        f(c).flatMap(self.publish)
      def publishAll(cs: Iterable[C]): ZIO[RC, EC, Boolean] =
        ZIO.foreach(cs)(f).flatMap(self.publishAll)
      def shutdown: UIO[Unit] =
        self.shutdown
      def size: UIO[Int] =
        self.size
      def subscribe: ZManaged[Any, Nothing, ZDequeue[RD, ED, D]] =
        self.subscribe.map(_.mapM(g))
    }

  /**
   * Filters messages published to the hub using the specified function.
   */
  final def filterInput[A1 <: A](f: A1 => Boolean): ZHub[RA, RB, EA, EB, A1, B] =
    filterInputM(a => ZIO.succeedNow(f(a)))

  /**
   * Filters messages published to the hub using the specified effectual
   * function.
   */
  final def filterInputM[RA1 <: RA, EA1 >: EA, A1 <: A](
    f: A1 => ZIO[RA1, EA1, Boolean]
  ): ZHub[RA1, RB, EA1, EB, A1, B] =
    new ZHub[RA1, RB, EA1, EB, A1, B] {
      def awaitShutdown: UIO[Unit] =
        self.awaitShutdown
      def capacity: Int =
        self.capacity
      def isShutdown: UIO[Boolean] =
        self.isShutdown
      def publish(a: A1): ZIO[RA1, EA1, Boolean] =
        f(a).flatMap(b => if (b) self.publish(a) else ZIO.succeedNow(false))
      def publishAll(as: Iterable[A1]): ZIO[RA1, EA1, Boolean] =
        ZIO.filter(as)(f).flatMap(as => if (as.nonEmpty) self.publishAll(as) else ZIO.succeedNow(false))
      def shutdown: UIO[Unit] =
        self.shutdown
      def size: UIO[Int] =
        self.size
      def subscribe: ZManaged[Any, Nothing, ZDequeue[RB, EB, B]] =
        self.subscribe
    }

  /**
   * Filters messages taken from the hub using the specified function.
   */
  final def filterOutput(f: B => Boolean): ZHub[RA, RB, EA, EB, A, B] =
    filterOutputM(b => ZIO.succeedNow(f(b)))

  /**
   * Filters messages taken from the hub using the specified effectual
   * function.
   */
  final def filterOutputM[RB1 <: RB, EB1 >: EB](
    f: B => ZIO[RB1, EB1, Boolean]
  ): ZHub[RA, RB1, EA, EB1, A, B] =
    new ZHub[RA, RB1, EA, EB1, A, B] {
      def awaitShutdown: UIO[Unit] =
        self.awaitShutdown
      def capacity: Int =
        self.capacity
      def isShutdown: UIO[Boolean] =
        self.isShutdown
      def publish(a: A): ZIO[RA, EA, Boolean] =
        self.publish(a)
      def publishAll(as: Iterable[A]): ZIO[RA, EA, Boolean] =
        self.publishAll(as)
      def shutdown: UIO[Unit] =
        self.shutdown
      def size: UIO[Int] =
        self.size
      def subscribe: ZManaged[Any, Nothing, ZDequeue[RB1, EB1, B]] =
        self.subscribe.map(_.filterOutputM(f))
    }

  /**
   * Transforms messages taken from the hub using the specified function.
   */
  final def map[C](f: B => C): ZHub[RA, RB, EA, EB, A, C] =
    mapM(b => ZIO.succeedNow(f(b)))

  /**
   * Transforms messages taken from the hub using the specified effectual
   * function.
   */
  final def mapM[RC <: RB, EC >: EB, C](f: B => ZIO[RC, EC, C]): ZHub[RA, RC, EA, EC, A, C] =
    dimapM(ZIO.succeedNow, f)

  /**
   * Views the hub as a queue that can only be written to.
   */
  final def toQueue: ZEnqueue[RA, EA, A] =
    new ZEnqueue[RA, EA, A] {
      def awaitShutdown: UIO[Unit] =
        self.awaitShutdown
      def capacity: Int =
        self.capacity
      def isShutdown: UIO[Boolean] =
        self.isShutdown
      def offer(a: A): ZIO[RA, EA, Boolean] =
        self.publish(a)
      def offerAll(as: Iterable[A]): ZIO[RA, EA, Boolean] =
        self.publishAll(as)
      def shutdown: UIO[Unit] =
        self.shutdown
      def size: UIO[Int] =
        self.size
      def take: ZIO[Nothing, Any, Any] =
        ZIO.unit
      def takeAll: ZIO[Nothing, Any, List[Any]] =
        ZIO.succeedNow(List.empty)
      def takeUpTo(max: Int): ZIO[Nothing, Any, List[Any]] =
        ZIO.succeedNow(List.empty)
    }
}

object ZHub {

  /**
   * Creates a bounded hub with the back pressure strategy. The hub will retain
   * messages until they have been taken by all subscribers, applying back
   * pressure to publishers if the hub is at capacity.
   *
   * For best performance use capacities that are powers of two.
   */
  def bounded[A](requestedCapacity: Int): UIO[Hub[A]] =
    ZIO.effectTotal(internal.Hub.bounded[A](requestedCapacity)).flatMap(makeHub(_, Strategy.BackPressure()))

  /**
   * Creates a bounded hub with the dropping strategy. The hub will drop new
   * messages if the hub is at capacity.
   *
   * For best performance use capacities that are powers of two.
   */
  def dropping[A](requestedCapacity: Int): UIO[Hub[A]] =
    ZIO.effectTotal(internal.Hub.bounded[A](requestedCapacity)).flatMap(makeHub(_, Strategy.Dropping()))

  /**
   * Creates a bounded hub with the sliding strategy. The hub will add new
   * messages and drop old messages if the hub is at capacity.
   *
   * For best performance use capacities that are powers of two.
   */
  def sliding[A](requestedCapacity: Int): UIO[Hub[A]] =
    ZIO.effectTotal(internal.Hub.bounded[A](requestedCapacity)).flatMap(makeHub(_, Strategy.Sliding()))

  /**
   * Creates an unbounded hub.
   */
  def unbounded[A]: UIO[Hub[A]] =
    ZIO.effectTotal(internal.Hub.unbounded[A]).flatMap(makeHub(_, Strategy.Dropping()))

  /**
   * Creates a hub with the specified strategy.
   */
  private def makeHub[A](hub: internal.Hub[A], strategy: Strategy[A]): UIO[Hub[A]] =
    ZManaged.ReleaseMap.make.flatMap { releaseMap =>
      Promise.make[Nothing, Unit].map { promise =>
        unsafeMakeHub(
          hub,
          Platform.newConcurrentSet[(internal.Hub.Subscription[A], MutableConcurrentQueue[Promise[Nothing, A]])](),
          releaseMap,
          promise,
          new AtomicBoolean(false),
          strategy
        )
      }
    }

  /**
   * Unsafely creates a hub with the specified strategy.
   */
  private def unsafeMakeHub[A](
    hub: internal.Hub[A],
    subscribers: Set[(internal.Hub.Subscription[A], MutableConcurrentQueue[Promise[Nothing, A]])],
    releaseMap: ZManaged.ReleaseMap,
    shutdownHook: Promise[Nothing, Unit],
    shutdownFlag: AtomicBoolean,
    strategy: Strategy[A]
  ): Hub[A] =
    new Hub[A] {
      val awaitShutdown: UIO[Unit] =
        shutdownHook.await
      val capacity: Int =
        hub.capacity
      val isShutdown: UIO[Boolean] =
        ZIO.effectTotal(shutdownFlag.get)
      def publish(a: A): UIO[Boolean] =
        ZIO.effectSuspendTotal {
          if (shutdownFlag.get) ZIO.interrupt
          else if (hub.publish(a)) {
            strategy.unsafeCompleteSubscribers(hub, subscribers)
            ZIO.succeedNow(true)
          } else {
            strategy.handleSurplus(hub, subscribers, Chunk(a), shutdownFlag)
          }
        }
      def publishAll(as: Iterable[A]): UIO[Boolean] =
        ZIO.effectSuspendTotal {
          if (shutdownFlag.get) ZIO.interrupt
          else {
            val surplus = unsafePublishAll(hub, as)
            strategy.unsafeCompleteSubscribers(hub, subscribers)
            if (surplus.isEmpty) ZIO.succeedNow(true)
            else strategy.handleSurplus(hub, subscribers, surplus, shutdownFlag)
          }
        }
      val shutdown: UIO[Unit] =
        ZIO.effectSuspendTotalWith { (_, fiberId) =>
          shutdownFlag.set(true)
          ZIO.whenM(shutdownHook.succeed(())) {
            releaseMap.releaseAll(Exit.interrupt(fiberId), ExecutionStrategy.Parallel) *> strategy.shutdown
          }
        }.uninterruptible
      val size: UIO[Int] =
        ZIO.effectSuspendTotal {
          if (shutdownFlag.get) ZIO.interrupt
          else ZIO.succeedNow(hub.size())
        }
      val subscribe: ZManaged[Any, Nothing, Dequeue[A]] =
        for {
          dequeue <- makeSubscription(hub, subscribers, strategy).toManaged_
          _       <- ZManaged.makeExit(releaseMap.add(_ => dequeue.shutdown))((finalizer, exit) => finalizer(exit))
        } yield dequeue
    }

  /**
   * Creates a subscription with the specified strategy.
   */
  private def makeSubscription[A](
    hub: internal.Hub[A],
    subscribers: Set[(internal.Hub.Subscription[A], MutableConcurrentQueue[Promise[Nothing, A]])],
    strategy: Strategy[A]
  ): UIO[Dequeue[A]] =
    Promise.make[Nothing, Unit].map { promise =>
      unsafeMakeSubscription(
        hub,
        subscribers,
        hub.subscribe(),
        MutableConcurrentQueue.unbounded[Promise[Nothing, A]],
        promise,
        new AtomicBoolean(false),
        strategy
      )
    }

  /**
   * Unsafely creates a subscription with the specified strategy.
   */
  private def unsafeMakeSubscription[A](
    hub: internal.Hub[A],
    subscribers: Set[(internal.Hub.Subscription[A], MutableConcurrentQueue[Promise[Nothing, A]])],
    subscription: internal.Hub.Subscription[A],
    pollers: MutableConcurrentQueue[Promise[Nothing, A]],
    shutdownHook: Promise[Nothing, Unit],
    shutdownFlag: AtomicBoolean,
    strategy: Strategy[A]
  ): Dequeue[A] =
    new Dequeue[A] { self =>
      val awaitShutdown: UIO[Unit] =
        shutdownHook.await
      val capacity: Int =
        hub.capacity
      val isShutdown: UIO[Boolean] =
        ZIO.effectTotal(shutdownFlag.get)
      def offer(a: Nothing): ZIO[Nothing, Any, Boolean] =
        ZIO.succeedNow(false)
      def offerAll(as: Iterable[Nothing]): ZIO[Nothing, Any, Boolean] =
        ZIO.succeedNow(false)
      val shutdown: UIO[Unit] =
        ZIO.effectSuspendTotalWith { (_, fiberId) =>
          shutdownFlag.set(true)
          ZIO.whenM(shutdownHook.succeed(())) {
            ZIO.foreachPar(unsafePollAll(pollers))(_.interruptAs(fiberId)) *>
              ZIO.effectTotal(subscription.unsubscribe())
          }
        }.uninterruptible
      val size: UIO[Int] =
        ZIO.effectSuspendTotal {
          if (shutdownFlag.get) ZIO.interrupt
          else ZIO.succeedNow(subscription.size())
        }
      val take: UIO[A] =
        ZIO.effectSuspendTotalWith { (_, fiberId) =>
          if (shutdownFlag.get) ZIO.interrupt
          else {
            val empty   = null.asInstanceOf[A]
            val message = if (pollers.isEmpty()) subscription.poll(empty) else empty
            message match {
              case null =>
                val promise = Promise.unsafeMake[Nothing, A](fiberId)
                ZIO.effectSuspendTotal {
                  pollers.offer(promise)
                  subscribers.add(subscription -> pollers)
                  strategy.unsafeCompletePollers(hub, subscribers, subscription, pollers)
                  if (shutdownFlag.get) ZIO.interrupt else promise.await
                }.onInterrupt(ZIO.effectTotal(unsafeRemove(pollers, promise)))
              case a =>
                strategy.unsafeOnHubEmptySpace(hub, subscribers)
                ZIO.succeedNow(a)
            }
          }
        }
      val takeAll: ZIO[Any, Nothing, List[A]] =
        ZIO.effectSuspendTotal {
          if (shutdownFlag.get) ZIO.interrupt
          else {
            val as = if (pollers.isEmpty()) unsafePollAll(subscription).toList else List.empty
            strategy.unsafeOnHubEmptySpace(hub, subscribers)
            ZIO.succeedNow(as)
          }
        }
      def takeUpTo(max: Int): ZIO[Any, Nothing, List[A]] =
        ZIO.effectSuspendTotal {
          if (shutdownFlag.get) ZIO.interrupt
          else {
            val as = if (pollers.isEmpty()) unsafePollN(subscription, max).toList else List.empty
            strategy.unsafeOnHubEmptySpace(hub, subscribers)
            ZIO.succeedNow(as)
          }
        }
    }

  /**
   * A `Strategy[A]` describes the protocol for how publishers and subscribers
   * will communicate with each other through the hub.
   */
  private sealed abstract class Strategy[A] {

    /**
     * Describes how publishers should signal to subscribers that they are
     * waiting for space to become available in the hub.
     */
    def handleSurplus(
      hub: internal.Hub[A],
      subscribers: Set[(internal.Hub.Subscription[A], MutableConcurrentQueue[Promise[Nothing, A]])],
      as: Iterable[A],
      isShutdown: AtomicBoolean
    ): UIO[Boolean]

    /**
     * Describes any finalization logic associated with this strategy.
     */
    def shutdown: UIO[Unit]

    /**
     * Describes how subscribers should signal to publishers waiting for space
     * to become available in the hub that space may be available.
     */
    def unsafeOnHubEmptySpace(
      hub: internal.Hub[A],
      subscribers: Set[(internal.Hub.Subscription[A], MutableConcurrentQueue[Promise[Nothing, A]])]
    ): Unit

    /**
     * Describes how subscribers waiting for additional values from the hub
     * should take those values and signal to publishers that they are no
     * longer waiting for additional values.
     */
    final def unsafeCompletePollers(
      hub: internal.Hub[A],
      subscribers: Set[(internal.Hub.Subscription[A], MutableConcurrentQueue[Promise[Nothing, A]])],
      subscription: internal.Hub.Subscription[A],
      pollers: MutableConcurrentQueue[Promise[Nothing, A]]
    ): Unit = {
      var keepPolling = true
      val nullPoller  = null.asInstanceOf[Promise[Nothing, A]]
      val empty       = null.asInstanceOf[A]

      while (keepPolling && !subscription.isEmpty()) {
        val poller = pollers.poll(nullPoller)
        if (poller eq nullPoller) {
          subscribers.remove(subscription -> pollers)
          if (pollers.isEmpty()) keepPolling = false
          else subscribers.add(subscription -> pollers)
        } else {
          subscription.poll(empty) match {
            case null =>
              unsafeOfferAll(pollers, poller +: unsafePollAll(pollers))
            case a =>
              unsafeCompletePromise(poller, a)
              unsafeOnHubEmptySpace(hub, subscribers)
          }
        }
      }
    }

    /**
     * Describes how publishers should signal to subscribers waiting for
     * additional values from the hub that new values are available.
     */
    final def unsafeCompleteSubscribers(
      hub: internal.Hub[A],
      subscribers: Set[(internal.Hub.Subscription[A], MutableConcurrentQueue[Promise[Nothing, A]])]
    ): Unit = {
      val iterator = subscribers.iterator
      while (iterator.hasNext) {
        val (subscription, pollers) = iterator.next()
        unsafeCompletePollers(hub, subscribers, subscription, pollers)
      }
    }
  }

  private object Strategy {

    /**
     * A strategy that applies back pressure to publishers when the hub is at
     * capacity. This guarantees that all subscribers will receive all messages
     * published to the hub while they are subscribed. However, it creates the
     * risk that a slow subscriber will slow down the rate at which messages
     * are published and received by other subscribers.
     */
    final case class BackPressure[A]() extends Strategy[A] {
      val publishers: MutableConcurrentQueue[(A, Promise[Nothing, Boolean], Boolean)] =
        MutableConcurrentQueue.unbounded[(A, Promise[Nothing, Boolean], Boolean)]

      def handleSurplus(
        hub: internal.Hub[A],
        subscribers: Set[(internal.Hub.Subscription[A], MutableConcurrentQueue[Promise[Nothing, A]])],
        as: Iterable[A],
        isShutDown: AtomicBoolean
      ): UIO[Boolean] =
        ZIO.effectSuspendTotalWith { (_, fiberId) =>
          val promise = Promise.unsafeMake[Nothing, Boolean](fiberId)
          ZIO.effectSuspendTotal {
            unsafeOffer(as, promise)
            unsafeOnHubEmptySpace(hub, subscribers)
            unsafeCompleteSubscribers(hub, subscribers)
            if (isShutDown.get) ZIO.interrupt else promise.await
          }.onInterrupt(ZIO.effectTotal(unsafeRemove(promise)))
        }

      def shutdown: UIO[Unit] =
        for {
          fiberId    <- ZIO.fiberId
          publishers <- ZIO.effectTotal(unsafePollAll(publishers))
          _ <- ZIO.foreachPar_(publishers) { case (_, promise, last) =>
                 if (last) promise.interruptAs(fiberId) else ZIO.unit
               }
        } yield ()

      def unsafeOnHubEmptySpace(
        hub: internal.Hub[A],
        subscribers: Set[(internal.Hub.Subscription[A], MutableConcurrentQueue[Promise[Nothing, A]])]
      ): Unit = {
        val empty       = null.asInstanceOf[(A, Promise[Nothing, Boolean], Boolean)]
        var keepPolling = true

        while (keepPolling && !hub.isFull()) {
          val publisher = publishers.poll(empty)
          if (publisher eq null) keepPolling = false
          else {
            val published = hub.publish(publisher._1)
            if (published && publisher._3) {
              unsafeCompletePromise(publisher._2, true)
            } else if (!published) {
              unsafeOfferAll(publishers, publisher +: unsafePollAll(publishers))
            }
            unsafeCompleteSubscribers(hub, subscribers)
          }
        }

      }

      private def unsafeOffer(as: Iterable[A], promise: Promise[Nothing, Boolean]): Unit =
        if (as.nonEmpty) {
          val iterator = as.iterator
          var a        = iterator.next()
          while (iterator.hasNext) {
            publishers.offer((a, promise, false))
            a = iterator.next()
          }
          publishers.offer((a, promise, true))
          ()
        }

      private def unsafeRemove(promise: Promise[Nothing, Boolean]): Unit = {
        unsafeOfferAll(publishers, unsafePollAll(publishers).filterNot(_._2 == promise))
        ()
      }
    }

    /**
     * A strategy that drops new messages when the hub is at capacity. This
     * guarantees that a slow subscriber will not slow down the rate at which
     * messages are published. However, it creates the risk that a slow
     * subscriber will slow down the rate at which messages are received by
     * other subscribers and that subscribers may not receive all messages
     * published to the hub while they are subscribed.
     */
    final case class Dropping[A]() extends Strategy[A] {

      def handleSurplus(
        hub: internal.Hub[A],
        subscribers: Set[(internal.Hub.Subscription[A], MutableConcurrentQueue[Promise[Nothing, A]])],
        as: Iterable[A],
        isShutdown: AtomicBoolean
      ): UIO[Boolean] =
        ZIO.succeedNow(false)

      def shutdown: UIO[Unit] =
        ZIO.unit

      def unsafeOnHubEmptySpace(
        hub: internal.Hub[A],
        subscribers: Set[(internal.Hub.Subscription[A], MutableConcurrentQueue[Promise[Nothing, A]])]
      ): Unit =
        ()
    }

    /**
     * A strategy that adds new messages and drops old messages when the hub is
     * at capacity. This guarantees that a slow subscriber will not slow down
     * the rate at which messages are published and received by other
     * subscribers. However, it creates the risk that a slow subscriber will
     * not receive some messages published to the hub while it is subscribed.
     */
    final case class Sliding[A]() extends Strategy[A] {

      def handleSurplus(
        hub: internal.Hub[A],
        subscribers: Set[(internal.Hub.Subscription[A], MutableConcurrentQueue[Promise[Nothing, A]])],
        as: Iterable[A],
        isShutdown: AtomicBoolean
      ): UIO[Boolean] = {
        def unsafeSlidingPublish(as: Iterable[A]): Unit =
          if (as.nonEmpty && hub.capacity > 0) {
            val iterator = as.iterator
            var a        = iterator.next()
            var loop     = true
            while (loop) {
              hub.slide()
              val published = hub.publish(a)
              if (published && iterator.hasNext) {
                a = iterator.next()
              } else if (published && !iterator.hasNext) {
                loop = false
              }
            }
          }

        ZIO.effectTotal {
          unsafeSlidingPublish(as)
          unsafeCompleteSubscribers(hub, subscribers)
          true
        }
      }

      def shutdown: UIO[Unit] =
        ZIO.unit

      def unsafeOnHubEmptySpace(
        hub: internal.Hub[A],
        subscribers: Set[(internal.Hub.Subscription[A], MutableConcurrentQueue[Promise[Nothing, A]])]
      ): Unit =
        ()
    }
  }

  /**
   * Unsafely completes a promise with the specified value.
   */
  private def unsafeCompletePromise[A](promise: Promise[Nothing, A], a: A): Unit =
    promise.unsafeDone(ZIO.succeedNow(a))

  /**
   * Unsafely offers the specified values to a queue.
   */
  private def unsafeOfferAll[A](queue: MutableConcurrentQueue[A], as: Iterable[A]): Chunk[A] =
    queue.offerAll(as)

  /**
   * Unsafely polls all values from a queue.
   */
  private def unsafePollAll[A](queue: MutableConcurrentQueue[A]): Chunk[A] =
    queue.pollUpTo(Int.MaxValue)

  /**
   * Unsafely polls all values from a subscription.
   */
  private def unsafePollAll[A](subscription: internal.Hub.Subscription[A]): Chunk[A] =
    subscription.pollUpTo(Int.MaxValue)

  /**
   * Unsafely polls the specified number of values from a subscription.
   */
  private def unsafePollN[A](subscription: internal.Hub.Subscription[A], max: Int): Chunk[A] =
    subscription.pollUpTo(max)

  /**
   * Unsafely publishes the specified values to a hub.
   */
  private def unsafePublishAll[A](hub: internal.Hub[A], as: Iterable[A]): Chunk[A] =
    hub.publishAll(as)

  /**
   * Unsafely removes the specified item from a queue.
   */
  private def unsafeRemove[A](queue: MutableConcurrentQueue[A], a: A): Unit = {
    unsafeOfferAll(queue, unsafePollAll(queue).filterNot(_ == a))
    ()
  }
}

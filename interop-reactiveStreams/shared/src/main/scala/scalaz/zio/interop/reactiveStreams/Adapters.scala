package scalaz.zio.interop.reactiveStreams

import org.reactivestreams.{ Publisher, Subscriber }
import scalaz.zio._
import scalaz.zio.interop.reactiveStreams.SubscriberHelpers._
import scalaz.zio.stream.{ Sink, Stream, Take }

object Adapters {

  /**
   * Create a `Subscriber` from a `Sink`. Returns a subscriber producing to the Sink and a `Task` of the value
   * produced by the Sink or any error either produced by the Sink or signaled to the subscriber.
   */
  def sinkToSubscriber[R, E <: Throwable, A1, A, B](
    sink: Sink[R, E, A1, A, B],
    bufferSize: Int = 10
  ): ZIO[R, Nothing, (Subscriber[A], Task[B])] =
    for {
      runtime    <- ZIO.runtime[Any]
      q          <- Queue.bounded[Take[Throwable, A]](bufferSize + 1)
      subscriber = new QueueSubscriber[A](runtime, q)
      fiber <- untakeQ(q)
                .tap(_ => subscriber.signalDemand)
                .run(sink)
                .fork
    } yield (subscriber, fiber.join)

  /**
   * Create a `Publisher` from a `Stream`.
   */
  def streamToPublisher[R, E <: Throwable, A](stream: Stream[R, E, A]): ZIO[R, Nothing, Publisher[A]] =
    ZIO.runtime.map { runtime => (subscriber: Subscriber[_ >: A]) =>
      if (subscriber == null) {
        throw new NullPointerException("Subscriber must not be null.")
      } else {
        runtime.unsafeRunAsync_(
          for {
            demand <- Queue.unbounded[Long]
            _      <- UIO(subscriber.onSubscribe(createSubscription(subscriber, demand, runtime)))
            _ <- stream
                  .run(demandUnfoldSink(subscriber, demand))
                  .catchAll(e => UIO(subscriber.onError(e)))
                  .fork
          } yield ()
        )
      }
    }

  /**
   * Create a `Sink` from a `Subscriber`. The Sink will never fail.
   *
   * A `Sink` cannot signal `Stream` failure to the subscriber, as this requires a side channel from the `Stream`.
   * For this reason, using this adapter with a `Stream` that may fail will most likely leak resources and lead to
   * unexpected behavior. For Streams that can fail use `subscriberToSinkWithError`.
   */
  def subscriberToSink[A](subscriber: Subscriber[A]): UIO[Sink[Any, Nothing, Unit, A, Unit]] =
    for {
      runtime      <- ZIO.runtime[Any]
      demand       <- Queue.unbounded[Long]
      subscription = createSubscription(subscriber, demand, runtime)
      _            <- UIO(subscriber.onSubscribe(subscription))
    } yield demandUnfoldSink(subscriber, demand)

  /**
   * Create a `Sink` from a `Subscriber`. Errors need to be transported via the returned future:
   *
   * ```
   * val subscriber: Subscriber[A] = ???
   * val stream: Stream[R, Throwable, A] = ???
   * for {
   *   sinkError     <- subscriber.toSinkWithError[Throwable]
   *   (error, sink) = sinkError
   *   _             <- stream.catchAll(t => error.fail(t)).fork
   * } yield sink
   * ```
   */
  def subscriberToSinkWithError[E <: Throwable, A](
    subscriber: Subscriber[A]
  ): UIO[(Promise[E, Unit], Sink[Any, Nothing, Unit, A, Unit])] =
    for {
      runtime      <- ZIO.runtime[Any]
      demand       <- Queue.unbounded[Long]
      error        <- Promise.make[E, Unit]
      subscription = createSubscription(subscriber, demand, runtime)
      _            <- UIO(subscriber.onSubscribe(subscription))
      _            <- error.await.catchAll(t => UIO(subscriber.onError(t)) *> demand.shutdown).fork
    } yield (error, demandUnfoldSink(subscriber, demand))

  /**
   * Create a `Stream` from a `Publisher`.
   */
  def publisherToStream[A](publisher: Publisher[A], bufferSize: Int): UIO[Stream[Any, Throwable, A]] =
    for {
      runtime    <- ZIO.runtime[Any]
      q          <- Queue.bounded[Take[Throwable, A]](bufferSize + 1)
      subscriber = new QueueSubscriber[A](runtime, q)
      _          <- UIO(publisher.subscribe(subscriber))
    } yield untakeQ(q).tap(_ => subscriber.signalDemand)

  private def untakeQ[R, E, A](q: Queue[Take[E, A]]): Stream[R, E, A] =
    Stream.fromQueue(q).unTake ++ Stream.fromEffect(q.shutdown).drain

}

package zio.stream.experimental

import zio._
import zio.internal.{Executor, SingleThreadedRingBuffer, UniqueKey}
import zio.stm._
import zio.stream.experimental.ZStream.{DebounceState, HandoffSignal}
import zio.stream.experimental.internal.Utils.zipChunks
import zio.stream.internal.{ZInputStream, ZReader}

import java.util.concurrent.atomic.{AtomicBoolean, AtomicReference}
import scala.reflect.ClassTag

class ZStream[-R, +E, +A](val channel: ZChannel[R, Any, Any, Any, E, Chunk[A], Any]) { self =>

  import ZStream.TerminationStrategy

  /**
   * Syntax for adding aspects.
   */
  final def @@[LowerR <: UpperR, UpperR <: R, LowerE >: E, UpperE >: LowerE, LowerA >: A, UpperA >: LowerA](
    aspect: ZStreamAspect[LowerR, UpperR, LowerE, UpperE, LowerA, UpperA]
  ): ZStream[UpperR, LowerE, LowerA] =
    aspect(self)

  /**
   * Symbolic alias for [[ZStream#cross]].
   */
  final def <*>[R1 <: R, E1 >: E, A2](that: ZStream[R1, E1, A2]): ZStream[R1, E1, (A, A2)] =
    self cross that

  /**
   * Symbolic alias for [[ZStream#crossLeft]].
   */
  final def <*[R1 <: R, E1 >: E, A2](that: ZStream[R1, E1, A2]): ZStream[R1, E1, A] =
    self crossLeft that

  /**
   * Symbolic alias for [[ZStream#crossRight]].
   */
  final def *>[R1 <: R, E1 >: E, A2](that: ZStream[R1, E1, A2]): ZStream[R1, E1, A2] =
    self crossRight that

  /**
   * Symbolic alias for [[ZStream#zip]].
   */
  final def <&>[R1 <: R, E1 >: E, A2](that: ZStream[R1, E1, A2]): ZStream[R1, E1, (A, A2)] =
    self zip that

  /**
   * Symbolic alias for [[ZStream#zipLeft]].
   */
  final def <&[R1 <: R, E1 >: E, A2](that: ZStream[R1, E1, A2]): ZStream[R1, E1, A] =
    self zipLeft that

  /**
   * Symbolic alias for [[ZStream#zipRight]].
   */
  final def &>[R1 <: R, E1 >: E, A2](that: ZStream[R1, E1, A2]): ZStream[R1, E1, A2] =
    self zipRight that

  /**
   * Symbolic alias for [[ZStream#flatMap]].
   */
  def >>=[R1 <: R, E1 >: E, A2](f0: A => ZStream[R1, E1, A2]): ZStream[R1, E1, A2] = flatMap(f0)

  // /**
  //  * Symbolic alias for [[ZStream#transduce]].
  //  */
  // def >>>[R1 <: R, E1 >: E, A2 >: A, A3](transducer: ZTransducer[R1, E1, A2, A3]) =
  //   transduce(transducer)

  /**
   * Symbolic alias for [[[zio.stream.ZStream!.run[R1<:R,E1>:E,B]*]]].
   */
  def >>>[R1 <: R, E2, A2 >: A, Z](sink: ZSink[R1, E, A2, E2, Any, Z]): ZIO[R1, E2, Z] =
    self.run(sink)

  /**
   * Symbolic alias for [[ZStream#concat]].
   */
  def ++[R1 <: R, E1 >: E, A1 >: A](that: => ZStream[R1, E1, A1]): ZStream[R1, E1, A1] =
    self concat that

  /**
   * Symbolic alias for [[ZStream#orElse]].
   */
  final def <>[R1 <: R, E2, A1 >: A](that: => ZStream[R1, E2, A1])(implicit ev: CanFail[E]): ZStream[R1, E2, A1] =
    self orElse that

  /**
   * Returns a stream that submerges the error case of an `Either` into the `ZStream`.
   */
  final def absolve[R1 <: R, E1, A1](implicit
    ev: ZStream[R, E, A] <:< ZStream[R1, E1, Either[E1, A1]]
  ): ZStream[R1, E1, A1] =
    ZStream.absolve(ev(self))

  /**
   * Aggregates elements of this stream using the provided sink for as long
   * as the downstream operators on the stream are busy.
   *
   * This operator divides the stream into two asynchronous "islands". Operators upstream
   * of this operator run on one fiber, while downstream operators run on another. Whenever
   * the downstream fiber is busy processing elements, the upstream fiber will feed elements
   * into the sink until it signals completion.
   *
   * Any sink can be used here, but see [[ZSink.foldWeightedM]] and [[ZSink.foldUntilM]] for
   * sinks that cover the common usecases.
   */
  final def aggregateAsync[R1 <: R, E1 >: E, E2, A1 >: A, B](
    sink: ZSink[R1, E1, A1, E2, A1, B]
  ): ZStream[R1 with Has[Clock], E2, B] =
    aggregateAsyncWithin(sink, Schedule.forever)

  /**
   * Like `aggregateAsyncWithinEither`, but only returns the `Right` results.
   *
   * @param sink used for the aggregation
   * @param schedule signalling for when to stop the aggregation
   * @return `ZStream[R1 with Has[Clock], E2, B]`
   */
  final def aggregateAsyncWithin[R1 <: R, E1 >: E, E2, A1 >: A, B](
    sink: ZSink[R1, E1, A1, E2, A1, B],
    schedule: Schedule[R1, Option[B], Any]
  ): ZStream[R1 with Has[Clock], E2, B] =
    aggregateAsyncWithinEither(sink, schedule).collect { case Right(v) =>
      v
    }

  /**
   * Aggregates elements using the provided sink until it completes, or until the
   * delay signalled by the schedule has passed.
   *
   * This operator divides the stream into two asynchronous islands. Operators upstream
   * of this operator run on one fiber, while downstream operators run on another. Elements
   * will be aggregated by the sink until the downstream fiber pulls the aggregated value,
   * or until the schedule's delay has passed.
   *
   * Aggregated elements will be fed into the schedule to determine the delays between
   * pulls.
   *
   * @param sink used for the aggregation
   * @param schedule signalling for when to stop the aggregation
   * @return `ZStream[R1 with Has[Clock], E2, Either[C, B]]`
   */
  def aggregateAsyncWithinEither[R1 <: R, E1 >: E, A1 >: A, E2, B, C](
    sink: ZSink[R1, E1, A1, E2, A1, B],
    schedule: Schedule[R1, Option[B], C]
  ): ZStream[R1 with Has[Clock], E2, Either[C, B]] = {
    type HandoffSignal = ZStream.HandoffSignal[C, E1, A]
    import ZStream.HandoffSignal._
    type SinkEndReason = ZStream.SinkEndReason[C]
    import ZStream.SinkEndReason._

    val deps =
      ZIO.mapN(
        ZStream.Handoff.make[HandoffSignal],
        Ref.make[SinkEndReason](SinkEnd),
        Ref.make(Chunk[A1]()),
        schedule.driver
      )((_, _, _, _))

    ZStream.fromEffect(deps).flatMap { case (handoff, sinkEndReason, sinkLeftovers, scheduleDriver) =>
      lazy val handoffProducer: ZChannel[Any, E1, Chunk[A], Any, Nothing, Nothing, Any] =
        ZChannel.readWithCause(
          (in: Chunk[A]) => ZChannel.fromEffect(handoff.offer(Emit(in))) *> handoffProducer,
          (cause: Cause[E1]) => ZChannel.fromEffect(handoff.offer(Halt(cause))),
          (_: Any) => ZChannel.fromEffect(handoff.offer(End(UpstreamEnd)))
        )

      lazy val handoffConsumer: ZChannel[Any, Any, Any, Any, E1, Chunk[A1], Unit] =
        ZChannel.unwrap(
          sinkLeftovers.getAndSet(Chunk.empty).flatMap { leftovers =>
            if (leftovers.nonEmpty) {
              UIO.succeed(ZChannel.write(leftovers) *> handoffConsumer)
            } else
              handoff.take.map {
                case Emit(chunk) => ZChannel.write(chunk) *> handoffConsumer
                case Halt(cause) => ZChannel.halt(cause)
                case End(reason) => ZChannel.fromEffect(sinkEndReason.set(reason))
              }
          }
        )

      def scheduledAggregator(
        lastB: Option[B]
      ): ZChannel[R1 with Has[Clock], Any, Any, Any, E2, Chunk[Either[C, B]], Any] = {
        val timeout =
          scheduleDriver
            .next(lastB)
            .foldCauseM(
              _.failureOrCause match {
                case Left(_)      => handoff.offer(End(ScheduleTimeout))
                case Right(cause) => handoff.offer(Halt(cause))
              },
              c => handoff.offer(End(ScheduleEnd(c)))
            )

        ZChannel
          .managed(timeout.forkManaged) { fiber =>
            (handoffConsumer >>> sink.channel).doneCollect.flatMap { case (leftovers, b) =>
              ZChannel.fromEffect(fiber.interrupt *> sinkLeftovers.set(leftovers.flatten)) *>
                ZChannel.unwrap {
                  sinkEndReason.modify {
                    case ScheduleEnd(c) =>
                      (ZChannel.write(Chunk(Right(b), Left(c))).as(Some(b)), SinkEnd)

                    case ScheduleTimeout =>
                      (ZChannel.write(Chunk(Right(b))).as(Some(b)), SinkEnd)

                    case SinkEnd =>
                      (ZChannel.write(Chunk(Right(b))).as(Some(b)), SinkEnd)

                    case UpstreamEnd =>
                      (ZChannel.write(Chunk(Right(b))).as(None), UpstreamEnd) // leftovers??
                  }
                }
            }
          }
          .flatMap {
            case None        => ZChannel.unit
            case s @ Some(_) => scheduledAggregator(s)
          }
      }

      ZStream.managed((self.channel >>> handoffProducer).runManaged.fork) *>
        new ZStream(scheduledAggregator(None))
    }
  }

  /**
   * Maps the success values of this stream to the specified constant value.
   */
  def as[A2](A2: => A2): ZStream[R, E, A2] =
    map(_ => A2)

  /**
   * Returns a stream whose failure and success channels have been mapped by
   * the specified pair of functions, `f` and `g`.
   */
  def bimap[E1, A1](f: E => E1, g: A => A1)(implicit ev: CanFail[E]): ZStream[R, E1, A1] =
    mapError(f).map(g)

  /**
   * Fan out the stream, producing a list of streams that have the same
   * elements as this stream. The driver stream will only ever advance the
   * `maximumLag` chunks before the slowest downstream stream.
   */
  final def broadcast(n: Int, maximumLag: Int): ZManaged[R, Nothing, List[ZStream[Any, E, A]]] =
    self
      .broadcastedQueues(n, maximumLag)
      .map(_.map(ZStream.fromQueueWithShutdown(_).flattenTake))

  /**
   * Fan out the stream, producing a dynamic number of streams that have the
   * same elements as this stream. The driver stream will only ever advance the
   * `maximumLag` chunks before the slowest downstream stream.
   */
  final def broadcastDynamic(
    maximumLag: Int
  ): ZManaged[R, Nothing, ZStream[Any, E, A]] =
    self
      .broadcastedQueuesDynamic(maximumLag)
      .map(ZStream.managed(_).flatMap(ZStream.fromQueue(_)).flattenTake)

  /**
   * Converts the stream to a managed list of queues. Every value will be
   * replicated to every queue with the slowest queue being allowed to buffer
   * `maximumLag` chunks before the driver is back pressured.
   *
   * Queues can unsubscribe from upstream by shutting down.
   */
  final def broadcastedQueues(
    n: Int,
    maximumLag: Int
  ): ZManaged[R, Nothing, List[Dequeue[Take[E, A]]]] =
    for {
      hub    <- Hub.bounded[Take[E, A]](maximumLag).toManaged
      queues <- ZManaged.collectAll(List.fill(n)(hub.subscribe))
      _      <- self.runIntoHubManaged(hub).fork
    } yield queues

  /**
   * Converts the stream to a managed dynamic amount of queues. Every chunk
   * will be replicated to every queue with the slowest queue being allowed to
   * buffer `maximumLag` chunks before the driver is back pressured.
   *
   * Queues can unsubscribe from upstream by shutting down.
   */
  final def broadcastedQueuesDynamic(
    maximumLag: Int
  ): ZManaged[R, Nothing, ZManaged[Any, Nothing, Dequeue[Take[E, A]]]] =
    toHub(maximumLag).map(_.subscribe)

  /**
   * Allows a faster producer to progress independently of a slower consumer by buffering
   * up to `capacity` elements in a queue.
   *
   * @note This combinator destroys the chunking structure. It's recommended to use chunkN afterwards.
   * @note Prefer capacities that are powers of 2 for better performance.
   */
  final def buffer(capacity: Int): ZStream[R, E, A] = {
    val queue = self.toQueueOfElements(capacity)
    new ZStream(
      ZChannel.managed(queue) { queue =>
        lazy val process: ZChannel[Any, Any, Any, Any, E, Chunk[A], Unit] =
          ZChannel.fromEffect {
            queue.take
          }.flatMap { (exit: Exit[Option[E], A]) =>
            exit.fold(
              Cause
                .flipCauseOption(_)
                .fold[ZChannel[Any, Any, Any, Any, E, Chunk[A], Unit]](ZChannel.end(()))(ZChannel.halt(_)),
              value => ZChannel.write(Chunk.single(value)) *> process
            )
          }

        process
      }
    )
  }

  /**
   * Allows a faster producer to progress independently of a slower consumer by buffering
   * up to `capacity` chunks in a queue.
   *
   * @note Prefer capacities that are powers of 2 for better performance.
   */
  final def bufferChunks(capacity: Int): ZStream[R, E, A] = {
    val queue = self.toQueue(capacity)
    new ZStream(
      ZChannel.managed(queue) { queue =>
        lazy val process: ZChannel[Any, Any, Any, Any, E, Chunk[A], Unit] =
          ZChannel.fromEffect {
            queue.take
          }.flatMap { (take: Take[E, A]) =>
            take.fold(
              ZChannel.end(()),
              error => ZChannel.halt(error),
              value => ZChannel.write(value) *> process
            )
          }

        process
      }
    )
  }

  /**
   * Allows a faster producer to progress independently of a slower consumer by buffering
   * up to `capacity` chunks in a dropping queue.
   *
   * @note Prefer capacities that are powers of 2 for better performance.
   */
  final def bufferChunksDropping(capacity: Int): ZStream[R, E, A] = {
    val queue = Queue.dropping[(Take[E, A], Promise[Nothing, Unit])](capacity).toManagedWith(_.shutdown)
    new ZStream(bufferSignal[R, E, A](queue, self.channel))
  }

  /**
   * Allows a faster producer to progress independently of a slower consumer by buffering
   * up to `capacity` chunks in a sliding queue.
   *
   * @note Prefer capacities that are powers of 2 for better performance.
   */
  final def bufferChunksSliding(capacity: Int): ZStream[R, E, A] = {
    val queue = Queue.sliding[(Take[E, A], Promise[Nothing, Unit])](capacity).toManagedWith(_.shutdown)
    new ZStream(bufferSignal[R, E, A](queue, self.channel))
  }

  /**
   * Allows a faster producer to progress independently of a slower consumer by buffering
   * up to `capacity` elements in a dropping queue.
   *
   * @note This combinator destroys the chunking structure. It's recommended to use chunkN afterwards.
   * @note Prefer capacities that are powers of 2 for better performance.
   */
  final def bufferDropping(capacity: Int): ZStream[R, E, A] = {
    val queue = Queue.dropping[(Take[E, A], Promise[Nothing, Unit])](capacity).toManagedWith(_.shutdown)
    new ZStream(bufferSignal[R, E, A](queue, self.chunkN(1).channel))
  }

  /**
   * Allows a faster producer to progress independently of a slower consumer by buffering
   * up to `capacity` elements in a sliding queue.
   *
   * @note This combinator destroys the chunking structure. It's recommended to use chunkN afterwards.
   * @note Prefer capacities that are powers of 2 for better performance.
   */
  final def bufferSliding(capacity: Int): ZStream[R, E, A] = {
    val queue = Queue.sliding[(Take[E, A], Promise[Nothing, Unit])](capacity).toManagedWith(_.shutdown)
    new ZStream(bufferSignal[R, E, A](queue, self.chunkN(1).channel))
  }

  private def bufferSignal[R1 <: R, E1 >: E, A1 >: A](
    managed: UManaged[Queue[(Take[E1, A1], Promise[Nothing, Unit])]],
    channel: ZChannel[R1, Any, Any, Any, E1, Chunk[A1], Any]
  ): ZChannel[R1, Any, Any, Any, E1, Chunk[A1], Unit] = {
    def producer(
      queue: Queue[(Take[E1, A1], Promise[Nothing, Unit])],
      ref: Ref[Promise[Nothing, Unit]]
    ): ZChannel[R1, E1, Chunk[A1], Any, Nothing, Nothing, Any] = {
      def terminate(take: Take[E1, A1]): ZChannel[R1, E1, Chunk[A1], Any, Nothing, Nothing, Any] =
        ZChannel.fromEffect {
          for {
            latch <- ref.get
            _     <- latch.await
            p     <- Promise.make[Nothing, Unit]
            _     <- queue.offer((take, p))
            _     <- ref.set(p)
            _     <- p.await
          } yield ()
        }

      ZChannel.readWith[R1, E1, Chunk[A1], Any, Nothing, Nothing, Any](
        in =>
          ZChannel.fromEffect {
            for {
              p     <- Promise.make[Nothing, Unit]
              added <- queue.offer((Take.chunk(in), p))
              _     <- ref.set(p).when(added)
            } yield ()
          } *> producer(queue, ref),
        err => terminate(Take.fail(err)),
        _ => terminate(Take.end)
      )
    }

    def consumer(
      queue: Queue[(Take[E1, A1], Promise[Nothing, Unit])]
    ): ZChannel[R1, Any, Any, Any, E1, Chunk[A1], Unit] = {
      lazy val process: ZChannel[Any, Any, Any, Any, E1, Chunk[A1], Unit] =
        ZChannel.fromEffect(queue.take).flatMap { case (take, promise) =>
          ZChannel.fromEffect(promise.succeed(())) *>
            take.fold(
              ZChannel.end(()),
              error => ZChannel.halt(error),
              value => ZChannel.write(value) *> process
            )
        }

      process
    }

    ZChannel.managed {
      for {
        queue <- managed
        start <- Promise.makeManaged[Nothing, Unit]
        _     <- start.succeed(()).toManaged
        ref   <- Ref.makeManaged(start)
        _     <- (channel >>> producer(queue, ref)).runManaged.fork
      } yield queue
    } { queue =>
      consumer(queue)
    }
  }

  /**
   * Allows a faster producer to progress independently of a slower consumer by buffering
   * chunks into an unbounded queue.
   */
  final def bufferUnbounded: ZStream[R, E, A] = {
    val queue = self.toQueueUnbounded
    new ZStream(
      ZChannel.managed(queue) { queue =>
        lazy val process: ZChannel[Any, Any, Any, Any, E, Chunk[A], Unit] =
          ZChannel.fromEffect {
            queue.take
          }.flatMap { (take: Take[E, A]) =>
            take.fold(
              ZChannel.end(()),
              error => ZChannel.halt(error),
              value => ZChannel.write(value) *> process
            )
          }

        process
      }
    )
  }

  /**
   * Switches over to the stream produced by the provided function in case this one
   * fails with a typed error.
   */
  final def catchAll[R1 <: R, E2, A1 >: A](f: E => ZStream[R1, E2, A1])(implicit ev: CanFail[E]): ZStream[R1, E2, A1] =
    catchAllCause(_.failureOrCause.fold(f, ZStream.halt(_)))

  /**
   * Switches over to the stream produced by the provided function in case this one
   * fails. Allows recovery from all causes of failure, including interruption if the
   * stream is uninterruptible.
   */
  final def catchAllCause[R1 <: R, E2, A1 >: A](f: Cause[E] => ZStream[R1, E2, A1]): ZStream[R1, E2, A1] =
    new ZStream(channel.catchAllCause(f(_).channel))

  /**
   * Switches over to the stream produced by the provided function in case this one
   * fails with some typed error.
   */
  final def catchSome[R1 <: R, E1 >: E, A1 >: A](pf: PartialFunction[E, ZStream[R1, E1, A1]]): ZStream[R1, E1, A1] =
    catchAll(pf.applyOrElse[E, ZStream[R1, E1, A1]](_, ZStream.fail(_)))

  /**
   * Switches over to the stream produced by the provided function in case this one
   * fails with some errors. Allows recovery from all causes of failure, including interruption if the
   * stream is uninterruptible.
   */
  final def catchSomeCause[R1 <: R, E1 >: E, A1 >: A](
    pf: PartialFunction[Cause[E], ZStream[R1, E1, A1]]
  ): ZStream[R1, E1, A1] =
    catchAllCause(pf.applyOrElse[Cause[E], ZStream[R1, E1, A1]](_, ZStream.halt(_)))

  /**
   * Returns a new stream that only emits elements that are not equal to the
   * previous element emitted, using natural equality to determine whether two
   * elements are equal.
   */
  def changes: ZStream[R, E, A] =
    changesWith(_ == _)

  /**
   * Returns a new stream that only emits elements that are not equal to the
   * previous element emitted, using the specified function to determine
   * whether two elements are equal.
   */
  def changesWith(f: (A, A) => Boolean): ZStream[R, E, A] = {
    def writer(last: Option[A]): ZChannel[R, E, Chunk[A], Any, E, Chunk[A], Unit] =
      ZChannel.readWithCause[R, E, Chunk[A], Any, E, Chunk[A], Unit](
        chunk => {
          val (newLast, newChunk) =
            chunk.foldLeft[(Option[A], Chunk[A])]((last, Chunk.empty)) {
              case ((Some(o), os), o1) if (f(o, o1)) => (Some(o1), os)
              case ((_, os), o1)                     => (Some(o1), os :+ o1)
            }

          ZChannel.write(newChunk) *> writer(newLast)
        },
        cause => ZChannel.halt(cause),
        _ => ZChannel.unit
      )

    new ZStream(self.channel >>> writer(None))
  }

  /**
   * Re-chunks the elements of the stream into chunks of
   * `n` elements each.
   * The last chunk might contain less than `n` elements
   */
  def chunkN(n: Int): ZStream[R, E, A] =
    ZStream.unwrap {
      ZIO.succeed {
        val rechunker = new ZStream.Rechunker[A](n)
        lazy val process: ZChannel[R, E, Chunk[A], Any, E, Chunk[A], Unit] =
          ZChannel.readWithCause(
            (chunk: Chunk[A]) =>
              if (chunk.size > 0) {
                var chunks: List[Chunk[A]] = Nil
                var result: Chunk[A]       = null
                var i                      = 0

                while (i < chunk.size) {
                  while (i < chunk.size && (result eq null)) {
                    result = rechunker.write(chunk(i))
                    i += 1
                  }

                  if (result ne null) {
                    chunks = result :: chunks
                    result = null
                  }
                }

                ZChannel.writeAll(chunks.reverse: _*) *> process
              } else process,
            (cause: Cause[E]) => rechunker.emitIfNotEmpty() *> ZChannel.halt(cause),
            (_: Any) => rechunker.emitIfNotEmpty()
          )

        new ZStream(channel >>> process)
      }
    }

  /**
   * Exposes the underlying chunks of the stream as a stream of chunks of elements
   */
  def chunks: ZStream[R, E, Chunk[A]] =
    mapChunks(Chunk.single)

  /**
   * Performs a filter and map in a single step.
   */
  final def collect[B](f: PartialFunction[A, B]): ZStream[R, E, B] =
    mapChunks(_.collect(f))

  /**
   * Filters any `Right` values.
   */
  final def collectLeft[L1, A1](implicit ev: A <:< Either[L1, A1]): ZStream[R, E, L1] = {
    val _ = ev
    self.asInstanceOf[ZStream[R, E, Either[L1, A1]]].collect { case Left(a) => a }
  }

  /**
   * Filters any 'None' values.
   */
  final def collectSome[A1](implicit ev: A <:< Option[A1]): ZStream[R, E, A1] = {
    val _ = ev
    self.asInstanceOf[ZStream[R, E, Option[A1]]].collect { case Some(a) => a }
  }

  /**
   * Filters any `Exit.Failure` values.
   */
  final def collectSuccess[L1, A1](implicit ev: A <:< Exit[L1, A1]): ZStream[R, E, A1] = {
    val _ = ev
    self.asInstanceOf[ZStream[R, E, Exit[L1, A1]]].collect { case Exit.Success(a) => a }
  }

  /**
   * Filters any `Left` values.
   */
  final def collectRight[L1, A1](implicit ev: A <:< Either[L1, A1]): ZStream[R, E, A1] = {
    val _ = ev
    self.asInstanceOf[ZStream[R, E, Either[L1, A1]]].collect { case Right(a) => a }
  }

  private def loopOnChunks[R1 <: R, E1 >: E, A1](
    f: Chunk[A] => ZChannel[R1, E1, Chunk[A], Any, E1, Chunk[A1], Boolean]
  ): ZStream[R1, E1, A1] = {
    lazy val loop: ZChannel[R1, E1, Chunk[A], Any, E1, Chunk[A1], Boolean] =
      ZChannel.readWith[R1, E1, Chunk[A], Any, E1, Chunk[A1], Boolean](
        chunk => f(chunk).flatMap(continue => if (continue) loop else ZChannel.Done(false)),
        ZChannel.fail(_),
        _ => ZChannel.succeed(false)
      )
    new ZStream(self.channel >>> loop)
  }

  private def loopOnPartialChunks[R1 <: R, E1 >: E, A1](
    f: (Chunk[A], A1 => UIO[Unit]) => ZIO[R1, E1, Boolean]
  ): ZStream[R1, E1, A1] =
    loopOnChunks(chunk =>
      ZChannel.unwrap {
        ZIO.suspendSucceed {
          val outputChunk           = ChunkBuilder.make[A1](chunk.size)
          val emit: A1 => UIO[Unit] = (a: A1) => UIO(outputChunk += a).unit
          f(chunk, emit).map { continue =>
            ZChannel.write(outputChunk.result()) *> ZChannel.end(continue)
          }.catchAll { failure =>
            ZIO.succeed {
              val partialResult = outputChunk.result()
              if (partialResult.nonEmpty)
                ZChannel.write(partialResult) *> ZChannel.fail(failure)
              else
                ZChannel.fail(failure)
            }
          }
        }
      }
    )

  private def loopOnPartialChunksElements[R1 <: R, E1 >: E, A1](
    f: (A, A1 => UIO[Unit]) => ZIO[R1, E1, Unit]
  ): ZStream[R1, E1, A1] =
    loopOnPartialChunks((chunk, emit) => ZIO.foreach_(chunk)(value => f(value, emit)).as(true))

  /**
   * Performs an effectful filter and map in a single step.
   */
  final def collectM[R1 <: R, E1 >: E, A1](pf: PartialFunction[A, ZIO[R1, E1, A1]]): ZStream[R1, E1, A1] =
    loopOnPartialChunksElements((a, emit) => pf.andThen(_.flatMap(emit).unit).applyOrElse(a, (_: A) => ZIO.unit))

  /**
   * Transforms all elements of the stream for as long as the specified partial function is defined.
   */
  def collectWhile[A1](pf: PartialFunction[A, A1]): ZStream[R, E, A1] = {
    lazy val loop: ZChannel[R, E, Chunk[A], Any, E, Chunk[A1], Any] =
      ZChannel.readWith[R, E, Chunk[A], Any, E, Chunk[A1], Any](
        in => {
          val mapped = in.collectWhile(pf)
          if (mapped.size == in.size)
            ZChannel.write(mapped) *> loop
          else
            ZChannel.write(mapped)
        },
        ZChannel.fail(_),
        ZChannel.succeed(_)
      )
    new ZStream(self.channel >>> loop)
  }

  /**
   * Terminates the stream when encountering the first `Right`.
   */
  final def collectWhileLeft[L1, A1](implicit ev: A <:< Either[L1, A1]): ZStream[R, E, L1] = {
    val _ = ev
    self.asInstanceOf[ZStream[R, E, Either[L1, A1]]].collectWhile { case Left(a) => a }
  }

  /**
   * Effectfully transforms all elements of the stream for as long as the specified partial function is defined.
   */
  final def collectWhileM[R1 <: R, E1 >: E, A1](pf: PartialFunction[A, ZIO[R1, E1, A1]]): ZStream[R1, E1, A1] =
    loopOnPartialChunks { (chunk, emit) =>
      val pfSome = (a: A) => pf.andThen(_.flatMap(emit).as(true)).applyOrElse(a, (_: A) => ZIO.succeed(false))

      def loop(chunk: Chunk[A]): ZIO[R1, E1, Boolean] =
        if (chunk.isEmpty) ZIO.succeed(true)
        else
          pfSome(chunk.head).flatMap(continue => if (continue) loop(chunk.tail) else ZIO.succeed(false))

      loop(chunk)
    }

  /**
   * Terminates the stream when encountering the first `None`.
   */
  final def collectWhileSome[A1](implicit ev: A <:< Option[A1]): ZStream[R, E, A1] = {
    val _ = ev
    self.asInstanceOf[ZStream[R, E, Option[A1]]].collectWhile { case Some(a) => a }
  }

  /**
   * Terminates the stream when encountering the first `Left`.
   */
  final def collectWhileRight[L1, A1](implicit ev: A <:< Either[L1, A1]): ZStream[R, E, A1] = {
    val _ = ev
    self.asInstanceOf[ZStream[R, E, Either[L1, A1]]].collectWhile { case Right(a) => a }
  }

  /**
   * Terminates the stream when encountering the first `Exit.Failure`.
   */
  final def collectWhileSuccess[L1, A1](implicit ev: A <:< Exit[L1, A1]): ZStream[R, E, A1] = {
    val _ = ev
    self.asInstanceOf[ZStream[R, E, Exit[L1, A1]]].collectWhile { case Exit.Success(a) => a }
  }

  /**
   * Combines the elements from this stream and the specified stream by repeatedly applying the
   * function `f` to extract an element using both sides and conceptually "offer"
   * it to the destination stream. `f` can maintain some internal state to control
   * the combining process, with the initial state being specified by `s`.
   *
   * Where possible, prefer [[ZStream#combineChunks]] for a more efficient implementation.
   */
  final def combine[R1 <: R, E1 >: E, S, A2, A3](that: ZStream[R1, E1, A2])(s: S)(
    f: (S, ZIO[R, Option[E], A], ZIO[R1, Option[E1], A2]) => ZIO[R1, Nothing, Exit[Option[E1], (A3, S)]]
  ): ZStream[R1, E1, A3] = {
    def producer[Err, Elem](
      handoff: ZStream.Handoff[Exit[Option[Err], Elem]],
      latch: ZStream.Handoff[Unit]
    ): ZChannel[R1, Err, Elem, Any, Nothing, Nothing, Any] =
      ZChannel.fromEffect(latch.take) *>
        ZChannel.readWithCause[R1, Err, Elem, Any, Nothing, Nothing, Any](
          value => ZChannel.fromEffect(handoff.offer(Exit.succeed(value))) *> producer(handoff, latch),
          cause => ZChannel.fromEffect(handoff.offer(Exit.halt(cause.map(Some(_))))),
          _ => ZChannel.fromEffect(handoff.offer(Exit.fail(None))) *> producer(handoff, latch)
        )

    new ZStream(
      ZChannel.managed {
        for {
          left   <- ZStream.Handoff.make[Exit[Option[E], A]].toManaged
          right  <- ZStream.Handoff.make[Exit[Option[E1], A2]].toManaged
          latchL <- ZStream.Handoff.make[Unit].toManaged
          latchR <- ZStream.Handoff.make[Unit].toManaged
          _      <- (self.channel.concatMap(ZChannel.writeChunk(_)) >>> producer(left, latchL)).runManaged.fork
          _      <- (that.channel.concatMap(ZChannel.writeChunk(_)) >>> producer(right, latchR)).runManaged.fork
        } yield (left, right, latchL, latchR)
      } { case (left, right, latchL, latchR) =>
        val pullLeft: IO[Option[E], A]    = latchL.offer(()) *> left.take.flatMap(ZIO.done(_))
        val pullRight: IO[Option[E1], A2] = latchR.offer(()) *> right.take.flatMap(ZIO.done(_))
        ZStream.unfoldM(s)(s => f(s, pullLeft, pullRight).flatMap(ZIO.done(_).optional)).channel
      }
    )
  }

  /**
   * Combines the chunks from this stream and the specified stream by repeatedly applying the
   * function `f` to extract a chunk using both sides and conceptually "offer"
   * it to the destination stream. `f` can maintain some internal state to control
   * the combining process, with the initial state being specified by `s`.
   */
  final def combineChunks[R1 <: R, E1 >: E, S, A2, A3](that: ZStream[R1, E1, A2])(s: S)(
    f: (
      S,
      ZIO[R, Option[E], Chunk[A]],
      ZIO[R1, Option[E1], Chunk[A2]]
    ) => ZIO[R1, Nothing, Exit[Option[E1], (Chunk[A3], S)]]
  ): ZStream[R1, E1, A3] = {
    def producer[Err, Elem](
      handoff: ZStream.Handoff[Take[Err, Elem]],
      latch: ZStream.Handoff[Unit]
    ): ZChannel[R1, Err, Chunk[Elem], Any, Nothing, Nothing, Any] =
      ZChannel.fromEffect(latch.take) *>
        ZChannel.readWithCause[R1, Err, Chunk[Elem], Any, Nothing, Nothing, Any](
          chunk => ZChannel.fromEffect(handoff.offer(Take.chunk(chunk))) *> producer(handoff, latch),
          cause => ZChannel.fromEffect(handoff.offer(Take.halt(cause))),
          _ => ZChannel.fromEffect(handoff.offer(Take.end)) *> producer(handoff, latch)
        )

    new ZStream(
      ZChannel.managed {
        for {
          left   <- ZStream.Handoff.make[Take[E, A]].toManaged
          right  <- ZStream.Handoff.make[Take[E1, A2]].toManaged
          latchL <- ZStream.Handoff.make[Unit].toManaged
          latchR <- ZStream.Handoff.make[Unit].toManaged
          _      <- (self.channel >>> producer(left, latchL)).runManaged.fork
          _      <- (that.channel >>> producer(right, latchR)).runManaged.fork
        } yield (left, right, latchL, latchR)
      } { case (left, right, latchL, latchR) =>
        val pullLeft  = latchL.offer(()) *> left.take.flatMap(_.done)
        val pullRight = latchR.offer(()) *> right.take.flatMap(_.done)
        ZStream.unfoldChunkM(s)(s => f(s, pullLeft, pullRight).flatMap(ZIO.done(_).optional)).channel
      }
    )
  }

  /**
   * Concatenates the specified stream with this stream, resulting in a stream
   * that emits the elements from this stream and then the elements from the specified stream.
   */
  def concat[R1 <: R, E1 >: E, A1 >: A](that: => ZStream[R1, E1, A1]): ZStream[R1, E1, A1] =
    new ZStream(channel *> that.channel)

  /**
   * Composes this stream with the specified stream to create a cartesian product of elements.
   * The `that` stream would be run multiple times, for every element in the `this` stream.
   *
   * See also [[ZStream#zip]] and [[ZStream#<&>]] for the more common point-wise variant.
   */
  final def cross[R1 <: R, E1 >: E, B](that: => ZStream[R1, E1, B]): ZStream[R1, E1, (A, B)] =
    new ZStream(self.channel.concatMap(a => that.channel.mapOut(b => a.flatMap(a => b.map(b => (a, b))))))

  /**
   * Composes this stream with the specified stream to create a cartesian product of elements,
   * but keeps only elements from this stream.
   * The `that` stream would be run multiple times, for every element in the `this` stream.
   *
   * See also [[ZStream#zip]] and [[ZStream#<&>]] for the more common point-wise variant.
   */
  final def crossLeft[R1 <: R, E1 >: E, B](that: => ZStream[R1, E1, B]): ZStream[R1, E1, A] =
    (self cross that).map(_._1)

  /**
   * Composes this stream with the specified stream to create a cartesian product of elements,
   * but keeps only elements from the other stream.
   * The `that` stream would be run multiple times, for every element in the `this` stream.
   *
   * See also [[ZStream#zip]] and [[ZStream#<&>]] for the more common point-wise variant.
   */
  def crossRight[R1 <: R, E1 >: E, B](that: => ZStream[R1, E1, B]): ZStream[R1, E1, B] =
    (self cross that).map(_._2)

  /**
   * Composes this stream with the specified stream to create a cartesian product of elements
   * with a specified function.
   * The `that` stream would be run multiple times, for every element in the `this` stream.
   *
   * See also [[ZStream#zip]] and [[ZStream#<&>]] for the more common point-wise variant.
   */
  final def crossWith[R1 <: R, E1 >: E, A2, C](that: ZStream[R1, E1, A2])(f: (A, A2) => C): ZStream[R1, E1, C] =
    self.flatMap(l => that.map(r => f(l, r)))

  /**
   * Produces the specified element if this stream is empty.
   */
  final def defaultIfEmpty[A1 >: A](a: A1): ZStream[R, E, A1] =
    defaultIfEmpty(Chunk.single(a))

  /**
   * Produces the specified chunk if this stream is empty.
   */
  final def defaultIfEmpty[A1 >: A](chunk: Chunk[A1]): ZStream[R, E, A1] =
    defaultIfEmpty(new ZStream(ZChannel.write(chunk)))

  /**
   * Switches to the provided stream in case this one is empty.
   */
  final def defaultIfEmpty[R1 <: R, E1 >: E, A1 >: A](stream: ZStream[R1, E1, A1]): ZStream[R1, E1, A1] = {
    lazy val writer: ZChannel[R1, E, Chunk[A], Any, E1, Chunk[A1], Any] =
      ZChannel.readWith(
        (in: Chunk[A]) => if (in.isEmpty) writer else ZChannel.write(in) *> ZChannel.identity[E, Chunk[A], Any],
        (e: E) => ZChannel.fail(e),
        (_: Any) => stream.channel
      )

    new ZStream(self.channel >>> writer)
  }

  /**
   * More powerful version of `ZStream#broadcast`. Allows to provide a function that determines what
   * queues should receive which elements. The decide function will receive the indices of the queues
   * in the resulting list.
   */
  final def distributedWith[E1 >: E](
    n: Int,
    maximumLag: Int,
    decide: A => UIO[Int => Boolean]
  ): ZManaged[R, Nothing, List[Dequeue[Exit[Option[E1], A]]]] =
    Promise.make[Nothing, A => UIO[UniqueKey => Boolean]].toManaged.flatMap { prom =>
      distributedWithDynamic(maximumLag, (a: A) => prom.await.flatMap(_(a)), _ => ZIO.unit).flatMap { next =>
        ZIO.collectAll {
          Range(0, n).map(id => next.map { case (key, queue) => ((key -> id), queue) })
        }.flatMap { entries =>
          val (mappings, queues) =
            entries.foldRight((Map.empty[UniqueKey, Int], List.empty[Dequeue[Exit[Option[E1], A]]])) {
              case ((mapping, queue), (mappings, queues)) =>
                (mappings + mapping, queue :: queues)
            }
          prom.succeed((a: A) => decide(a).map(f => (key: UniqueKey) => f(mappings(key)))).as(queues)
        }.toManaged
      }
    }

  /**
   * More powerful version of `ZStream#distributedWith`. This returns a function that will produce
   * new queues and corresponding indices.
   * You can also provide a function that will be executed after the final events are enqueued in all queues.
   * Shutdown of the queues is handled by the driver.
   * Downstream users can also shutdown queues manually. In this case the driver will
   * continue but no longer backpressure on them.
   */
  final def distributedWithDynamic(
    maximumLag: Int,
    decide: A => UIO[UniqueKey => Boolean],
    done: Exit[Option[E], Nothing] => UIO[Any] = (_: Any) => UIO.unit
  ): ZManaged[R, Nothing, UIO[(UniqueKey, Dequeue[Exit[Option[E], A]])]] =
    for {
      queuesRef <- Ref
                     .make[Map[UniqueKey, Queue[Exit[Option[E], A]]]](Map())
                     .toManagedWith(_.get.flatMap(qs => ZIO.foreach(qs.values)(_.shutdown)))
      add <- {
        val offer = (a: A) =>
          for {
            shouldProcess <- decide(a)
            queues        <- queuesRef.get
            _ <- ZIO
                   .foldLeft(queues)(List[UniqueKey]()) { case (acc, (id, queue)) =>
                     if (shouldProcess(id)) {
                       queue
                         .offer(Exit.succeed(a))
                         .foldCauseM(
                           {
                             // we ignore all downstream queues that were shut down and remove them later
                             case c if c.interrupted => ZIO.succeedNow(id :: acc)
                             case c                  => ZIO.halt(c)
                           },
                           _ => ZIO.succeedNow(acc)
                         )
                     } else ZIO.succeedNow(acc)
                   }
                   .flatMap(ids => if (ids.nonEmpty) queuesRef.update(_ -- ids) else ZIO.unit)
          } yield ()

        for {
          queuesLock <- Semaphore.make(1).toManaged
          newQueue <- Ref
                        .make[UIO[(UniqueKey, Queue[Exit[Option[E], A]])]] {
                          for {
                            queue <- Queue.bounded[Exit[Option[E], A]](maximumLag)
                            id     = UniqueKey()
                            _     <- queuesRef.update(_ + (id -> queue))
                          } yield (id, queue)
                        }
                        .toManaged
          finalize = (endTake: Exit[Option[E], Nothing]) =>
                       // we need to make sure that no queues are currently being added
                       queuesLock.withPermit {
                         for {
                           // all newly created queues should end immediately
                           _ <- newQueue.set {
                                  for {
                                    queue <- Queue.bounded[Exit[Option[E], A]](1)
                                    _     <- queue.offer(endTake)
                                    id     = UniqueKey()
                                    _     <- queuesRef.update(_ + (id -> queue))
                                  } yield (id, queue)
                                }
                           queues <- queuesRef.get.map(_.values)
                           _ <- ZIO.foreach(queues) { queue =>
                                  queue.offer(endTake).catchSomeCause {
                                    case c if c.interrupted => ZIO.unit
                                  }
                                }
                           _ <- done(endTake)
                         } yield ()
                       }
          _ <- self
                 .runForeachManaged(offer)
                 .foldCauseM(
                   cause => finalize(Exit.halt(cause.map(Some(_)))).toManaged,
                   _ => finalize(Exit.fail(None)).toManaged
                 )
                 .fork
        } yield queuesLock.withPermit(newQueue.get.flatten)
      }
    } yield add

  /**
   * Converts this stream to a stream that executes its effects but emits no
   * elements. Useful for sequencing effects using streams:
   *
   * {{{
   * (Stream(1, 2, 3).tap(i => ZIO(println(i))) ++
   *   Stream.fromEffect(ZIO(println("Done!"))).drain ++
   *   Stream(4, 5, 6).tap(i => ZIO(println(i)))).run(Sink.drain)
   * }}}
   */
  final def drain: ZStream[R, E, Nothing] =
    new ZStream(channel.drain)

  /**
   * Drains the provided stream in the background for as long as this stream is running.
   * If this stream ends before `other`, `other` will be interrupted. If `other` fails,
   * this stream will fail with that error.
   */
  final def drainFork[R1 <: R, E1 >: E](other: ZStream[R1, E1, Any]): ZStream[R1, E1, A] =
    ZStream.fromEffect(Promise.make[E1, Nothing]).flatMap { bgDied =>
      ZStream
        .managed(other.runForeachManaged(_ => ZIO.unit).catchAllCause(bgDied.halt(_).toManaged).fork) *>
        self.interruptWhen(bgDied)
    }

  /**
   * Drops the specified number of elements from this stream.
   */
  def drop(n: Int): ZStream[R, E, A] = {
    def loop(r: Int): ZChannel[R, E, Chunk[A], Any, E, Chunk[A], Any] =
      ZChannel
        .readWith(
          (in: Chunk[A]) => {
            val dropped  = in.drop(r)
            val leftover = (r - in.length).max(0)
            val more     = in.isEmpty || leftover > 0

            if (more) loop(leftover) else ZChannel.write(dropped) *> ZChannel.identity[E, Chunk[A], Any]
          },
          (e: E) => ZChannel.fail(e),
          (_: Any) => ZChannel.unit
        )

    new ZStream(channel >>> loop(n))
  }

  /**
   * Drops all elements of the stream for as long as the specified predicate
   * evaluates to `true`.
   */
  final def dropWhile(f: A => Boolean): ZStream[R, E, A] = {
    def loop: ZChannel[R, E, Chunk[A], Any, E, Chunk[A], Any] =
      ZChannel.readWith(
        (in: Chunk[A]) => {
          val leftover = in.dropWhile(f)
          val more     = leftover.isEmpty

          if (more) loop else ZChannel.write(leftover) *> ZChannel.identity[E, Chunk[A], Any]
        },
        (e: E) => ZChannel.fail(e),
        (_: Any) => ZChannel.unit
      )
    new ZStream(channel >>> loop)
  }

  /**
   * Drops all elements of the stream until the specified predicate evaluates
   * to `true`.
   */
  final def dropUntil(pred: A => Boolean): ZStream[R, E, A] =
    dropWhile(!pred(_)).drop(1)

  /**
   * Returns a stream whose failures and successes have been lifted into an
   * `Either`. The resulting stream cannot fail, because the failures have
   * been exposed as part of the `Either` success case.
   *
   * @note the stream will end as soon as the first error occurs.
   */
  final def either(implicit ev: CanFail[E]): ZStream[R, Nothing, Either[E, A]] =
    self.map(Right(_)).catchAll(e => ZStream(Left(e)))

  /**
   * Executes the provided finalizer after this stream's finalizers run.
   */
  final def ensuring[R1 <: R](fin: ZIO[R1, Nothing, Any]): ZStream[R1, E, A] =
    new ZStream(channel.ensuring(fin))

  /**
   * Executes the provided finalizer before this stream's finalizers run.
   */
  final def ensuringFirst[R1 <: R](fin: ZIO[R1, Nothing, Any]): ZStream[R1, E, A] =
    ???

  /**
   * Filters the elements emitted by this stream using the provided function.
   */
  final def filter(f: A => Boolean): ZStream[R, E, A] =
    mapChunks(_.filter(f))

  /**
   * Finds the first element emitted by this stream that satisfies the provided predicate.
   */
  final def find(f: A => Boolean): ZStream[R, E, A] = {
    lazy val loop: ZChannel[R, E, Chunk[A], Any, E, Chunk[A], Any] =
      ZChannel.readWith(
        (in: Chunk[A]) => in.find(f).fold(loop)(i => ZChannel.write(Chunk.single(i))),
        (e: E) => ZChannel.fail(e),
        (_: Any) => ZChannel.unit
      )

    new ZStream(self.channel >>> loop)
  }

  /**
   * Finds the first element emitted by this stream that satisfies the provided effectful predicate.
   */
  final def findM[R1 <: R, E1 >: E, S](f: A => ZIO[R1, E1, Boolean]): ZStream[R1, E1, A] = {
    lazy val loop: ZChannel[R1, E, Chunk[A], Any, E1, Chunk[A], Any] =
      ZChannel.readWith(
        (in: Chunk[A]) => ZChannel.unwrap(in.findM(f).map(_.fold(loop)(i => ZChannel.write(Chunk.single(i))))),
        (e: E) => ZChannel.fail(e),
        (_: Any) => ZChannel.unit
      )

    new ZStream(self.channel >>> loop)
  }

  /**
   * Executes a pure fold over the stream of values - reduces all elements in the stream to a value of type `S`.
   */
  final def runFold[S](s: S)(f: (S, A) => S): ZIO[R, E, S] =
    runFoldWhileManaged(s)(_ => true)((s, a) => f(s, a)).use(ZIO.succeedNow)

  /**
   * Executes an effectful fold over the stream of values.
   */
  final def runFoldM[R1 <: R, E1 >: E, S](s: S)(f: (S, A) => ZIO[R1, E1, S]): ZIO[R1, E1, S] =
    runFoldWhileManagedM[R1, E1, S](s)(_ => true)(f).use(ZIO.succeedNow)

  /**
   * Executes a pure fold over the stream of values.
   * Returns a Managed value that represents the scope of the stream.
   */
  final def runFoldManaged[S](s: S)(f: (S, A) => S): ZManaged[R, E, S] =
    runFoldWhileManaged(s)(_ => true)((s, a) => f(s, a))

  /**
   * Executes an effectful fold over the stream of values.
   * Returns a Managed value that represents the scope of the stream.
   */
  final def runFoldManagedM[R1 <: R, E1 >: E, S](s: S)(f: (S, A) => ZIO[R1, E1, S]): ZManaged[R1, E1, S] =
    runFoldWhileManagedM[R1, E1, S](s)(_ => true)(f)

  /**
   * Reduces the elements in the stream to a value of type `S`.
   * Stops the fold early when the condition is not fulfilled.
   * Example:
   * {{{
   *  Stream(1).forever.foldWhile(0)(_ <= 4)(_ + _) // UIO[Int] == 5
   * }}}
   */
  final def runFoldWhile[S](s: S)(cont: S => Boolean)(f: (S, A) => S): ZIO[R, E, S] =
    runFoldWhileManaged(s)(cont)((s, a) => f(s, a)).use(ZIO.succeedNow)

  /**
   * Executes an effectful fold over the stream of values.
   * Stops the fold early when the condition is not fulfilled.
   * Example:
   * {{{
   *   Stream(1)
   *     .forever                                // an infinite Stream of 1's
   *     .fold(0)(_ <= 4)((s, a) => UIO(s + a))  // UIO[Int] == 5
   * }}}
   *
   * @param cont function which defines the early termination condition
   */
  final def runFoldWhileM[R1 <: R, E1 >: E, S](s: S)(cont: S => Boolean)(f: (S, A) => ZIO[R1, E1, S]): ZIO[R1, E1, S] =
    runFoldWhileManagedM[R1, E1, S](s)(cont)(f).use(ZIO.succeedNow)

  /**
   * Executes a pure fold over the stream of values.
   * Returns a Managed value that represents the scope of the stream.
   * Stops the fold early when the condition is not fulfilled.
   */
  def runFoldWhileManaged[S](s: S)(cont: S => Boolean)(f: (S, A) => S): ZManaged[R, E, S] =
    runManaged(ZSink.fold(s)(cont)(f))

  /**
   * Executes an effectful fold over the stream of values.
   * Returns a Managed value that represents the scope of the stream.
   * Stops the fold early when the condition is not fulfilled.
   * Example:
   * {{{
   *   Stream(1)
   *     .forever                                // an infinite Stream of 1's
   *     .fold(0)(_ <= 4)((s, a) => UIO(s + a))  // Managed[Nothing, Int]
   *     .use(ZIO.succeed)                       // UIO[Int] == 5
   * }}}
   *
   * @param cont function which defines the early termination condition
   */
  final def runFoldWhileManagedM[R1 <: R, E1 >: E, S](
    s: S
  )(cont: S => Boolean)(f: (S, A) => ZIO[R1, E1, S]): ZManaged[R1, E1, S] =
    runManaged(ZSink.foldM(s)(cont)(f))

  /**
   * Consumes all elements of the stream, passing them to the specified callback.
   */
  final def foreach[R1 <: R, E1 >: E](f: A => ZIO[R1, E1, Any]): ZIO[R1, E1, Unit] =
    runForeach(f)

  /**
   * Consumes all elements of the stream, passing them to the specified callback.
   */
  final def runForeach[R1 <: R, E1 >: E](f: A => ZIO[R1, E1, Any]): ZIO[R1, E1, Unit] =
    run(ZSink.foreach(f))

  /**
   * Consumes all elements of the stream, passing them to the specified callback.
   */
  final def runForeachChunk[R1 <: R, E1 >: E](f: Chunk[A] => ZIO[R1, E1, Any]): ZIO[R1, E1, Unit] =
    run(ZSink.foreachChunk(f))

  /**
   * Like [[ZStream#foreachChunk]], but returns a `ZManaged` so the finalization order
   * can be controlled.
   */
  final def runForeachChunkManaged[R1 <: R, E1 >: E](f: Chunk[A] => ZIO[R1, E1, Any]): ZManaged[R1, E1, Unit] =
    runManaged(ZSink.foreachChunk(f))

  /**
   * Like [[ZStream#foreach]], but returns a `ZManaged` so the finalization order
   * can be controlled.
   */
  final def runForeachManaged[R1 <: R, E1 >: E](f: A => ZIO[R1, E1, Any]): ZManaged[R1, E1, Unit] =
    runManaged(ZSink.foreach(f))

  /**
   * Consumes elements of the stream, passing them to the specified callback,
   * and terminating consumption when the callback returns `false`.
   */
  final def runForeachWhile[R1 <: R, E1 >: E](f: A => ZIO[R1, E1, Boolean]): ZIO[R1, E1, Unit] =
    run(ZSink.foreachWhile(f))

  /**
   * Like [[ZStream#foreachWhile]], but returns a `ZManaged` so the finalization order
   * can be controlled.
   */
  final def runForeachWhileManaged[R1 <: R, E1 >: E](f: A => ZIO[R1, E1, Boolean]): ZManaged[R1, E1, Unit] =
    runManaged(ZSink.foreachWhile(f))

  /**
   * Repeats this stream forever.
   */
  def forever: ZStream[R, E, A] =
    new ZStream(channel.repeated)

  /**
   * Effectfully filters the elements emitted by this stream.
   */
  def filterM[R1 <: R, E1 >: E](f: A => ZIO[R1, E1, Boolean]): ZStream[R1, E1, A] =
    loopOnPartialChunksElements((a, emit) => f(a).flatMap(r => if (r) emit(a) else ZIO.unit))

  /**
   * Filters this stream by the specified predicate, removing all elements for
   * which the predicate evaluates to true.
   */
  final def filterNot(pred: A => Boolean): ZStream[R, E, A] = filter(a => !pred(a))

  /**
   * Emits elements of this stream with a fixed delay in between, regardless of how long it
   * takes to produce a value.
   */
  final def fixed(duration: Duration): ZStream[R with Has[Clock], E, A] =
    schedule(Schedule.fixed(duration))

  /**
   * Returns a stream made of the concatenation in strict order of all the streams
   * produced by passing each element of this stream to `f0`
   */
  final def flatMap[R1 <: R, E1 >: E, B](f: A => ZStream[R1, E1, B]): ZStream[R1, E1, B] =
    new ZStream(channel.concatMap(as => as.map(f).map(_.channel).fold(ZChannel.unit)(_ *> _)))

  /**
   * Maps each element of this stream to another stream and returns the
   * non-deterministic merge of those streams, executing up to `n` inner streams
   * concurrently. Up to `bufferSize` elements of the produced streams may be
   * buffered in memory by this operator.
   */
  def flatMapPar[R1 <: R, E1 >: E, B](n: Int, bufferSize: Int = 16)(f: A => ZStream[R1, E1, B]): ZStream[R1, E1, B] =
    new ZStream[R1, E1, B](
      channel.concatMap(ZChannel.writeChunk(_)).mergeMap[R1, Any, Any, Any, E1, Chunk[B]](n, bufferSize) {
        f(_).channel
      }
    )

  /**
   * Maps each element of this stream to another stream and returns the non-deterministic merge
   * of those streams, executing up to `n` inner streams concurrently. When a new stream is created
   * from an element of the source stream, the oldest executing stream is cancelled. Up to `bufferSize`
   * elements of the produced streams may be buffered in memory by this operator.
   */
  final def flatMapParSwitch[R1 <: R, E1 >: E, B](n: Int, bufferSize: Int = 16)(
    f: A => ZStream[R1, E1, B]
  ): ZStream[R1, E1, B] =
    new ZStream[R1, E1, B](
      channel
        .concatMap(ZChannel.writeChunk(_))
        .mergeMap[R1, Any, Any, Any, E1, Chunk[B]](n, bufferSize, ZChannel.MergeStrategy.BufferSliding) {
          f(_).channel
        }
    )

  /**
   * Flattens this stream-of-streams into a stream made of the concatenation in
   * strict order of all the streams.
   */
  def flatten[R1 <: R, E1 >: E, A1](implicit ev: A <:< ZStream[R1, E1, A1]): ZStream[R1, E1, A1] = flatMap(ev(_))

  /**
   * Submerges the chunks carried by this stream into the stream's structure, while
   * still preserving them.
   */
  def flattenChunks[A1](implicit ev: A <:< Chunk[A1]): ZStream[R, E, A1] =
    new ZStream(self.channel.mapOut(_.flatten))

  /**
   * Flattens [[Exit]] values. `Exit.Failure` values translate to stream failures
   * while `Exit.Success` values translate to stream elements.
   */
  def flattenExit[E1 >: E, A1](implicit ev: A <:< Exit[E1, A1]): ZStream[R, E1, A1] =
    mapM(a => ZIO.done(ev(a)))

  /**
   * Unwraps [[Exit]] values that also signify end-of-stream by failing with `None`.
   *
   * For `Exit[E, A]` values that do not signal end-of-stream, prefer:
   * {{{
   * stream.mapM(ZIO.done(_))
   * }}}
   */
  def flattenExitOption[E1 >: E, A1](implicit ev: A <:< Exit[Option[E1], A1]): ZStream[R, E1, A1] = {
    def processChunk(
      chunk: Chunk[Exit[Option[E1], A1]],
      cont: ZChannel[R, E, Chunk[Exit[Option[E1], A1]], Any, E1, Chunk[A1], Any]
    ): ZChannel[R, E, Chunk[Exit[Option[E1], A1]], Any, E1, Chunk[A1], Any] = {
      val (toEmit, rest) = chunk.splitWhere(!_.succeeded)
      val next = rest.headOption match {
        case Some(Exit.Success(_)) => ZChannel.end(())
        case Some(Exit.Failure(cause)) =>
          Cause.flipCauseOption(cause) match {
            case Some(cause) => ZChannel.halt(cause)
            case None        => ZChannel.end(())
          }
        case None => cont
      }
      ZChannel.write(toEmit.collect { case Exit.Success(a) => a }) *> next
    }

    lazy val process: ZChannel[R, E, Chunk[Exit[Option[E1], A1]], Any, E1, Chunk[A1], Any] =
      ZChannel.readWithCause[R, E, Chunk[Exit[Option[E1], A1]], Any, E1, Chunk[A1], Any](
        chunk => processChunk(chunk, process),
        cause => ZChannel.halt(cause),
        _ => ZChannel.end(())
      )

    new ZStream(channel.asInstanceOf[ZChannel[R, Any, Any, Any, E, Chunk[Exit[Option[E1], A1]], Any]] >>> process)
  }

  /**
   * Submerges the iterables carried by this stream into the stream's structure, while
   * still preserving them.
   */
  def flattenIterables[A1](implicit ev: A <:< Iterable[A1]): ZStream[R, E, A1] =
    map(a => Chunk.fromIterable(ev(a))).flattenChunks

  /**
   * Flattens a stream of streams into a stream by executing a non-deterministic
   * concurrent merge. Up to `n` streams may be consumed in parallel and up to
   * `outputBuffer` elements may be buffered by this operator.
   */
  def flattenPar[R1 <: R, E1 >: E, A1](n: Int, outputBuffer: Int = 16)(implicit
    ev: A <:< ZStream[R1, E1, A1]
  ): ZStream[R1, E1, A1] =
    flatMapPar[R1, E1, A1](n, outputBuffer)(ev(_))

  /**
   * Like [[flattenPar]], but executes all streams concurrently.
   */
  def flattenParUnbounded[R1 <: R, E1 >: E, A1](
    outputBuffer: Int = 16
  )(implicit ev: A <:< ZStream[R1, E1, A1]): ZStream[R1, E1, A1] =
    flattenPar[R1, E1, A1](Int.MaxValue, outputBuffer)

  /**
   * Unwraps [[Exit]] values and flatten chunks that also signify end-of-stream by failing with `None`.
   */
  final def flattenTake[E1 >: E, A1](implicit ev: A <:< Take[E1, A1]): ZStream[R, E1, A1] =
    map(_.exit).flattenExitOption[E1, Chunk[A1]].flattenChunks

  /**
   * More powerful version of [[ZStream.groupByKey]]
   */
  final def groupBy[R1 <: R, E1 >: E, K, V](
    f: A => ZIO[R1, E1, (K, V)],
    buffer: Int = 16
  ): ZStream.GroupBy[R1, E1, K, V] = {
    val qstream = ZStream.unwrapManaged {
      for {
        decider <- Promise.make[Nothing, (K, V) => UIO[UniqueKey => Boolean]].toManaged
        out <- Queue
                 .bounded[Exit[Option[E1], (K, Dequeue[Exit[Option[E1], V]])]](buffer)
                 .toManagedWith(_.shutdown)
        ref <- Ref.make[Map[K, UniqueKey]](Map()).toManaged
        add <- self
                 .mapM(f)
                 .distributedWithDynamic(
                   buffer,
                   (kv: (K, V)) => decider.await.flatMap(_.tupled(kv)),
                   out.offer
                 )
        _ <- decider.succeed { case (k, _) =>
               ref.get.map(_.get(k)).flatMap {
                 case Some(idx) => ZIO.succeedNow(_ == idx)
                 case None =>
                   add.flatMap { case (idx, q) =>
                     (ref.update(_ + (k -> idx)) *>
                       out.offer(Exit.succeed(k -> q.map(_.map(_._2))))).as(_ == idx)
                   }
               }
             }.toManaged
      } yield ZStream.fromQueueWithShutdown(out).flattenExitOption
    }
    new ZStream.GroupBy(qstream, buffer)
  }

  /**
   * Partition a stream using a function and process each stream individually.
   * This returns a data structure that can be used
   * to further filter down which groups shall be processed.
   *
   * After calling apply on the GroupBy object, the remaining groups will be processed
   * in parallel and the resulting streams merged in a nondeterministic fashion.
   *
   * Up to `buffer` elements may be buffered in any group stream before the producer
   * is backpressured. Take care to consume from all streams in order
   * to prevent deadlocks.
   *
   * Example:
   * Collect the first 2 words for every starting letter
   * from a stream of words.
   * {{{
   * ZStream.fromIterable(List("hello", "world", "hi", "holla"))
   *  .groupByKey(_.head) { case (k, s) => s.take(2).map((k, _)) }
   *  .runCollect
   *  .map(_ == List(('h', "hello"), ('h', "hi"), ('w', "world"))
   * }}}
   */
  final def groupByKey[K](
    f: A => K,
    buffer: Int = 16
  ): ZStream.GroupBy[R, E, K, A] =
    self.groupBy(a => ZIO.succeedNow((f(a), a)), buffer)

  /**
   * Partitions the stream with specified chunkSize
   * @param chunkSize size of the chunk
   */
  def grouped(chunkSize: Int): ZStream[R, E, Chunk[A]] =
    transduce(ZSink.collectAllN[E, A](chunkSize))

  /**
   * Partitions the stream with the specified chunkSize or until the specified
   * duration has passed, whichever is satisfied first.
   */
  def groupedWithin(chunkSize: Int, within: Duration): ZStream[R with Has[Clock], E, Chunk[A]] =
    aggregateAsyncWithin(ZSink.collectAllN[E, A](chunkSize), Schedule.spaced(within))

  /**
   * Halts the evaluation of this stream when the provided IO completes. The given IO
   * will be forked as part of the returned stream, and its success will be discarded.
   *
   * An element in the process of being pulled will not be interrupted when the IO
   * completes. See `interruptWhen` for this behavior.
   *
   * If the IO completes with a failure, the stream will emit that failure.
   */
  final def haltWhen[R1 <: R, E1 >: E](io: ZIO[R1, E1, Any]): ZStream[R1, E1, A] = {
    def writer(fiber: Fiber[E1, Any]): ZChannel[R1, E1, Chunk[A], Any, E1, Chunk[A], Unit] =
      ZChannel.unwrap {
        fiber.poll.map {
          case None =>
            ZChannel.readWith[R1, E1, Chunk[A], Any, E1, Chunk[A], Unit](
              in => ZChannel.write(in) *> writer(fiber),
              err => ZChannel.fail(err),
              _ => ZChannel.unit
            )

          case Some(exit) =>
            exit.fold(ZChannel.halt(_), _ => ZChannel.unit)
        }
      }

    new ZStream(
      ZChannel.unwrapManaged {
        io.forkManaged.map { fiber =>
          self.channel >>> writer(fiber)
        }
      }
    )
  }

  /**
   * Specialized version of haltWhen which halts the evaluation of this stream
   * after the given duration.
   *
   * An element in the process of being pulled will not be interrupted when the
   * given duration completes. See `interruptAfter` for this behavior.
   */
  final def haltAfter(duration: Duration): ZStream[R with Has[Clock], E, A] =
    haltWhen(Clock.sleep(duration))

  /**
   * Halts the evaluation of this stream when the provided promise resolves.
   *
   * If the promise completes with a failure, the stream will emit that failure.
   */
  final def haltWhen[E1 >: E](p: Promise[E1, _]): ZStream[R, E1, A] = {
    lazy val writer: ZChannel[R, E1, Chunk[A], Any, E1, Chunk[A], Unit] =
      ZChannel.unwrap {
        p.poll.map {
          case None =>
            ZChannel.readWith[R, E1, Chunk[A], Any, E1, Chunk[A], Unit](
              in => ZChannel.write(in) *> writer,
              err => ZChannel.fail(err),
              _ => ZChannel.unit
            )

          case Some(io) =>
            ZChannel.unwrap(io.fold(ZChannel.fail(_), _ => ZChannel.unit))
        }
      }

    new ZStream(self.channel >>> writer)
  }

  /**
   * Interleaves this stream and the specified stream deterministically by
   * alternating pulling values from this stream and the specified stream.
   * When one stream is exhausted all remaining values in the other stream
   * will be pulled.
   */
  final def interleave[R1 <: R, E1 >: E, A1 >: A](that: ZStream[R1, E1, A1]): ZStream[R1, E1, A1] =
    self.interleaveWith(that)(ZStream(true, false).forever)

  /**
   * Combines this stream and the specified stream deterministically using the
   * stream of boolean values `b` to control which stream to pull from next.
   * `true` indicates to pull from this stream and `false` indicates to pull
   * from the specified stream. Only consumes as many elements as requested by
   * `b`. If either this stream or the specified stream are exhausted further
   * requests for values from that stream will be ignored.
   */
  final def interleaveWith[R1 <: R, E1 >: E, A1 >: A](
    that: ZStream[R1, E1, A1]
  )(b: ZStream[R1, E1, Boolean]): ZStream[R1, E1, A1] = {
    def producer(handoff: ZStream.Handoff[Take[E1, A1]]): ZChannel[R1, E1, A1, Any, Nothing, Nothing, Unit] =
      ZChannel.readWithCause[R1, E1, A1, Any, Nothing, Nothing, Unit](
        value => ZChannel.fromEffect(handoff.offer(Take.single(value))) *> producer(handoff),
        cause => ZChannel.fromEffect(handoff.offer(Take.halt(cause))),
        _ => ZChannel.fromEffect(handoff.offer(Take.end))
      )

    new ZStream(
      ZChannel.managed {
        for {
          left  <- ZStream.Handoff.make[Take[E1, A1]].toManaged
          right <- ZStream.Handoff.make[Take[E1, A1]].toManaged
          _     <- (self.channel.concatMap(ZChannel.writeChunk(_)) >>> producer(left)).runManaged.fork
          _     <- (that.channel.concatMap(ZChannel.writeChunk(_)) >>> producer(right)).runManaged.fork
        } yield (left, right)
      } { case (left, right) =>
        def process(leftDone: Boolean, rightDone: Boolean): ZChannel[R1, E1, Boolean, Any, E1, Chunk[A1], Unit] =
          ZChannel.readWithCause[R1, E1, Boolean, Any, E1, Chunk[A1], Unit](
            bool =>
              (bool, leftDone, rightDone) match {
                case (true, false, _) =>
                  ZChannel.fromEffect(left.take).flatMap { take =>
                    take.fold(
                      if (rightDone) ZChannel.unit else process(true, rightDone),
                      cause => ZChannel.halt(cause),
                      chunk => ZChannel.write(chunk) *> process(leftDone, rightDone)
                    )
                  }
                case (false, _, false) =>
                  ZChannel.fromEffect(right.take).flatMap { take =>
                    take.fold(
                      if (leftDone) ZChannel.unit else process(leftDone, true),
                      cause => ZChannel.halt(cause),
                      chunk => ZChannel.write(chunk) *> process(leftDone, rightDone)
                    )
                  }
                case _ =>
                  process(leftDone, rightDone)
              },
            cause => ZChannel.halt(cause),
            _ => ZChannel.unit
          )

        b.channel.concatMap(ZChannel.writeChunk(_)) >>> process(false, false)
      }
    )
  }

  /**
   * Intersperse stream with provided element similar to <code>List.mkString</code>.
   */
  final def intersperse[A1 >: A](middle: A1): ZStream[R, E, A1] = {
    def writer(isFirst: Boolean): ZChannel[R, E, Chunk[A1], Any, E, Chunk[A1], Unit] =
      ZChannel.readWith[R, E, Chunk[A1], Any, E, Chunk[A1], Unit](
        chunk => {
          val builder    = ChunkBuilder.make[A1]()
          var flagResult = isFirst

          chunk.foreach { o =>
            if (flagResult) {
              flagResult = false
              builder += o
            } else {
              builder += middle
              builder += o
            }
          }

          ZChannel.write(builder.result()) *> writer(flagResult)
        },
        err => ZChannel.fail(err),
        _ => ZChannel.unit
      )

    new ZStream(self.channel >>> writer(true))
  }

  /**
   * Intersperse and also add a prefix and a suffix
   */
  final def intersperse[A1 >: A](start: A1, middle: A1, end: A1): ZStream[R, E, A1] =
    ZStream(start) ++ intersperse(middle) ++ ZStream(end)

  /**
   * Interrupts the evaluation of this stream when the provided IO completes. The given
   * IO will be forked as part of this stream, and its success will be discarded. This
   * combinator will also interrupt any in-progress element being pulled from upstream.
   *
   * If the IO completes with a failure before the stream completes, the returned stream
   * will emit that failure.
   */
  final def interruptWhen[R1 <: R, E1 >: E](io: ZIO[R1, E1, Any]): ZStream[R1, E1, A] =
    new ZStream(channel.interruptWhen(io))

  /**
   * Interrupts the evaluation of this stream when the provided promise resolves. This
   * combinator will also interrupt any in-progress element being pulled from upstream.
   *
   * If the promise completes with a failure, the stream will emit that failure.
   */
  final def interruptWhen[E1 >: E](p: Promise[E1, _]): ZStream[R, E1, A] =
    new ZStream(channel.interruptWhen(p.asInstanceOf[Promise[E1, Any]]))

  /**
   * Specialized version of interruptWhen which interrupts the evaluation of this stream
   * after the given duration.
   */
  final def interruptAfter(duration: Duration): ZStream[R with Has[Clock], E, A] =
    interruptWhen(Clock.sleep(duration))

  /**
   * Enqueues elements of this stream into a queue. Stream failure and ending will also be
   * signalled.
   */
  final def runInto[R1 <: R, E1 >: E](
    queue: ZQueue[R1, Nothing, Nothing, Any, Take[E1, A], Any]
  ): ZIO[R1, E1, Unit] =
    runIntoManaged(queue).use_(UIO.unit)

  /**
   * Publishes elements of this stream to a hub. Stream failure and ending will
   * also be signalled.
   */
  final def runIntoHub[R1 <: R, E1 >: E](
    hub: ZHub[R1, Nothing, Nothing, Any, Take[E1, A], Any]
  ): ZIO[R1, E1, Unit] =
    runInto(hub.toQueue)

  /**
   * Like [[ZStream#runIntoHub]], but provides the result as a [[ZManaged]] to
   * allow for scope composition.
   */
  final def runIntoHubManaged[R1 <: R, E1 >: E](
    hub: ZHub[R1, Nothing, Nothing, Any, Take[E1, A], Any]
  ): ZManaged[R1, E1, Unit] =
    runIntoManaged(hub.toQueue)

  /**
   * Like [[ZStream#into]], but provides the result as a [[ZManaged]] to allow for scope
   * composition.
   */
  final def runIntoManaged[R1 <: R, E1 >: E](
    queue: ZQueue[R1, Nothing, Nothing, Any, Take[E1, A], Any]
  ): ZManaged[R1, E1, Unit] = {
    lazy val writer: ZChannel[R, E, Chunk[A], Any, E, Take[E1, A], Any] = ZChannel
      .readWithCause[R, E, Chunk[A], Any, E, Take[E1, A], Any](
        in => ZChannel.write(Take.chunk(in)) *> writer,
        cause => ZChannel.write(Take.halt(cause)),
        _ => ZChannel.write(Take.end)
      )

    (self.channel >>> writer)
      .mapOutM(queue.offer)
      .drain
      .runManaged
      .unit
  }

  /*
   * Like [[ZStream#into]], but provides the result as a [[ZManaged]] to allow for scope
   * composition.
   */
  final def runIntoElementsManaged[R1 <: R, E1 >: E](
    queue: ZQueue[R1, Nothing, Nothing, Any, Exit[Option[E1], A], Any]
  ): ZManaged[R1, E1, Unit] = {
    lazy val writer: ZChannel[R1, E1, Chunk[A], Any, Nothing, Exit[Option[E1], A], Any] =
      ZChannel.readWith[R1, E1, Chunk[A], Any, Nothing, Exit[Option[E1], A], Any](
        in =>
          in.foldLeft[ZChannel[R1, Any, Any, Any, Nothing, Exit[Option[E1], A], Any]](ZChannel.unit) {
            case (channel, a) =>
              channel *> ZChannel.write(Exit.succeed(a))
          } *> writer,
        err => ZChannel.write(Exit.fail(Some(err))),
        _ => ZChannel.write(Exit.fail(None))
      )

    (self.channel >>> writer)
      .mapOutM(queue.offer)
      .drain
      .runManaged
      .unit
  }

  /**
   * Locks the execution of this stream to the specified executor. Any streams
   * that are composed after this one will automatically be shifted back to the
   * previous executor.
   */
  def lock(executor: Executor): ZStream[R, E, A] =
    ZStream.fromEffect(ZIO.descriptor).flatMap { descriptor =>
      ZStream.managed(ZManaged.lock(executor)) *>
        self <* ZStream.fromEffect(ZIO.shift(descriptor.executor))
    }

  /**
   * Transforms the elements of this stream using the supplied function.
   */
  final def map[B](f: A => B): ZStream[R, E, B] =
    new ZStream(channel.mapOut(_.map(f)))

  /**
   * Statefully maps over the elements of this stream to produce new elements.
   */
  def mapAccum[S, A1](s: S)(f: (S, A) => (S, A1)): ZStream[R, E, A1] = {
    def accumulator(currS: S): ZChannel[Any, E, Chunk[A], Any, E, Chunk[A1], Unit] =
      ZChannel.readWith(
        (in: Chunk[A]) => {
          val (nextS, a1s) = in.mapAccum(currS)(f)
          ZChannel.write(a1s) *> accumulator(nextS)
        },
        (err: E) => ZChannel.fail(err),
        (_: Any) => ZChannel.unit
      )

    new ZStream(self.channel >>> accumulator(s))
  }

  /**
   * Statefully and effectfully maps over the elements of this stream to produce
   * new elements.
   */
  final def mapAccumM[R1 <: R, E1 >: E, S, A1](s: S)(f: (S, A) => ZIO[R1, E1, (S, A1)]): ZStream[R1, E1, A1] = {
    def accumulator(s: S): ZChannel[R1, E, Chunk[A], Any, E1, Chunk[A1], Unit] =
      ZChannel.readWith(
        (in: Chunk[A]) =>
          ZChannel.unwrap(
            ZIO.suspendSucceed {
              val outputChunk           = ChunkBuilder.make[A1](in.size)
              val emit: A1 => UIO[Unit] = (a: A1) => UIO(outputChunk += a).unit
              ZIO
                .foldLeft[R1, E1, S, A](in)(s)((s1, a) => f(s1, a).flatMap(sa => emit(sa._2) as sa._1))
                .fold(
                  failure => {
                    val partialResult = outputChunk.result()
                    if (partialResult.nonEmpty)
                      ZChannel.write(partialResult) *> ZChannel.fail(failure)
                    else
                      ZChannel.fail(failure)
                  },
                  ZChannel.write(outputChunk.result()) *> accumulator(_)
                )
            }
          ),
        ZChannel.fail(_),
        (_: Any) => ZChannel.unit
      )

    new ZStream(self.channel >>> accumulator(s))
  }

  /**
   * Transforms the chunks emitted by this stream.
   */
  def mapChunks[A2](f: Chunk[A] => Chunk[A2]): ZStream[R, E, A2] =
    new ZStream(channel.mapOut(f))

  /**
   * Effectfully transforms the chunks emitted by this stream.
   */
  def mapChunksM[R1 <: R, E1 >: E, A2](f: Chunk[A] => ZIO[R1, E1, Chunk[A2]]): ZStream[R1, E1, A2] =
    new ZStream(channel.mapOutM(f))

  /**
   * Maps each element to an iterable, and flattens the iterables into the
   * output of this stream.
   */
  def mapConcat[A2](f: A => Iterable[A2]): ZStream[R, E, A2] =
    mapConcatChunk(a => Chunk.fromIterable(f(a)))

  /**
   * Maps each element to a chunk, and flattens the chunks into the output of
   * this stream.
   */
  def mapConcatChunk[A2](f: A => Chunk[A2]): ZStream[R, E, A2] =
    mapChunks(_.flatMap(f))

  /**
   * Effectfully maps each element to a chunk, and flattens the chunks into
   * the output of this stream.
   */
  final def mapConcatChunkM[R1 <: R, E1 >: E, A2](f: A => ZIO[R1, E1, Chunk[A2]]): ZStream[R1, E1, A2] =
    mapM(f).mapConcatChunk(identity)

  /**
   * Effectfully maps each element to an iterable, and flattens the iterables into
   * the output of this stream.
   */
  final def mapConcatM[R1 <: R, E1 >: E, A2](f: A => ZIO[R1, E1, Iterable[A2]]): ZStream[R1, E1, A2] =
    mapM(a => f(a).map(Chunk.fromIterable(_))).mapConcatChunk(identity)

  /**
   * Transforms the errors emitted by this stream using `f`.
   */
  def mapError[E2](f: E => E2): ZStream[R, E2, A] =
    new ZStream(self.channel.mapError(f))

  /**
   * Transforms the full causes of failures emitted by this stream.
   */
  def mapErrorCause[E2](f: Cause[E] => Cause[E2]): ZStream[R, E2, A] =
    new ZStream(self.channel.mapErrorCause(f))

  /**
   * Maps over elements of the stream with the specified effectful function.
   */
  def mapM[R1 <: R, E1 >: E, A1](f: A => ZIO[R1, E1, A1]): ZStream[R1, E1, A1] =
    loopOnPartialChunksElements((a, emit) => f(a) >>= emit)

  /**
   * Maps over elements of the stream with the specified effectful function,
   * executing up to `n` invocations of `f` concurrently. Transformed elements
   * will be emitted in the original order.
   *
   * @note This combinator destroys the chunking structure. It's recommended to use chunkN afterwards.
   */
  final def mapMPar[R1 <: R, E1 >: E, A2](n: Int)(f: A => ZIO[R1, E1, A2]): ZStream[R1, E1, A2] =
    new ZStream(self.channel.concatMap(ZChannel.writeChunk(_)).mapOutMPar[R1, E1, A2](n)(f).mapOut(Chunk.single))

  /**
   * Maps over elements of the stream with the specified effectful function,
   * executing up to `n` invocations of `f` concurrently. The element order
   * is not enforced by this combinator, and elements may be reordered.
   */
  final def mapMParUnordered[R1 <: R, E1 >: E, A2](n: Int)(f: A => ZIO[R1, E1, A2]): ZStream[R1, E1, A2] =
    flatMapPar[R1, E1, A2](n)(a => ZStream.fromEffect(f(a)))

  /**
   * Maps over elements of the stream with the specified effectful function,
   * partitioned by `p` executing invocations of `f` concurrently. The number
   * of concurrent invocations of `f` is determined by the number of different
   * outputs of type `K`. Up to `buffer` elements may be buffered per partition.
   * Transformed elements may be reordered but the order within a partition is maintained.
   */
  final def mapMPartitioned[R1 <: R, E1 >: E, A2, K](
    keyBy: A => K,
    buffer: Int = 16
  )(f: A => ZIO[R1, E1, A2]): ZStream[R1, E1, A2] =
    groupByKey(keyBy, buffer).apply { case (_, s) => s.mapM(f) }

  /**
   * Merges this stream and the specified stream together.
   *
   * New produced stream will terminate when both specified stream terminate if no termination
   * strategy is specified.
   */
  final def merge[R1 <: R, E1 >: E, A1 >: A](
    that: ZStream[R1, E1, A1],
    strategy: TerminationStrategy = TerminationStrategy.Both
  ): ZStream[R1, E1, A1] =
    self.mergeWith[R1, E1, A1, A1](that, strategy)(identity, identity) // TODO: Dotty doesn't infer this properly

  /**
   * Merges this stream and the specified stream together. New produced stream will
   * terminate when either stream terminates.
   */
  final def mergeTerminateEither[R1 <: R, E1 >: E, A1 >: A](that: ZStream[R1, E1, A1]): ZStream[R1, E1, A1] =
    self.merge[R1, E1, A1](that, TerminationStrategy.Either)

  /**
   * Merges this stream and the specified stream together. New produced stream will
   * terminate when this stream terminates.
   */
  final def mergeTerminateLeft[R1 <: R, E1 >: E, A1 >: A](that: ZStream[R1, E1, A1]): ZStream[R1, E1, A1] =
    self.merge[R1, E1, A1](that, TerminationStrategy.Left)

  /**
   * Merges this stream and the specified stream together. New produced stream will
   * terminate when the specified stream terminates.
   */
  final def mergeTerminateRight[R1 <: R, E1 >: E, A1 >: A](that: ZStream[R1, E1, A1]): ZStream[R1, E1, A1] =
    self.merge[R1, E1, A1](that, TerminationStrategy.Right)

  /**
   * Merges this stream and the specified stream together to produce a stream of
   * eithers.
   */
  final def mergeEither[R1 <: R, E1 >: E, A2](that: ZStream[R1, E1, A2]): ZStream[R1, E1, Either[A, A2]] =
    self.mergeWith(that)(Left(_), Right(_))

  /**
   * Merges this stream and the specified stream together to a common element
   * type with the specified mapping functions.
   *
   * New produced stream will terminate when both specified stream terminate if
   * no termination strategy is specified.
   */
  final def mergeWith[R1 <: R, E1 >: E, A2, A3](
    that: ZStream[R1, E1, A2],
    strategy: TerminationStrategy = TerminationStrategy.Both
  )(l: A => A3, r: A2 => A3): ZStream[R1, E1, A3] = {
    import TerminationStrategy.{Left, Right, Either}

    def handler(terminate: Boolean)(exit: Exit[E1, Any]): ZChannel.MergeDecision[R1, E1, Any, E1, Any] =
      if (terminate || !exit.succeeded) ZChannel.MergeDecision.done(ZIO.done(exit))
      else ZChannel.MergeDecision.await(ZIO.done(_))

    new ZStream(
      self
        .map(l)
        .channel
        .mergeWith(that.map(r).channel)(
          handler(strategy == Either || strategy == Left),
          handler(strategy == Either || strategy == Right)
        )
    )
  }

  /**
   * Runs the specified effect if this stream fails, providing the error to the effect if it exists.
   *
   * Note: Unlike [[ZIO.onError]], there is no guarantee that the provided effect will not be interrupted.
   */
  final def onError[R1 <: R](cleanup: Cause[E] => URIO[R1, Any]): ZStream[R1, E, A] =
    catchAllCause(cause => ZStream.fromEffect(cleanup(cause) *> ZIO.halt(cause)))

  /**
   * Switches to the provided stream in case this one fails with a typed error.
   *
   * See also [[ZStream#catchAll]].
   */
  def orElse[R1 <: R, E1, A1 >: A](that: => ZStream[R1, E1, A1])(implicit ev: CanFail[E]): ZStream[R1, E1, A1] =
    new ZStream(self.channel.orElse(that.channel))

  /**
   * Switches to the provided stream in case this one fails with a typed error.
   *
   * See also [[ZStream#catchAll]].
   */
  final def orElseEither[R1 <: R, E2, A2](
    that: => ZStream[R1, E2, A2]
  )(implicit ev: CanFail[E]): ZStream[R1, E2, Either[A, A2]] =
    self.map(Left(_)) orElse that.map(Right(_))

  /**
   * Fails with given error in case this one fails with a typed error.
   *
   * See also [[ZStream#catchAll]].
   */
  final def orElseFail[E1](e1: => E1)(implicit ev: CanFail[E]): ZStream[R, E1, A] =
    orElse(ZStream.fail(e1))

  /**
   * Switches to the provided stream in case this one fails with the `None` value.
   *
   * See also [[ZStream#catchAll]].
   */
  final def orElseOptional[R1 <: R, E1, A1 >: A](
    that: => ZStream[R1, Option[E1], A1]
  )(implicit ev: E <:< Option[E1]): ZStream[R1, Option[E1], A1] =
    catchAll(ev(_).fold(that)(e => ZStream.fail(Some(e))))

  /**
   * Succeeds with the specified value if this one fails with a typed error.
   */
  final def orElseSucceed[A1 >: A](A1: => A1)(implicit ev: CanFail[E]): ZStream[R, Nothing, A1] =
    orElse(ZStream.succeed(A1))

  /**
   * Partition a stream using a predicate. The first stream will contain all element evaluated to true
   * and the second one will contain all element evaluated to false.
   * The faster stream may advance by up to buffer elements further than the slower one.
   */
  def partition(p: A => Boolean, buffer: Int = 16): ZManaged[R, E, (ZStream[Any, E, A], ZStream[Any, E, A])] =
    self.partitionEither(a => if (p(a)) ZIO.succeedNow(Left(a)) else ZIO.succeedNow(Right(a)), buffer)

  /**
   * Split a stream by a predicate. The faster stream may advance by up to buffer elements further than the slower one.
   */
  final def partitionEither[R1 <: R, E1 >: E, A2, A3](
    p: A => ZIO[R1, E1, Either[A2, A3]],
    buffer: Int = 16
  ): ZManaged[R1, E1, (ZStream[Any, E1, A2], ZStream[Any, E1, A3])] =
    self
      .mapM(p)
      .distributedWith(
        2,
        buffer,
        {
          case Left(_)  => ZIO.succeedNow(_ == 0)
          case Right(_) => ZIO.succeedNow(_ == 1)
        }
      )
      .flatMap {
        case q1 :: q2 :: Nil =>
          ZManaged.succeedNow {
            (
              ZStream.fromQueueWithShutdown(q1).flattenExitOption.collectLeft,
              ZStream.fromQueueWithShutdown(q2).flattenExitOption.collectRight
            )
          }
        case otherwise => ZManaged.dieMessage(s"partitionEither: expected two streams but got ${otherwise}")
      }

  /**
   * Peels off enough material from the stream to construct a `Z` using the
   * provided [[ZSink]] and then returns both the `Z` and the rest of the
   * [[ZStream]] in a managed resource. Like all [[ZManaged]] values, the provided
   * stream is valid only within the scope of [[ZManaged]].
   */
  def peel[R1 <: R, E1 >: E, A1 >: A, Z](
    sink: ZSink[R1, E1, A1, E1, A1, Z]
  ): ZManaged[R1, E1, (Z, ZStream[R1, E1, A1])] = {
    sealed trait Signal
    case class Emit(els: Chunk[A1])   extends Signal
    case class Halt(cause: Cause[E1]) extends Signal
    case object End                   extends Signal

    (for {
      p       <- Promise.makeManaged[E1, Z]
      handoff <- ZStream.Handoff.make[Signal].toManaged
    } yield {
      val consumer: ZSink[R1, E, A1, E1, A1, Unit] = sink.exposeLeftover
        .foldM(
          e => ZSink.fromEffect(p.fail(e)) *> ZSink.fail(e),
          { case (z1, leftovers) =>
            lazy val loop: ZChannel[Any, E, Chunk[A1], Any, E1, Chunk[A1], Unit] = ZChannel.readWithCause(
              (in: Chunk[A1]) => ZChannel.fromEffect(handoff.offer(Emit(in))) *> loop,
              (e: Cause[E1]) => ZChannel.fromEffect(handoff.offer(Halt(e))) *> ZChannel.halt(e),
              (_: Any) => ZChannel.fromEffect(handoff.offer(End)) *> ZChannel.unit
            )

            new ZSink(
              ZChannel.fromEffect(p.succeed(z1)) *>
                ZChannel.fromEffect(handoff.offer(Emit(leftovers))) *>
                loop
            )
          }
        )

      lazy val producer: ZChannel[Any, Any, Any, Any, E1, Chunk[A1], Unit] = ZChannel.unwrap(
        handoff.take.map {
          case Emit(els)   => ZChannel.write(els) *> producer
          case Halt(cause) => ZChannel.halt(cause)
          case End         => ZChannel.unit
        }
      )

      for {
        _ <- self.runManaged(consumer).fork
        z <- p.await.toManaged
      } yield (z, new ZStream(producer))
    }).flatten
  }

  /**
   * Provides the stream with its required environment, which eliminates
   * its dependency on `R`.
   */
  final def provide(r: R)(implicit ev: NeedsEnv[R]): ZStream[Any, E, A] =
    new ZStream(channel.provide(r))

  /**
   * Provides the part of the environment that is not part of the `ZEnv`,
   * leaving a stream that only depends on the `ZEnv`.
   *
   * {{{
   * val loggingLayer: ZLayer[Any, Nothing, Logging] = ???
   *
   * val stream: ZStream[ZEnv with Logging, Nothing, Unit] = ???
   *
   * val stream2 = stream.provideCustomLayer(loggingLayer)
   * }}}
   */
  def provideCustomLayer[E1 >: E, R1](
    layer: ZLayer[ZEnv, E1, R1]
  )(implicit ev1: ZEnv with R1 <:< R, ev2: Has.Union[ZEnv, R1], tagged: Tag[R1]): ZStream[ZEnv, E1, A] =
    provideSomeLayer[ZEnv](layer)

  /**
   * Provides a layer to the stream, which translates it to another level.
   */
  final def provideLayer[E1 >: E, R0, R1](layer: ZLayer[R0, E1, R1])(implicit ev: R1 <:< R): ZStream[R0, E1, A] =
    new ZStream(ZChannel.managed(layer.build) { r =>
      self.channel.provide(r)
    })

  /**
   * Provides some of the environment required to run this effect,
   * leaving the remainder `R0`.
   */
  final def provideSome[R0](env: R0 => R)(implicit ev: NeedsEnv[R]): ZStream[R0, E, A] =
    ZStream.environment[R0].flatMap { r0 =>
      self.provide(env(r0))
    }

  /**
   * Splits the environment into two parts, providing one part using the
   * specified layer and leaving the remainder `R0`.
   *
   * {{{
   * val clockLayer: ZLayer[Any, Nothing, Has[Clock]] = ???
   *
   * val stream: ZStream[Has[Clock] with Has[Random], Nothing, Unit] = ???
   *
   * val stream2 = stream.provideSomeLayer[Has[Random]](clockLayer)
   * }}}
   */
  final def provideSomeLayer[R0]: ZStream.ProvideSomeLayer[R0, R, E, A] =
    new ZStream.ProvideSomeLayer[R0, R, E, A](self)

  /**
   * Keeps some of the errors, and terminates the fiber with the rest
   */
  final def refineOrDie[E1](
    pf: PartialFunction[E, E1]
  )(implicit ev1: E <:< Throwable, ev2: CanFail[E]): ZStream[R, E1, A] =
    refineOrDieWith(pf)(identity(_))

  /**
   * Keeps some of the errors, and terminates the fiber with the rest, using
   * the specified function to convert the `E` into a `Throwable`.
   */
  final def refineOrDieWith[E1](
    pf: PartialFunction[E, E1]
  )(f: E => Throwable)(implicit ev: CanFail[E]): ZStream[R, E1, A] =
    new ZStream(
      channel.catchAll(e =>
        if (pf.isDefinedAt(e))
          ZChannel.fail(pf.apply(e))
        else
          ZChannel.halt(Cause.die(f(e)))
      )
    )

  /**
   * Repeats the entire stream using the specified schedule. The stream will execute normally,
   * and then repeat again according to the provided schedule.
   */
  final def repeat[R1 <: R, B](schedule: Schedule[R1, Any, B]): ZStream[R1 with Has[Clock], E, A] =
    repeatEither(schedule) collect { case Right(a) => a }

  /**
   * Repeats the entire stream using the specified schedule. The stream will execute normally,
   * and then repeat again according to the provided schedule. The schedule output will be emitted at
   * the end of each repetition.
   */
  final def repeatEither[R1 <: R, B](schedule: Schedule[R1, Any, B]): ZStream[R1 with Has[Clock], E, Either[B, A]] =
    repeatWith(schedule)(Right(_), Left(_))

  /**
   * Repeats each element of the stream using the provided schedule. Repetitions are done in
   * addition to the first execution, which means using `Schedule.recurs(1)` actually results in
   * the original effect, plus an additional recurrence, for a total of two repetitions of each
   * value in the stream.
   */
  final def repeatElements[R1 <: R](schedule: Schedule[R1, A, Any]): ZStream[R1 with Has[Clock], E, A] =
    repeatElementsEither(schedule).collect { case Right(a) => a }

  /**
   * Repeats each element of the stream using the provided schedule. When the schedule is finished,
   * then the output of the schedule will be emitted into the stream. Repetitions are done in
   * addition to the first execution, which means using `Schedule.recurs(1)` actually results in
   * the original effect, plus an additional recurrence, for a total of two repetitions of each
   * value in the stream.
   */
  final def repeatElementsEither[R1 <: R, E1 >: E, B](
    schedule: Schedule[R1, A, B]
  ): ZStream[R1 with Has[Clock], E1, Either[B, A]] =
    repeatElementsWith(schedule)(Right.apply, Left.apply)

  /**
   * Repeats each element of the stream using the provided schedule. When the schedule is finished,
   * then the output of the schedule will be emitted into the stream. Repetitions are done in
   * addition to the first execution, which means using `Schedule.recurs(1)` actually results in
   * the original effect, plus an additional recurrence, for a total of two repetitions of each
   * value in the stream.
   *
   * This function accepts two conversion functions, which allow the output of this stream and the
   * output of the provided schedule to be unified into a single type. For example, `Either` or
   * similar data type.
   */
  final def repeatElementsWith[R1 <: R, E1 >: E, B, C](
    schedule: Schedule[R1, A, B]
  )(f: A => C, g: B => C): ZStream[R1 with Has[Clock], E1, C] = new ZStream(self.channel >>> ZChannel.unwrap {
    for {
      driver <- schedule.driver
    } yield {
      def feed(in: Chunk[A]): ZChannel[R1 with Has[Clock], E1, Chunk[A], Any, E1, Chunk[C], Unit] =
        in.headOption.fold(loop)(a => ZChannel.write(Chunk.single(f(a))) *> step(in.drop(1), a))

      def step(in: Chunk[A], a: A): ZChannel[R1 with Has[Clock], E1, Chunk[A], Any, E1, Chunk[C], Unit] = {
        val advance = driver.next(a).as(ZChannel.write(Chunk.single(f(a))) *> step(in, a))
        val reset
          : ZIO[R1 with Has[Clock], Nothing, ZChannel[R1 with Has[Clock], E1, Chunk[A], Any, E1, Chunk[C], Unit]] =
          for {
            b <- driver.last.orDie
            _ <- driver.reset
          } yield ZChannel.write(Chunk.single(g(b))) *> feed(in)

        ZChannel.unwrap(advance orElse reset)
      }

      lazy val loop: ZChannel[R1 with Has[Clock], E1, Chunk[A], Any, E1, Chunk[C], Unit] =
        ZChannel.readWith(
          feed,
          ZChannel.fail(_),
          (_: Any) => ZChannel.unit
        )

      loop
    }
  })

  /**
   * Repeats the entire stream using the specified schedule. The stream will execute normally,
   * and then repeat again according to the provided schedule. The schedule output will be emitted at
   * the end of each repetition and can be unified with the stream elements using the provided functions.
   */
  final def repeatWith[R1 <: R, B, C](
    schedule: Schedule[R1, Any, B]
  )(f: A => C, g: B => C): ZStream[R1 with Has[Clock], E, C] =
    ZStream.unwrap(
      for {
        driver <- schedule.driver
      } yield {
        val scheduleOutput = driver.last.orDie.map(g)
        val process        = self.map(f).channel
        lazy val loop: ZChannel[R1 with Has[Clock], Any, Any, Any, E, Chunk[C], Unit] =
          ZChannel.unwrap(
            driver
              .next(())
              .fold(
                _ => ZChannel.unit,
                _ => process *> ZChannel.unwrap(scheduleOutput.map(o => ZChannel.write(Chunk.single(o)))) *> loop
              )
          )

        new ZStream(process *> loop)
      }
    )

  /**
   * Fails with the error `None` if value is `Left`.
   */
  final def right[A1, A2](implicit ev: A <:< Either[A1, A2]): ZStream[R, Option[E], A2] =
    self.mapError(Some(_)).rightOrFail(None)

  /**
   * Fails with given error 'e' if value is `Left`.
   */
  final def rightOrFail[A1, A2, E1 >: E](e: => E1)(implicit ev: A <:< Either[A1, A2]): ZStream[R, E1, A2] =
    self.mapM(ev(_).fold(_ => ZIO.fail(e), ZIO.succeedNow(_)))

  /**
   * Runs the sink on the stream to produce either the sink's result or an error.
   */
  def run[R1 <: R, E2, Z](sink: ZSink[R1, E, A, E2, Any, Z]): ZIO[R1, E2, Z] =
    (channel pipeTo sink.channel).runDrain

  def runManaged[R1 <: R, E2, B](sink: ZSink[R1, E, A, E2, Any, B]): ZManaged[R1, E2, B] =
    (channel pipeTo sink.channel).drain.runManaged

  /**
   * Runs the stream and collects all of its elements to a chunk.
   */
  def runCollect: ZIO[R, E, Chunk[A]] =
    run(ZSink.collectAll)

  /**
   * Runs the stream and emits the number of elements processed
   *
   * Equivalent to `run(ZSink.count)`
   */
  final def runCount: ZIO[R, E, Long] =
    run(ZSink.count)

  /**
   * Runs the stream only for its effects. The emitted elements are discarded.
   */
  def runDrain: ZIO[R, E, Unit] =
    foreach(_ => ZIO.unit)

  /**
   * Runs the stream to completion and yields the first value emitted by it,
   * discarding the rest of the elements.
   */
  def runHead: ZIO[R, E, Option[A]] =
    run(ZSink.head)

  /**
   * Runs the stream to completion and yields the last value emitted by it,
   * discarding the rest of the elements.
   */
  def runLast: ZIO[R, E, Option[A]] =
    run(ZSink.last)

  /**
   * Runs the stream to a sink which sums elements, provided they are Numeric.
   *
   * Equivalent to `run(Sink.sum[A])`
   */
  final def runSum[A1 >: A](implicit ev: Numeric[A1]): ZIO[R, E, A1] =
    run(ZSink.sum[E, A1])

  /**
   * Statefully maps over the elements of this stream to produce all intermediate results
   * of type `S` given an initial S.
   */
  def scan[S](s: S)(f: (S, A) => S): ZStream[R, E, S] =
    scanM(s)((s, a) => ZIO.succeedNow(f(s, a)))

  /**
   * Statefully and effectfully maps over the elements of this stream to produce all
   * intermediate results of type `S` given an initial S.
   */
  def scanM[R1 <: R, E1 >: E, S](s: S)(f: (S, A) => ZIO[R1, E1, S]): ZStream[R1, E1, S] =
    ZStream(s) ++ mapAccumM[R1, E1, S, S](s)((s, a) => f(s, a).map(s => (s, s)))

  /**
   * Statefully maps over the elements of this stream to produce all intermediate results.
   *
   * See also [[ZStream#scan]].
   */
  def scanReduce[A1 >: A](f: (A1, A) => A1): ZStream[R, E, A1] =
    scanReduceM[R, E, A1]((curr, next) => ZIO.succeedNow(f(curr, next)))

  /**
   * Statefully and effectfully maps over the elements of this stream to produce all
   * intermediate results.
   *
   * See also [[ZStream#scanM]].
   */
  def scanReduceM[R1 <: R, E1 >: E, A1 >: A](f: (A1, A) => ZIO[R1, E1, A1]): ZStream[R1, E1, A1] =
    mapAccumM[R1, E1, Option[A1], A1](Option.empty[A1]) {
      case (Some(a1), a) => f(a1, a).map(a2 => Some(a2) -> a2)
      case (None, a)     => ZIO.succeedNow(Some(a) -> a)
    }

  /**
   * Schedules the output of the stream using the provided `schedule`.
   */
  final def schedule[R1 <: R](schedule: Schedule[R1, A, Any]): ZStream[R1 with Has[Clock], E, A] =
    scheduleEither(schedule).collect { case Right(a) => a }

  /**
   * Schedules the output of the stream using the provided `schedule` and emits its output at
   * the end (if `schedule` is finite).
   */
  final def scheduleEither[R1 <: R, E1 >: E, B](
    schedule: Schedule[R1, A, B]
  ): ZStream[R1 with Has[Clock], E1, Either[B, A]] =
    scheduleWith(schedule)(Right.apply, Left.apply)

  /**
   * Schedules the output of the stream using the provided `schedule` and emits its output at
   * the end (if `schedule` is finite).
   * Uses the provided function to align the stream and schedule outputs on the same type.
   */
  final def scheduleWith[R1 <: R, E1 >: E, B, C](
    schedule: Schedule[R1, A, B]
  )(f: A => C, g: B => C): ZStream[R1 with Has[Clock], E1, C] =
    ZStream.unwrap(
      schedule.driver.map(driver =>
        loopOnPartialChunksElements((a: A, emit: C => UIO[Unit]) =>
          driver.next(a).zipRight(emit(f(a))) orElse (driver.last.orDie.flatMap(b =>
            emit(f(a)) *> emit(g(b))
          ) <* driver.reset)
        )
      )
    )

  /**
   * Converts an option on values into an option on errors.
   */
  final def some[A2](implicit ev: A <:< Option[A2]): ZStream[R, Option[E], A2] =
    self.mapError(Some(_)).someOrFail(None)

  /**
   * Extracts the optional value, or returns the given 'default'.
   */
  final def someOrElse[A2](default: => A2)(implicit ev: A <:< Option[A2]): ZStream[R, E, A2] =
    map(_.getOrElse(default))

  /**
   * Extracts the optional value, or fails with the given error 'e'.
   */
  final def someOrFail[A2, E1 >: E](e: => E1)(implicit ev: A <:< Option[A2]): ZStream[R, E1, A2] =
    self.mapM(ev(_).fold[IO[E1, A2]](ZIO.fail(e))(ZIO.succeedNow(_)))

  /**
   * Takes the specified number of elements from this stream.
   */
  def take(n: Long): ZStream[R, E, A] = {
    def loop(n: Long): ZChannel[R, E, Chunk[A], Any, E, Chunk[A], Any] =
      ZChannel
        .readWith[R, E, Chunk[A], Any, E, Chunk[A], Any](
          (chunk: Chunk[A]) => {
            val taken    = chunk.take(n.min(Int.MaxValue).toInt)
            val leftover = (n - taken.length).max(0)
            val more     = leftover > 0

            if (more) ZChannel.write(taken) *> loop(leftover)
            else ZChannel.write(taken)
          },
          ZChannel.fail(_),
          ZChannel.succeed(_)
        )

    if (0 < n)
      new ZStream(self.channel >>> loop(n))
    else
      ZStream.empty
  }

  /**
   * Takes the last specified number of elements from this stream.
   */
  def takeRight(n: Int): ZStream[R, E, A] =
    if (n <= 0) ZStream.empty
    else
      new ZStream(
        ZChannel.unwrap(
          for {
            queue <- UIO(SingleThreadedRingBuffer[A](n))
          } yield {
            lazy val reader: ZChannel[Any, E, Chunk[A], Any, E, Chunk[A], Unit] = ZChannel.readWith(
              (in: Chunk[A]) => {
                in.foreach(queue.put)
                reader
              },
              ZChannel.fail(_),
              (_: Any) => ZChannel.write(queue.toChunk) *> ZChannel.unit
            )

            (self.channel >>> reader)
          }
        )
      )

  /**
   * Takes all elements of the stream until the specified predicate evaluates
   * to `true`.
   */
  def takeUntil(f: A => Boolean): ZStream[R, E, A] = {
    lazy val loop: ZChannel[R, E, Chunk[A], Any, E, Chunk[A], Any] =
      ZChannel
        .readWith[R, E, Chunk[A], Any, E, Chunk[A], Any](
          (chunk: Chunk[A]) => {
            val taken = chunk.takeWhile(!f(_))
            val last  = chunk.drop(taken.length).take(1)

            if (last.isEmpty) ZChannel.write(taken) *> loop
            else ZChannel.write(taken ++ last)
          },
          ZChannel.fail(_),
          ZChannel.succeed(_)
        )

    new ZStream(channel >>> loop)
  }

  /**
   * Takes all elements of the stream until the specified effectual predicate
   * evaluates to `true`.
   */
  def takeUntilM[R1 <: R, E1 >: E](f: A => ZIO[R1, E1, Boolean]): ZStream[R1, E1, A] =
    loopOnPartialChunks { (chunk, emit) =>
      for {
        taken <- chunk.takeWhileM(v => emit(v) *> f(v).map(!_))
        last   = chunk.drop(taken.length).take(1)
      } yield last.isEmpty
    }

  /**
   * Takes all elements of the stream for as long as the specified predicate
   * evaluates to `true`.
   */
  def takeWhile(f: A => Boolean): ZStream[R, E, A] = {
    lazy val loop: ZChannel[R, E, Chunk[A], Any, E, Chunk[A], Any] =
      ZChannel
        .readWith[R, E, Chunk[A], Any, E, Chunk[A], Any](
          (chunk: Chunk[A]) => {
            val taken = chunk.takeWhile(f)
            val more  = taken.length == chunk.length

            if (more) ZChannel.write(taken) *> loop else ZChannel.write(taken)
          },
          ZChannel.fail(_),
          ZChannel.succeed(_)
        )

    new ZStream(channel >>> loop)
  }

  /**
   * Adds an effect to consumption of every element of the stream.
   */
  final def tap[R1 <: R, E1 >: E](f0: A => ZIO[R1, E1, Any]): ZStream[R1, E1, A] =
    mapM(a => f0(a).as(a))

  /**
   * Throttles the chunks of this stream according to the given bandwidth parameters using the token bucket
   * algorithm. Allows for burst in the processing of elements by allowing the token bucket to accumulate
   * tokens up to a `units + burst` threshold. Chunks that do not meet the bandwidth constraints are dropped.
   * The weight of each chunk is determined by the `costFn` function.
   */
  final def throttleEnforce(units: Long, duration: Duration, burst: Long = 0)(
    costFn: Chunk[A] => Long
  ): ZStream[R with Has[Clock], E, A] =
    throttleEnforceM(units, duration, burst)(as => UIO.succeedNow(costFn(as)))

  /**
   * Throttles the chunks of this stream according to the given bandwidth parameters using the token bucket
   * algorithm. Allows for burst in the processing of elements by allowing the token bucket to accumulate
   * tokens up to a `units + burst` threshold. Chunks that do not meet the bandwidth constraints are dropped.
   * The weight of each chunk is determined by the `costFn` effectful function.
   */
  final def throttleEnforceM[R1 <: R, E1 >: E](units: Long, duration: Duration, burst: Long = 0)(
    costFn: Chunk[A] => ZIO[R1, E1, Long]
  ): ZStream[R1 with Has[Clock], E1, A] = {
    def loop(tokens: Long, timestamp: Long): ZChannel[R1 with Has[Clock], E1, Chunk[A], Any, E1, Chunk[A], Unit] =
      ZChannel.readWith[R1 with Has[Clock], E1, Chunk[A], Any, E1, Chunk[A], Unit](
        (in: Chunk[A]) =>
          ZChannel.unwrap((costFn(in) <*> Clock.nanoTime).map { case (weight, current) =>
            val elapsed = current - timestamp
            val cycles  = elapsed.toDouble / duration.toNanos
            val available = {
              val sum = tokens + (cycles * units).toLong
              val max =
                if (units + burst < 0) Long.MaxValue
                else units + burst

              if (sum < 0) max
              else math.min(sum, max)
            }

            if (weight <= available)
              ZChannel.write(in) *> loop(available - weight, current)
            else
              loop(available, current)
          }),
        (e: E1) => ZChannel.fail(e),
        (_: Any) => ZChannel.unit
      )

    new ZStream(ZChannel.fromEffect(Clock.nanoTime).flatMap(self.channel >>> loop(units, _)))
  }

  /**
   * Delays the chunks of this stream according to the given bandwidth parameters using the token bucket
   * algorithm. Allows for burst in the processing of elements by allowing the token bucket to accumulate
   * tokens up to a `units + burst` threshold. The weight of each chunk is determined by the `costFn`
   * function.
   */
  final def throttleShape(units: Long, duration: Duration, burst: Long = 0)(
    costFn: Chunk[A] => Long
  ): ZStream[R with Has[Clock], E, A] =
    throttleShapeM(units, duration, burst)(os => UIO.succeedNow(costFn(os)))

  /**
   * Delays the chunks of this stream according to the given bandwidth parameters using the token bucket
   * algorithm. Allows for burst in the processing of elements by allowing the token bucket to accumulate
   * tokens up to a `units + burst` threshold. The weight of each chunk is determined by the `costFn`
   * effectful function.
   */
  final def throttleShapeM[R1 <: R, E1 >: E](units: Long, duration: Duration, burst: Long = 0)(
    costFn: Chunk[A] => ZIO[R1, E1, Long]
  ): ZStream[R1 with Has[Clock], E1, A] = {
    def loop(tokens: Long, timestamp: Long): ZChannel[R1 with Has[Clock], E1, Chunk[A], Any, E1, Chunk[A], Unit] =
      ZChannel.readWith(
        (in: Chunk[A]) =>
          ZChannel.unwrap(for {
            weight  <- costFn(in)
            current <- Clock.nanoTime
          } yield {
            val elapsed = current - timestamp
            val cycles  = elapsed.toDouble / duration.toNanos
            val available = {
              val sum = tokens + (cycles * units).toLong
              val max =
                if (units + burst < 0) Long.MaxValue
                else units + burst

              if (sum < 0) max
              else math.min(sum, max)
            }

            val remaining = available - weight
            val waitCycles =
              if (remaining >= 0) 0
              else -remaining.toDouble / units

            val delay = Duration.Finite((waitCycles * duration.toNanos).toLong)

            if (delay > Duration.Zero)
              ZChannel.fromEffect(Clock.sleep(delay)) *> ZChannel.write(in) *> loop(remaining, current)
            else ZChannel.write(in) *> loop(remaining, current)
          }),
        (e: E1) => ZChannel.fail(e),
        (_: Any) => ZChannel.unit
      )

    new ZStream(ZChannel.fromEffect(Clock.nanoTime).flatMap(self.channel >>> loop(units, _)))
  }

  /**
   * Delays the emission of values by holding new values for a set duration. If no new values
   * arrive during that time the value is emitted, however if a new value is received during the holding period
   * the previous value is discarded and the process is repeated with the new value.
   *
   * This operator is useful if you have a stream of "bursty" events which eventually settle down and you
   * only need the final event of the burst.
   *
   * @example A search engine may only want to initiate a search after a user has paused typing
   *          so as to not prematurely recommend results.
   */
  final def debounce(d: Duration): ZStream[R with Has[Clock], E, A] = {
    import HandoffSignal._
    import DebounceState._

    ZStream.unwrap(
      for {
        scope   <- ZIO.forkScope
        handoff <- ZStream.Handoff.make[HandoffSignal[Unit, E, A]]
      } yield {
        def enqueue(last: Chunk[A]) =
          for {
            f <- Clock.sleep(d).as(last).forkIn(scope)
          } yield consumer(Previous(f))

        lazy val producer: ZChannel[R with Has[Clock], E, Chunk[A], Any, E, Nothing, Any] =
          ZChannel.readWithCause(
            (in: Chunk[A]) =>
              in.lastOption.fold(producer) { last =>
                ZChannel.fromEffect(handoff.offer(Emit(Chunk.single(last)))) *> producer
              },
            (cause: Cause[E]) => ZChannel.fromEffect(handoff.offer(Halt(cause))),
            (_: Any) => ZChannel.fromEffect(handoff.offer(End(ZStream.SinkEndReason.UpstreamEnd)))
          )

        def consumer(state: DebounceState[E, A]): ZChannel[R with Has[Clock], Any, Any, Any, E, Chunk[A], Any] =
          ZChannel.unwrap(
            state match {
              case NotStarted =>
                handoff.take.map {
                  case Emit(last) =>
                    ZChannel.unwrap(enqueue(last))
                  case HandoffSignal.Halt(error) =>
                    ZChannel.halt(error)
                  case HandoffSignal.End(_) =>
                    ZChannel.unit
                }
              case Current(fiber) =>
                fiber.join.map {
                  case HandoffSignal.Emit(last)  => ZChannel.unwrap(enqueue(last))
                  case HandoffSignal.Halt(error) => ZChannel.halt(error)
                  case HandoffSignal.End(_)      => ZChannel.unit
                }
              case Previous(fiber) =>
                fiber.join
                  .raceWith[R with Has[Clock], E, E, HandoffSignal[Unit, E, A], ZChannel[
                    R with Has[Clock],
                    Any,
                    Any,
                    Any,
                    E,
                    Chunk[A],
                    Any
                  ]](
                    handoff.take
                  )(
                    {
                      case (Exit.Success(a), current) =>
                        ZIO.succeedNow(ZChannel.write(a) *> consumer(Current(current)))
                      case (Exit.Failure(cause), current) =>
                        current.interrupt as ZChannel.halt(cause)
                    },
                    {
                      case (Exit.Success(Emit(last)), previous) =>
                        previous.interrupt *> enqueue(last)
                      case (Exit.Success(Halt(cause)), previous) =>
                        previous.interrupt as ZChannel.halt(cause)
                      case (Exit.Success(End(_)), previous) =>
                        previous.join.map(ZChannel.write(_) *> ZChannel.unit)
                      case (Exit.Failure(cause), previous) =>
                        previous.interrupt as ZChannel.halt(cause)
                    }
                  )
            }
          )

        ZStream.managed((self.channel >>> producer).runManaged.fork) *>
          new ZStream(consumer(NotStarted))
      }
    )
  }

  /**
   * Ends the stream if it does not produce a value after d duration.
   */
  final def timeout(d: Duration): ZStream[R with Has[Clock], E, A] =
    ZStream.fromPull {
      self.toPull.map(pull => pull.timeoutFail(None)(d))
    }

  /**
   * Fails the stream with given error if it does not produce a value after d duration.
   */
  final def timeoutFail[E1 >: E](e: => E1)(d: Duration): ZStream[R with Has[Clock], E1, A] =
    timeoutHalt(Cause.fail(e))(d)

  /**
   * Halts the stream with given cause if it does not produce a value after d duration.
   */
  final def timeoutHalt[E1 >: E](cause: Cause[E1])(d: Duration): ZStream[R with Has[Clock], E1, A] =
    ZStream.fromPull {
      self.toPull.map(pull => pull.timeoutHalt(cause.map(Some(_)))(d))
    }

  /**
   * Switches the stream if it does not produce a value after d duration.
   */
  final def timeoutTo[R1 <: R, E1 >: E, A2 >: A](
    d: Duration
  )(that: ZStream[R1, E1, A2]): ZStream[R1 with Has[Clock], E1, A2] = {
    final case class StreamTimeout() extends Throwable
    self.timeoutHalt(Cause.die(StreamTimeout()))(d).catchSomeCause { case Cause.Die(StreamTimeout()) => that }
  }

  /**
   * Converts the stream to a managed hub of chunks. After the managed hub is
   * used, the hub will never again produce values and should be discarded.
   */
  def toHub(capacity: Int): ZManaged[R, Nothing, ZHub[Nothing, Any, Any, Nothing, Nothing, Take[E, A]]] =
    for {
      hub <- Hub.bounded[Take[E, A]](capacity).toManagedWith(_.shutdown)
      _   <- self.runIntoHubManaged(hub).fork
    } yield hub

  /**
   * Converts this stream of bytes into a `java.io.InputStream` wrapped in a [[ZManaged]].
   * The returned input stream will only be valid within the scope of the ZManaged.
   */
  def toInputStream(implicit ev0: E <:< Throwable, ev1: A <:< Byte): ZManaged[R, E, java.io.InputStream] =
    for {
      runtime <- ZIO.runtime[R].toManaged
      pull    <- toPull.asInstanceOf[ZManaged[R, Nothing, ZIO[R, Option[Throwable], Chunk[Byte]]]]
    } yield ZInputStream.fromPull(runtime, pull)

  /**
   * Converts this stream into a `scala.collection.Iterator` wrapped in a [[ZManaged]].
   * The returned iterator will only be valid within the scope of the ZManaged.
   */
  def toIterator: ZManaged[R, Nothing, Iterator[Either[E, A]]] =
    for {
      runtime <- ZIO.runtime[R].toManaged
      pull    <- toPull
    } yield {
      def unfoldPull: Iterator[Either[E, A]] =
        runtime.unsafeRunSync(pull) match {
          case Exit.Success(chunk) => chunk.iterator.map(Right(_)) ++ unfoldPull
          case Exit.Failure(cause) =>
            cause.failureOrCause match {
              case Left(None)    => Iterator.empty
              case Left(Some(e)) => Iterator.single(Left(e))
              case Right(c)      => throw FiberFailure(c)
            }
        }

      unfoldPull
    }

  def toPull: ZManaged[R, Nothing, ZIO[R, Option[E], Chunk[A]]] =
    channel.toPull.map { pull =>
      pull.mapError(_.left.toOption)
    }

  /**
   * Converts this stream of chars into a `java.io.Reader` wrapped in a [[ZManaged]].
   * The returned reader will only be valid within the scope of the ZManaged.
   */
  def toReader(implicit ev0: E <:< Throwable, ev1: A <:< Char): ZManaged[R, E, java.io.Reader] =
    for {
      runtime <- ZIO.runtime[R].toManaged
      pull    <- toPull.asInstanceOf[ZManaged[R, Nothing, ZIO[R, Option[Throwable], Chunk[Char]]]]
    } yield ZReader.fromPull(runtime, pull)

  /**
   * Converts the stream to a managed queue of chunks. After the managed queue is used,
   * the queue will never again produce values and should be discarded.
   */
  final def toQueue(capacity: Int = 2): ZManaged[R, Nothing, Dequeue[Take[E, A]]] =
    for {
      queue <- Queue.bounded[Take[E, A]](capacity).toManagedWith(_.shutdown)
      _     <- self.runIntoManaged(queue).fork
    } yield queue

  /**
   * Converts the stream to a dropping managed queue of chunks. After the managed queue is used,
   * the queue will never again produce values and should be discarded.
   */
  final def toQueueDropping(capacity: Int = 2): ZManaged[R, Nothing, Dequeue[Take[E, A]]] =
    for {
      queue <- Queue.dropping[Take[E, A]](capacity).toManagedWith(_.shutdown)
      _     <- self.runIntoManaged(queue).fork
    } yield queue

  /**
   * Converts the stream to a managed queue of elements. After the managed queue is used,
   * the queue will never again produce values and should be discarded.
   */
  final def toQueueOfElements(capacity: Int = 2): ZManaged[R, Nothing, Dequeue[Exit[Option[E], A]]] =
    for {
      queue <- Queue.bounded[Exit[Option[E], A]](capacity).toManagedWith(_.shutdown)
      _     <- self.runIntoElementsManaged(queue).fork
    } yield queue

  /**
   * Converts the stream to a sliding managed queue of chunks. After the managed queue is used,
   * the queue will never again produce values and should be discarded.
   */
  final def toQueueSliding(capacity: Int = 2): ZManaged[R, Nothing, Dequeue[Take[E, A]]] =
    for {
      queue <- Queue.sliding[Take[E, A]](capacity).toManagedWith(_.shutdown)
      _     <- self.runIntoManaged(queue).fork
    } yield queue

  /**
   * Converts the stream into an unbounded managed queue. After the managed queue
   * is used, the queue will never again produce values and should be discarded.
   */
  final def toQueueUnbounded: ZManaged[R, Nothing, Dequeue[Take[E, A]]] =
    for {
      queue <- Queue.unbounded[Take[E, A]].toManagedWith(_.shutdown)
      _     <- self.runIntoManaged(queue).fork
    } yield queue

  /**
   * Applies the transducer to the stream and emits its outputs.
   */
  def transduce[R1 <: R, E1, A1 >: A, Z](sink: ZSink[R1, E, A1, E1, A1, Z]): ZStream[R1, E1, Z] =
    new ZStream(
      ZChannel.effectSuspendTotal {
        val leftovers: AtomicReference[Chunk[Chunk[A1]]] = new AtomicReference(Chunk.empty)
        val upstreamDone: AtomicBoolean                  = new AtomicBoolean(false)

        lazy val buffer: ZChannel[Any, E, Chunk[A1], Any, E, Chunk[A1], Any] =
          ZChannel.effectSuspendTotal {
            val l = leftovers.get

            if (l.isEmpty)
              ZChannel.readWith(
                (c: Chunk[A1]) => ZChannel.write(c) *> buffer,
                (e: E) => ZChannel.fail(e),
                (done: Any) => ZChannel.end(done)
              )
            else {
              leftovers.set(Chunk.empty)
              ZChannel.writeChunk(l) *> buffer
            }
          }

        def concatAndGet(c: Chunk[Chunk[A1]]): Chunk[Chunk[A1]] = {
          val ls     = leftovers.get
          val concat = ls ++ c.filter(_.nonEmpty)
          leftovers.set(concat)
          concat
        }

        lazy val upstreamMarker: ZChannel[Any, E, Chunk[A], Any, E, Chunk[A], Any] =
          ZChannel.readWith(
            (in: Chunk[A]) => ZChannel.write(in) *> upstreamMarker,
            (err: E) => ZChannel.fail(err),
            (done: Any) => ZChannel.effectTotal(upstreamDone.set(true)) *> ZChannel.end(done)
          )

        lazy val transducer: ZChannel[R1, E, Chunk[A1], Any, E1, Chunk[Z], Unit] =
          sink.channel.doneCollect.flatMap { case (leftover, z) =>
            ZChannel.effectTotal((upstreamDone.get, concatAndGet(leftover))).flatMap { case (done, newLeftovers) =>
              val nextChannel =
                if (done && newLeftovers.isEmpty) ZChannel.end(())
                else transducer

              ZChannel.write(Chunk.single(z)) *> nextChannel
            }
          }

        channel >>>
          upstreamMarker >>>
          buffer >>>
          transducer
      }
    )

  /**
   * Updates a service in the environment of this effect.
   */
  final def updateService[M] =
    new ZStream.UpdateService[R, E, A, M](self)

  /**
   * Threads the stream through the transformation function `f`.
   */
  final def via[R2, E2, A2](f: ZStream[R, E, A] => ZStream[R2, E2, A2]): ZStream[R2, E2, A2] = f(self)

  /**
   * Equivalent to [[filter]] but enables the use of filter clauses in for-comprehensions
   */
  def withFilter(predicate: A => Boolean): ZStream[R, E, A] =
    filter(predicate)

  /**
   * Zips this stream with another point-wise, but keeps only the outputs of this stream.
   *
   * The new stream will end when one of the sides ends.
   */
  def zipLeft[R1 <: R, E1 >: E, A2](that: ZStream[R1, E1, A2]): ZStream[R1, E1, A] = zipWith(that)((o, _) => o)

  /**
   * Zips this stream with another point-wise, but keeps only the outputs of the other stream.
   *
   * The new stream will end when one of the sides ends.
   */
  def zipRight[R1 <: R, E1 >: E, A2](that: ZStream[R1, E1, A2]): ZStream[R1, E1, A2] = zipWith(that)((_, A2) => A2)

  /**
   * Zips this stream with another point-wise and emits tuples of elements from both streams.
   *
   * The new stream will end when one of the sides ends.
   */
  def zip[R1 <: R, E1 >: E, A2](that: ZStream[R1, E1, A2]): ZStream[R1, E1, (A, A2)] = zipWith(that)((_, _))

  /**
   * Zips this stream with another point-wise, creating a new stream of pairs of elements
   * from both sides.
   *
   * The defaults `defaultLeft` and `defaultRight` will be used if the streams have different lengths
   * and one of the streams has ended before the other.
   */
  def zipAll[R1 <: R, E1 >: E, A1 >: A, A2](
    that: ZStream[R1, E1, A2]
  )(defaultLeft: A1, defaultRight: A2): ZStream[R1, E1, (A1, A2)] =
    zipAllWith(that)((_, defaultRight), (defaultLeft, _))((_, _))

  /**
   * Zips this stream with another point-wise, and keeps only elements from this stream.
   *
   * The provided default value will be used if the other stream ends before this one.
   */
  def zipAllLeft[R1 <: R, E1 >: E, A1 >: A, A2](that: ZStream[R1, E1, A2])(default: A1): ZStream[R1, E1, A1] =
    zipAllWith(that)(identity, _ => default)((o, _) => o)

  /**
   * Zips this stream with another point-wise, and keeps only elements from the other stream.
   *
   * The provided default value will be used if this stream ends before the other one.
   */
  def zipAllRight[R1 <: R, E1 >: E, A2](that: ZStream[R1, E1, A2])(default: A2): ZStream[R1, E1, A2] =
    zipAllWith(that)(_ => default, identity)((_, A2) => A2)

  /**
   * Zips this stream with another point-wise. The provided functions will be used to create elements
   * for the composed stream.
   *
   * The functions `left` and `right` will be used if the streams have different lengths
   * and one of the streams has ended before the other.
   */
  def zipAllWith[R1 <: R, E1 >: E, A2, A3](
    that: ZStream[R1, E1, A2]
  )(left: A => A3, right: A2 => A3)(both: (A, A2) => A3): ZStream[R1, E1, A3] =
    zipAllWithExec(that)(ExecutionStrategy.Parallel)(left, right)(both)

  /**
   * Zips this stream with another point-wise. The provided functions will be used to create elements
   * for the composed stream.
   *
   * The functions `left` and `right` will be used if the streams have different lengths
   * and one of the streams has ended before the other.
   *
   * The execution strategy `exec` will be used to determine whether to pull
   * from the streams sequentially or in parallel.
   */
  def zipAllWithExec[R1 <: R, E1 >: E, A2, A3](
    that: ZStream[R1, E1, A2]
  )(exec: ExecutionStrategy)(left: A => A3, right: A2 => A3)(both: (A, A2) => A3): ZStream[R1, E1, A3] = {
    sealed trait Status
    case object Running   extends Status
    case object LeftDone  extends Status
    case object RightDone extends Status
    case object End       extends Status
    type State = (Status, Either[Chunk[A], Chunk[A2]])

    def handleSuccess(
      maybeO: Option[Chunk[A]],
      maybeA2: Option[Chunk[A2]],
      excess: Either[Chunk[A], Chunk[A2]]
    ): Exit[Nothing, (Chunk[A3], State)] = {
      val (excessL, excessR) = excess.fold(l => (l, Chunk.empty), r => (Chunk.empty, r))
      val chunkL             = maybeO.fold(excessL)(upd => excessL ++ upd)
      val chunkR             = maybeA2.fold(excessR)(upd => excessR ++ upd)
      val (emit, newExcess)  = zipChunks(chunkL, chunkR, both)
      val (fullEmit, status) = (maybeO.isDefined, maybeA2.isDefined) match {
        case (true, true) => (emit, Running)
        case (false, false) =>
          val leftover: Chunk[A3] = newExcess.fold[Chunk[A3]](_.map(left), _.map(right))
          (emit ++ leftover, End)
        case (false, true) => (emit, LeftDone)
        case (true, false) => (emit, RightDone)
      }
      Exit.succeed((fullEmit, (status, newExcess)))
    }

    combineChunks(that)((Running, Left(Chunk())): State) {
      case ((Running, excess), pullL, pullR) =>
        exec match {
          case ExecutionStrategy.Sequential =>
            pullL.optional
              .zipWith(pullR.optional)(handleSuccess(_, _, excess))
              .catchAllCause(e => UIO.succeedNow(Exit.halt(e.map(Some(_)))))
          case _ =>
            pullL.optional
              .zipWithPar(pullR.optional)(handleSuccess(_, _, excess))
              .catchAllCause(e => UIO.succeedNow(Exit.halt(e.map(Some(_)))))
        }
      case ((LeftDone, excess), _, pullR) =>
        pullR.optional
          .map(handleSuccess(None, _, excess))
          .catchAllCause(e => UIO.succeedNow(Exit.halt(e.map(Some(_)))))
      case ((RightDone, excess), pullL, _) =>
        pullL.optional
          .map(handleSuccess(_, None, excess))
          .catchAllCause(e => UIO.succeedNow(Exit.halt(e.map(Some(_)))))
      case ((End, _), _, _) => UIO.succeedNow(Exit.fail(None))
    }
  }

  /**
   * Zips this stream with another point-wise and applies the function to the paired elements.
   *
   * The new stream will end when one of the sides ends.
   */
  def zipWith[R1 <: R, E1 >: E, A2, A3](
    that: ZStream[R1, E1, A2]
  )(f: (A, A2) => A3): ZStream[R1, E1, A3] = {
    sealed trait State[+W1, +W2]
    case class Running[W1, W2](excess: Either[Chunk[W1], Chunk[W2]]) extends State[W1, W2]
    case class LeftDone[W1](excessL: NonEmptyChunk[W1])              extends State[W1, Nothing]
    case class RightDone[W2](excessR: NonEmptyChunk[W2])             extends State[Nothing, W2]
    case object End                                                  extends State[Nothing, Nothing]

    def handleSuccess(
      leftUpd: Option[Chunk[A]],
      rightUpd: Option[Chunk[A2]],
      excess: Either[Chunk[A], Chunk[A2]]
    ): Exit[Option[Nothing], (Chunk[A3], State[A, A2])] = {
      val (left, right) = {
        val (leftExcess, rightExcess) = excess.fold(l => (l, Chunk.empty), r => (Chunk.empty, r))
        val l                         = leftUpd.fold(leftExcess)(upd => leftExcess ++ upd)
        val r                         = rightUpd.fold(rightExcess)(upd => rightExcess ++ upd)
        (l, r)
      }
      val (emit, newExcess): (Chunk[A3], Either[Chunk[A], Chunk[A2]]) = zipChunks(left, right, f)
      (leftUpd.isDefined, rightUpd.isDefined) match {
        case (true, true)   => Exit.succeed((emit, Running(newExcess)))
        case (false, false) => Exit.fail(None)
        case _ => {
          val newState = newExcess match {
            case Left(l)  => l.nonEmptyOrElse[State[A, A2]](End)(LeftDone(_))
            case Right(r) => r.nonEmptyOrElse[State[A, A2]](End)(RightDone(_))
          }
          Exit.succeed((emit, newState))
        }
      }
    }

    combineChunks(that)(Running(Left(Chunk.empty)): State[A, A2]) { (st, p1, p2) =>
      st match {
        case Running(excess) =>
          {
            p1.optional.zipWithPar(p2.optional) { case (l, r) =>
              handleSuccess(l, r, excess)
            }
          }.catchAllCause(e => UIO.succeedNow(Exit.halt(e.map(Some(_)))))
        case LeftDone(excessL) =>
          {
            p2.optional.map(handleSuccess(None, _, Left(excessL)))
          }.catchAllCause(e => UIO.succeedNow(Exit.halt(e.map(Some(_)))))
        case RightDone(excessR) => {
          p1.optional
            .map(handleSuccess(_, None, Right(excessR)))
            .catchAllCause(e => UIO.succeedNow(Exit.halt(e.map(Some(_)))))
        }
        case End => {
          UIO.succeedNow(Exit.fail(None))
        }
      }
    }
  }

  /**
   * Zips this stream together with the index of elements.
   */
  final def zipWithIndex: ZStream[R, E, (A, Long)] =
    mapAccum(0L)((index, a) => (index + 1, (a, index)))

  /**
   * Zips the two streams so that when a value is emitted by either of the two streams,
   * it is combined with the latest value from the other stream to produce a result.
   *
   * Note: tracking the latest value is done on a per-chunk basis. That means that
   * emitted elements that are not the last value in chunks will never be used for zipping.
   */
  final def zipWithLatest[R1 <: R, E1 >: E, A2, A3](
    that: ZStream[R1, E1, A2]
  )(f: (A, A2) => A3): ZStream[R1, E1, A3] = {
    val mergedChannel: ZChannel[R1, Any, Any, Any, E1, Option[Either[A, A2]], Any] =
      self.channel
        .mapOut(_.lastOption.map(Left(_)))
        .mergeWith(that.channel.mapOut(_.lastOption.map(Right(_))))(
          exit => ZChannel.MergeDecision.done(ZIO.done(exit)),
          exit => ZChannel.MergeDecision.done(ZIO.done(exit))
        )

    def writer(
      lastLeft: Option[A],
      lastRight: Option[A2]
    ): ZChannel[R1, E1, Option[Either[A, A2]], Any, E1, Chunk[A3], Unit] =
      ZChannel.readWith[R1, E1, Option[Either[A, A2]], Any, E1, Chunk[A3], Unit](
        {
          case Some(Left(a1)) =>
            lastRight match {
              case Some(a2) =>
                ZChannel.write(Chunk.single(f(a1, a2))) *> writer(Some(a1), lastRight)
              case None =>
                writer(Some(a1), lastRight)
            }
          case Some(Right(a2)) =>
            lastLeft match {
              case Some(a1) =>
                ZChannel.write(Chunk.single(f(a1, a2))) *> writer(lastLeft, Some(a2))
              case None =>
                writer(lastLeft, Some(a2))
            }
          case None =>
            writer(lastLeft, lastRight)
        },
        err => ZChannel.fail(err),
        _ => ZChannel.unit
      )

    new ZStream((mergedChannel >>> writer(None, None)))
  }

  /**
   * Zips each element with the next element if present.
   */
  final def zipWithNext: ZStream[R, E, (A, Option[A])] = {
    def process(last: Option[A]): ZChannel[Any, E, Chunk[A], Any, E, Chunk[(A, Option[A])], Unit] =
      ZChannel.readWith(
        in => {
          val (newLast, chunk) = in.mapAccum(last)((prev, curr) => (Some(curr), prev.map((_, curr))))
          val out              = chunk.collect { case Some((prev, curr)) => (prev, Some(curr)) }
          ZChannel.write(out) *> process(newLast)
        },
        err => ZChannel.fail(err),
        _ =>
          last match {
            case Some(value) =>
              ZChannel.write(Chunk.single((value, None))) *> ZChannel.unit
            case None =>
              ZChannel.unit
          }
      )

    new ZStream(self.channel >>> process(None))
  }

  /**
   * Zips each element with the previous element. Initially accompanied by `None`.
   */
  final def zipWithPrevious: ZStream[R, E, (Option[A], A)] =
    mapAccum[Option[A], (Option[A], A)](None)((prev, next) => (Some(next), (prev, next)))

  /**
   * Zips each element with both the previous and next element.
   */
  final def zipWithPreviousAndNext: ZStream[R, E, (Option[A], A, Option[A])] =
    zipWithPrevious.zipWithNext.map { case ((prev, curr), next) => (prev, curr, next.map(_._2)) }
}

object ZStream extends ZStreamPlatformSpecificConstructors {
  def fromPull[R, E, A](zio: ZManaged[R, Nothing, ZIO[R, Option[E], Chunk[A]]]): ZStream[R, E, A] =
    ZStream.unwrapManaged(zio.map(pull => repeatEffectChunkOption(pull)))

  /**
   * The default chunk size used by the various combinators and constructors of [[ZStream]].
   */
  final val DefaultChunkSize = 4096

  /**
   * Submerges the error case of an `Either` into the `ZStream`.
   */
  def absolve[R, E, O](xs: ZStream[R, E, Either[E, O]]): ZStream[R, E, O] =
    xs.mapM(ZIO.fromEither(_))

  /**
   * Accesses the environment of the stream.
   */
  def access[R]: AccessPartiallyApplied[R] =
    new AccessPartiallyApplied[R]

  /**
   * Accesses the environment of the stream in the context of an effect.
   */
  def accessM[R]: AccessMPartiallyApplied[R] =
    new AccessMPartiallyApplied[R]

  /**
   * Accesses the environment of the stream in the context of a stream.
   */
  def accessStream[R]: AccessStreamPartiallyApplied[R] =
    new AccessStreamPartiallyApplied[R]

  /**
   * Creates a pure stream from a variable list of values
   */
  def apply[A](as: A*): ZStream[Any, Nothing, A] = fromIterable(as)

  /**
   * Locks the execution of the specified stream to the blocking executor. Any
   * streams that are composed after this one will automatically be shifted
   * back to the previous executor.
   */
  def blocking[R, E, A](stream: ZStream[R, E, A]): ZStream[R, E, A] =
    ZStream.fromEffect(ZIO.blockingExecutor).flatMap(stream.lock)

  /**
   * Creates a stream from a single value that will get cleaned up after the
   * stream is consumed
   */
  def bracket[R, E, A](acquire: ZIO[R, E, A])(release: A => URIO[R, Any]): ZStream[R, E, A] =
    managed(ZManaged.bracket(acquire)(release))

  /**
   * Creates a stream from a single value that will get cleaned up after the
   * stream is consumed
   */
  def bracketExit[R, E, A](
    acquire: ZIO[R, E, A]
  )(release: (A, Exit[Any, Any]) => URIO[R, Any]): ZStream[R, E, A] =
    managed(ZManaged.bracketExit(acquire)(release))

  /**
   * Composes the specified streams to create a cartesian product of elements
   * with a specified function. Subsequent streams would be run multiple times,
   * for every combination of elements in the prior streams.
   *
   * See also [[ZStream#zipN[R,E,A,B,C]*]] for the more common point-wise variant.
   */
  def crossN[R, E, A, B, C](zStream1: ZStream[R, E, A], zStream2: ZStream[R, E, B])(
    f: (A, B) => C
  ): ZStream[R, E, C] =
    zStream1.crossWith(zStream2)(f)

  /**
   * Composes the specified streams to create a cartesian product of elements
   * with a specified function. Subsequent stream would be run multiple times,
   * for every combination of elements in the prior streams.
   *
   * See also [[ZStream#zipN[R,E,A,B,C,D]*]] for the more common point-wise variant.
   */
  def crossN[R, E, A, B, C, D](
    zStream1: ZStream[R, E, A],
    zStream2: ZStream[R, E, B],
    zStream3: ZStream[R, E, C]
  )(
    f: (A, B, C) => D
  ): ZStream[R, E, D] =
    for {
      a <- zStream1
      b <- zStream2
      c <- zStream3
    } yield f(a, b, c)

  /**
   * Composes the specified streams to create a cartesian product of elements
   * with a specified function. Subsequent stream would be run multiple times,
   * for every combination of elements in the prior streams.
   *
   * See also [[ZStream#zipN[R,E,A,B,C,D,F]*]] for the more common point-wise variant.
   */
  def crossN[R, E, A, B, C, D, F](
    zStream1: ZStream[R, E, A],
    zStream2: ZStream[R, E, B],
    zStream3: ZStream[R, E, C],
    zStream4: ZStream[R, E, D]
  )(
    f: (A, B, C, D) => F
  ): ZStream[R, E, F] =
    for {
      a <- zStream1
      b <- zStream2
      c <- zStream3
      d <- zStream4
    } yield f(a, b, c, d)

  /**
   * Concatenates all of the streams in the chunk to one stream.
   */
  def concatAll[R, E, O](streams: Chunk[ZStream[R, E, O]]): ZStream[R, E, O] =
    streams.foldLeft[ZStream[R, E, O]](empty)(_ ++ _)

  /**
   * The stream that dies with the `ex`.
   */
  def die(ex: => Throwable): ZStream[Any, Nothing, Nothing] =
    fromEffect(ZIO.die(ex))

  /**
   * The stream that dies with an exception described by `msg`.
   */
  def dieMessage(msg: => String): ZStream[Any, Nothing, Nothing] =
    fromEffect(ZIO.dieMessage(msg))

  /**
   * The stream that ends with the [[zio.Exit]] value `exit`.
   */
  def done[E, A](exit: Exit[E, A]): ZStream[Any, E, A] =
    fromEffect(ZIO.done(exit))

  /**
   * The empty stream
   */
  val empty: ZStream[Any, Nothing, Nothing] =
    new ZStream(ZChannel.write(Chunk.empty))

  /**
   * Accesses the whole environment of the stream.
   */
  def environment[R]: ZStream[R, Nothing, R] =
    fromEffect(ZIO.environment[R])

  /**
   * The stream that always fails with the `error`
   */
  def fail[E](error: => E): ZStream[Any, E, Nothing] =
    fromEffect(ZIO.fail(error))

  /**
   * Creates a one-element stream that never fails and executes the finalizer when it ends.
   */
  def finalizer[R](finalizer: URIO[R, Any]): ZStream[R, Nothing, Any] =
    bracket[R, Nothing, Unit](UIO.unit)(_ => finalizer)

  /**
   * Creates a stream from a [[zio.Chunk]] of values
   *
   * @param c a chunk of values
   * @return a finite stream of values
   */
  def fromChunk[O](c: => Chunk[O]): ZStream[Any, Nothing, O] =
    new ZStream(ZChannel.effectSuspendTotal(ZChannel.write(c)))

  /**
   * Creates a stream from a subscription to a hub.
   */
  def fromChunkHub[R, E, O](hub: ZHub[Nothing, R, Any, E, Nothing, Chunk[O]]): ZStream[R, E, O] =
    managed(hub.subscribe).flatMap(queue => fromChunkQueue(queue))

  /**
   * Creates a stream from a subscription to a hub in the context of a managed
   * effect. The managed effect describes subscribing to receive messages from
   * the hub while the stream describes taking messages from the hub.
   */
  def fromChunkHubManaged[R, E, O](
    hub: ZHub[Nothing, R, Any, E, Nothing, Chunk[O]]
  ): ZManaged[Any, Nothing, ZStream[R, E, O]] =
    hub.subscribe.map(queue => fromChunkQueue(queue))

  /**
   * Creates a stream from a subscription to a hub.
   *
   * The hub will be shut down once the stream is closed.
   */
  def fromChunkHubWithShutdown[R, E, O](hub: ZHub[Nothing, R, Any, E, Nothing, Chunk[O]]): ZStream[R, E, O] =
    fromChunkHub(hub).ensuringFirst(hub.shutdown)

  /**
   * Creates a stream from a subscription to a hub in the context of a managed
   * effect. The managed effect describes subscribing to receive messages from
   * the hub while the stream describes taking messages from the hub.
   *
   * The hub will be shut down once the stream is closed.
   */
  def fromChunkHubManagedWithShutdown[R, E, O](
    hub: ZHub[Nothing, R, Any, E, Nothing, Chunk[O]]
  ): ZManaged[Any, Nothing, ZStream[R, E, O]] =
    fromChunkHubManaged(hub).map(_.ensuringFirst(hub.shutdown))

  /**
   * Creates a stream from a queue of values
   */
  def fromChunkQueue[R, E, O](queue: ZQueue[Nothing, R, Any, E, Nothing, Chunk[O]]): ZStream[R, E, O] =
    repeatEffectChunkOption {
      queue.take
        .catchAllCause(c =>
          queue.isShutdown.flatMap { down =>
            if (down && c.interrupted) Pull.end
            else Pull.halt(c)
          }
        )
    }

  /**
   * Creates a stream from a queue of values. The queue will be shutdown once the stream is closed.
   */
  def fromChunkQueueWithShutdown[R, E, O](queue: ZQueue[Nothing, R, Any, E, Nothing, Chunk[O]]): ZStream[R, E, O] =
    fromChunkQueue(queue).ensuringFirst(queue.shutdown)

  /**
   * Creates a stream from an arbitrary number of chunks.
   */
  def fromChunks[O](cs: Chunk[O]*): ZStream[Any, Nothing, O] =
    fromIterable(cs).flatMap(fromChunk(_))

  /**
   * Creates a stream from an effect producing a value of type `A`
   */
  def fromEffect[R, E, A](fa: ZIO[R, E, A]): ZStream[R, E, A] =
    fromEffectOption(fa.mapError(Some(_)))

  /**
   * Creates a stream from an effect producing a value of type `A` or an empty Stream
   */
  def fromEffectOption[R, E, A](fa: ZIO[R, Option[E], A]): ZStream[R, E, A] =
    new ZStream(
      ZChannel.unwrap(
        fa.fold(
          {
            case Some(e) => ZChannel.fail(e)
            case None    => ZChannel.end(())
          },
          a => ZChannel.write(Chunk(a))
        )
      )
    )

  /**
   * Creates a stream from a subscription to a hub.
   */
  def fromHub[R, E, A](
    hub: ZHub[Nothing, R, Any, E, Nothing, A],
    maxChunkSize: Int = DefaultChunkSize
  ): ZStream[R, E, A] =
    managed(hub.subscribe).flatMap(queue => fromQueue(queue, maxChunkSize))

  /**
   * Creates a stream from a subscription to a hub in the context of a managed
   * effect. The managed effect describes subscribing to receive messages from
   * the hub while the stream describes taking messages from the hub.
   */
  def fromHubManaged[R, E, A](
    hub: ZHub[Nothing, R, Any, E, Nothing, A],
    maxChunkSize: Int = DefaultChunkSize
  ): ZManaged[Any, Nothing, ZStream[R, E, A]] =
    hub.subscribe.map(queue => fromQueueWithShutdown(queue, maxChunkSize))

  /**
   * Creates a stream from a subscription to a hub.
   *
   * The hub will be shut down once the stream is closed.
   */
  def fromHubWithShutdown[R, E, A](
    hub: ZHub[Nothing, R, Any, E, Nothing, A],
    maxChunkSize: Int = DefaultChunkSize
  ): ZStream[R, E, A] =
    fromHub(hub, maxChunkSize).ensuringFirst(hub.shutdown)

  /**
   * Creates a stream from a subscription to a hub in the context of a managed
   * effect. The managed effect describes subscribing to receive messages from
   * the hub while the stream describes taking messages from the hub.
   *
   * The hub will be shut down once the stream is closed.
   */
  def fromHubManagedWithShutdown[R, E, A](
    hub: ZHub[Nothing, R, Any, E, Nothing, A],
    maxChunkSize: Int = DefaultChunkSize
  ): ZManaged[Any, Nothing, ZStream[R, E, A]] =
    fromHubManaged(hub, maxChunkSize).map(_.ensuringFirst(hub.shutdown))

  /**
   * Creates a stream from an iterable collection of values
   */
  def fromIterable[O](as: => Iterable[O]): ZStream[Any, Nothing, O] =
    fromChunk(Chunk.fromIterable(as))

  /**
   * Creates a stream from an effect producing a value of type `Iterable[A]`
   */
  def fromIterableM[R, E, O](iterable: ZIO[R, E, Iterable[O]]): ZStream[R, E, O] =
    fromEffect(iterable).mapConcat(identity)

  def fromIterator[A](iterator: => Iterator[A]): ZStream[Any, Throwable, A] = {
    object StreamEnd extends Throwable

    ZStream.fromEffect(Task(iterator) <*> ZIO.runtime[Any]).flatMap { case (it, rt) =>
      ZStream.repeatEffectOption {
        Task {
          val hasNext: Boolean =
            try it.hasNext
            catch {
              case e: Throwable if !rt.platform.fatal(e) =>
                throw e
            }

          if (hasNext) {
            try it.next()
            catch {
              case e: Throwable if !rt.platform.fatal(e) =>
                throw e
            }
          } else throw StreamEnd
        }.mapError {
          case StreamEnd => None
          case e         => Some(e)
        }
      }
    }
  }

  /**
   * Creates a stream from an iterator that may potentially throw exceptions
   */
  def fromIteratorEffect[R, A](
    iterator: ZIO[R, Throwable, Iterator[A]]
  ): ZStream[R, Throwable, A] =
    fromEffect(iterator).flatMap(fromIterator(_))

  /**
   * Creates a stream from a managed iterator
   */
  def fromIteratorManaged[R, A](iterator: ZManaged[R, Throwable, Iterator[A]]): ZStream[R, Throwable, A] =
    managed(iterator).flatMap(fromIterator(_))

  /**
   * Creates a stream from an iterator
   */
  def fromIteratorTotal[A](iterator: => Iterator[A]): ZStream[Any, Nothing, A] = {
    def loop(iterator: Iterator[A]): ZChannel[Any, Any, Any, Any, Nothing, Chunk[A], Any] =
      ZChannel.unwrap {
        UIO {
          if (iterator.hasNext)
            ZChannel.write(Chunk.single(iterator.next())) *> loop(iterator)
          else ZChannel.end(())
        }
      }

    new ZStream(loop(iterator))
  }

  /**
   * Creates a stream from a Java iterator that may throw exceptions
   */
  def fromJavaIterator[A](iterator: => java.util.Iterator[A]): ZStream[Any, Throwable, A] =
    fromIterator {
      val it = iterator // Scala 2.13 scala.collection.Iterator has `iterator` in local scope
      new Iterator[A] {
        def next(): A        = it.next
        def hasNext: Boolean = it.hasNext
      }
    }

  /**
   * Creates a stream from a Java iterator that may potentially throw exceptions
   */
  def fromJavaIteratorEffect[R, A](
    iterator: ZIO[R, Throwable, java.util.Iterator[A]]
  ): ZStream[R, Throwable, A] =
    fromEffect(iterator).flatMap(fromJavaIterator(_))

  /**
   * Creates a stream from a managed iterator
   */
  def fromJavaIteratorManaged[R, A](iterator: ZManaged[R, Throwable, java.util.Iterator[A]]): ZStream[R, Throwable, A] =
    managed(iterator).flatMap(fromJavaIterator(_))

  /**
   * Creates a stream from a Java iterator
   */
  def fromJavaIteratorTotal[A](iterator: => java.util.Iterator[A]): ZStream[Any, Nothing, A] =
    fromIteratorTotal {
      val it = iterator // Scala 2.13 scala.collection.Iterator has `iterator` in local scope
      new Iterator[A] {
        def next(): A        = it.next
        def hasNext: Boolean = it.hasNext
      }
    }

  /**
   * Creates a stream from a queue of values
   *
   * @param maxChunkSize Maximum number of queued elements to put in one chunk in the stream
   */
  def fromQueue[R, E, O](
    queue: ZQueue[Nothing, R, Any, E, Nothing, O],
    maxChunkSize: Int = DefaultChunkSize
  ): ZStream[R, E, O] =
    repeatEffectChunkOption {
      queue
        .takeBetween(1, maxChunkSize)
        .map(Chunk.fromIterable)
        .catchAllCause(c =>
          queue.isShutdown.flatMap { down =>
            if (down && c.interrupted) Pull.end
            else Pull.halt(c)
          }
        )
    }

  /**
   * Creates a stream from a queue of values. The queue will be shutdown once the stream is closed.
   *
   * @param maxChunkSize Maximum number of queued elements to put in one chunk in the stream
   */
  def fromQueueWithShutdown[R, E, O](
    queue: ZQueue[Nothing, R, Any, E, Nothing, O],
    maxChunkSize: Int = DefaultChunkSize
  ): ZStream[R, E, O] =
    fromQueue(queue, maxChunkSize).ensuringFirst(queue.shutdown)

  /**
   * Creates a stream from a [[zio.Schedule]] that does not require any further
   * input. The stream will emit an element for each value output from the
   * schedule, continuing for as long as the schedule continues.
   */
  def fromSchedule[R, A](schedule: Schedule[R, Any, A]): ZStream[R with Has[Clock], Nothing, A] =
    unwrap(schedule.driver.map(driver => repeatEffectOption(driver.next(()))))

  /**
   * Creates a stream from a [[zio.stm.TQueue]] of values.
   */
  def fromTQueue[A](queue: TQueue[A]): ZStream[Any, Nothing, A] =
    repeatEffectChunk(queue.take.map(Chunk.single(_)).commit)

  /**
   * The stream that always halts with `cause`.
   */
  def halt[E](cause: => Cause[E]): ZStream[Any, E, Nothing] =
    fromEffect(ZIO.halt(cause))

  /**
   * The infinite stream of iterative function application: a, f(a), f(f(a)), f(f(f(a))), ...
   */
  def iterate[A](a: A)(f: A => A): ZStream[Any, Nothing, A] =
    unfold(a)(a => Some((a, f(a))))

  /**
   * Creates a single-valued stream from a managed resource
   */
  def managed[R, E, A](managed: ZManaged[R, E, A]): ZStream[R, E, A] =
    new ZStream(ZChannel.managedOut(managed.map(Chunk.single)))

  /**
   * Merges a variable list of streams in a non-deterministic fashion.
   * Up to `n` streams may be consumed in parallel and up to
   * `outputBuffer` chunks may be buffered by this operator.
   */
  def mergeAll[R, E, O](n: Int, outputBuffer: Int = 16)(
    streams: ZStream[R, E, O]*
  ): ZStream[R, E, O] =
    fromIterable(streams).flattenPar(n, outputBuffer)

  /**
   * Like [[mergeAll]], but runs all streams concurrently.
   */
  def mergeAllUnbounded[R, E, O](outputBuffer: Int = 16)(
    streams: ZStream[R, E, O]*
  ): ZStream[R, E, O] = mergeAll(Int.MaxValue, outputBuffer)(streams: _*)

  /**
   * The stream that never produces any value or fails with any error.
   */
  val never: ZStream[Any, Nothing, Nothing] =
    ZStream.fromEffect(ZIO.never)

  /**
   * Like [[unfoldM]], but allows the emission of values to end one step further than
   * the unfolding of the state. This is useful for embedding paginated APIs,
   * hence the name.
   */
  def paginate[R, E, A, S](s: S)(f: S => (A, Option[S])): ZStream[Any, Nothing, A] =
    paginateChunk(s) { s =>
      val page = f(s)
      Chunk.single(page._1) -> page._2
    }

  /**
   * Like [[unfoldM]], but allows the emission of values to end one step further than
   * the unfolding of the state. This is useful for embedding paginated APIs,
   * hence the name.
   */
  def paginateM[R, E, A, S](s: S)(f: S => ZIO[R, E, (A, Option[S])]): ZStream[R, E, A] =
    paginateChunkM(s)(f(_).map { case (a, s) => Chunk.single(a) -> s })

  /**
   * Like [[unfoldChunk]], but allows the emission of values to end one step further than
   * the unfolding of the state. This is useful for embedding paginated APIs,
   * hence the name.
   */
  def paginateChunk[A, S](s: S)(f: S => (Chunk[A], Option[S])): ZStream[Any, Nothing, A] = {
    def loop(s: S): ZChannel[Any, Any, Any, Any, Nothing, Chunk[A], Any] =
      f(s) match {
        case (as, Some(s)) => ZChannel.write(as) *> loop(s)
        case (as, None)    => ZChannel.write(as) *> ZChannel.end(())
      }

    new ZStream(loop(s))
  }

  /**
   * Like [[unfoldChunkM]], but allows the emission of values to end one step further than
   * the unfolding of the state. This is useful for embedding paginated APIs,
   * hence the name.
   */
  def paginateChunkM[R, E, A, S](s: S)(f: S => ZIO[R, E, (Chunk[A], Option[S])]): ZStream[R, E, A] = {
    def loop(s: S): ZChannel[R, Any, Any, Any, E, Chunk[A], Any] =
      ZChannel.unwrap {
        f(s).map {
          case (as, Some(s)) => ZChannel.write(as) *> loop(s)
          case (as, None)    => ZChannel.write(as) *> ZChannel.end(())
        }
      }

    new ZStream(loop(s))
  }

  /**
   * Constructs a stream from a range of integers (lower bound included, upper bound not included)
   */
  def range(min: Int, max: Int, chunkSize: Int = DefaultChunkSize): ZStream[Any, Nothing, Int] = {
    def go(current: Int): ZChannel[Any, Any, Any, Any, Nothing, Chunk[Int], Any] = {
      val remaining = max - current

      if (remaining > chunkSize)
        ZChannel.write(Chunk.fromArray(Array.range(current, current + chunkSize))) *> go(current + chunkSize)
      else {
        ZChannel.write(Chunk.fromArray(Array.range(current, current + remaining)))
      }
    }

    new ZStream(go(min))
  }

  /**
   * Repeats the provided value infinitely.
   */
  def repeat[A](a: => A): ZStream[Any, Nothing, A] =
    new ZStream(ZChannel.write(Chunk.single(a)).repeated)

  /**
   * Creates a stream from an effect producing a value of type `A` which repeats forever.
   */
  def repeatEffect[R, E, A](fa: ZIO[R, E, A]): ZStream[R, E, A] =
    repeatEffectOption(fa.mapError(Some(_)))

  /**
   * Creates a stream from an effect producing values of type `A` until it fails with None.
   */
  def repeatEffectOption[R, E, A](fa: ZIO[R, Option[E], A]): ZStream[R, E, A] =
    repeatEffectChunkOption(fa.map(Chunk.single(_)))

  /**
   * Creates a stream from an effect producing chunks of `A` values which repeats forever.
   */
  def repeatEffectChunk[R, E, A](fa: ZIO[R, E, Chunk[A]]): ZStream[R, E, A] =
    repeatEffectChunkOption(fa.mapError(Some(_)))

  /**
   * Creates a stream from an effect producing chunks of `A` values until it fails with None.
   */
  def repeatEffectChunkOption[R, E, A](fa: ZIO[R, Option[E], Chunk[A]]): ZStream[R, E, A] =
    unfoldChunkM(())(_ =>
      fa.map(chunk => Some((chunk, ()))).catchAll {
        case None    => ZIO.none
        case Some(e) => ZIO.fail(e)
      }
    )

  /**
   * Creates a stream from an effect producing a value of type `A`, which is repeated using the
   * specified schedule.
   */
  def repeatEffectWith[R, E, A](effect: ZIO[R, E, A], schedule: Schedule[R, A, Any]): ZStream[R with Has[Clock], E, A] =
    ZStream.fromEffect(effect zip schedule.driver).flatMap { case (a, driver) =>
      ZStream.succeed(a) ++
        ZStream.unfoldM(a)(driver.next(_).foldM(ZIO.succeed(_), _ => effect.map(nextA => Some(nextA -> nextA))))
    }

  /**
   * Repeats the value using the provided schedule.
   */
  def repeatWith[R, A](a: => A, schedule: Schedule[R, A, _]): ZStream[R with Has[Clock], Nothing, A] =
    repeatEffectWith(UIO.succeed(a), schedule)

  /**
   * Accesses the specified service in the environment of the effect.
   */
  def service[A: Tag]: ZStream[Has[A], Nothing, A] =
    ZStream.access(_.get[A])

  /**
   * Accesses the specified services in the environment of the effect.
   */
  def services[A: Tag, B: Tag]: ZStream[Has[A] with Has[B], Nothing, (A, B)] =
    ZStream.access(r => (r.get[A], r.get[B]))

  /**
   * Accesses the specified services in the environment of the effect.
   */
  def services[A: Tag, B: Tag, C: Tag]: ZStream[Has[A] with Has[B] with Has[C], Nothing, (A, B, C)] =
    ZStream.access(r => (r.get[A], r.get[B], r.get[C]))

  /**
   * Accesses the specified services in the environment of the effect.
   */
  def services[A: Tag, B: Tag, C: Tag, D: Tag]
    : ZStream[Has[A] with Has[B] with Has[C] with Has[D], Nothing, (A, B, C, D)] =
    ZStream.access(r => (r.get[A], r.get[B], r.get[C], r.get[D]))

  /**
   * Creates a single-valued pure stream
   */
  def succeed[A](a: => A): ZStream[Any, Nothing, A] =
    fromChunk(Chunk.single(a))

  /**
   * A stream that emits Unit values spaced by the specified duration.
   */
  def tick(interval: Duration): ZStream[Has[Clock], Nothing, Unit] =
    repeatWith((), Schedule.spaced(interval))

  /**
   * A stream that contains a single `Unit` value.
   */
  val unit: ZStream[Any, Nothing, Unit] =
    succeed(())

  /**
   * Creates a stream by peeling off the "layers" of a value of type `S`
   */
  def unfold[S, A](s: S)(f: S => Option[(A, S)]): ZStream[Any, Nothing, A] =
    unfoldChunk(s)(f(_).map { case (a, s) => Chunk.single(a) -> s })

  /**
   * Creates a stream by effectfully peeling off the "layers" of a value of type `S`
   */
  def unfoldM[R, E, A, S](s: S)(f: S => ZIO[R, E, Option[(A, S)]]): ZStream[R, E, A] =
    unfoldChunkM(s)(f(_).map(_.map { case (a, s) => Chunk.single(a) -> s }))

  /**
   * Creates a stream by peeling off the "layers" of a value of type `S`.
   */
  def unfoldChunk[S, A](s: S)(f: S => Option[(Chunk[A], S)]): ZStream[Any, Nothing, A] = {
    def loop(s: S): ZChannel[Any, Any, Any, Any, Nothing, Chunk[A], Any] =
      f(s) match {
        case Some((as, s)) => ZChannel.write(as) *> loop(s)
        case None          => ZChannel.end(())
      }

    new ZStream(loop(s))
  }

  /**
   * Creates a stream by effectfully peeling off the "layers" of a value of type `S`
   */
  def unfoldChunkM[R, E, A, S](s: S)(f: S => ZIO[R, E, Option[(Chunk[A], S)]]): ZStream[R, E, A] = {
    def loop(s: S): ZChannel[R, Any, Any, Any, E, Chunk[A], Any] =
      ZChannel.unwrap {
        f(s).map {
          case Some((as, s)) => ZChannel.write(as) *> loop(s)
          case None          => ZChannel.end(())
        }
      }

    new ZStream(loop(s))
  }

  /**
   * Creates a stream produced from an effect
   */
  def unwrap[R, E, A](fa: ZIO[R, E, ZStream[R, E, A]]): ZStream[R, E, A] =
    fromEffect(fa).flatten

  /**
   * Creates a stream produced from a [[ZManaged]]
   */
  def unwrapManaged[R, E, A](fa: ZManaged[R, E, ZStream[R, E, A]]): ZStream[R, E, A] =
    managed(fa).flatten

  /**
   * Zips the specified streams together with the specified function.
   */
  def zipN[R, E, A, B, C](zStream1: ZStream[R, E, A], zStream2: ZStream[R, E, B])(
    f: (A, B) => C
  ): ZStream[R, E, C] =
    zStream1.zipWith(zStream2)(f)

  /**
   * Zips with specified streams together with the specified function.
   */
  def zipN[R, E, A, B, C, D](zStream1: ZStream[R, E, A], zStream2: ZStream[R, E, B], zStream3: ZStream[R, E, C])(
    f: (A, B, C) => D
  ): ZStream[R, E, D] =
    (zStream1 <&> zStream2 <&> zStream3).map { case ((a, b), c) =>
      f(a, b, c)
    }

  /**
   * Returns an effect that executes the specified effects in parallel,
   * combining their results with the specified `f` function. If any effect
   * fails, then the other effects will be interrupted.
   */
  def zipN[R, E, A, B, C, D, F](
    zStream1: ZStream[R, E, A],
    zStream2: ZStream[R, E, B],
    zStream3: ZStream[R, E, C],
    zStream4: ZStream[R, E, D]
  )(f: (A, B, C, D) => F): ZStream[R, E, F] =
    (zStream1 <&> zStream2 <&> zStream3 <&> zStream4).map { case (((a, b), c), d) =>
      f(a, b, c, d)
    }

  final class AccessPartiallyApplied[R](private val dummy: Boolean = true) extends AnyVal {
    def apply[A](f: R => A): ZStream[R, Nothing, A] =
      ZStream.environment[R].map(f)
  }

  final class AccessMPartiallyApplied[R](private val dummy: Boolean = true) extends AnyVal {
    def apply[E, A](f: R => ZIO[R, E, A]): ZStream[R, E, A] =
      ZStream.environment[R].mapM(f)
  }

  final class AccessStreamPartiallyApplied[R](private val dummy: Boolean = true) extends AnyVal {
    def apply[E, A](f: R => ZStream[R, E, A]): ZStream[R, E, A] =
      ZStream.environment[R].flatMap(f)
  }

  /**
   * Representation of a grouped stream.
   * This allows to filter which groups will be processed.
   * Once this is applied all groups will be processed in parallel and the results will
   * be merged in arbitrary order.
   */
  final class GroupBy[-R, +E, +K, +V](
    private val grouped: ZStream[R, E, (K, Dequeue[Exit[Option[E], V]])],
    private val buffer: Int
  ) {

    /**
     * Only consider the first n groups found in the stream.
     */
    def first(n: Int): GroupBy[R, E, K, V] = {
      val g1 = grouped.zipWithIndex.filterM { case elem @ ((_, q), i) =>
        if (i < n) ZIO.succeedNow(elem).as(true)
        else q.shutdown.as(false)
      }.map(_._1)
      new GroupBy(g1, buffer)
    }

    /**
     * Filter the groups to be processed.
     */
    def filter(f: K => Boolean): GroupBy[R, E, K, V] = {
      val g1 = grouped.filterM { case elem @ (k, q) =>
        if (f(k)) ZIO.succeedNow(elem).as(true)
        else q.shutdown.as(false)
      }
      new GroupBy(g1, buffer)
    }

    /**
     * Run the function across all groups, collecting the results in an arbitrary order.
     */
    def apply[R1 <: R, E1 >: E, A](f: (K, ZStream[Any, E, V]) => ZStream[R1, E1, A]): ZStream[R1, E1, A] =
      grouped.flatMapPar[R1, E1, A](Int.MaxValue) { case (k, q) =>
        f(k, ZStream.fromQueueWithShutdown(q).flattenExitOption)
      }
  }

  final class ProvideSomeLayer[R0, -R, +E, +A](private val self: ZStream[R, E, A]) extends AnyVal {
    def apply[E1 >: E, R1](
      layer: ZLayer[R0, E1, R1]
    )(implicit ev1: R0 with R1 <:< R, ev2: Has.Union[R0, R1], tagged: Tag[R1]): ZStream[R0, E1, A] =
      self.provideLayer[E1, R0, R0 with R1](ZLayer.identity[R0] ++ layer)
  }

  final class UpdateService[-R, +E, +A, M](private val self: ZStream[R, E, A]) extends AnyVal {
    def apply[R1 <: R with Has[M]](f: M => M)(implicit ev: Has.IsHas[R1], tag: Tag[M]): ZStream[R1, E, A] =
      self.provideSome(ev.update(_, f))
  }

  type Pull[-R, +E, +A] = ZIO[R, Option[E], Chunk[A]]

  private[zio] object Pull {
    def emit[A](a: A): IO[Nothing, Chunk[A]]                                      = UIO(Chunk.single(a))
    def emit[A](as: Chunk[A]): IO[Nothing, Chunk[A]]                              = UIO(as)
    def fromDequeue[E, A](d: Dequeue[stream.Take[E, A]]): IO[Option[E], Chunk[A]] = d.take.flatMap(_.done)
    def fail[E](e: E): IO[Option[E], Nothing]                                     = IO.fail(Some(e))
    def halt[E](c: Cause[E]): IO[Option[E], Nothing]                              = IO.halt(c).mapError(Some(_))
    def empty[A]: IO[Nothing, Chunk[A]]                                           = UIO(Chunk.empty)
    val end: IO[Option[Nothing], Nothing]                                         = IO.fail(None)
  }

  @deprecated("use zio.stream.Take instead", "1.0.0")
  type Take[+E, +A] = Exit[Option[E], Chunk[A]]

  object Take {
    @deprecated("use zio.stream.Take.end instead", "1.0.0")
    val End: Exit[Option[Nothing], Nothing] = Exit.fail(None)
  }

  private[zio] case class BufferedPull[R, E, A](
    upstream: ZIO[R, Option[E], Chunk[A]],
    done: Ref[Boolean],
    cursor: Ref[(Chunk[A], Int)]
  ) {
    def ifNotDone[R1, E1, A1](fa: ZIO[R1, Option[E1], A1]): ZIO[R1, Option[E1], A1] =
      done.get.flatMap(
        if (_) Pull.end
        else fa
      )

    def update: ZIO[R, Option[E], Unit] =
      ifNotDone {
        upstream.foldM(
          {
            case None    => done.set(true) *> Pull.end
            case Some(e) => Pull.fail(e)
          },
          chunk => cursor.set(chunk -> 0)
        )
      }

    def pullElement: ZIO[R, Option[E], A] =
      ifNotDone {
        cursor.modify { case (chunk, idx) =>
          if (idx >= chunk.size) (update *> pullElement, (Chunk.empty, 0))
          else (UIO.succeedNow(chunk(idx)), (chunk, idx + 1))
        }.flatten
      }

    def pullChunk: ZIO[R, Option[E], Chunk[A]] =
      ifNotDone {
        cursor.modify { case (chunk, idx) =>
          if (idx >= chunk.size) (update *> pullChunk, (Chunk.empty, 0))
          else (UIO.succeedNow(chunk.drop(idx)), (Chunk.empty, 0))
        }.flatten
      }

  }

  private[zio] object BufferedPull {
    def make[R, E, A](
      pull: ZIO[R, Option[E], Chunk[A]]
    ): ZIO[R, Nothing, BufferedPull[R, E, A]] =
      for {
        done   <- Ref.make(false)
        cursor <- Ref.make[(Chunk[A], Int)](Chunk.empty -> 0)
      } yield BufferedPull(pull, done, cursor)
  }

  /**
   * A synchronous queue-like abstraction that allows a producer to offer
   * an element and wait for it to be taken, and allows a consumer to wait
   * for an element to be available.
   */
  private[zio] class Handoff[A](ref: Ref[Handoff.State[A]]) {
    def offer(a: A): UIO[Unit] =
      Promise.make[Nothing, Unit].flatMap { p =>
        ref.modify {
          case s @ Handoff.State.Full(_, notifyProducer) => (notifyProducer.await *> offer(a), s)
          case Handoff.State.Empty(notifyConsumer)       => (notifyConsumer.succeed(()) *> p.await, Handoff.State.Full(a, p))
        }.flatten
      }

    def take: UIO[A] =
      Promise.make[Nothing, Unit].flatMap { p =>
        ref.modify {
          case Handoff.State.Full(a, notifyProducer)   => (notifyProducer.succeed(()).as(a), Handoff.State.Empty(p))
          case s @ Handoff.State.Empty(notifyConsumer) => (notifyConsumer.await *> take, s)
        }.flatten
      }

    def poll: UIO[Option[A]] =
      Promise.make[Nothing, Unit].flatMap { p =>
        ref.modify {
          case Handoff.State.Full(a, notifyProducer) => (notifyProducer.succeed(()).as(Some(a)), Handoff.State.Empty(p))
          case s @ Handoff.State.Empty(_)            => (ZIO.succeedNow(None), s)
        }.flatten
      }
  }

  private[zio] object Handoff {
    def make[A]: UIO[Handoff[A]] =
      Promise
        .make[Nothing, Unit]
        .flatMap(p => Ref.make[State[A]](State.Empty(p)))
        .map(new Handoff(_))

    sealed trait State[+A]
    object State {
      case class Empty(notifyConsumer: Promise[Nothing, Unit])          extends State[Nothing]
      case class Full[+A](a: A, notifyProducer: Promise[Nothing, Unit]) extends State[A]
    }
  }

  sealed trait TerminationStrategy
  object TerminationStrategy {
    case object Left   extends TerminationStrategy
    case object Right  extends TerminationStrategy
    case object Both   extends TerminationStrategy
    case object Either extends TerminationStrategy
  }

  implicit final class RefineToOrDieOps[R, E <: Throwable, A](private val self: ZStream[R, E, A]) extends AnyVal {

    /**
     * Keeps some of the errors, and terminates the fiber with the rest.
     */
    def refineToOrDie[E1 <: E: ClassTag](implicit ev: CanFail[E]): ZStream[R, E1, A] =
      self.refineOrDie { case e: E1 => e }
  }

  private[zio] class Rechunker[A](n: Int) {
    private var builder: ChunkBuilder[A] = ChunkBuilder.make(n)
    private var pos: Int                 = 0

    def write(elem: A): Chunk[A] = {
      builder += elem
      pos += 1
      if (pos == n) {
        val result = builder.result()
        builder = ChunkBuilder.make(n)
        pos = 0
        result
      } else {
        null
      }
    }

    def emitIfNotEmpty(): ZChannel[Any, Any, Any, Any, Nothing, Chunk[A], Unit] =
      if (pos != 0) {
        ZChannel.write(builder.result())
      } else {
        ZChannel.unit
      }
  }

  private[zio] sealed trait SinkEndReason[+C]
  private[zio] object SinkEndReason {
    case object SinkEnd             extends SinkEndReason[Nothing]
    case object ScheduleTimeout     extends SinkEndReason[Nothing]
    case class ScheduleEnd[C](c: C) extends SinkEndReason[C]
    case object UpstreamEnd         extends SinkEndReason[Nothing]
  }

  private[zio] sealed trait HandoffSignal[C, E, A]
  private[zio] object HandoffSignal {
    case class Emit[C, E, A](els: Chunk[A])           extends HandoffSignal[C, E, A]
    case class Halt[C, E, A](error: Cause[E])         extends HandoffSignal[C, E, A]
    case class End[C, E, A](reason: SinkEndReason[C]) extends HandoffSignal[C, E, A]
  }

  private[zio] sealed trait DebounceState[+E, +A]
  private[zio] object DebounceState {
    case object NotStarted                                               extends DebounceState[Nothing, Nothing]
    case class Previous[A](fiber: Fiber[Nothing, Chunk[A]])              extends DebounceState[Nothing, A]
    case class Current[E, A](fiber: Fiber[E, HandoffSignal[Unit, E, A]]) extends DebounceState[E, A]
  }

}

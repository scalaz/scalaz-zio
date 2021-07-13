package zio.stream.experimental

import zio._

import java.util.concurrent.atomic.AtomicReference

class ZSink[-R, -InErr, -In, +OutErr, +L, +Z](val channel: ZChannel[R, InErr, Chunk[In], Any, OutErr, Chunk[L], Z])
    extends AnyVal { self =>

  /**
   * Operator alias for [[race]].
   */
  final def |[R1 <: R, InErr1 <: InErr, OutErr1 >: OutErr, A0, In1 <: In, L1 >: L, Z1 >: Z](
    that: ZSink[R1, InErr1, In1, OutErr1, L1, Z1]
  ): ZSink[R1, InErr1, In1, OutErr1, L1, Z1] =
    race(that)

  /**
   * Operator alias for [[zip]].
   */
  final def <*>[R1 <: R, InErr1 <: InErr, OutErr1 >: OutErr, A0, In1 <: In, L1 >: L <: In1, Z1](
    that: ZSink[R1, InErr1, In1, OutErr1, L1, Z1]
  )(implicit zippable: Zippable[Z, Z1], ev: L <:< In1): ZSink[R1, InErr1, In1, OutErr1, L1, zippable.Out] =
    zip(that)

  /**
   * Operator alias for [[zipPar]].
   */
  final def <&>[R1 <: R, InErr1 <: InErr, OutErr1 >: OutErr, A0, In1 <: In, L1 >: L <: In1, Z1](
    that: ZSink[R1, InErr1, In1, OutErr1, L1, Z1]
  )(implicit zippable: Zippable[Z, Z1]): ZSink[R1, InErr1, In1, OutErr1, L1, zippable.Out] =
    zipPar(that)

  /**
   * Operator alias for [[zipRight]].
   */
  final def *>[R1 <: R, InErr1 <: InErr, OutErr1 >: OutErr, A0, In1 <: In, L1 >: L <: In1, Z1](
    that: ZSink[R1, InErr1, In1, OutErr1, L1, Z1]
  )(implicit ev: L <:< In1): ZSink[R1, InErr1, In1, OutErr1, L1, Z1] =
    zipRight(that)

  /**
   * Operator alias for [[zipParRight]].
   */
  final def &>[R1 <: R, InErr1 <: InErr, OutErr1 >: OutErr, A0, In1 <: In, L1 >: L <: In1, Z1](
    that: ZSink[R1, InErr1, In1, OutErr1, L1, Z1]
  )(implicit ev: L <:< In1): ZSink[R1, InErr1, In1, OutErr1, L1, Z1] =
    zipParRight(that)

  /**
   * Operator alias for [[zipLeft]].
   */
  final def <*[R1 <: R, InErr1 <: InErr, OutErr1 >: OutErr, A0, In1 <: In, L1 >: L <: In1, Z1](
    that: ZSink[R1, InErr1, In1, OutErr1, L1, Z1]
  )(implicit ev: L <:< In1): ZSink[R1, InErr1, In1, OutErr1, L1, Z] =
    zipLeft(that)

  /**
   * Operator alias for [[zipParLeft]].
   */
  final def <&[R1 <: R, InErr1 <: InErr, OutErr1 >: OutErr, A0, In1 <: In, L1 >: L <: In1, Z1](
    that: ZSink[R1, InErr1, In1, OutErr1, L1, Z1]
  )(implicit ev: L <:< In1): ZSink[R1, InErr1, In1, OutErr1, L1, Z] =
    zipParLeft(that)

  /**
   * Replaces this sink's result with the provided value.
   */
  def as[Z2](z: => Z2): ZSink[R, InErr, In, OutErr, L, Z2] =
    map(_ => z)

  /**
   * Repeatedly runs the sink for as long as its results satisfy
   * the predicate `p`. The sink's results will be accumulated
   * using the stepping function `f`.
   */
  def collectAllWhileWith[S](z: S)(p: Z => Boolean)(f: (S, Z) => S)(implicit
    ev: L <:< In
  ): ZSink[R, InErr, In, OutErr, L, S] =
    new ZSink(
      ZChannel
        .fromZIO(Ref.make(Chunk[In]()).zip(Ref.make(false)))
        .flatMap { case (leftoversRef, upstreamDoneRef) =>
          lazy val upstreamMarker: ZChannel[Any, InErr, Chunk[In], Any, InErr, Chunk[In], Any] =
            ZChannel.readWith(
              (in: Chunk[In]) => ZChannel.write(in) *> upstreamMarker,
              ZChannel.fail(_: InErr),
              (x: Any) => ZChannel.fromZIO(upstreamDoneRef.set(true)).as(x)
            )

          def loop(currentResult: S): ZChannel[R, InErr, Chunk[In], Any, OutErr, Chunk[L], S] =
            channel.doneCollect
              .foldChannel(
                ZChannel.fail(_),
                { case (leftovers, doneValue) =>
                  if (p(doneValue)) {
                    for {
                      _                <- ZChannel.fromZIO(leftoversRef.set(leftovers.flatten.asInstanceOf[Chunk[In]]))
                      upstreamDone     <- ZChannel.fromZIO(upstreamDoneRef.get)
                      accumulatedResult = f(currentResult, doneValue)
                      result <- if (upstreamDone)
                                  ZChannel.write(leftovers.flatten).as(accumulatedResult)
                                else loop(accumulatedResult)
                    } yield result
                  } else ZChannel.write(leftovers.flatten).as(currentResult)
                }
              )

          upstreamMarker >>> ZChannel.bufferChunk(leftoversRef) >>> loop(z)
        }
    )

  /**
   * Transforms this sink's input elements.
   */
  def contramap[In1](f: In1 => In): ZSink[R, InErr, In1, OutErr, L, Z] =
    contramapChunks(_.map(f))

  /**
   * Transforms this sink's input chunks.
   * `f` must preserve chunking-invariance
   */
  def contramapChunks[In1](f: Chunk[In1] => Chunk[In]): ZSink[R, InErr, In1, OutErr, L, Z] = {
    lazy val loop: ZChannel[R, InErr, Chunk[In1], Any, InErr, Chunk[In], Any] =
      ZChannel.readWith[R, InErr, Chunk[In1], Any, InErr, Chunk[In], Any](
        chunk => ZChannel.write(f(chunk)) *> loop,
        ZChannel.fail(_),
        ZChannel.succeed(_)
      )
    new ZSink(loop >>> self.channel)
  }

  /**
   * Effectfully transforms this sink's input chunks.
   * `f` must preserve chunking-invariance
   */
  @deprecated("use contramapChunksZIO", "2.0.0")
  def contramapChunksM[R1 <: R, InErr1 <: InErr, In1](
    f: Chunk[In1] => ZIO[R1, InErr1, Chunk[In]]
  ): ZSink[R1, InErr1, In1, OutErr, L, Z] =
    contramapChunksZIO(f)

  /**
   * Effectfully transforms this sink's input chunks.
   * `f` must preserve chunking-invariance
   */
  def contramapChunksZIO[R1 <: R, InErr1 <: InErr, In1](
    f: Chunk[In1] => ZIO[R1, InErr1, Chunk[In]]
  ): ZSink[R1, InErr1, In1, OutErr, L, Z] = {
    lazy val loop: ZChannel[R1, InErr1, Chunk[In1], Any, InErr1, Chunk[In], Any] =
      ZChannel.readWith[R1, InErr1, Chunk[In1], Any, InErr1, Chunk[In], Any](
        chunk => ZChannel.fromZIO(f(chunk)).flatMap(ZChannel.write) *> loop,
        ZChannel.fail(_),
        ZChannel.succeed(_)
      )
    new ZSink(loop >>> self.channel)
  }

  /**
   * Effectfully transforms this sink's input elements.
   */
  @deprecated("use contramapZIO", "2.0.0")
  def contramapM[R1 <: R, InErr1 <: InErr, In1](
    f: In1 => ZIO[R1, InErr1, In]
  ): ZSink[R1, InErr1, In1, OutErr, L, Z] =
    contramapZIO(f)

  /**
   * Effectfully transforms this sink's input elements.
   */
  def contramapZIO[R1 <: R, InErr1 <: InErr, In1](
    f: In1 => ZIO[R1, InErr1, In]
  ): ZSink[R1, InErr1, In1, OutErr, L, Z] =
    contramapChunksZIO(_.mapZIO(f))

  /**
   * Transforms both inputs and result of this sink using the provided functions.
   */
  def dimap[In1, Z1](f: In1 => In, g: Z => Z1): ZSink[R, InErr, In1, OutErr, L, Z1] =
    contramap(f).map(g)

  /**
   * Transforms both input chunks and result of this sink using the provided functions.
   */
  def dimapChunks[In1, Z1](f: Chunk[In1] => Chunk[In], g: Z => Z1): ZSink[R, InErr, In1, OutErr, L, Z1] =
    contramapChunks(f).map(g)

  /**
   * Effectfully transforms both input chunks and result of this sink using the provided functions.
   * `f` and `g` must preserve chunking-invariance
   */
  @deprecated("use dimapChunksZIO", "2.0.0")
  def dimapChunksM[R1 <: R, InErr1 <: InErr, OutErr1 >: OutErr, In1, Z1](
    f: Chunk[In1] => ZIO[R1, InErr1, Chunk[In]],
    g: Z => ZIO[R1, OutErr1, Z1]
  ): ZSink[R1, InErr1, In1, OutErr1, L, Z1] =
    dimapChunksZIO(f, g)

  /**
   * Effectfully transforms both input chunks and result of this sink using the provided functions.
   * `f` and `g` must preserve chunking-invariance
   */
  def dimapChunksZIO[R1 <: R, InErr1 <: InErr, OutErr1 >: OutErr, In1, Z1](
    f: Chunk[In1] => ZIO[R1, InErr1, Chunk[In]],
    g: Z => ZIO[R1, OutErr1, Z1]
  ): ZSink[R1, InErr1, In1, OutErr1, L, Z1] =
    contramapChunksZIO(f).mapZIO(g)

  /**
   * Effectfully transforms both inputs and result of this sink using the provided functions.
   */
  @deprecated("use dimapZIO", "2.0.0")
  def dimapM[R1 <: R, InErr1 <: InErr, OutErr1 >: OutErr, In1, Z1](
    f: In1 => ZIO[R1, InErr1, In],
    g: Z => ZIO[R1, OutErr1, Z1]
  ): ZSink[R1, InErr1, In1, OutErr1, L, Z1] =
    dimapZIO(f, g)

  /**
   * Effectfully transforms both inputs and result of this sink using the provided functions.
   */
  def dimapZIO[R1 <: R, InErr1 <: InErr, OutErr1 >: OutErr, In1, Z1](
    f: In1 => ZIO[R1, InErr1, In],
    g: Z => ZIO[R1, OutErr1, Z1]
  ): ZSink[R1, InErr1, In1, OutErr1, L, Z1] =
    contramapZIO(f).mapZIO(g)

  def filterInput[In1 <: In](p: In1 => Boolean): ZSink[R, InErr, In1, OutErr, L, Z] =
    contramapChunks(_.filter(p))

  @deprecated("use filterInputZIO", "2.0.0")
  def filterInputM[R1 <: R, InErr1 <: InErr, In1 <: In](
    p: In1 => ZIO[R1, InErr1, Boolean]
  ): ZSink[R1, InErr1, In1, OutErr, L, Z] =
    filterInputZIO(p)

  def filterInputZIO[R1 <: R, InErr1 <: InErr, In1 <: In](
    p: In1 => ZIO[R1, InErr1, Boolean]
  ): ZSink[R1, InErr1, In1, OutErr, L, Z] =
    contramapChunksZIO(_.filterZIO(p))

  /**
   * Runs this sink until it yields a result, then uses that result to create another
   * sink from the provided function which will continue to run until it yields a result.
   *
   * This function essentially runs sinks in sequence.
   */
  def flatMap[R1 <: R, InErr1 <: InErr, OutErr1 >: OutErr, In1 <: In, L1 >: L <: In1, Z1](
    f: Z => ZSink[R1, InErr1, In1, OutErr1, L1, Z1]
  )(implicit ev: L <:< In1): ZSink[R1, InErr1, In1, OutErr1, L1, Z1] =
    foldSink(ZSink.fail(_), f)

  @deprecated("use foldSink", "2.0.0")
  def foldM[R1 <: R, InErr1 <: InErr, OutErr2, In1 <: In, L1 >: L <: In1, Z1](
    failure: OutErr => ZSink[R1, InErr1, In1, OutErr2, L1, Z1],
    success: Z => ZSink[R1, InErr1, In1, OutErr2, L1, Z1]
  )(implicit ev: L <:< In1): ZSink[R1, InErr1, In1, OutErr2, L1, Z1] =
    foldSink(failure, success)

  def foldSink[R1 <: R, InErr1 <: InErr, OutErr2, In1 <: In, L1 >: L <: In1, Z1](
    failure: OutErr => ZSink[R1, InErr1, In1, OutErr2, L1, Z1],
    success: Z => ZSink[R1, InErr1, In1, OutErr2, L1, Z1]
  )(implicit ev: L <:< In1): ZSink[R1, InErr1, In1, OutErr2, L1, Z1] =
    new ZSink(
      channel.doneCollect.foldChannel(
        failure(_).channel,
        { case (leftovers, z) =>
          ZChannel.effectSuspendTotal {
            val leftoversRef = new AtomicReference(leftovers.filter(_.nonEmpty))
            val refReader = ZChannel.effectTotal(leftoversRef.getAndSet(Chunk.empty)).flatMap { chunk =>
              // This cast is safe because of the L1 >: L <: In1 bound. It follows that
              // L <: In1 and therefore Chunk[L] can be safely cast to Chunk[In1].
              val widenedChunk = chunk.asInstanceOf[Chunk[Chunk[In1]]]
              ZChannel.writeChunk(widenedChunk)
            }

            val passthrough      = ZChannel.identity[InErr1, Chunk[In1], Any]
            val continuationSink = (refReader *> passthrough) >>> success(z).channel

            continuationSink.doneCollect.flatMap { case (newLeftovers, z1) =>
              ZChannel.effectTotal(leftoversRef.get).flatMap(ZChannel.writeChunk(_)) *>
                ZChannel.writeChunk(newLeftovers).as(z1)
            }
          }
        }
      )
    )

  /**
   * Transforms this sink's result.
   */
  def map[Z2](f: Z => Z2): ZSink[R, InErr, In, OutErr, L, Z2] = new ZSink(channel.map(f))

  /**
   * Transforms the errors emitted by this sink using `f`.
   */
  def mapError[OutErr2](f: OutErr => OutErr2): ZSink[R, InErr, In, OutErr2, L, Z] =
    new ZSink(channel.mapError(f))

  /**
   * Effectfully transforms this sink's result.
   */
  @deprecated("use mapZIO", "2.0.0")
  def mapM[R1 <: R, OutErr1 >: OutErr, Z1](f: Z => ZIO[R1, OutErr1, Z1]): ZSink[R1, InErr, In, OutErr1, L, Z1] =
    mapZIO(f)

  /**
   * Effectfully transforms this sink's result.
   */
  def mapZIO[R1 <: R, OutErr1 >: OutErr, Z1](f: Z => ZIO[R1, OutErr1, Z1]): ZSink[R1, InErr, In, OutErr1, L, Z1] =
    new ZSink(channel.mapZIO(f))

  /**
   * Runs both sinks in parallel on the input, , returning the result or the error from the
   * one that finishes first.
   */
  final def race[R1 <: R, InErr1 <: InErr, OutErr1 >: OutErr, A0, In1 <: In, L1 >: L, Z1 >: Z](
    that: ZSink[R1, InErr1, In1, OutErr1, L1, Z1]
  ): ZSink[R1, InErr1, In1, OutErr1, L1, Z1] =
    self.raceBoth(that).map(_.merge)

  /**
   * Runs both sinks in parallel on the input, returning the result or the error from the
   * one that finishes first.
   */
  final def raceBoth[R1 <: R, InErr1 <: InErr, OutErr1 >: OutErr, A0, In1 <: In, L1 >: L, Z1 >: Z](
    that: ZSink[R1, InErr1, In1, OutErr1, L1, Z1]
  ): ZSink[R1, InErr1, In1, OutErr1, L1, Either[Z, Z1]] =
    ???

  /**
   * Returns the sink that executes this one and times its execution.
   */
  final def timed: ZSink[R with Has[Clock], InErr, In, OutErr, L, (Z, Duration)] =
    summarized(Clock.nanoTime)((start, end) => Duration.fromNanos(end - start))

  def repeat: ZSink[R, InErr, In, OutErr, L, Chunk[Z]] = ???

  /**
   * Summarize a sink by running an effect when the sink starts and again when it completes
   */
  final def summarized[R1 <: R, E1 >: OutErr, B, C](summary: ZIO[R1, E1, B])(f: (B, B) => C) =
    new ZSink[R1, InErr, In, E1, L, (Z, C)](for {
      start <- ZChannel.fromZIO(summary)
      done  <- self.channel
      end   <- ZChannel.fromZIO(summary)
    } yield (done, f(start, end)))

  def orElse[R1 <: R, InErr1 <: InErr, In1 <: In, OutErr2 >: OutErr, L1 >: L, Z1 >: Z](
    that: => ZSink[R1, InErr1, In1, OutErr2, L1, Z1]
  ): ZSink[R1, InErr1, In1, OutErr2, L1, Z1] =
    new ZSink[R1, InErr1, In1, OutErr2, L1, Z1](self.channel.orElse(that.channel))

  def zip[R1 <: R, InErr1 <: InErr, In1 <: In, OutErr1 >: OutErr, L1 >: L <: In1, Z1](
    that: ZSink[R1, InErr1, In1, OutErr1, L1, Z1]
  )(implicit zippable: Zippable[Z, Z1], ev: L <:< In1): ZSink[R1, InErr1, In1, OutErr1, L1, zippable.Out] =
    zipWith[R1, InErr1, OutErr1, In1, L1, Z1, zippable.Out](that)(zippable.zip(_, _))

  /**
   * Like [[zip]], but keeps only the result from the `that` sink.
   */
  final def zipLeft[R1 <: R, InErr1 <: InErr, In1 <: In, OutErr1 >: OutErr, L1 >: L <: In1, Z1](
    that: ZSink[R1, InErr1, In1, OutErr1, L1, Z1]
  )(implicit ev: L <:< In1): ZSink[R1, InErr1, In1, OutErr1, L1, Z] =
    zipWith[R1, InErr1, OutErr1, In1, L1, Z1, Z](that)((z, _) => z)

  /**
   * Runs both sinks in parallel on the input and combines the results in a tuple.
   */
  final def zipPar[R1 <: R, InErr1 <: InErr, In1 <: In, OutErr1 >: OutErr, L1 >: L <: In1, Z1](
    that: ZSink[R1, InErr1, In1, OutErr1, L1, Z1]
  )(implicit zippable: Zippable[Z, Z1]): ZSink[R1, InErr1, In1, OutErr1, L1, zippable.Out] =
    zipWithPar[R1, InErr1, OutErr1, In1, L1, Z1, zippable.Out](that)(zippable.zip(_, _))

  /**
   * Like [[zipPar]], but keeps only the result from this sink.
   */
  final def zipParLeft[R1 <: R, InErr1 <: InErr, In1 <: In, OutErr1 >: OutErr, L1 >: L <: In1, Z1](
    that: ZSink[R1, InErr1, In1, OutErr1, L1, Z1]
  ): ZSink[R1, InErr1, In1, OutErr1, L1, Z] =
    zipWithPar[R1, InErr1, OutErr1, In1, L1, Z1, Z](that)((b, _) => b)

  /**
   * Like [[zipPar]], but keeps only the result from the `that` sink.
   */
  final def zipParRight[R1 <: R, InErr1 <: InErr, In1 <: In, OutErr1 >: OutErr, L1 >: L <: In1, Z1](
    that: ZSink[R1, InErr1, In1, OutErr1, L1, Z1]
  ): ZSink[R1, InErr1, In1, OutErr1, L1, Z1] =
    zipWithPar[R1, InErr1, OutErr1, In1, L1, Z1, Z1](that)((_, c) => c)

  /**
   * Like [[zip]], but keeps only the result from this sink.
   */
  final def zipRight[R1 <: R, InErr1 <: InErr, In1 <: In, OutErr1 >: OutErr, L1 >: L <: In1, Z1](
    that: ZSink[R1, InErr1, In1, OutErr1, L1, Z1]
  )(implicit ev: L <:< In1): ZSink[R1, InErr1, In1, OutErr1, L1, Z1] =
    zipWith[R1, InErr1, OutErr1, In1, L1, Z1, Z1](that)((_, z1) => z1)

  /**
   * Feeds inputs to this sink until it yields a result, then switches over to the
   * provided sink until it yields a result, finally combining the two results with `f`.
   */
  final def zipWith[R1 <: R, InErr1 <: InErr, OutErr1 >: OutErr, In1 <: In, L1 >: L <: In1, Z1, Z2](
    that: ZSink[R1, InErr1, In1, OutErr1, L1, Z1]
  )(f: (Z, Z1) => Z2)(implicit ev: L <:< In1): ZSink[R1, InErr1, In1, OutErr1, L1, Z2] =
    flatMap(z => that.map(f(z, _)))

  /**
   * Runs both sinks in parallel on the input and combines the results
   * using the provided function.
   */
  final def zipWithPar[R1 <: R, InErr1 <: InErr, OutErr1 >: OutErr, In1 <: In, L1 >: L <: In1, Z1, Z2](
    that: ZSink[R1, InErr1, In1, OutErr1, L1, Z1]
  )(f: (Z, Z1) => Z2): ZSink[R1, InErr1, In1, OutErr1, L1, Z2] =
    ???

  def exposeLeftover: ZSink[R, InErr, In, OutErr, Nothing, (Z, Chunk[L])] =
    new ZSink(channel.doneCollect.map { case (chunks, z) => (z, chunks.flatten) })

  def dropLeftover: ZSink[R, InErr, In, OutErr, Nothing, Z] =
    new ZSink(channel.drain)

  /**
   * Creates a sink that produces values until one verifies
   * the predicate `f`.
   */
  @deprecated("use untilOutputZIO", "2.0.0")
  def untilOutputM[R1 <: R, OutErr1 >: OutErr](
    f: Z => ZIO[R1, OutErr1, Boolean]
  )(implicit ev: L <:< In): ZSink[R1, InErr, In, OutErr1, L, Option[Z]] =
    untilOutputZIO(f)

  /**
   * Creates a sink that produces values until one verifies
   * the predicate `f`.
   */
  def untilOutputZIO[R1 <: R, OutErr1 >: OutErr](
    f: Z => ZIO[R1, OutErr1, Boolean]
  )(implicit ev: L <:< In): ZSink[R1, InErr, In, OutErr1, L, Option[Z]] =
    ???

  /**
   * Provides the sink with its required environment, which eliminates
   * its dependency on `R`.
   */
  def provide(r: R)(implicit ev: NeedsEnv[R]): ZSink[Any, InErr, In, OutErr, L, Z] =
    new ZSink(channel.provide(r))
}

object ZSink {

  /**
   * Accesses the environment of the sink in the context of a sink.
   */
  def accessSink[R]: AccessSinkPartiallyApplied[R] =
    new AccessSinkPartiallyApplied[R]

  def collectAll[Err, In]: ZSink[Any, Err, In, Err, Nothing, Chunk[In]] = {
    def loop(acc: Chunk[In]): ZChannel[Any, Err, Chunk[In], Any, Err, Nothing, Chunk[In]] =
      ZChannel.readWithCause(
        chunk => loop(acc ++ chunk),
        ZChannel.failCause(_),
        _ => ZChannel.succeed(acc)
      )
    new ZSink(loop(Chunk.empty))
  }

  /**
   * A sink that collects first `n` elements into a chunk. Note that the chunk
   * is preallocated and must fit in memory.
   */
  def collectAllN[Err, In](n: Int): ZSink[Any, Err, In, Err, In, Chunk[In]] =
    fromZIO(UIO(ChunkBuilder.make[In](n)))
      .flatMap(cb => foldUntil[Err, In, ChunkBuilder[In]](cb, n.toLong)(_ += _))
      .map(_.result())

  /**
   * A sink that collects all of its inputs into a map. The keys are extracted from inputs
   * using the keying function `key`; if multiple inputs use the same key, they are merged
   * using the `f` function.
   */
  def collectAllToMap[Err, In, K](key: In => K)(f: (In, In) => In): ZSink[Any, Err, In, Err, Nothing, Map[K, In]] =
    foldLeftChunks(Map[K, In]()) { (acc, as) =>
      as.foldLeft(acc) { (acc, a) =>
        val k = key(a)

        acc.updated(
          k,
          // Avoiding `get/getOrElse` here to avoid an Option allocation
          if (acc.contains(k)) f(acc(k), a)
          else a
        )
      }
    }

  /**
   * A sink that collects first `n` keys into a map. The keys are calculated
   * from inputs using the keying function `key`; if multiple inputs use the
   * the same key, they are merged using the `f` function.
   */
  def collectAllToMapN[Err, In, K](n: Long)(key: In => K)(f: (In, In) => In): ZSink[Any, Err, In, Err, In, Map[K, In]] =
    foldWeighted[Err, In, Map[K, In]](Map())((acc, in) => if (acc.contains(key(in))) 0 else 1, n) { (acc, in) =>
      val k = key(in)
      val v = if (acc.contains(k)) f(acc(k), in) else in

      acc.updated(k, v)
    }

  /**
   * A sink that collects all of its inputs into a set.
   */
  def collectAllToSet[Err, In]: ZSink[Any, Err, In, Err, Nothing, Set[In]] =
    foldLeftChunks(Set[In]())((acc, as) => as.foldLeft(acc)(_ + _))

  /**
   * A sink that collects first `n` distinct inputs into a set.
   */
  def collectAllToSetN[Err, In](n: Long): ZSink[Any, Err, In, Err, In, Set[In]] =
    foldWeighted[Err, In, Set[In]](Set())((acc, in) => if (acc.contains(in)) 0 else 1, n)(_ + _)

  /**
   * Accumulates incoming elements into a chunk as long as they verify predicate `p`.
   */
  def collectAllWhile[Err, In](p: In => Boolean): ZSink[Any, Err, In, Err, In, Chunk[In]] =
    fold[Err, In, (List[In], Boolean)]((Nil, true))(_._2) { case ((as, _), a) =>
      if (p(a)) (a :: as, true)
      else (as, false)
    }.map { case (is, _) =>
      Chunk.fromIterable(is.reverse)
    }

  /**
   * Accumulates incoming elements into a chunk as long as they verify effectful predicate `p`.
   */
  @deprecated("use collectAllWhileZIO", "2.0.0")
  def collectAllWhileM[Env, Err, In](p: In => ZIO[Env, Err, Boolean]): ZSink[Env, Err, In, Err, In, Chunk[In]] =
    collectAllWhileZIO(p)

  /**
   * Accumulates incoming elements into a chunk as long as they verify effectful predicate `p`.
   */
  def collectAllWhileZIO[Env, Err, In](p: In => ZIO[Env, Err, Boolean]): ZSink[Env, Err, In, Err, In, Chunk[In]] =
    foldZIO[Env, Err, In, (List[In], Boolean)]((Nil, true))(_._2) { case ((as, _), a) =>
      p(a).map(if (_) (a :: as, true) else (as, false))
    }.map { case (is, _) =>
      Chunk.fromIterable(is.reverse)
    }

  /**
   * A sink that counts the number of elements fed to it.
   */
  def count[Err]: ZSink[Any, Err, Any, Err, Nothing, Long] =
    foldLeft(0L)((s, _) => s + 1)

  /**
   * Creates a sink halting with the specified `Throwable`.
   */
  def die(e: => Throwable): ZSink[Any, Any, Any, Nothing, Nothing, Nothing] =
    ZSink.failCause(Cause.die(e))

  /**
   * Creates a sink halting with the specified message, wrapped in a
   * `RuntimeException`.
   */
  def dieMessage(m: => String): ZSink[Any, Any, Any, Nothing, Nothing, Nothing] =
    ZSink.failCause(Cause.die(new RuntimeException(m)))

  /**
   * A sink that ignores its inputs.
   */
  def drain[Err]: ZSink[Any, Err, Any, Err, Nothing, Unit] =
    new ZSink(ZChannel.read[Any].unit.repeated.catchAll(_ => ZChannel.unit))

  def dropWhile[Err, In](p: In => Boolean): ZSink[Any, Err, In, Err, In, Any] = {
    lazy val loop: ZChannel[Any, Err, Chunk[In], Any, Err, Chunk[In], Any] = ZChannel.readWith(
      (in: Chunk[In]) => {
        val leftover = in.dropWhile(p)
        val more     = leftover.isEmpty
        if (more) loop else ZChannel.write(leftover) *> ZChannel.identity[Err, Chunk[In], Any]
      },
      (e: Err) => ZChannel.fail(e),
      (_: Any) => ZChannel.unit
    )
    new ZSink(loop)
  }

  def dropWhileM[R, InErr, In](p: In => ZIO[R, InErr, Boolean]): ZSink[R, InErr, In, InErr, In, Any] = {
    lazy val loop: ZChannel[R, InErr, Chunk[In], Any, InErr, Chunk[In], Any] = ZChannel.readWith(
      (in: Chunk[In]) =>
        ZChannel.unwrap(in.dropWhileM(p).map { leftover =>
          val more = leftover.isEmpty
          if (more) loop else ZChannel.write(leftover) *> ZChannel.identity[InErr, Chunk[In], Any]
        }),
      (e: InErr) => ZChannel.fail(e),
      (_: Any) => ZChannel.unit
    )

    new ZSink(loop)
  }

  /**
   * Returns a lazily constructed sink that may require effects for its creation.
   */
  def effectSuspendTotal[Env, InErr, In, OutErr, Leftover, Done](
    sink: => ZSink[Env, InErr, In, OutErr, Leftover, Done]
  ): ZSink[Env, InErr, In, OutErr, Leftover, Done] =
    new ZSink(ZChannel.effectSuspendTotal(sink.channel))

  /**
   * Returns a sink that executes a total effect and ends with its result.
   */
  def effectTotal[A](a: => A): ZSink[Any, Any, Any, Nothing, Nothing, A] =
    new ZSink(ZChannel.effectTotal(a))

  /**
   * A sink that always fails with the specified error.
   */
  def fail[E](e: => E): ZSink[Any, Any, Any, E, Nothing, Nothing] = new ZSink(ZChannel.fail(e))

  /**
   * Creates a sink halting with a specified cause.
   */
  def failCause[E](e: => Cause[E]): ZSink[Any, Any, Any, E, Nothing, Nothing] =
    new ZSink(ZChannel.failCause(e))

  /**
   * A sink that folds its inputs with the provided function, termination predicate and initial state.
   */
  def fold[Err, In, S](z: S)(contFn: S => Boolean)(f: (S, In) => S): ZSink[Any, Err, In, Err, In, S] = {
    def foldChunkSplit(z: S, chunk: Chunk[In])(
      contFn: S => Boolean
    )(f: (S, In) => S): (S, Chunk[In]) = {
      def fold(s: S, chunk: Chunk[In], idx: Int, len: Int): (S, Chunk[In]) =
        if (idx == len) {
          (s, Chunk.empty)
        } else {
          val s1 = f(s, chunk(idx))
          if (contFn(s1)) {
            fold(s1, chunk, idx + 1, len)
          } else {
            (s1, chunk.drop(idx + 1))
          }
        }

      fold(z, chunk, 0, chunk.length)
    }

    def reader(s: S): ZChannel[Any, Err, Chunk[In], Any, Err, Chunk[In], S] =
      if (!contFn(s)) ZChannel.end(s)
      else
        ZChannel.readWith(
          (in: Chunk[In]) => {
            val (nextS, leftovers) = foldChunkSplit(s, in)(contFn)(f)

            if (leftovers.nonEmpty) ZChannel.write(leftovers).as(nextS)
            else reader(nextS)
          },
          (err: Err) => ZChannel.fail(err),
          (_: Any) => ZChannel.end(s)
        )

    new ZSink(reader(z))
  }

  /**
   * A sink that folds its input chunks with the provided function, termination predicate and initial state.
   * `contFn` condition is checked only for the initial value and at the end of processing of each chunk.
   * `f` and `contFn` must preserve chunking-invariance.
   */
  def foldChunks[Err, In, S](
    z: S
  )(contFn: S => Boolean)(f: (S, Chunk[In]) => S): ZSink[Any, Err, In, Err, Nothing, S] = {
    def reader(s: S): ZChannel[Any, Err, Chunk[In], Any, Err, Nothing, S] = ZChannel.readWith(
      (in: Chunk[In]) => {
        val nextS = f(s, in)

        if (contFn(nextS)) reader(nextS)
        else ZChannel.end(nextS)
      },
      (err: Err) => ZChannel.fail(err),
      (_: Any) => ZChannel.end(s)
    )

    new ZSink(
      if (contFn(z)) reader(z)
      else ZChannel.end(z)
    )
  }

  /**
   * A sink that effectfully folds its input chunks with the provided function, termination predicate and initial state.
   * `contFn` condition is checked only for the initial value and at the end of processing of each chunk.
   * `f` and `contFn` must preserve chunking-invariance.
   */
  @deprecated("use foldChunksZIO", "2.0.0")
  def foldChunksM[Env, Err, In, S](
    z: S
  )(contFn: S => Boolean)(f: (S, Chunk[In]) => ZIO[Env, Err, S]): ZSink[Env, Err, In, Err, In, S] =
    foldChunksZIO(z)(contFn)(f)

  /**
   * A sink that effectfully folds its input chunks with the provided function, termination predicate and initial state.
   * `contFn` condition is checked only for the initial value and at the end of processing of each chunk.
   * `f` and `contFn` must preserve chunking-invariance.
   */
  def foldChunksZIO[Env, Err, In, S](
    z: S
  )(contFn: S => Boolean)(f: (S, Chunk[In]) => ZIO[Env, Err, S]): ZSink[Env, Err, In, Err, In, S] = {
    def reader(s: S): ZChannel[Env, Err, Chunk[In], Any, Err, Nothing, S] =
      ZChannel.readWith(
        (in: Chunk[In]) =>
          ZChannel.fromZIO(f(s, in)).flatMap { nextS =>
            if (contFn(nextS)) reader(nextS)
            else ZChannel.end(nextS)
          },
        (err: Err) => ZChannel.fail(err),
        (_: Any) => ZChannel.end(s)
      )

    new ZSink(
      if (contFn(z)) reader(z)
      else ZChannel.end(z)
    )
  }

  /**
   * A sink that folds its inputs with the provided function and initial state.
   */
  def foldLeft[Err, In, S](z: S)(f: (S, In) => S): ZSink[Any, Err, In, Err, Nothing, S] =
    fold(z)(_ => true)(f).dropLeftover

  /**
   * A sink that folds its input chunks with the provided function and initial state.
   * `f` must preserve chunking-invariance.
   */
  def foldLeftChunks[Err, In, S](z: S)(f: (S, Chunk[In]) => S): ZSink[Any, Err, In, Err, Nothing, S] =
    foldChunks[Err, In, S](z)(_ => true)(f)

  /**
   * A sink that effectfully folds its input chunks with the provided function and initial state.
   * `f` must preserve chunking-invariance.
   */
  @deprecated("use foldLeftChunksZIO", "2.0.0")
  def foldLeftChunksM[R, Err, In, S](z: S)(
    f: (S, Chunk[In]) => ZIO[R, Err, S]
  ): ZSink[R, Err, In, Err, Nothing, S] =
    foldLeftChunksZIO[R, Err, In, S](z)(f)

  /**
   * A sink that effectfully folds its input chunks with the provided function and initial state.
   * `f` must preserve chunking-invariance.
   */
  def foldLeftChunksZIO[R, Err, In, S](z: S)(
    f: (S, Chunk[In]) => ZIO[R, Err, S]
  ): ZSink[R, Err, In, Err, Nothing, S] =
    foldChunksZIO[R, Err, In, S](z)(_ => true)(f).dropLeftover

  /**
   * A sink that effectfully folds its inputs with the provided function and initial state.
   */
  @deprecated("use foldLeftZIO", "2.0.0")
  def foldLeftM[R, Err, In, S](z: S)(
    f: (S, In) => ZIO[R, Err, S]
  ): ZSink[R, Err, In, Err, In, S] =
    foldLeftZIO(z)(f)

  /**
   * A sink that effectfully folds its inputs with the provided function and initial state.
   */
  def foldLeftZIO[R, Err, In, S](z: S)(
    f: (S, In) => ZIO[R, Err, S]
  ): ZSink[R, Err, In, Err, In, S] =
    foldZIO[R, Err, In, S](z)(_ => true)(f)

  /**
   * A sink that effectfully folds its inputs with the provided function, termination predicate and initial state.
   */
  @deprecated("use foldZIO", "2.0.0")
  def foldM[Env, Err, In, S](z: S)(contFn: S => Boolean)(
    f: (S, In) => ZIO[Env, Err, S]
  ): ZSink[Env, Err, In, Err, In, S] =
    foldZIO(z)(contFn)(f)

  /**
   * Creates a sink that folds elements of type `In` into a structure
   * of type `S` until `max` elements have been folded.
   *
   * Like [[foldWeighted]], but with a constant cost function of 1.
   */
  def foldUntil[Err, In, S](z: S, max: Long)(f: (S, In) => S): ZSink[Any, Err, In, Err, In, S] =
    fold[Err, In, (S, Long)]((z, 0))(_._2 < max) { case ((o, count), i) =>
      (f(o, i), count + 1)
    }.map(_._1)

  /**
   * Creates a sink that effectfully folds elements of type `In` into a structure
   * of type `S` until `max` elements have been folded.
   *
   * Like [[foldWeightedM]], but with a constant cost function of 1.
   */
  @deprecated("use foldUntilZIO", "2.0.0")
  def foldUntilM[Env, In, Err, S](z: S, max: Long)(
    f: (S, In) => ZIO[Env, Err, S]
  ): ZSink[Env, Err, In, Err, In, S] =
    foldUntilZIO(z, max)(f)

  /**
   * Creates a sink that effectfully folds elements of type `In` into a structure
   * of type `S` until `max` elements have been folded.
   *
   * Like [[foldWeightedM]], but with a constant cost function of 1.
   */
  def foldUntilZIO[Env, In, Err, S](z: S, max: Long)(
    f: (S, In) => ZIO[Env, Err, S]
  ): ZSink[Env, Err, In, Err, In, S] =
    foldZIO[Env, Err, In, (S, Long)]((z, 0))(_._2 < max) { case ((o, count), i) =>
      f(o, i).map((_, count + 1))
    }.map(_._1)

  /**
   * Creates a sink that folds elements of type `In` into a structure
   * of type `S`, until `max` worth of elements (determined by the `costFn`)
   * have been folded.
   *
   * @note Elements that have an individual cost larger than `max` will
   * force the sink to cross the `max` cost. See [[foldWeightedDecompose]]
   * for a variant that can handle these cases.
   */
  def foldWeighted[Err, In, S](z: S)(costFn: (S, In) => Long, max: Long)(
    f: (S, In) => S
  ): ZSink[Any, Err, In, Err, In, S] =
    foldWeightedDecompose[Err, In, S](z)(costFn, max, Chunk.single(_))(f)

  /**
   * Creates a sink that folds elements of type `In` into a structure
   * of type `S`, until `max` worth of elements (determined by the `costFn`)
   * have been folded.
   *
   * The `decompose` function will be used for decomposing elements that
   * cause an `S` aggregate to cross `max` into smaller elements. For
   * example:
   * {{{
   * Stream(1, 5, 1)
   *  .transduce(
   *    ZSink
   *      .foldWeightedDecompose(List[Int]())((i: Int) => i.toLong, 4,
   *        (i: Int) => Chunk(i - 1, 1)) { (acc, el) =>
   *        el :: acc
   *      }
   *      .map(_.reverse)
   *  )
   *  .runCollect
   * }}}
   *
   * The stream would emit the elements `List(1), List(4), List(1, 1)`.
   *
   * Be vigilant with this function, it has to generate "simpler" values
   * or the fold may never end. A value is considered indivisible if
   * `decompose` yields the empty chunk or a single-valued chunk. In
   * these cases, there is no other choice than to yield a value that
   * will cross the threshold.
   *
   * The [[foldWeightedDecomposeM]] allows the decompose function
   * to return a `ZIO` value, and consequently it allows the sink
   * to fail.
   */
  def foldWeightedDecompose[Err, In, S](
    z: S
  )(costFn: (S, In) => Long, max: Long, decompose: In => Chunk[In])(
    f: (S, In) => S
  ): ZSink[Any, Err, In, Err, In, S] = {
    def go(s: S, cost: Long, dirty: Boolean): ZChannel[Any, Err, Chunk[In], Any, Err, Chunk[In], S] =
      ZChannel.readWith(
        (in: Chunk[In]) => {
          def fold(in: Chunk[In], s: S, dirty: Boolean, cost: Long, idx: Int): (S, Long, Boolean, Chunk[In]) =
            if (idx == in.length) (s, cost, dirty, Chunk.empty)
            else {
              val elem  = in(idx)
              val total = cost + costFn(s, elem)

              if (total <= max) fold(in, f(s, elem), true, total, idx + 1)
              else {
                val decomposed = decompose(elem)

                if (decomposed.length <= 1 && !dirty)
                  // If `elem` cannot be decomposed, we need to cross the `max` threshold. To
                  // minimize "injury", we only allow this when we haven't added anything else
                  // to the aggregate (dirty = false).
                  (f(s, elem), total, true, in.drop(idx + 1))
                else if (decomposed.length <= 1 && dirty)
                  // If the state is dirty and `elem` cannot be decomposed, we stop folding
                  // and include `elem` in th leftovers.
                  (s, cost, dirty, in.drop(idx))
                else
                  // `elem` got decomposed, so we will recurse with the decomposed elements pushed
                  // into the chunk we're processing and see if we can aggregate further.
                  fold(decomposed ++ in.drop(idx + 1), s, dirty, cost, 0)
              }
            }

          val (nextS, nextCost, nextDirty, leftovers) = fold(in, s, dirty, cost, 0)

          if (leftovers.nonEmpty) ZChannel.write(leftovers) *> ZChannel.end(nextS)
          else if (cost > max) ZChannel.end(nextS)
          else go(nextS, nextCost, nextDirty)
        },
        (err: Err) => ZChannel.fail(err),
        (_: Any) => ZChannel.end(s)
      )

    new ZSink(go(z, 0, false))
  }

  /**
   * Creates a sink that effectfully folds elements of type `In` into a structure
   * of type `S`, until `max` worth of elements (determined by the `costFn`) have
   * been folded.
   *
   * The `decompose` function will be used for decomposing elements that
   * cause an `S` aggregate to cross `max` into smaller elements. Be vigilant with
   * this function, it has to generate "simpler" values or the fold may never end.
   * A value is considered indivisible if `decompose` yields the empty chunk or a
   * single-valued chunk. In these cases, there is no other choice than to yield
   * a value that will cross the threshold.
   *
   * See [[foldWeightedDecompose]] for an example.
   */
  @deprecated("use foldWeightedDecomposeZIO", "2.0.0")
  def foldWeightedDecomposeM[Env, Err, In, S](z: S)(
    costFn: (S, In) => ZIO[Env, Err, Long],
    max: Long,
    decompose: In => ZIO[Env, Err, Chunk[In]]
  )(f: (S, In) => ZIO[Env, Err, S]): ZSink[Env, Err, In, Err, In, S] =
    foldWeightedDecomposeZIO(z)(costFn, max, decompose)(f)

  /**
   * Creates a sink that effectfully folds elements of type `In` into a structure
   * of type `S`, until `max` worth of elements (determined by the `costFn`) have
   * been folded.
   *
   * The `decompose` function will be used for decomposing elements that
   * cause an `S` aggregate to cross `max` into smaller elements. Be vigilant with
   * this function, it has to generate "simpler" values or the fold may never end.
   * A value is considered indivisible if `decompose` yields the empty chunk or a
   * single-valued chunk. In these cases, there is no other choice than to yield
   * a value that will cross the threshold.
   *
   * See [[foldWeightedDecompose]] for an example.
   */
  def foldWeightedDecomposeZIO[Env, Err, In, S](z: S)(
    costFn: (S, In) => ZIO[Env, Err, Long],
    max: Long,
    decompose: In => ZIO[Env, Err, Chunk[In]]
  )(f: (S, In) => ZIO[Env, Err, S]): ZSink[Env, Err, In, Err, In, S] = {
    def go(s: S, cost: Long, dirty: Boolean): ZChannel[Env, Err, Chunk[In], Any, Err, Chunk[In], S] =
      ZChannel.readWith(
        (in: Chunk[In]) => {
          def fold(
            in: Chunk[In],
            s: S,
            dirty: Boolean,
            cost: Long,
            idx: Int
          ): ZIO[Env, Err, (S, Long, Boolean, Chunk[In])] =
            if (idx == in.length) UIO.succeed((s, cost, dirty, Chunk.empty))
            else {
              val elem = in(idx)
              costFn(s, elem).map(cost + _).flatMap { total =>
                if (total <= max) f(s, elem).flatMap(fold(in, _, true, total, idx + 1))
                else
                  decompose(elem).flatMap { decomposed =>
                    if (decomposed.length <= 1 && !dirty)
                      // If `elem` cannot be decomposed, we need to cross the `max` threshold. To
                      // minimize "injury", we only allow this when we haven't added anything else
                      // to the aggregate (dirty = false).
                      f(s, elem).map((_, total, true, in.drop(idx + 1)))
                    else if (decomposed.length <= 1 && dirty)
                      // If the state is dirty and `elem` cannot be decomposed, we stop folding
                      // and include `elem` in th leftovers.
                      UIO.succeed((s, cost, dirty, in.drop(idx)))
                    else
                      // `elem` got decomposed, so we will recurse with the decomposed elements pushed
                      // into the chunk we're processing and see if we can aggregate further.
                      fold(decomposed ++ in.drop(idx + 1), s, dirty, cost, 0)
                  }
              }
            }

          ZChannel.fromZIO(fold(in, s, dirty, cost, 0)).flatMap { case (nextS, nextCost, nextDirty, leftovers) =>
            if (leftovers.nonEmpty) ZChannel.write(leftovers) *> ZChannel.end(nextS)
            else if (cost > max) ZChannel.end(nextS)
            else go(nextS, nextCost, nextDirty)
          }
        },
        (err: Err) => ZChannel.fail(err),
        (_: Any) => ZChannel.end(s)
      )

    new ZSink(go(z, 0, false))
  }

  /**
   * Creates a sink that effectfully folds elements of type `In` into a structure
   * of type `S`, until `max` worth of elements (determined by the `costFn`) have
   * been folded.
   *
   * @note Elements that have an individual cost larger than `max` will
   * force the sink to cross the `max` cost. See [[foldWeightedDecomposeM]]
   * for a variant that can handle these cases.
   */
  @deprecated("use foldWeightedZIO", "2.0.0-")
  def foldWeightedM[Env, Err, In, S](
    z: S
  )(costFn: (S, In) => ZIO[Env, Err, Long], max: Long)(
    f: (S, In) => ZIO[Env, Err, S]
  ): ZSink[Env, Err, In, Err, In, S] =
    foldWeightedZIO(z)(costFn, max)(f)

  /**
   * Creates a sink that effectfully folds elements of type `In` into a structure
   * of type `S`, until `max` worth of elements (determined by the `costFn`) have
   * been folded.
   *
   * @note Elements that have an individual cost larger than `max` will
   * force the sink to cross the `max` cost. See [[foldWeightedDecomposeM]]
   * for a variant that can handle these cases.
   */
  def foldWeightedZIO[Env, Err, In, S](
    z: S
  )(costFn: (S, In) => ZIO[Env, Err, Long], max: Long)(
    f: (S, In) => ZIO[Env, Err, S]
  ): ZSink[Env, Err, In, Err, In, S] =
    foldWeightedDecomposeZIO(z)(costFn, max, (i: In) => UIO.succeedNow(Chunk.single(i)))(f)

  /**
   * A sink that effectfully folds its inputs with the provided function, termination predicate and initial state.
   */
  def foldZIO[Env, Err, In, S](z: S)(contFn: S => Boolean)(
    f: (S, In) => ZIO[Env, Err, S]
  ): ZSink[Env, Err, In, Err, In, S] = {
    def foldChunkSplitM(z: S, chunk: Chunk[In])(
      contFn: S => Boolean
    )(f: (S, In) => ZIO[Env, Err, S]): ZIO[Env, Err, (S, Option[Chunk[In]])] = {
      def fold(s: S, chunk: Chunk[In], idx: Int, len: Int): ZIO[Env, Err, (S, Option[Chunk[In]])] =
        if (idx == len) UIO.succeed((s, None))
        else
          f(s, chunk(idx)).flatMap { s1 =>
            if (contFn(s1)) {
              fold(s1, chunk, idx + 1, len)
            } else {
              UIO.succeed((s1, Some(chunk.drop(idx + 1))))
            }
          }

      fold(z, chunk, 0, chunk.length)
    }

    def reader(s: S): ZChannel[Env, Err, Chunk[In], Any, Err, Chunk[In], S] =
      ZChannel.readWith(
        (in: Chunk[In]) =>
          ZChannel.fromZIO(foldChunkSplitM(s, in)(contFn)(f)).flatMap { case (nextS, leftovers) =>
            leftovers match {
              case Some(l) => ZChannel.write(l).as(nextS)
              case None    => reader(nextS)
            }
          },
        (err: Err) => ZChannel.fail(err),
        (_: Any) => ZChannel.end(s)
      )

    new ZSink(
      if (contFn(z)) reader(z)
      else ZChannel.end(z)
    )
  }

  /**
   * A sink that executes the provided effectful function for every element fed to it.
   */
  def foreach[R, Err, In](f: In => ZIO[R, Err, Any]): ZSink[R, Err, In, Err, In, Unit] =
    foreachWhile(f(_).as(true))

  /**
   * A sink that executes the provided effectful function for every chunk fed to it.
   */
  def foreachChunk[R, Err, In](
    f: Chunk[In] => ZIO[R, Err, Any]
  ): ZSink[R, Err, In, Err, In, Unit] =
    foreachChunkWhile(f(_).as(true))

  /**
   * A sink that executes the provided effectful function for every element fed to it
   * until `f` evaluates to `false`.
   */
  final def foreachWhile[R, Err, In](
    f: In => ZIO[R, Err, Boolean]
  ): ZSink[R, Err, In, Err, In, Unit] = {
    def go(
      chunk: Chunk[In],
      idx: Int,
      len: Int,
      cont: ZChannel[R, Err, Chunk[In], Any, Err, Chunk[In], Unit]
    ): ZChannel[R, Err, Chunk[In], Any, Err, Chunk[In], Unit] =
      if (idx == len)
        cont
      else
        ZChannel
          .fromZIO(f(chunk(idx)))
          .flatMap(b => if (b) go(chunk, idx + 1, len, cont) else ZChannel.write(chunk.drop(idx)))
          .catchAll(e => ZChannel.write(chunk.drop(idx)) *> ZChannel.fail(e))

    lazy val process: ZChannel[R, Err, Chunk[In], Any, Err, Chunk[In], Unit] =
      ZChannel.readWithCause[R, Err, Chunk[In], Any, Err, Chunk[In], Unit](
        in => go(in, 0, in.length, process),
        halt => ZChannel.failCause(halt),
        _ => ZChannel.end(())
      )

    new ZSink(process)
  }

  /**
   * A sink that executes the provided effectful function for every chunk fed to it
   * until `f` evaluates to `false`.
   */
  def foreachChunkWhile[R, Err, In](
    f: Chunk[In] => ZIO[R, Err, Boolean]
  ): ZSink[R, Err, In, Err, In, Unit] = {
    lazy val reader: ZChannel[R, Err, Chunk[In], Any, Err, Nothing, Unit] =
      ZChannel.readWith(
        (in: Chunk[In]) =>
          ZChannel.fromZIO(f(in)).flatMap { continue =>
            if (continue) reader
            else ZChannel.end(())
          },
        (err: Err) => ZChannel.fail(err),
        (_: Any) => ZChannel.unit
      )

    new ZSink(reader)
  }

  /**
   * Creates a single-value sink produced from an effect
   */
  @deprecated("use fromZIO", "2.0.0")
  def fromEffect[R, E, Z](b: => ZIO[R, E, Z]): ZSink[R, Any, Any, E, Nothing, Z] =
    fromZIO(b)

  /**
   * Creates a single-value sink produced from an effect
   */
  def fromZIO[R, E, Z](b: => ZIO[R, E, Z]): ZSink[R, Any, Any, E, Nothing, Z] =
    new ZSink(ZChannel.fromZIO(b))

  /**
   * Creates a sink halting with a specified cause.
   */
  @deprecated("use failCause", "2.0.0")
  def halt[E](e: => Cause[E]): ZSink[Any, Any, Any, E, Nothing, Nothing] =
    failCause(e)

  /**
   * Creates a sink containing the first value.
   */
  def head[Err, In]: ZSink[Any, Err, In, Err, In, Option[In]] =
    fold(None: Option[In])(_.isEmpty) {
      case (s @ Some(_), _) => s
      case (None, in)       => Some(in)
    }

  /**
   * Creates a sink containing the last value.
   */
  def last[Err, In]: ZSink[Any, Err, In, Err, In, Option[In]] =
    foldLeft(None: Option[In])((_, in) => Some(in))

  def leftover[L](c: Chunk[L]): ZSink[Any, Any, Any, Nothing, L, Unit] =
    new ZSink(ZChannel.write(c))

  def mkString[Err]: ZSink[Any, Err, Any, Err, Nothing, String] =
    ZSink.effectSuspendTotal {
      val builder = new StringBuilder()

      foldLeftChunks[Err, Any, Unit](())((_, els: Chunk[Any]) => els.foreach(el => builder.append(el.toString))).map(
        _ => builder.result()
      )
    }

  def managed[R, InErr, In, OutErr >: InErr, A, L <: In, Z](resource: ZManaged[R, OutErr, A])(
    fn: A => ZSink[R, InErr, In, OutErr, L, Z]
  ): ZSink[R, InErr, In, OutErr, In, Z] =
    new ZSink(ZChannel.managed(resource)(fn(_).channel))

  val never: ZSink[Any, Any, Any, Nothing, Nothing, Nothing] = new ZSink(ZChannel.fromZIO(ZIO.never))

  /**
   * A sink that immediately ends with the specified value.
   */
  def succeed[Z](z: => Z): ZSink[Any, Any, Any, Nothing, Nothing, Z] = new ZSink(ZChannel.succeed(z))

  /**
   * A sink that sums incoming numeric values.
   */
  def sum[Err, A](implicit A: Numeric[A]): ZSink[Any, Err, A, Err, Nothing, A] =
    foldLeft(A.zero)(A.plus)

  /**
   * A sink that takes the specified number of values.
   */
  def take[Err, In](n: Int): ZSink[Any, Err, In, Err, In, Chunk[In]] =
    ZSink.foldChunks[Err, In, Chunk[In]](Chunk.empty)(_.length < n)(_ ++ _).flatMap { acc =>
      val (taken, leftover) = acc.splitAt(n)
      new ZSink(
        ZChannel.write(leftover) *> ZChannel.end(taken)
      )
    }

  def timed[Err]: ZSink[Has[Clock], Err, Any, Err, Nothing, Duration] = ZSink.drain.timed.map(_._2)

  final class AccessSinkPartiallyApplied[R](private val dummy: Boolean = true) extends AnyVal {
    def apply[InErr, In, OutErr, L, Z](f: R => ZSink[R, InErr, In, OutErr, L, Z]): ZSink[R, InErr, In, OutErr, L, Z] =
      new ZSink(ZChannel.unwrap(ZIO.access[R](f(_).channel)))
  }

  def utf8Decode[Err]: ZSink[Any, Err, Byte, Err, Byte, Option[String]] = {
    def is2ByteSequenceStart(b: Byte) = (b & 0xe0) == 0xc0
    def is3ByteSequenceStart(b: Byte) = (b & 0xf0) == 0xe0
    def is4ByteSequenceStart(b: Byte) = (b & 0xf8) == 0xf0
    def computeSplit(chunk: Chunk[Byte]) = {
      // There are 3 bad patterns we need to check to detect an incomplete chunk:
      // - 2/3/4 byte sequences that start on the last byte
      // - 3/4 byte sequences that start on the second-to-last byte
      // - 4 byte sequences that start on the third-to-last byte
      //
      // Otherwise, we can convert the entire concatenated chunk to a string.
      val len = chunk.length

      if (
        len >= 1 &&
        (is2ByteSequenceStart(chunk(len - 1)) ||
          is3ByteSequenceStart(chunk(len - 1)) ||
          is4ByteSequenceStart(chunk(len - 1)))
      )
        len - 1
      else if (
        len >= 2 &&
        (is3ByteSequenceStart(chunk(len - 2)) ||
          is4ByteSequenceStart(chunk(len - 2)))
      )
        len - 2
      else if (len >= 3 && is4ByteSequenceStart(chunk(len - 3)))
        len - 3
      else len
    }

    def chopBOM(bytes: Chunk[Byte]): Chunk[Byte] =
      if (
        bytes.length >= 3 &&
        bytes.byte(0) == -17 &&
        bytes.byte(1) == -69 &&
        bytes.byte(2) == -65
      ) bytes.drop(3)
      else bytes

    def channel(acc: Chunk[Byte]): ZChannel[Any, Err, Chunk[Byte], Any, Err, Chunk[Byte], Option[String]] =
      ZChannel.readWith(
        (in: Chunk[Byte]) => {
          val concat                    = acc ++ chopBOM(in)
          val (toConvert, newLeftovers) = concat.splitAt(computeSplit(concat))

          if (toConvert.isEmpty) channel(newLeftovers.materialize)
          else if (newLeftovers.isEmpty) ZChannel.end(Some(new String(toConvert.toArray[Byte], "UTF-8")))
          else ZChannel.write(newLeftovers).as(Some(new String(toConvert.toArray[Byte], "UTF-8")))
        },
        (err: Err) => ZChannel.fail(err),
        (_: Any) =>
          if (acc.isEmpty) ZChannel.end(None)
          else {
            val (toConvert, newLeftovers) = acc.splitAt(computeSplit(acc))

            if (toConvert.isEmpty)
              // Upstream has ended and all we read was an incomplete chunk, so we fallback to the
              // String constructor behavior.
              ZChannel.end(Some(new String(newLeftovers.toArray[Byte], "UTF-8")))
            else if (newLeftovers.nonEmpty)
              ZChannel.write(newLeftovers.materialize).as(Some(new String(toConvert.toArray[Byte], "UTF-8")))
            else
              ZChannel.end(Some(new String(toConvert.toArray[Byte], "UTF-8")))
          }
      )

    new ZSink(channel(Chunk.empty))
  }
}

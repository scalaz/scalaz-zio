package zio.stream.experimental

import zio.ZManaged.ReleaseMap
import zio._
import zio.stream.experimental.internal.{
  AsyncInputConsumer,
  AsyncInputProducer,
  ChannelExecutor,
  SingleProducerAsyncInput
}

import ChannelExecutor.ChannelState

/**
 * A `ZChannel[In, Env, Err, Out, Z]` is a nexus of I/O operations, which supports both reading and
 * writing. A channel may read values of type `In` and write values of type `Out`. When the channel
 * finishes, it yields a value of type `Z`. A channel may fail with a value of type `Err`.
 *
 * Channels are the foundation of ZIO Streams: both streams and sinks are built on channels.
 * Most users shouldn't have to use channels directly, as streams and sinks are much more convenient
 * and cover all common use cases. However, when adding new stream and sink operators, or doing
 * something highly specialized, it may be useful to use channels directly.
 *
 * Channels compose in a variety of ways:
 *
 *  - Piping. One channel can be piped to another channel, assuming the input type of the second
 *    is the same as the output type of the first.
 *  - Sequencing. The terminal value of one channel can be used to create another channel, and
 *    both the first channel and the function that makes the second channel can be composed into a
 *    channel.
 *  - Concating. The output of one channel can be used to create other channels, which are all
 *    concatenated together. The first channel and the function that makes the other channels can
 *    be composed into a channel.
 */
sealed trait ZChannel[-Env, -InErr, -InElem, -InDone, +OutErr, +OutElem, +OutDone] { self =>

  /**
   * Returns a new channel that is the sequential composition of this channel and the specified
   * channel. The returned channel terminates with a tuple of the terminal values of both channels.
   *
   * This is a symbol operator for [[ZChannel#zip]].
   */
  final def <*>[
    Env1 <: Env,
    InErr1 <: InErr,
    InElem1 <: InElem,
    InDone1 <: InDone,
    OutErr1 >: OutErr,
    OutElem1 >: OutElem,
    OutDone2
  ](
    that: => ZChannel[Env1, InErr1, InElem1, InDone1, OutErr1, OutElem1, OutDone2]
  )(implicit
    zippable: Zippable[OutDone, OutDone2]
  ): ZChannel[Env1, InErr1, InElem1, InDone1, OutErr1, OutElem1, zippable.Out] =
    self.flatMap(z => that.map(z2 => zippable.zip(z, z2)))

  /**
   * Returns a new channel that is the sequential composition of this channel and the specified
   * channel. The returned channel terminates with the terminal value of the other channel.
   *
   * This is a symbol operator for [[ZChannel#zipRight]].
   */
  final def *>[
    Env1 <: Env,
    InErr1 <: InErr,
    InElem1 <: InElem,
    InDone1 <: InDone,
    OutErr1 >: OutErr,
    OutElem1 >: OutElem,
    OutDone2
  ](
    that: => ZChannel[Env1, InErr1, InElem1, InDone1, OutErr1, OutElem1, OutDone2]
  ): ZChannel[Env1, InErr1, InElem1, InDone1, OutErr1, OutElem1, OutDone2] =
    self.flatMap(_ => that)

  /**
   * Returns a new channel that is the sequential composition of this channel and the specified
   * channel. The returned channel terminates with the terminal value of this channel.
   *
   * This is a symbol operator for [[ZChannel#zipLeft]].
   */
  final def <*[
    Env1 <: Env,
    InErr1 <: InErr,
    InElem1 <: InElem,
    InDone1 <: InDone,
    OutErr1 >: OutErr,
    OutElem1 >: OutElem,
    OutDone2
  ](
    that: => ZChannel[Env1, InErr1, InElem1, InDone1, OutErr1, OutElem1, OutDone2]
  ): ZChannel[Env1, InErr1, InElem1, InDone1, OutErr1, OutElem1, OutDone] =
    self.flatMap(z => that.as(z))

  /**
   * Returns a new channel that pipes the output of this channel into the specified channel.
   * The returned channel has the input type of this channel, and the output type of the specified
   * channel, terminating with the value of the specified channel.
   *
   * This is a symbolic operator for [[ZChannel#pipeTo]].
   */
  final def >>>[Env1 <: Env, OutErr2, OutElem2, OutDone2](
    that: => ZChannel[Env1, OutErr, OutElem, OutDone, OutErr2, OutElem2, OutDone2]
  ): ZChannel[Env1, InErr, InElem, InDone, OutErr2, OutElem2, OutDone2] =
    self pipeTo that

  /**
   * Returns a new channel that is the same as this one, except the terminal value of the channel
   * is the specified constant value.
   *
   * This method produces the same result as mapping this channel to the specified constant value.
   */
  final def as[OutDone2](z2: => OutDone2): ZChannel[Env, InErr, InElem, InDone, OutErr, OutElem, OutDone2] =
    self.map(_ => z2)

  /**
   * Returns a new channel that is the same as this one, except if this channel errors for any
   * typed error, then the returned channel will switch over to using the fallback channel returned
   * by the specified error handler.
   */
  final def catchAll[
    Env1 <: Env,
    InErr1 <: InErr,
    InElem1 <: InElem,
    InDone1 <: InDone,
    OutErr2,
    OutElem1 >: OutElem,
    OutDone1 >: OutDone
  ](
    f: OutErr => ZChannel[Env1, InErr1, InElem1, InDone1, OutErr2, OutElem1, OutDone1]
  ): ZChannel[Env1, InErr1, InElem1, InDone1, OutErr2, OutElem1, OutDone1] =
    catchAllCause((cause: Cause[OutErr]) => cause.failureOrCause.fold(f(_), ZChannel.failCause(_)))

  /**
   * Returns a new channel that is the same as this one, except if this channel errors for any
   * cause at all, then the returned channel will switch over to using the fallback channel
   * returned by the specified error handler.
   */
  final def catchAllCause[
    Env1 <: Env,
    InErr1 <: InErr,
    InElem1 <: InElem,
    InDone1 <: InDone,
    OutErr2,
    OutElem1 >: OutElem,
    OutDone1 >: OutDone
  ](
    f: Cause[OutErr] => ZChannel[Env1, InErr1, InElem1, InDone1, OutErr2, OutElem1, OutDone1]
  ): ZChannel[Env1, InErr1, InElem1, InDone1, OutErr2, OutElem1, OutDone1] =
    ZChannel.Fold(self, new ZChannel.Fold.K(ZChannel.Fold.successIdentity[OutDone1], f))

  /**
   * Returns a new channel whose outputs are fed to the specified factory function, which creates
   * new channels in response. These new channels are sequentially concatenated together, and all
   * their outputs appear as outputs of the newly returned channel.
   */
  final def concatMap[Env1 <: Env, InErr1 <: InErr, InElem1 <: InElem, InDone1 <: InDone, OutErr1 >: OutErr, OutElem2](
    f: OutElem => ZChannel[Env1, InErr1, InElem1, InDone1, OutErr1, OutElem2, Any]
  ): ZChannel[Env1, InErr1, InElem1, InDone1, OutErr1, OutElem2, Any] =
    concatMapWith(f)((_, _) => (), (_, _) => ())

  /**
   * Returns a new channel whose outputs are fed to the specified factory function, which creates
   * new channels in response. These new channels are sequentially concatenated together, and all
   * their outputs appear as outputs of the newly returned channel. The provided merging function
   * is used to merge the terminal values of all channels into the single terminal value of the
   * returned channel.
   */
  final def concatMapWith[
    Env1 <: Env,
    InErr1 <: InErr,
    InElem1 <: InElem,
    InDone1 <: InDone,
    OutErr1 >: OutErr,
    OutElem2,
    OutDone2,
    OutDone3
  ](
    f: OutElem => ZChannel[Env1, InErr1, InElem1, InDone1, OutErr1, OutElem2, OutDone2]
  )(
    g: (OutDone2, OutDone2) => OutDone2,
    h: (OutDone2, OutDone) => OutDone3
  ): ZChannel[Env1, InErr1, InElem1, InDone1, OutErr1, OutElem2, OutDone3] =
    ZChannel.ConcatAll(g, h, self, f)

  /**
   * Returns a new channel, which is the same as this one, except its outputs are filtered and
   * transformed by the specified partial function.
   */
  final def collect[OutElem2](
    f: PartialFunction[OutElem, OutElem2]
  ): ZChannel[Env, InErr, InElem, InDone, OutErr, OutElem2, OutDone] = {
    val pf = f.lift
    lazy val collector: ZChannel[Env, OutErr, OutElem, OutDone, OutErr, OutElem2, OutDone] =
      ZChannel.readWith(
        pf(_: OutElem) match {
          case None       => collector
          case Some(out2) => ZChannel.write(out2) *> collector
        },
        (e: OutErr) => ZChannel.fail(e),
        (z: OutDone) => ZChannel.end(z)
      )

    self pipeTo collector
  }

  /**
   * Returns a new channel, which is the concatenation of all the channels that are written out by
   * this channel. This method may only be called on channels that output other channels.
   */
  final def concatOut[Env1 <: Env, InErr1 <: InErr, InElem1 <: InElem, InDone1 <: InDone, OutErr1 >: OutErr, OutElem2](
    implicit ev: OutElem <:< ZChannel[Env1, InErr1, InElem1, InDone1, OutErr1, OutElem2, Any]
  ): ZChannel[Env1, InErr1, InElem1, InDone1, OutErr1, OutElem2, Any] =
    ZChannel.concatAll(self.mapOut(ev))

  def contramap[InDone0](f: InDone0 => InDone): ZChannel[Env, InErr, InElem, InDone0, OutErr, OutElem, OutDone] = {
    lazy val reader: ZChannel[Any, InErr, InElem, InDone0, InErr, InElem, InDone] =
      ZChannel.readWith(
        (in: InElem) => ZChannel.write(in) *> reader,
        (err: InErr) => ZChannel.fail(err),
        (done0: InDone0) => ZChannel.end(f(done0))
      )

    reader >>> self
  }

  def contramapIn[InElem0](f: InElem0 => InElem): ZChannel[Env, InErr, InElem0, InDone, OutErr, OutElem, OutDone] = {
    lazy val reader: ZChannel[Any, InErr, InElem0, InDone, InErr, InElem, InDone] =
      ZChannel.readWith(
        (in: InElem0) => ZChannel.write(f(in)) *> reader,
        (err: InErr) => ZChannel.fail(err),
        (done: InDone) => ZChannel.end(done)
      )

    reader >>> self
  }

  def contramapInZIO[InElem0, Env1 <: Env](
    f: InElem0 => ZIO[Env1, InErr, InElem]
  ): ZChannel[Env1, InErr, InElem0, InDone, OutErr, OutElem, OutDone] = {
    lazy val reader: ZChannel[Env1, InErr, InElem0, InDone, InErr, InElem, InDone] =
      ZChannel.readWith(
        (in: InElem0) => ZChannel.fromZIO(f(in)).flatMap(ZChannel.write(_)) *> reader,
        (err: InErr) => ZChannel.fail(err),
        (done: InDone) => ZChannel.end(done)
      )

    reader >>> self
  }

  def contramapZIO[InDone0, Env1 <: Env](
    f: InDone0 => ZIO[Env1, InErr, InDone]
  ): ZChannel[Env1, InErr, InElem, InDone0, OutErr, OutElem, OutDone] = {
    lazy val reader: ZChannel[Env1, InErr, InElem, InDone0, InErr, InElem, InDone] =
      ZChannel.readWith(
        (in: InElem) => ZChannel.write(in) *> reader,
        (err: InErr) => ZChannel.fail(err),
        (done0: InDone0) => ZChannel.fromZIO(f(done0))
      )

    reader >>> self
  }

  /**
   * Returns a new channel, which is the same as this one, except that all the outputs are
   * collected and bundled into a tuple together with the terminal value of this channel.
   *
   * As the channel returned from this channel collects all of this channel's output into an in-
   * memory chunk, it is not safe to call this method on channels that output a large or unbounded
   * number of values.
   */
  def doneCollect: ZChannel[Env, InErr, InElem, InDone, OutErr, Nothing, (Chunk[OutElem], OutDone)] =
    ZChannel.effectSuspendTotal {
      val builder = ChunkBuilder.make[OutElem]()

      lazy val reader: ZChannel[Env, OutErr, OutElem, OutDone, OutErr, Nothing, OutDone] =
        ZChannel.readWith(
          (out: OutElem) => ZChannel.effectTotal(builder += out) *> reader,
          (e: OutErr) => ZChannel.fail(e),
          (z: OutDone) => ZChannel.end(z)
        )

      (self pipeTo reader).flatMap(z => ZChannel.effectTotal((builder.result(), z)))
    }

  /**
   * Returns a new channel which reads all the elements from upstream's output channel
   * and ignores them, then terminates with the upstream result value.
   */
  def drain: ZChannel[Env, InErr, InElem, InDone, OutErr, Nothing, OutDone] = {
    lazy val drainer: ZChannel[Env, OutErr, OutElem, OutDone, OutErr, Nothing, OutDone] =
      ZChannel.readWith(
        (_: OutElem) => drainer,
        (e: OutErr) => ZChannel.fail(e),
        (z: OutDone) => ZChannel.end(z)
      )

    self.pipeTo(drainer)
  }

  def embedInput[InErr2, InElem2, InDone2](input: AsyncInputProducer[InErr2, InElem2, InDone2])(implicit
    noInputErrors: Any <:< InErr,
    noInputElements: Any <:< InElem,
    noInputDone: Any <:< InDone
  ): ZChannel[Env, InErr2, InElem2, InDone2, OutErr, OutElem, OutDone] =
    ZChannel.Bridge(input, self.asInstanceOf[ZChannel[Env, Any, Any, Any, OutErr, OutElem, OutDone]])

  /**
   * Returns a new channel, which is the same as this one, except it will be interrupted when the
   * specified effect completes. If the effect completes successfully before the underlying channel
   * is done, then the returned channel will yield the success value of the effect as its terminal
   * value. On the other hand, if the underlying channel finishes first, then the returned channel
   * will yield the success value of the underlying channel as its terminal value.
   */
  final def interruptWhen[Env1 <: Env, OutErr1 >: OutErr, OutDone1 >: OutDone](
    io: ZIO[Env1, OutErr1, OutDone1]
  ): ZChannel[Env1, InErr, InElem, InDone, OutErr1, OutElem, OutDone1] =
    self.mergeWith(ZChannel.fromZIO(io))(
      selfDone => ZChannel.MergeDecision.done(ZIO.done(selfDone)),
      ioDone => ZChannel.MergeDecision.done(ZIO.done(ioDone))
    )

  /**
   * Returns a new channel, which is the same as this one, except it will be interrupted when the
   * specified promise is completed. If the promise is completed before the underlying channel is
   * done, then the returned channel will yield the value of the promise. Otherwise, if the
   * underlying channel finishes first, then the returned channel will yield the value of the
   * underlying channel.
   */
  final def interruptWhen[OutErr1 >: OutErr, OutDone1 >: OutDone](
    promise: Promise[OutErr1, OutDone1]
  ): ZChannel[Env, InErr, InElem, InDone, OutErr1, OutElem, OutDone1] =
    interruptWhen(promise.await)

  /**
   * Returns a new channel that collects the output and terminal value of this channel, which it
   * then writes as output of the returned channel.
   */
  final def emitCollect: ZChannel[Env, InErr, InElem, InDone, OutErr, (Chunk[OutElem], OutDone), Unit] =
    doneCollect.flatMap(t => ZChannel.write(t))

  /**
   * Returns a new channel with an attached finalizer. The finalizer is guaranteed to be executed
   * so long as the channel begins execution (and regardless of whether or not it completes).
   */
  final def ensuringWith[Env1 <: Env](
    finalizer: Exit[OutErr, OutDone] => URIO[Env1, Any]
  ): ZChannel[Env1, InErr, InElem, InDone, OutErr, OutElem, OutDone] =
    ZChannel.Ensuring(self, finalizer)

  final def ensuring[Env1 <: Env](
    finalizer: URIO[Env1, Any]
  ): ZChannel[Env1, InErr, InElem, InDone, OutErr, OutElem, OutDone] =
    ensuringWith(_ => finalizer)

  /**
   * Returns a new channel, which sequentially combines this channel, together with the provided
   * factory function, which creates a second channel based on the terminal value of this channel.
   * The result is a channel that will first perform the functions of this channel, before
   * performing the functions of the created channel (including yielding its terminal value).
   */
  final def flatMap[
    Env1 <: Env,
    InErr1 <: InErr,
    InElem1 <: InElem,
    InDone1 <: InDone,
    OutErr1 >: OutErr,
    OutElem1 >: OutElem,
    OutDone2
  ](
    f: OutDone => ZChannel[Env1, InErr1, InElem1, InDone1, OutErr1, OutElem1, OutDone2]
  ): ZChannel[Env1, InErr1, InElem1, InDone1, OutErr1, OutElem1, OutDone2] =
    ZChannel.Fold(self, new ZChannel.Fold.K(f, ZChannel.Fold.failCauseIdentity[OutErr1]))

  /**
   * Returns a new channel, which flattens the terminal value of this channel. This function may
   * only be called if the terminal value of this channel is another channel of compatible types.
   */
  final def flatten[
    Env1 <: Env,
    InErr1 <: InErr,
    InElem1 <: InElem,
    InDone1 <: InDone,
    OutErr1 >: OutErr,
    OutElem1 >: OutElem,
    OutDone2
  ](implicit
    ev: OutDone <:< ZChannel[Env1, InErr1, InElem1, InDone1, OutErr1, OutElem1, OutDone2]
  ): ZChannel[Env1, InErr1, InElem1, InDone1, OutErr1, OutElem1, OutDone2] =
    self.flatMap(ev)

  /**
   */
  final def foldChannel[
    Env1 <: Env,
    InErr1 <: InErr,
    InElem1 <: InElem,
    InDone1 <: InDone,
    OutErr1,
    OutElem1 >: OutElem,
    OutDone1
  ](
    onErr: OutErr => ZChannel[Env1, InErr1, InElem1, InDone1, OutErr1, OutElem1, OutDone1],
    onSucc: OutDone => ZChannel[Env1, InErr1, InElem1, InDone1, OutErr1, OutElem1, OutDone1]
  ): ZChannel[Env1, InErr1, InElem1, InDone1, OutErr1, OutElem1, OutDone1] =
    foldCauseChannel(
      _.failureOrCause match {
        case Left(err)    => onErr(err)
        case Right(cause) => ZChannel.failCause(cause)
      },
      onSucc
    )

  /**
   */
  final def foldCauseChannel[
    Env1 <: Env,
    InErr1 <: InErr,
    InElem1 <: InElem,
    InDone1 <: InDone,
    OutErr1,
    OutElem1 >: OutElem,
    OutDone1
  ](
    onErr: Cause[OutErr] => ZChannel[Env1, InErr1, InElem1, InDone1, OutErr1, OutElem1, OutDone1],
    onSucc: OutDone => ZChannel[Env1, InErr1, InElem1, InDone1, OutErr1, OutElem1, OutDone1]
  ): ZChannel[Env1, InErr1, InElem1, InDone1, OutErr1, OutElem1, OutDone1] =
    ZChannel.Fold(self, new ZChannel.Fold.K(onSucc, onErr))

  /**
   * Returns a new channel that will perform the operations of this one, until failure, and then
   * it will switch over to the operations of the specified fallback channel.
   */
  final def orElse[
    Env1 <: Env,
    InErr1 <: InErr,
    InElem1 <: InElem,
    InDone1 <: InDone,
    OutErr2,
    OutElem1 >: OutElem,
    OutDone1 >: OutDone
  ](
    that: => ZChannel[Env1, InErr1, InElem1, InDone1, OutErr2, OutElem1, OutDone1]
  ): ZChannel[Env1, InErr1, InElem1, InDone1, OutErr2, OutElem1, OutDone1] =
    self.catchAll(_ => that)

  /**
   * Returns a new channel, which is the same as this one, except the terminal value of the
   * returned channel is created by applying the specified function to the terminal value of this
   * channel.
   */
  final def map[OutDone2](f: OutDone => OutDone2): ZChannel[Env, InErr, InElem, InDone, OutErr, OutElem, OutDone2] =
    self.flatMap(z => ZChannel.succeed(f(z)))

  /**
   * Returns a new channel, which is the same as this one, except the failure value of the returned
   * channel is created by applying the specified function to the failure value of this channel.
   */
  final def mapError[OutErr2](f: OutErr => OutErr2): ZChannel[Env, InErr, InElem, InDone, OutErr2, OutElem, OutDone] =
    mapErrorCause(_.map(f))

  /**
   * A more powerful version of [[mapError]] which also surfaces the [[Cause]] of the channel failure
   */
  final def mapErrorCause[OutErr2](
    f: Cause[OutErr] => Cause[OutErr2]
  ): ZChannel[Env, InErr, InElem, InDone, OutErr2, OutElem, OutDone] =
    catchAllCause((cause: Cause[OutErr]) => ZChannel.failCause(f(cause)))

  /**
   * Returns a new channel, which is the same as this one, except the terminal value of the
   * returned channel is created by applying the specified effectful function to the terminal value
   * of this channel.
   */
  final def mapZIO[Env1 <: Env, OutErr1 >: OutErr, OutDone2](
    f: OutDone => ZIO[Env1, OutErr1, OutDone2]
  ): ZChannel[Env1, InErr, InElem, InDone, OutErr1, OutElem, OutDone2] =
    self.flatMap(z => ZChannel.fromZIO(f(z)))

  /**
   * Returns a new channel, which is the merge of this channel and the specified channel, where
   * the behavior of the returned channel on left or right early termination is decided by the
   * specified `leftDone` and `rightDone` merge decisions.
   */
  final def mergeWith[
    Env1 <: Env,
    InErr1 <: InErr,
    InElem1 <: InElem,
    InDone1 <: InDone,
    OutErr2,
    OutErr3,
    OutElem1 >: OutElem,
    OutDone2,
    OutDone3
  ](that: ZChannel[Env1, InErr1, InElem1, InDone1, OutErr2, OutElem1, OutDone2])(
    leftDone: Exit[OutErr, OutDone] => ZChannel.MergeDecision[Env1, OutErr2, OutDone2, OutErr3, OutDone3],
    rightDone: Exit[OutErr2, OutDone2] => ZChannel.MergeDecision[Env1, OutErr, OutDone, OutErr3, OutDone3]
  ): ZChannel[Env1, InErr1, InElem1, InDone1, OutErr3, OutElem1, OutDone3] = {
    type MergeState = ZChannel.MergeState[Env1, OutErr, OutErr2, OutErr3, OutElem1, OutDone, OutDone2, OutDone3]
    import ZChannel.MergeState._

    val m =
      for {
        input      <- SingleProducerAsyncInput.make[InErr1, InElem1, InDone1].toManaged
        queueReader = ZChannel.fromInput(input)
        pullL      <- (queueReader >>> self).toPull
        pullR      <- (queueReader >>> that).toPull
      } yield {
        def handleSide[Err, Done, Err2, Done2](
          exit: Exit[Either[Err, Done], OutElem1],
          fiber: Fiber[Either[Err2, Done2], OutElem1],
          pull: ZIO[Env1, Either[Err, Done], OutElem1]
        )(
          done: Exit[Err, Done] => ZChannel.MergeDecision[Env1, Err2, Done2, OutErr3, OutDone3],
          both: (Fiber[Either[Err, Done], OutElem1], Fiber[Either[Err2, Done2], OutElem1]) => MergeState,
          single: (Exit[Err2, Done2] => ZIO[Env1, OutErr3, OutDone3]) => MergeState
        ) =
          exit match {
            case Exit.Success(elem) =>
              pull.fork.map { leftFiber =>
                ZChannel.write(elem) *> go(both(leftFiber, fiber))
              }

            case Exit.Failure(cause) =>
              done(Cause.flipCauseEither(cause).fold(Exit.failCause(_), Exit.succeed(_))) match {
                case ZChannel.MergeDecision.Done(zio) =>
                  UIO.succeed(ZChannel.fromZIO(fiber.interrupt *> zio))
                case ZChannel.MergeDecision.Await(f) =>
                  fiber.await.map {
                    case Exit.Success(elem) => ZChannel.write(elem) *> go(single(f))
                    case Exit.Failure(cause) =>
                      ZChannel
                        .fromZIO(f(Cause.flipCauseEither(cause).fold(Exit.failCause(_), Exit.succeed(_))))
                  }
              }
          }

        def go(state: MergeState): ZChannel[Env1, Any, Any, Any, OutErr3, OutElem1, OutDone3] =
          state match {
            case BothRunning(leftFiber, rightFiber) =>
              val lj: ZIO[Env1, Either[OutErr, OutDone], OutElem1]   = leftFiber.join
              val rj: ZIO[Env1, Either[OutErr2, OutDone2], OutElem1] = rightFiber.join

              ZChannel.unwrap {
                lj.raceWith(rj)(
                  (leftEx, _) => handleSide(leftEx, rightFiber, pullL)(leftDone, BothRunning(_, _), LeftDone(_)),
                  (rightEx, _) =>
                    handleSide(rightEx, leftFiber, pullR)(rightDone, (l, r) => BothRunning(r, l), RightDone(_))
                )
              }

            case LeftDone(f) =>
              ZChannel.unwrap {
                pullR.exit.map {
                  case Exit.Success(elem) => ZChannel.write(elem) *> go(LeftDone(f))
                  case Exit.Failure(cause) =>
                    ZChannel
                      .fromZIO(f(Cause.flipCauseEither(cause).fold(Exit.failCause(_), Exit.succeed(_))))
                }
              }

            case RightDone(f) =>
              ZChannel.unwrap {
                pullL.exit.map {
                  case Exit.Success(elem) => ZChannel.write(elem) *> go(RightDone(f))
                  case Exit.Failure(cause) =>
                    ZChannel
                      .fromZIO(f(Cause.flipCauseEither(cause).fold(Exit.failCause(_), Exit.succeed(_))))
                }
              }
          }

        ZChannel.fromZIO(pullL.fork.zipWith(pullR.fork)(BothRunning(_, _): MergeState)).flatMap(go).embedInput(input)
      }

    ZChannel.unwrapManaged(m)
  }

  def mergeMap[Env1 <: Env, InErr1 <: InErr, InElem1 <: InElem, InDone1 <: InDone, OutErr1 >: OutErr, OutElem2](
    n: Long
  )(
    f: OutElem => ZChannel[Env1, InErr1, InElem1, InDone1, OutErr1, OutElem2, Any]
  ): ZChannel[Env1, InErr1, InElem1, InDone1, OutErr1, OutElem2, Any] =
    ZChannel.mergeAll(self.mapOut(f), n)

  def mergeMapWith[
    Env1 <: Env,
    InErr1 <: InErr,
    InElem1 <: InElem,
    InDone1 <: InDone,
    OutErr1 >: OutErr,
    OutElem2,
    OutDone1 >: OutDone
  ](n: Long)(
    f: OutElem => ZChannel[Env1, InErr1, InElem1, InDone1, OutErr1, OutElem2, OutDone1]
  )(g: (OutDone1, OutDone1) => OutDone1): ZChannel[Env1, InErr1, InElem1, InDone1, OutErr1, OutElem2, OutDone1] =
    ???

  def mapOut[OutElem2](f: OutElem => OutElem2): ZChannel[Env, InErr, InElem, InDone, OutErr, OutElem2, OutDone] = {
    lazy val reader: ZChannel[Env, OutErr, OutElem, OutDone, OutErr, OutElem2, OutDone] =
      ZChannel.readWith(
        out => ZChannel.write(f(out)) *> reader,
        (e: OutErr) => ZChannel.fail(e),
        (z: OutDone) => ZChannel.end(z)
      )

    self >>> reader
  }

  def mapOutZIO[Env1 <: Env, OutErr1 >: OutErr, OutElem2](
    f: OutElem => ZIO[Env1, OutErr1, OutElem2]
  ): ZChannel[Env1, InErr, InElem, InDone, OutErr1, OutElem2, OutDone] = {
    lazy val reader: ZChannel[Env1, OutErr, OutElem, OutDone, OutErr1, OutElem2, OutDone] =
      ZChannel.readWith(
        (out: OutElem) => ZChannel.fromZIO(f(out)).flatMap(ZChannel.write(_)) *> reader,
        (e: OutErr1) => ZChannel.fail(e),
        (z: OutDone) => ZChannel.end(z)
      )

    self >>> reader
  }

  def mapOutZIOPar[Env1 <: Env, OutErr1 >: OutErr, OutElem2](n: Int)(
    f: OutElem => ZIO[Env1, OutErr1, OutElem2]
  ): ZChannel[Env1, InErr, InElem, InDone, OutErr1, OutElem2, OutDone] =
    ZChannel.managed {
      ZManaged.withChildren { getChildren =>
        for {
          _           <- ZManaged.finalizer(getChildren.flatMap(Fiber.interruptAll(_)))
          queue       <- Queue.bounded[ZIO[Env1, Either[OutErr1, OutDone], OutElem2]](n).toManagedWith(_.shutdown)
          errorSignal <- Promise.makeManaged[OutErr1, Nothing]
          permits     <- Semaphore.make(n.toLong).toManaged
          pull        <- self.toPull
          _ <- pull
                 .foldCauseZIO(
                   Cause.flipCauseEither[OutErr1, OutDone](_) match {
                     case Left(cause) =>
                       queue.offer(ZIO.failCause(cause.map(Left(_))))
                     case Right(outDone) =>
                       permits.withPermits(n.toLong)(ZIO.unit).interruptible *> queue.offer(ZIO.fail(Right(outDone)))
                   },
                   outElem =>
                     for {
                       p     <- Promise.make[OutErr1, OutElem2]
                       latch <- Promise.make[Nothing, Unit]
                       _     <- queue.offer(p.await.mapError(Left(_)))
                       _ <- permits.withPermit {
                              latch.succeed(()) *>
                                (errorSignal.await raceFirst f(outElem))
                                  .tapCause(errorSignal.failCause)
                                  .to(p)
                            }.fork
                       _ <- latch.await
                     } yield ()
                 )
                 .forever
                 .interruptible
                 .forkManaged
        } yield queue
      }
    } { queue =>
      lazy val consumer: ZChannel[Env1, Any, Any, Any, OutErr1, OutElem2, OutDone] =
        ZChannel.unwrap[Env1, Any, Any, Any, OutErr1, OutElem2, OutDone] {
          queue.take.flatten.foldCause(
            Cause.flipCauseEither[OutErr1, OutDone](_) match {
              case Right(outDone) => ZChannel.end(outDone)
              case Left(cause)    => ZChannel.failCause(cause)
            },
            outElem => ZChannel.write(outElem) *> consumer
          )
        }

      consumer
    }

  def mergeOut[Env1 <: Env, InErr1 <: InErr, InElem1 <: InElem, InDone1 <: InDone, OutErr1 >: OutErr, OutElem2](
    n: Long
  )(implicit
    ev: OutElem <:< ZChannel[Env1, InErr1, InElem1, InDone1, OutErr1, OutElem2, Any]
  ): ZChannel[Env1, InErr1, InElem1, InDone1, OutErr1, OutElem2, Any] =
    ZChannel.mergeAll(self.mapOut(ev), n)

  def mergeOutWith[
    Env1 <: Env,
    InErr1 <: InErr,
    InElem1 <: InElem,
    InDone1 <: InDone,
    OutErr1 >: OutErr,
    OutElem2,
    OutDone1 >: OutDone
  ](n: Long)(f: (OutDone1, OutDone1) => OutDone1)(implicit
    ev: OutElem <:< ZChannel[Env1, InErr1, InElem1, InDone1, OutErr1, OutElem2, OutDone1]
  ): ZChannel[Env1, InErr1, InElem1, InDone1, OutErr1, OutElem2, OutDone1] =
    ZChannel.mergeAllWith(self.mapOut(ev), n)(f)

  lazy val never: ZChannel[Any, Any, Any, Any, Nothing, Nothing, Nothing] =
    ZChannel.fromZIO(ZIO.never)

  def orDie(implicit ev: OutErr <:< Throwable): ZChannel[Env, InErr, InElem, InDone, Nothing, OutElem, OutDone] =
    orDieWith(ev)

  def orDieWith(f: OutErr => Throwable): ZChannel[Env, InErr, InElem, InDone, Nothing, OutElem, OutDone] =
    self.catchAll(e => throw f(e))

  def pipeTo[Env1 <: Env, OutErr2, OutElem2, OutDone2](
    that: => ZChannel[Env1, OutErr, OutElem, OutDone, OutErr2, OutElem2, OutDone2]
  ): ZChannel[Env1, InErr, InElem, InDone, OutErr2, OutElem2, OutDone2] =
    ZChannel.PipeTo(() => self, () => that)

  /**
   * Provides the channel with its required environment, which eliminates
   * its dependency on `Env`.
   */
  final def provide(env: Env)(implicit
    ev: NeedsEnv[Env]
  ): ZChannel[Any, InErr, InElem, InDone, OutErr, OutElem, OutDone] =
    ZChannel.Provide(env, self)

  def repeated: ZChannel[Env, InErr, InElem, InDone, OutErr, OutElem, Nothing] =
    self *> self.repeated

  def runManaged(implicit
    ev1: Any <:< InElem,
    ev2: OutElem <:< Nothing
  ): ZManaged[Env, OutErr, OutDone] =
    ZManaged
      .acquireReleaseExitWith(
        UIO(new ChannelExecutor[Env, InErr, InElem, InDone, OutErr, OutElem, OutDone](() => self, null))
      ) { (exec, exit) =>
        val finalize = exec.close(exit)
        if (finalize ne null) finalize
        else ZIO.unit
      }
      .mapZIO { exec =>
        ZIO.suspendSucceed {
          def interpret(channelState: ChannelExecutor.ChannelState[Env, OutErr]): ZIO[Env, OutErr, OutDone] =
            channelState match {
              case ChannelState.Effect(zio) =>
                zio *> interpret(exec.run().asInstanceOf[ChannelState[Env, OutErr]])
              case ChannelState.Emit =>
                // Can't really happen because Out <:< Nothing. So just skip ahead.
                interpret(exec.run().asInstanceOf[ChannelState[Env, OutErr]])
              case ChannelState.Done =>
                ZIO.done(exec.getDone)
            }

          interpret(exec.run().asInstanceOf[ChannelState[Env, OutErr]])
        }
      }

  def run(implicit ev1: Any <:< InElem, ev2: OutElem <:< Nothing): ZIO[Env, OutErr, OutDone] =
    runManaged.useNow

  def runCollect(implicit ev1: Any <:< InElem): ZIO[Env, OutErr, (Chunk[OutElem], OutDone)] =
    doneCollect.run

  def runDrain(implicit ev1: Any <:< InElem): ZIO[Env, OutErr, OutDone] =
    self.drain.run

  def unit: ZChannel[Env, InErr, InElem, InDone, OutErr, OutElem, Unit] =
    self.as(())

  def toPull: ZManaged[Env, Nothing, ZIO[Env, Either[OutErr, OutDone], OutElem]] =
    ZManaged
      .acquireReleaseExitWith(
        UIO(new ChannelExecutor[Env, InErr, InElem, InDone, OutErr, OutElem, OutDone](() => self, null))
      ) { (exec, exit) =>
        val finalize = exec.close(exit)
        if (finalize ne null) finalize
        else ZIO.unit
      }
      .map { exec =>
        def interpret(
          channelState: ChannelExecutor.ChannelState[Env, OutErr]
        ): ZIO[Env, Either[OutErr, OutDone], OutElem] =
          channelState match {
            case ChannelState.Done =>
              exec.getDone match {
                case Exit.Success(done)  => ZIO.fail(Right(done))
                case Exit.Failure(cause) => ZIO.failCause(cause.map(Left(_)))
              }
            case ChannelState.Emit =>
              ZIO.succeed(exec.getEmit)
            case ChannelState.Effect(zio) =>
              zio.mapError(Left(_)) *>
                interpret(exec.run().asInstanceOf[ChannelState[Env, OutErr]])
          }

        ZIO.suspendSucceed(interpret(exec.run().asInstanceOf[ChannelState[Env, OutErr]]))
      }

  def zip[
    Env1 <: Env,
    InErr1 <: InErr,
    InElem1 <: InElem,
    InDone1 <: InDone,
    OutErr1 >: OutErr,
    OutElem1 >: OutElem,
    OutDone2
  ](
    that: => ZChannel[Env1, InErr1, InElem1, InDone1, OutErr1, OutElem1, OutDone2]
  )(implicit
    zippable: Zippable[OutDone, OutDone2]
  ): ZChannel[Env1, InErr1, InElem1, InDone1, OutErr1, OutElem1, zippable.Out] =
    self.flatMap(l => that.map(r => zippable.zip(l, r)))

  def zipOutWith[
    Env1 <: Env,
    InErr1 <: InErr,
    InElem1 <: InElem,
    InDone1 <: InDone,
    OutErr1 >: OutErr,
    OutElem2,
    OutElem3,
    OutDone2
  ](that: => ZChannel[Env1, InErr1, InElem1, InDone1, OutErr1, OutElem2, OutDone2])(
    f: (OutElem, OutElem2) => OutElem3
  ): ZChannel[Env1, InErr1, InElem1, InDone1, OutErr1, OutElem3, (OutDone, OutDone2)] =
    ???

  def zipLeft[
    Env1 <: Env,
    InErr1 <: InErr,
    InElem1 <: InElem,
    InDone1 <: InDone,
    OutErr1 >: OutErr,
    OutElem1 >: OutElem,
    OutDone2
  ](
    that: => ZChannel[Env1, InErr1, InElem1, InDone1, OutErr1, OutElem1, OutDone2]
  ): ZChannel[Env1, InErr1, InElem1, InDone1, OutErr1, OutElem1, OutDone] =
    (self zip that).map(_._1)

  def zipPar[
    Env1 <: Env,
    InErr1 <: InErr,
    InElem1 <: InElem,
    InDone1 <: InDone,
    OutErr1 >: OutErr,
    OutElem1 >: OutElem,
    OutDone2
  ](
    that: => ZChannel[Env1, InErr1, InElem1, InDone1, OutErr1, OutElem1, OutDone2]
  )(implicit
    zippable: Zippable[OutDone, OutDone2]
  ): ZChannel[Env1, InErr1, InElem1, InDone1, OutErr1, OutElem1, zippable.Out] =
    self.mergeWith(that)(
      exit1 =>
        ZChannel.MergeDecision.Await[Env1, OutErr1, OutDone2, OutErr1, zippable.Out](exit2 =>
          ZIO.done(exit1.zip(exit2))
        ),
      exit2 =>
        ZChannel.MergeDecision.Await[Env1, OutErr1, OutDone, OutErr1, zippable.Out](exit1 => ZIO.done(exit1.zip(exit2)))
    )

  def zipParLeft[
    Env1 <: Env,
    InErr1 <: InErr,
    InElem1 <: InElem,
    InDone1 <: InDone,
    OutErr1 >: OutErr,
    OutElem1 >: OutElem,
    OutDone2
  ](
    that: => ZChannel[Env1, InErr1, InElem1, InDone1, OutErr1, OutElem1, OutDone2]
  ): ZChannel[Env1, InErr1, InElem1, InDone1, OutErr1, OutElem1, OutDone] =
    (self zipPar that).map(_._1)

  def zipParRight[
    Env1 <: Env,
    InErr1 <: InErr,
    InElem1 <: InElem,
    InDone1 <: InDone,
    OutErr1 >: OutErr,
    OutElem1 >: OutElem,
    OutDone2
  ](
    that: => ZChannel[Env1, InErr1, InElem1, InDone1, OutErr1, OutElem1, OutDone2]
  ): ZChannel[Env1, InErr1, InElem1, InDone1, OutErr1, OutElem1, OutDone2] =
    (self zipPar that).map(_._2)

  def zipRight[
    Env1 <: Env,
    InErr1 <: InErr,
    InElem1 <: InElem,
    InDone1 <: InDone,
    OutErr1 >: OutErr,
    OutElem1 >: OutElem,
    OutDone2
  ](
    that: => ZChannel[Env1, InErr1, InElem1, InDone1, OutErr1, OutElem1, OutDone2]
  ): ZChannel[Env1, InErr1, InElem1, InDone1, OutErr1, OutElem1, OutDone2] =
    (self zip that).map(_._2)
}

object ZChannel {
  private[zio] final case class PipeTo[
    Env,
    InErr,
    InElem,
    InDone,
    OutErr,
    OutErr2,
    OutElem,
    OutElem2,
    OutDone,
    OutDone2
  ](
    left: () => ZChannel[Env, InErr, InElem, InDone, OutErr, OutElem, OutDone],
    right: () => ZChannel[Env, OutErr, OutElem, OutDone, OutErr2, OutElem2, OutDone2]
  ) extends ZChannel[Env, InErr, InElem, InDone, OutErr2, OutElem2, OutDone2]

  private[zio] final case class Read[Env, InErr, InElem, InDone, OutErr, OutErr2, OutElem, OutElem2, OutDone, OutDone2](
    more: InElem => ZChannel[Env, InErr, InElem, InDone, OutErr2, OutElem2, OutDone2],
    done: Fold.K[Env, InErr, InElem, InDone, OutErr, OutErr2, OutElem2, OutDone, OutDone2]
  ) extends ZChannel[Env, InErr, InElem, InDone, OutErr2, OutElem2, OutDone2]
  private[zio] final case class Done[OutDone](terminal: OutDone)
      extends ZChannel[Any, Any, Any, Any, Nothing, Nothing, OutDone]
  private[zio] final case class Halt[OutErr](error: () => Cause[OutErr])
      extends ZChannel[Any, Any, Any, Any, OutErr, Nothing, Nothing]
  private[zio] final case class Effect[Env, OutErr, OutDone](zio: ZIO[Env, OutErr, OutDone])
      extends ZChannel[Env, Any, Any, Any, OutErr, Nothing, OutDone]
  private[zio] final case class Emit[OutElem](out: OutElem) extends ZChannel[Any, Any, Any, Any, Nothing, OutElem, Unit]
  private[zio] final case class EffectTotal[OutDone](effect: () => OutDone)
      extends ZChannel[Any, Any, Any, Any, Nothing, Nothing, OutDone]
  private[zio] final case class EffectSuspendTotal[Env, InErr, InElem, InDone, OutErr, OutElem, OutDone](
    effect: () => ZChannel[Env, InErr, InElem, InDone, OutErr, OutElem, OutDone]
  ) extends ZChannel[Env, InErr, InElem, InDone, OutErr, OutElem, OutDone]
  private[zio] final case class Ensuring[Env, InErr, InElem, InDone, OutErr, OutElem, OutDone](
    channel: ZChannel[Env, InErr, InElem, InDone, OutErr, OutElem, OutDone],
    finalizer: Exit[OutErr, OutDone] => ZIO[Env, Nothing, Any]
  ) extends ZChannel[Env, InErr, InElem, InDone, OutErr, OutElem, OutDone]
  private[zio] final case class ConcatAll[
    Env,
    InErr,
    InElem,
    InDone,
    OutErr,
    OutElem,
    OutElem2,
    OutDone,
    OutDone2,
    OutDone3
  ](
    combineInners: (OutDone, OutDone) => OutDone,
    combineAll: (OutDone, OutDone2) => OutDone3,
    value: ZChannel[Env, InErr, InElem, InDone, OutErr, OutElem, OutDone2],
    k: OutElem => ZChannel[Env, InErr, InElem, InDone, OutErr, OutElem2, OutDone]
  ) extends ZChannel[Env, InErr, InElem, InDone, OutErr, OutElem2, OutDone3]
  private[zio] final case class Fold[Env, InErr, InElem, InDone, OutErr, OutErr2, OutElem, OutDone, OutDone2](
    value: ZChannel[Env, InErr, InElem, InDone, OutErr, OutElem, OutDone],
    k: Fold.K[Env, InErr, InElem, InDone, OutErr, OutErr2, OutElem, OutDone, OutDone2]
  ) extends ZChannel[Env, InErr, InElem, InDone, OutErr2, OutElem, OutDone2]
  object Fold {
    sealed abstract class Continuation[-Env, -InErr, -InElem, -InDone, OutErr, +OutErr2, +OutElem, OutDone, +OutDone2]
    private[zio] final case class K[-Env, -InErr, -InElem, -InDone, OutErr, +OutErr2, +OutElem, OutDone, +OutDone2](
      onSuccess: OutDone => ZChannel[Env, InErr, InElem, InDone, OutErr2, OutElem, OutDone2],
      onHalt: Cause[OutErr] => ZChannel[Env, InErr, InElem, InDone, OutErr2, OutElem, OutDone2]
    ) extends Continuation[Env, InErr, InElem, InDone, OutErr, OutErr2, OutElem, OutDone, OutDone2] {
      def onExit(exit: Exit[OutErr, OutDone]): ZChannel[Env, InErr, InElem, InDone, OutErr2, OutElem, OutDone2] =
        exit match {
          case Exit.Success(value) => onSuccess(value)
          case Exit.Failure(cause) => onHalt(cause)
        }
    }
    private[zio] final case class Finalizer[Env, OutErr, OutDone](finalizer: Exit[OutErr, OutDone] => URIO[Env, Any])
        extends Continuation[Env, Any, Any, Any, OutErr, Nothing, Nothing, OutDone, Nothing]

    private[this] val SuccessIdentity: Any => ZChannel[Any, Any, Any, Any, Nothing, Nothing, Any] =
      ZChannel.end(_)
    def successIdentity[Z]: Z => ZChannel[Any, Any, Any, Any, Nothing, Nothing, Z] =
      SuccessIdentity.asInstanceOf[Z => ZChannel[Any, Any, Any, Any, Nothing, Nothing, Z]]

    private[this] val FailCauseIdentity: Cause[Any] => ZChannel[Any, Any, Any, Any, Any, Nothing, Nothing] =
      ZChannel.failCause(_)
    def failCauseIdentity[E]: Cause[E] => ZChannel[Any, Any, Any, Any, E, Nothing, Nothing] =
      FailCauseIdentity.asInstanceOf[Cause[E] => ZChannel[Any, Any, Any, Any, E, Nothing, Nothing]]
  }

  private[zio] final case class Bridge[-Env, -InErr, -InElem, -InDone, +OutErr, +OutElem, +OutDone](
    input: AsyncInputProducer[InErr, InElem, InDone],
    channel: ZChannel[Env, Any, Any, Any, OutErr, OutElem, OutDone]
  ) extends ZChannel[Env, InErr, InElem, InDone, OutErr, OutElem, OutDone]

  private[zio] final case class BracketOut[R, E, Z](
    acquire: ZIO[R, E, Z],
    finalizer: (Z, Exit[Any, Any]) => URIO[R, Any]
  ) extends ZChannel[R, Any, Any, Any, E, Z, Unit]

  private[zio] final case class Provide[Env, InErr, InElem, InDone, OutErr, OutElem, OutDone](
    environment: Env,
    inner: ZChannel[Env, InErr, InElem, InDone, OutErr, OutElem, OutDone]
  ) extends ZChannel[Any, InErr, InElem, InDone, OutErr, OutElem, OutDone]

  sealed abstract class MergeDecision[-R, -E0, -Z0, +E, +Z]
  object MergeDecision {
    case class Done[R, E, Z](zio: ZIO[R, E, Z])                        extends MergeDecision[R, Any, Any, E, Z]
    case class Await[R, E0, Z0, E, Z](f: Exit[E0, Z0] => ZIO[R, E, Z]) extends MergeDecision[R, E0, Z0, E, Z]

    def done[R, E, Z](zio: ZIO[R, E, Z]): MergeDecision[R, Any, Any, E, Z]                      = Done(zio)
    def await[R, E0, Z0, E, Z](f: Exit[E0, Z0] => ZIO[R, E, Z]): MergeDecision[R, E0, Z0, E, Z] = Await(f)
    def awaitConst[R, E, Z](zio: ZIO[R, E, Z]): MergeDecision[R, Any, Any, E, Z]                = Await(_ => zio)
  }

  def acquireReleaseOutWith[Env, OutErr, Acquired](acquire: ZIO[Env, OutErr, Acquired])(
    release: Acquired => URIO[Env, Any]
  ): ZChannel[Env, Any, Any, Any, OutErr, Acquired, Unit] =
    acquireReleaseOutExitWith(acquire)((z, _) => release(z))

  def acquireReleaseOutExitWith[Env, OutErr, Acquired](acquire: ZIO[Env, OutErr, Acquired])(
    release: (Acquired, Exit[Any, Any]) => URIO[Env, Any]
  ): ZChannel[Env, Any, Any, Any, OutErr, Acquired, Unit] =
    BracketOut(acquire, release)

  def acquireReleaseWith[Env, InErr, InElem, InDone, OutErr, Acquired, OutElem2, OutDone](
    acquire: ZIO[Env, OutErr, Acquired]
  )(release: Acquired => URIO[Env, Any])(
    use: Acquired => ZChannel[Env, InErr, InElem, InDone, OutErr, OutElem2, OutDone]
  ): ZChannel[Env, InErr, InElem, InDone, OutErr, OutElem2, OutDone] =
    acquireReleaseExitWith[Env, InErr, InElem, InDone, OutErr, Acquired, OutElem2, OutDone](acquire)((a, _) =>
      release(a)
    )(use)

  def acquireReleaseExitWith[Env, InErr, InElem, InDone, OutErr, Acquired, OutElem2, OutDone](
    acquire: ZIO[Env, OutErr, Acquired]
  )(release: (Acquired, Exit[OutErr, OutDone]) => URIO[Env, Any])(
    use: Acquired => ZChannel[Env, InErr, InElem, InDone, OutErr, OutElem2, OutDone]
  ): ZChannel[Env, InErr, InElem, InDone, OutErr, OutElem2, OutDone] =
    fromZIO(Ref.make[Exit[OutErr, OutDone] => URIO[Env, Any]](_ => UIO.unit)).flatMap { ref =>
      fromZIO(acquire.tap(a => ref.set(release(a, _))).uninterruptible)
        .flatMap(use)
        .ensuringWith(ex => ref.get.flatMap(_.apply(ex)))
    }

  /**
   * Creates a channel backed by a buffer. When the buffer is empty, the channel will simply
   * passthrough its input as output. However, when the buffer is non-empty, the value inside
   * the buffer will be passed along as output.
   */
  def buffer[InErr, InElem, InDone](
    empty: InElem,
    isEmpty: InElem => Boolean,
    ref: Ref[InElem]
  ): ZChannel[Any, InErr, InElem, InDone, InErr, InElem, InDone] =
    unwrap(
      ref.modify { v =>
        if (isEmpty(v))
          (
            ZChannel.readWith(
              (in: InElem) => ZChannel.write(in) *> buffer(empty, isEmpty, ref),
              (err: InErr) => ZChannel.fail(err),
              (done: InDone) => ZChannel.end(done)
            ),
            v
          )
        else
          (ZChannel.write(v) *> buffer(empty, isEmpty, ref), empty)
      }
    )

  def bufferChunk[InErr, InElem, InDone](
    ref: Ref[Chunk[InElem]]
  ): ZChannel[Any, InErr, Chunk[InElem], InDone, InErr, Chunk[InElem], InDone] =
    buffer[InErr, Chunk[InElem], InDone](Chunk.empty, _.isEmpty, ref)

  def concatAll[Env, InErr, InElem, InDone, OutErr, OutElem, OutDone](
    channels: ZChannel[
      Env,
      InErr,
      InElem,
      InDone,
      OutErr,
      ZChannel[Env, InErr, InElem, InDone, OutErr, OutElem, Any],
      Any
    ]
  ): ZChannel[Env, InErr, InElem, InDone, OutErr, OutElem, Any] =
    concatAllWith(channels)((_, _) => (), (_, _) => ())

  def concatAllWith[Env, InErr, InElem, InDone, OutErr, OutElem, OutDone, OutDone2, OutDone3](
    channels: ZChannel[
      Env,
      InErr,
      InElem,
      InDone,
      OutErr,
      ZChannel[Env, InErr, InElem, InDone, OutErr, OutElem, OutDone],
      OutDone2
    ]
  )(
    f: (OutDone, OutDone) => OutDone,
    g: (OutDone, OutDone2) => OutDone3
  ): ZChannel[Env, InErr, InElem, InDone, OutErr, OutElem, OutDone3] =
    ConcatAll(f, g, channels, (channel: ZChannel[Env, InErr, InElem, InDone, OutErr, OutElem, OutDone]) => channel)

  private[zio] def effectTotal[OutDone](effect: => OutDone): ZChannel[Any, Any, Any, Any, Nothing, Nothing, OutDone] =
    EffectTotal(() => effect)

  private[zio] def effectSuspendTotal[Env, InErr, InElem, InDone, OutErr, OutElem, OutDone](
    effect: => ZChannel[Env, InErr, InElem, InDone, OutErr, OutElem, OutDone]
  ): ZChannel[Env, InErr, InElem, InDone, OutErr, OutElem, OutDone] =
    EffectSuspendTotal(() => effect)

  def end[Z](result: => Z): ZChannel[Any, Any, Any, Any, Nothing, Nothing, Z] =
    Done(result)

  def endWith[R, Z](f: R => Z): ZChannel[R, Any, Any, Any, Nothing, Nothing, Z] =
    ZChannel.fromZIO(ZIO.access[R](f))

  def write[Out](out: Out): ZChannel[Any, Any, Any, Any, Nothing, Out, Unit] =
    Emit(out)

  def writeAll[Out](outs: Out*): ZChannel[Any, Any, Any, Any, Nothing, Out, Unit] =
    writeChunk(Chunk.fromIterable(outs))

  def writeChunk[Out](outs: Chunk[Out]): ZChannel[Any, Any, Any, Any, Nothing, Out, Unit] = {
    def writer(idx: Int, len: Int): ZChannel[Any, Any, Any, Any, Nothing, Out, Unit] =
      if (idx == len) ZChannel.unit
      else ZChannel.write(outs(idx)) *> writer(idx + 1, len)

    writer(0, outs.size)
  }

  def fail[E](e: => E): ZChannel[Any, Any, Any, Any, E, Nothing, Nothing] =
    failCause(Cause.fail(e))

  def fromEither[E, A](either: Either[E, A]): ZChannel[Any, Any, Any, Any, E, Nothing, A] =
    either.fold(ZChannel.fail(_), ZChannel.succeed(_))

  def fromOption[A](option: Option[A]): ZChannel[Any, Any, Any, Any, None.type, Nothing, A] =
    option.fold[ZChannel[Any, Any, Any, Any, None.type, Nothing, A]](ZChannel.fail(None))(ZChannel.succeed(_))

  def fromZIO[R, E, A](zio: ZIO[R, E, A]): ZChannel[R, Any, Any, Any, E, Nothing, A] =
    Effect(zio)

  def failCause[E](cause: => Cause[E]): ZChannel[Any, Any, Any, Any, E, Nothing, Nothing] =
    Halt(() => cause)

  def identity[Err, Elem, Done]: ZChannel[Any, Err, Elem, Done, Err, Elem, Done] =
    readWith(
      (in: Elem) => write(in) *> identity[Err, Elem, Done],
      (err: Err) => fail(err),
      (done: Done) => end(done)
    )

  def interrupt(fiberId: Fiber.Id): ZChannel[Any, Any, Any, Any, Nothing, Nothing, Nothing] =
    failCause(Cause.interrupt(fiberId))

  def managed[Env, InErr, InElem, InDone, OutErr, OutElem, OutDone, A](m: ZManaged[Env, OutErr, A])(
    use: A => ZChannel[Env, InErr, InElem, InDone, OutErr, OutElem, OutDone]
  ): ZChannel[Env, InErr, InElem, InDone, OutErr, OutElem, OutDone] =
    acquireReleaseWith[Env, InErr, InElem, InDone, OutErr, ReleaseMap, OutElem, OutDone](ReleaseMap.make)(
      _.releaseAll(
        Exit.unit, // FIXME: BracketOut should be BracketOutExit
        ExecutionStrategy.Sequential
      )
    ) { releaseMap =>
      fromZIO[Env, OutErr, A](
        m.zio
          .provideSome[Env]((_, releaseMap))
          .map(_._2)
      )
        .flatMap(use)
    }

  def managedOut[R, E, A](m: ZManaged[R, E, A]): ZChannel[R, Any, Any, Any, E, A, Any] =
    acquireReleaseOutWith(ReleaseMap.make)(
      _.releaseAll(
        Exit.unit, // FIXME: BracketOut should be BracketOutExit
        ExecutionStrategy.Sequential
      )
    ).concatMap { releaseMap =>
      fromZIO(m.zio.provideSome[R]((_, releaseMap)).map(_._2)).flatMap(write(_))
    }

  def mergeAll[Env, InErr, InElem, InDone, OutErr, OutElem](
    channels: ZChannel[
      Env,
      InErr,
      InElem,
      InDone,
      OutErr,
      ZChannel[Env, InErr, InElem, InDone, OutErr, OutElem, Any],
      Any
    ],
    n: Long
  ): ZChannel[Env, InErr, InElem, InDone, OutErr, OutElem, Any] =
    mergeAllWith(channels, n)((_, _) => ())

  def mergeAllUnbounded[Env, InErr, InElem, InDone, OutErr, OutElem](
    channels: ZChannel[
      Env,
      InErr,
      InElem,
      InDone,
      OutErr,
      ZChannel[Env, InErr, InElem, InDone, OutErr, OutElem, Any],
      Any
    ]
  ): ZChannel[Env, InErr, InElem, InDone, OutErr, OutElem, Any] =
    mergeAll(channels, Long.MaxValue)

  def mergeAllUnboundedWith[Env, InErr, InElem, InDone, OutErr, OutElem, OutDone](
    channels: ZChannel[
      Env,
      InErr,
      InElem,
      InDone,
      OutErr,
      ZChannel[Env, InErr, InElem, InDone, OutErr, OutElem, OutDone],
      OutDone
    ]
  )(f: (OutDone, OutDone) => OutDone): ZChannel[Env, InErr, InElem, InDone, OutErr, OutElem, OutDone] =
    mergeAllWith(channels, Long.MaxValue)(f)

  def mergeAllWith[Env, InErr, InElem, InDone, OutErr, OutElem, OutDone](
    channels: ZChannel[
      Env,
      InErr,
      InElem,
      InDone,
      OutErr,
      ZChannel[Env, InErr, InElem, InDone, OutErr, OutElem, OutDone],
      OutDone
    ],
    n: Long
  )(f: (OutDone, OutDone) => OutDone): ZChannel[Env, InErr, InElem, InDone, OutErr, OutElem, OutDone] =
    ???

  def readWithCause[Env, InErr, InElem, InDone, OutErr, OutElem, OutDone](
    in: InElem => ZChannel[Env, InErr, InElem, InDone, OutErr, OutElem, OutDone],
    halt: Cause[InErr] => ZChannel[Env, InErr, InElem, InDone, OutErr, OutElem, OutDone],
    done: InDone => ZChannel[Env, InErr, InElem, InDone, OutErr, OutElem, OutDone]
  ): ZChannel[Env, InErr, InElem, InDone, OutErr, OutElem, OutDone] =
    Read[Env, InErr, InElem, InDone, InErr, OutErr, OutElem, OutElem, InDone, OutDone](in, new Fold.K(done, halt))

  def readWith[Env, InErr, InElem, InDone, OutErr, OutElem, OutDone](
    in: InElem => ZChannel[Env, InErr, InElem, InDone, OutErr, OutElem, OutDone],
    error: InErr => ZChannel[Env, InErr, InElem, InDone, OutErr, OutElem, OutDone],
    done: InDone => ZChannel[Env, InErr, InElem, InDone, OutErr, OutElem, OutDone]
  ): ZChannel[Env, InErr, InElem, InDone, OutErr, OutElem, OutDone] =
    readWithCause(in, (c: Cause[InErr]) => c.failureOrCause.fold(error, ZChannel.failCause(_)), done)

  def readOrFail[E, In](e: E): ZChannel[Any, Any, In, Any, E, Nothing, In] =
    Read[Any, Any, In, Any, Any, E, Nothing, Nothing, In, In](
      in => Done(in),
      new Fold.K((_: Any) => ZChannel.fail(e), (_: Any) => ZChannel.fail(e))
    )

  def readOrIdentity[Env, Err, InElem, Done, OutElem](
    in: InElem => ZChannel[Any, Err, InElem, Done, Err, OutElem, Done]
  ): ZChannel[Env, Err, InElem, Done, Err, OutElem, Done] =
    readWith(in, ZChannel.fail(_), ZChannel.end(_))

  def read[In]: ZChannel[Any, Any, In, Any, None.type, Nothing, In] =
    readOrFail(None)

  def succeed[Z](z: => Z): ZChannel[Any, Any, Any, Any, Nothing, Nothing, Z] =
    end(z)

  val unit: ZChannel[Any, Any, Any, Any, Nothing, Nothing, Unit] =
    succeed(())

  def unwrap[Env, InErr, InElem, InDone, OutErr, OutElem, OutDone](
    channel: ZIO[Env, OutErr, ZChannel[Env, InErr, InElem, InDone, OutErr, OutElem, OutDone]]
  ): ZChannel[Env, InErr, InElem, InDone, OutErr, OutElem, OutDone] =
    ZChannel.fromZIO(channel).flatten

  def unwrapManaged[Env, InErr, InElem, InDone, OutErr, OutElem, OutDone](
    channel: ZManaged[Env, OutErr, ZChannel[Env, InErr, InElem, InDone, OutErr, OutElem, OutDone]]
  ): ZChannel[Env, InErr, InElem, InDone, OutErr, OutElem, OutDone] =
    ZChannel.concatAllWith(managedOut(channel))((d, _) => d, (d, _) => d)

  def fromHub[Err, Done, Elem](
    hub: Hub[Exit[Either[Err, Done], Elem]]
  ): ZChannel[Any, Any, Any, Any, Err, Elem, Done] =
    ZChannel.managed(hub.subscribe)(fromQueue)

  def fromInput[Err, Elem, Done](
    input: AsyncInputConsumer[Err, Elem, Done]
  ): ZChannel[Any, Any, Any, Any, Err, Elem, Done] =
    ZChannel.unwrap(
      input.takeWith(
        ZChannel.failCause(_),
        ZChannel.write(_) *> fromInput(input),
        ZChannel.end(_)
      )
    )

  def fromQueue[Err, Done, Elem](
    queue: Dequeue[Exit[Either[Err, Done], Elem]]
  ): ZChannel[Any, Any, Any, Any, Err, Elem, Done] =
    ZChannel.fromZIO(queue.take).flatMap {
      case Exit.Success(elem) => write(elem) *> fromQueue(queue)
      case Exit.Failure(cause) =>
        Cause.flipCauseEither(cause) match {
          case Left(cause) => failCause(cause)
          case Right(done) => end(done)
        }
    }

  def toHub[Err, Done, Elem](
    hub: Hub[Exit[Either[Err, Done], Elem]]
  ): ZChannel[Any, Err, Elem, Done, Nothing, Nothing, Any] =
    toQueue(hub.toQueue)

  def toQueue[Err, Done, Elem](
    queue: Enqueue[Exit[Either[Err, Done], Elem]]
  ): ZChannel[Any, Err, Elem, Done, Nothing, Nothing, Any] =
    ZChannel.readWithCause(
      (in: Elem) => ZChannel.fromZIO(queue.offer(Exit.succeed(in))) *> toQueue(queue),
      (cause: Cause[Err]) => ZChannel.fromZIO(queue.offer(Exit.failCause(cause.map(Left(_))))),
      (done: Done) => ZChannel.fromZIO(queue.offer(Exit.fail(Right(done))))
    )

  private[zio] sealed trait MergeState[Env, Err, Err1, Err2, Elem, Done, Done1, Done2]
  private[zio] object MergeState {
    case class BothRunning[Env, Err, Err1, Err2, Elem, Done, Done1, Done2](
      left: Fiber[Either[Err, Done], Elem],
      right: Fiber[Either[Err1, Done1], Elem]
    ) extends MergeState[Env, Err, Err1, Err2, Elem, Done, Done1, Done2]
    case class LeftDone[Env, Err, Err1, Err2, Elem, Done, Done1, Done2](
      f: Exit[Err1, Done1] => ZIO[Env, Err2, Done2]
    ) extends MergeState[Env, Err, Err1, Err2, Elem, Done, Done1, Done2]
    case class RightDone[Env, Err, Err1, Err2, Elem, Done, Done1, Done2](
      f: Exit[Err, Done] => ZIO[Env, Err2, Done2]
    ) extends MergeState[Env, Err, Err1, Err2, Elem, Done, Done1, Done2]
  }

}

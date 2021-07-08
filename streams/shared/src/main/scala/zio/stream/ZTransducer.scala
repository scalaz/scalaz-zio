/*
 * Copyright 2020-2021 John A. De Goes and the ZIO Contributors
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

package zio.stream

import zio._

import java.nio.charset.{Charset, StandardCharsets}
import scala.collection.mutable

// Contract notes for transducers:
// - When a None is received, the transducer must flush all of its internal state
//   and remain empty until subsequent Some(Chunk) values.
//
//   Stated differently, after a first push(None), all subsequent push(None) must
//   result in Chunk.empty.
abstract class ZTransducer[-R, +E, -I, +O](val push: ZManaged[R, Nothing, Option[Chunk[I]] => ZIO[R, E, Chunk[O]]]) {
  self =>

  /**
   * Compose this transducer with another transducer, resulting in a composite transducer.
   */
  def >>>[R1 <: R, E1 >: E, O2 >: O, O3](that: ZTransducer[R1, E1, O2, O3]): ZTransducer[R1, E1, I, O3] =
    ZTransducer {
      self.push.zipWith(that.push) { (pushLeft, pushRight) =>
        {
          case None =>
            pushLeft(None).flatMap(cl =>
              if (cl.isEmpty) pushRight(None)
              else pushRight(Some(cl)).zipWith(pushRight(None))(_ ++ _)
            )
          case inputs @ Some(_) =>
            pushLeft(inputs).flatMap(cl => pushRight(Some(cl)))
        }
      }
    }

  /**
   * Compose this transducer with a sink, resulting in a sink that processes elements by piping
   * them through this transducer and piping the results into the sink.
   */
  def >>>[R1 <: R, E1 >: E, O2 >: O, I1 <: I, L, Z](that: ZSink[R1, E1, O2, L, Z]): ZSink[R1, E1, I1, L, Z] =
    ZSink[R1, E1, I1, L, Z] {
      self.push.zipWith(that.push) { (pushSelf, pushThat) =>
        {
          case None =>
            pushSelf(None)
              .mapError(e => (Left(e), Chunk.empty))
              .flatMap(chunk => pushThat(Some(chunk)) *> pushThat(None))
          case inputs @ Some(_) =>
            pushSelf(inputs)
              .mapError(e => (Left(e), Chunk.empty))
              .flatMap(chunk => pushThat(Some(chunk)))
        }
      }
    }

  /**
   * Transforms the inputs of this transducer.
   */
  final def contramap[J](f: J => I): ZTransducer[R, E, J, O] =
    ZTransducer(self.push.map(push => is => push(is.map(_.map(f)))))

  /**
   * Effectually transforms the inputs of this transducer
   */
  final def contramapM[R1 <: R, E1 >: E, J](f: J => ZIO[R1, E1, I]): ZTransducer[R1, E1, J, O] =
    ZTransducer[R1, E1, J, O](self.push.map(push => is => ZIO.foreach(is)(_.mapM(f)).flatMap(push)))

  /**
   * Filters the outputs of this transducer.
   */
  final def filter(p: O => Boolean): ZTransducer[R, E, I, O] =
    ZTransducer(self.push.map(push => i => push(i).map(_.filter(p))))

  /**
   * Filters the inputs of this transducer.
   */
  final def filterInput[I1 <: I](p: I1 => Boolean): ZTransducer[R, E, I1, O] =
    ZTransducer(self.push.map(push => is => push(is.map(_.filter(p)))))

  /**
   * Effectually filters the inputs of this transducer.
   */
  final def filterInputM[R1 <: R, E1 >: E, I1 <: I](p: I1 => ZIO[R1, E1, Boolean]): ZTransducer[R1, E1, I1, O] =
    ZTransducer[R1, E1, I1, O](self.push.map(push => is => ZIO.foreach(is)(_.filterM(p)).flatMap(push)))

  /**
   * Transforms the outputs of this transducer.
   */
  final def map[P](f: O => P): ZTransducer[R, E, I, P] =
    ZTransducer(self.push.map(push => i => push(i).map(_.map(f))))

  /**
   * Transforms the chunks emitted by this transducer.
   */
  final def mapChunks[O2](f: Chunk[O] => Chunk[O2]): ZTransducer[R, E, I, O2] =
    ZTransducer {
      self.push.map(push => (input: Option[Chunk[I]]) => push(input).map(f))
    }

  /**
   * Effectfully transforms the chunks emitted by this transducer.
   */
  final def mapChunksM[R1 <: R, E1 >: E, O2](
    f: Chunk[O] => ZIO[R1, E1, Chunk[O2]]
  ): ZTransducer[R1, E1, I, O2] =
    ZTransducer {
      self.push.map(push => (input: Option[Chunk[I]]) => push(input).flatMap(f))
    }

  /**
   * Transforms the outputs of this transducer.
   */
  final def mapError[E1](f: E => E1): ZTransducer[R, E1, I, O] =
    ZTransducer(self.push.map(push => i => push(i).mapError(f)))

  /**
   * Effectually transforms the outputs of this transducer
   */
  final def mapM[R1 <: R, E1 >: E, P](f: O => ZIO[R1, E1, P]): ZTransducer[R1, E1, I, P] =
    ZTransducer[R1, E1, I, P](self.push.map(push => i => push(i).flatMap(_.mapM(f))))
}

object ZTransducer extends ZTransducerPlatformSpecificConstructors {
  def apply[R, E, I, O](
    push: ZManaged[R, Nothing, Option[Chunk[I]] => ZIO[R, E, Chunk[O]]]
  ): ZTransducer[R, E, I, O] =
    new ZTransducer(push) {}

  /**
   * Shorthand form for [[ZTransducer.identity]]. Use as:
   * {{{
   * ZTransducer[Int].filter(_ % 2 != 0)
   * }}}
   */
  def apply[I]: ZTransducer[Any, Nothing, I, I] = identity[I]

  /**
   * Reads the first n values from the stream and uses them to choose the transducer that will be used for the remainder of the stream.
   * If the stream ends before it has collected n values the partial chunk will be provided to f.
   */
  def branchAfter[R, E, I, O](n: Int)(f: Chunk[I] => ZTransducer[R, E, I, O]): ZTransducer[R, E, I, O] =
    ZTransducer {
      sealed trait State
      object State {
        final case class Collecting(data: Chunk[I]) extends State
        final case class Emitting(finalizer: ZManaged.Finalizer, push: Option[Chunk[I]] => ZIO[R, E, Chunk[O]])
            extends State
        val initial: State = Collecting(Chunk.empty)
      }

      val toCollect = Math.max(0, n)

      ZManaged.scope.flatMap { scope =>
        ZRefM.makeManaged(State.initial).map { stateRef =>
          {
            case None =>
              stateRef.getAndSet(State.initial).flatMap {
                case State.Emitting(finalizer, push) =>
                  push(None) <* finalizer(Exit.unit)
                case State.Collecting(data) =>
                  f(data).push.use(_(None))
              }
            case Some(data) =>
              stateRef.modify {
                case s @ State.Emitting(_, push) =>
                  push(Some(data)).map((_, s))
                case s @ State.Collecting(collected) =>
                  if (data.isEmpty) ZIO.succeedNow((Chunk.empty, s))
                  else {
                    val remaining = toCollect - collected.length
                    if (remaining <= data.length) {
                      val (newCollected, remainder) = data.splitAt(remaining)
                      scope(f(collected ++ newCollected).push).flatMap { case (finalizer, push) =>
                        push(Some(remainder)).map((_, State.Emitting(finalizer, push)))
                      }
                    } else {
                      ZIO.succeedNow((Chunk.empty, State.Collecting(collected ++ data)))
                    }
                  }
              }
          }
        }
      }
    }

  /**
   * Creates a transducer accumulating incoming values into chunks of maximum size `n`.
   */
  def collectAllN[I](n: Int): ZTransducer[Any, Nothing, I, Chunk[I]] =
    ZTransducer {

      def go(in: Chunk[I], leftover: Chunk[I], outBuilder: ChunkBuilder[Chunk[I]]): (Chunk[Chunk[I]], Chunk[I]) = {
        val (left, nextIn) = in.splitAt(n - leftover.size)

        if (leftover.size + left.size < n) outBuilder.result() -> (leftover ++ left)
        else {
          val nextOutBuilder =
            if (leftover.nonEmpty) outBuilder += (leftover ++ left)
            else outBuilder += left
          go(nextIn, Chunk.empty, nextOutBuilder)
        }
      }

      ZRef.makeManaged[Chunk[I]](Chunk.empty).map { stateRef =>
        {
          case None =>
            stateRef
              .getAndSet(Chunk.empty)
              .map { leftover =>
                if (leftover.nonEmpty) Chunk(leftover)
                else Chunk.empty
              }

          case Some(in) =>
            stateRef.modify { leftover =>
              val (out, nextLeftover) = go(in, leftover, ChunkBuilder.make())
              out -> nextLeftover
            }
        }
      }
    }

  /**
   * Creates a transducer accumulating incoming values into maps of up to `n` keys. Elements
   * are mapped to keys using the function `key`; elements mapped to the same key will
   * be merged with the function `f`.
   */
  def collectAllToMapN[K, I](n: Long)(key: I => K)(f: (I, I) => I): ZTransducer[Any, Nothing, I, Map[K, I]] =
    foldWeighted(Map[K, I]())((acc, i: I) => if (acc contains key(i)) 0 else 1, n) { (acc, i) =>
      val k = key(i)

      if (acc contains k) acc.updated(k, f(acc(k), i))
      else acc.updated(k, i)
    }.filter(_.nonEmpty)

  /**
   * Creates a transducer accumulating incoming values into sets of maximum size `n`.
   */
  def collectAllToSetN[I](n: Long): ZTransducer[Any, Nothing, I, Set[I]] =
    foldWeighted(Set[I]())((acc, i: I) => if (acc(i)) 0 else 1, n)(_ + _).filter(_.nonEmpty)

  /**
   * Accumulates incoming elements into a chunk as long as they verify predicate `p`.
   */
  def collectAllWhile[I](p: I => Boolean): ZTransducer[Any, Nothing, I, List[I]] =
    fold[I, (List[I], Boolean)]((Nil, true))(_._2) { case ((as, _), a) =>
      if (p(a)) (a :: as, true) else (as, false)
    }.map(_._1.reverse).filter(_.nonEmpty)

  /**
   * Accumulates incoming elements into a chunk as long as they verify effectful predicate `p`.
   */
  def collectAllWhileM[R, E, I](p: I => ZIO[R, E, Boolean]): ZTransducer[R, E, I, List[I]] =
    foldM[R, E, I, (List[I], Boolean)]((Nil, true))(_._2) { case ((as, _), a) =>
      p(a).map(if (_) (a :: as, true) else (as, false))
    }.map(_._1.reverse).filter(_.nonEmpty)

  /**
   * Creates a transducer that always dies with the specified exception.
   */
  def die(e: => Throwable): ZTransducer[Any, Nothing, Any, Nothing] =
    ZTransducer(Managed.succeed((_: Any) => IO.die(e)))

  /**
   * Creates a transducer that starts consuming values as soon as one fails
   * the predicate `p`.
   */
  def dropWhile[I](p: I => Boolean): ZTransducer[Any, Nothing, I, I] =
    ZTransducer {
      for {
        dropping <- ZRef.makeManaged(true)
        push = { (is: Option[Chunk[I]]) =>
          is match {
            case None => UIO(Chunk.empty)
            case Some(is) =>
              dropping.modify {
                case false => is -> false
                case true =>
                  val is1 = is.dropWhile(p)
                  is1 -> (is1.length == 0)
              }
          }
        }
      } yield push
    }

  /**
   * Creates a transducer that starts consuming values as soon as one fails
   * the effectful predicate `p`.
   */
  def dropWhileM[R, E, I](p: I => ZIO[R, E, Boolean]): ZTransducer[R, E, I, I] =
    ZTransducer {
      for {
        dropping <- ZRef.makeManaged(true)
        push = { (is: Option[Chunk[I]]) =>
          is match {
            case None => UIO(Chunk.empty)
            case Some(is) =>
              dropping.get.flatMap {
                case false => UIO(is -> false)
                case true  => is.dropWhileM(p).map(is1 => is1 -> is1.isEmpty)
              }.flatMap { case (is, pt) => dropping.set(pt) as is }
          }
        }
      } yield push
    }

  /**
   * Creates a transducer that always fails with the specified failure.
   */
  def fail[E](e: => E): ZTransducer[Any, E, Any, Nothing] =
    ZTransducer(ZManaged.succeed((_: Option[Any]) => ZIO.fail(e)))

  /**
   * Creates a transducer by folding over a structure of type `O` for as long as
   * `contFn` results in `true`. The transducer will emit a value when `contFn`
   * evaluates to `false` and then restart the folding.
   */
  def fold[I, O](z: O)(contFn: O => Boolean)(f: (O, I) => O): ZTransducer[Any, Nothing, I, O] =
    ZTransducer {
      def go(in: Chunk[I], state: O, progress: Boolean): (Chunk[O], O, Boolean) =
        in.foldLeft[(Chunk[O], O, Boolean)]((Chunk.empty, state, progress)) { case ((os0, state, _), i) =>
          val o = f(state, i)
          if (contFn(o))
            (os0, o, true)
          else
            (os0 :+ o, z, false)
        }

      ZRef.makeManaged[Option[O]](Some(z)).map { state =>
        {
          case Some(in) =>
            state.modify { s =>
              val (o, s2, progress) = go(in, s.getOrElse(z), s.nonEmpty)
              if (progress)
                o -> Some(s2)
              else
                o -> None
            }
          case None => state.getAndSet(None).map(_.fold[Chunk[O]](Chunk.empty)(Chunk.single(_)))
        }
      }
    }

  /**
   * Creates a transducer by folding over a structure of type `O`. The transducer will
   * fold the inputs until the stream ends, resulting in a stream with one element.
   */
  def foldLeft[I, O](z: O)(f: (O, I) => O): ZTransducer[Any, Nothing, I, O] =
    fold(z)(_ => true)(f)

  /**
   * Creates a transducer by effectfully folding over a structure of type `O`. The transducer will
   * fold the inputs until the stream ends, resulting in a stream with one element.
   */
  def foldLeftM[R, E, I, O](z: O)(f: (O, I) => ZIO[R, E, O]): ZTransducer[R, E, I, O] =
    foldM(z)(_ => true)(f)

  /**
   * Creates a sink by effectfully folding over a structure of type `S`.
   */
  def foldM[R, E, I, O](z: O)(contFn: O => Boolean)(f: (O, I) => ZIO[R, E, O]): ZTransducer[R, E, I, O] =
    ZTransducer {
      val initial = Some(z)

      def go(in: Chunk[I], state: O, progress: Boolean): ZIO[R, E, (Chunk[O], O, Boolean)] =
        in.foldM[R, E, (Chunk[O], O, Boolean)]((Chunk.empty, state, progress)) { case ((os0, state, _), i) =>
          f(state, i).map { o =>
            if (contFn(o))
              (os0, o, true)
            else
              (os0 :+ o, z, false)
          }
        }

      ZRef.makeManaged[Option[O]](initial).map { state =>
        {
          case Some(in) =>
            state.get.flatMap(s => go(in, s.getOrElse(z), s.nonEmpty)).flatMap { case (os, s, progress) =>
              if (progress)
                state.set(Some(s)) *> Push.emit(os)
              else
                state.set(None) *> Push.emit(os)
            }
          case None =>
            state.getAndSet(None).map(_.fold[Chunk[O]](Chunk.empty)(Chunk.single(_)))
        }
      }
    }

  /**
   * Creates a transducer that folds elements of type `I` into a structure
   * of type `O` until `max` elements have been folded.
   *
   * Like [[foldWeighted]], but with a constant cost function of 1.
   */
  def foldUntil[I, O](z: O, max: Long)(f: (O, I) => O): ZTransducer[Any, Nothing, I, O] =
    fold[I, (O, Long)]((z, 0))(_._2 < max) { case ((o, count), i) =>
      (f(o, i), count + 1)
    }.map(_._1)

  /**
   * Creates a transducer that effectfully folds elements of type `I` into a structure
   * of type `O` until `max` elements have been folded.
   *
   * Like [[foldWeightedM]], but with a constant cost function of 1.
   */
  def foldUntilM[R, E, I, O](z: O, max: Long)(f: (O, I) => ZIO[R, E, O]): ZTransducer[R, E, I, O] =
    foldM[R, E, I, (O, Long)]((z, 0))(_._2 < max) { case ((o, count), i) =>
      f(o, i).map((_, count + 1))
    }.map(_._1)

  /**
   * Creates a transducer that folds elements of type `I` into a structure
   * of type `O`, until `max` worth of elements (determined by the `costFn`)
   * have been folded.
   *
   * @note Elements that have an individual cost larger than `max` will
   * force the transducer to cross the `max` cost. See [[foldWeightedDecompose]]
   * for a variant that can handle these cases.
   */
  def foldWeighted[I, O](z: O)(costFn: (O, I) => Long, max: Long)(f: (O, I) => O): ZTransducer[Any, Nothing, I, O] =
    foldWeightedDecompose[I, O](z)(costFn, max, Chunk.single(_))(f)

  /**
   * Creates a transducer that folds elements of type `I` into a structure
   * of type `O`, until `max` worth of elements (determined by the `costFn`)
   * have been folded.
   *
   * The `decompose` function will be used for decomposing elements that
   * cause an `O` aggregate to cross `max` into smaller elements. For
   * example:
   * {{{
   * Stream(1, 5, 1)
   *  .aggregate(
   *    ZTransducer
   *      .foldWeightedDecompose(List[Int]())((_, i: Int) => i.toLong, 4,
   *        (i: Int) => if (i > 1) Chunk(i - 1, 1) else Chunk(i)) { (acc, el) =>
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
   * to return a `ZIO` value, and consequently it allows the transducer
   * to fail.
   */
  def foldWeightedDecompose[I, O](
    z: O
  )(costFn: (O, I) => Long, max: Long, decompose: I => Chunk[I])(f: (O, I) => O): ZTransducer[Any, Nothing, I, O] =
    ZTransducer {
      case class FoldWeightedState(result: O, cost: Long)

      val initial = FoldWeightedState(z, 0)

      def go(
        in: Chunk[I],
        os0: Chunk[O],
        state: FoldWeightedState,
        dirty: Boolean
      ): (Chunk[O], FoldWeightedState, Boolean) =
        in.foldLeft[(Chunk[O], FoldWeightedState, Boolean)]((os0, state, dirty)) { case ((os0, state, _), i) =>
          val total = state.cost + costFn(state.result, i)

          if (total > max) {
            val is = decompose(i)

            if (is.length <= 1 && !dirty)
              // If `i` cannot be decomposed, we need to cross the `max` threshold. To
              // minimize "injury", we only allow this when we haven't added anything else
              // to the aggregate (dirty = false).
              (os0 :+ f(state.result, if (is.nonEmpty) is(0) else i), initial, false)
            else if (is.length <= 1 && dirty) {
              // If the state is dirty and `i` cannot be decomposed, we close the current
              // aggregate and a create new one from `is`. We're not adding `f(initial, i)` to
              // the results immediately because it could be that `i` by itself does not
              // cross the threshold, so we can attempt to aggregate it with subsequent elements.
              val elem = if (is.nonEmpty) is(0) else i
              (os0 :+ state.result, FoldWeightedState(f(initial.result, elem), costFn(initial.result, elem)), true)
            } else
              // `i` got decomposed, so we will recurse and see whether the decomposition
              // can be aggregated without crossing `max`.
              go(is, os0, state, dirty)
          } else (os0, FoldWeightedState(f(state.result, i), total), true)
        }

      ZRef.makeManaged[Option[FoldWeightedState]](Some(initial)).map { state =>
        {
          case Some(in) =>
            state.modify { s =>
              val (o, s2, dirty) = go(in, Chunk.empty, s.getOrElse(initial), s.nonEmpty)
              if (dirty)
                o -> Some(s2)
              else
                o -> None
            }
          case None => state.getAndSet(None).map(_.fold[Chunk[O]](Chunk.empty)(s => Chunk.single(s.result)))
        }
      }
    }

  /**
   * Creates a transducer that effectfully folds elements of type `I` into a structure
   * of type `S`, until `max` worth of elements (determined by the `costFn`) have
   * been folded.
   *
   * @note Elements that have an individual cost larger than `max` will
   * force the transducer to cross the `max` cost. See [[foldWeightedDecomposeM]]
   * for a variant that can handle these cases.
   */
  def foldWeightedM[R, E, I, O](
    z: O
  )(costFn: (O, I) => ZIO[R, E, Long], max: Long)(f: (O, I) => ZIO[R, E, O]): ZTransducer[R, E, I, O] =
    foldWeightedDecomposeM(z)(costFn, max, (i: I) => UIO.succeedNow(Chunk.single(i)))(f)

  /**
   * Creates a transducer that effectfully folds elements of type `I` into a structure
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
  def foldWeightedDecomposeM[R, E, I, O](z: O)(
    costFn: (O, I) => ZIO[R, E, Long],
    max: Long,
    decompose: I => ZIO[R, E, Chunk[I]]
  )(f: (O, I) => ZIO[R, E, O]): ZTransducer[R, E, I, O] =
    ZTransducer {
      final case class FoldWeightedState(result: O, cost: Long)

      val initial = FoldWeightedState(z, 0)

      def go(
        in: Chunk[I],
        os: Chunk[O],
        state: FoldWeightedState,
        dirty: Boolean
      ): ZIO[R, E, (Chunk[O], FoldWeightedState, Boolean)] =
        in.foldM[R, E, (Chunk[O], FoldWeightedState, Boolean)]((os, state, dirty)) { case ((os, state, _), i) =>
          costFn(state.result, i).flatMap { cost =>
            val total = cost + state.cost
            if (total > max)
              decompose(i).flatMap(is =>
                // See comments on `foldWeightedDecompose` for details on every case here.
                if (is.length <= 1 && !dirty)
                  f(state.result, if (is.nonEmpty) is(0) else i).map(o => ((os :+ o), initial, false))
                else if (is.length <= 1 && dirty) {
                  val elem = if (is.nonEmpty) is(0) else i

                  f(initial.result, elem).zipWith(costFn(initial.result, elem)) { (s, cost) =>
                    (os :+ state.result, FoldWeightedState(s, cost), true)
                  }
                } else go(is, os, state, dirty)
              )
            else
              f(state.result, i).map(o => (os, FoldWeightedState(o, total), true))
          }
        }

      ZRef.makeManaged[Option[FoldWeightedState]](Some(initial)).map { state =>
        {
          case Some(in) =>
            state.get.flatMap(s => go(in, Chunk.empty, s.getOrElse(initial), s.nonEmpty)).flatMap {
              case (os, s, dirty) =>
                if (dirty)
                  state.set(Some(s)) *> Push.emit(os)
                else
                  state.set(None) *> Push.emit(os)
            }
          case None =>
            state.getAndSet(None).map(_.fold[Chunk[O]](Chunk.empty)(s => Chunk.single(s.result)))
        }
      }
    }

  /**
   * Creates a transducer that always evaluates the specified effect.
   */
  def fromEffect[R, E, A](zio: ZIO[R, E, A]): ZTransducer[R, E, Any, A] =
    ZTransducer(Managed.succeed((_: Any) => zio.map(Chunk.single(_))))

  /**
   * Creates a transducer that purely transforms incoming values.
   */
  def fromFunction[I, O](f: I => O): ZTransducer[Any, Nothing, I, O] =
    identity.map(f)

  /**
   * Creates a transducer that effectfully transforms incoming values.
   */
  def fromFunctionM[R, E, I, O](f: I => ZIO[R, E, O]): ZTransducer[R, E, I, O] =
    identity.mapM(f(_))

  /**
   * Creates a transducer from a chunk processing function.
   */
  def fromPush[R, E, I, O](push: Option[Chunk[I]] => ZIO[R, E, Chunk[O]]): ZTransducer[R, E, I, O] =
    ZTransducer(Managed.succeed(push))

  /**
   * Creates a transducer that groups on adjacent keys, calculated by function f.<br>
   * With this transducer we can mimic fs2 groupAdjacentBy.<br>
   * This can be used like e.g. zstream.aggregate(groupAdjacentBy(_._1))
   */
  def groupAdjacentBy[I, K](f: I => K): ZTransducer[Any, Nothing, I, (K, NonEmptyChunk[I])] =
    ZTransducer {
      def go(
        in: Chunk[I],
        state: Option[(K, NonEmptyChunk[I])]
      ): (Chunk[(K, NonEmptyChunk[I])], Option[(K, NonEmptyChunk[I])]) =
        in.foldLeft[(Chunk[(K, NonEmptyChunk[I])], Option[(K, NonEmptyChunk[I])])]((Chunk.empty, state)) {
          case ((os0, state), i) =>
            state match {
              case None => (os0, Some((f(i), NonEmptyChunk(i))))
              case Some((key, aggregated)) =>
                val newKey = f(i)
                if (key == newKey) (os0, Some((key, aggregated :+ i)))
                else (os0.appended((key, aggregated)), Some((newKey, NonEmptyChunk(i))))
            }
        }

      ZRef.makeManaged[Option[(K, NonEmptyChunk[I])]](None).map { state =>
        {
          case Some(in) => state.modify(go(in, _))
          case None     => state.getAndSet(None).map(_.fold[Chunk[(K, NonEmptyChunk[I])]](Chunk.empty)(Chunk.single))
        }
      }
    }

  /**
   * Creates a transducer that returns the first element of the stream, if it exists.
   */
  def head[O]: ZTransducer[Any, Nothing, O, Option[O]] =
    foldLeft[O, Option[O]](Option.empty[O]) { case (acc, a) =>
      acc match {
        case Some(_) => acc
        case None    => Some(a)
      }
    }

  /**
   * The identity transducer. Passes elements through.
   */
  def identity[I]: ZTransducer[Any, Nothing, I, I] =
    ZTransducer.fromPush {
      case Some(is) => ZIO.succeedNow(is)
      case None     => ZIO.succeedNow(Chunk.empty)
    }

  /**
   * Decodes chunks of ISO/IEC 8859-1 bytes into strings.
   *
   * This transducer uses the String constructor's behavior when handling malformed byte
   * sequences.
   */
  val iso_8859_1Decode: ZTransducer[Any, Nothing, Byte, String] =
    ZTransducer.fromPush {
      case Some(is) => ZIO.succeedNow(Chunk.single(new String(is.toArray, StandardCharsets.ISO_8859_1)))
      case None     => ZIO.succeedNow(Chunk.empty)
    }

  /**
   * Creates a transducer that returns the last element of the stream, if it exists.
   */
  def last[O]: ZTransducer[Any, Nothing, O, Option[O]] =
    foldLeft[O, Option[O]](Option.empty[O])((_, a) => Some(a))

  /**
   * Emits the provided chunk before emitting any other value.
   */
  def prepend[A](values: Chunk[A]): ZTransducer[Any, Nothing, A, A] =
    ZTransducer {
      ZRef.makeManaged(values).map { stateRef =>
        {
          case None =>
            stateRef.getAndSet(Chunk.empty)
          case Some(xs) =>
            stateRef.getAndSet(Chunk.empty).map(c => if (c.isEmpty) xs else c ++ xs)
        }
      }
    }

  /**
   * Splits strings on newlines. Handles both Windows newlines (`\r\n`) and UNIX newlines (`\n`).
   */
  val splitLines: ZTransducer[Any, Nothing, String, String] =
    ZTransducer {
      ZRef.makeManaged[(Option[String], Boolean)]((None, false)).map { stateRef =>
        {
          case None =>
            stateRef.getAndSet((None, false)).flatMap {
              case (None, _)      => ZIO.succeedNow(Chunk.empty)
              case (Some(str), _) => ZIO.succeedNow(Chunk(str))
            }

          case Some(strings) =>
            stateRef.modify { case (leftover, wasSplitCRLF) =>
              val buf    = mutable.ArrayBuffer[String]()
              var inCRLF = wasSplitCRLF
              var carry  = leftover getOrElse ""

              strings.foreach { string =>
                val concat = carry + string

                if (concat.length() > 0) {
                  var i =
                    // If we had a split CRLF, we start reading
                    // from the last character of the leftover (which was the '\r')
                    if (inCRLF && carry.length > 0) carry.length - 1
                    // Otherwise we just skip over the entire previous leftover as
                    // it doesn't contain a newline.
                    else carry.length
                  var sliceStart = 0

                  while (i < concat.length()) {
                    if (concat(i) == '\n') {
                      buf += concat.substring(sliceStart, i)
                      i += 1
                      sliceStart = i
                    } else if (concat(i) == '\r' && (i + 1) < concat.length && concat(i + 1) == '\n') {
                      buf += concat.substring(sliceStart, i)
                      i += 2
                      sliceStart = i
                    } else if (concat(i) == '\r' && i == concat.length - 1) {
                      inCRLF = true
                      i += 1
                    } else {
                      i += 1
                    }
                  }

                  carry = concat.substring(sliceStart, concat.length)
                }
              }

              (Chunk.fromArray(buf.toArray), (if (carry.length() > 0) Some(carry) else None, inCRLF))
            }
        }
      }
    }

  /**
   * Splits strings on a delimiter.
   */
  def splitOn(delimiter: String): ZTransducer[Any, Nothing, String, String] = {
    val chars = ZTransducer.fromFunction[String, Chunk[Char]](s => Chunk.fromArray(s.toArray)).mapChunks(_.flatten)
    val split = splitOnChunk(Chunk.fromArray(delimiter.toArray)).map(_.mkString(""))
    chars >>> split
  }

  /**
   * Splits elements on a delimiter and transforms the splits into desired output.
   */
  def splitOnChunk[A](delimiter: Chunk[A]): ZTransducer[Any, Nothing, A, Chunk[A]] =
    ZTransducer {
      ZRef.makeManaged[(Option[Chunk[A]], Int)](None -> 0).map { state =>
        {
          case None =>
            state.modify {
              case s @ (None, _)    => Chunk.empty         -> s
              case (Some(chunk), _) => Chunk.single(chunk) -> (None -> 0)
            }
          case Some(inputChunk: Chunk[A]) =>
            state.modify { s0 =>
              var out: mutable.ArrayBuffer[Chunk[A]] = null
              var chunkIndex                         = 0
              var buffer: Chunk[A]                   = s0._1.getOrElse(Chunk.empty)
              var delimIndex                         = s0._2
              while (chunkIndex < inputChunk.length) {
                val in    = buffer :+ inputChunk(chunkIndex)
                var index = buffer.length
                var start = 0
                buffer = Chunk.empty
                while (index < in.length) {
                  while (delimIndex < delimiter.length && index < in.length && in(index) == delimiter(delimIndex)) {
                    delimIndex += 1
                    index += 1
                  }
                  if (delimIndex == delimiter.length || in.isEmpty) {
                    if (out eq null) out = mutable.ArrayBuffer[Chunk[A]]()
                    val slice = in.slice(start, index - delimiter.length)
                    out += slice
                    delimIndex = 0
                    start = index
                  }
                  if (index < in.length) {
                    delimIndex = 0
                    while (index < in.length && in(index) != delimiter(0)) index += 1
                  }
                }

                if (start < in.length) {
                  buffer = in.drop(start)
                }

                chunkIndex += 1
              }

              val chunk = if (out eq null) Chunk.empty else Chunk.fromArray(out.toArray)
              val buf   = if (buffer.isEmpty) None else Some(buffer)
              chunk -> (buf -> delimIndex)
            }
        }
      }
    }

  /**
   * Decodes chunks of Unicode bytes into strings.
   *
   * Detects byte order marks for UTF-8, UTF-16BE, UTF-16LE, UTF-32BE, UTF-32LE or defaults
   * to UTF-8 if no BOM is detected.
   */
  val utfDecode: ZTransducer[Any, Nothing, Byte, String] =
    branchAfter(4) { bytes =>
      bytes.toList match {
        case 0 :: 0 :: -2 :: -1 :: Nil if Charset.isSupported("UTF-32BE") => utf32BEDecode
        case -2 :: -1 :: 0 :: 0 :: Nil if Charset.isSupported("UTF-32LE") => utf32LEDecode
        case -17 :: -69 :: -65 :: x1 :: Nil                               => prepend(Chunk(x1)) >>> utf8Decode
        case -2 :: -1 :: x1 :: x2 :: Nil                                  => prepend(Chunk(x1, x2)) >>> utf16BEDecode
        case -1 :: -2 :: x1 :: x2 :: Nil                                  => prepend(Chunk(x1, x2)) >>> utf16LEDecode
        case _                                                            => prepend(bytes) >>> utf8Decode
      }
    }

  /**
   * Decodes chunks of UTF-8 bytes into strings.
   *
   * This transducer uses the String constructor's behavior when handling malformed byte
   * sequences.
   */
  val utf8Decode: ZTransducer[Any, Nothing, Byte, String] = {
    val transducer = ZTransducer[Any, Nothing, Byte, String] {
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

      ZRef.makeManaged[Chunk[Byte]](Chunk.empty).map { stateRef =>
        {
          case None =>
            stateRef.getAndSet(Chunk.empty).flatMap { leftovers =>
              if (leftovers.isEmpty) ZIO.succeedNow(Chunk.empty)
              else ZIO.succeedNow(Chunk.single(new String(leftovers.toArray[Byte], StandardCharsets.UTF_8)))
            }

          case Some(bytes) =>
            stateRef.modify { leftovers =>
              val concat = leftovers ++ bytes

              val (toConvert, newLeftovers) = concat.splitAt(computeSplit(concat))

              if (toConvert.isEmpty) (Chunk.empty, newLeftovers.materialize)
              else (Chunk.single(new String(toConvert.toArray[Byte], "UTF-8")), newLeftovers.materialize)
            }
        }
      }
    }

    // handle optional byte order mark
    branchAfter(3) { bytes =>
      bytes.toList match {
        case -17 :: -69 :: -65 :: Nil =>
          transducer
        case _ =>
          prepend(bytes) >>> transducer
      }
    }
  }

  /**
   * Decodes chunks of UTF-16 bytes into strings.
   * If no byte order mark is found big-endianness is assumed.
   *
   * This transducer uses the endisn-specific String constructor's behavior when handling
   * malformed byte sequences.
   */
  val utf16Decode: ZTransducer[Any, Nothing, Byte, String] =
    branchAfter(2) { bytes =>
      bytes.toList match {
        case -2 :: -1 :: Nil =>
          utf16BEDecode
        case -1 :: -2 :: Nil =>
          utf16LEDecode
        case _ =>
          prepend(bytes) >>> utf16BEDecode
      }
    }

  /**
   * Decodes chunks of UTF-16BE bytes into strings.
   *
   * This transducer uses the String constructor's behavior when handling malformed byte
   * sequences.
   */
  val utf16BEDecode: ZTransducer[Any, Nothing, Byte, String] =
    utfFixedLengthDecode(StandardCharsets.UTF_16BE, 2)

  /**
   * Decodes chunks of UTF-16LE bytes into strings.
   *
   * This transducer uses the String constructor's behavior when handling malformed byte
   * sequences.
   */
  val utf16LEDecode: ZTransducer[Any, Nothing, Byte, String] =
    utfFixedLengthDecode(StandardCharsets.UTF_16LE, 2)

  /**
   * Decodes chunks of UTF-32 bytes into strings.
   * If no byte order mark is found big-endianness is assumed.
   */
  lazy val utf32Decode: ZTransducer[Any, Nothing, Byte, String] =
    branchAfter(4) { bytes =>
      bytes.toList match {
        case 0 :: 0 :: -2 :: -1 :: Nil =>
          utf32BEDecode
        case -1 :: -2 :: 0 :: 0 :: Nil =>
          utf32LEDecode
        case _ =>
          prepend(bytes) >>> utf32BEDecode
      }
    }

  /**
   * Decodes chunks of UTF-32BE bytes into strings.
   *
   * This transducer uses the String constructor's behavior when handling malformed byte
   * sequences.
   */
  lazy val utf32BEDecode: ZTransducer[Any, Nothing, Byte, String] =
    utfFixedLengthDecode(Charset.forName("UTF-32BE"), 4)

  /**
   * Decodes chunks of UTF-32LE bytes into strings.
   *
   * This transducer uses the String constructor's behavior when handling malformed byte
   * sequences.
   */
  lazy val utf32LEDecode: ZTransducer[Any, Nothing, Byte, String] =
    utfFixedLengthDecode(Charset.forName("UTF-32LE"), 4)

  private def utfFixedLengthDecode(charset: Charset, width: Int): ZTransducer[Any, Nothing, Byte, String] =
    ZTransducer {
      ZRef.makeManaged[Chunk[Byte]](Chunk.empty).map { stateRef =>
        {
          case None =>
            stateRef.getAndSet(Chunk.empty).flatMap { leftovers =>
              if (leftovers.isEmpty) ZIO.succeedNow(Chunk.empty)
              else ZIO.succeedNow(Chunk.single(new String(leftovers.toArray[Byte], charset)))
            }
          case Some(bytes) =>
            stateRef.modify { old =>
              val data      = old ++ bytes
              val remainder = data.length % width
              if (remainder == 0) {
                val decoded = new String(data.toArray, charset)
                (Chunk.single(decoded), Chunk.empty)
              } else {
                val (fullChunk, rest) = data.splitAt(data.length - remainder)
                val decoded           = new String(fullChunk.toArray, charset)
                (Chunk.single(decoded), rest)
              }
            }
        }
      }
    }

  /**
   * Decodes chunks of US-ASCII bytes into strings.
   *
   * This transducer uses the String constructor's behavior when handling malformed byte
   * sequences.
   */
  val usASCIIDecode: ZTransducer[Any, Nothing, Byte, String] =
    ZTransducer.fromPush {
      case Some(chunk) => ZIO.succeedNow(Chunk.single(new String(chunk.toArray, StandardCharsets.US_ASCII)))
      case None        => ZIO.succeedNow(Chunk.empty)
    }

  object Push {
    def emit[A](a: A): UIO[Chunk[A]]         = IO.succeedNow(Chunk.single(a))
    def emit[A](as: Chunk[A]): UIO[Chunk[A]] = IO.succeedNow(as)
    val next: UIO[Chunk[Nothing]]            = IO.succeedNow(Chunk.empty)
  }
}

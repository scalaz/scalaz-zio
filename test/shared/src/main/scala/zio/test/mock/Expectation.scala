/*
 * Copyright 2017-2020 John A. De Goes and the ZIO Contributors
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

package zio.test.mock

import zio.test.Assertion
import zio.test.mock.Expectation.{ And, Chain, Or, Repeated }
import zio.test.mock.Result.{ Fail, Succeed }
import zio.test.mock.internal.{ ExpectationState, MockException, MockState, ProxyFactory }
import zio.{ Has, IO, Managed, Tag, ULayer, URLayer, ZLayer }

import scala.language.implicitConversions

/**
 * An `Expectation[R]` is an immutable tree structure that represents
 * expectations on environment `R`.
 */
sealed abstract class Expectation[R <: Has[_]: Tag] { self =>

  /**
   * Operator alias for `and`.
   */
  def &&[R0 <: Has[_]: Tag](that: Expectation[R0]): Expectation[R with R0] =
    and[R0](that)

  /**
   * Operator alias for `or`.
   */
  def ||[R0 <: Has[_]: Tag](that: Expectation[R0]): Expectation[R with R0] =
    or[R0](that)

  /**
   * Operator alias for `andThen`.
   */
  def ++[R0 <: Has[_]: Tag](that: Expectation[R0]): Expectation[R with R0] =
    andThen[R0](that)

  /**
   * Compose two expectations, producing a new expectation to satisfy both.
   *
   * {{
   * val mockEnv = MockClock.sleep(equalTo(1.second)) and MockConsole.getStrLn(value("foo"))
   * }}
   */
  def and[R0 <: Has[_]: Tag](that: Expectation[R0]): Expectation[R with R0] =
    (self, that) match {
      case (And.Items(xs1), And.Items(xs2)) =>
        And(self.mock.compose ++ that.mock.compose)(xs1 ++ xs2).asInstanceOf[Expectation[R with R0]]
      case (And.Items(xs), _) =>
        And(self.mock.compose ++ that.mock.compose)(xs :+ that).asInstanceOf[Expectation[R with R0]]
      case (_, And.Items(xs)) =>
        And(self.mock.compose ++ that.mock.compose)(self :: xs).asInstanceOf[Expectation[R with R0]]
      case _ => And(self.mock.compose ++ that.mock.compose)(self :: that :: Nil).asInstanceOf[Expectation[R with R0]]
    }

  /**
   * Compose two expectations, producing a new expectation to satisfy both sequentially.
   *
   * {{
   * val mockEnv = MockClock.sleep(equalTo(1.second)) andThen MockConsole.getStrLn(value("foo"))
   * }}
   */
  def andThen[R0 <: Has[_]: Tag](that: Expectation[R0]): Expectation[R with R0] =
    (self, that) match {
      case (Chain.Items(xs1), Chain.Items(xs2)) =>
        Chain(self.mock.compose ++ that.mock.compose)(xs1 ++ xs2).asInstanceOf[Expectation[R with R0]]
      case (Chain.Items(xs), _) =>
        Chain(self.mock.compose ++ that.mock.compose)(xs :+ that).asInstanceOf[Expectation[R with R0]]
      case (_, Chain.Items(xs)) =>
        Chain(self.mock.compose ++ that.mock.compose)(self :: xs).asInstanceOf[Expectation[R with R0]]
      case _ => Chain(self.mock.compose ++ that.mock.compose)(self :: that :: Nil).asInstanceOf[Expectation[R with R0]]
    }

  /**
   * Lower-bounded variant of `repeated`, produces a new expectation to satisfy
   * itself sequentially at least given number of times.
   */
  def atLeast(min: Int): Expectation[R] =
    Repeated(self, min to -1)

  /**
   * Upper-bounded variant of `repeated`, produces a new expectation to satisfy
   * itself sequentially at most given number of times.
   */
  def atMost(max: Int): Expectation[R] =
    Repeated(self, 0 to max)

  /**
   * Alias for `atMost(1)`, produces a new expectation to satisfy itself at most once.
   */
  def optional: Expectation[R] =
    Repeated(self, 0 to 1)

  /**
   * Compose two expectations, producing a new expectation to satisfy one of them.
   *
   * {{
   * val mockEnv = MockClock.sleep(equalTo(1.second)) or MockConsole.getStrLn(value("foo"))
   * }}
   */
  def or[R0 <: Has[_]: Tag](that: Expectation[R0]): Expectation[R with R0] =
    (self, that) match {
      case (Or.Items(xs1), Or.Items(xs2)) =>
        Or(self.mock.compose ++ that.mock.compose)(xs1 ++ xs2).asInstanceOf[Expectation[R with R0]]
      case (Or.Items(xs), _) =>
        Or(self.mock.compose ++ that.mock.compose)(xs :+ that).asInstanceOf[Expectation[R with R0]]
      case (_, Or.Items(xs)) =>
        Or(self.mock.compose ++ that.mock.compose)(self :: xs).asInstanceOf[Expectation[R with R0]]
      case _ => Or(self.mock.compose ++ that.mock.compose)(self :: that :: Nil).asInstanceOf[Expectation[R with R0]]
    }

  /**
   * Repeats this expectation withing given bounds, producing a new expectation to
   * satisfy itself sequentially given number of times.
   *
   * {{
   * val mockEnv = MockClock.sleep(equalTo(1.second)).repeats(1, 5)
   * }}
   *
   * NOTE: once another repetition starts executing, it must be completed in order to satisfy
   * the composite expectation. For example (A ++ B).repeats(1, 2) will be satisfied by either
   * A->B (one repetition) or A->B->A->B (two repetitions), but will fail on A->B->A
   * (incomplete second repetition).
   */
  def repeats(range: Range): Expectation[R] =
    Repeated(self, range)

  /**
   * Invocations log.
   */
  private[test] val invocations: List[Int]

  /**
   * Environment to which expectation belongs.
   */
  private[test] val mock: Mock[R]

  /**
   * Mock execution state.
   */
  private[test] val state: ExpectationState
}

object Expectation {

  import ExpectationState._

  /**
   * Models expectations conjunction on environment `R`. Expectations are checked in the order they are provided,
   * meaning that earlier expectations may shadow later ones.
   */
  private[test] case class And[R <: Has[_]: Tag](
    children: List[Expectation[R]],
    state: ExpectationState,
    invocations: List[Int],
    mock: Mock.Composed[R]
  ) extends Expectation[R]

  private[test] object And {

    def apply[R <: Has[_]: Tag](compose: URLayer[Has[Proxy], R])(children: List[Expectation[_]]): And[R] =
      And(children.asInstanceOf[List[Expectation[R]]], Unsatisfied, List.empty, Mock.Composed(compose))

    object Items {

      private[test] def unapply[R <: Has[_]](and: And[R]): Option[(List[Expectation[R]])] =
        Some(and.children)
    }
  }

  /**
   * Models a call in environment `R` that takes input arguments `I` and returns an effect
   * that may fail with an error `E` or produce a single `A`.
   */
  private[test] case class Call[R <: Has[_]: Tag, I, E, A](
    capability: Capability[R, I, E, A],
    assertion: Assertion[I],
    returns: I => IO[E, A],
    state: ExpectationState,
    invocations: List[Int]
  ) extends Expectation[R] {
    val mock: Mock[R] = capability.mock
  }

  private[test] object Call {

    def apply[R <: Has[_]: Tag, I, E, A](
      capability: Capability[R, I, E, A],
      assertion: Assertion[I],
      returns: I => IO[E, A]
    ): Call[R, I, E, A] =
      Call(capability, assertion, returns, Unsatisfied, List.empty)
  }

  /**
   * Models sequential expectations on environment `R`.
   */
  private[test] case class Chain[R <: Has[_]: Tag](
    children: List[Expectation[R]],
    state: ExpectationState,
    invocations: List[Int],
    mock: Mock.Composed[R]
  ) extends Expectation[R]

  private[test] object Chain {

    def apply[R <: Has[_]: Tag](compose: URLayer[Has[Proxy], R])(children: List[Expectation[_]]): Chain[R] =
      Chain(children.asInstanceOf[List[Expectation[R]]], Unsatisfied, List.empty, Mock.Composed(compose))

    object Items {

      private[test] def unapply[R <: Has[_]](chain: Chain[R]): Option[(List[Expectation[R]])] =
        Some(chain.children)
    }
  }

  private[test] case class NoCalls[R <: Has[_]: Tag](mock: Mock[R]) extends Expectation[R] {

    override private[test] val invocations: List[Int] = Nil

    override private[test] val state: ExpectationState = Satisfied

  }

  /**
   * Models expectations disjunction on environment `R`. Expectations are checked in the order they are provided,
   * meaning that earlier expectations may shadow later ones.
   */
  private[test] case class Or[R <: Has[_]: Tag](
    children: List[Expectation[R]],
    state: ExpectationState,
    invocations: List[Int],
    mock: Mock.Composed[R]
  ) extends Expectation[R]

  private[test] object Or {

    def apply[R <: Has[_]: Tag](compose: URLayer[Has[Proxy], R])(children: List[Expectation[_]]): Or[R] =
      Or(children.asInstanceOf[List[Expectation[R]]], Unsatisfied, List.empty, Mock.Composed(compose))

    object Items {

      private[test] def unapply[R <: Has[_]](or: Or[R]): Option[(List[Expectation[R]])] =
        Some(or.children)
    }
  }

  /**
   * Models expectation repetition on environment `R`.
   */
  private[test] final case class Repeated[R <: Has[_]: Tag](
    child: Expectation[R],
    range: Range,
    state: ExpectationState,
    invocations: List[Int],
    started: Int,
    completed: Int
  ) extends Expectation[R] {
    val mock: Mock[R] = child.mock
  }

  private[test] object Repeated {

    def apply[R <: Has[_]: Tag](child: Expectation[R], range: Range): Repeated[R] =
      if (range.step <= 0) throw MockException.InvalidRangeException(range)
      else Repeated(child, range, if (range.start == 0) Satisfied else Unsatisfied, List.empty, 0, 0)
  }

  /**
   * Expectation result failing with `E`.
   */
  def failure[E](failure: E): Fail[Any, E] = Fail(_ => IO.fail(failure))

  /**
   * Maps the input arguments `I` to expectation result failing with `E`.
   */
  def failureF[I, E](f: I => E): Fail[I, E] = Fail(i => IO.succeed(i).map(f).flip)

  /**
   * Effectfully maps the input arguments `I` to expectation result failing with `E`.
   */
  def failureM[I, E](f: I => IO[E, Nothing]): Fail[I, E] = Fail(f)

  /**
   * Expectation result computing forever.
   */
  def never: Succeed[Any, Nothing] = valueM(_ => IO.never)

  /**
   * Expectation result succeeding with `Unit`.
   */
  def unit: Succeed[Any, Unit] = value(())

  /**
   * Expectation result succeeding with `A`.
   */
  def value[A](value: A): Succeed[Any, A] = Succeed(_ => IO.succeed(value))

  /**
   * Maps the input arguments `I` to expectation result succeeding with `A`.
   */
  def valueF[I, A](f: I => A): Succeed[I, A] = Succeed(i => IO.succeed(i).map(f))

  /**
   * Effectfully maps the input arguments `I` expectation result succeeding with `A`.
   */
  def valueM[I, A](f: I => IO[Nothing, A]): Succeed[I, A] = Succeed(f)

  /**
   * Implicitly converts Expectation to ZLayer mock environment.
   */
  implicit def toLayer[R <: Has[_]: Tag](trunk: Expectation[R]): ULayer[R] =
    ZLayer.fromManagedMany(
      for {
        state <- Managed.make(MockState.make(trunk))(MockState.checkUnmetExpectations)
        env   <- (ProxyFactory.mockProxy(state) >>> trunk.mock.compose).build
      } yield env
    )
}

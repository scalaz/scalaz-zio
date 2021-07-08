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

package zio.test

import zio._
import zio.test.Spec._
import zio.test.environment.TestEnvironment

/**
 * A `Spec[R, E, T]` is the backbone of _ZIO Test_. Every spec is either a
 * suite, which contains other specs, or a test of type `T`. All specs require
 * an environment of type `R` and may potentially fail with an error of type
 * `E`.
 */
final case class Spec[-R, +E, +T](caseValue: SpecCase[R, E, T, Spec[R, E, T]]) { self =>

  /**
   * Syntax for adding aspects.
   * {{{
   * test("foo") { assert(42, equalTo(42)) } @@ ignore
   * }}}
   */
  final def @@[R0 <: R1, R1 <: R, E0, E1, E2 >: E0 <: E1](
    aspect: TestAspect[R0, R1, E0, E1]
  )(implicit ev1: E <:< TestFailure[E2], ev2: T <:< TestSuccess): ZSpec[R1, E2] =
    aspect(self.asInstanceOf[ZSpec[R1, E2]])

  /**
   * Annotates each test in this spec with the specified test annotation.
   */
  final def annotate[V](key: TestAnnotation[V], value: V): Spec[R, E, T] =
    transform[R, E, T] {
      case c @ SuiteCase(_, _, _) => c
      case TestCase(label, test, annotations) =>
        Spec.TestCase(label, test, annotations.annotate(key, value))
    }

  /**
   * Returns a new spec with the annotation map at each node.
   */
  final def annotated: Spec[R with Annotations, Annotated[E], Annotated[T]] =
    transform[R with Annotations, Annotated[E], Annotated[T]] {
      case Spec.SuiteCase(label, specs, exec) =>
        Spec.SuiteCase(label, specs.mapError((_, TestAnnotationMap.empty)), exec)
      case Spec.TestCase(label, test, annotations) =>
        Spec.TestCase(label, Annotations.withAnnotation(test), annotations)
    }

  /**
   * Returns a new spec with remapped errors and tests.
   */
  @deprecated("use mapBoth", "2.0.0")
  final def bimap[E1, T1](f: E => E1, g: T => T1)(implicit ev: CanFail[E]): Spec[R, E1, T1] =
    mapBoth(f, g)

  /**
   * Returns the number of tests in the spec that satisfy the specified
   * predicate.
   */
  final def countTests(f: T => Boolean): ZManaged[R, E, Int] =
    fold[ZManaged[R, E, Int]] {
      case SuiteCase(_, specs, _) => specs.flatMap(ZManaged.collectAll(_).map(_.sum))
      case TestCase(_, test, _)   => test.map(t => if (f(t)) 1 else 0).toManaged_
    }

  /**
   * Returns an effect that models execution of this spec.
   */
  final def execute(defExec: ExecutionStrategy): ZManaged[R, Nothing, Spec[Any, E, T]] =
    ZManaged.accessManaged(provide(_).foreachExec(defExec)(ZIO.halt(_), ZIO.succeedNow))

  /**
   * Determines if any node in the spec is satisfied by the given predicate.
   */
  final def exists[R1 <: R, E1 >: E](f: SpecCase[R, E, T, Any] => ZIO[R1, E1, Boolean]): ZManaged[R1, E1, Boolean] =
    fold[ZManaged[R1, E1, Boolean]] {
      case c @ SuiteCase(_, specs, _) =>
        specs.flatMap(ZManaged.collectAll(_).map(_.exists(identity))).zipWith(f(c).toManaged_)(_ || _)
      case c @ TestCase(_, _, _) => f(c).toManaged_
    }

  /**
   * Returns a new spec with only those tests with annotations satisfying the
   * specified predicate. If no annotations satisfy the specified predicate
   * then returns `Some` with an empty suite if this is a suite or `None`
   * otherwise.
   */
  final def filterAnnotations[V](key: TestAnnotation[V])(f: V => Boolean): Option[Spec[R, E, T]] =
    caseValue match {
      case SuiteCase(label, specs, exec) =>
        Some(Spec.suite(label, specs.map(_.flatMap(_.filterAnnotations(key)(f).toList)), exec))
      case TestCase(label, test, annotations) =>
        if (f(annotations.get(key))) Some(Spec.test(label, test, annotations)) else None
    }

  /**
   * Returns a new spec with only those suites and tests satisfying the
   * specified predicate. If a suite label satisfies the predicate the entire
   * suite will be included in the new spec. Otherwise only those specs in a
   * suite that satisfy the specified predicate will be included in the new
   * spec. If no labels satisfy the specified predicate then returns `Some`
   * with an empty suite if this is a suite or `None` otherwise.
   */
  final def filterLabels(f: String => Boolean): Option[Spec[R, E, T]] =
    caseValue match {
      case SuiteCase(label, specs, exec) =>
        if (f(label)) Some(Spec.suite(label, specs, exec))
        else Some(Spec.suite(label, specs.map(_.flatMap(_.filterLabels(f).toList)), exec))
      case TestCase(label, test, annotations) =>
        if (f(label)) Some(Spec.test(label, test, annotations)) else None
    }

  /**
   * Returns a new spec with only those suites and tests with tags satisfying
   * the specified predicate. If no tags satisfy the specified predicate then
   * returns `Some` with an empty suite with the root label if this is a suite
   * or `None` otherwise.
   */
  final def filterTags(f: String => Boolean): Option[Spec[R, E, T]] =
    filterAnnotations(TestAnnotation.tagged)(_.exists(f))

  /**
   * Folds over all nodes to produce a final result.
   */
  final def fold[Z](f: SpecCase[R, E, T, Z] => Z): Z =
    caseValue match {
      case SuiteCase(label, specs, exec) => f(SuiteCase(label, specs.map(_.map(_.fold(f)).toVector), exec))
      case t @ TestCase(_, _, _)         => f(t)
    }

  /**
   * Effectfully folds over all nodes according to the execution strategy of
   * suites, utilizing the specified default for other cases.
   */
  final def foldM[R1 <: R, E1, Z](
    defExec: ExecutionStrategy
  )(f: SpecCase[R, E, T, Z] => ZManaged[R1, E1, Z]): ZManaged[R1, E1, Z] =
    caseValue match {
      case SuiteCase(label, specs, exec) =>
        specs.foldCauseM(
          c => f(SuiteCase(label, ZManaged.halt(c), exec)),
          ZManaged
            .foreachExec(_)(exec.getOrElse(defExec))(_.foldM(defExec)(f).release)
            .flatMap(z => f(SuiteCase(label, ZManaged.succeedNow(z.toVector), exec)))
        )
      case t @ TestCase(_, _, _) => f(t)
    }

  /**
   * Determines if all node in the spec are satisfied by the given predicate.
   */
  final def forall[R1 <: R, E1 >: E](f: SpecCase[R, E, T, Any] => ZIO[R1, E1, Boolean]): ZManaged[R1, E1, Boolean] =
    fold[ZManaged[R1, E1, Boolean]] {
      case c @ SuiteCase(_, specs, _) =>
        specs.flatMap(ZManaged.collectAll(_).map(_.forall(identity))).zipWith(f(c).toManaged_)(_ && _)
      case c @ TestCase(_, _, _) => f(c).toManaged_
    }

  /**
   * Iterates over the spec with the specified default execution strategy, and
   * effectfully transforming every test with the provided function, finally
   * reconstructing the spec with the same structure.
   */
  final def foreachExec[R1 <: R, E1, A](
    defExec: ExecutionStrategy
  )(failure: Cause[E] => ZIO[R1, E1, A], success: T => ZIO[R1, E1, A]): ZManaged[R1, Nothing, Spec[R1, E1, A]] =
    foldM[R1, Nothing, Spec[R1, E1, A]](defExec) {
      case SuiteCase(label, specs, exec) =>
        specs.foldCause(
          e => Spec.test(label, failure(e), TestAnnotationMap.empty),
          t => Spec.suite(label, ZManaged.succeedNow(t), exec)
        )
      case TestCase(label, test, annotations) =>
        test
          .foldCause(e => Spec.test(label, failure(e), annotations), t => Spec.test(label, success(t), annotations))
          .toManaged_
    }

  /**
   * Iterates over the spec with the sequential strategy as the default, and
   * effectfully transforming every test with the provided function, finally
   * reconstructing the spec with the same structure.
   */
  final def foreach[R1 <: R, E1, A](
    failure: Cause[E] => ZIO[R1, E1, A],
    success: T => ZIO[R1, E1, A]
  ): ZManaged[R1, Nothing, Spec[R1, E1, A]] =
    foreachExec(ExecutionStrategy.Sequential)(failure, success)

  /**
   * Iterates over the spec with the parallel strategy as the default, and
   * effectfully transforming every test with the provided function, finally
   * reconstructing the spec with the same structure.
   */
  final def foreachPar[R1 <: R, E1, A](
    failure: Cause[E] => ZIO[R1, E1, A],
    success: T => ZIO[R1, E1, A]
  ): ZManaged[R1, Nothing, Spec[R1, E1, A]] =
    foreachExec(ExecutionStrategy.Parallel)(failure, success)

  /**
   * Iterates over the spec with the parallel (`n`) strategy as the default, and
   * effectfully transforming every test with the provided function, finally
   * reconstructing the spec with the same structure.
   */
  final def foreachParN[R1 <: R, E1, A](
    n: Int
  )(failure: Cause[E] => ZIO[R1, E1, A], success: T => ZIO[R1, E1, A]): ZManaged[R1, Nothing, Spec[R1, E1, A]] =
    foreachExec(ExecutionStrategy.ParallelN(n))(failure, success)

  /**
   * Returns a new spec with remapped errors and tests.
   */
  final def mapBoth[E1, T1](f: E => E1, g: T => T1)(implicit ev: CanFail[E]): Spec[R, E1, T1] =
    transform[R, E1, T1] {
      case SuiteCase(label, specs, exec)      => SuiteCase(label, specs.mapError(f), exec)
      case TestCase(label, test, annotations) => TestCase(label, test.mapBoth(f, g), annotations)
    }

  /**
   * Returns a new spec with remapped errors.
   */
  final def mapError[E1](f: E => E1)(implicit ev: CanFail[E]): Spec[R, E1, T] =
    transform[R, E1, T] {
      case SuiteCase(label, specs, exec)      => SuiteCase(label, specs.mapError(f), exec)
      case TestCase(label, test, annotations) => TestCase(label, test.mapError(f), annotations)
    }

  /**
   * Returns a new spec with remapped labels.
   */
  final def mapLabel(f: String => String): Spec[R, E, T] =
    transform[R, E, T] {
      case SuiteCase(label, specs, exec)      => SuiteCase(f(label), specs, exec)
      case TestCase(label, test, annotations) => TestCase(f(label), test, annotations)
    }

  /**
   * Returns a new spec with remapped tests.
   */
  final def mapTest[T1](f: T => T1): Spec[R, E, T1] =
    transform[R, E, T1] {
      case SuiteCase(label, specs, exec)      => SuiteCase(label, specs, exec)
      case TestCase(label, test, annotations) => TestCase(label, test.map(f), annotations)
    }

  /**
   * Provides each test in this spec with its required environment
   */
  final def provide(r: R)(implicit ev: NeedsEnv[R]): Spec[Any, E, T] =
    provideSome(_ => r)

  /**
   * Provides each test with the part of the environment that is not part of
   * the `TestEnvironment`, leaving a spec that only depends on the
   * `TestEnvironment`.
   *
   * {{{
   * val loggingLayer: ZLayer[Any, Nothing, Logging] = ???
   *
   * val spec: ZSpec[TestEnvironment with Logging, Nothing] = ???
   *
   * val spec2 = spec.provideCustomLayer(loggingLayer)
   * }}}
   */
  def provideCustomLayer[E1 >: E, R1 <: Has[_]](
    layer: ZLayer[TestEnvironment, E1, R1]
  )(implicit ev: TestEnvironment with R1 <:< R, tagged: Tag[R1]): Spec[TestEnvironment, E1, T] =
    provideSomeLayer[TestEnvironment](layer)

  /**
   * Provides all tests with a shared version of the part of the environment
   * that is not part of the `TestEnvironment`, leaving a spec that only
   * depends on the `TestEnvironment`.
   *
   * {{{
   * val loggingLayer: ZLayer[Any, Nothing, Logging] = ???
   *
   * val spec: ZSpec[TestEnvironment with Logging, Nothing] = ???
   *
   * val spec2 = spec.provideCustomLayerShared(loggingLayer)
   * }}}
   */
  def provideCustomLayerShared[E1 >: E, R1 <: Has[_]](
    layer: ZLayer[TestEnvironment, E1, R1]
  )(implicit ev: TestEnvironment with R1 <:< R, tagged: Tag[R1]): Spec[TestEnvironment, E1, T] =
    provideSomeLayerShared[TestEnvironment](layer)

  /**
   * Provides a layer to the spec, translating it up a level.
   */
  final def provideLayer[E1 >: E, R0, R1](
    layer: ZLayer[R0, E1, R1]
  )(implicit ev1: R1 <:< R, ev2: NeedsEnv[R]): Spec[R0, E1, T] =
    transform[R0, E1, T] {
      case SuiteCase(label, specs, exec)      => SuiteCase(label, specs.provideLayer(layer), exec)
      case TestCase(label, test, annotations) => TestCase(label, test.provideLayer(layer), annotations)
    }

  /**
   * Provides a layer to the spec, sharing services between all tests.
   */
  final def provideLayerShared[E1 >: E, R0, R1](
    layer: ZLayer[R0, E1, R1]
  )(implicit ev1: R1 <:< R, ev2: NeedsEnv[R]): Spec[R0, E1, T] =
    caseValue match {
      case SuiteCase(label, specs, exec) =>
        Spec.suite(
          label,
          layer.memoize.flatMap(layer => specs.map(_.map(_.provideLayer(layer))).provideLayer(layer)),
          exec
        )
      case TestCase(label, test, annotations) =>
        Spec.test(label, test.provideLayer(layer), annotations)
    }

  /**
   * Uses the specified function to provide each test in this spec with part of
   * its required environment.
   */
  final def provideSome[R0](f: R0 => R)(implicit ev: NeedsEnv[R]): Spec[R0, E, T] =
    transform[R0, E, T] {
      case SuiteCase(label, specs, exec)      => SuiteCase(label, specs.provideSome(f), exec)
      case TestCase(label, test, annotations) => TestCase(label, test.provideSome(f), annotations)
    }

  /**
   * Splits the environment into two parts, providing each test with one part
   * using the specified layer and leaving the remainder `R0`.
   *
   * {{{
   * val clockLayer: ZLayer[Any, Nothing, Clock] = ???
   *
   * val spec: ZSpec[Clock with Random, Nothing] = ???
   *
   * val spec2 = spec.provideSomeLayer[Random](clockLayer)
   * }}}
   */
  final def provideSomeLayer[R0 <: Has[_]]: Spec.ProvideSomeLayer[R0, R, E, T] =
    new Spec.ProvideSomeLayer[R0, R, E, T](self)

  /**
   * Splits the environment into two parts, providing all tests with a shared
   * version of one part using the specified layer and leaving the remainder
   * `R0`.
   *
   * {{{
   * val clockLayer: ZLayer[Any, Nothing, Clock] = ???
   *
   * val spec: ZSpec[Clock with Random, Nothing] = ???
   *
   * val spec2 = spec.provideSomeLayerShared[Random](clockLayer)
   * }}}
   */
  final def provideSomeLayerShared[R0 <: Has[_]]: Spec.ProvideSomeLayerShared[R0, R, E, T] =
    new Spec.ProvideSomeLayerShared[R0, R, E, T](self)

  /**
   * Computes the size of the spec, i.e. the number of tests in the spec.
   */
  final def size: ZManaged[R, E, Int] =
    fold[ZManaged[R, E, Int]] {
      case SuiteCase(_, counts, _) => counts.flatMap(ZManaged.collectAll(_).map(_.sum))
      case TestCase(_, _, _)       => ZManaged.succeedNow(1)
    }

  /**
   * Transforms the spec one layer at a time.
   */
  final def transform[R1, E1, T1](
    f: SpecCase[R, E, T, Spec[R1, E1, T1]] => SpecCase[R1, E1, T1, Spec[R1, E1, T1]]
  ): Spec[R1, E1, T1] =
    caseValue match {
      case SuiteCase(label, specs, exec) => Spec(f(SuiteCase(label, specs.map(_.map(_.transform(f))), exec)))
      case t @ TestCase(_, _, _)         => Spec(f(t))
    }

  /**
   * Transforms the spec statefully, one layer at a time.
   */
  final def transformAccum[R1, E1, T1, Z](
    z0: Z
  )(
    f: (Z, SpecCase[R, E, T, Spec[R1, E1, T1]]) => (Z, SpecCase[R1, E1, T1, Spec[R1, E1, T1]])
  ): ZManaged[R, E, (Z, Spec[R1, E1, T1])] =
    caseValue match {
      case SuiteCase(label, specs, exec) =>
        for {
          specs <- specs
          result <- ZManaged.foldLeft(specs)(z0 -> Vector.empty[Spec[R1, E1, T1]]) { case ((z, vector), spec) =>
                      spec.transformAccum(z)(f).map { case (z1, spec1) => z1 -> (vector :+ spec1) }
                    }
          (z, specs1)     = result
          res             = f(z, SuiteCase(label, ZManaged.succeedNow(specs1), exec))
          (z1, caseValue) = res
        } yield z1 -> Spec(caseValue)
      case t @ TestCase(_, _, _) =>
        val (z, caseValue) = f(z0, t)
        ZManaged.succeedNow(z -> Spec(caseValue))
    }

  /**
   * Updates a service in the environment of this effect.
   */
  final def updateService[M] =
    new Spec.UpdateService[R, E, T, M](self)

  /**
   * Runs the spec only if the specified predicate is satisfied.
   */
  final def when(b: => Boolean)(implicit ev: T <:< TestSuccess): Spec[R with Annotations, E, TestSuccess] =
    whenM(ZIO.succeedNow(b))

  /**
   * Runs the spec only if the specified effectual predicate is satisfied.
   */
  final def whenM[R1 <: R, E1 >: E](
    b: ZIO[R1, E1, Boolean]
  )(implicit ev: T <:< TestSuccess): Spec[R1 with Annotations, E1, TestSuccess] =
    caseValue match {
      case SuiteCase(label, specs, exec) =>
        Spec.suite(
          label,
          b.toManaged_.flatMap(b =>
            if (b) specs.asInstanceOf[ZManaged[R1, E1, Vector[Spec[R1, E1, TestSuccess]]]]
            else ZManaged.succeedNow(Vector.empty)
          ),
          exec
        )
      case TestCase(label, test, annotations) =>
        Spec.test(
          label,
          b.flatMap(b =>
            if (b) test.asInstanceOf[ZIO[R1, E1, TestSuccess]]
            else Annotations.annotate(TestAnnotation.ignored, 1).as(TestSuccess.Ignored)
          ),
          annotations
        )
    }
}

object Spec {
  sealed abstract class SpecCase[-R, +E, +T, +A] { self =>
    final def map[B](f: A => B): SpecCase[R, E, T, B] = self match {
      case SuiteCase(label, specs, exec)      => SuiteCase(label, specs.map(_.map(f)), exec)
      case TestCase(label, test, annotations) => TestCase(label, test, annotations)
    }
  }
  final case class SuiteCase[-R, +E, +A](
    label: String,
    specs: ZManaged[R, E, Vector[A]],
    exec: Option[ExecutionStrategy]
  ) extends SpecCase[R, E, Nothing, A]
  final case class TestCase[-R, +E, +T](label: String, test: ZIO[R, E, T], annotations: TestAnnotationMap)
      extends SpecCase[R, E, T, Nothing]

  final def suite[R, E, T](
    label: String,
    specs: ZManaged[R, E, Vector[Spec[R, E, T]]],
    exec: Option[ExecutionStrategy]
  ): Spec[R, E, T] =
    Spec(SuiteCase(label, specs, exec))

  final def test[R, E, T](label: String, test: ZIO[R, E, T], annotations: TestAnnotationMap): Spec[R, E, T] =
    Spec(TestCase(label, test, annotations))

  final class ProvideSomeLayer[R0 <: Has[_], -R, +E, +T](private val self: Spec[R, E, T]) extends AnyVal {
    def apply[E1 >: E, R1 <: Has[_]](
      layer: ZLayer[R0, E1, R1]
    )(implicit ev1: R0 with R1 <:< R, ev2: NeedsEnv[R], tagged: Tag[R1]): Spec[R0, E1, T] =
      self.provideLayer[E1, R0, R0 with R1](ZLayer.identity[R0] ++ layer)
  }

  final class ProvideSomeLayerShared[R0 <: Has[_], -R, +E, +T](private val self: Spec[R, E, T]) extends AnyVal {
    def apply[E1 >: E, R1 <: Has[_]](
      layer: ZLayer[R0, E1, R1]
    )(implicit ev1: R0 with R1 <:< R, ev2: NeedsEnv[R], tagged: Tag[R1]): Spec[R0, E1, T] =
      self.caseValue match {
        case SuiteCase(label, specs, exec) =>
          Spec.suite(
            label,
            layer.memoize.flatMap(layer => specs.map(_.map(_.provideSomeLayer(layer))).provideSomeLayer(layer)),
            exec
          )
        case TestCase(label, test, annotations) =>
          Spec.test(label, test.provideSomeLayer(layer), annotations)
      }
  }

  final class UpdateService[-R, +E, +T, M](private val self: Spec[R, E, T]) extends AnyVal {
    def apply[R1 <: R with Has[M]](f: M => M)(implicit ev: Has.IsHas[R1], tag: Tag[M]): Spec[R1, E, T] =
      self.provideSome(ev.update(_, f))
  }
}

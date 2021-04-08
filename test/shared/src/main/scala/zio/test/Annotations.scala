package zio.test

import zio.duration.Duration
import zio.stream.{ZSink, ZStream}
import zio.test.environment.{TestClock, TestConsole, TestEnvironment, TestRandom, TestSystem, testEnvironment}
import zio.{Console, _}

import scala.collection.immutable.SortedSet
import scala.util.Try

/**
 * The `Has[Annotations]` trait provides access to an annotation map that tests
 * can add arbitrary annotations to. Each annotation consists of a string
 * identifier, an initial value, and a function for combining two values.
 * Has[Annotations] form monoids and you can think of `Has[Annotations]` as a more
 * structured logging service or as a super polymorphic version of the writer
 * monad effect.
 */
trait Annotations extends Serializable {
  def annotate[V](key: TestAnnotation[V], value: V): UIO[Unit]
  def get[V](key: TestAnnotation[V]): UIO[V]
  def withAnnotation[R, E, A](zio: ZIO[R, E, A]): ZIO[R, Annotated[E], Annotated[A]]
  def supervisedFibers: UIO[SortedSet[Fiber.Runtime[Any, Any]]]
}

object Annotations {

  /**
   * Accesses an `Has[Annotations]` instance in the environment and appends the
   * specified annotation to the annotation map.
   */
  def annotate[V](key: TestAnnotation[V], value: V): URIO[Has[Annotations], Unit] =
    ZIO.accessM(_.get.annotate(key, value))

  /**
   * Accesses an `Has[Annotations]` instance in the environment and retrieves the
   * annotation of the specified type, or its default value if there is none.
   */
  def get[V](key: TestAnnotation[V]): URIO[Has[Annotations], V] =
    ZIO.accessM(_.get.get(key))

  /**
   * Returns a set of all fibers in this test.
   */
  def supervisedFibers: ZIO[Has[Annotations], Nothing, SortedSet[Fiber.Runtime[Any, Any]]] =
    ZIO.accessM(_.get.supervisedFibers)

  /**
   * Constructs a new `Has[Annotations]` service.
   */
  val live: Layer[Nothing, Has[Annotations]] =
    ZLayer.apply(FiberRef.make(TestAnnotationMap.empty).map { fiberRef =>
      new Annotations {
        def annotate[V](key: TestAnnotation[V], value: V): UIO[Unit] =
          fiberRef.update(_.annotate(key, value))
        def get[V](key: TestAnnotation[V]): UIO[V] =
          fiberRef.get.map(_.get(key))
        def withAnnotation[R, E, A](zio: ZIO[R, E, A]): ZIO[R, Annotated[E], Annotated[A]] =
          fiberRef.locally(TestAnnotationMap.empty) {
            zio.foldM(e => fiberRef.get.map((e, _)).flip, a => fiberRef.get.map((a, _)))
          }
        def supervisedFibers: UIO[SortedSet[Fiber.Runtime[Any, Any]]] =
          ZIO.descriptorWith { descriptor =>
            get(TestAnnotation.fibers).flatMap {
              case Left(_) => ZIO.succeedNow(SortedSet.empty[Fiber.Runtime[Any, Any]])
              case Right(refs) =>
                ZIO
                  .foreach(refs)(ref => ZIO.effectTotal(ref.get))
                  .map(_.foldLeft(SortedSet.empty[Fiber.Runtime[Any, Any]])(_ ++ _))
                  .map(_.filter(_.id != descriptor.id))
            }
          }
      }
    })

  /**
   * Accesses an `Has[Annotations]` instance in the environment and executes the
   * specified effect with an empty annotation map, returning the annotation
   * map along with the result of execution.
   */
  def withAnnotation[R <: Has[Annotations], E, A](zio: ZIO[R, E, A]): ZIO[R, Annotated[E], Annotated[A]] =
    ZIO.accessM(_.get.withAnnotation(zio))
}

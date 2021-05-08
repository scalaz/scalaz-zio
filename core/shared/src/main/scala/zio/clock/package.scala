/*
 * Copyright 2017-2021 John A. De Goes and the ZIO Contributors
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

import zio.duration.Duration
import zio.internal.Scheduler

import java.time.{Instant, LocalDateTime, OffsetDateTime}
import java.util.concurrent.TimeUnit

package object clock {

  type Clock = Has[Clock.Service]

  object Clock extends PlatformSpecific with Serializable {
    trait Service extends Serializable {

      def currentTime(unit: TimeUnit): UIO[Long]

      def currentDateTime: UIO[OffsetDateTime]

      def instant: UIO[java.time.Instant]

      def localDateTime: UIO[java.time.LocalDateTime]

      def nanoTime: UIO[Long]

      def scheduler: UIO[Scheduler]

      def sleep(duration: Duration): UIO[Unit]
    }

    object Service {
      val live: Service = new Service {

        def currentDateTime: UIO[OffsetDateTime] =
          ZIO.effectTotal(OffsetDateTime.now())

        def currentTime(unit: TimeUnit): UIO[Long] =
          instant.map { instant =>
            // A nicer solution without loss of precision or range would be
            // unit.toChronoUnit.between(Instant.EPOCH, inst)
            // However, ChronoUnit is not available on all platforms
            unit match {
              case TimeUnit.NANOSECONDS =>
                instant.getEpochSecond() * 1000000000 + instant.getNano()
              case TimeUnit.MICROSECONDS =>
                instant.getEpochSecond() * 1000000 + instant.getNano() / 1000
              case _ => unit.convert(instant.toEpochMilli(), TimeUnit.MILLISECONDS)
            }
          }

        def instant: UIO[Instant] =
          ZIO.effectTotal(Instant.now())

        def localDateTime: UIO[LocalDateTime] =
          ZIO.effectTotal(LocalDateTime.now())

        def nanoTime: UIO[Long] =
          ZIO.effectTotal(System.nanoTime)

        def scheduler: UIO[Scheduler] =
          ZIO.effectTotal(globalScheduler)

        def sleep(duration: Duration): UIO[Unit] =
          ZIO.effectAsyncInterrupt { cb =>
            val canceler = globalScheduler.schedule(() => cb(UIO.unit), duration)
            Left(UIO.effectTotal(canceler()))
          }
      }
    }

    val any: ZLayer[Clock, Nothing, Clock] =
      ZLayer.requires[Clock]

    val live: Layer[Nothing, Clock] =
      ZLayer.succeed(Service.live)
  }

  /**
   * Get the current time, represented in the current timezone.
   */
  val currentDateTime: URIO[Clock, OffsetDateTime] =
    ZIO.accessM(_.get.currentDateTime)

  /**
   * Returns the current time, relative to the Unix epoch.
   */
  def currentTime(unit: => TimeUnit): URIO[Clock, Long] =
    ZIO.accessM(_.get.currentTime(unit))

  /**
   * Returns the current instant.
   */
  val instant: URIO[Clock, Instant] =
    ZIO.accessM(_.get.instant)

  /**
   * Returns the current date time.
   */
  val localDateTime: URIO[Clock, LocalDateTime] =
    ZIO.accessM(_.get.localDateTime)

  /**
   * Returns the system nano time, which is not relative to any date.
   */
  val nanoTime: URIO[Clock, Long] =
    ZIO.accessM(_.get.nanoTime)

  /**
   * Returns the scheduler used for scheduling effects.
   */
  val scheduler: URIO[Clock, Scheduler] =
    ZIO.accessM(_.get.scheduler)

  /**
   * Sleeps for the specified duration. This is always asynchronous.
   */
  def sleep(duration: => Duration): URIO[Clock, Unit] =
    ZIO.accessM(_.get.sleep(duration))
}

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

package zio.test.mock

import zio.clock.Clock
import zio.duration.Duration
import zio.{Has, UIO, URLayer, ZLayer}

import java.time.OffsetDateTime
import java.util.concurrent.TimeUnit

object MockClock extends Mock[Clock] {

  object CurrentTime     extends Effect[TimeUnit, Nothing, Long]
  object CurrentDateTime extends Effect[Unit, Nothing, OffsetDateTime]
  object Instant         extends Effect[Unit, Nothing, java.time.Instant]
  object LocalDateTime   extends Effect[Unit, Nothing, java.time.LocalDateTime]
  object NanoTime        extends Effect[Unit, Nothing, Long]
  object Scheduler       extends Effect[Unit, Nothing, zio.internal.Scheduler]
  object Sleep           extends Effect[Duration, Nothing, Unit]

  val compose: URLayer[Has[Proxy], Clock] =
    ZLayer.fromService(proxy =>
      new Clock.Service {
        def currentTime(unit: TimeUnit): UIO[Long]          = proxy(CurrentTime, unit)
        def currentDateTime: UIO[OffsetDateTime]            = proxy(CurrentDateTime)
        val nanoTime: UIO[Long]                             = proxy(NanoTime)
        def scheduler: UIO[zio.internal.Scheduler]          = proxy(Scheduler)
        def sleep(duration: Duration): UIO[Unit]            = proxy(Sleep, duration)
        def instant: zio.UIO[java.time.Instant]             = proxy(Instant)
        def localDateTime: zio.UIO[java.time.LocalDateTime] = proxy(LocalDateTime)
      }
    )
}

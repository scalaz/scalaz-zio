/*
 * Copyright 2017-2019 John A. De Goes and the ZIO Contributors
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

// Copyright (C) 2018 John A. De Goes. All rights reserved.
package scalaz.zio.clock

import java.util.concurrent.TimeUnit

import scalaz.zio.duration.Duration
import scalaz.zio.scheduler.{ Scheduler, SchedulerLive }
import scalaz.zio.{ IO, ZIO }

trait Clock extends Scheduler with Serializable {
  val clock: Clock.Service[Any]
}

object Clock extends Serializable {
  trait Service[R] extends Serializable {
    def currentTime(unit: TimeUnit): ZIO[R, Nothing, Long]
    val nanoTime: ZIO[R, Nothing, Long]
    def sleep(duration: Duration): ZIO[R, Nothing, Unit]
  }

  trait Live extends SchedulerLive with Clock {
    val clock: Service[Any] = new Service[Any] {
      def currentTime(unit: TimeUnit): ZIO[Any, Nothing, Long] =
        IO.sync(System.currentTimeMillis).map(l => unit.convert(l, TimeUnit.MILLISECONDS))

      val nanoTime: ZIO[Any, Nothing, Long] = IO.sync(System.nanoTime)

      def sleep(duration: Duration): ZIO[Any, Nothing, Unit] =
        scheduler.scheduler.flatMap(
          scheduler =>
            ZIO.asyncInterrupt[Any, Nothing, Unit] { k =>
              val canceler = scheduler
                .schedule(() => k(ZIO.unit), duration)

              Left(ZIO.sync(canceler()))
            }
        )
    }
  }
  object Live extends Live
}

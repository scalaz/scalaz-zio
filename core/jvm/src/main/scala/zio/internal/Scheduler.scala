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

package zio.internal

import zio.duration.Duration
import zio.internal.Scheduler.CancelToken

import java.util.concurrent.{ScheduledExecutorService, TimeUnit}

private[zio] abstract class Scheduler {
  def asScheduledExecutorService: ScheduledExecutorService
  def schedule(task: Runnable, duration: Duration): CancelToken
}

private[zio] object Scheduler {
  type CancelToken = () => Boolean

  def fromScheduledExecutorService(service: ScheduledExecutorService): Scheduler =
    new Scheduler {
      val ConstFalse = () => false

      def asScheduledExecutorService: ScheduledExecutorService =
        service

      def schedule(task: Runnable, duration: Duration): CancelToken = (duration: @unchecked) match {
        case Duration.Infinity => ConstFalse
        case Duration.Zero =>
          task.run()

          ConstFalse
        case Duration.Finite(_) =>
          val future = service.schedule(
            new Runnable {
              def run: Unit =
                task.run()
            },
            duration.toNanos,
            TimeUnit.NANOSECONDS
          )

          () => future.cancel(true)
      }
    }
}

/*
 * Copyright 2021 John A. De Goes and the ZIO Contributors
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

package zio.test.environment

import zio.duration._
import zio.internal.Scheduler
import zio.{UIO, ZIO}

trait TestClockPlatformSpecific { self: TestClock.Test =>

  def scheduler: UIO[Scheduler] =
    ZIO.runtime[Any].map { runtime =>
      new Scheduler {
        def schedule(runnable: Runnable, duration: Duration): Scheduler.CancelToken = {
          val canceler =
            runtime.unsafeRunAsyncCancelable(sleep(duration) *> ZIO.effectTotal(runnable.run()))(_ => ())
          () => canceler(zio.Fiber.Id.None).interrupted
        }
      }
    }
}

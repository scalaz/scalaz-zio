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

/**
 * A `SubscriptionRef[A]` contains a `Ref.Synchronized` with a value of type
 * `A` and a `ZStream` that can be subscribed to in order to receive the
 * current value as well as all changes to the value.
 */
final class SubscriptionRef[A] private (
  val ref: ZRef.Synchronized[Any, Any, Nothing, Nothing, A, A],
  val changes: Stream[Nothing, A]
)

object SubscriptionRef {

  /**
   * Creates a new `SubscriptionRef` with the specified value.
   */
  def make[A](a: A): UIO[SubscriptionRef[A]] =
    for {
      ref <- Ref.Synchronized.make(a)
      hub <- Hub.unbounded[A]
      changes = ZStream.unwrapManaged {
                  ZManaged {
                    ref.modifyZIO { a =>
                      ZIO.succeedNow(a).zipWith(hub.subscribe.zio) { case (a, (finalizer, queue)) =>
                        ((finalizer, ZStream(a) ++ ZStream.fromQueue(queue)), a)
                      }
                    }.uninterruptible
                  }
                }
    } yield new SubscriptionRef(ref.tapInput(hub.publish), changes)
}

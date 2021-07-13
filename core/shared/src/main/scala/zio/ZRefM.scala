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

@deprecated("use ZRef.Synchronized", "2.0.0")
object ZRefM {

  /**
   * Creates a new `RefM` and a `Dequeue` that will emit every change to the
   * `RefM`.
   */
  @deprecated("use SubscriptionRef", "2.0.0")
  def dequeueRef[A](a: A): UIO[(RefM[A], Dequeue[A])] =
    ZRef.Synchronized.dequeueRef(a)

  /**
   * Creates a new `ZRefM` with the specified value.
   */
  def make[A](a: A): UIO[RefM[A]] =
    ZRef.Synchronized.make(a)

  /**
   * Creates a new `ZRefM` with the specified value in the context of a
   * `Managed.`
   */
  def makeManaged[A](a: A): UManaged[RefM[A]] =
    ZRef.Synchronized.makeManaged(a)
}

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

object Ref extends Serializable {
  private[zio] type Atomic[A] = ZRef.Atomic[A]

  type Synchronized[A] = ZRef.Synchronized[Any, Any, Nothing, Nothing, A, A]
  val Synchronized: ZRef.Synchronized.type = ZRef.Synchronized

  /**
   * @see [[zio.ZRef.make]]
   */
  def make[A](a: A): UIO[Ref[A]] =
    ZRef.make(a)

  /**
   * @see [[zio.ZRef.makeManaged]]
   */
  def makeManaged[A](a: A): UManaged[Ref[A]] =
    ZRef.makeManaged(a)

  /**
   * @see [[zio.ZRef.unsafeMake]]
   */
  private[zio] def unsafeMake[A](a: A): Ref.Atomic[A] =
    ZRef.unsafeMake(a)
}

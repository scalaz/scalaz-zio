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

package zio

import scala.concurrent.{ CanAwait, ExecutionContext, Future }
import scala.concurrent.duration.Duration
import scala.util.Try

trait CancelableFuture[+E, +A] extends Future[A] with FutureTransformCompat[A] {
  private[zio] val future: Future[A]
  private[zio] val interrupt: UIO[Exit[E, A]]

  def cancel: UIO[Exit[E, A]] =
    interrupt

  def onComplete[U](f: Try[A] => U)(implicit executor: ExecutionContext): Unit =
    future.onComplete(f)(executor)

  def isCompleted: Boolean =
    future.isCompleted

  def value: Option[Try[A]] =
    future.value

  def ready(atMost: Duration)(implicit permit: CanAwait): this.type = {
    future.ready(atMost)(permit)
    this
  }

  def result(atMost: Duration)(implicit permit: CanAwait): A =
    future.result(atMost)
}

object CancelableFuture {
  private[zio] def unsafeMake[E, A](f: Future[A], c: UIO[Exit[E, A]]): CancelableFuture[E, A] =
    new CancelableFuture[E, A] {
      val future    = f
      val interrupt = c
    }
}

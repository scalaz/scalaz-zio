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

package object zio
    extends BuildFromCompat
    with EitherCompat
    with FunctionToLayerOps
    with IntersectionTypeCompat
    with PlatformSpecific
    with VersionSpecific
    with DurationModule {
  private[zio] type Callback[E, A] = Exit[E, A] => Any

  type Canceler[-R] = URIO[R, Any]

  type IO[+E, +A]   = ZIO[Any, E, A]         // Succeed with an `A`, may fail with `E`        , no requirements.
  type Task[+A]     = ZIO[Any, Throwable, A] // Succeed with an `A`, may fail with `Throwable`, no requirements.
  type RIO[-R, +A]  = ZIO[R, Throwable, A]   // Succeed with an `A`, may fail with `Throwable`, requires an `R`.
  type UIO[+A]      = ZIO[Any, Nothing, A]   // Succeed with an `A`, cannot fail              , no requirements.
  type URIO[-R, +A] = ZIO[R, Nothing, A]     // Succeed with an `A`, cannot fail              , requires an `R`.

  type Managed[+E, +A]   = ZManaged[Any, E, A]         //Manage an `A`, may fail with `E`        , no requirements
  type TaskManaged[+A]   = ZManaged[Any, Throwable, A] //Manage an `A`, may fail with `Throwable`, no requirements
  type RManaged[-R, +A]  = ZManaged[R, Throwable, A]   //Manage an `A`, may fail with `Throwable`, requires an `R`
  type UManaged[+A]      = ZManaged[Any, Nothing, A]   //Manage an `A`, cannot fail              , no requirements
  type URManaged[-R, +A] = ZManaged[R, Nothing, A]     //Manage an `A`, cannot fail              , requires an `R`

  val Managed: ZManaged.type = ZManaged

  type RLayer[-RIn, +ROut]  = ZLayer[RIn, Throwable, ROut]
  type URLayer[-RIn, +ROut] = ZLayer[RIn, Nothing, ROut]
  type Layer[+E, +ROut]     = ZLayer[Any, E, ROut]
  type ULayer[+ROut]        = ZLayer[Any, Nothing, ROut]
  type TaskLayer[+ROut]     = ZLayer[Any, Throwable, ROut]

  type Queue[A] = ZQueue[Any, Any, Nothing, Nothing, A, A]
  val Queue: ZQueue.type = ZQueue

  /**
   * A queue that can only be dequeued.
   */
  type ZDequeue[-R, +E, +A] = ZQueue[Nothing, R, Any, E, Nothing, A]
  type Dequeue[+A]          = ZQueue[Nothing, Any, Any, Nothing, Nothing, A]

  /**
   * A queue that can only be enqueued.
   */
  type ZEnqueue[-R, +E, -A] = ZQueue[R, Nothing, E, Any, A, Any]
  type Enqueue[-A]          = ZQueue[Any, Nothing, Nothing, Any, A, Any]

  type Ref[A] = ZRef[Any, Any, Nothing, Nothing, A, A]

  type ERef[+E, A] = ZRef[Any, Any, E, E, A, A]
  val ERef: ZRef.type = ZRef

  @deprecated("use ZRef.Synchronized", "2.0.0")
  type ZRefM[-RA, -RB, +EA, +EB, -A, +B] = ZRef.Synchronized[RA, RB, EA, EB, A, B]
  @deprecated("use Ref.Synchronized", "2.0.0")
  type RefM[A] = ZRefM[Any, Any, Nothing, Nothing, A, A]
  @deprecated("use ERef.Synchronized", "2.0.0")
  type ERefM[+E, A] = ZRefM[Any, Any, E, E, A, A]

  type FiberRef[A] = ZFiberRef[Nothing, Nothing, A, A]
  val FiberRef: ZFiberRef.type = ZFiberRef

  type Hub[A] = ZHub[Any, Any, Nothing, Nothing, A, A]
  val Hub: ZHub.type = ZHub

  type Semaphore = stm.TSemaphore
}

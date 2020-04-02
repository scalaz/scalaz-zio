/*
 * Copyright 2017-2020 John A. De Goes and the ZIO Contributors
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

package object zio extends EitherCompat with PlatformSpecific with VersionSpecific {
  private[zio] type Callback[E, A] = Exit[E, A] => Any

  type Canceler[-R] = URIO[R, Any]

  type RIO[-R, +A]  = ZIO[R, Throwable, A]
  type URIO[-R, +A] = ZIO[R, Nothing, A]
  type IO[+E, +A]   = ZIO[Any, E, A]
  type UIO[+A]      = ZIO[Any, Nothing, A]
  type Task[+A]     = ZIO[Any, Throwable, A]

  type RManaged[-R, +A]  = ZManaged[R, Throwable, A]
  type URManaged[-R, +A] = ZManaged[R, Nothing, A]
  type Managed[+E, +A]   = ZManaged[Any, E, A]
  type UManaged[+A]      = ZManaged[Any, Nothing, A]
  type TaskManaged[+A]   = ZManaged[Any, Throwable, A]

  type RLayer[-RIn, +ROut]  = ZLayer[RIn, Throwable, ROut]
  type URLayer[-RIn, +ROut] = ZLayer[RIn, Nothing, ROut]
  type Layer[+E, +ROut]     = ZLayer[Any, E, ROut]
  type ULayer[+ROut]        = ZLayer[Any, Nothing, ROut]
  type TaskLayer[+ROut]     = ZLayer[Any, Throwable, ROut]

  type Queue[A] = ZQueue[Any, Nothing, Any, Nothing, A, A]

  type Ref[A]      = ZRef[Nothing, Nothing, A, A]
  type ERef[+E, A] = ZRef[E, E, A, A]

  type ZOptic[-SG, -SS, -B, +EG, +ES, +A, +T] = (SG => Either[EG, A], B => SS => Either[ES, T])

  type ZLens[+EA, +EB, -S, +T, +A, -B]      = ZOptic[S, S, B, EA, EB, A, T]
  type ZPrism[+EA, +EB, -S, +T, +A, -B]     = ZOptic[S, Any, B, EA, EB, A, T]
  type ZTraversal[+EA, +EB, -S, +T, +A, -B] = ZOptic[S, S, List[B], EA, EB, List[A], T]

  type Lens[S, A]      = ZLens[Nothing, Nothing, S, S, A, A]
  type Optional[S, A]  = ZLens[Unit, Nothing, S, S, A, A]
  type Prism[S, A]     = ZPrism[Unit, Nothing, S, S, A, A]
  type Traversal[S, A] = ZTraversal[Nothing, Unit, S, S, A, A]

  object <*> {
    def unapply[A, B](ab: (A, B)): Some[(A, B)] =
      Some((ab._1, ab._2))
  }

  object Lens {
    def apply[S, A](get: S => A, set: A => S => S): Lens[S, A] =
      (s => Right(get(s)), a => s => Right(set(a)(s)))
  }

  object Optional {
    def apply[S, A](get: S => Option[A], set: A => S => S): Optional[S, A] =
      (s => get(s).fold[Either[Unit, A]](Left(()))(Right(_)), a => s => Right(set(a)(s)))
  }

  object Prism {
    def apply[S, A](get: S => Option[A], set: A => S): Prism[S, A] =
      (s => get(s).fold[Either[Unit, A]](Left(()))(Right(_)), a => _ => Right(set(a)))
  }

  object Traversal {
    def apply[S, A](get: S => List[A], set: List[A] => S => Option[S]): Traversal[S, A] =
      (s => Right(get(s)), a => s => set(a)(s).fold[Either[Unit, S]](Left(()))(Right(_)))
  }
}

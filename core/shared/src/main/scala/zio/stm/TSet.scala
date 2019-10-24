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

package zio.stm

class TSet[A] private (private val tmap: TMap[A, Unit]) extends AnyVal {
  final def contains(a: A): STM[Nothing, Boolean] =
    tmap.contains(a)

  final def delete(a: A): STM[Nothing, Unit] =
    tmap.delete(a)

  final def diff(other: TSet[A]): STM[Nothing, Unit] =
    for {
      vals <- other.toList.map(_.toSet)
      _    <- removeIf(vals.contains)
    } yield ()

  final def fold[B](zero: B)(op: (B, A) => B): STM[Nothing, B] =
    tmap.fold(zero)((acc, kv) => op(acc, kv._1))

  final def foldM[B, E](zero: B)(op: (B, A) => STM[E, B]): STM[E, B] =
    tmap.foldM(zero)((acc, kv) => op(acc, kv._1))

  final def foreach[E](f: A => STM[E, Unit]): STM[E, Unit] =
    foldM(())((_, a) => f(a))

  final def intersect(other: TSet[A]): STM[Nothing, Unit] =
    for {
      vals <- other.toList.map(_.toSet)
      _    <- retainIf(vals.contains)
    } yield ()

  final def put(a: A): STM[Nothing, Unit] =
    tmap.put(a, ())

  final def removeIf(p: A => Boolean): STM[Nothing, Unit] =
    tmap.removeIf((k, _) => p(k))

  final def retainIf(p: A => Boolean): STM[Nothing, Unit] =
    tmap.retainIf((k, _) => p(k))

  final def toList: STM[Nothing, List[A]] = tmap.keys

  final def transform(f: A => A): STM[Nothing, Unit] =
    tmap.transform((k, v) => f(k) -> v)

  final def transformM[E](f: A => STM[E, A]): STM[E, Unit] =
    tmap.transformM((k, v) => f(k).map(_ -> v))

  final def union(other: TSet[A]): STM[Nothing, Unit] =
    for {
      vals <- other.toList
      _    <- STM.collectAll(vals.map(put))
    } yield ()
}

object TSet {
  final def apply[A](data: A*): STM[Nothing, TSet[A]] = fromIterable(data)

  final def empty[A]: STM[Nothing, TSet[A]] = fromIterable(Nil)

  final def fromIterable[A](data: Iterable[A]): STM[Nothing, TSet[A]] =
    TMap.fromIterable(data.map((_, ()))).map(new TSet(_))
}

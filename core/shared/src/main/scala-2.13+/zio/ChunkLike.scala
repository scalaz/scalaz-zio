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

package zio

import scala.collection.immutable.{IndexedSeq, IndexedSeqOps, StrictOptimizedSeqOps}
import scala.collection.{IterableFactoryDefaults, SeqFactory}
import scala.reflect.ClassTag

/**
 * `ChunkLike` represents the capability for a `Chunk` to extend Scala's
 * collection library. Because of changes to Scala's collection library in
 * 2.13, separate versions of this trait are implemented for 2.11 / 2.12 and
 * 2.13 / Dotty. This allows code in `Chunk` to be written without concern for
 * the implementation details of Scala's collection library to the maximum
 * extent possible.
 *
 * Note that `IndexedSeq` is not a referentially transparent interface in that
 * it exposes methods that are partial (e.g. `apply`), allocate mutable state
 * (e.g. `iterator`), or are purely side effecting (e.g. `foreach`). `Chunk`
 * extends `IndexedSeq` to provide interoperability with Scala's collection
 * library but users should avoid these methods whenever possible.
 */
trait ChunkLike[+A]
    extends IndexedSeq[A]
    with IndexedSeqOps[A, Chunk, Chunk[A]]
    with StrictOptimizedSeqOps[A, Chunk, Chunk[A]]
    with IterableFactoryDefaults[A, Chunk] { self: Chunk[A] =>

  override final def appended[A1 >: A](a1: A1): Chunk[A1] =
    append(a1)

  override final def prepended[A1 >: A](a1: A1): Chunk[A1] =
    prepend(a1)

  /**
   * Returns a filtered, mapped subset of the elements of this `Chunk`.
   */
  override def collect[B](pf: PartialFunction[A, B]): Chunk[B] =
    collectChunk(pf)

  /**
   * Returns the concatenation of mapping every element into a new chunk using
   * the specified function.
   */
  override final def flatMap[B](f: A => IterableOnce[B]): Chunk[B] = {
    val iterator               = self.iterator
    var chunks: List[Chunk[B]] = Nil
    var total                  = 0
    var B0: ClassTag[B]        = null.asInstanceOf[ClassTag[B]]
    while (iterator.hasNext) {
      val a     = iterator.next()
      val bs    = f(a)
      val chunk = ChunkLike.from(bs)
      if (chunk.length > 0) {
        if (B0 == null) {
          B0 = Chunk.classTagOf(chunk)
        }
        chunks ::= chunk
        total += chunk.length
      }
    }
    if (B0 == null) Chunk.empty
    else {
      implicit val B: ClassTag[B] = B0
      val dest: Array[B]          = Array.ofDim(total)
      val it                      = chunks.iterator
      var n                       = total
      while (it.hasNext) {
        val chunk = it.next()
        n -= chunk.length
        chunk.toArray(n, dest)
      }
      Chunk.fromArray(dest)
    }
  }

  /**
   * Returns a `SeqFactory` that can construct `Chunk` values. The
   * `SeqFactory` exposes a `newBuilder` method that is not referentially
   * transparent because it allocates mutable state.
   */
  override val iterableFactory: SeqFactory[Chunk] =
    ChunkLike

  /**
   * Returns a chunk with the elements mapped by the specified function.
   */
  override final def map[B](f: A => B): Chunk[B] =
    mapChunk(f)

  /**
   * Zips this chunk with the index of every element.
   */
  override final def zipWithIndex: Chunk[(A, Int)] =
    zipWithIndexFrom(0)
}

object ChunkLike extends SeqFactory[Chunk] {

  /**
   * Returns the empty `Chunk`.
   */
  def empty[A]: Chunk[A] =
    Chunk.empty

  /**
   * Constructs a `Chunk` from the specified `IterableOnce`.
   */
  def from[A](source: IterableOnce[A]): Chunk[A] =
    source match {
      case iterable: Iterable[A] => Chunk.fromIterable(iterable)
      case iterableOnce =>
        val chunkBuilder = ChunkBuilder.make[A]()
        iterableOnce.iterator.foreach(chunkBuilder.addOne)
        chunkBuilder.result()
    }

  /**
   * Constructs a new `ChunkBuilder`. This operation allocates mutable state
   * and is not referentially transparent. It is provided for compatibility
   * with Scala's collection library and should not be used for other purposes.
   */
  def newBuilder[A]: ChunkBuilder[A] =
    ChunkBuilder.make()
}

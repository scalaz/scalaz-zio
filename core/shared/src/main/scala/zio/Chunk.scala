/*
 * Copyright 2018-2021 John A. De Goes and the ZIO Contributors
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

import java.nio._
import java.util.concurrent.atomic.AtomicInteger
import scala.annotation.tailrec
import scala.collection.mutable.Builder
import scala.reflect.{ClassTag, classTag}

/**
 * A `Chunk[A]` represents a chunk of values of type `A`. Chunks are designed
 * are usually backed by arrays, but expose a purely functional, safe interface
 * to the underlying elements, and they become lazy on operations that would be
 * costly with arrays, such as repeated concatenation.
 *
 * The implementation of balanced concatenation is based on the one for
 * Conc-Trees in "Conc-Trees for Functional and Parallel Programming" by
 * Aleksandar Prokopec and Martin Odersky.
 * [[http://aleksandar-prokopec.com/resources/docs/lcpc-conc-trees.pdf]]
 *
 * NOTE: For performance reasons `Chunk` does not box primitive types. As a
 * result, it is not safe to construct chunks from heterogeneous primitive
 * types.
 */
sealed abstract class Chunk[+A] extends ChunkLike[A] { self =>

  /**
   * Appends an element to the chunk
   */
  @deprecated("use :+", "1.0.0")
  final def +[A1 >: A](a: A1): Chunk[A1] =
    self :+ a

  /**
   * Returns the concatenation of this chunk with the specified chunk.
   */
  final def ++[A1 >: A](that: Chunk[A1]): Chunk[A1] =
    (self, that) match {
      case (Chunk.AppendN(start, buffer, bufferUsed, _), that) =>
        val chunk = Chunk.fromArray(buffer.asInstanceOf[Array[A1]]).take(bufferUsed)
        start ++ chunk ++ that
      case (self, Chunk.PrependN(end, buffer, bufferUsed, _)) =>
        val chunk = Chunk.fromArray(buffer.asInstanceOf[Array[A1]]).takeRight(bufferUsed)
        self ++ chunk ++ end
      case (self, that) =>
        val diff = that.depth - self.depth
        if (math.abs(diff) <= 1) Chunk.Concat(self, that)
        else if (diff < -1) {
          if (self.left.depth >= self.right.depth) {
            val nr = self.right ++ that
            Chunk.Concat(self.left, nr)
          } else {
            val nrr = self.right.right ++ that
            if (nrr.depth == self.depth - 3) {
              val nr = Chunk.Concat(self.right.left, nrr)
              Chunk.Concat(self.left, nr)
            } else {
              val nl = Chunk.Concat(self.left, self.right.left)
              Chunk.Concat(nl, nrr)
            }
          }
        } else {
          if (that.right.depth >= that.left.depth) {
            val nl = self ++ that.left
            Chunk.Concat(nl, that.right)
          } else {
            val nll = self ++ that.left.left
            if (nll.depth == that.depth - 3) {
              val nl = Chunk.Concat(nll, that.left.right)
              Chunk.Concat(nl, that.right)
            } else {
              val nr = Chunk.Concat(that.left.right, that.right)
              Chunk.Concat(nll, nr)
            }
          }
        }
    }

  final def ++[A1 >: A](that: NonEmptyChunk[A1]): NonEmptyChunk[A1] =
    that.prepend(self)

  /**
   * Converts a chunk of bytes to a chunk of bits.
   */
  final def asBits(implicit ev: A <:< Byte): Chunk[Boolean] =
    Chunk.BitChunk(self.map(ev), 0, length << 3)

  /**
   * Get the element at the specified index.
   */
  def boolean(index: Int)(implicit ev: A <:< Boolean): Boolean =
    ev(apply(index))

  /**
   * Get the element at the specified index.
   */
  def byte(index: Int)(implicit ev: A <:< Byte): Byte =
    ev(apply(index))

  /**
   * Get the element at the specified index.
   */
  def char(index: Int)(implicit ev: A <:< Char): Char =
    ev(apply(index))

  /**
   * Returns a filtered, mapped subset of the elements of this chunk based on a .
   */
  def collectM[R, E, B](pf: PartialFunction[A, ZIO[R, E, B]]): ZIO[R, E, Chunk[B]] =
    if (isEmpty) ZIO.succeedNow(Chunk.empty) else self.materialize.collectM(pf)

  /**
   * Transforms all elements of the chunk for as long as the specified partial function is defined.
   */
  def collectWhile[B](pf: PartialFunction[A, B]): Chunk[B] =
    if (isEmpty) Chunk.empty else self.materialize.collectWhile(pf)

  def collectWhileM[R, E, B](pf: PartialFunction[A, ZIO[R, E, B]]): ZIO[R, E, Chunk[B]] =
    if (isEmpty) ZIO.succeedNow(Chunk.empty) else self.materialize.collectWhileM(pf)

  /**
   * Determines whether this chunk and the specified chunk have the same length
   * and every pair of corresponding elements of this chunk and the specified
   * chunk satisfy the specified predicate.
   */
  final def corresponds[B](that: Chunk[B])(f: (A, B) => Boolean): Boolean =
    if (self.length != that.length) false
    else {
      val leftIterator    = self.arrayIterator
      val rightIterator   = that.arrayIterator
      var left: Array[A]  = null
      var right: Array[B] = null
      var leftLength      = 0
      var rightLength     = 0
      var i               = 0
      var j               = 0
      var equal           = true
      var done            = false
      while (equal && !done) {
        if (i < leftLength && j < rightLength) {
          val a = left(i)
          val b = right(j)
          if (!f(a, b)) {
            equal = false
          }
          i += 1
          j += 1
        } else if (i == leftLength && leftIterator.hasNext) {
          left = leftIterator.next()
          leftLength = left.length
          i = 0
        } else if (j == rightLength && rightIterator.hasNext) {
          right = rightIterator.next()
          rightLength = right.length
          j = 0
        } else if (i == leftLength && j == rightLength) {
          done = true
        } else {
          equal = false
        }
      }
      equal
    }

  /**
   * Get the element at the specified index.
   */
  def double(index: Int)(implicit ev: A <:< Double): Double =
    ev(apply(index))

  /**
   * Drops the first `n` elements of the chunk.
   */
  override def drop(n: Int): Chunk[A] = {
    val len = self.length

    if (n <= 0) self
    else if (n >= len) Chunk.empty
    else
      self match {
        case Chunk.Slice(c, o, l)        => Chunk.Slice(c, o + n, l - n)
        case Chunk.Singleton(_) if n > 0 => Chunk.empty
        case c @ Chunk.Singleton(_)      => c
        case Chunk.Empty                 => Chunk.empty
        case _                           => Chunk.Slice(self, n, len - n)
      }
  }

  /**
   * Drops all elements so long as the predicate returns true.
   */
  override def dropWhile(f: A => Boolean): Chunk[A] = {
    val iterator = arrayIterator
    var continue = true
    var i        = 0
    while (continue && iterator.hasNext) {
      val array  = iterator.next()
      val length = array.length
      var j      = 0
      while (continue && j < length) {
        val a = array(j)
        if (f(a)) {
          i += 1
          j += 1
        } else {
          continue = false
        }
      }
    }
    drop(i)
  }

  def dropWhileM[R, E](p: A => ZIO[R, E, Boolean]): ZIO[R, E, Chunk[A]] = ZIO.effectSuspendTotal {
    val length  = self.length
    val builder = ChunkBuilder.make[A]()
    builder.sizeHint(length)
    var dropping: ZIO[R, E, Boolean] = UIO.succeedNow(true)
    val iterator                     = arrayIterator
    while (iterator.hasNext) {
      val array  = iterator.next()
      val length = array.length
      var i      = 0
      while (i < length) {
        val j = i
        dropping = dropping.flatMap { d =>
          val a = array(j)
          (if (d) p(a) else UIO(false)).map {
            case true =>
              true
            case false =>
              builder += a
              false
          }
        }
        i += 1
      }
    }
    dropping as builder.result()
  }

  override final def equals(that: Any): Boolean =
    that match {
      case that: Chunk[_] =>
        if (self.length != that.length) false
        else {
          val leftIterator    = self.arrayIterator
          val rightIterator   = that.arrayIterator
          var left: Array[A]  = null
          var right: Array[_] = null
          var leftLength      = 0
          var rightLength     = 0
          var i               = 0
          var j               = 0
          var equal           = true
          var done            = false
          while (equal && !done) {
            if (i < leftLength && j < rightLength) {
              val a1 = left(i)
              val a2 = right(j)
              if (a1 != a2) {
                equal = false
              }
              i += 1
              j += 1
            } else if (i == leftLength && leftIterator.hasNext) {
              left = leftIterator.next()
              leftLength = left.length
              i = 0
            } else if (j == rightLength && rightIterator.hasNext) {
              right = rightIterator.next()
              rightLength = right.length
              j = 0
            } else if (i == leftLength && j == rightLength) {
              done = true
            } else {
              equal = false
            }
          }
          equal
        }
      case that: Seq[_] =>
        self.corresponds(that)(_ == _)
      case _ => false
    }

  /**
   * Determines whether a predicate is satisfied for at least one element of this chunk.
   */
  override final def exists(f: A => Boolean): Boolean = {
    val iterator = arrayIterator
    var exists   = false
    while (!exists && iterator.hasNext) {
      val array  = iterator.next()
      val length = array.length
      var i      = 0
      while (!exists && i < length) {
        val a = array(i)
        exists = f(a)
        i += 1
      }
    }
    exists
  }

  /**
   * Returns a filtered subset of this chunk.
   */
  override def filter(f: A => Boolean): Chunk[A] = {
    val iterator = arrayIterator
    val builder  = ChunkBuilder.make[A]()
    builder.sizeHint(length)
    while (iterator.hasNext) {
      val array  = iterator.next()
      val length = array.length
      var i      = 0
      while (i < length) {
        val a = array(i)
        if (f(a)) {
          builder += a
        }
        i += 1
      }
    }
    builder.result()
  }

  /**
   * Filters this chunk by the specified effectful predicate, retaining all elements for
   * which the predicate evaluates to true.
   */
  final def filterM[R, E](f: A => ZIO[R, E, Boolean]): ZIO[R, E, Chunk[A]] = ZIO.effectSuspendTotal {
    val iterator = arrayIterator
    val builder  = ChunkBuilder.make[A]()
    builder.sizeHint(length)
    var dest: ZIO[R, E, ChunkBuilder[A]] = IO.succeedNow(builder)
    while (iterator.hasNext) {
      val array  = iterator.next()
      val length = array.length
      var i      = 0
      while (i < length) {
        val a = array(i)
        dest = dest.zipWith(f(a)) { case (builder, res) =>
          if (res) builder += a else builder
        }
        i += 1
      }
    }
    dest.map(_.result())
  }

  /**
   * Returns the first element that satisfies the predicate.
   */
  override final def find(f: A => Boolean): Option[A] = {
    val iterator          = arrayIterator
    var result: Option[A] = None
    while (result.isEmpty && iterator.hasNext) {
      val array  = iterator.next()
      val length = array.length
      var i      = 0
      while (result.isEmpty && i < length) {
        val a = array(i)
        if (f(a)) {
          result = Some(a)
        }
        i += 1
      }
    }
    result
  }

  /**
   * Flattens a chunk of chunks into a single chunk by concatenating all chunks.
   */
  final def flatten[B](implicit ev: A <:< Chunk[B]): Chunk[B] =
    flatMap(ev(_))

  /**
   * Get the element at the specified index.
   */
  def float(index: Int)(implicit ev: A <:< Float): Float =
    ev(apply(index))

  /**
   * Folds over the elements in this chunk from the left.
   */
  override def foldLeft[S](s0: S)(f: (S, A) => S): S = {
    val iterator = arrayIterator
    var s        = s0
    while (iterator.hasNext) {
      val array  = iterator.next()
      val length = array.length
      var i      = 0
      while (i < length) {
        val a = array(i)
        s = f(s, a)
        i += 1
      }
    }
    s
  }

  /**
   * Effectfully folds over the elements in this chunk from the left.
   */
  final def foldM[R, E, S](s: S)(f: (S, A) => ZIO[R, E, S]): ZIO[R, E, S] =
    foldLeft[ZIO[R, E, S]](IO.succeedNow(s))((s, a) => s.flatMap(f(_, a)))

  /**
   * Folds over the elements in this chunk from the right.
   */
  override def foldRight[S](s0: S)(f: (A, S) => S): S = {
    val iterator = reverseArrayIterator
    var s        = s0
    while (iterator.hasNext) {
      val array  = iterator.next()
      val length = array.length
      var i      = length - 1
      while (i >= 0) {
        val a = array(i)
        s = f(a, s)
        i -= 1
      }
    }
    s
  }

  /**
   * Folds over the elements in this chunk from the left.
   * Stops the fold early when the condition is not fulfilled.
   */
  final def foldWhile[S](s0: S)(pred: S => Boolean)(f: (S, A) => S): S = {
    val iterator = arrayIterator
    var s        = s0
    var continue = pred(s)
    while (continue && iterator.hasNext) {
      val array  = iterator.next()
      val length = array.length
      var i      = 0
      while (continue && i < length) {
        val a = array(i)
        s = f(s, a)
        continue = pred(s)
        i += 1
      }
    }
    s
  }

  final def foldWhileM[R, E, S](z: S)(pred: S => Boolean)(f: (S, A) => ZIO[R, E, S]): ZIO[R, E, S] = {
    val iterator = arrayIterator

    def loop(s: S, iterator: Iterator[Array[A]], array: Array[A], i: Int, length: Int): ZIO[R, E, S] =
      if (i < length) {
        if (pred(s)) f(s, array(i)).flatMap(loop(_, iterator, array, i + 1, length))
        else IO.succeedNow(s)
      } else if (iterator.hasNext) {
        val array  = iterator.next()
        val length = array.length
        loop(s, iterator, array, 0, length)
      } else {
        ZIO.succeedNow(s)
      }

    if (iterator.hasNext) {
      val array  = iterator.next()
      val length = array.length
      loop(z, iterator, array, 0, length)
    } else {
      ZIO.succeedNow(z)
    }
  }

  /**
   * Determines whether a predicate is satisfied for all elements of this chunk.
   */
  override final def forall(f: A => Boolean): Boolean = {
    val iterator = arrayIterator
    var exists   = true
    while (exists && iterator.hasNext) {
      val array  = iterator.next()
      val length = array.length
      var i      = 0
      while (exists && i < length) {
        val a = array(i)
        exists = f(a)
        i += 1
      }
    }
    exists
  }

  override final def hashCode: Int = toArrayOption match {
    case None        => Seq.empty[A].hashCode
    case Some(array) => array.toSeq.hashCode
  }

  /**
   * Returns the first element of this chunk. Note that this method is partial
   * in that it will throw an exception if the chunk is empty. Consider using
   * `headOption` to explicitly handle the possibility that the chunk is empty
   * or iterating over the elements of the chunk in lower level, performance
   * sensitive code unless you really only need the first element of the chunk.
   */
  override def head: A =
    self(0)

  /**
   * Returns the first element of this chunk if it exists.
   */
  override final def headOption: Option[A] =
    if (isEmpty) None else Some(self(0))

  /**
   * Returns the first index for which the given predicate is satisfied after or at some given index.
   */
  override final def indexWhere(f: A => Boolean, from: Int): Int = {
    val iterator = arrayIterator
    var i        = 0
    var result   = -1
    while (result < 0 && iterator.hasNext) {
      val array  = iterator.next()
      val length = array.length
      var j      = 0
      while (result < 0 & j < length) {
        if (i >= from) {
          val a = array(j)
          if (f(a)) {
            result = i
          }
        }
        i += 1
        j += 1
      }

    }
    result
  }

  /**
   * Get the element at the specified index.
   */
  def int(index: Int)(implicit ev: A <:< Int): Int =
    ev(apply(index))

  /**
   * Determines if the chunk is empty.
   */
  override final def isEmpty: Boolean =
    length == 0

  /**
   * Returns the last element of this chunk if it exists.
   */
  override final def lastOption: Option[A] =
    if (isEmpty) None else Some(self(self.length - 1))

  /**
   * Get the element at the specified index.
   */
  def long(index: Int)(implicit ev: A <:< Long): Long =
    ev(apply(index))

  /**
   * Statefully maps over the chunk, producing new elements of type `B`.
   */
  final def mapAccum[S1, B](s1: S1)(f1: (S1, A) => (S1, B)): (S1, Chunk[B]) = {
    val iterator = arrayIterator
    val builder  = ChunkBuilder.make[B]()
    builder.sizeHint(length)
    var s = s1
    while (iterator.hasNext) {
      val array  = iterator.next()
      val length = array.length
      var i      = 0
      while (i < length) {
        val a     = array(i)
        val tuple = f1(s, a)
        s = tuple._1
        builder += tuple._2
        i += 1
      }
    }
    (s, builder.result())
  }

  /**
   * Statefully and effectfully maps over the elements of this chunk to produce
   * new elements.
   */
  final def mapAccumM[R, E, S1, B](s1: S1)(f1: (S1, A) => ZIO[R, E, (S1, B)]): ZIO[R, E, (S1, Chunk[B])] =
    ZIO.effectSuspendTotal {
      val iterator = arrayIterator
      val builder  = ChunkBuilder.make[B]()
      builder.sizeHint(length)
      var dest: ZIO[R, E, S1] = UIO.succeedNow(s1)
      while (iterator.hasNext) {
        val array  = iterator.next()
        val length = array.length
        var i      = 0
        while (i < length) {
          val a = array(i)
          dest = dest.flatMap { state =>
            f1(state, a).map { case (state2, b) =>
              builder += b
              state2
            }
          }
          i += 1
        }
      }
      dest.map((_, builder.result()))
    }

  /**
   * Effectfully maps the elements of this chunk.
   */
  final def mapM[R, E, B](f: A => ZIO[R, E, B]): ZIO[R, E, Chunk[B]] =
    ZIO.foreach(self)(f)

  /**
   * Effectfully maps the elements of this chunk in parallel.
   */
  final def mapMPar[R, E, B](f: A => ZIO[R, E, B]): ZIO[R, E, Chunk[B]] =
    ZIO.foreachPar(self)(f)

  /**
   * Effectfully maps the elements of this chunk in parallel purely for the effects.
   */
  final def mapMPar_[R, E](f: A => ZIO[R, E, Any]): ZIO[R, E, Unit] =
    ZIO.foreachPar_(self)(f)

  /**
   * Effectfully maps the elements of this chunk purely for the effects.
   */
  final def mapM_[R, E](f: A => ZIO[R, E, Any]): ZIO[R, E, Unit] =
    ZIO.foreach_(self)(f)

  /**
   * Materializes a chunk into a chunk backed by an array. This method can
   * improve the performance of bulk operations.
   */
  def materialize[A1 >: A]: Chunk[A1] =
    self.toArrayOption[A1] match {
      case None        => Chunk.Empty
      case Some(array) => Chunk.fromArray(array)
    }

  /**
   * Runs `fn` if a `chunk` is not empty or returns default value
   */
  def nonEmptyOrElse[B](ifEmpty: => B)(fn: NonEmptyChunk[A] => B): B =
    if (isEmpty) ifEmpty else fn(NonEmptyChunk.nonEmpty(self))

  /**
   * Partitions the elements of this chunk into two chunks using the specified
   * function.
   */
  override final def partitionMap[B, C](f: A => Either[B, C]): (Chunk[B], Chunk[C]) = {
    val bs = ChunkBuilder.make[B]()
    val cs = ChunkBuilder.make[C]()
    foreach { a =>
      f(a) match {
        case Left(b)  => bs += b
        case Right(c) => cs += c
      }
    }
    (bs.result(), cs.result())
  }

  /**
   * Get the element at the specified index.
   */
  def short(index: Int)(implicit ev: A <:< Short): Short =
    ev(apply(index))

  /**
   * Splits this chunk into `n` equally sized chunks.
   */
  final def split(n: Int): Chunk[Chunk[A]] = {
    val length    = self.length
    val quotient  = length / n
    val remainder = length % n
    val iterator  = self.iterator
    val chunks    = ChunkBuilder.make[Chunk[A]]()
    var i         = 0
    while (i < remainder) {
      val chunk = ChunkBuilder.make[A]()
      var j     = 0
      while (j <= quotient) {
        chunk += iterator.next()
        j += 1
      }
      chunks += chunk.result()
      i += 1
    }
    if (quotient > 0) {
      while (i < n) {
        val chunk = ChunkBuilder.make[A]()
        var j     = 0
        while (j < quotient) {
          chunk += iterator.next()
          j += 1
        }
        chunks += chunk.result()
        i += 1
      }
    }
    chunks.result()
  }

  /**
   * Returns two splits of this chunk at the specified index.
   */
  override final def splitAt(n: Int): (Chunk[A], Chunk[A]) =
    (take(n), drop(n))

  /**
   * Splits this chunk on the first element that matches this predicate.
   */
  final def splitWhere(f: A => Boolean): (Chunk[A], Chunk[A]) = {
    val iterator = arrayIterator
    var continue = true
    var i        = 0
    while (continue && iterator.hasNext) {
      val array  = iterator.next()
      val length = array.length
      var j      = 0
      while (continue && j < length) {
        val a = array(j)
        if (f(a)) {
          continue = false
        } else {
          i += 1
          j += 1
        }
      }
    }
    splitAt(i)
  }

  /**
   * Takes the first `n` elements of the chunk.
   */
  override def take(n: Int): Chunk[A] =
    if (n <= 0) Chunk.Empty
    else if (n >= length) this
    else
      self match {
        case Chunk.Empty => Chunk.Empty
        case Chunk.Slice(c, o, l) =>
          if (n >= l) this
          else Chunk.Slice(c, o, n)
        case c @ Chunk.Singleton(_) => c
        case _                      => Chunk.Slice(self, 0, n)
      }

  /**
   * Takes all elements so long as the predicate returns true.
   */
  override def takeWhile(f: A => Boolean): Chunk[A] = {
    val iterator = arrayIterator
    var continue = true
    var i        = 0
    while (continue && iterator.hasNext) {
      val array  = iterator.next()
      val length = array.length
      var j      = 0
      while (continue && j < length) {
        val a = array(j)
        if (!f(a)) {
          continue = false
        } else {
          i += 1
          j += 1
        }
      }
    }
    take(i)
  }

  /**
   * Takes all elements so long as the effectual predicate returns true.
   */
  def takeWhileM[R, E](p: A => ZIO[R, E, Boolean]): ZIO[R, E, Chunk[A]] =
    ZIO.effectSuspendTotal {
      val length  = self.length
      val builder = ChunkBuilder.make[A]()
      builder.sizeHint(length)
      var taking: ZIO[R, E, Boolean] = UIO.succeedNow(true)
      val iterator                   = arrayIterator
      while (iterator.hasNext) {
        val array  = iterator.next()
        val length = array.length
        var i      = 0
        while (i < length) {
          val j = i
          taking = taking.flatMap { b =>
            val a = array(j)
            (if (b) p(a) else UIO(false)).map {
              case true =>
                builder += a
                true
              case false =>
                false
            }
          }
          i += 1
        }
      }
      taking as builder.result()
    }

  /**
   * Converts the chunk into an array.
   */
  override def toArray[A1 >: A: ClassTag]: Array[A1] = {
    val dest = Array.ofDim[A1](self.length)

    self.toArray(0, dest)

    dest
  }

  /**
   * Renders this chunk of bits as a binary string.
   */
  final def toBinaryString(implicit ev: A <:< Boolean): String = {
    val bits    = self.asInstanceOf[Chunk[Boolean]]
    val builder = new scala.collection.mutable.StringBuilder
    bits.foreach(bit => if (bit) builder.append("1") else builder.append("0"))
    builder.toString
  }

  override final def toList: List[A] = {
    val listBuilder = List.newBuilder[A]
    fromBuilder(listBuilder)
  }

  override final def toVector: Vector[A] = {
    val vectorBuilder = Vector.newBuilder[A]
    fromBuilder(vectorBuilder)
  }

  override final def toString: String =
    toArrayOption.fold("Chunk()")(_.mkString("Chunk(", ",", ")"))

  /**
   * Zips this chunk with the specified chunk to produce a new chunk with
   * pairs of elements from each chunk. The returned chunk will have the
   * length of the shorter chunk.
   */
  final def zip[B](that: Chunk[B]): Chunk[(A, B)] =
    zipWith(that)((_, _))

  /**
   * Zips this chunk with the specified chunk to produce a new chunk with
   * pairs of elements from each chunk, filling in missing values from the
   * shorter chunk with `None`. The returned chunk will have the length of the
   * longer chunk.
   */
  final def zipAll[B](that: Chunk[B]): Chunk[(Option[A], Option[B])] =
    zipAllWith(that)(a => (Some(a), None), b => (None, Some(b)))((a, b) => (Some(a), Some(b)))

  /**
   * Zips with chunk with the specified chunk to produce a new chunk with
   * pairs of elements from each chunk combined using the specified function
   * `both`. If one chunk is shorter than the other uses the specified
   * function `left` or `right` to map the element that does exist to the
   * result type.
   */
  final def zipAllWith[B, C](
    that: Chunk[B]
  )(left: A => C, right: B => C)(both: (A, B) => C): Chunk[C] = {
    val length = self.length.max(that.length)
    if (length == 0) Chunk.empty
    else {
      val leftIterator  = self.arrayIterator
      val rightIterator = that.arrayIterator
      val builder       = ChunkBuilder.make[C]()
      builder.sizeHint(length)
      var leftArray: Array[A]  = null
      var rightArray: Array[B] = null
      var leftLength           = 0
      var rightLength          = 0
      var i                    = 0
      var j                    = 0
      var k                    = 0
      while (i < length) {
        if (j < leftLength && k < rightLength) {
          val a = leftArray(j)
          val b = rightArray(k)
          val c = both(a, b)
          builder += c
          i += 1
          j += 1
          k += 1
        } else if (j == leftLength && leftIterator.hasNext) {
          leftArray = leftIterator.next()
          leftLength = leftArray.length
          j = 0
        } else if (k == rightLength && rightIterator.hasNext) {
          rightArray = rightIterator.next()
          rightLength = rightArray.length
          k = 0
        } else if (j < leftLength) {
          val a = leftArray(j)
          val c = left(a)
          builder += c
          i += 1
          j += 1
        } else if (k < rightLength) {
          val b = rightArray(k)
          val c = right(b)
          builder += c
          i += 1
          k += 1
        }
      }
      builder.result()
    }
  }

  /**
   * Zips this chunk with the specified chunk using the specified combiner.
   */
  final def zipWith[B, C](that: Chunk[B])(f: (A, B) => C): Chunk[C] = {
    val length = self.length.min(that.length)
    if (length == 0) Chunk.empty
    else {
      val leftIterator  = self.arrayIterator
      val rightIterator = that.arrayIterator
      val builder       = ChunkBuilder.make[C]()
      builder.sizeHint(length)
      var left: Array[A]  = null
      var right: Array[B] = null
      var leftLength      = 0
      var rightLength     = 0
      var i               = 0
      var j               = 0
      var k               = 0
      while (i < length) {
        if (j < leftLength && k < rightLength) {
          val a = left(j)
          val b = right(k)
          val c = f(a, b)
          builder += c
          i += 1
          j += 1
          k += 1
        } else if (j == leftLength && leftIterator.hasNext) {
          left = leftIterator.next()
          leftLength = left.length
          j = 0
        } else if (k == rightLength && rightIterator.hasNext) {
          right = rightIterator.next()
          rightLength = right.length
          k = 0
        }
      }
      builder.result()
    }
  }

  /**
   * Zips this chunk with the index of every element, starting from the initial
   * index value.
   */
  final def zipWithIndexFrom(indexOffset: Int): Chunk[(A, Int)] = {
    val iterator = arrayIterator
    val builder  = ChunkBuilder.make[(A, Int)]()
    builder.sizeHint(length)
    var idx = indexOffset
    while (iterator.hasNext) {
      val array  = iterator.next()
      val length = array.length
      var i      = 0
      while (i < length) {
        val a = array(i)
        builder += ((a, idx))
        i += 1
        idx += 1
      }
    }
    builder.result()
  }

  //noinspection AccessorLikeMethodIsUnit
  protected[zio] def toArray[A1 >: A](n: Int, dest: Array[A1]): Unit =
    if (isEmpty) () else materialize.toArray(n, dest)

  /**
   * Appends an element to the chunk.
   */
  protected def append[A1 >: A](a1: A1): Chunk[A1] = {
    val buffer = Array.ofDim[AnyRef](Chunk.BufferSize)
    buffer(0) = a1.asInstanceOf[AnyRef]
    Chunk.AppendN(self, buffer, 1, new AtomicInteger(1))
  }

  /**
   * Returns an `Iterator` that iterates over the arrays underlying this
   * `Chunk`. Note that this method is side effecting because it allocates
   * mutable state and should only be used internally.
   */
  private[zio] def arrayIterator[A1 >: A]: Iterator[Array[A1]] =
    materialize.arrayIterator

  /**
   * Returns a filtered, mapped subset of the elements of this chunk.
   */
  protected def collectChunk[B](pf: PartialFunction[A, B]): Chunk[B] =
    if (isEmpty) Chunk.empty else self.materialize.collectChunk(pf)

  protected def depth: Int =
    0

  protected def left: Chunk[A] =
    Chunk.empty

  /**
   * Returns a chunk with the elements mapped by the specified function.
   */
  protected def mapChunk[B](f: A => B): Chunk[B] = {
    val iterator = self.arrayIterator
    val builder  = ChunkBuilder.make[B]()
    builder.sizeHint(length)
    while (iterator.hasNext) {
      val array  = iterator.next()
      val length = array.length
      var i      = 0
      while (i < length) {
        val a = array(i)
        val b = f(a)
        builder += b
        i += 1
      }
    }
    builder.result()
  }

  /**
   * Prepends an element to the chunk.
   */
  protected def prepend[A1 >: A](a1: A1): Chunk[A1] = {
    val buffer = Array.ofDim[AnyRef](Chunk.BufferSize)
    buffer(Chunk.BufferSize - 1) = a1.asInstanceOf[AnyRef]
    Chunk.PrependN(self, buffer, 1, new AtomicInteger(1))
  }

  /**
   * Returns an `Iterator` that iterates over the arrays underlying this
   * `Chunk` in reverse order. While the arrays will be iterated over in
   * reverse order the ordering of elements in the arrays themselves will not
   * be changed.  Note that this method is side effecting because it allocates
   * mutable state and should only be used internally.
   */
  private[zio] def reverseArrayIterator[A1 >: A]: Iterator[Array[A1]] =
    materialize.arrayIterator

  protected def right: Chunk[A] =
    Chunk.empty

  private final def fromBuilder[A1 >: A, B[_]](builder: Builder[A1, B[A1]]): B[A1] = {
    val c   = materialize
    var i   = 0
    val len = c.length
    builder.sizeHint(len)
    while (i < len) {
      builder += c(i)
      i += 1
    }
    builder.result()
  }

  /**
   * A helper function that converts the chunk into an array if it is not empty.
   */
  private final def toArrayOption[A1 >: A]: Option[Array[A1]] =
    self match {
      case Chunk.Empty => None
      case chunk       => Some(chunk.toArray(Chunk.classTagOf(self)))
    }
}

object Chunk extends ChunkFactory with ChunkPlatformSpecific {

  /**
   * Returns the empty chunk.
   */
  val empty: Chunk[Nothing] =
    Empty

  /**
   * Returns a chunk from a number of values.
   */
  override def apply[A](as: A*): Chunk[A] =
    fromIterable(as)

  /**
   * Returns a chunk backed by an array.
   *
   * WARNING: The array must not be mutated after creating the chunk.
   */
  def fromArray[A](array: Array[A]): Chunk[A] =
    (if (array.isEmpty) Empty
     else
       (array.asInstanceOf[AnyRef]: @unchecked) match {
         case x: Array[AnyRef]  => AnyRefArray(x)
         case x: Array[Int]     => IntArray(x)
         case x: Array[Double]  => DoubleArray(x)
         case x: Array[Long]    => LongArray(x)
         case x: Array[Float]   => FloatArray(x)
         case x: Array[Char]    => CharArray(x)
         case x: Array[Byte]    => ByteArray(x)
         case x: Array[Short]   => ShortArray(x)
         case x: Array[Boolean] => BooleanArray(x)
       }).asInstanceOf[Chunk[A]]

  /**
   * Returns a chunk backed by a [[java.nio.ByteBuffer]].
   */
  def fromByteBuffer(buffer: ByteBuffer): Chunk[Byte] = {
    val dest = Array.ofDim[Byte](buffer.remaining())
    val pos  = buffer.position()
    buffer.get(dest)
    buffer.position(pos)
    Chunk.fromArray(dest)
  }

  /**
   * Returns a chunk backed by a [[java.nio.CharBuffer]].
   */
  def fromCharBuffer(buffer: CharBuffer): Chunk[Char] = {
    val dest = Array.ofDim[Char](buffer.remaining())
    val pos  = buffer.position()
    buffer.get(dest)
    buffer.position(pos)
    Chunk.fromArray(dest)
  }

  /**
   * Returns a chunk backed by a [[java.nio.DoubleBuffer]].
   */
  def fromDoubleBuffer(buffer: DoubleBuffer): Chunk[Double] = {
    val dest = Array.ofDim[Double](buffer.remaining())
    val pos  = buffer.position()
    buffer.get(dest)
    buffer.position(pos)
    Chunk.fromArray(dest)
  }

  /**
   * Returns a chunk backed by a [[java.nio.FloatBuffer]].
   */
  def fromFloatBuffer(buffer: FloatBuffer): Chunk[Float] = {
    val dest = Array.ofDim[Float](buffer.remaining())
    val pos  = buffer.position()
    buffer.get(dest)
    buffer.position(pos)
    Chunk.fromArray(dest)
  }

  /**
   * Returns a chunk backed by a [[java.nio.IntBuffer]].
   */
  def fromIntBuffer(buffer: IntBuffer): Chunk[Int] = {
    val dest = Array.ofDim[Int](buffer.remaining())
    val pos  = buffer.position()
    buffer.get(dest)
    buffer.position(pos)
    Chunk.fromArray(dest)
  }

  /**
   * Returns a chunk backed by a [[java.nio.LongBuffer]].
   */
  def fromLongBuffer(buffer: LongBuffer): Chunk[Long] = {
    val dest = Array.ofDim[Long](buffer.remaining())
    val pos  = buffer.position()
    buffer.get(dest)
    buffer.position(pos)
    Chunk.fromArray(dest)
  }

  /**
   * Returns a chunk backed by a [[java.nio.ShortBuffer]].
   */
  def fromShortBuffer(buffer: ShortBuffer): Chunk[Short] = {
    val dest = Array.ofDim[Short](buffer.remaining())
    val pos  = buffer.position()
    buffer.get(dest)
    buffer.position(pos)
    Chunk.fromArray(dest)
  }

  /**
   * Returns a chunk backed by an iterable.
   */
  def fromIterable[A](it: Iterable[A]): Chunk[A] =
    it match {
      case chunk: Chunk[A]              => chunk
      case iterable if iterable.isEmpty => Empty
      case vector: Vector[A]            => VectorChunk(vector)
      case iterable =>
        val builder = ChunkBuilder.make[A]()
        builder.sizeHint(iterable.size)
        builder ++= iterable
        builder.result()
    }

  /**
   * Creates a chunk from an iterator.
   */
  def fromIterator[A](iterator: Iterator[A]): Chunk[A] = {
    val builder = ChunkBuilder.make[A]()
    builder ++= iterator
    builder.result()
  }

  override def fill[A](n: Int)(elem: => A): Chunk[A] =
    if (n <= 0) Chunk.empty
    else {
      val builder = ChunkBuilder.make[A]()
      builder.sizeHint(n)

      var i = 0
      while (i < n) {
        builder += elem
        i += 1
      }
      builder.result()
    }

  /**
   * Returns a singleton chunk, eagerly evaluated.
   */
  def single[A](a: A): Chunk[A] =
    Singleton(a)

  /**
   * Alias for [[Chunk.single]].
   */
  def succeed[A](a: A): Chunk[A] =
    single(a)

  /**
   * Constructs a `Chunk` by repeatedly applying the function `f` as long as it
   * returns `Some`.
   */
  def unfold[S, A](s: S)(f: S => Option[(A, S)]): Chunk[A] = {

    @tailrec
    def go(s: S, builder: ChunkBuilder[A]): Chunk[A] =
      f(s) match {
        case Some((a, s)) => go(s, builder += a)
        case None         => builder.result()
      }

    go(s, ChunkBuilder.make[A]())
  }

  /**
   * Constructs a `Chunk` by repeatedly applying the effectual function `f` as
   * long as it returns `Some`.
   */
  def unfoldM[R, E, A, S](s: S)(f: S => ZIO[R, E, Option[(A, S)]]): ZIO[R, E, Chunk[A]] =
    ZIO.effectSuspendTotal {

      def go(s: S, builder: ChunkBuilder[A]): ZIO[R, E, Chunk[A]] =
        f(s).flatMap {
          case Some((a, s)) => go(s, builder += a)
          case None         => ZIO.succeedNow(builder.result())
        }

      go(s, ChunkBuilder.make[A]())
    }

  /**
   * The unit chunk
   */
  val unit: Chunk[Unit] = single(())

  /**
   * Returns the `ClassTag` for the element type of the chunk.
   */
  private[zio] def classTagOf[A](chunk: Chunk[A]): ClassTag[A] =
    chunk match {
      case x: AppendN[A]     => x.classTag
      case x: Arr[A]         => x.classTag
      case x: Concat[A]      => x.classTag
      case Empty             => classTag[java.lang.Object].asInstanceOf[ClassTag[A]]
      case x: PrependN[A]    => x.classTag
      case x: Singleton[A]   => x.classTag
      case x: Slice[A]       => x.classTag
      case x: VectorChunk[A] => x.classTag
      case _: BitChunk       => ClassTag.Boolean.asInstanceOf[ClassTag[A]]
    }

  /**
   * The maximum number of elements in the buffer for fast append.
   */
  private val BufferSize: Int =
    64

  private final case class AppendN[A](start: Chunk[A], buffer: Array[AnyRef], bufferUsed: Int, chain: AtomicInteger)
      extends Chunk[A] { self =>

    implicit val classTag: ClassTag[A] = classTagOf(start)

    val length: Int =
      start.length + bufferUsed

    override protected def append[A1 >: A](a1: A1): Chunk[A1] =
      if (bufferUsed < buffer.length && chain.compareAndSet(bufferUsed, bufferUsed + 1)) {
        buffer(bufferUsed) = a1.asInstanceOf[AnyRef]
        AppendN(start, buffer, bufferUsed + 1, chain)
      } else {
        val buffer = Array.ofDim[AnyRef](BufferSize)
        buffer(0) = a1.asInstanceOf[AnyRef]
        val chunk = Chunk.fromArray(self.buffer.asInstanceOf[Array[A1]]).take(bufferUsed)
        AppendN(start ++ chunk, buffer, 1, new AtomicInteger(1))
      }

    def apply(n: Int): A =
      if (n < start.length) start(n) else buffer(n - start.length).asInstanceOf[A]

    override protected[zio] def toArray[A1 >: A](n: Int, dest: Array[A1]): Unit = {
      start.toArray(n, dest)
      val _ = buffer.asInstanceOf[Array[A]].copyToArray(dest, n + start.length, bufferUsed)
    }
  }

  private final case class PrependN[A](end: Chunk[A], buffer: Array[AnyRef], bufferUsed: Int, chain: AtomicInteger)
      extends Chunk[A] { self =>

    implicit val classTag: ClassTag[A] = classTagOf(end)

    val length: Int =
      end.length + bufferUsed

    override protected def prepend[A1 >: A](a1: A1): Chunk[A1] =
      if (bufferUsed < buffer.length && chain.compareAndSet(bufferUsed, bufferUsed + 1)) {
        buffer(BufferSize - bufferUsed - 1) = a1.asInstanceOf[AnyRef]
        PrependN(end, buffer, bufferUsed + 1, chain)
      } else {
        val buffer = Array.ofDim[AnyRef](BufferSize)
        buffer(BufferSize - 1) = a1.asInstanceOf[AnyRef]
        val chunk = Chunk.fromArray(self.buffer.asInstanceOf[Array[A1]]).takeRight(bufferUsed)
        PrependN(chunk ++ end, buffer, 1, new AtomicInteger(1))
      }

    def apply(n: Int): A =
      if (n < bufferUsed) buffer(BufferSize - bufferUsed + n).asInstanceOf[A] else end(n - bufferUsed)

    override protected[zio] def toArray[A1 >: A](n: Int, dest: Array[A1]): Unit = {
      val length = math.min(bufferUsed, math.max(dest.length - n, 0))
      Array.copy(buffer, BufferSize - bufferUsed, dest, n, length)
      val _ = end.toArray(n + length, dest)
    }
  }

  private[zio] sealed abstract class Arr[A] extends Chunk[A] with Serializable { self =>

    val array: Array[A]

    implicit val classTag: ClassTag[A] =
      ClassTag(array.getClass.getComponentType)

    override val length: Int =
      array.length

    override def apply(n: Int): A =
      array(n)

    override def collectM[R, E, B](pf: PartialFunction[A, ZIO[R, E, B]]): ZIO[R, E, Chunk[B]] = ZIO.effectSuspendTotal {
      val len     = array.length
      val builder = ChunkBuilder.make[B]()
      builder.sizeHint(len)
      val orElse                           = (_: A) => UIO.succeedNow(null.asInstanceOf[B])
      var dest: ZIO[R, E, ChunkBuilder[B]] = UIO.succeedNow(builder)

      var i = 0
      while (i < len) {
        // `zipWith` is lazy in the RHS, so we need to capture to evaluate the
        // `pf.applyOrElse` strictly to make sure we use the right value of `i`.
        val rhs = pf.applyOrElse(array(i), orElse)

        dest = dest.zipWith(rhs)((builder, b) => if (b != null) (builder += b) else builder)

        i += 1
      }

      dest.map(_.result())
    }

    override def collectWhile[B](pf: PartialFunction[A, B]): Chunk[B] = {
      val self    = array
      val len     = self.length
      val builder = ChunkBuilder.make[B]()
      builder.sizeHint(len)

      var i    = 0
      var done = false
      while (!done && i < len) {
        val b = pf.applyOrElse(self(i), (_: A) => null.asInstanceOf[B])

        if (b != null) {
          builder += b
        } else {
          done = true
        }

        i += 1
      }

      builder.result()
    }

    override def collectWhileM[R, E, B](pf: PartialFunction[A, ZIO[R, E, B]]): ZIO[R, E, Chunk[B]] =
      ZIO.effectSuspendTotal {
        val len     = self.length
        val builder = ChunkBuilder.make[B]()
        builder.sizeHint(len)
        var dest: ZIO[R, E, ChunkBuilder[B]] = IO.succeedNow(builder)

        var i    = 0
        var done = false
        val orElse = (_: A) => {
          done = true
          UIO.succeedNow(null.asInstanceOf[B])
        }

        while (!done && i < len) {
          val j = i
          // `zipWith` is lazy in the RHS, and we rely on the side-effects of `orElse` here.
          val rhs = pf.applyOrElse(self(j), orElse)
          dest = dest.zipWith(rhs) { case (builder, b) =>
            if (b != null) (builder += b) else builder
          }
          i += 1
        }

        dest.map(_.result())
      }

    override def dropWhile(f: A => Boolean): Chunk[A] = {
      val self = array
      val len  = self.length

      var i = 0
      while (i < len && f(self(i))) {
        i += 1
      }

      drop(i)
    }

    override def filter(f: A => Boolean): Chunk[A] = {
      val len     = self.length
      val builder = ChunkBuilder.make[A]()
      builder.sizeHint(len)

      var i = 0
      while (i < len) {
        val elem = self(i)

        if (f(elem)) {
          builder += elem
        }

        i += 1
      }

      builder.result()
    }

    override def foldLeft[S](s0: S)(f: (S, A) => S): S = {
      val len = self.length
      var s   = s0

      var i = 0
      while (i < len) {
        s = f(s, self(i))
        i += 1
      }

      s
    }

    override def foldRight[S](s0: S)(f: (A, S) => S): S = {
      val self = array
      val len  = self.length
      var s    = s0

      var i = len - 1
      while (i >= 0) {
        s = f(self(i), s)
        i -= 1
      }

      s
    }

    override def foreach[B](f: A => B): Unit =
      array.foreach(f)

    override def materialize[A1 >: A]: Chunk[A1] =
      self

    /**
     * Takes all elements so long as the predicate returns true.
     */
    override def takeWhile(f: A => Boolean): Chunk[A] = {
      val self = array
      val len  = length

      var i = 0
      while (i < len && f(self(i))) {
        i += 1
      }

      take(i)
    }

    override protected[zio] def toArray[A1 >: A](n: Int, dest: Array[A1]): Unit =
      Array.copy(array, 0, dest, n, length)

    override private[zio] def arrayIterator[A1 >: A]: Iterator[Array[A1]] =
      Iterator.single(array.asInstanceOf[Array[A1]])

    override protected def collectChunk[B](pf: PartialFunction[A, B]): Chunk[B] = {
      val len     = self.length
      val builder = ChunkBuilder.make[B]()
      builder.sizeHint(len)

      var i = 0
      while (i < len) {
        val b = pf.applyOrElse(self(i), (_: A) => null.asInstanceOf[B])
        if (b != null) {
          builder += b
        }

        i += 1
      }
      builder.result()
    }

    override protected def mapChunk[B](f: A => B): Chunk[B] = {
      val len     = self.length
      val builder = ChunkBuilder.make[B]()
      builder.sizeHint(len)

      var i = 0
      while (i < len) {
        builder += f(self(i))
        i += 1
      }

      builder.result()
    }

    override private[zio] def reverseArrayIterator[A1 >: A]: Iterator[Array[A1]] =
      Iterator.single(array.asInstanceOf[Array[A1]])

    /**
     * Generates a readable string representation of this chunk.
     */
    override def mkString: String =
      array.asInstanceOf[AnyRef] match {
        case a: Array[Char] =>
          new String(a)
        case _ =>
          mkString("", "", "")
      }
  }

  private final case class Concat[A](override protected val left: Chunk[A], override protected val right: Chunk[A])
      extends Chunk[A] {
    self =>

    implicit val classTag: ClassTag[A] =
      left match {
        case Empty => classTagOf(right)
        case _     => classTagOf(left)
      }

    override val depth: Int =
      1 + math.max(left.depth, right.depth)

    override val length: Int =
      left.length + right.length

    override def apply(n: Int): A =
      if (n < left.length) left(n) else right(n - left.length)

    override def foreach[B](f: A => B): Unit = {
      left.foreach(f)
      right.foreach(f)
    }

    override def toArray[A1 >: A](n: Int, dest: Array[A1]): Unit = {
      left.toArray(n, dest)
      right.toArray(n + left.length, dest)
    }

    override private[zio] def arrayIterator[A1 >: A]: Iterator[Array[A1]] =
      (left.arrayIterator ++ right.arrayIterator).asInstanceOf[Iterator[Array[A1]]]

    override private[zio] def reverseArrayIterator[A1 >: A]: Iterator[Array[A1]] =
      (right.reverseArrayIterator ++ left.reverseArrayIterator).asInstanceOf[Iterator[Array[A1]]]
  }

  private final case class Singleton[A](a: A) extends Chunk[A] {

    implicit val classTag: ClassTag[A] =
      Tags.fromValue(a)

    override val length =
      1

    override def apply(n: Int): A =
      if (n == 0) a
      else throw new ArrayIndexOutOfBoundsException(s"Singleton chunk access to $n")

    override def foreach[B](f: A => B): Unit = {
      val _ = f(a)
    }

    override def toArray[A1 >: A](n: Int, dest: Array[A1]): Unit =
      dest(n) = a

    private[zio] override def arrayIterator[A1 >: A]: Iterator[Array[A1]] =
      Iterator.single(Array(a).asInstanceOf[Array[A1]])

    private[zio] override def reverseArrayIterator[A1 >: A]: Iterator[Array[A1]] =
      arrayIterator
  }

  private final case class Slice[A](private val chunk: Chunk[A], offset: Int, l: Int) extends Chunk[A] {

    implicit val classTag: ClassTag[A] =
      classTagOf(chunk)

    override val length: Int =
      l

    override def apply(n: Int): A =
      chunk.apply(offset + n)

    override def foreach[B](f: A => B): Unit = {
      var i = 0
      while (i < length) {
        f(apply(i))
        i += 1
      }
    }

    override def toArray[A1 >: A](n: Int, dest: Array[A1]): Unit = {
      var i = 0
      var j = n

      while (i < length) {
        dest(j) = apply(i)

        i += 1
        j += 1
      }
    }
  }

  private final case class VectorChunk[A](private val vector: Vector[A]) extends Chunk[A] {

    implicit val classTag: ClassTag[A] =
      Tags.fromValue(vector(0))

    override val length: Int =
      vector.length

    override def apply(n: Int): A =
      vector(n)

    override def foreach[B](f: A => B): Unit =
      vector.foreach(f)

    override def toArray[A1 >: A](n: Int, dest: Array[A1]): Unit = {
      val _ = vector.copyToArray(dest, n, length)
    }
  }

  private[zio] final case class BitChunk(bytes: Chunk[Byte], minBitIndex: Int, maxBitIndex: Int)
      extends Chunk[Boolean] {
    self =>

    override val length: Int =
      maxBitIndex - minBitIndex

    override def apply(n: Int): Boolean =
      (bytes(n >> 3) & (1 << (7 - (n & 7)))) != 0

    override def drop(n: Int): BitChunk = {
      val index  = (minBitIndex + n) min maxBitIndex
      val toDrop = index >> 3
      val min    = index & 7
      val max    = maxBitIndex - index + min
      BitChunk(bytes.drop(toDrop), min, max)
    }

    override def foreach[A](f: Boolean => A): Unit = {
      val minByteIndex    = (minBitIndex + 7) >> 3
      val maxByteIndex    = maxBitIndex >> 3
      val minFullBitIndex = (minByteIndex << 3) min maxBitIndex
      val maxFullBitIndex = (maxByteIndex << 3) max minFullBitIndex
      var i               = minBitIndex
      while (i < minFullBitIndex) {
        f(apply(i))
        i += 1
      }
      i = minByteIndex
      while (i < maxByteIndex) {
        val byte = bytes(i)
        f((byte & 128) != 0)
        f((byte & 64) != 0)
        f((byte & 32) != 0)
        f((byte & 16) != 0)
        f((byte & 8) != 0)
        f((byte & 4) != 0)
        f((byte & 2) != 0)
        f((byte & 1) != 0)
        i += 1
      }
      i = maxFullBitIndex
      while (i < maxBitIndex) {
        f(apply(i))
        i += 1
      }
    }

    override def take(n: Int): BitChunk = {
      val index  = (minBitIndex + n) min maxBitIndex
      val toTake = (index + 7) >> 3
      BitChunk(bytes.take(toTake), minBitIndex, index)
    }

    override def toArray[A1 >: Boolean](n: Int, dest: Array[A1]): Unit = {
      var i = n
      while (i < length) {
        dest(i + n) = apply(i)
        i += 1
      }
    }

    override private[zio] def arrayIterator[A1 >: Boolean]: Iterator[Array[A1]] = {
      val array = Array.ofDim[Boolean](length)
      toArray(0, array)
      Iterator.single(array.asInstanceOf[Array[A1]])
    }

    override private[zio] def reverseArrayIterator[A1 >: Boolean]: Iterator[Array[A1]] =
      arrayIterator

  }

  private case object Empty extends Chunk[Nothing] { self =>

    override val length: Int =
      0

    override def apply(n: Int): Nothing =
      throw new ArrayIndexOutOfBoundsException(s"Empty chunk access to $n")

    override def foreach[B](f: Nothing => B): Unit = {
      val _ = f
    }

    /**
     * Materializes a chunk into a chunk backed by an array. This method can
     * improve the performance of bulk operations.
     */
    override def materialize[A1]: Chunk[A1] =
      Empty

    override def toArray[A1: ClassTag]: Array[A1] =
      Array.empty

    override private[zio] def arrayIterator[A]: Iterator[Array[A]] =
      Iterator.empty

    override private[zio] def reverseArrayIterator[A]: Iterator[Array[A]] =
      Iterator.empty
  }

  final case class AnyRefArray[A <: AnyRef](array: Array[A]) extends Arr[A]

  final case class ByteArray(array: Array[Byte]) extends Arr[Byte] {
    override def byte(index: Int)(implicit ev: Byte <:< Byte): Byte = array(index)
  }

  final case class CharArray(array: Array[Char]) extends Arr[Char] {
    override def char(index: Int)(implicit ev: Char <:< Char): Char = array(index)
  }

  final case class IntArray(array: Array[Int]) extends Arr[Int] {
    override def int(index: Int)(implicit ev: Int <:< Int): Int = array(index)
  }

  final case class LongArray(array: Array[Long]) extends Arr[Long] {
    override def long(index: Int)(implicit ev: Long <:< Long): Long = array(index)
  }

  final case class DoubleArray(array: Array[Double]) extends Arr[Double] {
    override def double(index: Int)(implicit ev: Double <:< Double): Double = array(index)
  }

  final case class FloatArray(array: Array[Float]) extends Arr[Float] {
    override def float(index: Int)(implicit ev: Float <:< Float): Float = array(index)
  }

  final case class ShortArray(array: Array[Short]) extends Arr[Short] {
    override def short(index: Int)(implicit ev: Short <:< Short): Short = array(index)
  }

  final case class BooleanArray(array: Array[Boolean]) extends Arr[Boolean] {
    override def boolean(index: Int)(implicit ev: Boolean <:< Boolean): Boolean = array(index)
  }
}

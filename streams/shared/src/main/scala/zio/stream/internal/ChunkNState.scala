package zio.stream.internal

import zio.{ Chunk, ChunkBuilder }

/**
 * An auxiliary class for implementing [[ZStream#chunkN]].
 * Tries to avoid creating deeply nested chunk structures.
 */
private[zio] sealed trait ChunkNState[T]

private[zio] case class Accumulating[T](n: Int, buffer: ChunkBuilder[T], totalSize: Int) extends ChunkNState[T] {
  def append(chunk: Chunk[T]): (Option[Chunk[T]], ChunkNState[T]) = {
    val chunkSize = chunk.size
    if (chunkSize > 0) {
      if (chunkSize + totalSize >= n) {
        if (totalSize == 0) {
          (Some(chunk.take(n)), Serving(n, chunk, n, chunkSize))
        } else {
          val toAdd        = n - totalSize
          val leftoverSize = chunkSize - toAdd
          val emit = {
            buffer ++= chunk.take(toAdd)
            buffer.result()
          }
          val newState = if (leftoverSize < n) {
            buffer.clear()
            buffer ++= chunk.drop(toAdd)
            Accumulating(n, buffer, leftoverSize)
          } else {
            Serving(n, chunk, toAdd, chunkSize)
          }
          (Some(emit), newState)
        }
      } else {
        buffer ++= chunk
        (None, Accumulating(n, buffer, totalSize + chunkSize))
      }
    } else {
      (None, this)
    }
  }

  def drain(): Option[Chunk[T]] =
    if (totalSize > 0) Some(buffer.result()) else None

}

private[zio] case class Serving[T](n: Int, bigChunk: Chunk[T], offset: Int, size: Int) extends ChunkNState[T] {
  def tryTake(): (Option[Chunk[T]], ChunkNState[T]) = {
    val available = size - offset
    if (available >= n) {
      (Some(bigChunk.slice(offset, offset + n)), Serving(n, bigChunk, offset + n, size))
    } else {
      val buf = ChunkBuilder.make[T](n)
      buf ++= bigChunk.drop(offset)
      (None, Accumulating(n, buf, available))
    }
  }
}

private[zio] case class Done[T]() extends ChunkNState[T]

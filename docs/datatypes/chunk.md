---
id: datatypes_chunk
title:  "Chunk"
---
A `Chunk[A]` represents a chunk of values of type `A`. Chunks are designed are usually backed by arrays, but expose a purely functional, safe interface to the underlying elements, and they become lazy on operations that would be costly with arrays, such as repeated concatenation.

```scala mdoc
import zio._
```

## Creating a Chunk

```scala mdoc
Chunk(1,2,3)
```

## Concatenating chunk:

`++` Returns the concatenation of this chunk with the specified chunk. For example:

```scala mdoc
Chunk(1,2,3) ++ Chunk(4,5,6)
```

## Collecting chunk:

`collect` Returns a filtered, mapped subset of the elements of this chunk.
How to use `collect` function to cherry-pick all strings from Chunk[A]:

```scala mdoc
val collectChunk = Chunk("Hello ZIO", 1.5, "Hello ZIO NIO", 2.0, "Some string", 2.5)

collectChunk.collect { case string: String => string }
```
How to use `collect` function to cherry-pick all the digits from Chunk[A]:

```scala mdoc
collectChunk.collect { case digit: Double => digit }
```

`collectWhile` collects the elements (from left to right) until the predicate returns "false" for the first time:

```scala mdoc
Chunk("Sarah", "Bob", "Jane").collectWhile { case element if element != "Bob" => true }
```
or another example:

```scala mdoc
Chunk(9, 2, 5, 1, 6).collectWhile { case element if element >= 2 => true }
```
## Dropping chunk:

`drop` drops the first `n` elements of the chunk:

```scala mdoc
Chunk("Sarah", "Bob", "Jane").drop(1)
```

`dropWhile` drops all elements so long as the predicate returns true:

```scala mdoc
Chunk(9, 2, 5, 1, 6).dropWhile(_ >= 2)
```

## Comparing chunks:

```scala mdoc
Chunk("A","B") == Chunk("A", "C")
```

#Converting chunks

`toArray` converts the chunk into an Array.

```scala mdoc:silent
Chunk(1,2,3).toArray
```

`toSeq`converts the chunk into `Seq`.


``` scala mdoc
Chunk(1,2,3).toSeq
```
 

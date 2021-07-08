---
id: ztransducer 
title: "ZTransducer"
---

```scala mdoc:invisible
import zio.stream.{UStream, ZStream, ZTransducer, ZSink}
import zio.{Schedule, Chunk, IO, ZIO}
import zio.console._
import zio.blocking.Blocking
import java.nio.file.Paths
```

## Introduction

A `ZTransducer[R, E, I, O]` is a stream transformer. Transducers accept a stream as input, and return the transformed stream as output.

ZTransducers can be thought of as a recipe for calling a bunch of methods on a source stream, to yield a new (transformed) stream. A nice mental model is the following type alias:

```scala
type ZTransducer[Env, Err, In, Out] = ZStream[R, E, O] => ZStream[R, E, O]
```

There is no fundamental requirement for transducers to exist, because everything transducers do can be done directly on a stream. However, because transducers separate the stream transformation from the source stream itself, it becomes possible to abstract over stream transformations at the level of values, creating, storing, and passing around reusable transformation pipelines that can be applied to many different streams. 

## Creation

### From Effect

The `ZTransducer.fromEffect` creates a transducer that always evaluates the specified effect. Let's write a transducer that fails with a message: 

```scala mdoc:silent:nest
val error: ZTransducer[Any, String, Any, Nothing] = ZTransducer.fromEffect(IO.fail("Ouch"))
```

### From Function

By using `ZTransducer.fromFunction` we convert a function into a transducer. Let's create a transducer which converts a stream of strings into a stream of characters:

```scala mdoc:silent:nest
val chars: ZTransducer[Any, Nothing, String, Char] = 
  ZTransducer
    .fromFunction[String, Chunk[Char]](s => Chunk.fromArray(s.toArray))
    .mapChunks(_.flatten)
```

There is also a `ZTransducer.fromFunctionM` which is an effecful version of this constructor.

## Built-in Transducers

### Identity

The identity transducer passes elements through without any modification:

```scala mdoc:silent:nest
ZStream(1,2,3).transduce(ZTransducer.identity)
// Ouput: 1, 2, 3
```

### head and last

The `ZTransducer.head` and `ZTransducer.last` are two transducers that return the _first_ and _last_ element of a stream:

```scala mdoc:silent:nest
val stream: UStream[Int] = ZStream(1, 2, 3, 4)
val head: UStream[Option[Int]] = stream.transduce(ZTransducer.head)
val last: UStream[Option[Int]] = stream.transduce(ZTransducer.last)
```

### Splitting

**ZTransducer.splitOn** — A transducer that splits strings on a delimiter:

```scala mdoc:silent:nest
ZStream("1-2-3", "4-5", "6", "7-8-9-10")
  .transduce(ZTransducer.splitOn("-"))
  .map(_.toInt)
// Ouput: 1, 2, 3, 4, 5, 6, 7, 8, 9 10
```

**ZTransducer.splitLines** — A transducer that splits strings on newlines. Handles both Windows newlines (`\r\n`) and UNIX newlines (`\n`):

```scala mdoc:silent:nest
ZStream("This is the first line.\nSecond line.\nAnd the last line.")
  .transduce(ZTransducer.splitLines)
// Output: "This is the first line.", "Second line.", "And the last line."
```

**ZTransducer.splitOnChunk** — A transducer that splits elements on a delimiter and transforms the splits into desired output:

```scala mdoc:silent:nest
ZStream(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
  .transduce(ZTransducer.splitOnChunk(Chunk(4, 5, 6)))
// Output: Chunk(1, 2, 3), Chunk(7, 8, 9, 10)
```

### Dropping

**ZTransducer.dropWhile** — Creates a transducer that starts consuming values as soon as one fails the given predicate:

```scala mdoc:silent:nest
ZStream(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
  .transduce(ZTransducer.dropWhile(_ <= 5))
// Output: 6, 7, 8, 9, 10
```

The `ZTransducer` also has `dropWhileM` which takes an effectful predicate `p: I => ZIO[R, E, Boolean]`.

### Folding

**ZTransducer.fold** — Using `ZTransudcer.fold` we can fold incoming elements until we reach the false predicate, then the transducer emits the computed value and restarts the folding process:

```scala mdoc:silent:nest
ZStream
  .range(0, 8)
  .transduce(
    ZTransducer.fold[Int, Chunk[Int]](Chunk.empty)(_.length < 3)((s, i) =>
      s ++ Chunk(i)
    )
  )
// Ouput: Chunk(0, 1, 2), Chunk(3, 4, 5), Chunk(6, 7)
```

Note that the `ZTransducer.foldM` is like `fold`, but it folds effectfully.

**ZTransducer.foldWeighted** — Creates a transducer that folds incoming elements until reaches the `max` worth of elements determined by the `costFn`, then the transducer emits the computed value and restarts the folding process:

```scala
object ZTransducer {
  def foldWeighted[I, O](z: O)(costFn: (O, I) => Long, max: Long)(
    f: (O, I) => O
  ): ZTransducer[Any, Nothing, I, O] = ???
}
```

In the following example, each time we consume a new element we return one as the weight of that element using cost function. After three times, the sum of the weights reaches to the `max` number, and the folding process restarted. So we expect this transducer to group each three elements in one `Chunk`:

```scala mdoc:silent:nest
ZStream(3, 2, 4, 1, 5, 6, 2, 1, 3, 5, 6)
  .aggregate(
    ZTransducer
      .foldWeighted(Chunk[Int]())(
        (_, _: Int) => 1,
        3
      ) { (acc, el) =>
        acc ++ Chunk(el)
      }
  )
// Output: Chunk(3,2,4),Chunk(1,5,6),Chunk(2,1,3),Chunk(5,6)
```

Another example is when we want to group element which sum of them equal or less than a specific number:

```scala mdoc:silent:nest
ZStream(1, 2, 2, 4, 2, 1, 1, 1, 0, 2, 1, 2)
  .aggregate(
    ZTransducer
      .foldWeighted(Chunk[Int]())(
        (_, i: Int) => i.toLong,
        5
      ) { (acc, el) =>
        acc ++ Chunk(el)
      }
  )
// Output: Chunk(1,2,2),Chunk(4),Chunk(2,1,1,1,0),Chunk(2,1,2)
```

> _**Note**_
>
> The `ZTransducer.foldWeighted` cannot decompose elements whose weight is more than the `max` number. So elements that have an individual cost larger than `max` will force the transducer to cross the `max` cost. In the last example, if the source stream was `ZStream(1, 2, 2, 4, 2, 1, 6, 1, 0, 2, 1, 2)` the output would be `Chunk(1,2,2),Chunk(4),Chunk(2,1),Chunk(6),Chunk(1,0,2,1),Chunk(2)`. As we see, the `6` element crossed the `max` cost.
>
> To decompose these elements, we should use `ZTransducer.foldWeightedDecompose` function.

**ZTransducer.foldWeightedDecompose** — As we saw in the previous section, we need a way to decompose elements — whose cause the output aggregate cross the `max` — into smaller elements. This version of fold takes `decompose` function and enables us to do that:

```scala
object ZTransducer {
  def foldWeightedDecompose[I, O](
      z: O
  )(costFn: (O, I) => Long, max: Long, decompose: I => Chunk[I])(
      f: (O, I) => O
  ): ZTransducer[Any, Nothing, I, O] = ???
}
```

In the following example, we are break down elements that are bigger than 5, using `decompose` function:

```scala mdoc:silent:nest
ZStream(1, 2, 2, 2, 1, 6, 1, 7, 2, 1, 2)
  .aggregate(
    ZTransducer
      .foldWeightedDecompose(Chunk[Int]())(
        (_, i: Int) => i.toLong,
        5,
        (i: Int) =>
          if (i > 5) Chunk(i - 1, 1) else Chunk(i)
      )((acc, el) => acc ++ Chunk.succeed(el))
  )
// Ouput: Chunk(1,2,2),Chunk(2,1),Chunk(5),Chunk(1,1),Chunk(5),Chunk(1,1,2,1),Chunk(2)
```

**ZTransducer.foldUntil** — Creates a transducer that folds incoming element until specific `max` elements have been folded:

```scala mdoc:silent:nest
ZStream(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
  .transduce(ZTransducer.foldUntil(0, 3)(_ + _))
// Output: 6, 15, 24, 10
```

**ZTransducer.foldLeft** — This transducer will fold the inputs until the stream ends, resulting in a stream with one element:

```scala mdoc:silent:nest
val stream: ZStream[Any, Nothing, Int] = 
  ZStream(1, 2, 3, 4).transduce(ZTransducer.foldLeft(0)(_ + _))
// Output: 10
```

### Prepending

The `ZTransducer.prepend` creates a transducer that emits the provided chunks before emitting any other values:

```scala mdoc:silent:nest
ZStream(2, 3, 4).transduce(
  ZTransducer.prepend(Chunk(0, 1))
)
// Output: 0, 1, 2, 3, 4
```

### Branching/Switching

The `ZTransducer.branchAfter` takes `n` as an input and creates a transducer that reads the first `n` values from the stream and uses them to choose the transducer that will be used for the rest of the stream.

In the following example, we are prompting the user to enter a series of numbers. If the sum of the first three elements is less than 5, we continue to emit the remaining elements by using `ZTransducer.identity`, otherwise, we retry prompting the user to enter another series of numbers:

```scala mdoc:silent:nest
ZStream
  .fromEffect(
    putStr("Enter numbers separated by comma: ") *> getStrLn
  )
  .mapConcat(_.split(","))
  .map(_.trim.toInt)
  .transduce(
    ZTransducer.branchAfter(3) { elements =>
      if (elements.sum < 5)
        ZTransducer.identity
      else
        ZTransducer.fromEffect(
          putStrLn(s"received elements are not applicable: $elements")
        ) >>> ZTransducer.fail("boom")
    }
  )
  .retry(Schedule.forever)
```

### Collecting

**ZTransducer.collectAllN** — Collects incoming values into chunk of maximum size of `n`:

```scala mdoc:silent:nest
ZStream(1, 2, 3, 4, 5).transduce(
  ZTransducer.collectAllN(3)
)
// Output: Chunk(1,2,3), Chunk(4,5)
```

**ZTransducer.collectAllWhile** — Accumulates incoming elements into a chunk as long as they verify the given predicate:

```scala mdoc:silent:nest
ZStream(1, 2, 0, 4, 0, 6, 7).transduce(
  ZTransducer.collectAllWhile(_ != 0)
)
// Output: Chunk(1,2), Chunk(4), Chunk(6,7)
```

**ZTransducer.collectAllToMapN** — Creates a transducer accumulating incoming values into maps of up to `n` keys. Elements are mapped to keys using the function `key`; elements mapped to the same key will be merged with the function `f`:

```scala
object ZTransducer {
  def collectAllToMapN[K, I](n: Long)(key: I => K)(
      f: (I, I) => I
  ): ZTransducer[Any, Nothing, I, Map[K, I]] = ???
}
```

Let's do an example:

```scala mdoc:silent:nest
ZStream(1, 2, 0, 4, 5).transduce(
  ZTransducer.collectAllToMapN[Int, Int](10)(_ % 3)(_ + _)
)
// Output: Map(1 -> 5, 2 -> 7, 0 -> 0)
```

**ZTransducer.collectAllToSetN** — Creates a transducer accumulating incoming values into sets of maximum size `n`:

```scala mdoc:silent:nest
ZStream(1, 2, 1, 2, 1, 3, 0, 5, 0, 2).transduce(
  ZTransducer.collectAllToSetN(3)
)
// Output: Set(1,2,3), Set(0,5,2), Set(1)
```

### Compression

**ZTransducer.deflate** — The `deflate` transducer compresses a stream of bytes as specified by [RFC 1951](https://tools.ietf.org/html/rfc1951).

```scala mdoc:silent
import zio.stream.ZStream
import zio.stream.Transducer.deflate
import zio.stream.compression.{CompressionLevel, CompressionStrategy, FlushMode}

def compressWithDeflate(clearText: ZStream[Any, Nothing, Byte]): ZStream[Any, Nothing, Byte] = {
  val bufferSize: Int = 64 * 1024 // Internal buffer size. Few times bigger than upstream chunks should work well.
  val noWrap: Boolean = false // For HTTP Content-Encoding should be false.
  val level: CompressionLevel = CompressionLevel.DefaultCompression
  val strategy: CompressionStrategy = CompressionStrategy.DefaultStrategy
  val flushMode: FlushMode = FlushMode.NoFlush
  clearText.transduce(deflate(bufferSize, noWrap, level, strategy, flushMode))
}

def deflateWithDefaultParameters(clearText: ZStream[Any, Nothing, Byte]): ZStream[Any, Nothing, Byte] =
  clearText.transduce(deflate())
```

**ZTransducer.gzip** — The `gzip` transducer compresses a stream of bytes as using _gzip_ method:

```scala mdoc:silent:nest
import zio.stream.compression._

ZStream
  .fromFile(Paths.get("file.txt"))
  .transduce(
    ZTransducer.gzip(
      bufferSize = 64 * 1024,
      level = CompressionLevel.DefaultCompression,
      strategy = CompressionStrategy.DefaultStrategy,
      flushMode = FlushMode.NoFlush
    )
  )
  .run(
    ZSink.fromFile(Paths.get("file.gz"))
  )
```

### Decompression

If we are reading `Content-Encoding: deflate`, `Content-Encoding: gzip` streams, or other such streams of compressed data, the following transducers can be helpful. Both decompression methods will fail with `CompressionException` when input wasn't properly compressed:

**ZTransducer.inflate** — This transducer allows decompressing stream of _deflated_ inputs, according to [RFC 1951](https://tools.ietf.org/html/rfc1951).

```scala mdoc:silent:nest
import zio.stream.ZStream
import zio.stream.Transducer.{ gunzip, inflate }
import zio.stream.compression.CompressionException

def decompressDeflated(deflated: ZStream[Any, Nothing, Byte]): ZStream[Any, CompressionException, Byte] = {
  val bufferSize: Int = 64 * 1024 // Internal buffer size. Few times bigger than upstream chunks should work well.
  val noWrap: Boolean = false     // For HTTP Content-Encoding should be false.
  deflated.transduce(inflate(bufferSize, noWrap))
}
```

**ZTransducer.gunzip** — This transducer can be used to decompress stream of _gzipped_ inputs, according to [RFC 1952](https://tools.ietf.org/html/rfc1952):

```scala mdoc:silent:nest
import zio.stream.ZStream
import zio.stream.Transducer.{ gunzip, inflate }
import zio.stream.compression.CompressionException

def decompressGzipped(gzipped: ZStream[Any, Nothing, Byte]): ZStream[Any, CompressionException, Byte] = {
  val bufferSize: Int = 64 * 1024 // Internal buffer size. Few times bigger than upstream chunks should work well.
  gzipped.transduce(gunzip(bufferSize))
}
```

### Decoders

ZIO stream has a wide variety of transducers to decode chunks of bytes into strings:

| Decoder                     | Input          | Output |
|-----------------------------|----------------|--------|
| `ZTransducer.utfDecode`     | Unicode bytes  | String |
| `ZTransducer.utf8Decode`    | UTF-8 bytes    | String |
| `ZTransducer.utf16Decode`   | UTF-16         | String |
| `ZTransducer.utf16BEDecode` | UTF-16BE bytes | String |
| `ZTransducer.utf16LEDecode` | UTF-16LE bytes | String |
| `ZTransducer.utf32Decode`   | UTF-32 bytes   | String |
| `ZTransducer.utf32BEDecode` | UTF-32BE bytes | String |
| `ZTransducer.utf32LEDecode` | UTF-32LE bytes | String |
| `ZTransducer.usASCIIDecode` | US-ASCII bytes | String |

## Operations

### Filtering

Transducers have two types of filtering operations, the `ZTransducer#filter` used for filtering outgoing elements and the `ZTransducer#filterInput` is used for filtering incoming elements:

```scala mdoc:silent:nest
ZStream(1, -2, 0, 1, 3, -3, 4, 2, 0, 1, -3, 1, 1, 6)
  .transduce(
    ZTransducer
      .collectAllN[Int](3)
      .filterInput[Int](_ > 0)
      .filter(_.sum > 5)
  )
// Output: Chunk(4,2,1), Chunk(1,1,6)
```

### Input Transformation (Mapping)

To transform the _outputs_ of the transducer, we can use the `ZTransducer#map` combinator for the success channel, and the `ZTransducer#mapError` combinator for the failure channel. Also, the `ZTransducer.mapChunks` takes a function of type `Chunk[O] => Chunk[O2]` and transforms chunks emitted by the transducer.

### Output Transformation (Contramap)

To transform the _inputs_ of the transducer, we can use the `ZTransducer#contramap` combinator. It takes a map function of type `J => I` and convert a `ZTransducer[R, E, I, O]` to `ZTransducer[R, E, J, O]`:

```scala
class ZTransducer[-R, +E, -I, +O] {
  final def contramap[J](f: J => I): ZTransducer[R, E, J, O] = ???
}
```

Let's create an integer parser transducer using `ZTransducer.contramap`:

```scala mdoc:silent:nest
val numbers: ZStream[Any, Nothing, Int] =
  ZStream("1-2-3-4-5")
    .mapConcat(_.split("-"))
    .transduce(
      ZTransducer.identity[Int].contramap[String](_.toInt)
    )
```

### Composing

We can compose transducers in two ways:

1. **Composing Two Transducers** — One transducer can be composed with another transducer, resulting in a composite transducer:

```scala mdoc:silent:nest
val lines: ZStream[Blocking, Throwable, String] =
  ZStream
    .fromFile(Paths.get("file.txt"))
    .transduce(
      ZTransducer.utf8Decode >>> ZTransducer.splitLines
    )
```

2. **Composing ZTransducer with ZSink** — One transducer can be composed with a sink, resulting in a sink that processes elements by piping them through the transducer and piping the results into the sink:

```scala mdoc:silent:nest
val refine: ZIO[Blocking, Throwable, Long] =
  ZStream
    .fromFile(Paths.get("file.txt"))
    .run(
      ZTransducer.utf8Decode >>> ZTransducer.splitLines.filter(_.contains('₿')) >>>
        ZSink
          .fromFile(Paths.get("file.refined.txt"))
          .contramapChunks[String](
            _.flatMap(line => (line + System.lineSeparator()).getBytes())
          )
    )
```

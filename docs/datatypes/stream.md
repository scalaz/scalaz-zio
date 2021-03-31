---
id: datatypes_stream
title:  "Stream"
---

A `Stream[E, A]` represents an effectful stream that can produce values of
type `A`, or potentially fail with a value of type `E`.

## Creating a Stream

```scala mdoc:silent
import zio.stream._

val stream: Stream[Nothing, Int] = Stream(1,2,3)
```

Or from an Iterable :

```scala mdoc:silent
import zio.stream._

val streamFromIterable: Stream[Nothing, Int] = Stream.fromIterable(0 to 100)
```

You might also want to create it from a callback API :
```scala mdoc:silent
import zio._
import zio.stream._

def callbackAPI(callback: String => Unit): Unit = ???

val streamCallbackAPI: ZStream[Any, Throwable, String] =
    ZStream.fromEffect(
        Task.effectAsync[String] { cb =>
            callbackAPI(message => cb(Task.succeed(message)))
        }
    ).forever 
```



## Transforming a Stream

ZIO Stream supports many standard transforming functions like `map`, `partition`, `grouped`, `groupByKey`, `groupedWithin`
and many others. Here are examples of how to use them.   

### map
```scala mdoc:silent
import zio.stream._

val intStream: Stream[Nothing, Int] = Stream.fromIterable(0 to 100)
val stringStream: Stream[Nothing, String] = intStream.map(_.toString)
```
### partition
`partition` function splits the stream into tuple of streams based on predicate. The first stream contains all
element evaluated to true and the second one contains all element evaluated to false.
The faster stream may advance by up to `buffer` elements further than the slower one. Two streams are 
wrapped by `ZManaged` type. In the example below, left stream consists of even numbers only.

```scala mdoc:silent
import zio._
import zio.stream._

val partitionResult: ZManaged[Any, Nothing, (ZStream[Any, Nothing, Int], ZStream[Any, Nothing, Int])] =
  Stream
    .fromIterable(0 to 100)
    .partition(_ % 2 == 0, buffer = 50)
```

### grouped
To partition the stream results with the specified chunk size, you can use `grouped` function.

```scala mdoc:silent
import zio._
import zio.stream._

val groupedResult: ZStream[Any, Nothing, Chunk[Int]] =
  Stream
    .fromIterable(0 to 100)
    .grouped(50)
```

### groupByKey
To partition the stream by function result you can use `groupByKey` or `groupBy`. In the example below
exam results are grouped into buckets and counted. 

```scala mdoc:silent
import zio._
import zio.stream._

  case class Exam(person: String, score: Int)

  val examResults = Seq(
    Exam("Alex", 64),
    Exam("Michael", 97),
    Exam("Bill", 77),
    Exam("John", 78),
    Exam("Bobby", 71)
  )

  val groupByKeyResult: ZStream[Any, Nothing, (Int, Int)] =
    Stream
      .fromIterable(examResults)
      .groupByKey(exam => exam.score / 10 * 10) {
        case (k, s) => ZStream.fromEffect(s.runCollect.map(l => k -> l.size))
      }
```

### groupedWithin
`groupedWithin` allows to group events by time or chunk size, whichever is satisfied first. In the example below
every chunk consists of 30 elements and is produced every 3 seconds.

```scala mdoc:silent
import zio._
import zio.stream._
import zio.duration._
import zio.clock.Clock

val groupedWithinResult: ZStream[Any with Clock, Nothing, Chunk[Int]] =
  Stream.fromIterable(0 to 10)
    .repeat(Schedule.spaced(1.seconds))
    .groupedWithin(30, 10.seconds)
```

## Consuming a Stream

```scala mdoc:silent
import zio._
import zio.console._
import zio.stream._

val result: RIO[Console, Unit] = Stream.fromIterable(0 to 100).foreach(i => putStrLn(i.toString))
```

### Using a Sink

A `Sink[E, A0, A, B]` consumes values of type `A`, ultimately producing
either an error of type `E`, or a value of type `B` together with a remainder
of type `A0`.

You can for example reduce a `Stream` to a `ZIO` value using `Sink.foldLeft` : 

```scala mdoc:silent
import zio._
import zio.stream._

def streamReduce(total: Int, element: Int): Int = total + element
val resultFromSink: UIO[Int] = Stream(1,2,3).run(Sink.foldLeft(0)(streamReduce))
```

## Working on several streams

You can merge several streams using the `merge` method :

```scala mdoc:silent
import zio.stream._

val merged: Stream[Nothing, Int] = Stream(1,2,3).merge(Stream(2,3,4))
```

Or zipping streams : 

```scala mdoc:silent
import zio.stream._

val zippedStream: Stream[Nothing, (Int, Int)] = Stream(1,2,3).zip(Stream(2,3,4))
```

Then you would be able to reduce the stream into to a `ZIO` value : 

```scala mdoc:silent
import zio._

def tupleStreamReduce(total: Int, element: (Int, Int)) = {
  val (a,b) = element
  total + (a + b)
} 

val reducedResult: UIO[Int] = zippedStream.run(Sink.foldLeft(0)(tupleStreamReduce))
```

## Compressed streams

### Decompression

If you read `Content-Encoding: deflate`, `Content-Encoding: gzip` or streams other such streams of compressed data, following transducers can be helpful:
* `inflate` transducer allows to decompress stream of _deflated_ inputs, according to [RFC 1951](https://tools.ietf.org/html/rfc1951).
* `gunzip` transducer can be used to decompress stream of _gzipped_ inputs, according to [RFC 1952](https://tools.ietf.org/html/rfc1952).

Both decompression methods will fail with `CompressionException` when input wasn't properly compressed.

```scala mdoc:silent
import zio.stream.ZStream
import zio.stream.Transducer.{ gunzip, inflate }
import zio.stream.compression.CompressionException

def decompressDeflated(deflated: ZStream[Any, Nothing, Byte]): ZStream[Any, CompressionException, Byte] = {
  val bufferSize: Int = 64 * 1024 // Internal buffer size. Few times bigger than upstream chunks should work well.
  val noWrap: Boolean = false     // For HTTP Content-Encoding should be false.
  deflated.transduce(inflate(bufferSize, noWrap))
}

def decompressGzipped(gzipped: ZStream[Any, Nothing, Byte]): ZStream[Any, CompressionException, Byte] = {
  val bufferSize: Int = 64 * 1024 // Internal buffer size. Few times bigger than upstream chunks should work well.
  gzipped.transduce(gunzip(bufferSize))
}

```

### Compression

The `deflate` transducer compresses a stream of bytes as specified by [RFC 1951](https://tools.ietf.org/html/rfc1951).

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

---
id: overview_handling_errors
title:  "Handling Errors"
---

This section looks at some of the common ways to detect and respond to failure.

```scala mdoc
import zio._
```

## Either

You can surface failures with `ZIO#either`, which takes an `ZIO[R, E, A]` and produces an `ZIO[R, Nothing, Either[E, A]]`.

```scala mdoc:silent
val zeither: UIO[Either[String, Int]] = 
  IO.fail("Uh oh!").either
```

You can submerge failures with `ZIO.absolve`, which is the opposite of `either` and turns an `ZIO[R, Nothing, Either[E, A]]` into a `ZIO[R, E, A]`:

```scala mdoc:silent
def sqrt(io: UIO[Double]): IO[String, Double] =
  ZIO.absolve(
    io.map(value =>
      if (value < 0.0) Left("Value must be >= 0.0")
      else Right(Math.sqrt(value))
    )
  )
```

## Catching All Errors

If you want to catch and recover from all types of errors and effectfully attempt recovery, you can use the `catchAll` method:

```scala mdoc
import java.io.{ FileNotFoundException, IOException }

def openFile(s: String): IO[IOException, Array[Byte]] = 
  ZIO.effect(???).refineToOrDie[IOException]
```

```scala mdoc:silent
val z: IO[IOException, Array[Byte]] = 
  openFile("primary.json").catchAll(_ => 
    openFile("backup.json"))
```

In the callback passed to `catchAll`, you may return an effect with a different error type (or perhaps `Nothing`), which will be reflected in the type of effect returned by `catchAll`.

## Catching Some Errors

If you want to catch and recover from only some types of exceptions and effectfully attempt recovery, you can use the `catchSome` method:

```scala mdoc:silent
val data: IO[IOException, Array[Byte]] = 
  openFile("primary.data").catchSome {
    case _ : FileNotFoundException => 
      openFile("backup.data")
  }
```

Unlike `catchAll`, `catchSome` cannot reduce or eliminate the error type, although it can widen the error type to a broader class of errors.

## Fallback

You can try one effect, or, if it fails, try another effect, with the `orElse` combinator:

```scala mdoc:silent
val primaryOrBackupData: IO[IOException, Array[Byte]] = 
  openFile("primary.data").orElse(openFile("backup.data"))
```

## Folding

Scala's `Option` and `Either` data types have `fold`, which let you handle both failure and success at the same time. In a similar fashion, `ZIO` effects also have several methods that allow you to handle both failure and success.

The first fold method, `fold`, lets you non-effectfully handle both failure and success, by supplying a non-effectful handler for each case:

```scala mdoc:silent
lazy val DefaultData: Array[Byte] = Array(0, 0)

val primaryOrDefaultData: UIO[Array[Byte]] = 
  openFile("primary.data").fold(
    _    => DefaultData,
    data => data)
```

The second fold method, `foldM`, lets you effectfully handle both failure and success, by supplying an effectful (but still pure) handler for each case:

```scala mdoc:silent
val primaryOrSecondaryData: IO[IOException, Array[Byte]] = 
  openFile("primary.data").foldM(
    _    => openFile("secondary.data"),
    data => ZIO.succeed(data))
```

Nearly all error handling methods are defined in terms of `foldM`, because it is both powerful and fast.

In the following example, `foldM` is used to handle both failure and success of the `readUrls` method:

```scala mdoc
sealed trait Content
case class NoContent(t: Throwable) extends Content
case class OkContent(s: String) extends Content
def readUrls(file: String): Task[List[String]] = IO.succeed("Hello" :: Nil)
def fetchContent(urls: List[String]): UIO[Content] = IO.succeed(OkContent("Roger"))
```
```scala mdoc:silent
val urls: UIO[Content] =
  readUrls("urls.json").foldM(
    error   => IO.succeed(NoContent(error)), 
    success => fetchContent(success)
  )
```

## Retrying

There are a number of useful methods on the ZIO data type for retrying failed effects. 

The most basic of these is `ZIO#retry`, which takes a `Schedule` and returns a new effect that will retry the first effect if it fails, according to the specified policy:

```scala mdoc:silent
import zio.clock._

val retriedOpenFile: ZIO[Clock, IOException, Array[Byte]] = 
  openFile("primary.data").retry(Schedule.recurs(5))
```

The next most powerful function is `ZIO#retryOrElse`, which allows specification of a fallback to use, if the effect does not succeed with the specified policy:

```scala mdoc
  openFile("primary.data").retryOrElse(
    Schedule.recurs(5), 
    (_, _: Long) => ZIO.succeed(DefaultData))
```

The final method, `ZIO#retryOrElseEither`, allows returning a different type for the fallback.

For more information on how to build schedules, see the documentation on [Schedule](../datatypes/schedule.md).

## Next Steps

If you are comfortable with basic error handling, then the next step is to learn about safe [resource handling](handling_resources.md).

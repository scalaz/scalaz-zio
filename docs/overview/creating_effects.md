---
id: overview_creating_effects
title:  "Creating Effects"
---

This section explores some of the common ways to create ZIO effects from values, from common Scala types, and from both synchronous and asynchronous side-effects.

```scala mdoc
import zio.{ ZIO, Task, UIO, URIO, IO }
```

## From Success Values

Using the `ZIO.succeed` method, you can create an effect that succeeds with the specified value:

```scala mdoc:silent
val s1 = ZIO.succeed(42)
```

You can also use methods in the companion objects of the `ZIO` type aliases:

```scala mdoc:silent
val s2: Task[Int] = Task.succeed(42)
```

The `succeed` method takes a by-name parameter to make sure that any accidental side effects from constructing the value can be properly managed by the ZIO Runtime. However, `succeed` is intended for values which do not have any side effects. If you know that your value does have side effects consider using `ZIO.effectTotal` for clarity.

```scala mdoc:silent
val now = ZIO.effectTotal(System.currentTimeMillis())
```

The value inside a successful effect constructed with `ZIO.effectTotal` will only be constructed if absolutely required.

## From Failure Values

Using the `ZIO.fail` method, you can create an effect that models failure:

```scala mdoc:silent
val f1 = ZIO.fail("Uh oh!")
```

For the `ZIO` data type, there is no restriction on the error type. You may use strings, exceptions, or custom data types appropriate for your application.

Many applications will model failures with classes that extend `Throwable` or `Exception`:

```scala mdoc:silent
val f2 = Task.fail(new Exception("Uh oh!"))
```

Note that unlike the other effect companion objects, the `UIO` companion object does not have `UIO.fail`, because `UIO` values cannot fail.

## From Scala Values

Scala's standard library contains a number of data types that can be converted into ZIO effects.

### Option

An `Option` can be converted into a ZIO effect using `ZIO.fromOption`:

```scala mdoc:silent
val zoption: IO[Option[Nothing], Int] = ZIO.fromOption(Some(2))
```

The error type of the resulting effect is `Option[Nothing]`, which provides no information on why the value is not there. You can change the `Option[Nothing]` into a more specific error type using `ZIO#mapError`:

```scala mdoc:silent
val zoption2: IO[String, Int] = zoption.mapError(_ => "It wasn't there!")
```

You can also readily compose it with other operators while preserving the optional nature of the result (similar to an `OptionT`)

```scala mdoc
trait Team
```

```scala mdoc:silent
val maybeId: IO[Option[Nothing], String] = ZIO.fromOption(Some("abc123"))
def getUser(userId: String): IO[Throwable, Option[User]] = ???
def getTeam(teamId: String): IO[Throwable, Team] = ???


val result: IO[Throwable, Option[(User, Team)]] = (for {
  id   <- maybeId
  user <- getUser(id).some
  team <- getTeam(user.teamId).asSomeError 
} yield (user, team)).optional 
```

### Either

An `Either` can be converted into a ZIO effect using `ZIO.fromEither`:

```scala mdoc:silent
val zeither = ZIO.fromEither(Right("Success!"))
```

The error type of the resulting effect will be whatever type the `Left` case has, while the success type will be whatever type the `Right` case has.

### Try

A `Try` value can be converted into a ZIO effect using `ZIO.fromTry`:

```scala mdoc:silent
import scala.util.Try

val ztry = ZIO.fromTry(Try(42 / 0))
```

The error type of the resulting effect will always be `Throwable`, because `Try` can only fail with values of type `Throwable`.

### Function

A function `A => B` can be converted into a ZIO effect with `ZIO.fromFunction`:

```scala mdoc:silent
val zfun: URIO[Int, Int] =
  ZIO.fromFunction((i: Int) => i * i)
```

The environment type of the effect is `A` (the input type of the function), because in order to run the effect, it must be supplied with a value of this type.

### Future

A `Future` can be converted into a ZIO effect using `ZIO.fromFuture`:

```scala mdoc:silent
import scala.concurrent.Future

lazy val future = Future.successful("Hello!")

val zfuture: Task[String] =
  ZIO.fromFuture { implicit ec =>
    future.map(_ => "Goodbye!")
  }
```

The function passed to `fromFuture` is passed an `ExecutionContext`, which allows ZIO to manage where the `Future` runs (of course, you can ignore this `ExecutionContext`).

The error type of the resulting effect will always be `Throwable`, because `Future` can only fail with values of type `Throwable`.

## From Side-Effects

ZIO can convert both synchronous and asynchronous side-effects into ZIO effects (pure values).

These functions can be used to wrap procedural code, allowing you to seamlessly use all features of ZIO with legacy Scala and Java code, as well as third-party libraries.

### Synchronous Side-Effects

A synchronous side-effect can be converted into a ZIO effect using `ZIO.effect`:

```scala mdoc:silent
import scala.io.StdIn

val getStrLn: Task[String] =
  ZIO.effect(StdIn.readLine())
```

The error type of the resulting effect will always be `Throwable`, because side-effects may throw exceptions with any value of type `Throwable`.

If a given side-effect is known to not throw any exceptions, then the side-effect can be converted into a ZIO effect using `ZIO.effectTotal`:

```scala mdoc:silent
def putStrLn(line: String): UIO[Unit] =
  ZIO.effectTotal(println(line))
```

You should be careful when using `ZIO.effectTotal`—when in doubt about whether or not a side-effect is total, prefer `ZIO.effect` to convert the effect.

If you wish to refine the error type of an effect (by treating other errors as fatal), then you can use the `ZIO#refineToOrDie` method:

```scala mdoc:silent
import java.io.IOException

val getStrLn2: IO[IOException, String] =
  ZIO.effect(StdIn.readLine()).refineToOrDie[IOException]
```

### Asynchronous Side-Effects

An asynchronous side-effect with a callback-based API can be converted into a ZIO effect using `ZIO.effectAsync`:

```scala mdoc
trait User { 
  def teamId: String
}
trait AuthError
```

```scala mdoc:silent
object legacy {
  def login(
    onSuccess: User => Unit,
    onFailure: AuthError => Unit): Unit = ???
}

val login: IO[AuthError, User] =
  IO.effectAsync[AuthError, User] { callback =>
    legacy.login(
      user => callback(IO.succeed(user)),
      err  => callback(IO.fail(err))
    )
  }
```

Asynchronous ZIO effects are much easier to use than callback-based APIs, and they benefit from ZIO features like interruption, resource-safety, and superior error handling.

## Blocking Synchronous Side-Effects

Some side-effects use blocking IO or otherwise put a thread into a waiting state. If not carefully managed, these side-effects can deplete threads from your application's main thread pool, resulting in work starvation.

ZIO provides the `zio.blocking` package, which can be used to safely convert such blocking side-effects into ZIO effects.

A blocking side-effect can be converted directly into a ZIO effect blocking with the `effectBlocking` method:

```scala mdoc:silent
import zio.blocking._

val sleeping =
  effectBlocking(Thread.sleep(Long.MaxValue))
```

The resulting effect will be executed on a separate thread pool designed specifically for blocking effects.

Blocking side-effects can be interrupted by invoking `Thread.interrupt` using the `effectBlockingInterrupt` method.

Some blocking side-effects can only be interrupted by invoking a cancellation effect. You can convert these side-effects using the `effectBlockingCancelable` method:

```scala mdoc:silent
import java.net.ServerSocket
import zio.UIO

def accept(l: ServerSocket) =
  effectBlockingCancelable(l.accept())(UIO.effectTotal(l.close()))
```

If a side-effect has already been converted into a ZIO effect, then instead of `effectBlocking`, the `blocking` method can be used to ensure the effect will be executed on the blocking thread pool:

```scala mdoc:silent
import scala.io.{ Codec, Source }

def download(url: String) =
  Task.effect {
    Source.fromURL(url)(Codec.UTF8).mkString
  }

def safeDownload(url: String) =
  blocking(download(url))
```

## Next Steps

If you are comfortable creating effects from values, Scala data types, and side-effects, the next step is learning [basic operations](basic_operations.md) on effects.

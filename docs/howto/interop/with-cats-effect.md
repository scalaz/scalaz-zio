---
id: with-cats-effect
title: "How to Interop with Cats Effect?"
---

```scala mdoc:invisible
// Contributor Note:
// This page has some unchecked markdown documentation. They are related
// to the zio-interop-cats module compatible with the Cats Effect 2.x
// series. So before committing any change to those examples, please make
// sure they are compilable and work properly with Cat Effect 2.x.
```

## Introduction

[`interop-cats`](https://github.com/zio/interop-cats) has instances for the [Cats](https://typelevel.org/cats/), [Cats MTL](https://github.com/typelevel/cats-mtl) and [Cats Effect](https://typelevel.org/cats-effect/) libraries, which allow you to use ZIO with any libraries that rely on these, like [Doobie](https://github.com/tpolecat/doobie), [Http4s](https://github.com/http4s/http4s), [FS2](https://github.com/functional-streams-for-scala/fs2) or [Circe](https://github.com/circe/circe)

Depends on which version of Cats Effect we are using, we should pick the right version of `zio-interop-cats`. In this tutorial, whenever we're working with Cats Effect 2.x, we are using:

```scala
libraryDependencies += "dev.zio" %% "zio-interop-cats" % "2.5.1.0"
```

And whenever we are using Cats Effect 3.x instances, we are using:

```scala
libraryDependencies += "dev.zio" %% "zio-interop-cats" % "3.1.1.0"
```

Most of the interop functionality resides in the following package:

```scala mdoc:silent
import zio.interop.catz._
```

## Cats Effect Instances

ZIO integrates with Typelevel libraries by providing instances of Cats Effect type classes. These type classes are used by `fs2`, `doobie`, `http4s`, and a variety of other libraries in the functional Scala ecosystem.

Due to the limitations of the Cats Effect, ZIO cannot provide instances for arbitrary error types. Instead, we can obtain instances only for effects whose error type extends `Throwable`.

For convenience, ZIO includes the `Task` and `RIO` type aliases, which fix the error type to `Throwable`, and may be useful for interop with Cats Effect:

```scala mdoc:invisible
import zio.{ZIO, Task, RIO}
```

```scala
type Task[+A]    = ZIO[Any, Throwable, A]
type RIO[-R, +A] = ZIO[  R, Throwable, A]
```

### Providing Runtime Manually

To use Cats Effect instances for these types, we should have an implicit `Runtime[R]` in scope for the environment type of our effects. The following code snippet creates an implicit `Runtime` for all the modules built into ZIO:

```scala mdoc:silent:nest
import cats.implicits._
import zio.interop.catz._

object ZioCatsEffectInterop extends scala.App {
  def catsEffectApp[F[_]: cats.effect.Sync]: F[Unit] =
    cats.effect.Sync[F].delay(
        println("Hello from Cats Effect World!")
      )
      
  implicit val runtime: zio.Runtime[zio.ZEnv] = zio.Runtime.default

  val zioApp: zio.Task[Unit] = catsEffectApp[zio.Task]
  runtime.unsafeRun(zioApp.exitCode)
}
```

If we are working with Cats Effect 3.x, the `catsEffectApp[Task]` will be expanded as if we called the following code explicitly:

```scala mdoc:invisible
import ZioCatsEffectInterop.catsEffectApp
```

```scala mdoc:silent:nest
object ZioCatsEffectInterop extends scala.App {
  val runtime: zio.Runtime[zio.ZEnv] = zio.Runtime.default
  
  val zioApp: zio.Task[Unit] = catsEffectApp[zio.Task](
    zio.interop.catz.asyncRuntimeInstance(runtime) 
  )
  
  runtime.unsafeRun(zioApp.exitCode) 
}
```

And if we are working with Cats Effect 2.x, it will be expanded as if we called following code explicitly:

```scala
object ZioCatsEffectInterop extends scala.App {
  val runtime: zio.Runtime[zio.ZEnv] = zio.Runtime.default

  val zioApp = catsEffectApp[zio.Task](zio.interop.catz.taskConcurrentInstance)
  runtime.unsafeRun(zioApp.exitCode)
}
```

If we are using `RIO` for a custom environment `R`, then we will have to create our own `Runtime[R]`, and ensure that implicit wherever we need Cats Effect instances.

### Using `CatsApp` Runtime

As a convenience, our application can extend `CatsApp`, which automatically brings an implicit `Runtime[ZEnv]` into our scope:

```scala mdoc:silent:nest
import zio.interop.catz._
import cats.implicits._

object ZioCatsEffectInteropWithCatsApp extends CatsApp {
  def catsEffectApp[F[_]: cats.effect.Sync]: F[Unit] =
    cats.effect.Sync[F].delay(println("Hello from Cats Effect World!"))

  override def run(args: List[String]): zio.URIO[zio.ZEnv, zio.ExitCode] = 
    catsEffectApp[zio.Task].exitCode
}
```

This example works properly in both Cats Effect 2.x and 3.x versions.

## Cats Effect 2.x

### Timer Instance

In order to get a `cats.effect.Timer[zio.Task]` instance, we need an extra import (`zio.interop.catz.implicits._`):

```scala
import java.util.concurrent.TimeUnit

import cats.implicits._
import zio.interop.catz._
import zio.interop.catz.implicits._ // Provides `zio.Task instance` for `cats.effect.Time` type class
import zio.{ ExitCode, Task, URIO }

import scala.concurrent.duration.DurationInt

object ZioCatsEffectTimerInterop extends zio.interop.catz.CatsApp {
  override def run(args: List[String]): zio.URIO[zio.ZEnv, zio.ExitCode] =
    catsEffectTimerApp[zio.Task].exitCode

  def catsEffectTimerApp[F[_]: cats.effect.Clock: cats.effect.Timer: cats.effect.Sync]: F[Unit] = for {
    t2 <- cats.effect.Clock[F].monotonic(TimeUnit.SECONDS)
    _  <- cats.effect.Timer[F].sleep(2.seconds)
    t1 <- cats.effect.Clock[F].monotonic(TimeUnit.SECONDS)
    _  <- cats.effect.Sync[F].delay(println(t1 - t2))
  } yield ()
}
```

The reason a `cats.effect.Timer[zio.Task]` instance is not provided by the default _interop_ import is that it makes testing programs that require timing capabilities very difficult. The extra import (wherever needed) makes reasoning about timing-related effects much easier.

If we're using `RIO` for a custom environment then our environment must use the `Clock` service, e.g. `R <: Clock` to get a timer.

### Converting Resource to ZManaged

To convert Cats Effect `Resource` into `ZManaged`, we can call `toManaged` on `Resource`.

For example, assume we have the following `File` API:

```scala mdoc:silent
case class File[F[_]: cats.effect.Sync]() {
  import cats.syntax.apply._
  def read: F[String] =
    cats.effect.Sync[F].delay(println("Reading file.")) *>
      cats.effect.Sync[F].pure("Hello, World!")
  def close: F[Unit]  =
    cats.effect.Sync[F].delay(println("Closing file."))
}

object File {
  import cats.syntax.apply._
  def open[F[_]: cats.effect.Sync](name: String): F[File[F]] =
    cats.effect.Sync[F].delay(println(s"opening $name file")) *>
      cats.effect.Sync[F].delay(File())
}
```

And, also assume we have `fileResource` defined as follows:

```scala mdoc:silent:nest
def fileResource[F[_]: cats.effect.Sync](name: String): cats.effect.Resource[F, File[F]] =
  cats.effect.Resource.make(File.open[F](name))(_.close)
```

Let's convert that to `ZManaged`:

```scala
val resource: zio.ZManaged[zio.ZEnv, Throwable, File[zio.Task]] =
  fileResource[zio.Task]("log.txt").toManaged[zio.ZEnv]
```

Here is a complete working example:

```scala
import zio.interop.catz._

object CatsEffectResourceInterop extends CatsApp {
  def fileResource[F[_]: cats.effect.Sync](name: String): cats.effect.Resource[F, File[F]] =
    cats.effect.Resource.make(File.open[F](name))(_.close)

  def myApp: zio.ZIO[zio.ZEnv, Throwable, Unit] = for {
    c <- fileResource[zio.Task]("log.txt").toManaged[zio.ZEnv].use(_.read)
    _ <- zio.console.putStr(s"file content: $c")
  } yield ()

  override def run(args: List[String]): zio.URIO[zio.ZEnv, zio.ExitCode] =
    myApp.exitCode
}
```

### Converting ZManaged to Resource

We have an extension method on `ZManaged` called `ZManaged#toResource` which converts a ZIO managed resource to Cats Effect resource:

```scala 
final class ZManagedSyntax[R, E, A](private val managed: ZManaged[R, E, A]) {
  def toResource[F[_]](implicit
      F: Async[F],
      ev: Effect[ZIO[R, E, *]]
  ): Resource[F, A] = ???
}
```

Let's try an example:

```scala
import zio.interop.catz._

object ZManagedToResource extends cats.effect.IOApp {
  implicit val zioRuntime: zio.Runtime[zio.ZEnv] = zio.Runtime.default

  val resource: cats.effect.Resource[cats.effect.IO, java.io.InputStream] =
    zio.ZManaged
      .fromAutoCloseable(
        zio.ZIO.effect(
          java.nio.file.Files.newInputStream(
            java.nio.file.Paths.get("file.txt")
          )
        )
      )
      .toResource[cats.effect.IO]

  val effect: cats.effect.IO[Unit] =
    resource
      .use { is =>
        cats.effect.IO.delay(is.readAllBytes())
      }
      .flatMap(bytes =>
        cats.effect.IO.delay(
          println(s"file length: ${bytes.length}")
        )
      )

  override def run(args: List[String]): cats.effect.IO[cats.effect.ExitCode] =
    effect.as(cats.effect.ExitCode.Success)
}
```

## Cats Effect 3.x

### Type class Instances for ZIO's Task

ZIO integrates with Cats Effect 3.x as well as 2.x. The `interop-cats` module provides `Concurrent`, `Temporal` and `Async` for `zio.Task`.

An example of ZIO interoperability with CE3:

```scala mdoc:silent:nest
import cats.implicits._
import zio.interop.catz._
import scala.concurrent.duration.DurationInt

object ZioCatsEffectInterop extends zio.interop.catz.CatsApp {

  def catsEffectTimerApp[F[_]: cats.effect.Async]: F[Unit] = for {
    t2 <- cats.effect.Clock[F].monotonic
    _  <- cats.effect.Temporal[F].sleep(2.seconds)
    t1 <- cats.effect.Clock[F].monotonic
    _  <- cats.effect.Sync[F].delay(println(t1 - t2))
  } yield ()
  
  override def run(args: List[String]): zio.URIO[zio.ZEnv, zio.ExitCode] = {
    catsEffectTimerApp[zio.Task].exitCode
  }
}
```

### Converting Resource to ZManaged

To convert a Cats Effect's `Resource` to `ZManaged` we can use `cats.effect.Resource#toZManaged` extension method by importing `zio.interop.catz._` package and also we should provide an implicit instance of `Dispatcher`:
 
```scala
import zio.interop.catz._
import scala.concurrent.ExecutionContextExecutor

object ResourceToZManagedExample extends zio.App {
  implicit val ceRuntime: cats.effect.unsafe.IORuntime =
    cats.effect.unsafe.IORuntime.global
  implicit val ec: ExecutionContextExecutor =
    scala.concurrent.ExecutionContext.global

  implicit val dispatcher: cats.effect.std.Dispatcher[cats.effect.IO] =
    cats.effect.std
      .Dispatcher[cats.effect.IO]
      .allocated
      .unsafeRunSync()
      ._1

  def catsResource[F[_]: cats.effect.Sync]
      : cats.effect.Resource[F, java.io.InputStream] =
    cats.effect.Resource
      .fromAutoCloseable(
        cats.effect
          .Sync[F]
          .delay(
            java.nio.file.Files.newInputStream(
              java.nio.file.Paths.get("file.txt")
            )
          )
      )

  val myApp: zio.ZIO[zio.console.Console, Throwable, Unit] =
    catsResource[cats.effect.IO].toManaged
      .use { is =>
        zio.console.putStrLn(is.readAllBytes().length.toString)
      }
      
  override def run(args: List[String]): zio.URIO[zio.ZEnv, zio.ExitCode] =
    myApp.exitCode
}
```

## Cats Core

There is another package in `interop-cats` module called `zio.interop.catz.core._` which helps us to interop with core data types. This package contains instances of `zio.Chunk` data type for Cats Core module like `cats.Order`, `cats.Hash`, `cats.Traverse`, and so forth.

In the following example, we are going to use `zio.Chunk` in a Cats Effect application:

```scala mdoc:silent:nest
import cats.implicits._
import zio.interop.catz.core._

object ZioInteropWithCatsCore extends cats.effect.IOApp {
  val chunk = zio.Chunk("1", "2", "3", "4", "5")

  def parseInt(s: String): Option[Int] =
    Either.catchOnly[NumberFormatException](s.toInt).toOption

  val parseAll = cats.Traverse[zio.Chunk].traverse(chunk)(parseInt)

  override def run(args: List[String]): cats.effect.IO[cats.effect.ExitCode] =
    cats.effect.IO.println(parseAll).as(cats.effect.ExitCode.Success)
}
```

This package also contains utilities to support `zio.NonEmptyChunk` interoperability with Cats Core module.

## FS2 Streams

The `interop-cats` module contains extension methods to convert _FS2 Stream_ to _ZStream_ and vice versa. These methods support both FS2 series, 2.x and 3.x:

### From FS2 Stream to ZStream

By importing `zio.stream.interop.fs2z._` into our application, the `fs2.Stream#toZStream` extension method converts a `fs2.Stream` to `ZStream`:

```scala mdoc:silent:nest
import zio.stream.ZStream
import zio.stream.interop.fs2z._
val zstream: ZStream[Any, Throwable, Int] = fs2.Stream.range(1, 10).toZStream()
```
### From ZStream to FS2 Stream

Also, the `ZStream#toFs2Stream` converts a ZIO Stream into FS2 Stream:

```scala mdoc:silent:nest
import zio.stream.ZStream
import zio.Chunk
import zio.stream.interop.fs2z._
val fs2stream = ZStream.fromChunks(Chunk(1, 2, 3, 4)).toFs2Stream
```

## Using ZQueue with Cats Effect
The `interop-cats` library has an `import zio.interop.Queue` package to lift creation of `ZQueue` effect from `UIO[Queue[A]]` to `F[Queue[F, A]]` which enables us to run `ZQueue` under Cats Effect runtime. It supports all variants of `ZQueue` like `bounded`, `unbounded`, `sliding` and `dropping`.

```scala
def bounded[F[+_], A](capacity: Int)(implicit R: Runtime[ZEnv], F: LiftIO[F]): F[Queue[F, A]]
```

### Cats Effect 2.x

In the following example, we are going to lift the `ZQueue` creation effect to Cats `IO` effect. If we are integrating with 2.x Cats Effect library, this snippet works properly:

```scala
import zio.interop.Queue
import cats.effect.IO

implicit val runtime = Runtime.default
def liftedToIO: IO[List[Int]] = for {
  q <- Queue.bounded[IO, Int](100)
  _  <- q.offer(1)
  _  <- q.offer(2)
  r <- q.takeAll
} yield (r)
```

### Cats Effect 3.x

To run `ZQueue` with Cats Effect 3.x we also need to provide an instance of `Dispatcher` to our contextual environment:

```scala mdoc:silent:nest
import zio.interop.Queue

object ZioQueueInteropWithCats extends scala.App {

  implicit val ceRuntime: cats.effect.unsafe.IORuntime =
    cats.effect.unsafe.IORuntime.global

  implicit val zioRuntime: zio.Runtime[zio.ZEnv] =
    zio.Runtime.default

  implicit val ec: scala.concurrent.ExecutionContextExecutor =
    scala.concurrent.ExecutionContext.global

  implicit val dispatcher: cats.effect.std.Dispatcher[cats.effect.IO] =
    cats.effect.std
      .Dispatcher[cats.effect.IO]
      .allocated
      .unsafeRunSync()
      ._1

  def liftedToIO: cats.effect.IO[List[Int]] = for {
    q <- Queue.bounded[cats.effect.IO, Int](100)
    _ <- q.offer(1)
    _ <- q.offer(2)
    r <- q.takeAll
  } yield (r)

  val catsApp = liftedToIO
    .flatMap { e =>
      cats.effect.IO.println(s"List of elements retrieved from Queue: $e")
    }
    .as(cats.effect.ExitCode.Success)

  catsApp.unsafeRunSync()
}
```

## Using STM with Cats Effect

The `zio.interop.stm` provides a wrapper data type on `STM[Throwable, A]` which enables us to run `STM` with the Cats Effect library.

Currently, the `interop-cats` support `TRef`, `TPromise`,  `TQueue` and `TSemaphore` data types.

Let's try a working example using `STM` and `TRef`:

```scala mdoc:silent:nest
import cats.effect.IO
import cats.effect.unsafe.IORuntime
import zio.interop.stm.{STM, TRef}

implicit val zioRuntime: zio.Runtime[zio.ZEnv] = zio.Runtime.default
implicit val catsRuntime: IORuntime            = IORuntime.global

def transferMoney(
    from: TRef[IO, Long],
    to: TRef[IO, Long],
    amount: Long
): STM[IO, Long] =
  for {
    senderBal <- from.get
    _ <-
      if (senderBal < amount)
        STM.fail[IO](new Exception("Not enough money"))
      else
        STM.unit[IO]
    _       <- from.update(existing => existing - amount)
    _       <- to.update(existing => existing + amount)
    recvBal <- to.get
  } yield recvBal

val program: IO[Long] = for {
  sndAcc <- STM.atomically[cats.effect.IO, TRef[IO, Long]](
    TRef.make[IO, Long](1000)
  )
  rcvAcc <- STM.atomically[cats.effect.IO, TRef[IO, Long]](
    TRef.make[IO, Long](200)
  )
  recvAmt <- STM.atomically(transferMoney(sndAcc, rcvAcc, 500L))
} yield recvAmt

program
  .flatMap(amount =>
    IO.println(s"Balance of second account after transaction: $amount")
  )
  .unsafeRunSync()
```

## Examples

Cats Effect and Type-Level libraries are older than the ZIO ecosystem. So there are very nice libraries like doobie and http4s, that a ZIO user would like to use in its application.

We have provided some full working example of using these important libraries:

### Using ZIO with Doobie

The following example shows how to use ZIO with Doobie (a library for JDBC access) and FS2 (a streaming library), which both rely on Cats Effect instances:

```scala mdoc:silent:nest
// This snippet works with both CE2 and CE3
import doobie._
import doobie.implicits._
import fs2.Stream
import zio.Task
import zio.interop.catz._

implicit val zioRuntime: zio.Runtime[zio.ZEnv] = zio.Runtime.default

case class User(id: String, name: String, age: Int)

def xa: Transactor[Task] =
  Transactor.fromDriverManager[Task](
    "org.h2.Driver",
    "jdbc:h2:mem:users;DB_CLOSE_DELAY=-1"
  )

def createTable: doobie.ConnectionIO[Int] =
  sql"""|CREATE TABLE IF NOT EXISTS USERS(
        |id   INT SERIAL UNIQUE,
        |name VARCHAR NOT NULL UNIQUE,
        |age  SMALLINT
        |)""".stripMargin.update.run

def dropTable: doobie.ConnectionIO[Int] =
  sql"""DROP TABLE IF EXISTS USERS""".update.run

def insert(name: String, age: Int): doobie.ConnectionIO[Int] =
  sql"insert into users (name, age) values ($name, $age)".update.run

def loadUsers: Stream[doobie.ConnectionIO, User] =
  sql"""SELECT * FROM users""".query[User].stream

val doobieApp: Stream[doobie.ConnectionIO, User] = for {
  _ <- fs2.Stream.eval(dropTable)
  _ <- fs2.Stream.eval(createTable)
  _ <- fs2.Stream.eval(insert("Olivia", 21))
  _ <- fs2.Stream.eval(insert("Oliver", 30))
  u <- loadUsers
} yield u

val run: Stream[Task, User] = doobieApp.transact(xa)

val allUsers: List[User] =
  zioRuntime.unsafeRun(run.compile.toList)
```

Sounds good, but how can we specify a specialized transactor than the default one? Creating a customized transactor in CE2 differs from CE3. 

Let's try doing that in each of which:

#### Customized Transactor (CE2)

ZIO provides a specific blocking thread pool for blocking operations. The `doobie-hikari` module helps us create a transactor with two separated executors, one for blocking operations like JDBC operations, and the other one for non-blocking operations like performing awaiting connections to the database. So we shouldn't run blocking JDBC operations on the main thread pool. 

So let's fix this issue in the previous example. In the following snippet we are going to create a `ZMHikari` of Hikari transactor. In this example we are using `0.13.4` version of doobie which supports CE2:

```scala
import zio.ZManaged
import zio.blocking.Blocking
import zio.{ Runtime, Task, ZIO, ZManaged }
import doobie.hikari.HikariTransactor
import cats.effect.Blocker
import zio.interop.catz._

def transactor: ZManaged[Blocking, Throwable, HikariTransactor[Task]] =
  for {
    rt <- ZIO.runtime[Any].toManaged_
    be <- zio.blocking.blockingExecutor.toManaged_ // our blocking EC
    xa <- HikariTransactor
            .newHikariTransactor[Task](
              "org.h2.Driver",                      // driver classname
              "jdbc:h2:mem:test;DB_CLOSE_DELAY=-1", // connect URL
              "sa",                                 // username
              "",                                   // password
              rt.platform.executor.asEC,            // await connection here
              Blocker.liftExecutionContext(be.asEC) // execute JDBC operations here
            )
            .toManagedZIO
  } yield xa
```

Now we can `transact` our `doobieApp` with this `transactor` and convert that to the `ZIO` effect:

```scala
val zioApp: ZIO[Blocking, Throwable, List[User]] =
  transactor.use(xa => doobieApp.transact(xa).compile.toList)
```

#### Customized Transactor (CE3)

In Cats Effect 3.x, the `cats.effect.Blocker` has been removed. So the transactor constructor doesn't require us a blocking executor; it happens under the hood using the `Sync[F].blocking` operation.

To create a `Transactor` in CE3, we need to create an instance of `Dispatcher` for `zio.Task`. The following example is based on Doobie's `1.0.0-M5` version which supports CE3:

```scala mdoc:silent:nest
import doobie.hikari.HikariTransactor
import zio.blocking.Blocking
import zio.interop.catz._
import zio.{Task, ZIO, ZManaged}

implicit val zioRuntime: zio.Runtime[zio.ZEnv] =
  zio.Runtime.default

implicit val dispatcher: cats.effect.std.Dispatcher[zio.Task] =
  zioRuntime
    .unsafeRun(
      cats.effect.std
        .Dispatcher[zio.Task]
        .allocated
    )
    ._1

def transactor: ZManaged[Blocking, Throwable, HikariTransactor[Task]] =
  for {
    rt <- ZIO.runtime[Any].toManaged_
    xa <-
      HikariTransactor
        .newHikariTransactor[Task](
          "org.h2.Driver",                      // driver classname
          "jdbc:h2:mem:test;DB_CLOSE_DELAY=-1", // connect URL
          "sa",                                 // username
          "",                                   // password
          rt.platform.executor.asEC             // await connection here
        )
        .toManaged
  } yield xa
```

Now we can `transact` our `doobieApp` with this `transactor` and convert that to the `ZIO` effect:

```scala mdoc:silent:nest
val zioApp: ZIO[Blocking, Throwable, List[User]] =
  transactor.use(xa => doobieApp.transact(xa).compile.toList)
```

### Http4s

Here is the full working example of using http4s in ZIO App:

#### Cats Effect 2.x

The following example is based on http4s's `0.21.24` version which supports CE2:

```scala
import cats.effect.{ConcurrentEffect, Sync, Timer}
import cats.implicits._
import fs2.Stream
import org.http4s.HttpRoutes
import org.http4s.client.blaze.BlazeClientBuilder
import org.http4s.dsl.Http4sDsl
import org.http4s.server.blaze.BlazeServerBuilder
import zio.interop.catz._
import zio.interop.catz.implicits._ 
import zio.{Task, URIO}

import scala.concurrent.ExecutionContext.global

object ZioHttp4sInterop extends CatsApp {
  def run(args: List[String]): URIO[zio.ZEnv, zio.ExitCode] =
    stream[Task].compile.drain.exitCode
  
  def stream[F[_]: ConcurrentEffect: Timer]: Stream[F, Nothing] = {
    import org.http4s.implicits._
    val httpApp = helloWorldRoute[F].orNotFound
    for {
      _ <- BlazeClientBuilder[F](global).stream
      exitCode <- BlazeServerBuilder[F](global)
        .bindHttp(8080, "0.0.0.0")
        .withHttpApp(httpApp)
        .serve
    } yield exitCode
  }.drain

  def helloWorldRoute[F[_]: Sync]: HttpRoutes[F] = {
    val dsl = new Http4sDsl[F]{}
    import dsl._
    HttpRoutes.strict[F] {
      case GET -> Root =>
        Ok("Hello, World!")
    }
  }
}
```

#### Cats Effect 3.x

The following example is based on http4s's `0.23.0-RC1` version which supports CE3:

```scala mdoc:silent:reset
import cats.Applicative
import cats.effect.Async
import fs2.Stream
import org.http4s.HttpRoutes
import org.http4s.blaze.client.BlazeClientBuilder
import org.http4s.blaze.server.BlazeServerBuilder
import org.http4s.dsl.Http4sDsl
import zio.interop.catz._
import zio.{Task, URIO}

import scala.concurrent.ExecutionContext.global

object ZioHttp4sInterop extends zio.interop.catz.CatsApp {
  def stream[F[_]: Async]: Stream[F, Nothing] = {
    import org.http4s.implicits._
    val httpApp = helloWorldRoute[F].orNotFound
    for {
      _ <- BlazeClientBuilder[F](global).stream
      exitCode <- BlazeServerBuilder[F](global)
        .bindHttp(8080, "0.0.0.0")
        .withHttpApp(httpApp)
        .serve
    } yield exitCode
  }.drain

  def helloWorldRoute[F[_]: Applicative]: HttpRoutes[F] = {
    val dsl = new Http4sDsl[F] {}
    import dsl._
    HttpRoutes.strict[F] { case GET -> Root =>
      Ok("Hello, World!")
    }
  }

  def run(args: List[String]): URIO[zio.ZEnv, zio.ExitCode] =
    stream[Task].compile.drain.exitCode
}
```
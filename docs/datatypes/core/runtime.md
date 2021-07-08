---
id: runtime
title: "Runtime"
---
```scala mdoc:invisible
import zio.{Runtime, ZIO, UIO, URIO, Has, Task}
import zio.internal.Platform
```

A `Runtime[R]` is capable of executing tasks within an environment `R`.

To run an effect, we need a `Runtime`, which is capable of executing effects. Runtimes bundle a thread pool together with the environment that effects need.

## What is a Runtime System?

Whenever we write a ZIO program, we create a ZIO effect from ZIO constructors plus using its combinators. We are building a blueprint. ZIO effect is just a data structure that describes the execution of a concurrent program. So we end up with a tree data structure that contains lots of different data structures combined together to describe what the ZIO effect should do. This data structure doesn't do anything, it is just a description of a concurrent program.

So the most thing we should keep in mind when we are working with a functional effect system like ZIO is that when we are writing code, printing a string onto the console, reading a file, querying a database, and so forth; We are just writing a workflow or blueprint of an application. We are just building a data structure.

So how ZIO run these workflows? This is where ZIO Runtime System comes into play. Whenever we run an `unsaferun` function, the Runtime System is responsible to step through all the instructions described by the ZIO effect and execute them.

To simplify everything, we can think of a Runtime System like a black box that takes both the ZIO effect (`ZIO[R, E, A]`) and its environment (`R`), it will run this effect and then will return its result as an `Either[E, A]` value.


![ZIO Runtime System](assets/zio-runtime-system.svg)

## Responsibilities of the Runtime System

Runtime Systems have a lot of responsibilities:

1. **Execute every step of the blueprint** — They have to execute every step of the blueprint in a while loop until it's done.

2. **Handle unexpected errors** — They have to handle unexpected errors, not just the expected ones but also the unexpected ones. 

3. **Spawn concurrent fiber** — They are actually responsible for the concurrency that effect systems have. They have to spawn a fiber every time we call `fork` on an effect to spawn off a new fiber.

4. **Cooperatively yield to other fibers** — They have to cooperatively yield to other fibers so that fibers that are sort of hogging the spotlight, don't get to monopolize all the CPU resources. They have to make sure that the fibers split the CPU cores among all the fibers that are working.

5. **Capture execution and stack traces** — They have to keep track of where we are in the progress of our own user-land code so the nice detailed execution traces can be captured. 

6. **Ensure finalizers are run appropriately** — They have to ensure finalizers are run appropriately at the right point in all circumstances to make sure that resources are closed that clean-up logic is executed. This is the feature that powers ZManaged and all the other resource-safe constructs in ZIO.

7. **Handle asynchronous callback** — They have to handle this messy job of dealing with asynchronous callbacks. So we don't have to deal with async code. When we are doing ZIO, everything is just async out of the box. 

## Running a ZIO Effect

There are two ways to run ZIO effect:
1. **Using `zio.App` entry point**
2. **Using `unsafeRun` method directly**

### Using zio.App

In most cases we use this method to run our ZIO effect. `zio.App` has a `run` function which is the main entry point for running a ZIO application on the JVM:

```scala
package zio
trait App {
  def run(args: List[String]): URIO[ZEnv, ExitCode]
}
```

Assume we have written an effect using ZIO:

```scala mdoc:silent
import zio.console._

def myAppLogic =
  for {
    _ <- putStrLn("Hello! What is your name?")
    n <- getStrLn
    _ <- putStrLn("Hello, " + n + ", good to meet you!")
  } yield ()
```

Now we can run that effect using `run` entry point:

```scala mdoc:silent
object MyApp extends zio.App {
  final def run(args: List[String]) =
    myAppLogic.exitCode
}
```

### Using unsafeRun

Another way to execute ZIO effect is to feed the ZIO effect to the `unsafeRun` method of Runtime system:

```scala mdoc:silent:nest
object RunZIOEffectUsingUnsafeRun extends scala.App {
  zio.Runtime.default.unsafeRun(
    myAppLogic
  )
}
```

We don't usually use this method to run our effects. One of the use cases of this method is when we are integrating the legacy (non-effectful code) with the ZIO effect. It also helps us to refactor a large legacy code base into a ZIO effect gradually; Assume we have decided to refactor a component in the middle of a legacy code and rewrite that with ZIO. We can start rewriting that component with the ZIO effect and then integrate that component with the existing code base, using the `unsafeRun` function.

## Default Runtime

ZIO contains a default runtime called `Runtime.default`, configured with the `ZEnv` (the default ZIO environment) and a default `Platform` designed to work well for mainstream usage. It is already implemented as below:

```scala
object Runtime {
  lazy val default: Runtime[ZEnv] = Runtime(ZEnv.Services.live, Platform.default)
}
```

The default runtime includes a default `Platform` which contains minimum capabilities to bootstrap execution of ZIO tasks and live (production) versions of all ZIO built-in services. The default ZIO environment (`ZEnv`) for the `JS` platform includes `Clock`, `Console`, `System`, `Random`; and the `JVM` platform also has a `Blocking` service:

```scala
// Default JS environment
type ZEnv = Clock with Console with System with Random

// Default JVM environment
type ZEnv = Clock with Console with System with Random with Blocking
```

We can easily access the default `Runtime` to run an effect:

```scala mdoc:silent:nest
object MainApp extends scala.App {
  val runtime = Runtime.default
  runtime.unsafeRun(myAppLogic)
}
```

## Custom Runtime

Sometimes we need to create a custom `Runtime` with a user-defined environment and user-specified `Platform`. Many real applications should not use `Runtime.default`. Instead, they should make their own `Runtime` which configures the `Platform` and environment accordingly.

Some use-cases of custom Runtimes:

### Providing Environment to Runtime System

The custom runtime can be used to run many different effects that all require the same environment, so we don't have to call `ZIO#provide` on all of them before we run them.

For example, assume we want to create a `Runtime` for services that are for testing purposes, and they don't interact with real external APIs. So we can create a runtime, especially for testing.

Let's say we have defined two `Logging` and `Email` services:

```scala mdoc:silent:nest
trait Logging {
  def log(line: String): UIO[Unit]
}

object Logging {
  def log(line: String): URIO[Has[Logging], Unit] =
    ZIO.serviceWith[Logging](_.log(line))
}

trait Email {
  def send(user: String, content: String): Task[Unit]
}

object Email {
  def send(user: String, content: String): ZIO[Has[Email], Throwable, Unit] =
    ZIO.serviceWith[Email](_.send(user, content))
}
```

We are going to implement a live version of `Logging` service and also a mock version of `Email` service for testing:

```scala mdoc:silent:nest
case class LoggingLive() extends Logging {
  override def log(line: String): UIO[Unit] =
    ZIO.effectTotal(print(line))
}

case class EmailMock() extends Email {
  override def send(user: String, content: String): Task[Unit] =
    ZIO.effect(println(s"sending email to $user"))
}
```

Let's create a custom runtime that contains these two service implementations in its environment:

```scala mdoc:silent:nest
val testableRuntime = Runtime(
  Has.allOf[Logging, Email](LoggingLive(), EmailMock()),
  Platform.default
)
```

Also, we can map the default runtime to the new runtime, so we can append new services to the default ZIO environment:

```scala mdoc:silent:nest
val testableRuntime: Runtime[zio.ZEnv with Has[Logging] with Has[Email]] =
  Runtime.default
    .map((zenv: zio.ZEnv) =>
      zenv ++ Has.allOf[Logging, Email](LoggingLive(), EmailMock())
    )
```

Now we can run our effects using this custom `Runtime`:

```scala mdoc:silent:nest
testableRuntime.unsafeRun(
  for {
    _ <- Logging.log("sending newsletter")
    _ <- Email.send("David", "Hi! Here is today's newsletter.")
  } yield ()
)
```

### Application Monitoring

Sometimes to diagnose runtime issues and understand what is going on in our application we need to add some sort of monitoring task to the Runtime System. It helps us to track fibers and their status.

By adding a `Supervisor` to the current platform of the Runtime System, we can track the activity of fibers in a program. So every time a fiber gets started, forked, or every time a fiber ends its life, all these contextual pieces of information get reported to that `Supervisor`.

For example, the [ZIO ZMX](https://zio.github.io/zio-zmx/) enables us to monitor our ZIO application. To include that in our project we must add the following line to our `build.sbt`:

```scala
libraryDependencies += "dev.zio" %% "zio-zmx" % "0.0.6"
```

ZIO ZMX has a specialized `Supervisor` called `ZMXSupervisor` that can be added to our existing `Runtime`:

```scala
import zio._
import zio.console._
import zio.zmx._
import zio.zmx.diagnostics._

val program: ZIO[Console, Throwable, Unit] =
  for {
    _ <- putStrLn("Waiting for input")
    a <- getStrLn
    _ <- putStrLn("Thank you for " + a)
  } yield ()

val diagnosticsLayer: ZLayer[ZEnv, Throwable, Has[Diagnostics]] =
  Diagnostics.make("localhost", 1111)

val runtime: Runtime[ZEnv] =
  Runtime.default.mapPlatform(_.withSupervisor(ZMXSupervisor))

runtime.unsafeRun(program.provideCustomLayer(diagnosticsLayer))
```

### Application Tracing

We can enable or disable execution tracing or configure its setting. Execution tracing has full of junk. There are lots of allocations that all need to be garbage collected afterward. So it has a tremendous impact on the complexity of the application runtime.

Users often turn off tracing in critical areas of their application. Also, when we are doing benchmark operation, it is better to create a `Runtime` without tracing capability:

```scala mdoc:silent:nest
import zio.internal.Tracing
import zio.internal.tracing.TracingConfig

val rt1 = Runtime.default.mapPlatform(_.withTracing(Tracing.disabled))
val rt2 = Runtime.default.mapPlatform(_.withTracing(Tracing.enabledWith(TracingConfig.stackOnly)))

val config = TracingConfig(
  traceExecution = true,
  traceEffectOpsInExecution = true,
  traceStack = true,
  executionTraceLength = 100,
  stackTraceLength = 100,
  ancestryLength = 10,
  ancestorExecutionTraceLength = 10,
  ancestorStackTraceLength = 10
)
val rt3 = Runtime.default.mapPlatform(_.withTracingConfig(config))
```

### User-defined Executor

An executor is responsible for executing effects. The way how each effect will be run including detail of threading, scheduling, and so forth, is separated from the caller. So, if we need to have a specialized executor according to our requirements, we can provide that to the ZIO `Runtime`:

```scala mdoc:silent:nest
import zio.internal.Executor
import java.util.concurrent.{ThreadPoolExecutor, TimeUnit, LinkedBlockingQueue}

val runtime = Runtime.default.mapPlatform(
  _.withExecutor(
    Executor.fromThreadPoolExecutor(_ => 1024)(
      new ThreadPoolExecutor(
        5,
        10,
        5000,
        TimeUnit.MILLISECONDS,
        new LinkedBlockingQueue[Runnable]()
      )
    )
  )
)
```

### Benchmarking

To do benchmark operation, we need a `Runtime` with settings suitable for that. It would be better to disable tracing and auto-yielding. ZIO has a built-in `Platform` proper for benchmark operations, called `Platform.benchmark` which we can map the default `Platform` to the benchmark version:

```scala mdoc:silent:nest
val benchmarkRuntime = Runtime.default.mapPlatform(_ => Platform.benchmark)
```

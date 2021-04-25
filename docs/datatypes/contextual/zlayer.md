---
id: zlayer
title: "ZLayer"
---

A `ZLayer[A, E, B]` describes a layer of an application: every layer in an application requires some services (the input) and produces some services (the output). 

Layers can be thought of as recipes for producing bundles of services, given their dependencies (other services).

Construction of layers can be effectful and utilize resources that must be acquired and safely released when the services are done being utilized.

Layers are shared by default, meaning that if the same layer is used twice, the layer will only be allocated a single time. 

Because of their excellent composition properties, layers are the idiomatic way in ZIO to create services that depend on other services.

For example, the `ZLayer[Blocking with Logging with Database, Throwable, UserRepoService]` is a recipe for building a service that requires `Blocking`, `Logging` and `Database` service, and it produces a `UserRepoService` service.

## Creation

### From Simple Values

With `ZLayer.succeed` we can construct a `ZLayer` from a value. It returns a `ULayer[Has[A]]` value, which represents a layer of application that _has_ a service of type `A`:

```scala
def succeed[A: Tag](a: A): ULayer[Has[A]]
```

In the following example, we are going to create a `nameLayer` that provides us the name of `Adam`.

```scala mdoc:invisible
import zio._
```

```scala mdoc:silent
val nameLayer: ULayer[Has[String]] = ZLayer.succeed("Adam")
```

In most cases, we use `ZLayer.succeed` to provide a layer of service of type `A`.

For example, assume we have written the following service:

```scala mdoc:silent
object terminal {
  type Terminal = Has[Terminal.Service]

  object Terminal {
    trait Service {
      def putStrLn(line: String): UIO[Unit]
    }

    object Service {
      val live: Service = new Service {
        override def putStrLn(line: String): UIO[Unit] =
          ZIO.effectTotal(println(line))
      }
    }
  }
}
```

Now we can create a `ZLayer` from the `live` version of this service:

```scala mdoc:silent
import terminal._
val live: ZLayer[Any, Nothing, Terminal] = ZLayer.succeed(Terminal.Service.live)
```

### From Managed Resources

We can lift any `ZManaged` to `ZLayer` by providing a managed resource to the `ZIO.fromManaged` constructor:

```scala mdoc:invisible
import scala.io.BufferedSource
```

```scala mdoc:silent:nest
val managedFile = ZManaged.fromAutoCloseable(
  ZIO.effect(scala.io.Source.fromFile("file.txt"))
)
val fileLayer: ZLayer[Any, Throwable, Has[BufferedSource]] = 
  ZLayer.fromManaged(managedFile)
```

Also, every `ZManaged` can be converted to `ZLayer` by calling `ZLayer#toLayer`:

```scala mdoc:silent:nest
val fileLayer: ZLayer[Any, Throwable, Has[BufferedSource]] = managedFile.toLayer
```

Let's see another real-world example of creating a layer from managed resources. Assume we have written a managed `UserRepository`:

```scala mdoc:invisible:reset
import zio._
import zio.blocking._
import zio.console._
import scala.io.Source._
import java.io.{FileInputStream, FileOutputStream, Closeable}

trait DBConfig
trait Transactor

def dbConfig: Task[DBConfig] = Task.effect(???)
def initializeDb(config: DBConfig): Task[Unit] = Task.effect(???)
def makeTransactor(config: DBConfig): ZManaged[Blocking, Throwable, Transactor] = ???

case class UserRepository(xa: Transactor)
object UserRepository {
  def apply(xa: Transactor): UserRepository = new UserRepository(xa) 
}
```

```scala mdoc:silent:nest
def userRepository: ZManaged[Blocking with Console, Throwable, UserRepository] = for {
  cfg <- dbConfig.toManaged_
  _ <- initializeDb(cfg).toManaged_
  xa <- makeTransactor(cfg)
} yield new UserRepository(xa)
```

We can convert that to `ZLayer` with `ZLayer.fromManaged` or `ZManaged#toLayer`:

```scala mdoc:nest
val usersLayer  = userRepository.toLayer
val usersLayer_ = ZLayer.fromManaged(userRepository)
```

Also, we can create a `ZLayer` directly from `acquire` and `release` actions of a managed resource:

```scala mdoc:nest
def acquire = ZIO.effect(new FileInputStream("file.txt"))
def release(resource: Closeable) = ZIO.effectTotal(resource.close())

val inputStreamLayer = ZLayer.fromAcquireRelease(acquire)(release)
```

### From ZIO Effects

We can create `ZLayer` from any `ZIO` effect by using `ZLayer.fromEffect` constructor, or calling `ZIO#toLayer` method:

```scala mdoc
val layer = ZLayer.fromEffect(ZIO.succeed("Hello, World!"))
val layer_ = ZIO.succeed("Hello, World!").toLayer
```

Assume we have a `ZIO` effect that read the application config from a file, we can create a layer from that:

```scala mdoc:invisible
trait AppConfig
```

```scala mdoc:nest
def loadConfig: Task[AppConfig] = Task.effect(???)
val configLayer = ZLayer.fromEffect(loadConfig)
```

### From another Service

Every `ZLayer` describes an application that requires some services as input and produces some services as output. Sometimes when we are writing a new layer, we may need to access and depend on one or several services.

The `ZLayer.fromService` construct a layer that purely depends on the specified service:

```scala
def fromService[A: Tag, B: Tag](f: A => B): ZLayer[Has[A], Nothing, Has[B]]
```

Assume we want to write a `live` version of the following logging service:

```scala mdoc:silent:nest
object logging {
  type Logging = Has[Logging.Service]

  object Logging {
    trait Service {
      def log(msg: String): UIO[Unit]
    }
  }
}
```

We can create that by using `ZLayer.fromService` constructor, which depends on `Console` service:

```scala mdoc:invisible
import logging.Logging
import logging.Logging._
```

```scala mdoc:silent:nest
val live: ZLayer[Console, Nothing, Logging] = ZLayer.fromService(console =>
  new Service {
    override def log(msg: String): UIO[Unit] = console.putStrLn(msg)
  }
)
```

## Examples

### The simplest ZLayer application

This application demonstrates a ZIO program with a single dependency on a simple string value:

```scala mdoc:silent
import zio._

object Example extends zio.App {

  // Define our simple ZIO program
  val zio: ZIO[Has[String], Nothing, Unit] = for {
    name <- ZIO.access[Has[String]](_.get)
    _    <- UIO(println(s"Hello, $name!"))
  } yield ()

  // Create a ZLayer that produces a string and can be used to satisfy a string
  // dependency that the program has
  val nameLayer: ULayer[Has[String]] = ZLayer.succeed("Adam")

  // Run the program, providing the `nameLayer`
  def run(args: List[String]): URIO[ZEnv, ExitCode] =
    zio.provideLayer(nameLayer).as(ExitCode.success)
}

```

### ZLayer application with dependencies 

In the following example, our ZIO application has several dependencies:
 - `zio.clock.Clock`
 - `zio.console.Console`
 - `ModuleB`

`ModuleB` in turn depends upon `ModuleA`:

```scala mdoc:silent
import zio._
import zio.clock._
import zio.console._
import zio.duration.Duration._
import java.io.IOException

object moduleA {
  type ModuleA = Has[ModuleA.Service]

  object ModuleA {
    trait Service {
      def letsGoA(v: Int): UIO[String]
    }

    val any: ZLayer[ModuleA, Nothing, ModuleA] =
      ZLayer.requires[ModuleA]

    val live: Layer[Nothing, Has[Service]] = ZLayer.succeed {
      new Service {
        def letsGoA(v: Int): UIO[String] = UIO(s"done: v = $v ")
      }
    }
  }

  def letsGoA(v: Int): URIO[ModuleA, String] =
    ZIO.accessM(_.get.letsGoA(v))
}

import moduleA._

object moduleB {
  type ModuleB = Has[ModuleB.Service]

  object ModuleB {
    trait Service {
      def letsGoB(v: Int): UIO[String]
    }

    val any: ZLayer[ModuleB, Nothing, ModuleB] =
      ZLayer.requires[ModuleB]

    val live: ZLayer[ModuleA, Nothing, ModuleB] = ZLayer.fromService { (moduleA: ModuleA.Service) =>
      new Service {
        def letsGoB(v: Int): UIO[String] =
          moduleA.letsGoA(v)
      }
    }
  }

  def letsGoB(v: Int): URIO[ModuleB, String] =
    ZIO.accessM(_.get.letsGoB(v))
}

object ZLayerApp0 extends zio.App {

  import moduleB._

  val env = Console.live ++ Clock.live ++ (ModuleA.live >>> ModuleB.live)
  val program: ZIO[Console with Clock with moduleB.ModuleB, IOException, Unit] =
    for {
      _ <- putStrLn(s"Welcome to ZIO!")
      _ <- sleep(Finite(1000))
      r <- letsGoB(10)
      _ <- putStrLn(r)
    } yield ()

  def run(args: List[String]) =
    program.provideLayer(env).exitCode

}

// output: 
// [info] running ZLayersApp 
// Welcome to ZIO!
// done: v = 10 
```

### ZLayer example with complex dependencies

In this example, we can see that `ModuleC` depends upon `ModuleA`, `ModuleB`, and `Clock`. The layer provided to the runnable application shows how dependency layers can be combined using `++` into a single combined layer. The combined layer will then be able to produce both of the outputs of the original layers as a single layer:

```scala mdoc:silent
import zio._
import zio.clock._

object ZLayerApp1 extends scala.App {
  val rt = Runtime.default

  type ModuleA = Has[ModuleA.Service]

  object ModuleA {

    trait Service {}

    val any: ZLayer[ModuleA, Nothing, ModuleA] =
      ZLayer.requires[ModuleA]

    val live: ZLayer[Any, Nothing, ModuleA] =
      ZLayer.succeed(new Service {})
  }

  type ModuleB = Has[ModuleB.Service]

  object ModuleB {

    trait Service {}

    val any: ZLayer[ModuleB, Nothing, ModuleB] =
      ZLayer.requires[ModuleB]

    val live: ZLayer[Any, Nothing, ModuleB] =
      ZLayer.succeed(new Service {})
  }

  type ModuleC = Has[ModuleC.Service]

  object ModuleC {

    trait Service {
      def foo: UIO[Int]
    }

    val any: ZLayer[ModuleC, Nothing, ModuleC] =
      ZLayer.requires[ModuleC]

    val live: ZLayer[ModuleA with ModuleB with Clock, Nothing, ModuleC] =
      ZLayer.succeed {
        new Service {
          val foo: UIO[Int] = UIO.succeed(42)
        }
      }

    val foo: URIO[ModuleC, Int] =
      ZIO.accessM(_.get.foo)
  }

  val env = (ModuleA.live ++ ModuleB.live ++ ZLayer.identity[Clock]) >>> ModuleC.live

  val res = ModuleC.foo.provideCustomLayer(env)

  val out = rt.unsafeRun(res)
  println(out)
  // 42
}
```
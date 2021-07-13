---
id: index
title: "Introduction"
---

## ZIO Environment

The `ZIO[-R, +E, +A]` data type describes an effect that requires an input type of `R`, as an environment, may fail with an error of type `E` or succeed and produces a value of type `A`.

The input type is also known as _environment type_. This type-parameter indicates that to run an effect we need one or some services as an environment of that effect. In other word, `R` represents the _requirement_ for the effect to run, meaning we need to fulfill the requirement in order to make the effect _runnable_.

`R` represents dependencies; whatever services, config, or wiring a part of a ZIO program depends upon to work. We will explore what we can do with `R`, as it plays a crucial role in `ZIO`.

For example, when we have `ZIO[Console, Nothing, Unit]`, this shows that to run this effect we need to provide an implementation of the `Console` service:
```scala mdoc:invisible
import zio.ZIO
import zio.console._
```

```scala mdoc:silent
val effect: ZIO[Console, Nothing, Unit] = putStrLn("Hello, World!").orDie
```

So finally when we provide a live version of `Console` service to our `effect`, it will be converted to an effect that doesn't require any environmental service:

```scala mdoc:silent
val mainApp: ZIO[Any, Nothing, Unit] = effect.provideLayer(Console.live)
```

Finally, to run our application we can put our `mainApp` inside the `run` method:

```scala mdoc:silent:nest
import zio.{ExitCode, ZEnv, ZIO}
import zio.console._

object MainApp extends zio.App {
  val effect: ZIO[Console, Nothing, Unit] = putStrLn("Hello, World!").orDie
  val mainApp: ZIO[Any, Nothing, Unit] = effect.provideLayer(Console.live)

  override def run(args: List[String]): ZIO[ZEnv, Nothing, ExitCode] = 
    mainApp.exitCode
}
```

Sometimes an effect needs more than one environmental service, it doesn't matter, in these cases, we compose all dependencies by `++` operator:

```scala mdoc:silent:nest
import zio.console._
import zio.random._

val effect: ZIO[Console with Random, Nothing, Unit] = for {
  r <- nextInt
  _ <- putStrLn(s"random number: $r").orDie
} yield ()

val mainApp: ZIO[Any, Nothing, Unit] = effect.provideLayer(Console.live ++ Random.live)
```

We don't need to provide live layers for built-in services (don't worry, we will discuss layers later in this page). ZIO has a `ZEnv` type alias for the composition of all ZIO built-in services (Clock, Console, System, Random, and Blocking). So we can run the above `effect` as follows:

```scala mdoc:silent:nest
import zio.console._
import zio.random._
import zio.{ExitCode, ZEnv, ZIO}

object MainApp extends zio.App {
  val effect: ZIO[Console with Random, Nothing, Unit] = for {
    r <- nextInt
    _ <- putStrLn(s"random number: $r").orDie
  } yield ()

  override def run(args: List[String]): ZIO[ZEnv, Nothing, ExitCode] =
    effect.exitCode
}
```

ZIO environment facility enables us to:

1. **Code to Interface** — like object-oriented paradigm, in ZIO we encouraged to code to interfaces and defer the implementation. It is the best practice, but ZIO does not enforce us to do that.

2. **Write a Testable Code** — By coding to an interface, whenever we want to test our effects, we can easily mock any external services, by providing a _test_ version of those instead of the `live` version.

## Contextual Data Types

Defining service in ZIO is not very different from object-oriented style, it has the same principle; coding to an interface, not an implementation. But the way ZIO encourages us to implement this principle by using _Module Pattern_ which doesn't very differ from the object-oriented style. 

ZIO have two data type that plays a key role in writing ZIO services using _Module Pattern_: 
1. Has
2. ZLayer

So, before diving into the _Module Patter_, We need to learn more about ZIO Contextual Data Types. Let's review each of them:

### Has

`Has[A]` represents a dependency on a service of type `A`, e.g. Has[Logging]. Some components in an application might depend upon more than one service. 

ZIO wrap services with `Has` data type to:

1. **Wire/bind** services into their implementations. This data type has an internal map to maintain this binding. 

2. **Combine** multiple services together. Two or more `Has[_]` elements can be combined _horizontally_ using their `++` operator.


### ZLayer

`ZLayer[-RIn, +E, +ROut]` is a recipe to build an environment of type `ROut`, starting from a value `RIn`, and possibly producing an error `E` during creation.

We can compose `layerA` and `layerB` _horizontally_ to build a layer that has the requirements of both layers, to provide the capabilities of both layers, through `layerA ++ layerB`

We can also compose layers _vertically_, meaning the output of one layer is used as input for the subsequent layer to build the next layer, resulting in one layer with the requirement of the first, and the output of the second layer: `layerA >>> layerB`. When doing this, the first layer must output all the services required by the second layer, but we can defer creating some of these services and require them as part of the input of the final layer using `ZLayer.identity`.  

## Defining Services in OOP

Before diving into writing services in ZIO style, let's review how we define them in object-oriented fashion:

1. **Service Definition** — In object-oriented programming, we define services with traits. A service is a bundle of related functionality which are defined in a trait:

```scala mdoc:silent:nest
trait FooService {

}
```

2. **Service Implementation** — We implement these services by using classes:

```scala mdoc:silent:nest
class FooServiceImpl extends FooService {
    
}
```

3. **Defining Dependencies** — If the creation of a service depends on other services, we can define these dependencies by using constructors:

```scala mdoc:silent:nest
trait ServiceA {

}

trait ServiceB {

}

class FooServiceImpl(a: ServiceA, b: ServiceB) {

}
```

In object-oriented programming, the best practice is to _program to an interface, not an implementation_. So in the previous example, `ServiceA` and `ServiceB` are interfaces, not concrete classes. 

4. **Injecting Dependencies** — Now, the client of `FooServiceImpl` service can provide its own implementation of `ServiceA` and `ServiceB`, and inject them to the `FooServiceImpl` constructor:

```scala mdoc:silent:nest
class ServiceAImpl extends ServiceA
class ServiceBImpl extends ServiceB
val fooService = new FooServiceImpl(new ServiceAImpl, new ServiceBImpl)
```

Sometimes, as the number of dependent services grows and the dependency graph of our application becomes complicated, we need an automatic way of wiring and providing dependencies into the services of our application. In these situations, we might use a dependency injection framework to do all its magic machinery for us.

## Defining Services in ZIO

A service is a group of functions that deals with only one concern. Keeping the scope of each service limited to a single responsibility improves our ability to understand code, in that we need to focus only on one topic at a time without juggling too many concepts together in our head.

`ZIO` itself provides the basic capabilities through modules, e.g. see how `ZEnv` is defined.

In the functional Scala as well as in object-oriented programming the best practice is to _Program to an Interface, Not an Implementation_. This is the most important design principle in software development and helps us to write maintainable code by:

* Allowing the client to hold an interface as a contract and don't worry about the implementation. The interface signature determines all operations that should be done. 

* Enabling a developer to write more testable programs. When we write a test for our business logic we don't have to run and interact with real services like databases which makes our test run very slow. If our code is correct our test code should always pass, there should be no hidden variables or depend on outside sources. We can't know that the database is always running correctly. We don't want to fail our tests because of the failure of external service.

* Providing the ability to write more modular applications. So we can plug in different implementations for different purposes without a major modification.

It is not mandatory but ZIO encourages us to follow this principle by bundling related functionality as an interface by using _Module Pattern_. 

The core idea is that a layer depends upon the interfaces exposed by the layers immediately below itself, but is completely unaware of its dependencies' internal implementations.

In object-oriented programming:

- **Service Definition** is done by using _interfaces_ (Scala trait or Java Interface).
- **Service Implementation** is done by implementing interfaces using _classes_ or creating _new object_ of the interface.
- **Defining Dependencies** is done by using _constructors_. They allow us to build classes, give their dependencies. This is called constructor-based dependency injection.

We have a similar analogy in Module Pattern, except instead of using _constructors_ we use **`ZLayer`** to define dependencies. So in ZIO fashion, we can think of `ZLayer` as a service constructor.

ZIO has two patterns to write services. The first version of _Module Pattern_ has some boilerplate, but the second version is very concise and straightforward. ZIO doesn't mandate any of them, you can use whichever you like.

### Module Pattern 1.0

Let's start learning this pattern by writing a `Logging` service:

1. **Bundling** — Define an object that gives the name to the module, this can be (not necessarily) a package object. We create a `logging` object, all the definitions and implementations will be included in this object.

2. **Wrapping Service Type Definition with `Has[_]` Data Type** — At the first step, we create a package object of `logging`, and inside that we define the `Logging` module as a type alias for `Has[Logging.Service]`.

3. **Service Definition** — Then we create the `Logging` companion object. Inside the companion object, we define the service definition with a trait named `Service`. Traits are how we define services. A service could be all the stuff that is related to one concept with singular responsibility.

4. **Service Implementation** — After that, we implement our service by creating a new Service and then lifting that entire implementation into the `ZLayer` data type by using the `ZLayer.succeed` constructor.

5. **Defining Dependencies** — If our service has a dependency on other services, we should use constructors like `ZLayer.fromService` and `ZLayer.fromServices`.

6. **Accessor Methods** — Finally, to create the API more ergonomic, it's better to write accessor methods for all of our service methods. 

Accessor methods allow us to utilize all the features inside the service through the ZIO Environment. That means, if we call `log`, we don't need to pull out the `log` function from the ZIO Environment. The `accessM` method helps us to access the environment of effect and reduce the redundant operation, every time.

```scala mdoc:invisible:reset
import zio._
import zio.console._
```

```scala mdoc:invisible
import zio.{Has, UIO, Layer, ZLayer, ZIO, URIO}
```

```scala mdoc:silent:nest
object logging {
  type Logging = Has[Logging.Service]

  // Companion object exists to hold service definition and also the live implementation.
  object Logging {
    trait Service {
      def log(line: String): UIO[Unit]
    }

    val live: ULayer[Logging] = ZLayer.succeed {
      new Service {
        override def log(line: String): UIO[Unit] =
          ZIO.effectTotal(println(line))
      }
    }
  }

  // Accessor Methods
  def log(line: => String): URIO[Logging, Unit] =
    ZIO.accessM(_.get.log(line))
}
```

We might need `Console` and `Clock` services to implement the `Logging` service. In this case, we use `ZLayer.fromServices` constructor:

```scala mdoc:invisible
import zio.clock.Clock
```

```scala mdoc:silent:nest
object logging {
  type Logging = Has[Logging.Service]

  // Companion object exists to hold service definition and also the live implementation.
  object Logging {
    trait Service {
      def log(line: String): UIO[Unit]
    }

    val live: URLayer[Clock with Console, Logging] =
      ZLayer.fromServices[Clock.Service, Console.Service, Logging.Service] {
        (clock: Clock.Service, console: Console.Service) =>
          new Service {
            override def log(line: String): UIO[Unit] =
              for {
                current <- clock.currentDateTime.orDie
                _ <- console.putStrLn(current.toString + "--" + line).orDie
              } yield ()
          }
      }
  }

  // Accessor Methods
  def log(line: => String): URIO[Logging, Unit] =
    ZIO.accessM(_.get.log(line))
}
```


This is how ZIO services are created. Let's use the `Logging` service in our application:

```scala mdoc:silent:nest
object LoggingExample extends zio.App {
  import zio.RIO
  import logging._
 
  private val app: RIO[Logging, Unit] = log("Hello, World!") 

  override def run(args: List[String]) = 
    app.provideLayer(Logging.live).exitCode
}
```

During writing an application we don't care which implementation version of the `Logging` service will be injected into our `app`, later at the end of the day, it will be provided by methods like `provideLayer`.

### Module Pattern 2.0

Writing services with _Module Pattern 2.0_ is much easier than the previous one. It removes some level of indirection from the previous version, and much more similar to the object-oriented approach in writing services.

_Module Pattern 2.0_ has more similarity with object-oriented way of defining services. We use classes to implement services, and we use constructors to define service dependencies; At the end of the day, we lift class constructor into the `ZLayer`.

1. **Service Definition** — Defining service in this version has changed slightly compared to the previous version. We would take the service definition and pull it out into the top-level:

```scala mdoc:invisible:reset
import zio._
```

```scala mdoc:silent
trait Logging {
  def log(line: String): UIO[Unit]
}
```

2. **Service Implementation** — It is the same as what we did in object-oriented fashion. We implement the service with Scala class. By convention, we name the live version of its implementation as `LoggingLive`:

```scala mdoc:silent:nest
case class LoggingLive() extends Logging {
  override def log(line: String): UIO[Unit] = 
    ZIO.effectTotal(print(line))
}
```

3. **Define Service Dependencies** — We might need `Console` and `Clock` services to implement the `Logging` service. In this case, we put its dependencies into its constructor. All the dependencies are just interfaces, not implementation. Just like what we did in object-oriented style:

```scala mdoc:invisible:reset
import java.time._
import zio._

trait Logging {
  def log(line: String): UIO[Unit]
}
```

```scala mdoc:silent
import zio.console.Console
import zio.clock.Clock
case class LoggingLive(console: Console.Service, clock: Clock.Service) extends Logging {
  override def log(line: String): UIO[Unit] = 
    for {
      current <- clock.currentDateTime.orDie
      _       <- console.putStrLn(current.toString + "--" + line).orDie
    } yield ()
}
```

4. **Defining ZLayer** — Now, we create a companion object for `LoggingLive` data type and lift the service implementation into the `ZLayer`:

```scala mdoc:silent
object LoggingLive {
  val layer: URLayer[Has[Console.Service] with Has[Clock.Service], Has[Logging]] =
    (LoggingLive(_, _)).toLayer
}
```

5. **Accessor Methods** — Finally, to create the API more ergonomic, it's better to write accessor methods for all of our service methods. Just like what we did in Module Pattern 1.0, but with a slight change, in this case, instead of using `ZIO.accessM` we use `ZIO.serviceWith` method to define accessors inside the service companion object:

```scala mdoc:silent
object Logging {
  def log(line: String): URIO[Has[Logging], Unit] = ZIO.serviceWith[Logging](_.log(line))
}
```

That's it! Very simple! ZIO encourages us to follow some of the best practices in object-oriented programming. So it doesn't require us to throw away all our object-oriented knowledge. 

> **Note:**
>
> In _Module Pattern 2.0_ we don't use type aliases for Has wrappers, like `type Logging = Has[Logging.Service]`. So unlike the previous pattern, we encourage using explicitly the `Has` wrappers whenever we want to specify the dependency on a service.
>
> So instead of writing `ZLayer[Console with Clock, Nothing, Logging]`, we write `ZLayer[Has[Console] with Has[Clock], Nothing, Has[Logging]]`.

Finally, we provide required layers to our `app` effect:

```scala mdoc:silent:nest
 import zio._
 val app = Logging.log("Application Started")

 zio.Runtime.default.unsafeRun(
   app.provideLayer(LoggingLive.layer)
 )
```

## Dependency Injection in ZIO

ZLayers combined with the ZIO environment, allow us to use ZIO for dependency injection. There are two parts for dependency injection:
1. **Building Dependency Graph**
2. **Dependency Propagation**

ZIO has a full solution to the dependency injection problem. It solves the first problem by using compositional properties of `ZLayer`, and solves the second by using ZIO Environment facilities like `ZIO#provide`. 

The way ZIO manages dependencies between application components gives us extreme power in terms of compositionality and offering the capability to easily change different implementations. This is particularly useful during _testing_ and _mocking_.

By using ZLayer and ZIO Environment we can solve the propagation and wire-up problems in dependency injection. But it doesn't necessary to use it, we can still use things like [Guice](https://github.com/google/guice) with ZIO, or we might like to use [izumi distage](https://izumi.7mind.io/distage/index.html) solution for dependency injection.

### Building Dependency Graph

Assume we have several services with their dependencies, and we need a way to compose and wiring up these dependencies and create the dependency graph of our application. `ZLayer` is a ZIO solution for this problem, it allows us to build up the whole application dependency graph by composing layers horizontally and vertically. More information about how to compose layers is on the [ZLayer](zlayer.md) page.

### Dependency Propagation

When we write an application, our application has a lot of dependencies. We need a way to provide implementations and feeding and propagating all dependencies throughout the whole application. We can solve the propagation problem by using _ZIO environment_.

During the development of an application, we don't care about implementations. Incrementally, when we use various effects with different requirements on their environment, all part of our application composed together, and at the end of the day we have a ZIO effect which requires some services as an environment. Before running this effect by `unsafeRun` we should provide an implementation of these services into the ZIO Environment of that effect.

ZIO has some facilities for doing this. `ZIO#provide` is the core function that allows us to _feed_ an `R` to an effect that requires an `R`. 

Notice that the act of `provide`ing an effect with its environment, eliminates the environment dependency in the resulting effect type, represented by type `Any` of the resulting environment.

#### Using `provide` Method

The `ZIO#provide` takes an `R` environment and provides it to the `ZIO` effect which eliminates its dependency on `R`:

```scala
trait ZIO[-R, +E, +A] {
  def provide(r: R)(implicit ev: NeedsEnv[R]): IO[E, A]
}
```

This is similar to dependency injection, and the `provide` function can be thought of as `inject`.

```scala mdoc:invisible:reset
import zio._
```

Assume we have the following services:

```scala mdoc:silent:nest
trait Logging {
  def log(str: String): UIO[Unit]
}

object Logging {
  def log(line: String) = ZIO.serviceWith[Logging](_.log(line))
}
```

Let's write a simple program using `Logging` service:

```scala mdoc:silent:nest
val app: ZIO[Has[Logging], Nothing, Unit] = Logging.log("Application Started!")
```

We can `provide` implementation of `Logging` service into the `app` effect:

```scala mdoc:silent:nest
val loggingImpl = Has(new Logging {
  override def log(line: String): UIO[Unit] =
    UIO.effectTotal(println(line))
})

val effect = app.provide(loggingImpl)
```

Most of the time, we don't use `Has` directly to implement our services, instead; we use `ZLayer` to construct the dependency graph of our application, then we use methods like `ZIO#provideLayer` to propagate dependencies into the environment of our ZIO effect.

#### Using `provideLayer` Method

Unlike the `ZIO#provide` which takes and an `R`, the `ZIO#provideLayer` takes a `ZLayer` to the ZIO effect and translates it to another level. 

Assume we have written this piece of program that requires Clock and Console services:

```scala mdoc:silent:nest
import zio.clock._
import zio.console._
import zio.random._

val myApp: ZIO[Random with Console with Clock, Nothing, Unit] = for {
  random  <- nextInt 
  _       <- putStrLn(s"A random number: ${random.toString}").orDie
  current <- currentDateTime.orDie
  _       <- putStrLn(s"Current Data Time: ${current.toString}").orDie
} yield ()
```

We can compose the live implementation of `Random`, `Console` and `Clock` services horizontally and then provide them to the `myApp` effect by using `ZIO#provideLayer` method:

```scala mdoc:silent:nest
val mainEffect: ZIO[Any, Nothing, Unit] = 
  myApp.provideLayer(Random.live ++ Console.live ++ Clock.live)
```

As we see, the type of our effect converted from `ZIO[Random with Console with Clock, Nothing, Unit]` which requires two services to `ZIO[Any, Nothing, Unit]` effect which doesn't require any services.

#### Using `provideSomeLayer` Method

Sometimes we have written a program, and we don't want to provide all its requirements. In these cases, we can use `ZIO#provideSomeLayer` to partially apply some layers to the `ZIO` effect.

In the previous example, if we just want to provide the `Console`, we should use `ZIO#provideSomeLayer`:

```scala mdoc:silent:nest
val mainEffect: ZIO[Random with Clock, Nothing, Unit] = 
  myApp.provideSomeLayer[Random with Clock](Console.live)
```

> **Note:**
>
> When using `ZIO#provideSomeLayer[R0 <: Has[_]]`, we should provide the remaining type as `R0` type parameter. This workaround helps the compiler to infer the proper types.

#### Using `provideCustomLayer` Method

`ZEnv` is a convenient type alias that provides several built-in ZIO layers that are useful in most applications.

Sometimes we have written a program that contains ZIO built-in services and some other services that are not part of `ZEnv`.  

 As `ZEnv` provides us the implementation of built-in services, we just need to provide layers for those services that are not part of the `ZEnv`. 

`ZIO#provideCustomLayer` helps us to do so and returns an effect that only depends on `ZEnv`.

Let's write an effect that has some built-in services and also has a `Logging` service:

```scala mdoc:invisible:reset
import zio._
import zio.console._
import zio.clock._
```

```scala mdoc:silent
trait Logging {
  def log(str: String): UIO[Unit]
}

object Logging {
  def log(line: String) = ZIO.serviceWith[Logging](_.log(line))
}

object LoggingLive {
  val layer: ULayer[Has[Logging]] = ZLayer.succeed {
    new Logging {
      override def log(str: String): UIO[Unit] = ???
    }
  }
}

val myApp: ZIO[Has[Logging] with Console with Clock, Nothing, Unit] = for {
  _       <- Logging.log("Application Started!")
  current <- currentDateTime.orDie
  _       <- putStrLn(s"Current Data Time: ${current.toString}").orDie
} yield ()
```

This program uses two ZIO built-in services, `Console` and `Clock`. We don't need to provide `Console` and `Clock` manually, to reduce some boilerplate, we use `ZEnv` to satisfy some common base requirements.

By using `ZIO#provideCustomLayer` we only provide the `Logging` layer, and it returns a `ZIO` effect which only requires `ZEnv`:

```scala mdoc:silent
val mainEffect: ZIO[ZEnv, Nothing, Unit] = myApp.provideCustomLayer(LoggingLive.layer)
```

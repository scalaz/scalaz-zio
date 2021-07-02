---
id: officials 
title:  "Official ZIO Libraries"
---

## Introduction 

These libraries are hosted in the [ZIO organization](https://github.com/zio/) on Github, and are generally maintained by core contributors to ZIO.

Each project in the ZIO organization namespace has a _Stage Badge_ which indicates the current status of that project:

* **Production Ready** — The project is stable and already used in production. We can expect reliability for the implemented use cases.

* **Development** — The project already has RC or milestone releases, but is still under active development. We should not expect full stability yet.

* **Experimental** — The project is not yet released, but an important part of the work is already done.

* **Research** — The project is at the design stage, with some sketches of work but nothing usable yet.

* **Concept** — The project is just an idea, development hasn't started yet.

* **Deprecated** — The project is not maintained anymore, and we don't recommend its usage.

## Official Libraries

- [ZIO Actors](https://github.com/zio/zio-actors) — A high-performance, purely-functional library for building, composing, and supervising typed actors based on ZIO
- [ZIO Akka Cluster](https://github.com/zio/zio-akka-cluster) — A ZIO wrapper for Akka Cluster
- [ZIO Cache](https://github.com/zio/zio-cache) - A ZIO native cache with a simple and compositional interface
- [ZIO Config](https://github.com/zio/zio-config) — A ZIO based configuration parsing library
- [ZIO Kafka](https://github.com/zio/zio-kafka) — A Kafka client for ZIO and ZIO Streams
- [ZIO Keeper](https://github.com/zio/zio-keeper) — A functional library for consistent replication of metadata across dynamic clusters
- [ZIO Logging](https://github.com/zio/zio-logging) — An environmental effect for adding logging into any ZIO application, with choice of pluggable back-ends
- [ZIO Microservice](https://github.com/zio/zio-microservice) — ZIO-powered microservices via HTTP and other protocols
- [ZIO NIO](https://github.com/zio/zio-nio) — A performant, purely-functional, low-level, and unopinionated wrapper around Java NIO functionality
- [ZIO Optics](https://github.com/zio/zio-optics) - Easily modify parts of larger data structures
- [ZIO Prelude](https://github.com/zio/zio-prelude) - A lightweight, distinctly Scala take on functional abstractions, with tight ZIO integration
- [ZIO Redis](https://github.com/zio/zio-redis) - A ZIO-native Redis client
- [ZIO SQS](https://github.com/zio/zio-sqs) — A ZIO-powered client for AWS SQS
- [ZIO Telemetry](https://github.com/zio/zio-telemetry) — A ZIO-powered OpenTelemetry library 
- [ZIO ZMX](https://github.com/zio/zio-zmx) - Monitoring, metrics and diagnostics for ZIO

## ZIO Actors

[ZIO Actors](https://github.com/zio/zio-actors) is a high-performance, purely functional library for building, composing, and supervising typed actors based on ZIO.

ZIO Actors is based on the _Actor Model_ which is a conceptual model of concurrent computation. In the actor model, the _actor_ is the fundamental unit of computation, unlike the ZIO concurrency model, which is the fiber.

Each actor has a mailbox that stores and processes the incoming messages in FIFO order. An actor allowed to:
- create another actor.
- send a message to itself or other actors.
- handle the incoming message, and:
    - decide **what to do** based on the current state and the received message.
    - decide **what is the next state** based on the current state and the received message.

Some characteristics of an _Actor Model_:

- **Isolated State** — Each actor holds its private state. They only have access to their internal state. They are isolated from each other, and they do not share the memory. The only way to change the state of an actor is to send a message to that actor.

- **Process of One Message at a Time** — Each actor handles and processes one message at a time. They read messages from their inboxes and process them sequentially.

- **Actor Persistence** — A persistent actor records its state as events. The actor can recover its state from persisted events after a crash or restart.

- **Remote Messaging** — Actors can communicate with each other only through messages. They can run locally or remotely on another machine. Remote actors can communicate with each other transparently as if there are located locally.

- **Actor Supervision** — Parent actors can supervise their child actors. For example, if a child actor fails, the supervisor actor can restart that actor.

To use this library, we need to add the following line to our library dependencies in `build.sbt` file:

```scala
val zioActorsVersion =  "0.0.9" // Check the original repo for the latest version
libraryDependencies += "dev.zio" %% "zio-actors" % zioActorsVersion
```

Let's try to implement a simple Counter Actor which receives two `Increase` and `Get` commands:

```scala mdoc:silent:nest
import zio.actors.Actor.Stateful
import zio.actors._
import zio.clock.Clock
import zio.console.putStrLn
import zio.{ExitCode, UIO, URIO, ZIO}

sealed trait Message[+_]
case object Increase extends Message[Unit]
case object Get      extends Message[Int]

object CounterActorExample extends zio.App {

  // Definition of stateful actor
  val counterActor: Stateful[Any, Int, Message] =
    new Stateful[Any, Int, Message] {
      override def receive[A](
          state: Int,
          msg: Message[A],
          context: Context
      ): UIO[(Int, A)] =
        msg match {
          case Increase => UIO((state + 1, ()))
          case Get      => UIO((state, state))
        }
    }

  val myApp: ZIO[Clock, Throwable, Int] =
    for {
      system <- ActorSystem("MyActorSystem")
      actor  <- system.make("counter", Supervisor.none, 0, counterActor)
      _      <- actor ! Increase
      _      <- actor ! Increase
      s      <- actor ? Get
    } yield s

  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] =
    myApp
      .flatMap(state => putStrLn(s"The final state of counter: $state"))
      .exitCode
}
```

Akka actors also has some other optional modules for persistence (which is useful for event sourcing) and integration with Akka toolkit:

```scala
libraryDependencies += "dev.zio" %% "zio-actors-persistence" % zioActorsVersion
libraryDependencies += "dev.zio" %% "zio-actors-persistence-jdbc" % zioActorVersion
libraryDependencies += "dev.zio" %% "zio-actors-akka-interop" % zioActorVersion
```

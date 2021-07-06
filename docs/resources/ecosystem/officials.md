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

## ZIO Akka Cluster

The [ZIO Akka Cluster](https://github.com/zio/zio-akka-cluster) library is a ZIO wrapper on [Akka Cluster](https://doc.akka.io/docs/akka/current/index-cluster.html). We can use clustering features of the Akka toolkit without the need to use the actor model.

This library provides us following features:

- **Akka Cluster** — This feature contains two Akka Cluster Membership operations called `join` and `leave` and also it has some methods to retrieve _Cluster State_ and _Cluster Events_.

- **Akka Distributed PubSub** — Akka has a _Distributed Publish Subscribe_ facility in the cluster. It helps us to send a message to all actors in the cluster that have registered and subscribed for a specific topic name without knowing their physical address or without knowing which node they are running on.

- **Akka Cluster Sharding** — Cluster sharding is useful when we need to _distribute actors across several nodes in the cluster_ and want to be able to interact with them using their logical identifier without having to care about their physical location in the cluster, which might also change over time. When we have many stateful entities in our application that together they consume more resources (e.g. memory) than fit on one machine, it is useful to use _Akka Cluster Sharding_ to distribute our entities to multiple nodes.

In order to use this library, we need to add the following line in our `build.sbt` file:

```scala
libraryDependencies += "dev.zio" %% "zio-akka-cluster" % "0.2.0" // Check the repo for the latest version
```

In the following example, we are using all these three features. We have a distributed counter application that lives in the Akka Cluster using _Akka Cluster Sharding_ feature. So the location of `LiveUsers` and `TotalRequests` entities in the cluster is transparent for us. We send the result of each entity to the _Distributed PubSub_. So every node in the cluster can subscribe and listen to those results. Also, we have created a fiber that is subscribed to the cluster events. All the new events will be logged to the console:

```scala mdoc:silent:nest
import akka.actor.ActorSystem
import com.typesafe.config.{Config, ConfigFactory}
import zio.akka.cluster.Cluster
import zio.akka.cluster.sharding.{Entity, Sharding}
import zio.console.putStrLn
import zio.{ExitCode, Has, Managed, Task, URIO, ZIO, ZLayer}

sealed trait Counter extends Product with Serializable
case object Inc extends Counter
case object Dec extends Counter

case class CounterApp(port: String) {
  val config: Config =
    ConfigFactory.parseString(
      s"""
         |akka {
         |  actor {
         |    provider = "cluster"
         |  }
         |  remote {
         |    netty.tcp {
         |      hostname = "127.0.0.1"
         |      port = $port
         |    }
         |  }
         |  cluster {
         |    seed-nodes = ["akka.tcp://CounterApp@127.0.0.1:2551"]
         |  }
         |}
         |""".stripMargin)

  val actorSystem: ZLayer[Any, Throwable, Has[ActorSystem]] =
    ZLayer.fromManaged(
      Managed.make(Task(ActorSystem("CounterApp", config)))(sys =>
        Task.fromFuture(_ => sys.terminate()).either
      )
    )

  val counterApp: ZIO[zio.ZEnv, Throwable, Unit] =
    actorSystem.build.use(sys =>
      for {
        queue <- Cluster
          .clusterEvents(true)
          .provideCustomLayer(ZLayer.succeedMany(sys))

        pubsub <- zio.akka.cluster.pubsub.PubSub
          .createPubSub[Int]
          .provideCustomLayer(ZLayer.succeedMany(sys))

        liveUsersLogger <- pubsub
          .listen("LiveUsers")
          .flatMap(
            _.take.tap(u => putStrLn(s"Number of live users: $u")).forever
          )
          .fork
        totalRequestLogger <- pubsub
          .listen("TotalRequests")
          .flatMap(
            _.take.tap(r => putStrLn(s"Total request until now: $r")).forever
          )
          .fork

        clusterEvents <- queue.take
          .tap(x => putStrLn("New event in cluster: " + x.toString))
          .forever
          .fork

        counterEntityLogic = (c: Counter) =>
          for {
            entity <- ZIO.environment[Entity[Int]]
            newState <- c match {
              case Inc =>
                entity.get.state.updateAndGet(s => Some(s.getOrElse(0) + 1))
              case Dec =>
                entity.get.state.updateAndGet(s => Some(s.getOrElse(0) - 1))
            }
            _ <- pubsub.publish(entity.get.id, newState.getOrElse(0)).orDie
          } yield ()
        cluster <- Sharding
          .start("CounterEntity", counterEntityLogic)
          .provideCustomLayer(ZLayer.succeedMany(sys))

        _ <- cluster.send("LiveUsers", Inc)
        _ <- cluster.send("TotalRequests", Inc)
        _ <- cluster.send("LiveUsers", Dec)
        _ <- cluster.send("LiveUsers", Inc)
        _ <- cluster.send("LiveUsers", Inc)
        _ <- cluster.send("TotalRequests", Inc)
        _ <- cluster.send("TotalRequests", Inc)

        _ <-
          clusterEvents.join zipPar liveUsersLogger.join zipPar totalRequestLogger.join
      } yield ()
    )
}
```

Now, let's create a cluster comprising two nodes:

```scala mdoc:silent:nest
object CounterApp1 extends zio.App {
  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] = 
    CounterApp("2551").counterApp.exitCode
}

object CounterApp2 extends zio.App {
  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] = 
    CounterApp("2552").counterApp.exitCode
}
```

## ZIO Cache

ZIO Cache is a library that makes it easy to optimize the performance of our application by caching values.

Sometimes we may call or receive requests to do overlapping work. Assume we are writing a service that is going to handle all incoming requests. We don't want to handle duplicate requests. Using ZIO Cache we can make our application to be more **performant** by preventing duplicated works.

### Introduction

Some key features of ZIO Cache:

- **Compositionality** — If we want our applications to be **compositional**, different parts of our application may do overlapping work. ZIO Cache helps us to stay benefit from compositionality while using caching.

- **Unification of Synchronous and Asynchronous Caches** — Compositional definition of cache in terms of _lookup function_ unifies synchronous and asynchronous caches. So the lookup function can compute value either synchronously or asynchronously.

- **Deep ZIO Integration** — ZIO Cache is a ZIO native solution. So without losing the power of ZIO it includes support for _concurrent lookups_, _failure_, and _interruption_.

- **Caching Policy** — Using caching policy, the ZIO Cache can determine when values should/may be removed from the cache. So, if we want to build something more complex and custom we have a lot of flexibility. The caching policy has two parts and together they define a whole caching policy:

    - **Priority (Optional Removal)** — When we are running out of space, it defines the order that the existing values **might** be removed from the cache to make more space.

    - **Evict (Mandatory Removal)** — Regardless of space when we **must** remove existing values because they are no longer valid anymore. They might be invalid because they do not satisfy business requirements (e.g., maybe it's too old). This is a function that determines whether an entry is valid based on the entry and the current time.

- **Composition Caching Policy** — We can define much more complicated caching policies out of much simpler ones.

- **Cache/Entry Statistics** — ZIO Cache maintains some good statistic metrics, such as entries, memory size, hits, misses, loads, evictions, and total load time. So we can look at how our cache is doing and decide where we should change our caching policy to improve caching metrics.

### Installation

In order to use this library, we need to add the following line in our `build.sbt` file:

```scala
libraryDependencies += "dev.zio" %% "zio-cache" % "0.1.0" // Check the repo for the latest version
```

### Example

In this example, we are calling `timeConsumingEffect` three times in parallel with the same key. The ZIO Cache runs this effect only once. So the concurrent lookups will suspend until the value being computed is available:

```scala mdoc:silent:nest
import zio.cache.{Cache, Lookup}
import zio.clock.Clock
import zio.console.{Console, putStrLn}
import zio.duration.{Duration, durationInt}
import zio.{ExitCode, URIO, ZIO}

import java.io.IOException

def timeConsumingEffect(key: String): ZIO[Clock, Nothing, Int] =
  ZIO.sleep(5.seconds) *> ZIO.succeed(key.hashCode)

val myApp: ZIO[Console with Clock, IOException, Unit] =
  for {
    cache <- Cache.make(
      capacity = 100,
      timeToLive = Duration.Infinity,
      lookup = Lookup(timeConsumingEffect)
    )
    result <- cache.get("key1")
                .zipPar(cache.get("key1"))
                .zipPar(cache.get("key1"))
    _ <- putStrLn(s"Result of parallel execution three effects with the same key: $result")

    hits <- cache.cacheStats.map(_.hits)
    misses <- cache.cacheStats.map(_.misses)
    _ <- putStrLn(s"Number of cache hits: $hits")
    _ <- putStrLn(s"Number of cache misses: $misses")
  } yield ()
```

The output of this program should be as follows: 

```
Result of parallel execution three effects with the same key: ((3288498,3288498),3288498)
Number of cache hits: 2
Number of cache misses: 1
```

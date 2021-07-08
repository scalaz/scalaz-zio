---
id: blocking 
title: "Blocking"
---

```scala mdoc:invisible
import zio.duration._
import zio.{Schedule, RIO, UIO}
```

## Introduction

The **Blocking** service provides access to a thread pool that can be used for performing
blocking operations, such as thread sleeps, synchronous socket/file reads, and so forth. 

By default, ZIO is asynchronous and all effects will be executed on a default primary thread pool which is optimized for asynchronous operations. As ZIO uses a fiber-based concurrency model, if we run **Blocking I/O** or **CPU Work** workloads on a primary thread pool, they are going to monopolize all threads of **primary thread pool**.

In the following example, we create 20 blocking tasks to run parallel on the primary async thread pool. Assume we have a machine with an 8 CPU core, so the ZIO creates a thread pool of size 16 (2 * 8). If we run this program, all of our threads got stuck, and the remaining 4 blocking tasks (20 - 16) haven't any chance to run on our thread pool:

```scala mdoc:silent
import zio.{ZIO, URIO}
import zio.console._ 
def blockingTask(n: Int): URIO[Console, Unit] =
  putStrLn(s"running blocking task number $n").orDie *>
    ZIO.effectTotal(Thread.sleep(3000)) *>
    blockingTask(n)

val program = ZIO.foreachPar((1 to 100).toArray)(blockingTask)
```

## Creating Blocking Effects

ZIO has a separate **blocking thread pool** specially designed for **Blocking I/O** and, also **CPU Work** workloads. We should run blocking workloads on this thread pool to prevent interfering with the primary thread pool.

The contract is that the thread pool will accept unlimited tasks (up to the available memory)
and continuously create new threads as necessary.

The `blocking` operator takes a ZIO effect and return another effect that is going to run on a blocking thread pool:

```scala mdoc:invisible:nest
val program = ZIO.foreachPar((1 to 100).toArray)(t => zio.blocking.blocking(blockingTask(t)))
```

Also, we can directly import a synchronous effect that does blocking operation into ZIO effect by using `effectBlocking`:

```scala mdoc:silent:nest
import zio.blocking._
def blockingTask(n: Int) = effectBlocking {
  do {
    println(s"Running blocking task number $n on dedicated blocking thread pool")
    Thread.sleep(3000) 
  } while (true)
}
```

## Interruption of Blocking Operations

By default, when we convert a blocking operation into the ZIO effects using `effectBlocking`, there is no guarantee that if that effect is interrupted the underlying effect will be interrupted.

Let's create a blocking effect from an endless loop:

```scala mdoc:silent:nest
for {
  _ <- putStrLn("Starting a blocking operation")
  fiber <- effectBlocking {
    while (true) {
      Thread.sleep(1000)
      println("Doing some blocking operation")
    }
  }.ensuring(
    putStrLn("End of a blocking operation").orDie
  ).fork
  _ <- fiber.interrupt.schedule(
    Schedule.delayed(
      Schedule.duration(1.seconds)
    )
  )
} yield ()
```

When we interrupt this loop after one second, it will not interrupted. It will only stop when the entire JVM stops. So the `effectBlocking` doesn't translate the ZIO interruption into thread interruption (`Thread.interrupt`). 

Instead, we should use `effectBlockingInterrupt` to create interruptible blocking effects:

```scala mdoc:silent:nest
for {
  _ <- putStrLn("Starting a blocking operation")
  fiber <- effectBlockingInterrupt {
    while(true) {
      Thread.sleep(1000)
      println("Doing some blocking operation")
    }
  }.ensuring(
     putStrLn("End of the blocking operation").orDie
   ).fork
  _ <- fiber.interrupt.schedule(
    Schedule.delayed(
      Schedule.duration(3.seconds)
    )
  )
} yield ()
```

Notes:

1. If we are converting a blocking I/O to the ZIO effect, it would be better to use `effectBlockingIO` which refines the error type to the `java.io.IOException`.

2. The `effectBlockingInterrupt` method adds significant overhead. So for performance-sensitive applications, it is better to handle interruptions manually using `effectBlockingCancel`.

## Cancellation of Blocking Operation

Some blocking operations do not respect `Thread#interrupt` by swallowing `InterruptedException`. So, they will not be interrupted via `effectBlockingInterrupt`. Instead, they may provide us an API to signal them to _cancel_ their operation.

The following `BloclingService` will not be interrupted in case of `Thread#interrupt` call, but it checks the `released` flag constantly. If this flag becomes true, the blocking service will finish its job:

```scala mdoc:silent:nest
import java.util.concurrent.atomic.AtomicReference
final case class BlockingService() {
  private val released = new AtomicReference(false)

  def start(): Unit = {
    while (!released.get()) {
      println("Doing some blocking operation")
      try Thread.sleep(1000)
      catch {
        case _: InterruptedException => () // Swallowing InterruptedException
      }
    }
    println("Blocking operation closed.")
  }

  def close(): Unit = {
    println("Releasing resources and ready to be closed.")
    released.getAndSet(true)
  }
}
```

So, to translate ZIO interruption into cancellation of these types of blocking operations we should use `effectBlockingCancelation`. This method takes a `cancel` effect which responsible to signal the blocking code to close itself when ZIO interruption occurs:

```scala mdoc:silent:nest
val myApp =
  for {
    service <- ZIO.effect(BlockingService())
    fiber   <- effectBlockingCancelable(
      effect = service.start()
    )(
      cancel = UIO.effectTotal(service.close())
    ).fork
    _       <- fiber.interrupt.schedule(
      Schedule.delayed(
        Schedule.duration(3.seconds)
      )
    )
  } yield ()
```

Here is another example of the cancelation of a blocking operation. When we `accept` a server socket, this blocking operation will never interrupted until we close that using `ServerSocket#close` method:

```scala mdoc:silent:nest
import java.net.{Socket, ServerSocket}
def accept(ss: ServerSocket): RIO[Blocking, Socket] =
  effectBlockingCancelable(ss.accept())(UIO.effectTotal(ss.close()))
```

---
id: fiber
title: "Fiber"
---

To perform an effect without blocking the current process, we can use fibers, which are a lightweight concurrency mechanism.

We can `fork` any `IO[E, A]` to immediately yield an `UIO[Fiber[E, A]]`. The provided `Fiber` can be used to `join` the fiber, which will resume on production of the fiber's value, or to `interrupt` the fiber, which immediately terminates the fiber and safely releases all resources acquired by the fiber.

```scala mdoc:invisible
import zio._
```
```scala mdoc:invisible
sealed trait Analysis
case object Analyzed extends Analysis

val data: String = "tut"

def analyzeData[A](data: A): UIO[Analysis] = IO.succeed(Analyzed)
def validateData[A](data: A): UIO[Boolean] = IO.succeed(true)
```

```scala mdoc:silent
val analyzed =
  for {
    fiber1   <- analyzeData(data).fork  // IO[E, Analysis]
    fiber2   <- validateData(data).fork // IO[E, Boolean]
    // Do other stuff
    valid    <- fiber2.join
    _        <- if (!valid) fiber1.interrupt
                else IO.unit
    analyzed <- fiber1.join
  } yield analyzed
```

## Operations

### fork and join
Whenever we need to start a fiber, we have to `fork` an effect, it gives us a fiber. It is similar to the `start` method on Java thread or submitting a new thread to the thread pool in Java, it is the same idea. Also, joining is a way of waiting for that fiber to compute its value. We are going to wait until it's done.

In the following example, we are going to run sleep and printing on a separate fiber and at the end, waiting for that fiber to compute its value:

```scala mdoc:silent
import zio._
import zio.console._
import zio.clock._
import zio.duration._
for {
  fiber <- (sleep(3.seconds) *>
    putStrLn("Hello, after 3 second") *>
    ZIO.succeed(10)).fork
  _ <- putStrLn(s"Hello, World!")
  res <- fiber.join
  _ <- putStrLn(s"Our fiber succeeded with $res")
} yield ()
```

### fork0
A more powerful variant of `fork`, called `fork0`, allows specification of supervisor that will be passed any non-recoverable errors from the forked fiber, including all such errors that occur in finalizers. If this supervisor is not specified, then the supervisor of the parent fiber will be used, recursively, up to the root handler, which can be specified in `Runtime` (the default supervisor merely prints the stack trace).

### forkDaemon

The `ZIO#forkDaemon` forks the effect into a new fiber **attached to the global scope**. Because the new fiber is attached to the global scope, when the fiber executing the returned effect terminates, the forked fiber will continue running.

In the following example, we have three effects: `inner`, `outer`, and `mainApp`. The outer effect is forking the `inner` effect using `ZIO#forkDaemon`. The `mainApp` effect is forking the `inner` fiber using `ZIO#fork` method and interrupt it after 3 seconds. Since the `inner` effect is forked in global scope, it will not be interrupted and continue its job:

```scala mdoc:silent:nest
val inner = putStrLn("Inner job is running.")
  .delay(1.seconds)
  .forever
  .onInterrupt(putStrLn("Inner job interrupted.").orDie)

val outer = (
  for {
    f <- inner.forkDaemon
    _ <- putStrLn("Outer job is running.").delay(1.seconds).forever
    _ <- f.join
  } yield ()
).onInterrupt(putStrLn("Outer job interrupted.").orDie)

val myApp = for {
  fiber <- outer.fork
  _     <- fiber.interrupt.delay(3.seconds)
  _     <- ZIO.never
} yield ()
```

### interrupt
Whenever we want to get rid of our fiber, we can simply call interrupt on that. The interrupt operation does not resume until the fiber has completed or has been interrupted and all its finalizers have been run. These precise semantics allow construction of programs that do not leak resources.

### await
To inspect whether our fiber succeeded or failed, we can call `await` on fiber. if we call `await` it will wait for that fiber to terminate, and it will give us back the fiber's value as an `Exit`. That exit value could be failure or success. 

```scala mdoc:silent
import zio.console._
import zio.random._
for {
  b <- nextBoolean
  fiber <- (if (b) ZIO.succeed(10) else ZIO.fail("The boolean was not true")).fork
  exitValue <- fiber.await
  _ <- exitValue match {
    case Exit.Success(value) => putStrLn(s"Fiber succeeded with $value")
    case Exit.Failure(cause) => putStrLn(s"Fiber failed")
  }
} yield ()
```

The `await` is similar to join but lower level than join. When we call join if the underlying fiber failed then our attempt to join it will also fail with the same error.  

### Parallelism
To execute actions in parallel, the `zipPar` method can be used:

```scala mdoc:invisible
case class Matrix()
def computeInverse(m: Matrix): UIO[Matrix] = IO.succeed(m)
def applyMatrices(m1: Matrix, m2: Matrix, m3: Matrix): UIO[Matrix] = IO.succeed(m1)
```

```scala mdoc:silent
def bigCompute(m1: Matrix, m2: Matrix, v: Matrix): UIO[Matrix] =
  for {
    t <- computeInverse(m1).zipPar(computeInverse(m2))
    (i1, i2) = t
    r <- applyMatrices(i1, i2, v)
  } yield r
```

The `zipPar` combinator has resource-safe semantics. If one computation fails, the other computation will be interrupted, to prevent wasting resources.

### Racing

Two `IO` actions can be *raced*, which means they will be executed in parallel, and the value of the first action that completes successfully will be returned.

```scala
fib(100) race fib(200)
```

The `race` combinator is resource-safe, which means that if one of the two actions returns a value, the other one will be interrupted, to prevent wasting resources.

The `race` and even `zipPar` combinators are a specialization of a much-more powerful combinator called `raceWith`, which allows executing user-defined logic when the first of two actions succeeds.

On the JVM, fibers will use threads, but will not consume *unlimited* threads. Instead, fibers yield cooperatively during periods of high-contention.

```scala mdoc:silent
def fib(n: Int): UIO[Int] =
  if (n <= 1) {
    IO.succeed(1)
  } else {
    for {
      fiber1 <- fib(n - 2).fork
      fiber2 <- fib(n - 1).fork
      v2     <- fiber2.join
      v1     <- fiber1.join
    } yield v1 + v2
  }
```

## Error Model
The `IO` error model is simple, consistent, permits both typed errors and termination, and does not violate any laws in the `Functor` hierarchy.

An `IO[E, A]` value may only raise errors of type `E`. These errors are recoverable by using the `either` method.  The resulting effect cannot fail, because the failure case bas been exposed as part of the `Either` success case.  

```scala mdoc:silent
val error: Task[String] = IO.fail(new RuntimeException("Some Error"))
val errorEither: ZIO[Any, Nothing, Either[Throwable, String]] = error.either
```

Separately from errors of type `E`, a fiber may be terminated for the following reasons:

* **The fiber self-terminated or was interrupted by another fiber**. The "main" fiber cannot be interrupted because it was not forked from any other fiber.

* **The fiber failed to handle some error of type `E`**. This can happen only when an `IO.fail` is not handled. For values of type `UIO[A]`, this type of failure is impossible.

* **The fiber has a defect that leads to a non-recoverable error**. There are only two ways this can happen:

     1. A partial function is passed to a higher-order function such as `map` or `flatMap`. For example, `io.map(_ => throw e)`, or `io.flatMap(a => throw e)`. The solution to this problem is to not to pass impure functions to purely functional libraries like ZIO, because doing so leads to violations of laws and destruction of equational reasoning.
     
     2. Error-throwing code was embedded into some value via `IO.effectTotal`, etc. For importing partial effects into `IO`, the proper solution is to use a method such as `IO.effect`, which safely translates exceptions into values.

When a fiber is terminated, the reason for the termination, expressed as a `Throwable`, is passed to the fiber's supervisor, which may choose to log, print the stack trace, restart the fiber, or perform some other action appropriate to the context.

A fiber cannot stop its own interruption. However, all finalizers will be run during termination, even when some finalizers throw non-recoverable errors. Errors thrown by finalizers are passed to the fiber's supervisor.

There are no circumstances in which any errors will be "lost", which makes the `IO` error model more diagnostic-friendly than the `try`/`catch`/`finally` construct that is baked into both Scala and Java, which can easily lose errors.

## Fiber Interruption

In Java, a thread can be interrupted via `Thread#interrupt` via another thread, but it may don't respect the interruption request. Unlike Java, in ZIO when a fiber interrupts another fiber, we know that the interruption occurs, and it always works.

When working with ZIO fibers, we should consider these notes about fiber interruptions:

### Interruptible/Uninterruptible Regions

All fibers are interruptible by default. To make an effect uninterruptible we can use `Fiber#uninterruptible`, `ZIO#uninterruptible` or `ZIO.uninterruptible`. We have also interruptible versions of these methods to make an uninterruptible effect, interruptible.

```scala mdoc:silent:nest
for {
  fiber <- clock.currentDateTime
    .flatMap(time => putStrLn(time.toString))
    .schedule(Schedule.fixed(1.seconds))
    .uninterruptible
    .fork
  _     <- fiber.interrupt // Runtime stuck here and does not go further
} yield ()
```

Note that there is no way to stop interruption. We can only delay it, by making an effect uninterruptible.

### Fiber Finalization on Interruption

When a fiber done its work or even interrupted, the finalizer of that fiber is guaranteed to be executed:

```scala mdoc:silent:nest
for {
  fiber <- putStrLn("Working on the first job")
    .schedule(Schedule.fixed(1.seconds))
    .ensuring {
      (putStrLn(
        "Finalizing or releasing a resource that is time-consuming"
      ) *> ZIO.sleep(7.seconds)).orDie
    }
    .fork
  _     <- fiber.interrupt.delay(4.seconds)
  _     <- putStrLn(
          "Starting another task when the interruption of the previous task finished"
        )
} yield ()
```

The `release` action may take some time freeing up resources. So it may slow down the fiber's interruption.

### Fast Interruption

As we saw in the previous section, the ZIO runtime gets stuck on interruption task until the fiber's finalizer finishes its job. We can prevent this behavior by using `ZIO#disconnect` or `Fiber#interruptFork` which perform fiber's interruption in the background or in separate daemon fiber:

Let's try the `Fiber#interruptFork`:

```scala mdoc:silent:nest
for {
  fiber <- putStrLn("Working on the first job")
    .schedule(Schedule.fixed(1.seconds))
    .ensuring {
      (putStrLn(
        "Finalizing or releasing a resource that is time-consuming"
      ) *> ZIO.sleep(7.seconds)).orDie
    }
    .fork
  _ <- fiber.interruptFork.delay(4.seconds) // fast interruption
  _ <- putStrLn(
    "Starting another task while interruption of the previous fiber happening in the background"
  )
} yield ()
```

### Interrupting Blocking Operations

The `zio.blocking.effectBlocking` is interruptible by default, but its interruption will not translate to the JVM thread interruption. Instead, we can use `zio.blocking.effectBlockingInterruptible` method. By using `effectBlockingInterruptible` method if that effect is interrupted, it will translate the ZIO interruption to the JVM thread interruption. ZIO has a comprehensive guide about blocking operation at [blocking service](../../services/blocking.md) page.

### Automatic Interruption

If we never _cancel_ a running effect explicitly, ZIO performs **automatic interruption** for several reasons:

1. **Structured Concurrency** — If a parent fiber terminates, then by default, all child fibers are interrupted, and they cannot outlive their parent. We can prevent this behavior by using `ZIO#forkDaemon` or `ZIO#forkIn` instead of `ZIO#fork`.

2. **Parallelism** — If one effect fails during the execution of many effects in parallel, the others will be canceled. Examples include `foreachPar`, `zipPar`, and all other parallel operators.

3. **Timeouts** — If a running effect being timed out has not been completed in the specified amount of time, then the execution is canceled.

4. **Racing** — The loser of a race, if still running, is canceled.

### Joining an Interrupted Fiber

We can join an interrupted fiber, which will cause our fiber to become interrupted. And this process does not inhibit finalization. So, **ZIO's concurrency model respect brackets even we are going to _join_ an interrupted fiber**:

```scala mdoc:silent:nest
val myApp =
  (
    for {
      fiber <- putStrLn("Running a job").delay(1.seconds).forever.fork
      _     <- fiber.interrupt.delay(3.seconds)
      _     <- fiber.join // Joining an interrupted fiber
    } yield ()
  ).ensuring(
    putStrLn(
      "This finalizer will be executed without occurring any deadlock"
    ).orDie
  )
```

A fiber that is interrupted because of joining another interrupted fiber will properly finalize; this is a distinction between ZIO and the other effect systems, which _deadlock_ the joining fiber.

## Thread Shifting - JVM
By default, fibers make no guarantees as to which thread they execute on. They may shift between threads, especially as they execute for long periods of time.

Fibers only ever shift onto the thread pool of the runtime system, which means that by default, fibers running for a sufficiently long time will always return to the runtime system's thread pool, even when their (asynchronous) resumptions were initiated from other threads.

For performance reasons, fibers will attempt to execute on the same thread for a (configurable) minimum period, before yielding to other fibers. Fibers that resume from asynchronous callbacks will resume on the initiating thread, and continue for some time before yielding and resuming on the runtime thread pool.

These defaults help guarantee stack safety and cooperative multitasking. They can be changed in `Runtime` if automatic thread shifting is not desired.

## Type of Workloads
Let's discuss the type of workloads that a fiber can handle. There are three types of workloads that a fiber can handle:
1. **CPU Work/Pure CPU Bound** is a computation that uses the computational power of a CPU intensively and purely, without exceeding the computation boundary. By intensive, means a huge chunk of work which involves a significant amount of time to computed by CPU, e.g. complex numerical computation.
2. **Blocking I/O** is a computation, which exceeds the computational boundary by doing communication in a blocking fashion. For example, waiting for a certain amount of time to elapse or waiting for an external event to happen are blocking I/O operations.
3. **Asynchronous I/O** is a computation, which exceeds the computation boundary by doing communication asynchronously, e.g. registering a callback for a specific event.

### CPU Work
What we refer to as CPU Work is pure computational firepower without involving any interaction and communication with the outside world. It doesn't involve any I/O operation. It's a pure computation. By I/O, we mean anything that involves reading from and writing to an external resource such as a file or a socket or web API, or anything that would be characterized as I/O. 

Fibers are designed to be **cooperative** which means that **they will yield to each other as required to preserve some level of fairness**. If we have a fiber that's is doing CPU Work which passes through one or more ZIO operations such as flatMap or map, as long as there exists a touchpoint where the ZIO runtime system can sort of keep a tab on that ongoing CPU Work then that fiber will yield to other fibers. These touchpoints allow many fibers who are doing CPU Work to end up sharing the same thread.

What if though, we have a CPU Work operation that takes a really long time to run? Let's say 30 seconds or something it does pure CPU Work very computationally intensive? What happens if we take that single gigantic function and put that into a `ZIO.effect`? So there is no way for the ZIO Runtime that can force that fiber to yield to other fibers. In this situation, the ZIO Runtime cannot preserve some level of fairness, and that single big CPU operation monopolizes the underlying thread. It is not a good practice to monopolize the underlying thread.

ZIO has a special thread pool that can be used to do these computations. That's the **blocking thread pool**. There is a `ZIO.blocking._` package that contains an operator called `blocking`, which can be used to run a big CPU Work on a dedicated thread. So, it doesn't interfere with all the other work that is going on simultaneously in the ZIO Runtime system.

If a CPU Work doesn't yield quickly, then that is going to monopolize a thread. So how can we determine that our CPU Work can yield quickly or not? 
- If we wrote that CPU Work by composing many ZIO operations, even that CPU Work is very CPU intensive, due to the composition of ZIO operations, it has a chance to yield quickly to other fibers and don't monopolizing a thread.
- If that CPU work doesn't compose any ZIO operation, or we lift that from a legacy library, the ZIO Runtime doesn't have any chance of yielding quickly to other fibers. So this fiber going to monopolize the underlying thread.

The best practice is to run those huge CPU Work on a dedicated thread pool, by lifting them with the `blocking` operator in the `ZIO.blocking` package.

> _**Note**:_
>
> So as a rule of thumb, when we have a huge CPU Work that is not chunked with built-in ZIO operations and going to monopolize the underlying thread, we should run that on a dedicated thread pool that is designed to perform CPU-driven tasks.

### Blocking I/O
Inside Java, there are many methods that we can call them that will put our thread to sleep. For example, if we call `read` on a socket and there is nothing to read right now because not enough bytes have been read from the other side over the TCP/IP protocol, then that will put our thread to sleep. 

Most of the I/O operations and certainly all the classic I/O operations like `InputStream` and `OutputStream` are utilizing a locking mechanism that will parks a thread. When we `write` to the `OutputStream`, actually before the data has been written to that file, that method will just `block` it, and it will `park` the thread. The thread will `wait` until the data can actually be written and after it is actually written then it can return. It is the same way for `read` and similar blocking operations. Anytime we use a `lock`, anything in `java.util.concurrent.locks`, all those locks use this mechanism. All these operations are called blocking because they `park` the thread that is doing the work, and the thread that's doing the work goes to sleep.

> _**Note:**_ 
>
> What we refer to as blocking I/O is not necessarily just an I/O operation. Remember every time we use a `lock` we are also `park`ing a thread. It goes to `sleep`, and it has to be woken up again. We refer to this entire class of operations as **blocking I/O**.

There are multiple types of overhead associated with parking a thread:

1. When we `park` a thread then that thread is still consuming resources, it's still obviously consuming stack resources, the heap, and all metadata associated with the underlying thread in the JVM.

2. Then even further down deeper, in the operating system because every JVM thread corresponds to an operating system level thread, there's a large amount of overhead. Every thread has a pre-allocated stack size so that memory is reserved even if that thread's not doing any work. That memory is sort of reserved for the thread, and we cannot touch it. 

3. Besides, the actual process of putting a thread to sleep and then later waking it up again is computationally intensive. It slows down our computations.

This is why it has become a sort of best practice and part of the architectural pattern of reactive applications to design what is called **non-blocking application**. Non-blocking is synonymous with asynchronous. Non-blocking and asynchronous and to some degree even reactive, they're all trying to get at something which is we want scalable applications.

Scalable applications cannot afford to have thousands of threads just sitting around doing nothing and just consuming work and taking a long time to wake up again. We cannot do so-called blocking I/O, in scalable applications. It is considered an anti-pattern because it is not efficient. That is not a way to build scalable applications and nonetheless, we have to support that use case. 

Today, we have lots of common Java libraries that use blocking constructs, like `InputStream` and `OutputStream`, and `Reader` and `Writer`. Also, the JDBC is entirely blocking. The only way of doing database I/O in Java is blocking. So obviously, we have to do blocking I/O. We can do blocking I/O from a fiber. So is it best practice? No, it should be avoided whenever possible, but the reality is we have to do blocking I/O. 

Whenever we lift a blocking I/O operation into ZIO, the ZIO Runtime is executing a fiber that is doing blocking I/O. The underlying thread will be parked, and it has to be woken up later. It doesn't have any ability to stop from happening. It can't stop an underlying thread from being parked that's just the way these APIs are designed. So, we have to block. There's no way around that. That fiber will be monopolizing the underneath thread and therefore that thread is not available for performing all the work of the other fibers in the system. So that can be a bottleneck point in our application. 

And again, the solution to this problem is the same as the solution to the first class of problems, the CPU Work. The solution is to run this **using the blocking thread pool** in ZIO which will ensure that this blocking code executes on its dedicated thread pool. **So it doesn't have to interfere or compete with the threads that are used for doing the bulk of work in your application**. So basically ZIO's philosophy is if we have to do CPU Work or if we have to do synchronous that's ok we can do that. Just we need to do it in the right place. So it doesn't interfere with our primary thread pool.

ZIO has one primary built-in fixed thread pool. This sort of workhorse thread pool is designed to be used for the majority of our application requirements. It has a certain number of threads in it and that stays constant over the lifetime of our application.

Why is that the case? Well because for the majority of workloads in our applications, it does not actually help things to create more threads than the number of CPU cores. If we have eight cores, it does not accelerate any sort of processing to create more than eight threads. Because at the end of the day our hardware is only capable of running eight things at the same time. 

If we create a thousand threads on a system that can only run eight of them in parallel at a time, then what does the operating system have to do? As we have not as much as the needed CPU core, the operating system starts giving a little slice of the eight core to all these threads by switching between them over and over again.

The context switching overhead is significant. The CPU has to load in new registers, refill all its caches, it has to go through all these crazy complex processes that interfere with its main job to get stuff done. There's significant overhead associated with that. As a result, it's not going to be very efficient. We are going to waste a lot of our time and resources just switching back and forth between all these threads, that would kill our application.

So for that reason, ZIO's default thread pool is fixed with a number of threads equal to the number of CPU cores. That is the best practice. That means that no matter how much work we create if we create a hundred thousand fibers, they will still run on a fixed number of threads.

Let's say we do blocking I/O on the main ZIO thread pool, so we have got eight threads all sitting and parked on a socket read. What happens to all the other 100000 fibers in our system? They line up in a queue waiting for their chance to run. That's not ideal. That's why we should take these effects that either do blocking I/O, or they do big CPU Work that's not chunked and run them using ZIO's blocking thread pool which will give us a dedicated thread. 

That dedicated thread is not efficient but again, sometimes we have to interact with legacy code and legacy code is full of all blocking codes. We just need to be able to handle that gracefully and ZIO‌ does that using the blocking thread pool.

### Asynchronous I/O
The third category is asynchronous I/O, and we refer to it as Async Work. Async Work is a code that whenever it runs into something that it needs to wait on, instead of blocking and parking the thread, it registers a callback, and returns immediately.

 It allows us to register a callback and then when that result is available then our callback will be invoked. Callbacks are the fundamental way by which all async code on the JVM works. There is no mechanism in the JVM right now to support async code natively and once that would probably happen in the Loom project, in the future, and will simplify a lot of things.

But for now, in current days, sort of JVM all published JVM versions there's no such thing. The only way we can get a non-blocking async code is to have this callback registering mechanism. 

Callbacks have the pro that they don't wait for CPU. Instead of waiting to read the next chunk from a socket or instead of waiting to acquire a lock, all we have to do is call it and give it a callback. It doesn't need to do anything else it can return control to the thread pool and then later on when that data has been read from the socket or when that lock has been acquired or when that amount of time has elapsed if we're sleeping for a certain amount of time then our callback can be invoked. 

It has the potential to be extraordinarily efficient. The drawback of callbacks is they are not so pretty and fun to work with. They don't compose well with try-finally constructs. Error handling is really terrible, we have to do error propagation on our own. So that is what gave rise to data types like `Future` which eliminates the need for callbacks. By using `Future`s we can wrap callback-based APIs and get the benefits of async but without the callbacks. Also, it has a for-comprehension that we can structure our code as a nice linear sequence. 

Similarly in ZIO we never see a callback with ZIO, but fundamentally everything boils down to asynchronous on the JVM in a callback fashion. Callback base code is obscenely efficient but it is extraordinarily painful to deal with directly. Data types like `Future` and `ZIO` allow us to avoid even seeing a callback in our code. 

With ZIO, we do not have to think about a callback nonetheless sometimes when we need to integrate with legacy code. ZIO has an appropriate constructor to turn that ugly callback-based API into a ZIO effect. It is the `async` constructor.

Every time we do one of ZIO blocking operations it doesn't actually block the underlying thread, but also it is a semantic blocking managed by ZIO. For example, every time we see something like `ZIO.sleep` or when we take something from a queue (`queue.take`) or offer something to a queue (`queue.offer`) or if we acquire a permit from a semaphore (`semaphore.withPermit`) or if we acquire a lock (`ZIO.lock`) and so forth, we are blocking semantically. If we use the same stuff in Java, like `Thread.sleep` or any of its `lock` machinery then those things are going to block a thread. So this is why we say ZIO is 100% blocking but the Java thread is not.

All of the pieces of machinery ZIO gives us are 100% asynchronous and non-blocking. As they don't block and monopolize the thread, all of the async work is executed on the primary thread pool in ZIO.


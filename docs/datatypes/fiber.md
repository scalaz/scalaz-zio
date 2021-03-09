---
id: datatypes_fiber
title:  "Fiber"
---

To perform an effect without blocking the current process, you can use fibers, which are a lightweight mechanism for concurrency.

You can `fork` any `IO[E, A]` to immediately yield an `UIO[Fiber[E, A]]`. The provided `Fiber` can be used to `join` the fiber, which will resume on production of the fiber's value, or to `interrupt` the fiber, which immediately terminates the fiber and safely releases all resources acquired by the fiber.

```scala mdoc:silent
import zio._
```

```scala mdoc
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

The interrupt operation does not resume until the fiber has completed or has been interrupted and all its finalizers have been run. These precise semantics allow construction of programs that do not leak resources.

A more powerful variant of `fork`, called `fork0`, allows specification of supervisor that will be passed any non-recoverable errors from the forked fiber, including all such errors that occur in finalizers. If this supervisor is not specified, then the supervisor of the parent fiber will be used, recursively, up to the root handler, which can be specified in `Runtime` (the default supervisor merely prints the stack trace).

## Error Model

The `IO` error model is simple, consistent, permits both typed errors and termination, and does not violate any laws in the `Functor` hierarchy.

An `IO[E, A]` value may only raise errors of type `E`. These errors are recoverable by using the `either` method.  The resulting effect cannot fail, because the failure case bas been exposed as part of the `Either` success case.  

```scala mdoc:silent
val error: Task[String] = IO.fail(new RuntimeException("Some Error"))
val errorEither: ZIO[Any, Nothing, Either[Throwable, String]] = error.either
```

Separately from errors of type `E`, a fiber may be terminated for the following reasons:

 * The fiber self-terminated or was interrupted by another fiber. The "main" fiber cannot be interrupted because it was not forked from any other fiber.
 * The fiber failed to handle some error of type `E`. This can happen only when an `IO.fail` is not handled. For values of type `UIO[A]`, this type of failure is impossible.
 * The fiber has a defect that leads to a non-recoverable error. There are only two ways this can happen:
     1. A partial function is passed to a higher-order function such as `map` or `flatMap`. For example, `io.map(_ => throw e)`, or `io.flatMap(a => throw e)`. The solution to this problem is to not to pass impure functions to purely functional libraries like ZIO, because doing so leads to violations of laws and destruction of equational reasoning.
     2. Error-throwing code was embedded into some value via `IO.effectTotal`, etc. For importing partial effects into `IO`, the proper solution is to use a method such as `IO.effect`, which safely translates exceptions into values.

When a fiber is terminated, the reason for the termination, expressed as a `Throwable`, is passed to the fiber's supervisor, which may choose to log, print the stack trace, restart the fiber, or perform some other action appropriate to the context.

A fiber cannot stop its own interruption. However, all finalizers will be run during termination, even when some finalizers throw non-recoverable errors. Errors thrown by finalizers are passed to the fiber's supervisor.

There are no circumstances in which any errors will be "lost", which makes the `IO` error model more diagnostic-friendly than the `try`/`catch`/`finally` construct that is baked into both Scala and Java, which can easily lose errors.

## Parallelism

To execute actions in parallel, the `zipPar` method can be used:

```scala mdoc
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

```scala mdoc:silent
fib(100) race fib(200)
```

The `race` combinator is resource-safe, which means that if one of the two actions returns a value, the other one will be interrupted, to prevent wasting resources.

The `race` and even `zipPar` combinators are a specialization of a much-more powerful combinator called `raceWith`, which allows executing user-defined logic when the first of two actions succeeds.

## Thread Shifting - JVM

By default, fibers make no guarantees as to which thread they execute on. They may shift between threads, especially as they execute for long periods of time.

Fibers only ever shift onto the thread pool of the runtime system, which means that by default, fibers running for a sufficiently long time will always return to the runtime system's thread pool, even when their (asynchronous) resumptions were initiated from other threads.

For performance reasons, fibers will attempt to execute on the same thread for a (configurable) minimum period, before yielding to other fibers. Fibers that resume from asynchronous callbacks will resume on the initiating thread, and continue for some time before yielding and resuming on the runtime thread pool.

These defaults help guarantee stack safety and cooperative multitasking. They can be changed in `Runtime` if automatic thread shifting is not desired.

---
id: interop_future
title:  "Future"
---

## Scala Future

Basic interoperability with Scala's `Future` is now provided by ZIO, and does not require a separate module.

### From Future

Scala's `Future` can be converted into a ZIO effect with `ZIO.fromFuture`:

```scala mdoc
import zio. { UIO, ZIO, Task }
import scala.concurrent. { ExecutionContext, Future }

def loggedFuture[A](future: ExecutionContext => Future[A]): Task[A] = {
  ZIO.fromFuture { implicit ec =>
    future(ec).flatMap { result =>
      Future("Future succeeded with " + result).map(_ => result)
    }
  }
}
```

Scala's `Future` can also be converted into a `Fiber` with `Fiber.fromFuture`:

```scala mdoc
import zio.Fiber
def futureToFiber[A](future: => Future[A]): Fiber[Throwable, A] = 
  Fiber.fromFuture(future)
```

This is a pure operation, given any sensible notion of fiber equality.

### To Future

A ZIO `Task` effect can be converted into a `Future` with `ZIO#toFuture`:

```scala mdoc
def taskToFuture[A](task: Task[A]): UIO[Future[A]] = 
  task.toFuture
```

Because converting a `Task` into an (eager) `Future` is effectful, the return value of `ZIO#toFuture` is an effect. To actually begin the computation, and access the started `Future`, it is necessary to execute the effect with a runtime.

A ZIO `Fiber` can be converted into a `Future` with `Fiber#toFuture`:

```scala mdoc
def fiberToFuture[A](fiber: Fiber[Throwable, A]): UIO[Future[A]] = 
  fiber.toFuture
```

## Run to Future

The `Runtime` type has a method `unsafeRunToFuture`, which can execute a ZIO effect asynchronously, and return a `Future` that will be completed when the execution of the effect is complete.
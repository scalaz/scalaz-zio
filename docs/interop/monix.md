---
id: interop_monix
title:  "Monix"
---

Checkout `interop-monix` module for inter-operation support.

## `Task` conversions

Interop layer provides the following conversions:

- from `Task[A]` to `UIO[Task[A]]`
- from `Task[A]` to `Task[A]`

To convert an `IO` value to `Task`, use the following method:

```scala
def toTask: UIO[eval.Task[A]]
```

To perform conversion in other direction, use the following extension method
available on `IO` companion object:

```scala
def fromTask[A](task: eval.Task[A])(implicit scheduler: Scheduler): Task[A]
```

Note that in order to convert the `Task` to an `IO`, an appropriate `Scheduler`
needs to be available.

### Example

```scala mdoc
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import zio.{ IO, Runtime }
import zio.interop.monix._

object UnsafeExample extends App {

  val runtime = Runtime.default

  def main(args: Array[String]): Unit = {
    val io1 = IO.succeed(10)
    val t1  = runtime.unsafeRun(io1.toTask)

    t1.runToFuture.foreach(r => println(s"IO to task result is $r"))

    val t2  = Task(10)
    val io2 = IO.fromTask(t2).map(r => s"Task to IO result is $r")

    println(runtime.unsafeRun(io2))
  }
}
```

## `Coeval` conversions

To convert an `IO` value to `Coeval`, use the following method:

```scala
def toCoeval: UIO[eval.Coeval[A]]
```

To perform conversion in other direction, use the following extension method
available on `IO` companion object:

```scala
def fromCoeval[A](coeval: eval.Coeval[A]): Task[A]
```

### Example

```scala mdoc:reset
import monix.eval.Coeval
import zio.{ IO, Runtime }
import zio.interop.monix._

object UnsafeExample extends App {

  val runtime = Runtime.default

  def main(args: Array[String]): Unit = {
    val io1 = IO.succeed(10)
    val c1  = runtime.unsafeRun(io1.toCoeval) 

    println(s"IO to coeval result is ${c1.value}")

    val c2  = Coeval(10)
    val io2 = IO.fromCoeval(c2).map(r => s"Coeval to IO result is $r")

    println(runtime.unsafeRun(io2))
  }
}
```

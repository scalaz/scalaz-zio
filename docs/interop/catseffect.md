---
id: interop_catseffect
title:  "Cats Effect"
---

The `interop-cats` module provides interoperability for the Cats Effect ecosystem.

To use this module, add the following to your `build.sbt`:

```scala
libraryDependencies += "dev.zio" %% "zio-interop-cats" % "<version>"
```

Most of the interop functionality resides in the following package:

```scala mdoc
import zio.interop.catz._
```

## Cats Effect Instances

ZIO integrates with Typelevel libraries by providing instances of Cats Effect type classes. These type classes are used by `fs2`, `doobie` and `http4s`, and a variety of other libraries in the functional Scala ecosystem.

Due to limitations of Cats Effect, ZIO cannot provide instances for arbitrary error types. Instead, you can obtain instances only for effects whose error type extends `Throwable`.

For convenience, ZIO includes the `Task` and `RIO` type aliases, which fix the error type to `Throwable`, and may be useful for interop with Cats Effect:

```scala
type Task[   +A] = ZIO[Any, Throwable, A]
type RIO[-R, +A] = ZIO[  R, Throwable, A]
```

In order to use Cats Effect instances for these types, you should have an implicit `Runtime[R]` in scope for the environment type of your effects. The following code snippet creates an implicit `Runtime` for all the modules built into ZIO:

```scala mdoc
import zio. { Runtime, ZEnv }
implicit val runtime: Runtime[ZEnv] = Runtime.default
```

If you are using `RIO` for a custom environment `R`, then you will have to create your own `Runtime[R]`, and ensure that implicit wherever you need Cats Effect instances.

### Cats App

As a convenience, your application can extend `CatsApp`, which automatically brings an implicit `Runtime[Environment]` into scope.

### Timer

In order to get a `cats.effect.Timer[Task]` instance, we need an extra import:

```scala mdoc
import zio.interop.catz.implicits._
```

The reason why a `Timer[Task]` is not provided by the default "interop" import is that it makes testing programs that require timing capabilities very difficult. The extra import (wherever needed) makes reasoning about timing-related effects much easier.
If you are using `RIO` for a custom environment then your environment must use the `Clock` service, e.g. `R <: Clock` to get a timer.

## Example

The following example shows how to use ZIO with Doobie (a library for JDBC access) and FS2 (a streaming library), which both rely on Cats Effect instances:

```scala
import doobie.imports._
import fs2.Stream
import zio.Task
import zio.interop.catz._

val xa: Transactor[Task] = Transactor.fromDriverManager[Task](...)

def loadUsers: Stream[Task, User] =
  sql"""SELECT * FROM users""".query[User].stream.transact(xa)

val allUsers: List[User] = unsafeRun(loadUsers.compile.toList)
```

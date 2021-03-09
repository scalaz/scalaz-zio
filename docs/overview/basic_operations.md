---
id: overview_basic_operations
title:  "Basic Operations"
---

```scala mdoc
import zio._
import zio.console._
```

## Mapping

You can map over the success channel of an effect by calling the `ZIO#map` method. This lets you transform the success values of effects.

```scala mdoc:silent
import zio._

val succeeded: UIO[Int] = IO.succeed(21).map(_ * 2)
```

You can map over the error channel of an effect by calling the `ZIO#mapError` method. This lets you transform the failure values of effects.

```scala mdoc:silent
val failed: IO[Exception, Unit] = 
  IO.fail("No no!").mapError(msg => new Exception(msg))
```

Note that mapping over an effect's success or error channel does not change the success or failure of the effect, in the same way that mapping over an `Either` does not change whether the `Either` is `Left` or `Right`.

## Chaining

You can execute two effects in sequence with the `flatMap` method, which requires that you pass a callback, which will receive the value of the first effect, and can return a second effect that depends on this value:

```scala mdoc:silent
val sequenced = 
  getStrLn.flatMap(input => putStrLn(s"You entered: $input"))
```

If the first effect fails, the callback passed to `flatMap` will never be invoked, and the composed effect returned by `flatMap` will also fail.

In _any_ chain of effects, the first failure will short-circuit the whole chain, just like throwing an exception will prematurely exit a sequence of statements.

## For Comprehensions

Because the `ZIO` data type supports both `flatMap` and `map`, you can use Scala's _for comprehensions_ to build sequential effects:

```scala mdoc:silent
val program = 
  for {
    _    <- putStrLn("Hello! What is your name?")
    name <- getStrLn
    _    <- putStrLn(s"Hello, ${name}, welcome to ZIO!")
  } yield ()
```

_For comprehensions_ provide a more procedural syntax for composing chains of effects.

## Zipping

You can combine two effects into a single effect with the `ZIO#zip` method. The resulting effect succeeds with a tuple that contains the success values of both effects:

```scala mdoc:silent
val zipped: UIO[(String, Int)] = 
  ZIO.succeed("4").zip(ZIO.succeed(2))
```

Note that `zip` operates sequentially: the effect on the left side is executed before the effect on the right side.

In any `zip` operation, if either the left or right hand sides fail, then the composed effect will fail, because _both_ values are required to construct the tuple.

Sometimes, when the success value of an effect is not useful (or example, it is `Unit`), it can be more convenient to use the `ZIO#zipLeft` or `ZIO#zipRight` functions, which first perform a `zip`, and then map over the tuple to discard one side or the other:

```scala mdoc:silent
val zipRight1 = 
  putStrLn("What is your name?").zipRight(getStrLn)
```

The `zipRight` and `zipLeft` functions have symbolic aliases, known as `*>` and `<*`, respectively. Some developers find these operators easier to read:

```scala mdoc:silent
val zipRight2 = 
  putStrLn("What is your name?") *>
  getStrLn
```

## Next Step

If you are comfortable with the basic operations on ZIO effects, then the next step is to learn about [error handling](handling_errors.md).

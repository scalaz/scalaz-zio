---
id: howto_mock_services
title:  "Mock services"
---

## How to test interactions between services?

Whenever possible, we should strive to make our functions pure, which makes testing such function easy - you just need to assert on the return value.
However in larger applications there is a need for intermediate layers that delegate the work to specialized services.

For example, in a HTTP server the first layer of indirection are so called _routes_, whose job is to match the request and delegate the processing to
downstream layers. Often below there is a second layer of indirection, so called _controllers_, which consist of several business logic units grouped
by their domain. In a RESTful API that would be all operations on a certain model. The _controller_ to perform its job might call on further
specialized services for communicating with the database, sending email, logging, et cetera.

If the job of the _capability_ is to call on another _capability_, how should we test it?

## Hidden outputs

A pure function is such a function which operates only on its inputs and produces only its output. Command-like methods, by definition are impure, as
their job is to change state of the collaborating object (performing a _side effect_). For example:

```scala mdoc
import scala.concurrent.ExecutionContext.Implicits.global

trait Event
```

```scala mdoc:silent
import scala.concurrent.Future

def processEvent(event: Event): Future[Unit] = Future(println(s"Got $event"))
```

The signature of this method `Event => Future[Unit]` hints us we're dealing with a command. It returns `Unit` (well, wrapped in future, but it does
not matter here), you can't do anything useful with `Unit` and it does not contain any information. It is the equivalent of returning nothing. It is
also an unreliable return type, as when Scala expects the return type to be `Unit` it will discard whatever value it had (for details see
[Section 6.26.1][link-sls-6.26.1] of the Scala Language Specification), which may shadow the fact that the final value produced (and discarded) was
not the one you expected.

Inside the future there may be happening any side effects. It may open a file, print to console, connect to databases. We simply don't know. Let's have a look how this problem would be solved using ZIO's effect system:

```scala mdoc:reset
trait Event
```

```scala mdoc:silent
import zio._
import zio.console.Console

def processEvent(event: Event): URIO[Console, Unit] =
  console.putStrLn(s"Got $event")
```

With ZIO, we've regained to ability to reason about the effects called. We know that `processEvent` can only call on _capabilities_ of `Console`, so even though we still have `Unit` as the result, we have narrowed the possible effects space to a few.

> **Note:** this is true assuming the programmer disciplines themselves to only perform effects expressed in the type signature.
> There is no way (at the moment) to enforce this by the compiler. There is some research done in this space, perhaps future programming languages
> will enable us to further constrain side effects.

However, the same method could be implemented as:

```scala mdoc:silent
def processEvent2(event: Event): URIO[Console, Unit] =
  ZIO.unit
```

How can we test it did exactly what we expected it to do?

## Mocking

In this sort of situations we need mock implementations of our collaborator service. As _Martin Fowler_ puts it in his excellent article
[Mocks Aren't Stubs][link-test-doubles]:

> **Mocks** are (...) objects pre-programmed with expectations which form a specification of the calls they are expected to receive.

ZIO Test provides a framework for mocking your modules.

## Creating a mock service

We'll be assuming you've read [How to use modules and layers][doc-use-modules-and-layers] guide. In the main sources we define the _service_, a module alias and _capability accessors_. In test sources we're defining the _mock object_ which extends `zio.test.mock.Mock` which holds _capability tags_ and _compose layer_.

```scala mdoc:silent
// main sources

import zio.stream.{ ZSink, ZStream }
import zio.test.mock._

type Example = Has[Example.Service]

object Example {
  trait Service {
    val static                                 : UIO[String]
    def zeroArgs                               : UIO[Int]
    def zeroArgsWithParens()                   : UIO[Long]
    def singleArg(arg1: Int)                   : UIO[String]
    def multiArgs(arg1: Int, arg2: Long)       : UIO[String]
    def multiParamLists(arg1: Int)(arg2: Long) : UIO[String]
    def command(arg1: Int)                     : UIO[Unit]
    def overloaded(arg1: Int)                  : UIO[String]
    def overloaded(arg1: Long)                 : UIO[String]
    def function(arg1: Int)                    : String
    def sink(a: Int)                           : ZSink[Any, String, Int, Int, List[Int]]
    def stream(a: Int)                         : ZStream[Any, String, Int]
  }
}
```

```scala mdoc:silent
// test sources

object ExampleMock extends Mock[Example] {
  object Static             extends Effect[Unit, Nothing, String]
  object ZeroArgs           extends Effect[Unit, Nothing, Int]
  object ZeroArgsWithParens extends Effect[Unit, Nothing, Long]
  object SingleArg          extends Effect[Int, Nothing, String]
  object MultiArgs          extends Effect[(Int, Long), Nothing, String]
  object MultiParamLists    extends Effect[(Int, Long), Nothing, String]
  object Command            extends Effect[Int, Nothing, Unit]
  object Overloaded {
    object _0 extends Effect[Int, Nothing, String]
    object _1 extends Effect[Long, Nothing, String]
  }
  object Function extends Method[Int, Throwable, String]
  object Sink     extends Sink[Any, String, Int, Int, List[Int]]
  object Stream   extends Stream[Any, String, Int]

  val compose: URLayer[Has[Proxy], Example] = ???
}
```

A _capability tag_ is just a value which extends the `zio.test.mock.Capability[R <: Has[_], I, E, A]` type constructor, where:
- `R` is the type of environment the method belongs to
- `I` is the type of methods input arguments
- `E` is the type of error it can fail with
- `A` is the type of return value it can produce

The `Capability` type is not publicly available, instead you have to extend `Mock` dependent types `Effect`, `Method`, `Sink` or `Stream`.

We model input arguments according to following scheme:
- for zero arguments the type is `Unit`
- for one or more arguments, regardless in how many parameter lists, the type is a `TupleN` where `N` is the size of arguments list

> **Note:** we're using tuples to represent multiple argument methods, which follows with a limit to max 22 arguments, as is Scala itself limited.

For overloaded methods we nest a list of numbered objects, each representing subsequent overloads.

Finally we need to define a _compose layer_ that can create our environment from a `Proxy`.
A `Proxy` holds the mock state and serves predefined responses to calls.

```scala mdoc
def withRuntime[R]: URIO[R, Runtime[R]] = ???
```

```scala mdoc:silent
import ExampleMock._

val compose: URLayer[Has[Proxy], Example] =
  ZLayer.fromServiceM { proxy =>
    withRuntime.map { rts =>
      new Example.Service {
        val static                                 = proxy(Static)
        def zeroArgs                               = proxy(ZeroArgs)
        def zeroArgsWithParens()                   = proxy(ZeroArgsWithParens)
        def singleArg(arg1: Int)                   = proxy(SingleArg, arg1)
        def multiArgs(arg1: Int, arg2: Long)       = proxy(MultiArgs, arg1, arg2)
        def multiParamLists(arg1: Int)(arg2: Long) = proxy(MultiParamLists, arg1, arg2)
        def command(arg1: Int)                     = proxy(Command, arg1)
        def overloaded(arg1: Int)                  = proxy(Overloaded._0, arg1)
        def overloaded(arg1: Long)                 = proxy(Overloaded._1, arg1)
        def function(arg1: Int)                    = rts.unsafeRunTask(proxy(Function, arg1))
        def sink(a: Int)                           = rts.unsafeRun(proxy(Sink, a).catchAll(error => UIO(ZSink.fail[String, Int](error))))
        def stream(a: Int)                         = rts.unsafeRun(proxy(Stream, a))
      }
    }
  }
```

> **Note:** The `withRuntime` helper is defined in `Mock`. It accesses the Runtime via `ZIO.runtime` and if you're on JS platform, it will replace the executor to an unyielding one.

A reference to this layer is passed to _capability tags_ so it can be used to automatically build environment for composed expectations on
multiple services.

> **Note:** for non-effectful capabilities you need to unsafely run the final effect to satisfy the required interface. For `ZSink` you also need to map the error into a failed sink as demonstrated above.

## Complete example

```scala mdoc:reset
trait AccountEvent
```

```scala mdoc:silent
// main sources

import zio._
import zio.console.Console
import zio.test.mock._

type AccountObserver = Has[AccountObserver.Service]

object AccountObserver {
  trait Service {
    def processEvent(event: AccountEvent): UIO[Unit]
    def runCommand(): UIO[Unit]
  }

  def processEvent(event: AccountEvent) =
    ZIO.accessM[AccountObserver](_.get.processEvent(event))

  def runCommand() =
    ZIO.accessM[AccountObserver](_.get.runCommand)

  val live: ZLayer[Console, Nothing, AccountObserver] =
    ZLayer.fromService[Console.Service, Service] { console =>
      new Service {
        def processEvent(event: AccountEvent): UIO[Unit] =
          for {
            _    <- console.putStrLn(s"Got $event")
            line <- console.getStrLn.orDie
            _    <- console.putStrLn(s"You entered: $line")
          } yield ()

        def runCommand(): UIO[Unit] =
          console.putStrLn("Done!")
      }
    }
}
```

```scala mdoc:silent
// test sources

object AccountObserverMock extends Mock[AccountObserver] {

  object ProcessEvent extends Effect[AccountEvent, Nothing, Unit]
  object RunCommand   extends Effect[Unit, Nothing, Unit]

  val compose: URLayer[Has[Proxy], AccountObserver] =
    ZLayer.fromService { proxy =>
      new AccountObserver.Service {
        def processEvent(event: AccountEvent) = proxy(ProcessEvent, event)
        def runCommand(): UIO[Unit]           = proxy(RunCommand)
      }
    }
}
```

> **Note:** ZIO provides some useful macros to help you generate repetitive code, see [Scrapping the boilerplate with macros][doc-macros].

## Provided ZIO services

For each built-in ZIO service you will find their mockable counterparts in `zio.test.mock` package:
- `MockClock` for `zio.clock.Clock`
- `MockConsole` for `zio.console.Console`
- `MockRandom` for `zio.random.Random`
- `MockSystem` for `zio.system.System`

## Setting up expectations

To create expectations we use the previously defined _capability tags_:

```scala mdoc
object Example {
  trait Service {
    def zeroArgs: UIO[Int]
    def singleArg(arg1: Int): UIO[String]
  }
}
object ExampleMock extends Mock[Has[Example.Service]] {
  object ZeroArgs  extends Effect[Unit, Nothing, Int]
  object SingleArg extends Effect[Int, Nothing, String]
  val compose: URLayer[Has[Proxy], Has[Example.Service]] = ZLayer.fromService(proxy => new Example.Service {
    def zeroArgs             = proxy(ZeroArgs)
    def singleArg(arg1: Int) = proxy(SingleArg, arg1)
  })
}
```

```scala mdoc:silent
import zio.test.Assertion._
import zio.test.mock.Expectation._
import zio.test.mock.MockSystem

val exp01 = ExampleMock.SingleArg( // capability to build an expectation for
  equalTo(42), // assertion of the expected input argument
  value("bar") // result, that will be returned
)
```

For methods that take input, the first argument will be an assertion on input, and the second the predefined result.

In the most robust example, the result can be either a successful value or a failure. To construct either we must use
one of following combinators from `zio.test.mock.Expectation` companion object:

- `failure[E](failure: E)` Expectation result failing with `E`
- `failureF[I, E](f: I => E)` Maps the input arguments `I` to expectation result failing with `E`.
- `failureM[I, E](f: I => IO[E, Nothing])` Effectfully maps the input arguments `I` to expectation result failing with `E`.
- `never` Expectation result computing forever.
- `unit` Expectation result succeeding with `Unit`.
- `value[A](value: A)` Expectation result succeeding with `A`.
- `valueF[I, A](f: I => A)` Maps the input arguments `I` to expectation result succeeding with `A`.
- `valueM[I, A](f: I => IO[Nothing, A])` Effectfully maps the input arguments `I` expectation result succeeding with `A`.

For methods that take no input, we only define the expected output.

```scala mdoc:silent
val exp02 = ExampleMock.ZeroArgs(value(42))
```

For methods that may return `Unit`, we may skip the predefined result (it will default to successful value) or use `unit` helper.

```scala mdoc:silent
import zio.test.mock.MockConsole

val exp03 = MockConsole.PutStrLn(equalTo("Welcome to ZIO!"))
val exp04 = MockConsole.PutStrLn(equalTo("Welcome to ZIO!"), unit)
```

For methods that may return `Unit` and take no input we can skip both:

```scala mdoc:silent
val exp05 = AccountObserverMock.RunCommand()
```

Finally we're all set and can create ad-hoc mock environments with our services.

```scala mdoc:silent
import zio.test._

val event = new AccountEvent {}
val app: URIO[AccountObserver, Unit] = AccountObserver.processEvent(event)
val mockEnv: ULayer[Console] = (
  MockConsole.PutStrLn(equalTo(s"Got $event"), unit) ++
  MockConsole.GetStrLn(value("42")) ++
  MockConsole.PutStrLn(equalTo("You entered: 42"))
)
```

We can combine our expectation to build complex scenarios using combinators defined in `zio.test.mock.Expectation`:

- `andThen` (alias `++`) Compose two expectations, producing a new expectation to **satisfy both sequentially**.
- `and` (alias `&&`) Compose two expectations, producing a new expectation to **satisfy both in any order**.
- `or` (alias `||`) Compose two expectations, producing a new expectation to **satisfy only one of them**.
- `repeated` Repeat expectation within given bounds, produces a new expectation to **satisfy itself sequentially given number of times**.
- `atLeast` Lower-bounded variant of `repeated`, produces a new expectation to satisfy **itself sequentially at least given number of times**.
- `atMost` Upper-bounded variant of `repeated`, produces a new expectation to satisfy **itself sequentially at most given number of times**.
- `optional` Alias for `atMost(1)`, produces a new expectation to satisfy **itself at most once**.

## Providing mocked environment

```scala mdoc:silent
object AccountObserverSpec extends DefaultRunnableSpec {
  def spec = suite("processEvent")(
    testM("calls putStrLn > getStrLn > putStrLn and returns unit") {
      val result = app.provideLayer(mockEnv >>> AccountObserver.live)
      assertM(result)(isUnit)
    }
  )
}
```

## Mocking unused collaborators

Often the dependency on a collaborator is only in some branches of the code. To test the correct behaviour of branches without depedencies, we still have to provide it to the environment, but we would like to assert it was never called. With the `Mock.empty` method you can obtain a `ZLayer` with an empty service (no calls expected).

```scala mdoc:silent
object MaybeConsoleSpec extends DefaultRunnableSpec {
  def spec = suite("processEvent")(
    testM("expect no call") {
      def maybeConsole(invokeConsole: Boolean) =
        ZIO.when(invokeConsole)(console.putStrLn("foo"))

      val maybeTest1 = maybeConsole(false).provideLayer(MockConsole.empty)
      val maybeTest2 = maybeConsole(true).provideLayer(MockConsole.PutStrLn(equalTo("foo")))
      assertM(maybeTest1)(isUnit) *> assertM(maybeTest2)(isUnit)
    }
  )
}
```

## Mocking multiple collaborators

In some cases we have more than one collaborating service being called. You can create mocks for rich environments and as you enrich the environment by using _capability tags_ from another service, the underlaying mocked layer will be updated.

```scala mdoc:silent
import zio.console.Console
import zio.random.Random
import zio.test.mock.MockRandom

val combinedEnv: ULayer[Console with Random] = (
  MockConsole.PutStrLn(equalTo("What is your name?")) ++
  MockConsole.GetStrLn(value("Mike")) ++
  MockRandom.NextInt(value(42)) ++
  MockConsole.PutStrLn(equalTo("Mike, your lucky number today is 42!"))
)

val combinedApp =
  for {
    _    <- console.putStrLn("What is your name?")
    name <- console.getStrLn.orDie
    num  <- random.nextInt
    _    <- console.putStrLn(s"$name, your lucky number today is $num!")
  } yield ()

val result = combinedApp.provideLayer(combinedEnv)
assertM(result)(isUnit)
```

## Polymorphic capabilities

Mocking polymorphic methods is also supported, but the interface must require `zio.Tag` implicit evidence for each type parameter.

```scala mdoc:silent
// main sources
type PolyExample = Has[PolyExample.Service]

object PolyExample {
  trait Service {
    def polyInput[I: Tag](input: I): Task[String]
    def polyError[E: Tag](input: Int): IO[E, String]
    def polyOutput[A: Tag](input: Int): Task[A]
    def polyAll[I: Tag, E: Tag, A: Tag](input: I): IO[E, A]
  }
}
```

In the test sources we construct partially applied _capability tags_ by extending `Method.Poly` family. The unknown types
must be provided at call site. To produce a final monomorphic `Method` tag we must use the `of` combinator and pass the
missing types.

```scala mdoc:silent
// test sources
object PolyExampleMock extends Mock[PolyExample] {

  object PolyInput  extends Poly.Effect.Input[Throwable, String]
  object PolyError  extends Poly.Effect.Error[Int, String]
  object PolyOutput extends Poly.Effect.Output[Int, Throwable]
  object PolyAll    extends Poly.Effect.InputErrorOutput

  val compose: URLayer[Has[Proxy], PolyExample] =
    ZLayer.fromServiceM { proxy =>
      withRuntime.map { rts =>
        new PolyExample.Service {
          def polyInput[I: Tag](input: I)                     = proxy(PolyInput.of[I], input)
          def polyError[E: Tag](input: Int)                   = proxy(PolyError.of[E], input)
          def polyOutput[A: Tag](input: Int)                  = proxy(PolyOutput.of[A], input)
          def polyAll[I: Tag, E: Tag, A: Tag](input: I) = proxy(PolyAll.of[I, E, A], input)
        }
      }
    }
}
```

Similarly, we use the same `of` combinator to refer to concrete monomorphic call in our test suite when building expectations:

```scala mdoc:silent
import PolyExampleMock._

val exp06 = PolyInput.of[String](equalTo("foo"), value("bar"))
val exp07 = PolyInput.of[Int](equalTo(42), failure(new Exception))
val exp08 = PolyInput.of[Long](equalTo(42L), value("baz"))

val exp09 = PolyAll.of[Int, Throwable, String](equalTo(42), value("foo"))
val exp10 = PolyAll.of[Int, Throwable, String](equalTo(42), failure(new Exception))
```

## More examples

You can find more examples in the `examples` and `test-tests` subproject:

- [MockExampleSpec][link-gh-mock-example-spec]
- [BasicMockSpec][link-gh-basic-mock-spec]
- [AdvancedMockSpec][link-gh-advanced-mock-spec]
- [EmptyMockSpec][link-gh-empty-mock-spec]
- [ComposedMockSpec][link-gh-composed-mock-spec]
- [ComposedEmptyMockSpec][link-gh-composed-empty-mock-spec]
- [PolyMockSpec][link-gh-poly-mock-spec]

[doc-use-modules-and-layers]: use_modules_and_layers.md
[doc-macros]: howto_macros.md
[link-sls-6.26.1]: https://scala-lang.org/files/archive/spec/2.13/06-expressions.html#value-conversions
[link-test-doubles]: https://martinfowler.com/articles/mocksArentStubs.html
[link-gh-mock-example-spec]: https://github.com/zio/zio/blob/master/examples/shared/src/test/scala/zio/examples/test/MockExampleSpec.scala
[link-gh-basic-mock-spec]: https://github.com/zio/zio/blob/master/test-tests/shared/src/test/scala/zio/test/mock/BasicMockSpec.scala
[link-gh-advanced-mock-spec]: https://github.com/zio/zio/blob/master/test-tests/shared/src/test/scala/zio/test/mock/AdvancedMockSpec.scala
[link-gh-empty-mock-spec]: https://github.com/zio/zio/blob/master/test-tests/shared/src/test/scala/zio/test/mock/EmptyMockSpec.scala
[link-gh-composed-mock-spec]: https://github.com/zio/zio/blob/master/test-tests/shared/src/test/scala/zio/test/mock/ComposedMockSpec.scala
[link-gh-composed-empty-mock-spec]: https://github.com/zio/zio/blob/master/test-tests/shared/src/test/scala/zio/test/mock/ComposedEmptyMockSpec.scala
[link-gh-poly-mock-spec]: https://github.com/zio/zio/blob/master/test-tests/shared/src/test/scala/zio/test/mock/PolyMockSpec.scala

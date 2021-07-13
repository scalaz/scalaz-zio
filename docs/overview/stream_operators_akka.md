---
id: stream_operators_akka
title:  "Stream operators (akka)"
---

This page provides an overview of ZIO stream operators, listed from the point of view of a person familiar with [akka streams](https://doc.akka.io/docs/akka/current/stream/index.html). Presenting the operators in this way can help with migrating an Akka application.

## Operators on streams

The equivalent of an akka `Source[T,M]` is a ZIO `ZStream[R,E,A]`. In both cases, the stream consists of elements of type `T` / `E`.

ZIO does not have the equivalent of akka's "materialized value" `M`, since in ZIO all effects are ideally postponed until the outermost application layer. If an Akka application is relying on a materialized value, an equivalent can be a ZIO function that returns a tuple, e.g. `Source[T,M]` would be something akin to `(Stream[E,T], IO[E,M])`. If needed, the `Stream.broadcast` operator can be used to turn one ZIO stream into these two values.

The selection of akka operators is taken from akka's [operator overview](https://doc.akka.io/docs/akka/current/stream/operators/index.html).

| Akka operator                                     | ZIO equivalent                                                                                                                                                                                          |
|:--------------------------------------------------|:--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `source.asSourceWithContext`                      | No direct equivalent. Use tuples for manual context propagation.                                                                                                                                        |
| `source.asSubscriber`                             | Use [`zio.interop.reactivestreams`](https://zio.dev/docs/interop/interop_reactivestreams)                                                                                                               |
| `Source.combine(a,b)(Concat)`                     | `a.concat(b)`                                                                                                                                                                                           |
| `Source.combine(a,b)(Merge)`                      | Either `a.merge(b)` (with undeterministic order, like akka),<br/> or `a.interleave(b)` (deterministic ordering), <br/>or `a.interleaveWith(b)(s)` (requiring manual control of ordering)                |
| `Source.completionStage(c)`                       | No direct equivalent. Use `ZStream.fromEffect(ZIO.fromFuture(_ => c.asScala))` using `import scala.jdk.FutureConverters._` for 2.13, or `.toScala` from `scala.compat.java8.FutureConverters` for 2.12) |
| `Source.completionStageSource(c)`                 | No direct equivalent. Assuming `c` is a `CompletionStage[ZStream[...]]`, one would do `ZStream.unwrap(ZIO.fromFuture(_ => c.asScala))`                                                                  |
| `Source.cycle(i: => Iterator[T])`                 | `ZStream.fromIterator(iterator).forever` (since akka's `.cycle` internally repeats the iterator forever)                                                                                                |
| `Source.empty`                                    | `ZStream.empty`                                                                                                                                                                                         |
| `Source.failed(x: Throwable)`                     | `ZStream.fail(error: E)` (`ZStream` can fail with other types than just `Throwable`)                                                                                                                    |
| `Source.apply(seq: immutable.Seq[T])`             | `ZStream.fromIterable(i: Iterable[T])` (akka requires an immutable seq and wraps it; ZIO takes any `Iterable` but immediately converts it to a `Chunk` internally)                                      |
| `Source.fromIterator(i: () => Iterator[T])`       | `ZStream.fromIterator(i: =>Iterator[T])`, both will create the iterator when the stream starts, and request new elements on demand.                                                                     |
| `Source.fromJavaStream(s: () => BaseStream[T,S])` | No direct equivalent. One could do `ZStream.fromIterator(s.iterator())`, but this will NOT close the stream when done. An example using `ZManaged` could be given here.                                 |
| `Source.fromPublisher`                            | Use [`zio.interop.reactivestreams`](https://zio.dev/docs/interop/interop_reactivestreams)                                                                                                               |
| `Source.future(f: Future[T])`                     | No direct equivalent. Use `ZStream.fromEffect(ZIO.fromFuture(_ => f))`.                                                                                                                                 |
| `Source.futureSource(f: Future[Source[T,M]])`     | No direct equivalent. Assuming `c` is a `Future[ZStream[...]]`, one would do `ZStream.unwrap(ZIO.fromFuture(_ => c))`                                                                                   |
| `Source.lazy *`                                   | No direct equivalent. ZIO is more "lazy" in that evaluation is more naturally postponed, but akka's `lazy` methods postpone creation until there is actual stream _demand_.                             |
| `Source.maybe(p: Promise[Option[T]])`             | No direct equivalent. Use `ZStream.fromEffect(ZIO.fromFuture(_ => p.future)).collect { case Some(t) => t }`                                                                                             |
| `Source.never`                                    | `ZStream.never`                                                                                                                                                                                         |
| `Source.queue(...): Source[T,SourceQueue[T]]`     | `ZStream.fromQueue(q: ZQueue[...T])` (usage will be slightly different, since akka _materializes_ to the queue instance, wheras in ZIO it's a method argument).                                         |
| `Source.range`                                    | `ZStream.range` (but ZIO lacks the ability to use a step size other than 1)                                                                                                                             |
| `Source.repeat(t: T)`                             | `ZStream.repeat(t: => T)` (but ZIO evaluates `=> T` for each element)                                                                                                                                   |
| `Source.single(t: T)`                             | `ZStream.apply(t)`                                                                                                                                                                                      |
| `Source.tick(delay, interval, t: T)`              | No direct equivalent. Use `ZStream.apply(t).schedule(Schedule.duration(delay) andThen Schedule.spaced(interval))`                                                                                       |
| `Source.unfold(s: S)(f: S => Option[(S,E)])`      | `ZStream.unfold(s)(f)`                                                                                                                                                                                  |
| `Source.unfoldAsync`                              | `ZStream.unfoldM` (but that expects a `ZIO` inside, so you have to do `ZIO.fromFuture` where needed)                                                                                                    |
| `Source.unfoldResource`                           | No direct equivalent, construct a `ZManaged[ZStream[...]]` instead, with appropriate constructors.                                                                                                      |
| `Source.zipN`                                     | No equivalent. ZIO provides `zipN`, but only for up to 4 differently-typed streams. Akka's is for N same-typed streams.                                                                                 |
| `Source.zipWithN`                                 | `ZStream.zipN`, but only for up to 4 differently-typed streams. Akka's is for N same-typed streams.                                                                                                     |

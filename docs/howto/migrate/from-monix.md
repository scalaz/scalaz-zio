---
id: from-monix
title:  "How to Migrate from Monix to ZIO?"
---

Monix's `Task[A]` can be easily replaced with ZIO's `Task[A]` (an alias for `ZIO[Any, Throwable, A]`).
Translation should be relatively straightfoward. Below, you'll find tables showing the ZIO equivalents of 
 `monix.eval.Task`'s methods. 

Once you've completed the initial translation, you'll find that ZIO is outfitted with many additional
methods which have no Monix equivalents, so have fun exploring the API and see if you can rewrite some
of your logic at a higher level of abstraction, with more powerful combinators and fewer lines code.

If you are using operators from from Cats Effect extension methods see also 
[here](https://zio.dev/docs/howto/migrate/from-cats-effect).

### Methods on Trait

| Monix | ZIO |
|-------|-----|
| `attempt` | `either` |
| `bracketCase` | `bracketExit` |
| `bracketE` | `bracketExit` |
| `bracket` | `bracket` |
| `delayExecution` | `delay` |
| `dematerialize` | `absolve` |
| `doOnCancel` | `onInterrupt` |
| `doOnFinish` | `onExit` |
| `failed` | `flip` |
| `flatMap` | `flatMap` |
| `flatten` | `flatten` |
| `guaranteeCase` | `ensuringExit` |
| `guarantee` | `ensuring` |
| `loopForever` | `forever` |
| `materialize` | `either` |
| `memoize` | `memoize` |
| `onErrorFallbackTo` | `orElse` |
| `onErrorHandleWith` | `catchAll` |
| `onErrorRecoverWith` | `catchSome` |
| `onErrorRestart` | `retryN` |
| `redeemWith` | `foldM` |
| `redeem` | `fold` |
| `restartUntil` | `repeatUntil` |
| `start` | `fork` |
| `timed` | `timed` |
| `timeout` | `timeout` |
| `uncancelable` | `uninterruptible` |

### Methods on Companion Object

| Monix  | ZIO |
|-------|-----|
| `apply` | `apply` |
| `asyncF` | `effectAsyncM` |
| `async` | `effectAsync` |
| `cancelable` | `effectAsyncInterrupt` |
| `deferFuture` | `fromFuture` |
| `defer` | `effectSuspend` |
| `delay` | `effect` |
| `eval` | `effect` |
| `fromEither` | `fromEither` |
| `fromFuture` | `fromFuture` |
| `fromTry` | `fromTry` |
| `map2` | `mapN` |
| `mapBoth` | `mapParN` |
| `never` | `never` |
| `now` | `succeed` |
| `parMap2` | `mapParN` |
| `parSequenceN` | `collectAllParN` |
| `parSequence` | `collectAllPar` |
| `parTraverseN` | `foreachParN` |
| `parTraverse` | `foreachPar` |
| `parZip2` | `tupledPar` |
| `pure` | `succeed` |
| `racePair` | `raceWith` |
| `race` | `raceFirst` |
| `raiseError` | `fail` |
| `sequence` | `collectAll` |
| `shift` | `yield` |
| `sleep` | `sleep` |
| `suspend` | `effectSuspend` |
| `traverse` | `foreach` |
| `unit` | `unit` |

### Data Structures

| Monix / Cats Effect | ZIO |
|-------|-----|
| `Deferred` | `Promise` |
| `Fiber` | `Fiber` |
| `MVar` | `Queue` |
| `Ref` | `Ref` |
| `Resource` | `ZManaged` |
| `Semaphore` | `Semaphore` |
| `TaskApp` | `App` |
| `TaskLocal` | `FiberRef` |
| `Task` | `Task` |


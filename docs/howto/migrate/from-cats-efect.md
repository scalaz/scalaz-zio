---
id: from-cats-efect
title:  "How to Migrate from Cats Effect to ZIO?"
---

Cats `IO[A]` can be easily replaced with ZIO's `Task[A]` (an alias for `ZIO[Any, Throwable, A]`).
Translation should be relatively straightfoward. Below, you'll find tables showing the ZIO equivalents of
various `cats.*`'s methods.

### Methods on cats.FlatMap.Ops

| cats | ZIO |
|-------|-----|
|`flatMap`|`flatMap`|
|`flatten`|`flatten`|
|`productREval`|`zipRight`|
|`productLEval`|`zipLeft`|
|`mproduct`|`zipPar`|
|`flatTap`|`tap`|

### TODO

TODO
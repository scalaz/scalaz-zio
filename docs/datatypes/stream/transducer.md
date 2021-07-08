---
id: transducer
title: "Transducer"
---

`Transducer[E, A, B]` is a type alias for `ZTransducer[Any, E, A, B]`. It is a stream transducer that doesn't require any services, so except the `R` type-parameter, all other things are the same.

```scala
type Sink[+E, A, +L, +B] = ZSink[Any, E, A, L, B]
```

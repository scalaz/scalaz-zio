---
id: stream
title: "Stream"
---

`Stream[E, A]` is a type alias for `ZStream[Any, E, A]`, which represents a ZIO stream that does not require any services, and may fail with an `E`, or produce elements with an `A`.

```scala
type Stream[+E, +A] = ZStream[Any, E, A]
```

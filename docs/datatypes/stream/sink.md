---
id: sink 
title: "Sink"
---

`Sink[E, A, L, B]` is a type alias for `ZSink[Any, E, A, L, B]`. We can think of a `Sink` as a function that does not require any services and will consume a variable amount of `A` elements (could be 0, 1, or many!), might fail with an error of type `E`, and will eventually yield a value of type `B`. The `L` is the type of elements in the leftover.

```scala
type Sink[+E, A, +L, +B] = ZSink[Any, E, A, L, B]
```
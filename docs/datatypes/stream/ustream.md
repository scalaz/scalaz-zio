---
id: ustream 
title: "UStream"
---

`UStream[A]` is a type alias for `ZStream[Any, Nothing, A]`, which represents a ZIO stream that does not require any services, it cannot fail, and after evaluation, it may emit zero or more values of type `A`.

```scala
type UStream[+A] = ZStream[Any, Nothing, A]
```

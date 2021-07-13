---
id: zrefsynchronized
title: "ZRef.Synchronized"
---

A `ZRef.Synchronized[RA, RB, EA, EB, A, B]` is a polymorphic, purely functional description of a mutable reference. 

The fundamental operations of a `ZRef.Synchronized`are `set` and `get`. 
- **`set`** takes a value of type `A` and sets the reference to a new value, requiring an environment of type `RA` and potentially failing with an error of type `EA`. 
- **`get`** gets the current value of the reference and returns a value of type `B`, requiring an environment of type
`RB` and potentially failing with an error of type `EB`.

When the error and value types of the `ZRef.Synchronized` are unified, that is, it is a `ZRef.Synchronized[E, E, A, A]`, the `ZRef.Synchronized` also supports atomic `modify` and `update` operations.


> _**Note:**_
>
> Unlike `ZRef`, `ZRef.Synchronized` allows performing effects within update operations, at some cost to performance. Writes will semantically block other writers, while multiple readers can read simultaneously.

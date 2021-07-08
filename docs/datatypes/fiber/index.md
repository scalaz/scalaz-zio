---
id: index
title: "Introduction"
---

## Introduction

A Fiber can be thought of as a virtual thread. A Fiber is the analog of a Java thread (`java.lang.thread`), but it performs much better. Fibers, on the other hand, are implemented in such a fashion that a single JVM thread will end up executing many fibers. We can think of fibers as unbounded JVM threads.

> _**Warning**, if you are not an advanced programmer:_
>
> You should avoid fibers. If you can avoid fibers, then do it. ZIO gives you many concurrent primitives like `raceWith`, `zipPar`, `foreachPar`, and so forth, which allows you to avoid using fibers directly.
>
> Fibers just like threads are low-level constructs. It's not generally recommended for an average programmer to manually use fibers. It is very easy to make lots of mistakes or to introduce performance problems by manually using them.

## Why Fibers?

There are some limitations with JVM threads:

1. **They are scarce** — Threads on the JVM map to the operating system level threads which imposes an upper bound on the number of threads that we can have inside our application.

2. **Expensive on creation** — Their creation is expensive in terms of time and memory complexity.

3. **Much Overhead on Context Switching** — Switching between the execution of one thread to another thread is not cheap, and it takes a lot of time.

4. **Lack of Composability** — Threads are not typed. They don't have a meaningful return type, due to this limitation, we cannot compose threads. Also, it has no type parameters for error and it is assumed if that thread started it might throw some exception of throwable type. In Java when we create a thread, we should provide a `run` function that returns void. It's a void returning method. So threads cannot finish with any specific value.

5. **Synchronous**

In the following sections, we are going to discuss the key features of fibers, and how fibers overcame Java thread drawbacks.

### Unbounded Size

So whereas the mapping from JVM threads to operating system threads is one-to-one, **the mapping of fiber to a thread is many-to-one**. That is to say, any JVM thread will end up executing anywhere from hundreds to thousands even tens of thousands of threads concurrently, by hopping back and forth between them as necessary. They give us virtual threads in which it has the benefits of threads but the scalability way beyond threads. In other words, fibers provide us massive concurrently with **lightweight green threading** on the JVM.

As a rough rule of thumb, we can have an application with a thousand real threads. No problem, modern servers can support applications with a thousand threads. However, we cannot have an application with a hundred thousand threads, that application will die. That just won't make any progress. The JVM nor our operating system can physically support a hundred thousand threads. However, it is no problem to have a Scala application with a hundred thousand fibers that application can perform in a very high-performance fashion, and the miracle that enables that to happen is fiber.

### Lightweight

**JVM threads are expensive to create in order of time and memory complexity.** Also it takes a lot of time to switch between one thread of execution to another. Fibers are virtual and, and as they use **green threading**, they are considered to be **lightweight cooperative threads**, this means that fibers always _yield_ their executions to each other without the overhead of preemptive scheduling.

### Asynchronous

Fiber is asynchronous and, a thread is always synchronous. That is why fibers have higher scalability because they are asynchronous. Threads are not, that is why they don't scale as well.

### Typed and Composable

**Fibers have typed error and success values**. So actually fiber has two type parameters `E` and `A`:

- The `E` corresponds to the error channel. It indicates the error type with which the fiber can fail.

- The `A` corresponds to the success value of the computation. That is the type with which the fiber can succeed. Whereas fibers can finish with the value of type `A`.

The fact, that fibers are typed allows us to write more type-safe programs. Also, it increases the compositional properties of our programs. Because we can say, we are going to wait on that fiber to finish and when it's done, we are going to get its value of type `A`.

### Interrupt Safe

With threads in Java, it is not a safe operation to terminate them, by using the stop method. The stop operation has been [deprecated](https://docs.oracle.com/javase/1.5.0/docs/guide/misc/threadPrimitiveDeprecation.html). So this is not a safe operation to force kill a thread. Instead, we should try to request an interruption to the thread, but in this case, **the thread may not respond to our request, and it may just go forever**.

**Fiber has a safe version of this functionality that works very well**. Just like we can interrupt a thread, we can interrupt a fiber too, but interruption of fibers is much more reliable. It will always work, and **it probably works very fast**. We don't need to wait around, we can just try to interrupt them, and they will be gone very soon.

### Structured Concurrency

Until now, we find that ZIO fiber solves a lot of drawbacks of using Java threads. With fibers, we can have hundreds of thousands and even thousands of thousands of fibers are started and working together. We reached a very massive concurrently with fibers. Now how can we manage them? Some of them are top-level fibers and some others are forked and become children of their parents. How can we manage their scopes, how to keep track of all fibers, and prevent them to leak? What happens during the execution of a child fiber, the parent execution interrupted? The child fibers should be scoped to their parent fibers. We need a way to manage these scopes automatically. This is where structured concurrency shines.

> _**Important**:_
>
> It's worth mentioning that in the ZIO model, all codes run on the fiber. There is no such thing as code that is executing outside of the fiber. When we create a main function in ZIO that returns an effect, even if we don't explicitly fork a fiber when we execute that effect, that effect will execute on what is called the main fiber. It's a top-level fiber.
>
>It's just like if we have a main function in Java then that main function will execute on the main thread. There is no code in Java that does not execute on a thread. All code executes on a thread even if you didn't create a thread.

ZIO has support for structured concurrency. The way ZIO structured concurrency works is that **the child fibers are scoped to their parent fibers** which means **when the parent effect is done running then its child's effects will be automatically interrupted**. So when we fork, and we get back a fiber, the fiber's lifetime is bound to the parent fiber, that forked it. It is very difficult to leak fibers because child fibers are guaranteed to complete before their parents.

The structure concurrency gives us a way to reason about fiber lifespans. We can statically reason about the lifetimes of children fibers just by looking at our code. We don't need to insert complicated logic to keep track of all the child fibers and manually shut them down.

#### Global Lifetime

Sometimes we want a child fiber to outlive the scope of the parent. what do you do in that case? well, we have another operator called `forkDaemon`. The `forkDaemon` forks the fiber as a daemon fiber. Daemon fibers can outlive their parents. They can live forever. They run in the background doing their work until they end with failure or success. This gives us a way to spawn background jobs that should just keep on going regardless of what happens to the parent.

#### Fine-grained Scope

If we need a very flexible fine-grained control over the lifetime of a fiber there is another operator called `forkin`. We can fork a fiber inside a specific scope, when that scope is closed then the fiber will be terminated.

## Fiber Data Types

ZIO fiber contains a few data types that can help us solve complex problems:

- **[Fiber](fiber.md)** — A fiber value models an `IO` value that has started running, and is the moral equivalent of a green thread.
- **[FiberRef](fiberref.md)** — `FiberRef[A]` models a mutable reference to a value of type `A`. As opposed to `Ref[A]`, a value is bound to an executing `Fiber` only.  You can think of it as Java's `ThreadLocal` on steroids.
- **[Fiber.Status](fiberstatus.md)** — `Fiber.Status` describe the current status of a Fiber.
- **[Fiber.Id](fiberid.md)** — `Fiber.Id` describe the unique identity of a Fiber.
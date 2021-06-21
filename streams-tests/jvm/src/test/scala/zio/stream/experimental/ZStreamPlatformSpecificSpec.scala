package zio.stream.experimental

import zio.test.Assertion.{containsCause, equalTo, failsCause, isFalse, isTrue}
import zio.test.{Gen, ZSpec, assert, assertM, checkM}
import zio.{Cause, Chunk, IO, Promise, Ref, Task, UIO, ZIO, ZIOBaseSpec}

import scala.concurrent.ExecutionContext.global

object ZStreamPlatformSpecificSpec extends ZIOBaseSpec {
  def spec: ZSpec[Environment, Failure] = suite("ZStream JVM experimental")(
    suite("Constructors")(
      suite("effectAsyncMaybe")(
        testM("effectAsyncMaybe signal end stream") {
          for {
            result <- ZStream
                        .effectAsyncMaybe[Any, Nothing, Int] { k =>
                          k(IO.fail(None))
                          None
                        }
                        .runCollect
          } yield assert(result)(equalTo(Chunk.empty))
        },
        testM("effectAsyncMaybe Some")(checkM(Gen.chunkOf(Gen.anyInt)) { chunk =>
          val s = ZStream.effectAsyncMaybe[Any, Throwable, Int](_ => Some(ZStream.fromIterable(chunk)))

          assertM(s.runCollect.map(_.take(chunk.size)))(equalTo(chunk))
        }),
        testM("effectAsyncMaybe None")(checkM(Gen.chunkOf(Gen.anyInt)) { chunk =>
          val s = ZStream.effectAsyncMaybe[Any, Throwable, Int] { k =>
            global.execute(() => chunk.foreach(a => k(Task.succeed(Chunk.single(a)))))
            None
          }

          assertM(s.take(chunk.size.toLong).runCollect)(equalTo(chunk))
        }),
        testM("effectAsyncMaybe back pressure") {
          for {
            refCnt  <- Ref.make(0)
            refDone <- Ref.make[Boolean](false)
            stream = ZStream.effectAsyncMaybe[Any, Throwable, Int](
                       cb => {
                         global.execute { () =>
                           // 1st consumed by sink, 2-6 – in queue, 7th – back pressured
                           (1 to 7).foreach(i => cb(refCnt.set(i) *> ZIO.succeedNow(Chunk.single(1))))
                           cb(refDone.set(true) *> ZIO.fail(None))
                         }
                         None
                       },
                       5
                     )
            run    <- stream.run(ZSink.take(1) *> ZSink.never).fork
            _      <- refCnt.get.repeatWhile(_ != 7)
            isDone <- refDone.get
            _      <- run.interrupt
          } yield assert(isDone)(isFalse)
        }
      ),
      suite("effectAsyncM")(
        testM("effectAsyncM")(checkM(Gen.chunkOf(Gen.anyInt).filter(_.nonEmpty)) { chunk =>
          for {
            latch <- Promise.make[Nothing, Unit]
            fiber <- ZStream
                       .effectAsyncM[Any, Throwable, Int] { k =>
                         global.execute(() => chunk.foreach(a => k(Task.succeed(Chunk.single(a)))))
                         latch.succeed(()) *>
                           Task.unit
                       }
                       .take(chunk.size.toLong)
                       .run(ZSink.collectAll[Throwable, Int])
                       .fork
            _ <- latch.await
            s <- fiber.join
          } yield assert(s)(equalTo(chunk))
        }),
        testM("effectAsyncM signal end stream") {
          for {
            result <- ZStream
                        .effectAsyncM[Any, Nothing, Int] { k =>
                          global.execute(() => k(IO.fail(None)))
                          UIO.unit
                        }
                        .runCollect
          } yield assert(result)(equalTo(Chunk.empty))
        },
        testM("effectAsyncM back pressure") {
          for {
            refCnt  <- Ref.make(0)
            refDone <- Ref.make[Boolean](false)
            stream = ZStream.effectAsyncM[Any, Throwable, Int](
                       cb => {
                         global.execute { () =>
                           println("Rawr")
                           // 1st consumed by sink, 2-6 – in queue, 7th – back pressured
                           (1 to 7).foreach(i => cb(refCnt.set(i) *> ZIO.succeedNow(Chunk.single(1))))
                           cb(refDone.set(true) *> ZIO.fail(None))
                         }
                         UIO.unit
                       },
                       5
                     )
            run    <- stream.run(ZSink.take(1) *> ZSink.never).fork
            _      <- refCnt.get.repeatWhile(_ != 7)
            isDone <- refDone.get
            _      <- run.interrupt
          } yield assert(isDone)(isFalse)
        }
      ),
      suite("effectAsyncInterrupt")(
        testM("effectAsyncInterrupt Left") {
          for {
            cancelled <- Ref.make(false)
            latch     <- Promise.make[Nothing, Unit]
            fiber <- ZStream
                       .effectAsyncInterrupt[Any, Nothing, Unit] { offer =>
                         global.execute(() => offer(ZIO.succeedNow(Chunk.unit)))
                         Left(cancelled.set(true))
                       }
                       .tap(_ => latch.succeed(()))
                       .runDrain
                       .fork
            _      <- latch.await
            _      <- fiber.interrupt
            result <- cancelled.get
          } yield assert(result)(isTrue)
        },
        testM("effectAsyncInterrupt Right")(checkM(Gen.chunkOf(Gen.anyInt)) { chunk =>
          val s = ZStream.effectAsyncInterrupt[Any, Throwable, Int](_ => Right(ZStream.fromIterable(chunk)))

          assertM(s.take(chunk.size.toLong).runCollect)(equalTo(chunk))
        }),
        testM("effectAsyncInterrupt signal end stream ") {
          for {
            result <- ZStream
                        .effectAsyncInterrupt[Any, Nothing, Int] { k =>
                          global.execute(() => k(IO.fail(None)))
                          Left(UIO.succeedNow(()))
                        }
                        .runCollect
          } yield assert(result)(equalTo(Chunk.empty))
        },
        testM("effectAsyncInterrupt back pressure") {
          for {
            selfId  <- ZIO.fiberId
            refCnt  <- Ref.make(0)
            refDone <- Ref.make[Boolean](false)
            stream = ZStream.effectAsyncInterrupt[Any, Throwable, Int](
                       cb => {
                         global.execute { () =>
                           // 1st consumed by sink, 2-6 – in queue, 7th – back pressured
                           (1 to 7).foreach(i => cb(refCnt.set(i) *> ZIO.succeedNow(Chunk.single(1))))
                           cb(refDone.set(true) *> ZIO.fail(None))
                         }
                         Left(UIO.unit)
                       },
                       5
                     )
            run    <- stream.run(ZSink.take(1) *> ZSink.never).fork
            _      <- refCnt.get.repeatWhile(_ != 7)
            isDone <- refDone.get
            exit   <- run.interrupt
          } yield assert(isDone)(isFalse) &&
            assert(exit.untraced)(failsCause(containsCause(Cause.interrupt(selfId))))
        }
      )
    ),
    testM("fromBlockingIterator") {
      checkM(Gen.small(Gen.chunkOfN(_)(Gen.anyInt)), Gen.small(Gen.const(_), 1)) { (chunk, maxChunkSize) =>
        assertM(ZStream.fromBlockingIterator(chunk.iterator, maxChunkSize).runCollect)(equalTo(chunk))
      }
    }
  )
}

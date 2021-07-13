package zio.stream

import zio._
import zio.blocking.{Blocking, effectBlockingIO}
import zio.duration._
import zio.test.Assertion._
import zio.test.TestAspect._
import zio.test._

import java.io.{FileNotFoundException, FileReader, IOException, OutputStream, Reader}
import java.net.InetSocketAddress
import java.nio.channels.AsynchronousSocketChannel
import java.nio.file.{Files, NoSuchFileException, Paths}
import java.nio.{Buffer, ByteBuffer}
import java.util.concurrent.CountDownLatch
import scala.concurrent.ExecutionContext.global

object ZStreamPlatformSpecificSpec extends ZIOBaseSpec {

  def socketClient(port: Int): ZManaged[Blocking, Throwable, AsynchronousSocketChannel] =
    ZManaged.make(effectBlockingIO(AsynchronousSocketChannel.open()).flatMap { client =>
      ZIO
        .fromFutureJava(client.connect(new InetSocketAddress("localhost", port)))
        .map(_ => client)
    })(c => ZIO.effectTotal(c.close()))

  def spec: ZSpec[Environment, Failure] = suite("ZStream JVM")(
    suite("Constructors")(
      testM("effectAsync")(checkM(Gen.chunkOf(Gen.anyInt)) { chunk =>
        val s = ZStream.effectAsync[Any, Throwable, Int] { k =>
          global.execute(() => chunk.foreach(a => k(Task.succeed(Chunk.single(a)))))
        }

        assertM(s.take(chunk.size.toLong).runCollect)(equalTo(chunk))
      }),
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
            run    <- stream.run(ZSink.fromEffect[Any, Nothing, Int, Nothing](ZIO.never)).fork
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
                       .run(ZSink.collectAll[Int])
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
                           // 1st consumed by sink, 2-6 – in queue, 7th – back pressured
                           (1 to 7).foreach(i => cb(refCnt.set(i) *> ZIO.succeedNow(Chunk.single(1))))
                           cb(refDone.set(true) *> ZIO.fail(None))
                         }
                         UIO.unit
                       },
                       5
                     )
            run    <- stream.run(ZSink.fromEffect[Any, Nothing, Int, Nothing](ZIO.never)).fork
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
            run    <- stream.run(ZSink.fromEffect[Any, Throwable, Int, Nothing](ZIO.never)).fork
            _      <- refCnt.get.repeatWhile(_ != 7)
            isDone <- refDone.get
            exit   <- run.interrupt
          } yield assert(isDone)(isFalse) &&
            assert(exit.untraced)(failsCause(containsCause(Cause.interrupt(selfId))))
        }
      ),
      testM("fromBlockingIterator") {
        checkM(Gen.small(Gen.chunkOfN(_)(Gen.anyInt)), Gen.small(Gen.const(_), 1)) { (chunk, maxChunkSize) =>
          assertM(ZStream.fromBlockingIterator(chunk.iterator, maxChunkSize).runCollect)(equalTo(chunk))
        }
      },
      suite("fromFile")(
        testM("reads from an existing file") {
          val data = (0 to 100).mkString

          Task(Files.createTempFile("stream", "fromFile")).bracket(path => Task(Files.delete(path)).orDie) { path =>
            Task(Files.write(path, data.getBytes("UTF-8"))) *>
              assertM(ZStream.fromFile(path, 24).transduce(ZTransducer.utf8Decode).runCollect.map(_.mkString))(
                equalTo(data)
              )
          }
        },
        testM("fails on a nonexistent file") {
          assertM(ZStream.fromFile(Paths.get("nonexistent"), 24).runDrain.run)(
            fails(isSubtype[NoSuchFileException](anything))
          )
        }
      ),
      suite("fromReader")(
        testM("reads non-empty file") {
          Task(Files.createTempFile("stream", "reader")).bracket(path => UIO(Files.delete(path))) { path =>
            for {
              data <- UIO((0 to 100).mkString)
              _    <- Task(Files.write(path, data.getBytes("UTF-8")))
              read <- ZStream.fromReader(new FileReader(path.toString)).runCollect.map(_.mkString)
            } yield assert(read)(equalTo(data))
          }
        },
        testM("reads empty file") {
          Task(Files.createTempFile("stream", "reader-empty")).bracket(path => UIO(Files.delete(path))) { path =>
            ZStream
              .fromReader(new FileReader(path.toString))
              .runCollect
              .map(_.mkString)
              .map(assert(_)(isEmptyString))
          }
        },
        testM("fails on a failing reader") {
          final class FailingReader extends Reader {
            def read(x: Array[Char], a: Int, b: Int): Int = throw new IOException("failed")

            def close(): Unit = ()
          }

          ZStream
            .fromReader(new FailingReader)
            .runDrain
            .run
            .map(assert(_)(fails(isSubtype[IOException](anything))))
        }
      ),
      suite("fromResource")(
        testM("returns the content of the resource") {
          ZStream
            .fromResource("zio/stream/bom/quickbrown-UTF-8-with-BOM.txt")
            .transduce(ZTransducer.utf8Decode)
            .runCollect
            .map(b => assert(b.mkString)(startsWithString("Sent")))
        },
        testM("fails with FileNotFoundException if the stream does not exist") {
          assertM(ZStream.fromResource("does_not_exist").runDrain.run)(
            fails(isSubtype[FileNotFoundException](hasMessage(containsString("does_not_exist"))))
          )
        }
      ),
      suite("fromSocketServer")(
        testM("read data")(checkM(Gen.anyString.filter(_.nonEmpty)) { message =>
          for {
            refOut <- Ref.make("")

            server <- ZStream
                        .fromSocketServer(8886)
                        .foreach { c =>
                          c.read
                            .transduce(ZTransducer.utf8Decode)
                            .runCollect
                            .map(_.mkString)
                            .flatMap(s => refOut.update(_ + s))
                        }
                        .fork

            _ <- socketClient(8886)
                   .use(c => ZIO.fromFutureJava(c.write(ByteBuffer.wrap(message.getBytes))))
                   .retry(Schedule.forever)

            receive <- refOut.get.repeatWhileM(s => ZIO.succeed(s.isEmpty))

            _ <- server.interrupt
          } yield assert(receive)(equalTo(message))
        }),
        testM("write data")(checkM(Gen.anyString.filter(_.nonEmpty)) { message =>
          (for {
            refOut <- Ref.make("")

            server <- ZStream
                        .fromSocketServer(8887)
                        .foreach(c => ZStream.fromIterable(message.getBytes).run(c.write))
                        .fork

            _ <- socketClient(8887).use { c =>
                   val buffer = ByteBuffer.allocate(message.getBytes.length)

                   ZIO
                     .fromFutureJava(c.read(buffer))
                     .repeatUntil(_ < 1)
                     .flatMap { _ =>
                       (buffer: Buffer).flip()
                       refOut.update(_ => new String(buffer.array))
                     }
                 }.retry(Schedule.forever)

            receive <- refOut.get.repeatWhileM(s => ZIO.succeed(s.isEmpty))

            _ <- server.interrupt
          } yield assert(receive)(equalTo(message)))
        }),
        testM("write large") {
          val message = "XABCDEFGHIJKMNOP" * 180000
          for {
            refOut     <- Ref.make("")
            refWritten <- Ref.make(0)

            server <- ZStream
                        .fromSocketServer(8885)
                        .foreach(c =>
                          ZStream.fromChunk(Chunk.fromIterable(message.getBytes)).run(c.write).flatMap(refWritten.set _)
                        )
                        .fork

            _ <- socketClient(8885).use { c =>
                   val buffer = ByteBuffer.allocate(message.getBytes.length)

                   ZIO
                     .fromFutureJava(c.read(buffer))
                     .repeatUntil(_ < 1)
                     .flatMap { _ =>
                       (buffer: Buffer).flip()
                       refOut.update(_ => new String(buffer.array))
                     }
                 }.retry(Schedule.forever)

            receive <- refOut.get.repeatWhileM(s => ZIO.succeed(s.isEmpty))

            _ <- server.interrupt

            serverWritten <- refWritten.get

          } yield assert(receive)(equalTo(message)) && assert(serverWritten)(equalTo(message.length))
        }
      ),
      suite("fromOutputStreamWriter")(
        testM("reads what is written") {
          checkM(Gen.listOf(Gen.chunkOf(Gen.anyByte)), Gen.int(1, 10)) { (bytess, chunkSize) =>
            val write    = (out: OutputStream) => for (bytes <- bytess) out.write(bytes.toArray)
            val expected = bytess.foldLeft[Chunk[Byte]](Chunk.empty)(_ ++ _)
            ZStream.fromOutputStreamWriter(write, chunkSize).runCollect.map(assert(_)(equalTo(expected)))
          }
        },
        testM("captures errors") {
          val write = (_: OutputStream) => throw new Exception("boom")
          ZStream.fromOutputStreamWriter(write).runDrain.run.map(assert(_)(fails(hasMessage(equalTo("boom")))))
        },
        testM("is not affected by closing the output stream") {
          val data  = Array.tabulate[Byte](ZStream.DefaultChunkSize * 5 / 2)(_.toByte)
          val write = (out: OutputStream) => { out.write(data); out.close() }
          ZStream.fromOutputStreamWriter(write).runCollect.map(assert(_)(equalTo(Chunk.fromArray(data))))
        } @@ timeout(10.seconds) @@ flaky,
        testM("is interruptable") {
          val latch = new CountDownLatch(1)
          val write = (out: OutputStream) => { latch.await(); out.write(42); }
          ZStream.fromOutputStreamWriter(write).runDrain.fork.flatMap(_.interrupt).map(assert(_)(isInterrupted))
        }
      )
    )
  )
}

package zio.stream.io

import zio.blocking.Blocking
import zio.stream.ZStream._
import zio.stream.{Stream, Transducer, ZSink, ZStream, ZTransducer}
import zio.test.Assertion.equalTo
import zio.test.{Gen, ZSpec, assert}
import zio.{Chunk, Hub, Promise, Queue, Ref, Schedule, ZHub, ZIO, ZIOBaseSpec}

import java.net.{InetSocketAddress, SocketAddress}
import scala.util.Try

/**
 * Contains interesting examples of how the API can be applied
 */
object SocketSpec extends ZIOBaseSpec {

  def spec: ZSpec[Environment, Failure] = suite("ZStream JVM")(
    suite("socket")(
      testM("Server responds with length of request") {
        val message = "message"
        for {
          _ <- runServer(
                 6867,
                 Socket.handlerServerM(_ =>
                   _.runCollect.map(_.size).map(length => ZStream.fromIterable(s"length: $length".getBytes()))
                 )
               )

          receive <- requestChunk(6867, message)

        } yield assert(receive)(equalTo(s"length: ${message.length}"))
      },
      testM("Server echoes the request") {
        val message = "XABCDEFGHIJKMNOP"
        for {
          //echo
          _ <- runServer(8887, Socket.handlerServer(_ => Predef.identity))

          receive <- requestChunk(8887, message)
        } yield assert(receive)(equalTo(message))
      },
      testM("Server ignores the request, using fixed response") {

        for {

          server <-
            runServer(
              6877,
              Socket.handlerServerM(_ => _.runDrain >>> ZIO.succeed(ZStream.fromIterable(("Fixed").getBytes())))
            )

          receive <- requestChunk(6877, "message")

          _ <- server.interrupt
        } yield assert(receive)(equalTo("Fixed"))
      },
      testM("server responds with every request byte incremented by one") {
        val message = "012345678"
        for {
          //increment byte
          server <- runServer(8888, Socket.handlerServer(_ => _.map(b => (b + 1).toByte)))

          conn   <- Socket.fromSocketClient(8888, "localhost").retry(Schedule.forever)
          stream <- Socket.requestStream(Chunk.fromArray(message.getBytes()))(conn)
          receive <- stream
                       .transduce(ZTransducer.utf8Decode)
                       .runCollect
                       .map(_.mkString)

          _ <- server.interrupt
        } yield assert(receive)(equalTo("123456789"))
      },
      testM("Independent processing of request and response ") {
        val message = "012345678"
        val command = "request"
        for {
          queue <- Queue.unbounded[Byte]

          server <- runServer(
                      8889,
                      Socket.bidi(
                        _.run(ZSink.fromQueue(queue)),
                        down => ZStream.fromIterable(message.getBytes).run(down).unit
                      )
                    )

          response <- requestChunk(8889, command)

          contents <- queue.takeAll
          request   = new String(Chunk.fromIterable(contents).toArray)

          _ <- server.interrupt
        } yield assert(response)(equalTo(message)) && assert(request)(equalTo(command))

      },
      testM("Independent processing of request and response with client address") {
        val message = "012345678"
        val command = "request"
        for {
          queue <- Queue.unbounded[Byte]

          server <- runServer(
                      8899,
                      Socket.bidiServer(_ =>
                        (
                          _.run(ZSink.fromQueue(queue)),
                          down => ZStream.fromIterable(message.getBytes).run(down).unit
                        )
                      )
                    )

          response <- requestChunk(8899, command)

          contents <- queue.takeAll
          request   = new String(Chunk.fromIterable(contents).toArray)

          _ <- server.interrupt
        } yield assert(response)(equalTo(message)) && assert(request)(equalTo(command))

      },
      testM("Server maintains running count, incremented by client requests") {
        def incrementer(state: Ref[Int]): SocketAddress => Stream[Throwable, Byte] => Stream[Throwable, Byte] = { _ =>
          _.transduce(ZTransducer.utf8Decode)
            .transduce(ZTransducer.splitLines)
            .map(s => Try(s.toInt).getOrElse(0))
            .mapM(bump => state.update(_ + bump))
            .mapM(_ => state.get)
            .mapConcatChunk(i => Chunk.fromIterable((i.toString).getBytes))
        }

        val incrementOne = 12
        val incrementTwo = 123
        for {
          count <- Ref.make(0)

          server <- runServer(8881, Socket.handlerServer(incrementer(count)))

          responseOne <- requestChunk(8881, incrementOne.toString)

          responseTwo <- requestChunk(8881, incrementTwo.toString)

          _ <- server.interrupt
        } yield assert(responseOne.toInt)(equalTo(incrementOne)) && assert(responseTwo.toInt)(
          equalTo(incrementOne + incrementTwo)
        )

      },
      /**
       * Mocks a chat service that copies all input to each client;
       * records the connect/disconnect of each client and the bytes received from each.
       *
       * Note this test uses Gen within the test to generate test messages
       */
      testM("Record client connectivity with all interactions delivered via Hub subscriber") {
        def chat(hub: Hub[String])(addr: SocketAddress)(in: Stream[Throwable, Byte]) = {
          def notify(tag: String) = ZStream((s"$tag ${addr.asInstanceOf[InetSocketAddress].getPort}\n"))
          Stream
            .fromHub(hub)
            .interruptWhen(
              (notify("start") ++ in.transduce(Transducer.usASCIIDecode) ++ notify("end")).run(ZSink.fromHub(hub))
            )
            .mapConcatChunk(s => Chunk.fromArray(s.getBytes()))
        }
        for {
          expectedStore <- Ref.make("")
          promise       <- Promise.make[Nothing, Unit]
          hub           <- ZHub.unbounded[String]

          server        <- runServer(6887, Socket.handlerServer(chat(hub)))
          managed        = ZStream.fromHubManaged(hub).tapM(_ => promise.succeed(()))
          recorderStream = ZStream.unwrapManaged(managed)
          recorder      <- recorderStream.runCollect.fork
          _             <- promise.await

          messages: Seq[String] <- Gen.alphaNumericString.filter(_.nonEmpty).runCollect
          _ <- ZIO.foreach(messages) { message =>
                 for {
                   connEcho    <- Socket.fromSocketClient(6887, "localhost").retry(Schedule.forever)
                   echoAddress <- connEcho.localAddress
                   portEcho     = echoAddress.asInstanceOf[InetSocketAddress].getPort
                   _           <- Socket.requestChunk(Chunk.fromArray(message.getBytes()))(connEcho)
                   _           <- expectedStore.update(_ + s"start $portEcho\n${message}end $portEcho\n")
                 } yield ()
               }

          _ <- hub.shutdown

          recorderExit <- recorder.join
          recorded      = recorderExit.mkString

          expected <- expectedStore.get
          _        <- server.interrupt
        } yield assert(recorded)(equalTo(expected))

      }
    )
  )

  /**
   * Note mapMParUnordered is appropriate since each client connection is independent and has different lifetimes
   */
  private final def runServer(port: Int, f: Connection => ZIO[Blocking, Throwable, Unit]) =
    ZStream
      .fromSocketServer(port)
      .mapMParUnordered(4)(f)
      .runDrain
      .fork

  private final def requestChunk(port: Int, request: String) = for {
    conn    <- Socket.fromSocketClient(port, "localhost").retry(Schedule.forever)
    receive <- Socket.requestChunk(Chunk.fromArray(request.getBytes()))(conn)
  } yield new String(receive.toArray)
}

package zio.stream

import zio.duration._
import zio.test._
import zio.{UIO, ZIOBaseSpec}

object StreamLazinessSpec extends ZIOBaseSpec {

  def assertLazy(f: (=> Nothing) => Any): UIO[TestResult] =
    UIO.effectTotal {
      val _ = f(throw new RuntimeException("not lazy"))
      assertCompletes
    }

  def spec: ZSpec[Environment, Failure] = suite("StreamLazinessSpec")(
    suite("ZSink")(
      testM("die")(assertLazy(ZSink.die)),
      testM("fail")(assertLazy(ZSink.fail)),
      testM("halt")(assertLazy(ZSink.halt)),
      testM("succeed")(assertLazy(ZSink.succeed))
    ),
    suite("ZStream")(
      testM("die")(assertLazy(ZStream.die)),
      testM("dieMessage")(assertLazy(ZStream.dieMessage)),
      testM("fail")(assertLazy(ZStream.fail)),
      testM("fromChunk")(assertLazy(ZStream.fromChunk)),
      testM("fromIterable")(assertLazy(ZStream.fromIterable)),
      testM("halt")(assertLazy(ZStream.halt)),
      testM("succeed")(assertLazy(ZStream.succeed)),
      testM("timeoutError")(
        assertLazy(
          ZStream.succeed(1).timeoutError(_)(Duration.Infinity)
        )
      )
    )
  )
}

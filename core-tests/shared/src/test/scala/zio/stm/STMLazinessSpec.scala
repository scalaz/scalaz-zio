package zio.stm

import zio.test._
import zio.{ UIO, ZIOBaseSpec }

object STMLazinessSpec extends ZIOBaseSpec {

  def assertLazy(f: (=> Nothing) => Any): UIO[TestResult] =
    UIO.effectTotal {
      val _ = f(throw new RuntimeException("not lazy"))
      assertCompletes
    }

  def spec: ZSpec[Environment, Failure] = suite("STMLazinessSpec")(
    suite("STM")(
      testM("check")(assertLazy(STM.check)),
      testM("die")(assertLazy(STM.die)),
      testM("dieMessage")(assertLazy(STM.dieMessage)),
      testM("fail")(assertLazy(STM.fail)),
      testM("fromEither")(assertLazy(STM.fromEither)),
      testM("fromTry")(assertLazy(STM.fromTry)),
      testM("succeed")(assertLazy(STM.succeed))
    ),
    suite("ZSTM")(
      testM("check")(assertLazy(ZSTM.check)),
      testM("die")(assertLazy(ZSTM.die)),
      testM("dieMessage")(assertLazy(ZSTM.dieMessage)),
      testM("fail")(assertLazy(ZSTM.fail)),
      testM("fromEither")(assertLazy(ZSTM.fromEither)),
      testM("fromTry")(assertLazy(ZSTM.fromTry)),
      testM("succeed")(assertLazy(ZSTM.succeed))
    )
  )
}

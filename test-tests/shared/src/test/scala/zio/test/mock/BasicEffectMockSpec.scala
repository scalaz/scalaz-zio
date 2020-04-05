package zio.test.mock

import zio.duration._
import zio.test.mock.internal.{ InvalidCall, MockException }
import zio.test.mock.module.{ PureModule, PureModuleMock }
import zio.test.{ suite, Assertion, ZIOBaseSpec }
import zio.{ IO, UIO }

object BasicEffectMockSpec extends ZIOBaseSpec with MockSpecUtils[PureModule] {

  import Assertion._
  import Expectation._
  import InvalidCall._
  import MockException._

  def spec = suite("BasicEffectMockSpec")(
    suite("effects")(
      suite("static")(
        testValue("returns value")(
          PureModuleMock.Static(value("foo")),
          PureModule.static,
          equalTo("foo")
        ),
        testError("returns failure")(
          PureModuleMock.Static(failure("foo")),
          PureModule.static,
          equalTo("foo")
        )
      ),
      suite("zeroParams")(
        testValue("returns value")(
          PureModuleMock.ZeroParams(value("foo")),
          PureModule.zeroParams,
          equalTo("foo")
        ),
        testError("returns failure")(
          PureModuleMock.ZeroParams(failure("foo")),
          PureModule.zeroParams,
          equalTo("foo")
        )
      ),
      suite("zeroParamsWithParens")(
        testValue("returns value")(
          PureModuleMock.ZeroParamsWithParens(value("foo")),
          PureModule.zeroParamsWithParens(),
          equalTo("foo")
        ),
        testError("returns failure")(
          PureModuleMock.ZeroParamsWithParens(failure("foo")),
          PureModule.zeroParamsWithParens(),
          equalTo("foo")
        )
      ),
      suite("singleParam")(
        testValue("returns value")(
          PureModuleMock.SingleParam(equalTo(1), value("foo")),
          PureModule.singleParam(1),
          equalTo("foo")
        ),
        testValue("returns valueF")(
          PureModuleMock.SingleParam(equalTo(1), valueF(i => s"foo $i")),
          PureModule.singleParam(1),
          equalTo("foo 1")
        ),
        testValue("returns valueM")(
          PureModuleMock.SingleParam(equalTo(1), valueM(i => UIO.succeed(s"foo $i"))),
          PureModule.singleParam(1),
          equalTo("foo 1")
        ),
        testError("returns failure")(
          PureModuleMock.SingleParam(equalTo(1), failure("foo")),
          PureModule.singleParam(1),
          equalTo("foo")
        ),
        testError("returns failureF")(
          PureModuleMock.SingleParam(equalTo(1), failureF(i => s"foo $i")),
          PureModule.singleParam(1),
          equalTo("foo 1")
        ),
        testError("returns failureM")(
          PureModuleMock.SingleParam(equalTo(1), failureM(i => IO.fail(s"foo $i"))),
          PureModule.singleParam(1),
          equalTo("foo 1")
        )
      ),
      suite("manyParams")(
        testValue("returns value")(
          PureModuleMock.ManyParams(equalTo((1, "2", 3L)), value("foo")),
          PureModule.manyParams(1, "2", 3L),
          equalTo("foo")
        ),
        testValue("returns valueF")(
          PureModuleMock.ManyParams(equalTo((1, "2", 3L)), valueF(i => s"foo $i")),
          PureModule.manyParams(1, "2", 3L),
          equalTo("foo (1,2,3)")
        ),
        testValue("returns valueM")(
          PureModuleMock.ManyParams(equalTo((1, "2", 3L)), valueM(i => UIO.succeed(s"foo $i"))),
          PureModule.manyParams(1, "2", 3L),
          equalTo("foo (1,2,3)")
        ),
        testError("returns failure")(
          PureModuleMock.ManyParams(equalTo((1, "2", 3L)), failure("foo")),
          PureModule.manyParams(1, "2", 3L),
          equalTo("foo")
        ),
        testError("returns failureF")(
          PureModuleMock.ManyParams(equalTo((1, "2", 3L)), failureF(i => s"foo $i")),
          PureModule.manyParams(1, "2", 3L),
          equalTo("foo (1,2,3)")
        ),
        testError("returns failureM")(
          PureModuleMock.ManyParams(equalTo((1, "2", 3L)), failureM(i => IO.fail(s"foo $i"))),
          PureModule.manyParams(1, "2", 3L),
          equalTo("foo (1,2,3)")
        )
      ),
      suite("manyParamLists")(
        testValue("returns value")(
          PureModuleMock.ManyParamLists(equalTo((1, "2", 3L)), value("foo")),
          PureModule.manyParamLists(1)("2")(3L),
          equalTo("foo")
        ),
        testValue("returns valueF")(
          PureModuleMock.ManyParamLists(equalTo((1, "2", 3L)), valueF(i => s"foo $i")),
          PureModule.manyParamLists(1)("2")(3L),
          equalTo("foo (1,2,3)")
        ),
        testValue("returns valueM")(
          PureModuleMock.ManyParamLists(equalTo((1, "2", 3L)), valueM(i => UIO.succeed(s"foo $i"))),
          PureModule.manyParamLists(1)("2")(3L),
          equalTo("foo (1,2,3)")
        ),
        testError("returns failure")(
          PureModuleMock.ManyParamLists(equalTo((1, "2", 3L)), failure("foo")),
          PureModule.manyParamLists(1)("2")(3L),
          equalTo("foo")
        ),
        testError("returns failureF")(
          PureModuleMock.ManyParamLists(equalTo((1, "2", 3L)), failureF(i => s"foo $i")),
          PureModule.manyParamLists(1)("2")(3L),
          equalTo("foo (1,2,3)")
        ),
        testError("returns failureM")(
          PureModuleMock.ManyParamLists(equalTo((1, "2", 3L)), failureM(i => IO.fail(s"foo $i"))),
          PureModule.manyParamLists(1)("2")(3L),
          equalTo("foo (1,2,3)")
        )
      ),
      suite("command")(
        testValue("returns unit")(
          PureModuleMock.Command(),
          PureModule.command,
          isUnit
        )
      ),
      suite("parameterizedCommand")(
        testValue("returns unit")(
          PureModuleMock.ParameterizedCommand(equalTo(1)),
          PureModule.parameterizedCommand(1),
          isUnit
        )
      ),
      suite("overloaded")(
        suite("_0")(
          testValue("returns value")(
            PureModuleMock.Overloaded._0(equalTo(1), value("foo")),
            PureModule.overloaded(1),
            equalTo("foo")
          ),
          testValue("returns valueF")(
            PureModuleMock.Overloaded._0(equalTo(1), valueF(i => s"foo $i")),
            PureModule.overloaded(1),
            equalTo("foo 1")
          ),
          testValue("returns valueM")(
            PureModuleMock.Overloaded._0(equalTo(1), valueM(i => UIO.succeed(s"foo $i"))),
            PureModule.overloaded(1),
            equalTo("foo 1")
          ),
          testError("returns failure")(
            PureModuleMock.Overloaded._0(equalTo(1), failure("foo")),
            PureModule.overloaded(1),
            equalTo("foo")
          ),
          testError("returns failureF")(
            PureModuleMock.Overloaded._0(equalTo(1), failureF(i => s"foo $i")),
            PureModule.overloaded(1),
            equalTo("foo 1")
          ),
          testError("returns failureM")(
            PureModuleMock.Overloaded._0(equalTo(1), failureM(i => IO.fail(s"foo $i"))),
            PureModule.overloaded(1),
            equalTo("foo 1")
          )
        ),
        suite("_1")(
          testValue("returns value")(
            PureModuleMock.Overloaded._1(equalTo(1L), value("foo")),
            PureModule.overloaded(1L),
            equalTo("foo")
          ),
          testValue("returns valueF")(
            PureModuleMock.Overloaded._1(equalTo(1L), valueF(i => s"foo $i")),
            PureModule.overloaded(1L),
            equalTo("foo 1")
          ),
          testValue("returns valueM")(
            PureModuleMock.Overloaded._1(equalTo(1L), valueM(i => UIO.succeed(s"foo $i"))),
            PureModule.overloaded(1L),
            equalTo("foo 1")
          ),
          testError("returns failure")(
            PureModuleMock.Overloaded._1(equalTo(1L), failure("foo")),
            PureModule.overloaded(1L),
            equalTo("foo")
          ),
          testError("returns failureF")(
            PureModuleMock.Overloaded._1(equalTo(1L), failureF(i => s"foo $i")),
            PureModule.overloaded(1L),
            equalTo("foo 1")
          ),
          testError("returns failureM")(
            PureModuleMock.Overloaded._1(equalTo(1L), failureM(i => IO.fail(s"foo $i"))),
            PureModule.overloaded(1L),
            equalTo("foo 1")
          )
        )
      ),
      suite("maxParams")(
        testValue("returns value")(
          PureModuleMock.MaxParams(equalTo(intTuple22), value("foo")),
          (PureModule.maxParams _).tupled(intTuple22),
          equalTo("foo")
        ),
        testValue("returns valueF")(
          PureModuleMock.MaxParams(equalTo(intTuple22), valueF(i => s"foo $i")),
          (PureModule.maxParams _).tupled(intTuple22),
          equalTo("foo (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22)")
        ),
        testValue("returns valueM")(
          PureModuleMock.MaxParams(equalTo(intTuple22), valueM(i => UIO.succeed(s"foo $i"))),
          (PureModule.maxParams _).tupled(intTuple22),
          equalTo("foo (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22)")
        ),
        testError("returns failure")(
          PureModuleMock.MaxParams(equalTo(intTuple22), failure("foo")),
          (PureModule.maxParams _).tupled(intTuple22),
          equalTo("foo")
        ),
        testError("returns failureF")(
          PureModuleMock.MaxParams(equalTo(intTuple22), failureF(i => s"foo $i")),
          (PureModule.maxParams _).tupled(intTuple22),
          equalTo("foo (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22)")
        ),
        testError("returns failureM")(
          PureModuleMock.MaxParams(equalTo(intTuple22), failureM(i => IO.fail(s"foo $i"))),
          (PureModule.maxParams _).tupled(intTuple22),
          equalTo("foo (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22)")
        )
      ),
      suite("looped")(
        testValueTimeboxed("returns never")(500.millis)(
          PureModuleMock.Looped(equalTo(1), never),
          PureModule.looped(1),
          isNone
        )
      )
    ),
    suite("assertions composition")(
      testValue("&&")(
        PureModuleMock.SingleParam(equalTo(3) && isWithin(1, 5), valueF(input => s"foo $input")),
        PureModule.singleParam(3),
        equalTo("foo 3")
      ),
      testValue("||")(
        PureModuleMock.SingleParam(equalTo(10) || isWithin(1, 5), valueF(input => s"foo $input")),
        PureModule.singleParam(3),
        equalTo("foo 3")
      )
    ),
    suite("expectations failure")(
      testDied("invalid arguments")(
        PureModuleMock.ParameterizedCommand(equalTo(1)),
        PureModule.parameterizedCommand(2),
        equalTo(InvalidCallException(List(InvalidArguments(PureModuleMock.ParameterizedCommand, 2, equalTo(1)))))
      ),
      testDied("invalid method")(
        PureModuleMock.ParameterizedCommand(equalTo(1)),
        PureModule.singleParam(1),
        equalTo(
          InvalidCallException(
            List(InvalidCapability(PureModuleMock.SingleParam, PureModuleMock.ParameterizedCommand, equalTo(1)))
          )
        )
      ), {
        type E0 = Chain[PureModule]
        type E1 = Call[PureModule, Int, Unit, Unit]
        type L  = List[Expectation[PureModule]]
        type X  = UnsatisfiedExpectationsException[PureModule]

        def cmd(n: Int) = PureModuleMock.ParameterizedCommand(equalTo(n))

        def hasCall(index: Int, satisfied: Boolean, saturated: Boolean, invocations: List[Int]) =
          hasAt(index)(
            isSubtype[E1](
              hasField[E1, Boolean]("satisfied", _.satisfied, equalTo(satisfied)) &&
                hasField[E1, Boolean]("saturated", _.saturated, equalTo(saturated)) &&
                hasField[E1, List[Int]]("invocations", _.invocations, equalTo(invocations))
            )
          )

        testDied("unsatisfied expectations")(
          cmd(1) ++ cmd(2) ++ cmd(3),
          PureModule.parameterizedCommand(1),
          isSubtype[X](
            hasField(
              "expectation",
              _.expectation,
              isSubtype[E0](
                hasField[E0, L](
                  "children",
                  _.children,
                  isSubtype[L](
                    hasCall(0, true, true, List(1)) &&
                      hasCall(1, false, false, List.empty) &&
                      hasCall(2, false, false, List.empty)
                  )
                ) &&
                  hasField[E0, Boolean]("satisfied", _.satisfied, equalTo(false)) &&
                  hasField[E0, Boolean]("saturated", _.saturated, equalTo(false)) &&
                  hasField[E0, List[Int]]("invocations", _.invocations, equalTo(List(1)))
              )
            )
          )
        )
      }, {
        type M = Capability[PureModule, (Int, String, Long), String, String]
        type X = UnexpectedCallExpection[PureModule, (Int, String, Long), String, String]

        testDied("unexpected call")(
          PureModuleMock.SingleParam(equalTo(1), value("foo")),
          PureModule.singleParam(1) *> PureModule.manyParams(2, "3", 4L),
          isSubtype[X](
            hasField[X, M]("capability", _.capability, equalTo(PureModuleMock.ManyParams)) &&
              hasField[X, Any]("args", _.args, equalTo((2, "3", 4L)))
          )
        )
      }
    )
  )
}

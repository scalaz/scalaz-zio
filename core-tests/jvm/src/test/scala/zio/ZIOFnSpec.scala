package zio

import zio.internal.stacktracer.ZTraceElement
import zio.internal.stacktracer.ZTraceElement.{ NoLocation, SourceLocation }
import zio.internal.stacktracer.impl.AkkaLineNumbersTracer
import zio.internal.tracing.ZIOFn
import zio.test.Assertion._
import zio.test._

object ZIOFnSpec extends ZIOBaseSpec {

  //wrapper for clickable source locations in test failure reports
  case class Locations[+A <: SourceLocation](underlying: List[A]) extends Iterable[A] {
    override def toString: String = underlying.map(zte => s"    ${zte.prettyPrint}").mkString("\n").trim + "\n"

    override def iterator: Iterator[A] = underlying.iterator
  }

  def findInternal(trace: List[ZTraceElement]): UIO[List[SourceLocation]] =
    ZIO.succeed(trace.collect { case sl: SourceLocation if sl.file != "ZIOFnSpec.scala" => sl })

  def checkTrace[E, A](zio: => IO[E, A]): ZIO[Any, Any, BoolAlgebra[FailureDetails]] =
    for {
      trace <- zio *> ZIO.trace
      traceTarget = zio match {
                      case fm: ZIO.FlatMap[_, _, _, _] => ZIOFn.unwrap(fm.k)
                      case _                           => zio

                      /* case _: ZIO.EffectAsync[_, _, _] => ???
        case _: ZIO.Fold[_, _, _, _, _] => ???
        case _: ZIO.InterruptStatus[_, _, _] => ???
        case _: ZIO.CheckInterrupt[_, _, _] => ???
        case _: ZIO.Descriptor[_, _, _] => ???
        case _: ZIO.Lock[_, _, _] => ???
        case _: ZIO.Read[_, _, _] => ???
        case _: ZIO.EffectSuspendTotalWith[_, _, _] => ???
        case _: ZIO.TracingStatus[_, _, _] => ???
        case _: ZIO.CheckTracing[_, _, _] => ???
        case _: ZIO.RaceWith[_, _, _, _, _, _, _] => ???
        case _: ZIO.Supervise[_, _, _] => ???
        case _: ZIO.GetForkScope[_, _, _] => ???
        case _: ZIO.OverrideForkScope[_, _, _] => ???*/
                    }
      callSite <- tracer.traceLocation(traceTarget) match {
                    case nl: NoLocation     => ZIO.fail(nl)
                    case sl: SourceLocation => ZIO.succeed(sl)
                  }
      internalExecution <- findInternal(trace.executionTrace)
      internalStack     <- findInternal(trace.stackTrace)
      executionTrace     = trace.executionTrace.collect { case sl: SourceLocation => sl }
    } yield {
      val internalExecutionLocations = Locations(internalExecution)
      val internalStackLocations     = Locations(internalStack)
      val executionTraceLocations    = Locations(executionTrace)
      assert(executionTraceLocations)(contains(callSite) ?? "") &&
      //assert(executionTraceLocations)(isEmpty ?? "remove me") &&
      assert(internalStackLocations)(isEmpty ?? "Internal stack must be hidden") &&
      assert(internalExecutionLocations)(isEmpty ?? "Internal execution must be hidden")
    }

  val x: IO[Int, Int] = IO.succeed(1)
  val y: IO[Int, Int] = IO.succeed(2)
  val tracer          = new AkkaLineNumbersTracer

  def spec: ZSpec[Environment, Failure] = suite("ZIO internals are hidden")(
    testM("&&&")(checkTrace(x &&& y)),
    testM("&>")(checkTrace(x &> y)),
    //testM("***")(checkTrace(???)),
    testM("*>")(checkTrace(x *> y)),
    //testM("|||")(checkTrace(???)),
    //testM("+++")(checkTrace(???)),
    testM("<&")(checkTrace(x <& y)),
    testM("<&>")(checkTrace(x <&> y)),
    testM("<*")(checkTrace(x <* y)),
    testM("<*>")(checkTrace(x <*> y)),
    testM("<|>")(checkTrace(x <|> y)),
    testM("<+>")(checkTrace(x <+> y)),
    testM("<<<")(checkTrace(x <<< y)),
    testM("<>")(checkTrace(x <> y)),
    testM(">>=")(checkTrace(x >>= (_ => y))),
    testM("flatMap")(checkTrace(x flatMap (_ => y))),
    testM(">>>")(checkTrace(x >>> y)),
    //testM("absolve")(checkTrace(???)),
    //testM("absorb")(checkTrace(???)),
    //testM("absorbWith")(checkTrace(???)),
    testM("andThen")(checkTrace(x andThen y)),
    testM("as")(checkTrace(x as y)),
    testM("asService")(checkTrace(x.asService)),
    testM("asSome")(checkTrace(x.asSome)),
    testM("asSomeError")(checkTrace(x.asSomeError)),
    //testM("awaitAllChildren")(checkTrace(???)),
    testM("bimap")(checkTrace(x bimap (_ => y, _ => y))),
    //testM("bracket")(checkTrace(???)),
    //testM("bracket")(checkTrace(???)),
    //testM("bracket_")(checkTrace(???)),
    //testM("bracket_")(checkTrace(???)),
    //testM("bracketExit")(checkTrace(???)),
    //testM("bracketExit")(checkTrace(???)),
    //testM("bracketOnError")(checkTrace(???)),
    //testM("cached")(checkTrace(???)),
    testM("catchAll")(checkTrace(x catchAll (_ => y))),
    testM("catchAllCause")(checkTrace(x catchAllCause (_ => y))),
    testM("catchAllDefect")(checkTrace(x catchAllDefect (_ => y))),
    testM("catchAllTrace")(checkTrace(x catchAllTrace (_ => y))),
    //testM("catchSome")(checkTrace(???)),
    //testM("catchSomeCause")(checkTrace(???)),
    //testM("catchSomeDefect")(checkTrace(???)),
    //testM("catchSomeTrace")(checkTrace(???)),
    //testM("cause")(checkTrace(???)),
    //testM("collect")(checkTrace(???)),
    //testM("collectM")(checkTrace(???)),
    //testM("compose")(checkTrace(???)),
    //testM("delay")(checkTrace(???)),
    //testM("disconnect")(checkTrace(???)),
    //testM("either")(checkTrace(???)),
    //testM("ensuring")(checkTrace(???)),
    //testM("ensuringChild")(checkTrace(???)),
    //testM("eventually")(checkTrace(???)),
    //testM("exitCode")(checkTrace(???)),
    //testM("filterOrDie")(checkTrace(???)),
    //testM("filterOrDieMessage")(checkTrace(???)),
    //testM("filterOrElse")(checkTrace(???)),
    //testM("filterOrElse_")(checkTrace(???)),
    //testM("filterOrFail")(checkTrace(???)),
    //testM("firstSuccessOf")(checkTrace(???)),
    //testM("flatMapError")(checkTrace(???)),
    //testM("flatten")(checkTrace(???)),
    //testM("flattenErrorOption")(checkTrace(???)),
    //testM("flip")(checkTrace(???)),
    //testM("flipWith")(checkTrace(???)),
    //testM("fold")(checkTrace(???)),
    //testM("foldCause")(checkTrace(???)),
    //testM("foldCauseM")(checkTrace(???)),
    //testM("foldM")(checkTrace(???)),
    //testM("foldTraceM")(checkTrace(???)),
    //testM("forever")(checkTrace(???)),
    //testM("fork")(checkTrace(???)),
    //testM("forkAs")(checkTrace(???)),
    //testM("forkDaemon")(checkTrace(???)),
    //testM("forkIn")(checkTrace(???)),
    //testM("forkInternal")(checkTrace(???)),
    //testM("forkManaged")(checkTrace(???)),
    //testM("forkOn")(checkTrace(???)),
    //testM("forkWithErrorHandler")(checkTrace(???)),
    //testM("get")(checkTrace(???)),
    //testM("head")(checkTrace(???)),
    //testM("ignore")(checkTrace(???)),
    //testM("in")(checkTrace(???)),
    //testM("interruptAllChildren")(checkTrace(???)),
    //testM("interruptible")(checkTrace(???)),
    //testM("interruptStatus")(checkTrace(???)),
    //testM("isFailure")(checkTrace(???)),
    //testM("isSuccess")(checkTrace(???)),
    //testM("join")(checkTrace(???)),
    //testM("left")(checkTrace(???)),
    //testM("leftOrFail")(checkTrace(???)),
    //testM("leftOrFailException")(checkTrace(???)),
    //testM("leftOrFailWith")(checkTrace(???)),
    //testM("lock")(checkTrace(???)),
    //testM("mapEffect")(checkTrace(???)),
    //testM("mapError")(checkTrace(???)),
    //testM("mapErrorCause")(checkTrace(???)),
    //testM("memoize")(checkTrace(???)),
    //testM("merge")(checkTrace(???)),
    //testM("negate")(checkTrace(???)),
    //testM("none")(checkTrace(???)),
    //testM("on")(checkTrace(???)),
    //testM("once")(checkTrace(???)),
    //testM("onError")(checkTrace(???)),
    //testM("onExit")(checkTrace(???)),
    //testM("onFirst")(checkTrace(???)),
    //testM("onInterrupt")(checkTrace(???)),
    //testM("onInterrupt")(checkTrace(???)),
    //testM("onLeft")(checkTrace(???)),
    //testM("onRight")(checkTrace(???)),
    //testM("onSecond")(checkTrace(???)),
    //testM("onTermination")(checkTrace(???)),
    //testM("option")(checkTrace(???)),
    //testM("optional")(checkTrace(???)),
    //testM("orDie")(checkTrace(???)),
    //testM("orDieWith")(checkTrace(???)),
    //testM("orElse")(checkTrace(???)),
    //testM("orElseEither")(checkTrace(???)),
    //testM("orElseFail")(checkTrace(???)),
    //testM("orElseOptional")(checkTrace(???)),
    //testM("orElseSucceed")(checkTrace(???)),
    //testM("overrideForkScope")(checkTrace(???)),
    //testM("parallelErrors")(checkTrace(???)),
    //testM("provide")(checkTrace(???)),
    //testM("provideCustomLayer")(checkTrace(???)),
    //testM("provideLayer")(checkTrace(???)),
    //testM("provideSome")(checkTrace(???)),
    //testM("provideSomeLayer")(checkTrace(???)),
    //testM("race")(checkTrace(???)),
    //testM("raceAll")(checkTrace(???)),
    //testM("raceEither")(checkTrace(???)),
    //testM("raceFirst")(checkTrace(???)),
    //testM("raceWith")(checkTrace(???)),
    //testM("refailWithTrace")(checkTrace(???)),
    //testM("refineOrDie")(checkTrace(???)),
    //testM("refineOrDieWith")(checkTrace(???)),
    //testM("reject")(checkTrace(???)),
    //testM("rejectM")(checkTrace(???)),
    //testM("repeat")(checkTrace(???)),
    //testM("repeatN")(checkTrace(???)),
    //testM("repeatOrElse")(checkTrace(???)),
    //testM("repeatOrElseEither")(checkTrace(???)),
    //testM("repeatUntil")(checkTrace(???)),
    //testM("repeatUntilEquals")(checkTrace(???)),
    //testM("repeatUntilM")(checkTrace(???)),
    //testM("repeatWhile")(checkTrace(???)),
    //testM("repeatWhileEquals")(checkTrace(???)),
    //testM("repeatWhileM")(checkTrace(???)),
    //testM("resetForkScope")(checkTrace(???)),
    //testM("resurrect")(checkTrace(???)),
    //testM("retry")(checkTrace(???)),
    //testM("retryN")(checkTrace(???)),
    //testM("retryOrElse")(checkTrace(???)),
    //testM("retryOrElseEither")(checkTrace(???)),
    //testM("retryUntil")(checkTrace(???)),
    //testM("retryUntilEquals")(checkTrace(???)),
    //testM("retryUntilM")(checkTrace(???)),
    //testM("retryWhile")(checkTrace(???)),
    //testM("retryWhileEquals")(checkTrace(???)),
    //testM("retryWhileM")(checkTrace(???)),
    //testM("right")(checkTrace(???)),
    //testM("rightOrFail")(checkTrace(???)),
    //testM("rightOrFailException")(checkTrace(???)),
    //testM("rightOrFailWith")(checkTrace(???)),
    //testM("run")(checkTrace(???)),
    //testM("sandbox")(checkTrace(???)),
    //testM("sandboxWith")(checkTrace(???)),
    //testM("schedule")(checkTrace(???)),
    //testM("scheduleFrom")(checkTrace(???)),
    //testM("some")(checkTrace(???)),
    //testM("someOrElse")(checkTrace(???)),
    //testM("someOrElseM")(checkTrace(???)),
    //testM("someOrFail")(checkTrace(???)),
    //testM("someOrFailException")(checkTrace(???)),
    //testM("summarized")(checkTrace(???)),
    //testM("supervised")(checkTrace(???)),
    //testM("tap")(checkTrace(???)),
    //testM("tapBoth")(checkTrace(???)),
    //testM("tapCause")(checkTrace(???)),
    //testM("tapError")(checkTrace(???)),
    //testM("tapErrorTrace")(checkTrace(???)),
    //testM("timed")(checkTrace(???)),
    //testM("timedWith")(checkTrace(???)),
    //testM("timeout")(checkTrace(???)),
    //testM("timeoutFail")(checkTrace(???)),
    //testM("timeoutTo")(checkTrace(???)),
    //testM("to")(checkTrace(???)),
    //testM("toFuture")(checkTrace(???)),
    //testM("toFutureWith")(checkTrace(???)),
    //testM("toLayer")(checkTrace(???)),
    //testM("toLayerMany")(checkTrace(???)),
    //testM("toManaged")(checkTrace(???)),
    //testM("toManaged_")(checkTrace(???)),
    //testM("traced")(checkTrace(???)),
    //testM("tracingStatus")(checkTrace(???)),
    //testM("uncause")(checkTrace(???)),
    //testM("uninterruptible")(checkTrace(???)),
    //testM("unit")(checkTrace(???)),
    //testM("unless")(checkTrace(???)),
    //testM("unlessM")(checkTrace(???)),
    //testM("unrefine")(checkTrace(???)),
    //testM("unrefineTo")(checkTrace(???)),
    //testM("unrefineWith")(checkTrace(???)),
    //testM("unsandbox")(checkTrace(???)),
    //testM("untraced")(checkTrace(???)),
    //testM("updateService")(checkTrace(???)),
    //testM("validate")(checkTrace(???)),
    //testM("validatePar")(checkTrace(???)),
    //testM("validateWith")(checkTrace(???)),
    //testM("validateWithPar")(checkTrace(???)),
    //testM("when")(checkTrace(???)),
    //testM("whenM")(checkTrace(???)),
    testM("zip")(checkTrace(x zip y)),
    testM("zipLeft")(checkTrace(x zipLeft y)),
    testM("zipPar")(checkTrace(x zipPar y)),
    testM("zipParLeft")(checkTrace(x zipParLeft y)),
    testM("zipParRight")(checkTrace(x zipParRight y)),
    testM("zipRight")(checkTrace(x zipRight y))
    //testM("zipWith")(checkTrace(???)),
    //testM("zipWithPar")(checkTrace(???))
  )
}

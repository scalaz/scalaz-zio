package zio.test.sbt

import sbt.testing.{EventHandler, Logger, Task, TaskDef}
import zio.clock.Clock
import zio.test.{AbstractRunnableSpec, FilteredSpec, SummaryBuilder, TestArgs, TestLogger}
import zio.{Layer, Runtime, UIO, ZIO, ZLayer}

abstract class BaseTestTask(
  val taskDef: TaskDef,
  val testClassLoader: ClassLoader,
  val sendSummary: SendSummary,
  val args: TestArgs
) extends Task {

  protected lazy val specInstance: AbstractRunnableSpec = {
    import org.portablescala.reflect._
    val fqn = taskDef.fullyQualifiedName().stripSuffix("$") + "$"
    Reflect
      .lookupLoadableModuleClass(fqn, testClassLoader)
      .getOrElse(throw new ClassNotFoundException("failed to load object: " + fqn))
      .loadModule()
      .asInstanceOf[AbstractRunnableSpec]
  }

  protected def run(eventHandler: EventHandler): ZIO[TestLogger with Clock, Throwable, Unit] =
    for {
      spec   <- if(args.fixSnapshots)
        specInstance.fixSnapshot(FilteredSpec(specInstance.spec, args), taskDef.fullyQualifiedName())
      else
        specInstance.runSpec(FilteredSpec(specInstance.spec, args))
      summary = SummaryBuilder.buildSummary(spec)
      _      <- sendSummary.provide(summary)
      events  = ZTestEvent.from(spec, taskDef.fullyQualifiedName(), taskDef.fingerprint())
      _      <- ZIO.foreach(events)(e => ZIO.effect(eventHandler.handle(e)))
    } yield ()

  protected def sbtTestLayer(loggers: Array[Logger]): Layer[Nothing, TestLogger with Clock] =
    ZLayer.succeed[TestLogger.Service](new TestLogger.Service {
      def logLine(line: String): UIO[Unit] =
        ZIO.effect(loggers.foreach(_.info(colored(line)))).ignore
    }) ++ Clock.live

  override def execute(eventHandler: EventHandler, loggers: Array[Logger]): Array[Task] =
    try {
      Runtime((), specInstance.platform).unsafeRun {
        run(eventHandler)
          .provideLayer(sbtTestLayer(loggers))
          .onError(e => UIO(println(e.prettyPrint)))
      }
      Array()
    } catch {
      case t: Throwable =>
        t.printStackTrace()
        throw t
    }

  override def tags(): Array[String] = Array.empty

}

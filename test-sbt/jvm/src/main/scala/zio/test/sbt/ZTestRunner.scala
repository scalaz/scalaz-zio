/*
 * Copyright 2019-2021 John A. De Goes and the ZIO Contributors
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package zio.test.sbt

import sbt.testing._
import zio.test.{AbstractRunnableSpec, Summary, TestArgs}
import zio.{Runtime, ZIO}

import java.util.concurrent.atomic.AtomicReference

final class ZTestRunner(val args: Array[String], val remoteArgs: Array[String], testClassLoader: ClassLoader)
    extends Runner {
  val summaries: AtomicReference[Vector[Summary]] = new AtomicReference(Vector.empty)

  var layerCache: CustomSpecLayerCache = _

  val sendSummary: SendSummary = SendSummary.fromSendM(summary =>
    ZIO.effectTotal {
      summaries.updateAndGet(_ :+ summary)
      ()
    }
  )

  override def done(): String = {
//    println(s"=-=-=-=-> ${System.identityHashCode(this).toHexString}: done()")
//    (new Exception).printStackTrace()

    // TODO: BZ: This guard is needed because an extra runner gets created (when
    //  running the JUnit tests: `testOnly *.MavenJunitSpec`) that doesn't run
    //  any tests.
    //  Find out why this happens: https://github.com/sbt/sbt/blob/v1.5.0/main/src/main/scala/sbt/Defaults.scala#L1539
    if (layerCache != null)
      Runtime.default.unsafeRun(layerCache.release)

    val allSummaries = summaries.get

    val total  = allSummaries.map(_.total).sum
    val ignore = allSummaries.map(_.ignore).sum

    if (allSummaries.isEmpty || total == ignore)
      s"${Console.YELLOW}No tests were executed${Console.RESET}"
    else
      allSummaries
        .map(_.summary)
        .filter(_.nonEmpty)
        .flatMap(summary => colored(summary) :: "\n" :: Nil)
        .mkString("", "", "Done")
  }

  override def tasks(defs: Array[TaskDef]): Array[Task] = {
//    println(s"=-=-=-=-> ${System.identityHashCode(this).toHexString}: tasks(${defs.mkString(",")})")

    val testArgs = TestArgs.parse(args)

    layerCache = Runtime.default.unsafeRun(CustomSpecLayerCache.make)

    val (specs, tasks) =
      defs.map { taskDef =>
        val spec: AbstractRunnableSpec = lookupSpec(taskDef, testClassLoader)

        spec -> new ZTestTask(
          taskDef,
          testClassLoader,
          sendSummary,
          testArgs,
          spec,
          layerCache
        )
      }.unzip

    val entrypointClass = testArgs.testTaskPolicy.getOrElse(classOf[ZTestTaskPolicyDefaultImpl].getName)
    val taskPolicy = getClass.getClassLoader
      .loadClass(entrypointClass)
      .getConstructor()
      .newInstance()
      .asInstanceOf[ZTestTaskPolicy]
    val mergedTasks = taskPolicy.merge(tasks)

    Runtime.default.unsafeRun(
      layerCache.cacheLayers(specs)
    )

    mergedTasks
  }
}

final class ZTestTask(
  taskDef: TaskDef,
  testClassLoader: ClassLoader,
  sendSummary: SendSummary,
  testArgs: TestArgs,
  specInstance: AbstractRunnableSpec,
  layerCache: CustomSpecLayerCache
) extends BaseTestTask(
      taskDef,
      testClassLoader,
      sendSummary,
      testArgs,
      specInstance,
      layerCache
    )

abstract class ZTestTaskPolicy {
  def merge(zioTasks: Array[ZTestTask]): Array[Task]
}

class ZTestTaskPolicyDefaultImpl extends ZTestTaskPolicy {
  override def merge(zioTasks: Array[ZTestTask]): Array[Task] = zioTasks.toArray
}

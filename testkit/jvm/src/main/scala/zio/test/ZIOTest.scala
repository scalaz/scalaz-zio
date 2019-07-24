/*
 * Copyright 2019 John A. De Goes and the ZIO Contributors
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

package zio.test

import zio._
import zio.internal.{ Platform, PlatformLive }

abstract class ZIOTest[R, E] { self =>

  /**
   * Construct your environment here.
   */
  val environment: Managed[Nothing, R]

  /**
   * Place all your tests here.
   */
  val tests: ZSpec[R, E, String]

  /**
   * The platform that will be used to run the tests if the tests are run
   * through the main function.
   */
  def makePlatform(): Platform = PlatformLive.makeDefault().withReportFailure(_ => ())

  final def main(args: Array[String]): Unit = {
    val platform  = self.makePlatform()
    val bootstrap = Runtime((), platform)

    val runner =
      if (args.exists(_.contains("-par"))) ZSpecRunner.parallel(tests.size)
      else ZSpecRunner.sequential

    // TODO: Choose platform-appropriate reporter:
    val reporter: ZSpecReporter = ???

    bootstrap.unsafeRun(environment.use { env =>
      runner(tests.provide(env)).flatMap { results =>
        val exit = if (results.exists(_._2.failure)) 1 else 0

        reporter.report(results) *> ZIO.effect(System.exit(exit)).ignore
      }
    })
  }
}

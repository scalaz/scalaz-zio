import sbt._
import sbt.Keys.{ name, organization }

import com.typesafe.tools.mima.plugin.MimaKeys._
import com.typesafe.tools.mima.core._
import com.typesafe.tools.mima.core.ProblemFilters._

object MimaSettings {
  lazy val bincompatVersionToCompare = "1.0.1"

  def mimaSettings(failOnProblem: Boolean) =
    Seq(
      mimaPreviousArtifacts := Set(organization.value %% name.value % bincompatVersionToCompare),
      mimaBinaryIssueFilters ++= Seq(
        exclude[Problem]("zio.internal.*"),
        exclude[Problem]("zio.Promise#internal#*"),
        exclude[DirectMissingMethodProblem]("zio.ZManaged.reserve"),
        exclude[DirectMissingMethodProblem]("zio.ZIO#Fork.this"),
        exclude[IncompatibleResultTypeProblem]("zio.stm.TSemaphore.assertNonNegative$extension")
      ),
      mimaFailOnProblem := failOnProblem
    )
}

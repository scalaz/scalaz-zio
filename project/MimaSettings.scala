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
        exclude[Problem]("zio.stm.ZSTM#internal#*"),
        exclude[DirectMissingMethodProblem]("zio.ZManaged.reserve"),
        exclude[DirectMissingMethodProblem]("zio.ZIO#Fork.this"),
        exclude[DirectMissingMethodProblem]("zio.stm.ZSTM.done"),
        exclude[DirectMissingMethodProblem]("zio.stm.STM.done")
      ),
      mimaFailOnProblem := failOnProblem
    )
}

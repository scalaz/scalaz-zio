resolvers += Resolver.bintrayRepo("ktosopl", "sbt-plugins/sbt-jcstress")

addSbtPlugin("pl.project13.scala"                % "sbt-jmh"                       % "0.3.7")
addSbtPlugin("pl.project13.scala"                % "sbt-jcstress"                  % "0.2.0")
addSbtPlugin("org.scala-js"                      % "sbt-scalajs"                   % "0.6.32")
addSbtPlugin("org.portable-scala"                % "sbt-scalajs-crossproject"      % "0.6.1")
addSbtPlugin("org.scalameta"                     % "sbt-scalafmt"                  % "2.3.1")
addSbtPlugin("com.eed3si9n"                      % "sbt-buildinfo"                 % "0.9.0")
addSbtPlugin("com.thoughtworks.sbt-api-mappings" % "sbt-api-mappings"              % "3.0.0")
addSbtPlugin("com.github.cb372"                  % "sbt-explicit-dependencies"     % "0.2.12")
addSbtPlugin("de.heikoseeberger"                 % "sbt-header"                    % "5.4.0")
addSbtPlugin("ch.epfl.lamp"                      % "sbt-dotty"                     % "0.4.0")
addSbtPlugin("org.scalameta"                     % "sbt-mdoc"                      % "1.3.6")
addSbtPlugin("ch.epfl.scala"                     % "sbt-bloop"                     % "1.3.5")
addSbtPlugin("org.scala-native"                  % "sbt-scala-native"              % "0.4.0-M2")
addSbtPlugin("org.portable-scala"                % "sbt-scala-native-crossproject" % "0.6.1")
addSbtPlugin("ch.epfl.scala"                     % "sbt-scalafix"                  % "0.9.11")
addSbtPlugin("com.geirsson"                      % "sbt-ci-release"                % "1.5.2")

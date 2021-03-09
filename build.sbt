import BuildHelper._
import MimaSettings.mimaSettings
import explicitdeps.ExplicitDepsPlugin.autoImport.moduleFilterRemoveValue
import sbt.Keys
// shadow sbt-scalajs' crossProject from Scala.js 0.6.x
import sbtcrossproject.CrossPlugin.autoImport.crossProject

Global / onChangedBuildSource := ReloadOnSourceChanges

inThisBuild(
  List(
    organization := "dev.zio",
    homepage := Some(url("https://zio.dev")),
    licenses := List(
      "Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")
    ),
    developers := List(
      Developer(
        "jdegoes",
        "John De Goes",
        "john@degoes.net",
        url("http://degoes.net")
      )
    ),
    pgpPassphrase := sys.env.get("PGP_PASSPHRASE").map(_.toArray),
    pgpPublicRing := file("/tmp/public.asc"),
    pgpSecretRing := file("/tmp/secret.asc")
  )
)

addCommandAlias("build", "; prepare; testJVM")
addCommandAlias("prepare", "; fix; fmt")
addCommandAlias(
  "fix",
  "all compile:scalafix test:scalafix; all scalafmtSbt scalafmtAll"
)
addCommandAlias(
  "fixCheck",
  "; compile:scalafix --check ; test:scalafix --check"
)
addCommandAlias("fmt", "all root/scalafmtSbt root/scalafmtAll")
addCommandAlias("fmtCheck", "all root/scalafmtSbtCheck root/scalafmtCheckAll")
addCommandAlias(
  "compileJVM",
  ";coreTestsJVM/test:compile;stacktracerJVM/test:compile;streamsTestsJVM/test:compile;testTestsJVM/test:compile;testMagnoliaTestsJVM/test:compile;testRefinedJVM/test:compile;testRunnerJVM/test:compile;examplesJVM/test:compile;macrosJVM/test:compile"
)
addCommandAlias(
  "testNative",
  ";coreNative/compile;stacktracerNative/compile;streamsNative/compile;testNative/compile;testRunnerNative/compile"
)
addCommandAlias(
  "testJVM",
  ";coreTestsJVM/test;stacktracerJVM/test;streamsTestsJVM/test;testTestsJVM/test;testMagnoliaTestsJVM/test;testRefinedJVM/test;testRunnerJVM/test:run;examplesJVM/test:compile;benchmarks/test:compile;macrosJVM/test;testJunitRunnerTestsJVM/test"
)
addCommandAlias(
  "testJVMNoBenchmarks",
  ";coreTestsJVM/test;stacktracerJVM/test;streamsTestsJVM/test;testTestsJVM/test;testMagnoliaTestsJVM/test;testRefinedJVM/test:compile;testRunnerJVM/test:run;examplesJVM/test:compile"
)
addCommandAlias(
  "testJVMDotty",
  ";coreTestsJVM/test;stacktracerJVM/test:compile;streamsTestsJVM/test;testTestsJVM/test;testMagnoliaTestsJVM/test;testRefinedJVM/test;testRunnerJVM/test:run;examplesJVM/test:compile"
)
addCommandAlias(
  "testJVM211",
  ";coreTestsJVM/test;stacktracerJVM/test;streamsTestsJVM/test;testTestsJVM/test;testRunnerJVM/test:run;examplesJVM/test:compile;macrosJVM/test"
)
addCommandAlias(
  "testJS",
  ";coreTestsJS/test;stacktracerJS/test;streamsTestsJS/test;testTestsJS/test;testMagnoliaTestsJS/test;testRefinedJS/test;examplesJS/test:compile;macrosJS/test"
)
addCommandAlias(
  "testJS211",
  ";coreTestsJS/test;stacktracerJS/test;streamsTestsJS/test;testTestsJS/test;examplesJS/test:compile;macrosJS/test"
)
addCommandAlias(
  "mimaChecks",
  "all coreJVM/mimaReportBinaryIssues streamsJVM/mimaReportBinaryIssues testJVM/mimaReportBinaryIssues"
)

lazy val root = project
  .in(file("."))
  .settings(
    name := "zio",
    skip in publish := true,
    console := (console in Compile in coreJVM).value,
    unusedCompileDependenciesFilter -= moduleFilter(
      "org.scala-js",
      "scalajs-library"
    ),
    welcomeMessage
  )
  .aggregate(
    coreJVM,
    coreJS,
    coreNative,
    coreTestsJVM,
    coreTestsJS,
    macrosJVM,
    macrosJS,
    docs,
    streamsJVM,
    streamsJS,
    streamsNative,
    streamsTestsJVM,
    streamsTestsJS,
    benchmarks,
    testJVM,
    testJS,
    testNative,
    testTestsJVM,
    testTestsJS,
    stacktracerJS,
    stacktracerJVM,
    stacktracerNative,
    testRunnerJS,
    testRunnerJVM,
    testRunnerNative,
    testJunitRunnerJVM,
    testJunitRunnerTestsJVM,
    testMagnoliaJVM,
    testMagnoliaJS,
    testRefinedJVM,
    testRefinedJS
  )
  .enablePlugins(ScalaJSPlugin)

lazy val core = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .in(file("core"))
  .dependsOn(stacktracer)
  .settings(stdSettings("zio"))
  .settings(crossProjectSettings)
  .settings(buildInfoSettings("zio"))
  .settings(scalaReflectSettings)
  .enablePlugins(BuildInfoPlugin)

lazy val coreJVM = core.jvm
  .settings(dottySettings)
  .settings(replSettings)
  .settings(mimaSettings(failOnProblem = true))

lazy val coreJS = core.js

lazy val coreNative = core.native
  .settings(nativeSettings)
  .settings(
    libraryDependencies ++= Seq(
      "com.github.lolgab" %%% "native-loop-core" % "0.2.0"
    )
  )

lazy val coreTests = crossProject(JSPlatform, JVMPlatform)
  .in(file("core-tests"))
  .dependsOn(core)
  .dependsOn(test)
  .settings(stdSettings("core-tests"))
  .settings(crossProjectSettings)
  .settings(testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework"))
  .dependsOn(testRunner)
  .settings(buildInfoSettings("zio"))
  .settings(skip in publish := true)
  .settings(
    Compile / classLoaderLayeringStrategy := ClassLoaderLayeringStrategy.Flat
  )
  .enablePlugins(BuildInfoPlugin)

lazy val coreTestsJVM = coreTests.jvm
  .settings(dottySettings)
  .configure(_.enablePlugins(JCStressPlugin))
  .settings(replSettings)

lazy val coreTestsJS = coreTests.js

lazy val macros = crossProject(JSPlatform, JVMPlatform)
  .in(file("macros"))
  .dependsOn(core)
  .settings(stdSettings("zio-macros"))
  .settings(crossProjectSettings)
  .settings(macroDefinitionSettings)
  .settings(macroExpansionSettings)
  .settings(testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework"))
  .dependsOn(testRunner)

lazy val macrosJVM = macros.jvm.settings(dottySettings)
lazy val macrosJS  = macros.js

lazy val streams = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .in(file("streams"))
  .dependsOn(core)
  .settings(stdSettings("zio-streams"))
  .settings(crossProjectSettings)
  .settings(buildInfoSettings("zio.stream"))
  .settings(streamReplSettings)
  .enablePlugins(BuildInfoPlugin)

lazy val streamsJVM = streams.jvm
  .settings(dottySettings)
  // No bincompat on streams yet
  .settings(mimaSettings(failOnProblem = false))

lazy val streamsJS = streams.js

lazy val streamsNative = streams.native
  .settings(nativeSettings)

lazy val streamsTests = crossProject(JSPlatform, JVMPlatform)
  .in(file("streams-tests"))
  .dependsOn(streams)
  .dependsOn(coreTests % "test->test;compile->compile")
  .settings(stdSettings("streams-tests"))
  .settings(crossProjectSettings)
  .settings(testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework"))
  .dependsOn(testRunner)
  .settings(buildInfoSettings("zio.stream"))
  .settings(skip in publish := true)
  .settings(
    Compile / classLoaderLayeringStrategy := ClassLoaderLayeringStrategy.AllLibraryJars
  )
  .enablePlugins(BuildInfoPlugin)

lazy val streamsTestsJVM = streamsTests.jvm
  .dependsOn(coreTestsJVM % "test->compile")
  .settings(dottySettings)

lazy val streamsTestsJS = streamsTests.js

lazy val test = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .in(file("test"))
  .dependsOn(core, streams)
  .settings(stdSettings("zio-test"))
  .settings(crossProjectSettings)
  .settings(macroDefinitionSettings)
  .settings(macroExpansionSettings)
  .settings(
    libraryDependencies ++= Seq(
      ("org.portable-scala" %%% "portable-scala-reflect" % "1.1.1")
        .withDottyCompat(scalaVersion.value)
    )
  )

lazy val testJVM = test.jvm
  .settings(dottySettings)
  // No bincompat on zio-test yet
  .settings(mimaSettings(failOnProblem = false))
lazy val testJS = test.js
  .settings(
    libraryDependencies ++= List(
      "io.github.cquiroz" %%% "scala-java-time"      % "2.2.0",
      "io.github.cquiroz" %%% "scala-java-time-tzdb" % "2.2.0"
    )
  )
lazy val testNative = test.native
  .settings(nativeSettings)
  .settings(libraryDependencies += "org.ekrich" %%% "sjavatime" % "1.1.2")

lazy val testTests = crossProject(JSPlatform, JVMPlatform)
  .in(file("test-tests"))
  .dependsOn(test)
  .settings(stdSettings("test-tests"))
  .settings(crossProjectSettings)
  .settings(testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework"))
  .dependsOn(testRunner)
  .settings(buildInfoSettings("zio.test"))
  .settings(skip in publish := true)
  .settings(macroExpansionSettings)
  .enablePlugins(BuildInfoPlugin)

lazy val testTestsJVM = testTests.jvm.settings(dottySettings)
lazy val testTestsJS  = testTests.js

lazy val testMagnolia = crossProject(JVMPlatform, JSPlatform)
  .in(file("test-magnolia"))
  .dependsOn(test)
  .settings(stdSettings("zio-test-magnolia"))
  .settings(crossProjectSettings)
  .settings(macroDefinitionSettings)
  .settings(
    crossScalaVersions --= Seq(Scala211),
    scalacOptions ++= {
      if (isDotty.value) {
        Seq.empty
      } else {
        Seq("-language:experimental.macros")
      }
    },
    libraryDependencies ++= {
      if (isDotty.value) {
        Seq.empty
      } else {
        Seq(
          ("com.propensive" %%% "magnolia" % "0.17.0")
            .exclude("org.scala-lang", "scala-compiler")
        )
      }
    }
  )

lazy val testMagnoliaJVM = testMagnolia.jvm
  .settings(dottySettings)
lazy val testMagnoliaJS = testMagnolia.js

lazy val testMagnoliaTests = crossProject(JVMPlatform, JSPlatform)
  .in(file("test-magnolia-tests"))
  .dependsOn(testMagnolia)
  .dependsOn(testTests % "test->test;compile->compile")
  .settings(stdSettings("test-magnolia-tests"))
  .settings(crossProjectSettings)
  .settings(testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework"))
  .dependsOn(testRunner)
  .settings(buildInfoSettings("zio.test"))
  .settings(skip in publish := true)
  .enablePlugins(BuildInfoPlugin)

lazy val testMagnoliaTestsJVM = testMagnoliaTests.jvm
  .settings(dottySettings)
lazy val testMagnoliaTestsJS = testMagnoliaTests.js

lazy val testRefined = crossProject(JVMPlatform, JSPlatform)
  .in(file("test-refined"))
  .dependsOn(testMagnolia)
  .settings(stdSettings("zio-test-refined"))
  .settings(crossProjectSettings)
  .settings(macroDefinitionSettings)
  .settings(
    crossScalaVersions --= Seq(Scala211),
    libraryDependencies ++=
      Seq(
        ("eu.timepit" %% "refined" % "0.9.21")
          .withDottyCompat(scalaVersion.value)
      )
  )

lazy val testRefinedJVM = testRefined.jvm
  .settings(dottySettings)
lazy val testRefinedJS = testRefined.js

lazy val stacktracer = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .in(file("stacktracer"))
  .settings(stdSettings("zio-stacktracer"))
  .settings(crossProjectSettings)
  .settings(buildInfoSettings("zio.internal.stacktracer"))
  .enablePlugins(BuildInfoPlugin)

lazy val stacktracerJS = stacktracer.js
lazy val stacktracerJVM = stacktracer.jvm
  .settings(dottySettings)
  .settings(replSettings)

lazy val stacktracerNative = stacktracer.native
  .settings(nativeSettings)
  .settings(scalacOptions -= "-Xfatal-warnings") // Issue 3112

lazy val testRunner = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .in(file("test-sbt"))
  .settings(stdSettings("zio-test-sbt"))
  .settings(crossProjectSettings)
  .settings(mainClass in (Test, run) := Some("zio.test.sbt.TestMain"))
  .dependsOn(core)
  .dependsOn(test)

lazy val testRunnerJVM = testRunner.jvm
  .settings(dottySettings)
  .settings(libraryDependencies ++= Seq("org.scala-sbt" % "test-interface" % "1.0"))
lazy val testRunnerJS = testRunner.js
  .settings(libraryDependencies ++= Seq("org.scala-js" %% "scalajs-test-interface" % scalaJSVersion))
lazy val testRunnerNative = testRunner.native
  .settings(nativeSettings)
  .settings(libraryDependencies ++= Seq("org.scala-native" %%% "test-interface" % nativeVersion))

lazy val testJunitRunner = crossProject(JVMPlatform)
  .in(file("test-junit"))
  .settings(stdSettings("zio-test-junit"))
  .settings(crossProjectSettings)
  .settings(libraryDependencies ++= Seq("junit" % "junit" % "4.13.2"))
  .dependsOn(test)

lazy val testJunitRunnerJVM = testJunitRunner.jvm.settings(dottySettings)

lazy val testJunitRunnerTests = crossProject(JVMPlatform)
  .in(file("test-junit-tests"))
  .settings(stdSettings("test-junit-tests"))
  .settings(crossProjectSettings)
  .settings(fork in Test := true)
  .settings(javaOptions in Test ++= {
    Seq(
      s"-Dproject.dir=${baseDirectory.value}",
      s"-Dproject.version=${version.value}"
    )
  })
  .settings(skip in publish := true)
  .settings(testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework"))
  .settings(
    libraryDependencies ++= Seq(
      "junit"                   % "junit"     % "4.13.2" % Test,
      "org.scala-lang.modules" %% "scala-xml" % "1.3.0"  % Test,
      // required to run embedded maven in the tests
      "org.apache.maven"       % "maven-embedder"         % "3.6.3"  % Test,
      "org.apache.maven"       % "maven-compat"           % "3.6.3"  % Test,
      "org.apache.maven.wagon" % "wagon-http"             % "3.4.3"  % Test,
      "org.eclipse.aether"     % "aether-connector-basic" % "1.1.0"  % Test,
      "org.eclipse.aether"     % "aether-transport-wagon" % "1.1.0"  % Test,
      "org.slf4j"              % "slf4j-simple"           % "1.7.30" % Test
    )
  )
  .dependsOn(test)
  .dependsOn(testRunner)

lazy val testJunitRunnerTestsJVM = testJunitRunnerTests.jvm
  .settings(dottySettings)
  // publish locally so embedded maven runs against locally compiled zio
  .settings(
    Keys.test in Test :=
      (Keys.test in Test)
        .dependsOn(Keys.publishM2 in testJunitRunnerJVM)
        .dependsOn(Keys.publishM2 in testJVM)
        .dependsOn(Keys.publishM2 in coreJVM)
        .dependsOn(Keys.publishM2 in streamsJVM)
        .dependsOn(Keys.publishM2 in stacktracerJVM)
        .value
  )

/**
 * Examples sub-project that is not included in the root project.
 * To run tests :
 * `sbt "examplesJVM/test"`
 */
lazy val examples = crossProject(JVMPlatform, JSPlatform)
  .in(file("examples"))
  .settings(stdSettings("examples"))
  .settings(crossProjectSettings)
  .settings(macroExpansionSettings)
  .settings(scalacOptions += "-Xfatal-warnings")
  .settings(testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework"))
  .dependsOn(macros, testRunner)

lazy val examplesJS = examples.js
lazy val examplesJVM = examples.jvm
  .settings(dottySettings)
  .dependsOn(testJunitRunnerJVM)

lazy val benchmarks = project.module
  .dependsOn(coreJVM, streamsJVM, testJVM)
  .enablePlugins(JmhPlugin)
  .settings(replSettings)
  .settings(
    // skip 2.11 benchmarks because akka stop supporting scala 2.11 in 2.6.x
    crossScalaVersions -= Scala211,
    //
    skip in publish := true,
    libraryDependencies ++=
      Seq(
        "co.fs2"                    %% "fs2-core"       % "2.5.0",
        "com.google.code.findbugs"   % "jsr305"         % "3.0.2",
        "com.twitter"               %% "util-core"      % "21.2.0",
        "com.typesafe.akka"         %% "akka-stream"    % "2.6.13",
        "io.monix"                  %% "monix"          % "3.3.0",
        "io.projectreactor"          % "reactor-core"   % "3.4.3",
        "io.reactivex.rxjava2"       % "rxjava"         % "2.2.21",
        "org.jctools"                % "jctools-core"   % "3.3.0",
        "org.ow2.asm"                % "asm"            % "9.1",
        "org.scala-lang"             % "scala-compiler" % scalaVersion.value % Provided,
        "org.scala-lang"             % "scala-reflect"  % scalaVersion.value,
        "org.typelevel"             %% "cats-effect"    % "2.3.3",
        "org.scalacheck"            %% "scalacheck"     % "1.15.3",
        "qa.hedgehog"               %% "hedgehog-core"  % "0.6.5",
        "com.github.japgolly.nyaya" %% "nyaya-gen"      % "0.9.2"
      ),
    unusedCompileDependenciesFilter -= libraryDependencies.value
      .map(moduleid =>
        moduleFilter(
          organization = moduleid.organization,
          name = moduleid.name
        )
      )
      .reduce(_ | _),
    scalacOptions in Compile in console := Seq(
      "-Ypartial-unification",
      "-language:higherKinds",
      "-language:existentials",
      "-Yno-adapted-args",
      "-Xsource:2.13",
      "-Yrepl-class-based"
    ),
    resolvers += Resolver.url(
      "bintray-scala-hedgehog",
      url("https://dl.bintray.com/hedgehogqa/scala-hedgehog")
    )(Resolver.ivyStylePatterns)
  )

lazy val jsdocs = project
  .settings(libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "1.0.0")
  .enablePlugins(ScalaJSPlugin)
lazy val docs = project.module
  .in(file("zio-docs"))
  .settings(
    skip.in(publish) := true,
    moduleName := "zio-docs",
    unusedCompileDependenciesFilter -= moduleFilter("org.scalameta", "mdoc"),
    scalacOptions -= "-Yno-imports",
    scalacOptions -= "-Xfatal-warnings",
    scalacOptions ~= { _ filterNot (_ startsWith "-Ywarn") },
    scalacOptions ~= { _ filterNot (_ startsWith "-Xlint") },
    libraryDependencies ++= Seq(
      "commons-io"          % "commons-io"                  % "2.7"    % "provided",
      "org.jsoup"           % "jsoup"                       % "1.13.1" % "provided",
      "org.reactivestreams" % "reactive-streams-examples"   % "1.0.3"  % "provided",
      "dev.zio"            %% "zio-interop-cats"            % "2.3.1.0",
      "dev.zio"            %% "zio-interop-monix"           % "3.0.0.0-RC7",
      "dev.zio"            %% "zio-interop-scalaz7x"        % "7.2.27.0-RC9",
      "dev.zio"            %% "zio-interop-reactivestreams" % "1.3.0.7-2",
      "dev.zio"            %% "zio-interop-twitter"         % "20.10.0.0",
      "org.scalaz"         %% "scalaz-core"                 % "7.3.2",
    )
  )
  .settings(macroExpansionSettings)
  .settings(mdocJS := Some(jsdocs))
  .dependsOn(coreJVM, streamsJVM, testJVM, testMagnoliaJVM, coreJS)
  .enablePlugins(MdocPlugin, DocusaurusPlugin)

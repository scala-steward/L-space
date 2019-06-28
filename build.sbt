import Dependencies._
import sbtcrossproject.CrossProject
// shadow sbt-scalajs' crossProject and CrossType until Scala.js 1.0.0 is released
import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}

ThisBuild / organization := "eu.l-space"
ThisBuild / scalaVersion := "2.12.8"

lazy val settings = commonSettings

lazy val compilerOptions = Seq(
  "-unchecked",
  "-feature",
  "-language:existentials",
  "-language:higherKinds",
  "-language:implicitConversions",
  "-language:postfixOps",
  "-language:reflectiveCalls",
//  "-language:experimental.macros",
  "-Ypartial-unification",
//  "-Yliteral-types",
//  "-Xlog-implicits",
//  "-Ytyper-debug",
//  "-Ybreak-cycles",
//  "-Ylog:all",
//  "-verbose",
//  "-Xdev",
//  "-Ydebug",
  "-deprecation",
  "-encoding",
  "utf8"
)

lazy val projectSettings = Seq(
  homepage := Some(url("https://github.com/L-space/L-space")),
  licenses := List("MIT" -> url("https://opensource.org/licenses/MIT")),
  developers := List(
    Developer(
      "thijsbroersen",
      "Thijs Broersen",
      "thijsbroersen@gmail.com",
      url("https://github.com/ThijsBroersen")
    )
  )
)

lazy val commonSettings = projectSettings ++ Seq(
  scalacOptions ++= compilerOptions,
  scalaVersion := "2.12.8",
  crossScalaVersions := Seq("2.11.12", "2.12.8"),
  publishArtifact in (Test, packageBin) := true,
  updateOptions := updateOptions.value.withCachedResolution(true)
)

val dirtyEnd = """(\+\d\d\d\d\d\d\d\d-\d\d\d\d)-SNAPSHOT$""".r
def stripTime(version: String) = dirtyEnd.findFirstIn(version) match {
  case Some(end) => version.stripSuffix(end) + "-SNAPSHOT"
  case None => version
}

ThisBuild / version ~= stripTime
ThisBuild / dynver ~= stripTime

lazy val lspace = project
  .in(file("."))
  .settings(settings)
  .settings(skip in publish := true)
  .aggregate(core.jvm, core.js, parse.jvm, parse.js, parseArgonaut.jvm, parseArgonaut.js, parseCirce.jvm, parseCirce.js, client.jvm, client.js, graph, services)

lazy val core = (crossProject(JSPlatform, JVMPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Full) in file("core"))
  .settings(settings)
  .settings(
    name := "lspace-core",
    libraryDependencies ++= coreDeps.value,
    Test / parallelExecution := false
  )
  .jvmSettings(
    libraryDependencies ++= coreJvmDeps
  )
  .jsSettings(
    jsEnv in Test := new org.scalajs.jsenv.nodejs.NodeJSEnv(),
    libraryDependencies ++= coreJsDeps.value
  )

lazy val parse = (crossProject(JSPlatform, JVMPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Full) in file("parse/core"))
  .dependsOn(core % "compile->compile;test->test")
  .settings(settings)
  .settings(
    name := "lspace-parse",
    libraryDependencies ++= parseDeps.value,
    Test / parallelExecution := false
  )
  .jvmSettings(
    libraryDependencies ++= parseJvmDeps
  )
  .jsSettings(
    jsEnv in Test := new org.scalajs.jsenv.nodejs.NodeJSEnv(),
    libraryDependencies ++= parseJsDeps.value
  )

lazy val parseArgonaut = (crossProject(JSPlatform, JVMPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure) in file("parse/argonaut"))
  .dependsOn(parse % "compile->compile;test->test")
  .settings(settings)
  .settings(
    name := "lspace-parse-argonaut",
    libraryDependencies ++= parseArgonautDeps.value,
    Test / parallelExecution := false
  )
  .jvmSettings(
  )
  .jsSettings(
    jsEnv in Test := new org.scalajs.jsenv.nodejs.NodeJSEnv()
  )

lazy val parseCirce = (crossProject(JSPlatform, JVMPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure) in file("parse/circe"))
  .dependsOn(parse % "compile->compile;test->test")
  .settings(settings)
  .settings(
    name := "lspace-parse-circe",
    libraryDependencies ++= parseCirceDeps.value,
    Test / parallelExecution := false
  )
  .jvmSettings(
  )
  .jsSettings(
    jsEnv in Test := new org.scalajs.jsenv.nodejs.NodeJSEnv()
  )

lazy val client =
  (crossProject(JSPlatform, JVMPlatform)
    .withoutSuffixFor(JVMPlatform)
    .crossType(CrossType.Full) in file("client"))
    .dependsOn(core % "compile->compile;test->test")
    .settings(settings)
    .settings(
      name := "lspace-client",
      libraryDependencies ++= clientDeps.value,
      Test / parallelExecution := false
    )
    .jvmSettings(
      libraryDependencies ++= clientJvmDeps
    )
    .jsSettings(
      jsEnv in Test := new org.scalajs.jsenv.nodejs.NodeJSEnv(),
      libraryDependencies ++= clientJsDeps.value
    )

lazy val graph = (project in file("graph"))
  .dependsOn(parse.jvm % "compile->compile;test->test", parseArgonaut.jvm % "test->compile")
  .settings(settings)
  .settings(
    name := "lspace-graph",
    libraryDependencies ++= graphDeps
  )

lazy val cassandra = (project in file("store/cassandra"))
  .dependsOn(graph % "compile->compile;test->test", parseArgonaut.jvm % "test->compile")
  .settings(settings)
  .settings(
    name := "lspace-store-cassandra",
    libraryDependencies ++= storeCassandraDeps,
    Test / parallelExecution := true
  )

lazy val kafka = (project in file("store/kafka"))
  .dependsOn(graph % "compile->compile;test->test")
  .settings(settings)
  .settings(
    name := "lspace-store-kafka",
    libraryDependencies ++= storeKafkaDeps,
    Test / parallelExecution := true
  )

lazy val elasticsearch = (project in file("index/elasticsearch"))
  .dependsOn(graph % "compile->compile;test->test")
  .settings(settings)
  .settings(
    name := "lspace-index-elasticsearch",
    libraryDependencies ++= indexElasticsearchDeps,
    Test / parallelExecution := true
  )

lazy val services = (project in file("services"))
  .dependsOn(client.jvm % "compile->compile;test->test", parse.jvm % "compile->compile;test->test", parseArgonaut.jvm % "test")
  .settings(settings)
  .settings(
    name := "lspace-services",
    libraryDependencies ++= servicesDeps
  )

val makeSettingsYml = Def.task {
  val file     = (resourceManaged in Compile).value / "site" / "data" / "settings.yml"
  val contents = s"version: ${version.value}"
  IO.write(file, contents)
  Seq(file)
}

lazy val site = (project in file("site"))
  .enablePlugins(MicrositesPlugin)
  .dependsOn(services % "compile->compile;compile->test")
  .settings(name := "lspace-site")
  .settings(skip in publish := true)
  .settings(projectSettings)
  .settings(
    resourceGenerators in Compile += makeSettingsYml.taskValue,
    makeMicrosite := (makeMicrosite dependsOn makeSettingsYml).value,
    scalacOptions in Tut := compilerOptions
  )
  .settings(
    micrositeName := "L-space",
    micrositeDescription := "L-space, a graph computing framework for Scala",
    micrositeDataDirectory := (resourceManaged in Compile).value / "site" / "data",
    //    unmanagedResources ++= Seq(
    //
    //    ),
    //    micrositeDocumentationUrl := "/yoursite/docs",
    //    micrositeDocumentationLabelDescription := "Documentation",
    micrositeAuthor := "Thijs Broersen",
    micrositeHomepage := "https://docs.l-space.eu",
    micrositeOrganizationHomepage := "https://l-space.eu",
    //    micrositeOrganizationHomepage := "",
    excludeFilter in ghpagesCleanSite := //preserves github-settings for custom domain, each time CNAME is written custom domain is reset?
      new FileFilter{
        def accept(f: File) = (ghpagesRepository.value / "CNAME").getCanonicalPath == f.getCanonicalPath
      } || "versions.html",
    micrositeGithubOwner := "L-space",
    micrositeGithubRepo := "L-space",
    micrositeGitterChannelUrl := "L-space/L-space",
    micrositeFooterText := Some(
      "<b>Knowledge is Power</b> <- <i>BOOKS = KNOWLEDGE = POWER = (FORCE X DISTANCE ÷ TIME)</i>")
  )

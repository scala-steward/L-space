import com.softwaremill.SbtSoftwareMillCommon.commonSmlBuildSettings

ThisBuild / scalaVersion := "3.1.3-RC1-bin-20220324-d26a860-NIGHTLY"
// ThisBuild / crossScalaVersions := Seq("2.13.7", "3.1.0")
ThisBuild / githubWorkflowJavaVersions := Seq(sbtghactions.JavaSpec.graalvm("21.3.0","17"))

inThisBuild(
  List(
    organization := "eu.l-space",
    homepage     := Some(url("https://github.com/L-space/L-space")),
    licenses     := List("MIT" -> url("https://opensource.org/licenses/MIT")),
    developers := List(
      Developer(
        "thijsbroersen",
        "Thijs Broersen",
        "thijsbroersen@gmail.com",
        url("https://gitlab.com/ThijsBroersen")
      ),
      Developer(
        "thijsbroersen",
        "Thijs Broersen",
        "thijsbroersen@gmail.com",
        url("https://github.com/ThijsBroersen")
      )
    ),
    // scalacOptions ++= Seq(
    //   "-Ywarn-unused"
    // ),
    scalafixDependencies += "com.github.liancheng" %% "organize-imports" % "0.5.0"
    // scalafixScalaBinaryVersion := CrossVersion.binaryScalaVersion(scalaVersion.value)
  )
)

ThisBuild / githubWorkflowTargetTags ++= Seq("v*")
ThisBuild / githubWorkflowPublishTargetBranches :=
  Seq(RefPredicate.StartsWith(Ref.Tag("v")))

ThisBuild / githubWorkflowPublish := Seq(
  WorkflowStep.Sbt(
    List("ci-release"),
    env = Map(
      "PGP_PASSPHRASE"    -> "${{ secrets.PGP_PASSPHRASE }}",
      "PGP_SECRET"        -> "${{ secrets.PGP_SECRET }}",
      "SONATYPE_PASSWORD" -> "${{ secrets.SONATYPE_PASSWORD }}",
      "SONATYPE_USERNAME" -> "${{ secrets.SONATYPE_USERNAME }}"
    )
  )
)

lazy val commonSettings = commonSmlBuildSettings ++ Seq(
  Test / packageBin / publishArtifact := true,
  //  publishArtifact in (IntegrationTest, packageBin) := true,
  updateOptions        := updateOptions.value.withCachedResolution(true),
  Compile / run / fork := true,
  //  Test / fork := true,
  //  Test / testForkedParallel := true
  scalacOptions ++= Seq(
    "-Yexplicit-nulls"
    // "-language:strictEquality"
  )
)

val skipInPublish = Seq(
  (publish / skip) := true,
  publish          := {}
)

lazy val lspace = project
  .in(file("."))
  .settings(skipInPublish)
  .aggregate(
    // core.jvm,
    // core.js
    model.jvm,
    model.js
    // parse,
    // client.jvm,
    // client.js,
    // graph
//    store
//    services
  )

lazy val model =
  (crossProject(JSPlatform, JVMPlatform)
    .withoutSuffixFor(JVMPlatform)
    .crossType(CrossType.Full) in file("model"))
    .settings(commonSettings)
    .settings(
      name := "lspace-model",
      libraryDependencies ++= Seq(
        "org.typelevel" %% "squants" % Version.squants
      )
    )
    .settings(
      SharedSettings.test
    )

// lazy val core = (crossProject(JSPlatform, JVMPlatform)
//   .withoutSuffixFor(JVMPlatform)
//   .crossType(CrossType.Full) in file("core"))
//   .settings(commonSettings)
//   .settings(
//     name := "lspace-core",
//     libraryDependencies ++= Dependencies.core.value
//   )
//   .jvmSettings(
//     libraryDependencies ++= Dependencies.coreJvm
//   )
//   .jsSettings(
// //    scalaJSLinkerConfig ~= { _.withOptimizer(false) },
//     libraryDependencies ++= Dependencies.coreJs.value
//   )

// lazy val parse = project
//   .in(file("parse"))
//   .settings(skipInPublish)
//   .aggregate(
//     parseCore.jvm,
//     parseCore.js,
//     parseArgonaut.jvm,
//     parseArgonaut.js,
//     parseCirce.jvm,
//     parseCirce.js
//   )

// lazy val parseCore = (crossProject(JSPlatform, JVMPlatform)
//   .withoutSuffixFor(JVMPlatform)
//   .crossType(CrossType.Full) in file("parse/core"))
//   .dependsOn(core % "compile->compile;test->test")
//   .settings(commonSettings)
//   .settings(
//     name := "lspace-parse",
//     libraryDependencies ++= Dependencies.parse.value
//   )
//   .jvmSettings(
//     libraryDependencies ++= Dependencies.parseJvm
//   )
//   .jsSettings(
// //    scalaJSLinkerConfig ~= { _.withOptimizer(false) },
//     libraryDependencies ++= Dependencies.parseJs.value
//   )

// lazy val parseArgonaut = (crossProject(JSPlatform, JVMPlatform)
//   .withoutSuffixFor(JVMPlatform)
//   .crossType(CrossType.Full) in file("parse/argonaut"))
//   .dependsOn(parseCore % "compile->compile;test->test")
//   .settings(commonSettings)
//   .settings(
//     name := "lspace-parse-argonaut",
//     libraryDependencies ++= Dependencies.parseArgonaut.value
//   )
//   .jvmSettings(
//   )
//   .jsSettings(
// //    scalaJSLinkerConfig ~= { _.withOptimizer(false) },
//   )

// lazy val parseCirce = (crossProject(JSPlatform, JVMPlatform)
//   .withoutSuffixFor(JVMPlatform)
//   .crossType(CrossType.Full) in file("parse/circe"))
//   .dependsOn(parseCore % "compile->compile;test->test")
//   .settings(commonSettings)
//   .settings(
//     name := "lspace-parse-circe",
//     libraryDependencies ++= Dependencies.parseCirce.value
//   )
//   .jvmSettings(
//   )
//   .jsSettings(
// //    scalaJSLinkerConfig ~= { _.withOptimizer(false) },
//   )

// lazy val client =
//   (crossProject(JSPlatform, JVMPlatform)
//     .withoutSuffixFor(JVMPlatform)
//     .crossType(CrossType.Full) in file("client"))
//     .dependsOn(core % "compile->compile;test->test")
//     .settings(commonSettings)
//     .settings(
//       name := "lspace-client",
//       libraryDependencies ++= Dependencies.client.value
//     )
//     .jvmSettings(
//       libraryDependencies ++= Dependencies.clientJvm
//     )
//     .jsSettings(
// //      scalaJSLinkerConfig ~= { _.withOptimizer(false) },
//       libraryDependencies ++= Dependencies.clientJs.value
//     )

// lazy val graph = (project in file("graph"))
//   .dependsOn(parseCore.jvm % "compile->compile;test->test", parseArgonaut.jvm % "test->compile")
//   .settings(commonSettings)
//   .settings(
//     name := "lspace-graph",
//     libraryDependencies ++= Dependencies.graph
//   )

// lazy val store = project
//   .in(file("store"))
//   .settings(skipInPublish)
//   .aggregate(
//     cassandra
// //    kafka
//   )

// lazy val cassandra = (project in file("store/cassandra"))
//   .dependsOn(graph % "compile->compile;test->test", parseArgonaut.jvm)
//   .settings(commonSettings)
//   .settings(
//     name := "lspace-store-cassandra",
//     libraryDependencies ++= Dependencies.storeCassandra,
//     Test / parallelExecution := true
//   )

//lazy val kafka = (project in file("store/kafka"))
//  .dependsOn(graph % "compile->compile;test->test")
//  .settings(settings)
//  .settings(
//    name := "lspace-store-kafka",
//    libraryDependencies ++= storeKafka,
//    Test / parallelExecution := true
//  )
//
//lazy val elasticsearch = (project in file("index/elasticsearch"))
//  .dependsOn(graph % "compile->compile;test->test")
//  .settings(settings)
//  .settings(
//    name := "lspace-index-elasticsearch",
//    libraryDependencies ++= indexElasticsearch,
//    Test / parallelExecution := true
//  )

// lazy val endpoints = (crossProject(JSPlatform, JVMPlatform)
//   .withoutSuffixFor(JVMPlatform)
//   .crossType(CrossType.Pure) in file("endpoints"))
//   .dependsOn(
//     client        % "compile->compile;test->test",
//     parseCore     % "compile->compile;test->test",
//     parseArgonaut % "test"
//   )
//   .settings(commonSettings)
//   .settings(
//     name := "lspace-endpoints",
//     libraryDependencies ++= Dependencies.endpoints.value
//   )
// //  .jvmSettings(
// //    libraryDependencies ++= Dependencies.endpointsJvm
// //  )
// //  .jsSettings(
// //    //    scalaJSLinkerConfig ~= { _.withOptimizer(false) },
// //    libraryDependencies ++= Dependencies.endpointsJs.value
// //  )

// lazy val services = (project in file("services/core"))
//   .dependsOn(
//     client.jvm        % "compile->compile;test->test",
//     parseCore.jvm     % "compile->compile;test->test",
//     parseArgonaut.jvm % "test"
//   )
//   .settings(commonSettings)
//   .settings(
//     name := "lspace-services",
//     libraryDependencies ++= Dependencies.services
//   )

//lazy val servicesFinch = (project in file("services/finch"))
//  .dependsOn(services % "compile->compile;test->test")
//  .settings(settings)
//  .settings(
//    name := "lspace-services-finch",
//    scalaVersion := "2.12.10",
//    crossScalaVersions := Seq("2.12.10"),
//    libraryDependencies ++= servicesFinch
//  )

// lazy val site = (project in file("site"))
//   .enablePlugins(MicrositesPlugin)
//   .enablePlugins(MdocPlugin)
//   .dependsOn(parseCore.jvm % "compile->compile;compile->test")
//   .settings(name := "lspace-site")
//   .settings((publish / skip) := true)
//   .settings(
// //    micrositeCompilingDocsTool := WithMdoc,
//     mdocVariables := Map("VERSION" -> version.value)
//   )
//   .settings(
//     micrositeName := "L-space",
//     micrositeDescription := "L-space, a graph computing framework for Scala",
//     micrositeDataDirectory := (Compile / resourceDirectory).value / "data",
//     //    micrositeDocumentationUrl := "/yoursite/docs",
//     //    micrositeDocumentationLabelDescription := "Documentation",
//     micrositeUrl := "https://docs.l-space.eu",
// //    micrositeBaseUrl := "/l-space",
//     micrositeAuthor := "Thijs Broersen",
//     micrositeHomepage := "https://docs.l-space.eu",
//     micrositeOrganizationHomepage := "https://l-space.eu",
//     micrositeDocumentationUrl := "/docs",
//     (makeSite / includeFilter) := "*.html" | "*.css" | "*.png" | "*.jpg" | "*.gif" | "*.js" | "*.swf" | "*.md" | "*.svg",
//     (ghpagesCleanSite / excludeFilter) := //preserves github-settings for custom domain, each time CNAME is written custom domain is reset?
//       new FileFilter {
//         def accept(f: File) = (ghpagesRepository.value / "CNAME").getCanonicalPath == f.getCanonicalPath
//       } || "versions.html",
//     micrositeGithubOwner := "L-space",
//     micrositeGithubRepo := "L-space",
//     micrositeGitterChannelUrl := "L-space/L-space",
//     micrositeFooterText := Some(
//       "<b>Knowledge is Power</b> <- <i>BOOKS = KNOWLEDGE = POWER = (FORCE X DISTANCE ÷ TIME)</i>"
//     )
//   )

//def scalaPartV = Def.setting(CrossVersion.partialVersion(scalaVersion.value))
//lazy val crossVersionSharedSources: Seq[Setting[_]] =
//  Seq(Compile, Test).map { sc =>
//    (sc / unmanagedSourceDirectories) ++= {
//      (sc / unmanagedSourceDirectories).value.flatMap { dir =>
//        Seq(
//          scalaPartV.value match {
//            case Some((2, y)) if y == 12 => new File(dir.getPath + "_2.12")
//            case Some((2, y)) if y >= 13 => new File(dir.getPath + "_2.13")
//          },
//          scalaPartV.value match {
//            case Some((2, n)) if n >= 12 => new File(dir.getPath + "_2.12+")
//            case _                       => new File(dir.getPath + "_2.12-")
//          },
//          scalaPartV.value match {
//            case Some((2, n)) if n >= 13 => new File(dir.getPath + "_2.13+")
//            case _                       => new File(dir.getPath + "_2.13-")
//          }
//        )
//      }
//    }
//  }

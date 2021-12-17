import sbt._
import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport._

object Dependencies {

  lazy val core = Def.setting(
    Seq(
      "eu.l-space"   %%% "types"          % Version.lspaceTypes,
      "io.monix"     %%% "monix-reactive" % Version.monix,
      ("com.chuusai" %%% "shapeless"      % Version.shapeless).cross(CrossVersion.for3Use2_13),
//      "eu.timepit"   %%% "refined"        % Version.refined,
//      "org.typelevel" %%% "squants"        % "1.5.0",
//      "org.typelevel" %%% "spire"          % "0.16.0",
      ("com.outr"     %%% "scribe"    % Version.scribe).cross(CrossVersion.for3Use2_13),
      "org.scalatest" %%% "scalatest" % Version.scalaTest % "test",
      // "io.monix"          %%% "minitest"        % Version.minitest  % "test",
      "io.github.cquiroz" %%% "scala-java-time" % Version.scalaJavaTime
    )
  )

  lazy val coreJs = Def.setting(
    Seq(
//      "org.scala-js" %%% "scalajs-dom" % "1.0.0"
    )
  )

  lazy val coreJvm = Seq()

  lazy val parse = Def.setting(
    Seq(
      "com.softwaremill.sttp.shared"  %%% "monix" % Version.sttpShared,
      "com.softwaremill.sttp.client3" %%% "core"  % Version.sttpClient
    )
  )

  lazy val parseJs = Def.setting(
    Seq(
      ("com.softwaremill.sttp.client3" %%% "monix" % Version.sttpClient).cross(CrossVersion.for3Use2_13)
    )
  )

  lazy val parseJvm = Seq(
    ("com.softwaremill.sttp.client3" %% "async-http-client-backend-monix" % Version.sttpClient).cross(
      CrossVersion.for3Use2_13
    )
  )

  lazy val parseArgonaut = Def.setting(Seq("io.argonaut" %%% "argonaut" % Version.argonaut))

  lazy val parseCirce = Def.setting(Seq("io.circe" %%% "circe-parser" % Version.circe))

  lazy val client = Def.setting(
    Seq(
      "com.softwaremill.sttp.tapir" %%% "tapir-sttp-client" % Version.sttpTapir
    )
  )

  lazy val clientJs = Def.setting(Seq())

  lazy val clientJvm = Seq()

  lazy val graph = Seq(
//    "com.github.cb372"       %% "scalacache-monix" % "0.27.0",
    "com.github.pureconfig" %% "pureconfig"         % Version.pureconfig,
    "com.github.pureconfig" %% "pureconfig-generic" % Version.pureconfig
  )

//   lazy val storeCassandra = Seq(
// //    "com.outworkers" %% "phantom-dsl" % Version.phantom
// //    "ch.qos.logback"             % "logback-classic" % "1.2.3",
// //    "com.typesafe.scala-logging" %% "scala-logging"  % "3.9.2"
//   ) ++ Seq(
//     "com.dimafeng" %% "testcontainers-scala-scalatest" % Version.testContainers % "test",
//     "com.dimafeng" %% "testcontainers-scala-cassandra" % Version.testContainers % "test"
//   )

  lazy val storeKafka = Seq(
    "io.monix" %% "monix-kafka-1x" % "1.0.0-RC7"
//    ,
//    "org.apache.kafka" % "kafka"           % "2.3.1"
  )

  lazy val indexElasticsearch = Seq(
//    "com.sksamuel.elastic4s"   %% "elastic4s-core"                      % Version.elastic4s,
//    "com.sksamuel.elastic4s"   %% "elastic4s-http-streams"              % Version.elastic4s,
    "org.elasticsearch.client" % "elasticsearch-rest-high-level-client" % Version.elastic4s
//    "com.sksamuel.elastic4s" %% "elastic4s-effect-monix" % Version.elastic4s exclude ("io.monix", "monix")
  )

  lazy val endpoints = Def.setting(
    Seq(
      "io.monix"                     %%% "monix"            % Version.monix,
      "com.softwaremill.sttp.shared" %%% "monix"            % Version.sttpShared,
      "com.softwaremill.sttp.tapir"  %%% "tapir-core"       % Version.sttpTapir,
      "com.softwaremill.sttp.tapir"  %%% "tapir-json-circe" % Version.sttpTapir,
      "com.softwaremill.sttp.tapir"  %%% "tapir-enumeratum" % Version.sttpTapir
    )
  )

  lazy val services = Seq(
    "com.softwaremill.sttp.tapir" %% "tapir-core"               % Version.sttpTapir,
    "com.softwaremill.sttp.tapir" %% "tapir-akka-http-server"   % Version.sttpTapir,
    "com.softwaremill.sttp.tapir" %% "tapir-openapi-docs"       % Version.sttpTapir,
    "com.softwaremill.sttp.tapir" %% "tapir-openapi-circe-yaml" % Version.sttpTapir,
    "com.vmunier"                 %% "scalajs-scripts"          % "1.1.4",
    //    "com.github.t3hnar" %% "scala-bcrypt" % "3.1",
    "com.lihaoyi" %% "scalatags" % "0.9.4" // TODO: replace with Laminar
//    "com.raquo" %% "domtypes"   % "0.9.5",
//    "com.raquo" %% "dombuilder" % "0.9.2"
  )
  lazy val servicesFinch = Seq(
//    "com.github.finagle" %% "finchx-core"          % Version.finch,
//    "com.github.finagle" %% "finchx-generic"       % Version.finch,
//    "com.github.finagle" %% "finchx-argonaut"      % Version.finch,
//    "com.github.finagle" %% "finchx-fs2"           % Version.finch,
//    "com.github.finagle" %% "finchx-refined"       % Version.finch,
//    "co.fs2" %% "fs2-reactive-streams" % "2.4.4"
  )
}

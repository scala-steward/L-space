import sbt._
import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport._

object Dependencies {

  val coreDeps = Def.setting(
    Seq(
      "eu.l-space"  %%% "types"          % "0.2.1",
      "io.monix"    %%% "monix-reactive" % Version.monix,
      "com.chuusai" %%% "shapeless"      % "2.3.7",
//      "org.typelevel" %%% "squants"        % "1.5.0",
//      "org.typelevel" %%% "spire"          % "0.16.0",
      "com.outr"          %%% "scribe"          % "3.5.5",
      "org.scalatest"     %%% "scalatest"       % Version.scalatest % "test",
      "io.monix"          %%% "minitest"        % Version.minitest  % "test",
      "io.github.cquiroz" %%% "scala-java-time" % "2.3.0"
    )
  )

  val coreJsDeps = Def.setting(
    Seq(
//      "org.scala-js" %%% "scalajs-dom" % "1.0.0"
    )
  )

  val coreJvmDeps = Seq()

  val parseDeps = Def.setting(
    Seq(
      "com.softwaremill.sttp.client" %%% "core"  % Version.sttp,
      "com.softwaremill.sttp.client" %%% "monix" % Version.sttp
    )
  )

  val parseJsDeps = Def.setting(Seq())

  val parseJvmDeps = Seq(
    "com.softwaremill.sttp.client" %% "async-http-client-backend-monix" % Version.sttp
  )

  val parseArgonautDeps = Def.setting(Seq("io.argonaut" %%% "argonaut" % Version.argonaut))

  val parseCirceDeps = Def.setting(Seq("io.circe" %%% "circe-parser" % Version.circe))

  val clientDeps = Def.setting(Seq())

  val clientJsDeps = Def.setting(Seq())

  val clientJvmDeps = Seq()

  val graphDeps = Seq(
//    "com.github.cb372"       %% "scalacache-monix" % "0.27.0",
    "com.github.pureconfig" %% "pureconfig"         % Version.pureconfig,
    "com.github.pureconfig" %% "pureconfig-generic" % Version.pureconfig
  )

  val storeCassandraDeps = Seq(
    // "com.outworkers" %% "phantom-dsl" % Version.phantom
//    "ch.qos.logback"             % "logback-classic" % "1.2.3",
//    "com.typesafe.scala-logging" %% "scala-logging"  % "3.9.2"
  )

  val storeKafkaDeps = Seq(
    // "io.monix" %% "monix-kafka-1x" % "1.0.0-RC6"
//    ,
//    "org.apache.kafka" % "kafka"           % "2.3.1"
  )

  val indexElasticsearchDeps = Seq(
//    "com.sksamuel.elastic4s"   %% "elastic4s-core"                      % Version.elastic4s,
//    "com.sksamuel.elastic4s"   %% "elastic4s-http-streams"              % Version.elastic4s,
    "org.elasticsearch.client" % "elasticsearch-rest-high-level-client" % Version.elastic4s
//    "com.sksamuel.elastic4s" %% "elastic4s-effect-monix" % Version.elastic4s exclude ("io.monix", "monix")
  )

  val servicesDeps = Seq(
    "com.softwaremill.sttp.tapir" %% "tapir-core"               % Version.tapir,
    "com.softwaremill.sttp.tapir" %% "tapir-akka-http-server"   % Version.tapir,
    "com.softwaremill.sttp.tapir" %% "tapir-openapi-docs"       % Version.tapir,
    "com.softwaremill.sttp.tapir" %% "tapir-openapi-circe-yaml" % Version.tapir,
    "com.vmunier"                 %% "scalajs-scripts"          % "1.1.4",
    //    "com.github.t3hnar" %% "scala-bcrypt" % "3.1",
    "com.lihaoyi" %% "scalatags" % "0.9.4" //TODO: replace with Laminar
//    "com.raquo" %% "domtypes"   % "0.9.5",
//    "com.raquo" %% "dombuilder" % "0.9.2"
  )
  val servicesFinchDeps = Seq(
    // "com.github.finagle" %% "finchx-core"          % Version.finch,
    // "com.github.finagle" %% "finchx-generic"       % Version.finch,
    // "com.github.finagle" %% "finchx-argonaut"      % Version.finch,
    // "com.github.finagle" %% "finchx-fs2"           % Version.finch,
    // "com.github.finagle" %% "finchx-refined"       % Version.finch,
    "co.fs2"             %% "fs2-reactive-streams" % "3.1.1"
  )
}

import sbt._
import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport._

object Version {
  val finch      = "0.31.0"
  val monix      = "3.1.0"
  val sttpClient = "2.0.2"
  val sttpTapir  = "0.12.12"
  val elastic4s  = "7.3.1"
  val phantom    = "2.42.0"
}

object Dependencies {

  val coreDeps = Def.setting(
    Seq(
      "eu.l-space"  %%% "types"          % "0.0.4.3",
      "io.monix"    %%% "monix-reactive" % Version.monix,
      "com.chuusai" %%% "shapeless"      % "2.3.3",
//      "org.typelevel" %%% "squants"        % "1.5.0",
//      "org.typelevel" %%% "spire"          % "0.16.0",
      "com.outr"          %%% "scribe"          % "2.7.10",
      "org.scalatest"     %%% "scalatest"       % "3.1.0" % "test",
      "io.monix"          %%% "minitest"        % "2.7.0" % "test",
      "io.github.cquiroz" %%% "scala-java-time" % "2.0.0-RC3"
    ))

  val coreJsDeps = Def.setting(
    Seq(
      "org.scala-js" %%% "scalajs-dom" % "0.9.7"
    ))

  val coreJvmDeps = Seq()

  val parseDeps = Def.setting(
    Seq(
      "com.softwaremill.sttp.client" %%% "core"  % Version.sttpClient,
      "com.softwaremill.sttp.client" %%% "monix" % Version.sttpClient
    ))

  val parseJsDeps = Def.setting(Seq())

  val parseJvmDeps = Seq("com.softwaremill.sttp.client" %% "okhttp-backend-monix" % Version.sttpClient)

  val parseArgonautDeps = Def.setting(Seq("io.argonaut" %%% "argonaut" % "6.2.3"))

  val parseCirceDeps = Def.setting(Seq("io.circe" %%% "circe-parser" % "0.12.3"))

  val clientDeps = Def.setting(Seq())

  val clientJsDeps = Def.setting(Seq())

  val clientJvmDeps = Seq()

  val graphDeps = Seq(
//    "com.github.cb372"       %% "scalacache-monix" % "0.27.0",
    "com.github.pureconfig" %% "pureconfig"         % "0.12.2",
    "com.github.pureconfig" %% "pureconfig-generic" % "0.12.2"
  )

  val storeCassandraDeps = Seq(
    "com.outworkers"             %% "phantom-dsl"    % Version.phantom,
    "ch.qos.logback"             % "logback-classic" % "1.2.3",
    "com.typesafe.scala-logging" %% "scala-logging"  % "3.9.2"
  )

  val storeKafkaDeps = Seq(
    "io.monix" %% "monix-kafka-1x" % "1.0.0-RC5"
//    ,
//    "org.apache.kafka" % "kafka"           % "2.3.1"
  )

  val indexElasticsearchDeps = Seq(
//    "com.sksamuel.elastic4s"   %% "elastic4s-core"                      % Version.elastic4s,
//    "com.sksamuel.elastic4s"   %% "elastic4s-http-streams"              % Version.elastic4s,
    "org.elasticsearch.client" % "elasticsearch-rest-high-level-client" % "7.4.2"
//    "com.sksamuel.elastic4s" %% "elastic4s-effect-monix" % Version.elastic4s exclude ("io.monix", "monix")
  )

  val servicesDeps = Seq(
//    "com.twitter"        %% "twitter-server"       % "19.4.0" % "test",
    "com.softwaremill.sttp.tapir" %% "tapir-core" % Version.sttpTapir,
//    "com.softwaremill.sttp.tapir" %% "tapir-finatra-server" % Version.sttpTapir,
    "com.softwaremill.sttp.tapir" %% "tapir-akka-http-server" % Version.sttpTapir,
    "com.vmunier"                 %% "scalajs-scripts"        % "1.1.4",
    //    "com.github.t3hnar" %% "scala-bcrypt" % "3.1",
    "com.lihaoyi" %% "scalatags" % "0.7.0" //TODO: replace with Laminar
//    "com.raquo" %% "domtypes"   % "0.9.5",
//    "com.raquo" %% "dombuilder" % "0.9.2"
//    "org.scalatest" %% "scalatest" % "3.0.8" % "test"
  )
  val servicesFinchDeps = Seq(
    "com.github.finagle" %% "finchx-core"          % Version.finch,
    "com.github.finagle" %% "finchx-generic"       % Version.finch,
    "com.github.finagle" %% "finchx-argonaut"      % Version.finch,
    "com.github.finagle" %% "finchx-fs2"           % Version.finch,
    "com.github.finagle" %% "finchx-refined"       % Version.finch,
    "co.fs2"             %% "fs2-reactive-streams" % "2.1.0"
  )
}

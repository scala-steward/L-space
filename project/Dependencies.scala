import sbt._
import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport._

object Version {
  val scala     = "2.12.8"
  val finch     = "0.29.0"
  val monix     = "3.0.0-RC3"
  val sttp      = "1.6.0"
  val elastic4s = "7.0.1"
  val phantom   = "2.37.0"
}

object Dependencies {

  val coreDeps = Def.setting(
    Seq(
      "io.monix"      %%% "monix-reactive" % Version.monix,
      "com.chuusai"   %%% "shapeless"      % "2.3.3",
      "org.typelevel" %%% "squants"        % "1.4.0",
//      "org.typelevel" %%% "spire"          % "0.16.0",
      "com.outr"      %%% "scribe"    % "2.7.7",
      "org.scalatest" %%% "scalatest" % "3.0.8" % "test"
    ))

  val coreJsDeps = Def.setting(
    Seq(
      "io.github.cquiroz" %%% "scala-java-time" % "2.0.0-RC3",
      "org.scala-js"      %%% "scalajs-dom"     % "0.9.7"
    ))

  val coreJvmDeps = Seq()

  val parseDeps = Def.setting(
    Seq(
      "com.softwaremill.sttp" %%% "core"  % Version.sttp,
      "com.softwaremill.sttp" %%% "monix" % Version.sttp
    ))

  val parseJsDeps = Def.setting(Seq())

  val parseJvmDeps = Seq("com.softwaremill.sttp" %% "okhttp-backend-monix" % Version.sttp)

  val parseArgonautDeps = Def.setting(Seq("io.argonaut" %%% "argonaut" % "6.2.3"))

  val parseCirceDeps = Def.setting(Seq("io.circe" %%% "circe-parser" % "0.11.1"))

  val clientDeps = Def.setting(Seq())

  val clientJsDeps = Def.setting(Seq())

  val clientJvmDeps = Seq()

  val graphDeps = Seq(
//    "com.github.cb372"       %% "scalacache-monix" % "0.27.0",
  )

  val storeCassandraDeps = Seq(
    "com.outworkers"             %% "phantom-dsl"    % Version.phantom,
    "ch.qos.logback"             % "logback-classic" % "1.2.3",
    "com.typesafe.scala-logging" %% "scala-logging"  % "3.9.2"
  )

  val storeKafkaDeps = Seq(
    "io.monix" %% "monix-kafka-1x" % "1.0.0-RC4"
  )

  val indexElasticsearchDeps = Seq(
    "com.sksamuel.elastic4s" %% "elastic4s-core"         % Version.elastic4s,
    "com.sksamuel.elastic4s" %% "elastic4s-http-streams" % Version.elastic4s,
    "com.sksamuel.elastic4s" %% "elastic4s-effect-monix" % Version.elastic4s exclude ("io.monix", "monix")
  )

  val servicesDeps = Seq(
    "com.github.finagle" %% "finchx-core"          % Version.finch,
    "com.github.finagle" %% "finchx-generic"       % Version.finch,
    "com.github.finagle" %% "finchx-argonaut"      % Version.finch,
    "com.github.finagle" %% "finchx-fs2"           % Version.finch,
    "com.github.finagle" %% "finchx-refined"       % Version.finch,
    "co.fs2"             %% "fs2-reactive-streams" % "1.0.5",
    "com.twitter"        %% "twitter-server"       % "19.4.0",
    "com.vmunier"        %% "scalajs-scripts"      % "1.1.2",
    //    "com.github.t3hnar" %% "scala-bcrypt" % "3.1",
    "com.lihaoyi"   %% "scalatags" % "0.7.0",
    "org.scalatest" %% "scalatest" % "3.0.8" % "test"
  )
}

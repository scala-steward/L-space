import sbt._
import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport._

object Version {
  val scala     = "2.12.8"
  val finch     = "0.27.0"
  val argonaut  = "6.2.2"
  val monix     = "3.0.0-RC2"
  val sttp      = "1.5.1"
  val elastic4s = "6.5.0"
  val phantom   = "2.30.0"
}

object Dependencies {

  val coreDeps = Def.setting(
    Seq(
      "io.monix"      %%% "monix-reactive" % Version.monix,
      "com.chuusai"   %%% "shapeless"      % "2.3.3",
      "org.typelevel" %%% "squants"        % "1.4.0",
//      "org.typelevel" %%% "spire"          % "0.16.0",
      "org.scalatest" %%% "scalatest" % "3.0.5" % "test"
    ))

  val coreJsDeps = Def.setting(
    Seq(
      "io.github.cquiroz" %%% "scala-java-time" % "2.0.0-RC1",
      "org.scala-js"      %%% "scalajs-dom"     % "0.9.6"
    ))

  val coreJvmDeps = Seq("ch.qos.logback" % "logback-classic" % "1.2.3")

  val parseDeps = Def.setting(
    Seq(
      "io.argonaut" %%% "argonaut" % Version.argonaut
    ))

  val parseJsDeps = Def.setting(Seq())

  val parseJvmDeps = Seq()

  val clientDeps = Def.setting(
    Seq(
      "com.softwaremill.sttp" %%% "core"  % Version.sttp,
      "com.softwaremill.sttp" %%% "monix" % Version.sttp
    ))

  val clientJsDeps = Def.setting(Seq())

  val clientJvmDeps = Seq(
    "com.softwaremill.sttp" %% "okhttp-backend-monix" % Version.sttp
  )

  val graphDeps = Seq(
//    "com.github.cb372"       %% "scalacache-monix" % "0.27.0",
  )

  val storeCassandraDeps = Seq(
    "com.outworkers" %% "phantom-dsl" % Version.phantom
  )

  val storeKafkaDeps = Seq(
    "io.monix" %% "monix-kafka-1x" % "1.0.0-RC2"
  )

  val indexElasticsearchDeps = Seq(
    "com.sksamuel.elastic4s" %% "elastic4s-core"  % Version.elastic4s,
    "com.sksamuel.elastic4s" %% "elastic4s-http"  % Version.elastic4s,
    "com.sksamuel.elastic4s" %% "elastic4s-monix" % Version.elastic4s exclude ("io.monix", "monix")
  )

  val servicesDeps = Seq(
    "com.github.finagle" %% "finchx-core"     % Version.finch,
    "com.github.finagle" %% "finchx-generic"  % Version.finch,
    "com.github.finagle" %% "finchx-argonaut" % Version.finch,
    "com.github.finagle" %% "finchx-sse"      % Version.finch,
    "com.twitter"        %% "twitter-server"  % "18.12.0",
    "com.vmunier"        %% "scalajs-scripts" % "1.1.2",
    //    "com.github.t3hnar" %% "scala-bcrypt" % "3.1",
    "com.lihaoyi"   %% "scalatags" % "0.6.7",
    "org.scalatest" %% "scalatest" % "3.0.5" % "test"
  )
}

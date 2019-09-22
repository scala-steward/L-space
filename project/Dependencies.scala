import sbt._
import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport._

object Version {
//  val scala     = "2.12.8"
  val finch     = "0.31.0"
  val monix     = "3.0.0"
  val sttp      = "1.6.7"
  val elastic4s = "7.3.1"
  val phantom   = "2.42.0"
}

object Dependencies {

  val coreDeps = Def.setting(
    Seq(
      "eu.l-space"  %%% "types"          % "0.0.4.2",
      "io.monix"    %%% "monix-reactive" % Version.monix,
      "com.chuusai" %%% "shapeless"      % "2.3.3",
//      "org.typelevel" %%% "squants"        % "1.5.0",
//      "org.typelevel" %%% "spire"          % "0.16.0",
      "com.outr"          %%% "scribe"          % "2.7.10",
      "org.scalatest"     %%% "scalatest"       % "3.1.0-RC2" % "test",
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
      "com.softwaremill.sttp" %%% "core"  % Version.sttp,
      "com.softwaremill.sttp" %%% "monix" % Version.sttp
    ))

  val parseJsDeps = Def.setting(Seq())

  val parseJvmDeps = Seq("com.softwaremill.sttp" %% "okhttp-backend-monix" % Version.sttp)

  val parseArgonautDeps = Def.setting(Seq("io.argonaut" %%% "argonaut" % "6.2.3"))

  val parseCirceDeps = Def.setting(Seq("io.circe" %%% "circe-parser" % "0.12.1"))

  val clientDeps = Def.setting(Seq())

  val clientJsDeps = Def.setting(Seq())

  val clientJvmDeps = Seq()

  val graphDeps = Seq(
//    "com.github.cb372"       %% "scalacache-monix" % "0.27.0",
    "com.github.pureconfig" %% "pureconfig"         % "0.12.0",
    "com.github.pureconfig" %% "pureconfig-generic" % "0.12.0"
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
    "com.sksamuel.elastic4s" %% "elastic4s-http-streams" % Version.elastic4s
//    "com.sksamuel.elastic4s" %% "elastic4s-effect-monix" % Version.elastic4s exclude ("io.monix", "monix")
  )

  val servicesDeps = Seq(
    "com.github.finagle" %% "finchx-core"          % Version.finch,
    "com.github.finagle" %% "finchx-generic"       % Version.finch,
    "com.github.finagle" %% "finchx-argonaut"      % Version.finch,
    "com.github.finagle" %% "finchx-fs2"           % Version.finch,
    "com.github.finagle" %% "finchx-refined"       % Version.finch,
    "co.fs2"             %% "fs2-reactive-streams" % "2.0.0",
//    "com.twitter"        %% "twitter-server"       % "19.4.0" % "test",
    "com.vmunier" %% "scalajs-scripts" % "1.1.4",
    //    "com.github.t3hnar" %% "scala-bcrypt" % "3.1",
    "com.lihaoyi" %% "scalatags" % "0.7.0" //TODO: replace with Laminar
//    "com.raquo" %% "domtypes"   % "0.9.5",
//    "com.raquo" %% "dombuilder" % "0.9.2"
//    "org.scalatest" %% "scalatest" % "3.0.8" % "test"
  )
}

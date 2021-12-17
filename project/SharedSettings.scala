import sbt._
import Keys._
import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport._

object SharedSettings {

  lazy val test = Seq(
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % Version.scalaTest % "it,test",
      // "org.scalamock" %% "scalamock"                      % Version.scalaMock      % "test",
      // "com.dimafeng"  %% "testcontainers-scala-scalatest" % Version.testContainers % "it",
      "dev.zio" %%% "zio-test"     % Version.zio,
      "dev.zio"  %% "zio-test-sbt" % Version.zio % "test"
    ),
    testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework")
  )
}

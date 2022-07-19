import sbt._
import Keys._
import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport._

object SharedSettings {

  def test(scope: String = "test") = Seq(
    libraryDependencies ++= Seq(
      "org.scalatest" %%% "scalatest" % Version.scalaTest % scope,
      // "org.scalamock" %% "scalamock"                      % Version.scalaMock      % scope,
      "dev.zio" %%% "zio-test"     % Version.zio,
      "dev.zio"  %%% "zio-test-sbt" % Version.zio % scope
    ),
    testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework")
  )
}

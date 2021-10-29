addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject"      % "1.1.0")
addSbtPlugin("org.portable-scala" % "sbt-scala-native-crossproject" % "1.1.0")
addSbtPlugin("org.scala-js"       % "sbt-scalajs"                   % "1.7.0")
// addSbtPlugin("org.scala-native"   % "sbt-scala-native"              % "0.4.0")

addSbtPlugin("com.47deg" % "sbt-microsites" % "1.3.4")

addSbtPlugin("com.github.sbt" % "sbt-native-packager" % "1.9.4")

// addSbtPlugin("com.lightbend.sbt" % "sbt-proguard" % "0.4.0")

addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "1.0.0")

//sbt-scalafmt, sbt-tpolecat
addSbtPlugin("com.softwaremill.sbt-softwaremill" % "sbt-softwaremill-common" % "2.0.7")
//sbt-ci-release
addSbtPlugin("com.softwaremill.sbt-softwaremill" % "sbt-softwaremill-publish" % "2.0.7")
//sbt-updates, sbt-dependency-check
addSbtPlugin("com.softwaremill.sbt-softwaremill" % "sbt-softwaremill-extra" % "2.0.7")

addSbtPlugin("ch.epfl.scala" % "sbt-scalafix" % "0.9.30")

addSbtPlugin("com.codecommit" % "sbt-github-actions" % "0.13.0")

ThisBuild / libraryDependencySchemes += "org.scala-lang.modules" %% "scala-parser-combinators" % "always"

addSbtPlugin("org.scoverage" % "sbt-scoverage" % "1.8.2")

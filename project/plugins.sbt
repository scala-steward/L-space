addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject"      % "1.2.0")
addSbtPlugin("org.portable-scala" % "sbt-scala-native-crossproject" % "1.2.0")
addSbtPlugin("org.scala-js"       % "sbt-scalajs"                   % "1.10.1")
addSbtPlugin("org.scala-native"   % "sbt-scala-native"              % "0.4.5")

addSbtPlugin("com.47deg" % "sbt-microsites" % "1.3.4")

addSbtPlugin("com.github.sbt" % "sbt-native-packager" % "1.9.9")

// addSbtPlugin("com.lightbend.sbt" % "sbt-proguard" % "0.4.0")

addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "1.2.0")

//sbt-scalafmt, sbt-tpolecat
addSbtPlugin("com.softwaremill.sbt-softwaremill" % "sbt-softwaremill-common" % "2.0.9")
//sbt-ci-release
addSbtPlugin("com.softwaremill.sbt-softwaremill" % "sbt-softwaremill-publish" % "2.0.9")
//sbt-updates, sbt-dependency-check
addSbtPlugin("com.softwaremill.sbt-softwaremill" % "sbt-softwaremill-extra" % "2.0.9")

addSbtPlugin("ch.epfl.scala" % "sbt-scalafix" % "0.10.1")

// ThisBuild / libraryDependencySchemes += "org.scala-lang.modules" %% "scala-parser-combinators" % "always"

addSbtPlugin("org.scoverage" % "sbt-scoverage" % "2.0.0")

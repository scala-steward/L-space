addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % "1.0.0")
//addSbtPlugin("org.portable-scala" % "sbt-scala-native-crossproject" % "1.0.0")
addSbtPlugin("org.scala-js" % "sbt-scalajs" % "1.5.1")
//addSbtPlugin("org.scala-native" % "sbt-scala-native" % "0.3.9")

addSbtPlugin("com.47deg"        % "sbt-microsites" % "1.3.4")

addSbtPlugin("ch.epfl.scala" % "sbt-bloop" % "1.4.8")

addSbtPlugin("com.typesafe.sbt" % "sbt-native-packager" % "1.8.1")

addSbtPlugin("com.lightbend.sbt" % "sbt-proguard" % "0.4.0")

addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "0.15.0")

//sbt-scalafmt, sbt-tpolecat
addSbtPlugin("com.softwaremill.sbt-softwaremill" % "sbt-softwaremill-common" % "2.0.5")
//sbt-ci-release
addSbtPlugin("com.softwaremill.sbt-softwaremill" % "sbt-softwaremill-publish" % "2.0.5")
//sbt-updates, sbt-dependency-check
addSbtPlugin("com.softwaremill.sbt-softwaremill" % "sbt-softwaremill-extra" % "2.0.5")

addSbtPlugin("org.scoverage" % "sbt-scoverage" % "1.8.0")

addSbtPlugin("ch.epfl.scala" % "sbt-scala3-migrate" % "0.4.5")

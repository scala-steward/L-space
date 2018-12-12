logLevel := Level.Warn

resolvers += "Typesafe repository" at "https://repo.typesafe.com/typesafe/releases/"

addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % "0.6.0")
//addSbtPlugin("org.portable-scala" % "sbt-scala-native-crossproject" % "0.5.0")
addSbtPlugin("org.scala-js" % "sbt-scalajs" % "0.6.26")
//addSbtPlugin("org.scala-native" % "sbt-scala-native" % "0.3.8")

//libraryDependencies += "org.scala-js" %% "scalajs-env-jsdom-nodejs" % "1.0.0-M3"

addSbtPlugin("com.github.gseitz" % "sbt-release" % "1.0.10")

// Microsite (Docs)
addSbtPlugin("com.47deg" % "sbt-microsites" % "0.7.24")
//addSbtPlugin("com.typesafe.sbt" % "sbt-site" % "1.3.1")

// Scala code auto-formatting
addSbtPlugin("com.geirsson" % "sbt-scalafmt" % "1.5.1")

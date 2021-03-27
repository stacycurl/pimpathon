libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value

addSbtPlugin("com.jsuereth" % "sbt-pgp" % "2.0.1")
addSbtPlugin("com.geirsson" % "sbt-ci-release" % "1.5.6")

scalacOptions += "-deprecation"

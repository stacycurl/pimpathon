import sbt._

import sbt.Keys._
//import scoverage.ScoverageKeys._


object PimpathonBuild extends Build {
  lazy val pimpathon = (project in file(".")
    aggregate(core, frills)
    settings(commonSettings: _*)
    settings(publish := (), publishLocal := (), doc <<= version.apply(Documentation.generate))
  )

  lazy val core = (project in file("core")
    settings(commonSettings: _*)
    settings(Publishing.settings: _*)
  )

  lazy val frills = (project in file("frills")
    dependsOn core % "compile -> compile; test -> test"
    settings(commonSettings: _*)
    settings(Publishing.settings: _*)
    settings(libraryDependencies ++= Seq(
      "com.github.julien-truffaut" %% "monocle-core"     % "1.2.2"  % "provided",
      "io.argonaut"                %% "argonaut"         % "6.2-M2" % "provided",
      "io.argonaut"                %% "argonaut-monocle" % "6.2-M2" % "provided",
      "org.scalaz"                 %% "scalaz-core"      % "7.2.2"  % "provided"
    ))
  )

  lazy val runAll = TaskKey[Unit]("run-all")

  def runAllIn(config: Configuration) = {
    runAll in config <<= (discoveredMainClasses in config, runner in run, fullClasspath in config, streams) map {
      (classes, runner, cp, s) ⇒ classes.foreach(c ⇒ runner.run(c, Attributed.data(cp), Seq(), s.log))
    }
  }

  def commonSettings = Seq(
    organization := "com.github.stacycurl",
    scalaVersion := "2.11.7",
    maxErrors := 1,
    parallelExecution in Test := true,
    scalacOptions := Seq("-feature", "-Xfatal-warnings", "-deprecation", "-unchecked"),
    resolvers += "Stacy Curl's repo" at "http://dl.bintray.com/stacycurl/repo/",
    libraryDependencies ++= Seq(
      "com.novocode"          % "junit-interface" % "0.11"   % "test",
      "com.github.stacycurl" %% "delta-matchers"  % "1.0.17" % "test",
      "org.scala-lang"        % "scala-compiler"  % scalaVersion.value
    )
//    coverageEnabled := true,
//    coverageMinimum := 100,
//    coverageHighlighting := true,
//    coverageFailOnMinimum := true
  )
}
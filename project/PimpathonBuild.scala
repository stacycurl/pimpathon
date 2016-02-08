import sbt._

import net.virtualvoid.sbt.graph.{Plugin ⇒ GraphPlugin}

import sbt.Keys._
import scoverage.ScoverageKeys._


object PimpathonBuild extends Build {
  lazy val pimpathon = (project in file(".")
    aggregate(core, frills)
    settings(commonSettings: _*)
    settings(publish := (), publishLocal := (), doc := Documentation.generate)
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
      "io.argonaut" %% "argonaut"    % "6.1" % "provided",
      "org.scalaz"  %% "scalaz-core" % "7.1.1"  % "provided"
    ))
  )

  lazy val runAll = TaskKey[Unit]("run-all")

  def runAllIn(config: Configuration) = {
    runAll in config <<= (discoveredMainClasses in config, runner in run, fullClasspath in config, streams) map {
      (classes, runner, cp, s) ⇒ classes.foreach(c ⇒ runner.run(c, Attributed.data(cp), Seq(), s.log))
    }
  }

  def commonSettings = GraphPlugin.graphSettings ++ Seq(
    organization := "com.github.stacycurl",
    scalaVersion := "2.11.7",
    maxErrors := 1,
    parallelExecution in Test := true,
    scalacOptions := Seq("-feature", "-Xfatal-warnings", "-deprecation", "-unchecked"),
    libraryDependencies += "com.novocode"  % "junit-interface" % "0.11"  % "test",
    coverageEnabled := true,
    coverageMinimum := 100,
    coverageHighlighting := true,
    coverageFailOnMinimum := true
  )
}
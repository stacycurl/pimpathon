import sbt._

import net.virtualvoid.sbt.graph.{Plugin ⇒ GraphPlugin}
import org.scalastyle.sbt.ScalastylePlugin

import sbt.Keys._
import scoverage.ScoverageSbtPlugin._
import scoverage.ScoverageSbtPlugin.ScoverageKeys._


object PimpathonBuild extends Build {
  lazy val pimpathon = (project in file(".")
    aggregate(core, frills)
    settings(commonSettings: _*)
    settings(publish := (), publishLocal := ())
  )

  lazy val core = (project in file("core")
    settings(commonSettings: _*)
    settings(Publishing.settings: _*)
  )

  lazy val frills = (project in file("frills")
    dependsOn core
    settings(commonSettings: _*)
    settings(Publishing.settings: _*)
    settings(libraryDependencies ++= Seq(
      "io.argonaut" %% "argonaut"    % "6.1-M4" % "provided",
      "org.scalaz"  %% "scalaz-core" % "7.1.0"  % "provided"
    ))
  )

  lazy val runAll = TaskKey[Unit]("run-all")

  def runAllIn(config: Configuration) = {
    runAll in config <<= (discoveredMainClasses in config, runner in run, fullClasspath in config, streams) map {
      (classes, runner, cp, s) ⇒ classes.foreach(c ⇒ runner.run(c, Attributed.data(cp), Seq(), s.log))
    }
  }

  def commonSettings = GraphPlugin.graphSettings ++ CoverallsPlugin.coverallsSettings ++
  // uncomment when you want to reset the formatting of the project
  // SbtScalariform.scalariformSettings ++
  ScalastylePlugin.Settings ++ instrumentSettings ++ Seq(
    organization := "com.github.stacycurl",
    scalaVersion := "2.11.2",
    maxErrors := 1,
    parallelExecution in Test := true,
    parallelExecution in ScoverageTest := false,
    scalacOptions := Seq(
      "-feature",
      "-Xfatal-warnings",
      "-deprecation",
      "-unchecked"
    ),
    libraryDependencies += "com.novocode"  % "junit-interface" % "0.11"  % "test",
    initialCommands in console := """import pimpathon._""",
    minimumCoverage := 100,
    highlighting := true,
    failOnMinimumCoverage := true
  )
}
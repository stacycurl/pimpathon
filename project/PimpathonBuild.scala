import sbt._

import net.virtualvoid.sbt.graph.{Plugin => GraphPlugin}
import org.scalastyle.sbt.ScalastylePlugin

import sbt.Keys._
import scoverage.ScoverageSbtPlugin._
import scoverage.ScoverageSbtPlugin.ScoverageKeys._


object PimpathonBuild extends Build {
  lazy val pimpathon = Project(
    id = "pimpathon-parent",
    base = file("."),
    settings = commonSettings,
    aggregate = Seq(pimpathonCore, pimpathonFrills)
  )

  lazy val pimpathonCore = Project(
    id = "pimpathon-core",
    base = file("core"),
    settings = commonSettings ++ Publishing.settings
  )

  lazy val pimpathonFrills = Project(
    id = "pimpathon-frills",
    base = file("frills"),
    dependencies = Seq(pimpathonCore),
    settings = commonSettings ++ Publishing.settings ++ Seq(
      libraryDependencies += "io.argonaut" %% "argonaut" % "6.1-M4" % "provided"
    )
  )

  lazy val runAll = TaskKey[Unit]("run-all")

  def runAllIn(config: Configuration) = {
    runAll in config <<= (discoveredMainClasses in config, runner in run, fullClasspath in config, streams) map {
      (classes, runner, cp, s) => classes.foreach(c => runner.run(c, Attributed.data(cp), Seq(), s.log))
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
    scalacOptions := Seq(
      "-feature",
      "-language:higherKinds",
      "-language:implicitConversions",
      "-language:reflectiveCalls",
      "-Xfatal-warnings",
      "-deprecation",
      "-unchecked"
    ),
    libraryDependencies ++= Seq(
      "com.novocode"  % "junit-interface" % "0.11"  % "test",
      "org.scalaz"   %% "scalaz-core"     % "7.1.0" % "test"
    ),
    initialCommands in console := """import pimpathon._""",
    minimumCoverage := 100,
    highlighting := true,
    failOnMinimumCoverage := true
  )
}

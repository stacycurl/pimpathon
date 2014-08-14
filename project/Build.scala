import sbt._

import com.typesafe.sbt.SbtScalariform.scalariformSettings
import net.virtualvoid.sbt.graph.Plugin.graphSettings
import org.scalastyle.sbt.ScalastylePlugin.{ Settings => scalaStyleSettings }

import sbt.Keys._
import scoverage.ScoverageSbtPlugin._
import scoverage.ScoverageSbtPlugin.ScoverageKeys._


object PimpathonBuild extends Build {
  lazy val pimpathon = Project(
    id = "pimpathon-parent",
    base = file("."),
    settings = commonSettings,
    aggregate = Seq(pimpathonCore, pimpathonExamples)
  )

  lazy val pimpathonCore = Project(
    id = "pimpathon-core",
    base = file("core"),
    settings = commonSettings ++ Publishing.settings
  )

  lazy val pimpathonExamples = Project(
    id = "pimpathon-examples",
    base = file("examples"),
    dependencies = Seq(pimpathonCore),
    settings = commonSettings ++ Seq(
      runAllIn(Compile)
    )
  )

  lazy val runAll = TaskKey[Unit]("run-all")

  def runAllIn(config: Configuration) = {
    runAll in config <<= (discoveredMainClasses in config, runner in run, fullClasspath in config, streams) map {
      (classes, runner, cp, s) => classes.foreach(c => runner.run(c, Attributed.data(cp), Seq(), s.log))
    }
  }

  def commonSettings = graphSettings ++
  // uncomment when you want to reset the formatting of the project
  // scalariformSettings ++
  scalaStyleSettings ++ instrumentSettings ++ Seq(
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
    libraryDependencies += "com.novocode"  % "junit-interface" % "0.10"  % "test",
    libraryDependencies += "org.scalaz"   %% "scalaz-core"     % "7.1.0" % "test",
    initialCommands in console := """import pimpathon._""",
    minimumCoverage := 100,
    highlighting := true,
    failOnMinimumCoverage := true
  )
}

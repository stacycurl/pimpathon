import sbt._

import com.typesafe.sbt.SbtScalariform.scalariformSettings
import net.virtualvoid.sbt.graph.Plugin.graphSettings
import org.scalastyle.sbt.ScalastylePlugin.{ Settings => scalaStyleSettings }

import sbt.Keys._


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
  scalaStyleSettings ++ Seq(
    organization := "com.github.stacycurl",
    scalaVersion := "2.9.2",
    maxErrors := 1,
    parallelExecution in Test := true,
    scalacOptions := Seq(
      "-Xfatal-warnings",
      "-deprecation",
      "-unchecked"
    ),
    libraryDependencies += "com.novocode"  % "junit-interface" % "0.11"  % "test",
    libraryDependencies += "org.scalaz"   %% "scalaz-core"     % "7.0.0" % "test",
    initialCommands in console := """import pimpathon._"""
  )
}

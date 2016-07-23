import sbt._

import sbt.Keys._
//import scoverage.ScoverageKeys._


object PimpathonBuild extends Build {
  lazy val pimpathon = (project in file(".")
    settings(
      organization              := "com.github.stacycurl",
      scalaVersion              := "2.11.7",
      scalacOptions             := Seq("-feature", "-Xfatal-warnings", "-deprecation", "-unchecked", "-target:jvm-1.8"),
      maxErrors                 := 1,
      parallelExecution in Test := true
      //    coverageEnabled := true,
      //    coverageMinimum := 100,
      //    coverageHighlighting := true,
      //    coverageFailOnMinimum := true
    )
    settings(Publishing.settings: _*)
    settings(doc <<= version.apply(Documentation.generate))
    settings(libraryDependencies ++= Seq(
      "org.scala-lang"             %  "scala-compiler"   % scalaVersion.value,
      "com.github.julien-truffaut" %% "monocle-core"     % "1.2.2"  % "provided",
      "io.argonaut"                %% "argonaut"         % "6.2-M2" % "provided",
      "io.argonaut"                %% "argonaut-monocle" % "6.2-M2" % "provided",
      "org.scalaz"                 %% "scalaz-core"      % "7.2.2"  % "provided",
      "io.gatling"                 %% "jsonpath"         % "0.6.7"  % "provided",
      "com.novocode"               %  "junit-interface"  % "0.11"   % "test",
      "com.github.stacycurl"       %% "delta-matchers"   % "1.0.19" % "test"
    )
  ))
}

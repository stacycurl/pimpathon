import com.jsuereth.sbtpgp.SbtPgp.autoImport._
import sbt.Keys._
import sbt._

inThisBuild(List(
  organization := "com.github.stacycurl",
  homepage     := Some(url("https://github.com/stacycurl/pimpathon")),
  licenses     := List("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")),
  developers   := List(
    Developer("stacycurl", "Stacy Curl", "stacy.curl@gmail.com", url("https://github.com/stacycurl"))
  ),
  usePgpKeyHex("pimpathon ci")
))

lazy val pimpathon: Project = (project in file(".")
  settings(
    organization              := "com.github.stacycurl",
    scalaVersion              := "2.12.12",
    scalacOptions             := Seq("-feature", "-Xfatal-warnings", "-deprecation", "-unchecked", "-target:jvm-1.8"),
    javacOptions              := Seq("-source", "1.8", "-target", "1.8", "-Xlint"),
    maxErrors                 := 1,
    parallelExecution in Test := true,
    resolvers += "Stacy Curl's repo" at "https://dl.bintray.com/stacycurl/repo/",
    resolvers += "jcenter"           at "https://jcenter.bintray.com",
    libraryDependencies ++= scalaVersion(dependencies("2.12.12" → List(
      "org.scala-lang"             % "scala-compiler"    % "2.12.0" exclude("org.scala-lang.modules", "scala-xml_2.12"),
      "org.scala-lang"             % "scala-library"     % "2.12.0"    % "test",
      "com.github.julien-truffaut" %% "monocle-core"     % "1.3.2"     % "provided",
      "io.argonaut"                %% "argonaut"         % "6.2-RC1"   % "provided",
      "io.argonaut"                %% "argonaut-monocle" % "6.2-RC1"   % "provided",
      "org.scalaz"                 %% "scalaz-core"      % "7.3.0-M6"  % "provided",
      "io.gatling"                 %% "jsonpath"         % "0.6.8"     % "provided",
      "com.novocode"               % "junit-interface"   % "0.11"      % "test",
      "com.github.stacycurl"       %% "delta-matchers"   % "1.1.0"     % "test"
    ))).value,
    doc := version.apply(Documentation.generate).value,
    initialize := {
      val _ = initialize.value
      require(sys.props("java.specification.version") == "1.8", "Java 8 is required for this project.")
    },
    addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.3")
  )
)

def dependencies(modules: (String, List[ModuleID])*)(version: String): List[sbt.ModuleID] = modules.toMap.apply(version)
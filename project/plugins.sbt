def plugin(moduleID: ModuleID) = // Workaround http://github.com/sbt/sbt/issues/1439
  Defaults.sbtPluginExtra(moduleID, "0.13", "2.10") excludeAll ExclusionRule("org.scala-lang")

resolvers ++= Seq("bintray-sbt-plugin-releases" at "http://dl.bintray.com/content/sbt/sbt-plugin-releases")

resolvers += Resolver.url("fix-sbt-plugin-releases", url("http://dl.bintray.com/sbt/sbt-plugin-releases"))(Resolver.ivyStylePatterns)

libraryDependencies ++= Seq(
  plugin("com.timushev.sbt" %  "sbt-updates"           % "0.1.7"),
  plugin("net.virtual-void" %  "sbt-dependency-graph"  % "0.7.4"),
  plugin("org.scoverage"    %% "sbt-scoverage"         % "1.3.5"),
  plugin("me.lessis"        %  "bintray-sbt"           % "0.1.2")
  // https://github.com/typelevel/wartremover/issues/108
  //plugin("org.brianmckenna" %% "wartremover" % "0.9")
)

scalacOptions += "-deprecation"

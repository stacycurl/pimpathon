/*
 * Copyright (c) 2011 Alois Cochard
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
import bintray.Plugin.bintrayPublishSettings
import scala.util.Properties

import bintray.Keys._
import sbt._
import Keys._

object Publishing extends Sonatype {
  def projectUrl    = "https://github.com/stacycurl"
  def developerId   = "stacycurl"
  def developerName = "Stacy Curl"
}

/* Sonatype Publishing */

// Please note it's necessary to:
//
// 1. PGP sign artifacts using the following plugin:
// http://www.scala-sbt.org/xsbt-gpg-plugin/
//
// 2. Add your sonatype credentials in '~/.ivy2/.credentials' using the following format:
// realm=Sonatype Nexus Repository Manager
// host=oss.sonatype.org
// user=<username>
// password=<password>

abstract class Sonatype {
  val ossSnapshots = "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/"
  val ossStaging   = "Sonatype OSS Staging" at "https://oss.sonatype.org/service/local/staging/deploy/maven2/"

  def projectUrl: String
  def developerId: String
  def developerName: String

  def scmUrl              = projectUrl
  def scmConnection       = "scm:git:" + scmUrl

  def generatePomExtra(scalaVersion: String): xml.NodeSeq = {
    <url>{ projectUrl }</url>
    <scm>
      <url>{ scmUrl }</url>
      <connection>{ scmConnection }</connection>
    </scm>
    <developers>
      <developer>
        <id>{ developerId }</id>
        <name>{ developerName }</name>
      </developer>
    </developers>
  }

  // travis ⇒ bintray, local ⇒ sonatype
  def settings: Seq[Setting[_]] = commonSettings ++ (Properties.envOrNone("BINTRAY_API_KEY") match {
    case Some(apiKey) ⇒ bintrayPublishSettings ++ Seq(
      repository in bintray := "repo",
      bintrayOrganization in bintray := None
    )
    case None ⇒ Seq(
      credentialsSetting,
      publishTo <<= version((v: String) ⇒ Some(if (v.trim endsWith "SNAPSHOT") ossSnapshots else ossStaging)),
      pomIncludeRepository := (_ ⇒ false),
      pomExtra <<= scalaVersion(generatePomExtra)
    )
  })

  private def commonSettings: Seq[Setting[_]] = Seq(
    moduleName := "pimpathon",
    publishMavenStyle := true,
    publishArtifact in Test := false,
    licenses +=("Apache-2.0", url("http://www.apache.org/licenses/LICENSE-2.0.html"))
  )

  lazy val credentialsSetting = credentials += {
    Seq("SONATYPE_USERNAME", "SONATYPE_PASSWORD").map(k ⇒ sys.env.get(k)) match {
      case Seq(Some(user), Some(pass)) ⇒
        Credentials("Sonatype Nexus Repository Manager", "oss.sonatype.org", user, pass)
      case _                           ⇒
        Credentials(Path.userHome / ".ivy2" / ".credentials")
    }
  }
}
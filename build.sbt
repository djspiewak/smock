/*
 * Copyright 2017 Daniel Spiewak
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

val releaseVersions = List(
  "3.8.4" -> "2.11.12",

  "3.9.1" -> "2.11.12",
  "3.9.1" -> "2.12.8",

  "4.0.2" -> "2.11.12",
  "4.0.2" -> "2.12.8",

  "4.2.0" -> "2.11.12",
  "4.2.0" -> "2.12.8",

  "4.3.6" -> "2.11.12",
  "4.3.6" -> "2.12.8"
)

addCommandAlias("release", releaseCommand)
addCommandAlias("testAll", testAllCommand)

organization := "com.codecommit"

name := "smock"

lazy val specs2Version = settingKey[String]("specs2 version")

specs2Version := {
  if (isTravisBuild.value)
    sys.env("SPECS2_VERSION")
  else
    "4.3.6"   // default to the most current version
}

val scalazVersion = "7.2.27"

libraryDependencies ++= Seq(
  "org.scalaz" %% "scalaz-core" % scalazVersion,
  "org.scalaz" %% "scalaz-effect" % scalazVersion % Test,
  "org.specs2" %% "specs2-core" % specs2Version.value)

/*
 * Compatibility version.  Use this to declare what version with
 * which `master` remains in compatibility.  This is literally
 * backwards from how -SNAPSHOT versioning works, but it avoids
 * the need to pre-declare (before work is done) what kind of
 * compatibility properties the next version will have (i.e. major
 * or minor bump).
 *
 * As an example, the builds of a project might go something like
 * this:
 *
 * - 0.1-hash1
 * - 0.1-hash2
 * - 0.1-hash3
 * - 0.1
 * - 0.1-hash1
 * - 0.2-hash2
 * - 0.2
 * - 0.2-hash1
 * - 0.2-hash2
 * - 1.0-hash3
 * - 1.0-hash4
 * - 1.0
 *
 * The value of BaseVersion starts at 0.1 and remains there until
 * compatibility with the 0.1 line is lost, which happens just
 * prior to the release of 0.2.  Then the base version again remains
 * 0.2-compatible until that compatibility is broken, with the major
 * version bump of 1.0.  Again, this is all to avoid pre-committing
 * to a major/minor bump before the work is done (see: Scala 2.8).
 */
val BaseVersion = "0.4"

ThisBuild / organizationName := "Daniel Spiewak"
ThisBuild / startYear := Some(2018)
ThisBuild / licenses += ("Apache-2.0", url("http://www.apache.org/licenses/"))

ThisBuild / developers := List(
  Developer("djspiewak", "Daniel Spiewak", "djspiewak@gmail.com", url("https://github.com/djspiewak")))

ThisBuild / versionScheme := Some("pvp")

homepage := Some(url("https://github.com/djspiewak/smock"))

scmInfo := Some(
  ScmInfo(
    url("https://github.com/djspiewak/smock"),
    "scm:git@github.com:djspiewak/smock.git"))

publishTo := sonatypePublishToBundle.value

/***********************************************************************\
                      Boilerplate below these lines
\***********************************************************************/

enablePlugins(AutomateHeaderPlugin)

coursierUseSbtCredentials := true
coursierChecksums := Nil      // workaround for nexus sync bugs

addCompilerPlugin("org.spire-math" % "kind-projector" % "0.9.7" cross CrossVersion.binary)

// Adapted from Rob Norris' post at https://tpolecat.github.io/2014/04/11/scalac-flags.html
scalacOptions ++= Seq(
  "-language:_",
  "-deprecation",
  "-encoding", "UTF-8", // yes, this is 2 args
  "-feature",
  "-unchecked",
  "-Xfatal-warnings",
  "-Xlint",
  "-Yno-adapted-args",
  "-Ywarn-dead-code"
)

scalacOptions ++= {
  CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((2, major)) if major >= 11 => Seq(
      "-Ywarn-unused-import", // Not available in 2.10
      "-Ywarn-numeric-widen" // In 2.10 this produces a some strange spurious error
    )
    case _ => Seq.empty
  }
}

scalacOptions ++= {
  CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((2, major)) if major >= 12 || scalaVersion.value == "2.11.12" =>
      Seq("-Ypartial-unification")

    case _ => Seq.empty
  }
}

Test / scalacOptions += "-Yrangepos"

Compile / console / scalacOptions ~= (_ filterNot (Set("-Xfatal-warnings", "-Ywarn-unused-import").contains))

Test / console / scalacOptions := (Compile / console / scalacOptions).value

libraryDependencies ++= {
  scalaVersion.value match {
    case "2.11.8" => Seq(compilerPlugin("com.milessabin" % "si2712fix-plugin" % "1.2.0" cross CrossVersion.full))
    case "2.10.6" => Seq(compilerPlugin("com.milessabin" % "si2712fix-plugin" % "1.2.0" cross CrossVersion.full))
    case _ => Seq.empty
  }
}

enablePlugins(GitVersioning)

val ReleaseTag = """^v([\d\.]+)$""".r

git.baseVersion := BaseVersion + s"-specs2-${specs2Version.value}"

git.gitTagToVersionNumber := {
  case ReleaseTag(version) => Some(version + s"-specs2-${specs2Version.value}")
  case _ => None
}

git.formattedShaVersion := {
  val suffix = git.makeUncommittedSignifierSuffix(git.gitUncommittedChanges.value, git.uncommittedSignifier.value)

  git.gitHeadCommit.value map { _.substring(0, 7) } map { sha =>
    git.baseVersion.value + "-" + sha + suffix
  }
}


def releaseCommand: String = {
  ";reload;" + releaseVersions.map { case (specs2Version, scalaVersion) =>
    setVersions(specs2Version, scalaVersion) + ";publish"
  }.mkString(";") + ";sonatypeBundleRelease"
}

def testAllCommand: String = {
  releaseVersions.map { case (specs2Version, scalaVersion) =>
    setVersions(specs2Version, scalaVersion) + ";test"
  }.mkString(";", ";", "")
}

def setVersions(specsVersion: String, scalaVersion: String) = {
  s"""set specs2Version := "$specsVersion"; ++ "$scalaVersion""""
}

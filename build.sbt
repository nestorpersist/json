import sbtassembly.Plugin._

import AssemblyKeys._

name := "persist-json"

version := "0.13"

scalaVersion := "2.10.1"

resolvers += "typesafe0" at "http://repo.typesafe.com/typesafe/releases"

libraryDependencies ++=Seq(
        "org.scala-lang" % "scala-reflect" % "2.10.1",
        "org.scalatest" %% "scalatest" % "1.9.1" % "test",
        "junit" % "junit" % "4.10" % "test"
)

assemblySettings

test in assembly := {}

jarName in assembly := "persist-json-0.1.jar"

publishTo <<= version { v: String =>
  val nexus = "https://oss.sonatype.org/"
  if (v.trim.endsWith("SNAPSHOT"))
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases" at nexus + "service/local/staging/deploy/maven2")
}

organization := "com.persist"

publishMavenStyle := true

publishArtifact in Test := false

pomIncludeRepository := { _ => false }

pomExtra := (
  <url>https://github.com/nestorpersist/json</url>
  <licenses>
    <license>
      <name>Apache 2</name>
      <url>http://www.apache.org/licenses/LICENSE-2.0.html</url>
      <distribution>repo</distribution>
    </license>
  </licenses>
  <scm>
    <url>git@github.com:nestorpersist/json.git</url>
    <connection>scm:git@github.com:nestorpersist/json.git</connection>
  </scm>
  <developers>
    <developer>
      <id>johnnestor</id>
      <name>John Nestor</name>
      <email>nestor@persist.com</email>
      <url>http://http://www.persist.com</url>
    </developer>
  </developers>
)



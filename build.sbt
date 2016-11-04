name := "persist-json"

organization := "com.persist"

version := "1.2.0"

scalaVersion := "2.12.0"

scalacOptions ++= Seq("-deprecation", "-unchecked")

viewSettings

libraryDependencies ++= Seq(
        "org.scala-lang" % "scala-reflect" % scalaVersion.value,
        "com.chuusai" %% "shapeless" % "2.3.2",
        "org.scalatest" %% "scalatest" % "3.0.0" % "test",
        "org.specs2" %% "specs2-core" % "3.8.6" % "test",
        "junit" % "junit" % "4.12" % "test"
)

publishTo <<= version { v: String =>
  val nexus = "https://oss.sonatype.org/"
  if (v.trim.endsWith("SNAPSHOT"))
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases" at nexus + "service/local/staging/deploy/maven2")
}

useGpg := true

publishMavenStyle := true

publishArtifact in Test := false

pomIncludeRepository := { _ => false }

licenses := Seq("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0.html"))

homepage := Some(url("https://github.com/nestorpersist/json"))

scmInfo := Some(ScmInfo(url("https://github.com/nestorpersist/json"), "scm:git@github.com:nestorpersist/json.git"))

pomExtra := (
  <developers>
    <developer>
      <id>johnnestor</id>
      <name>John Nestor</name>
      <email>nestor@persist.com</email>
      <url>http://http://www.persist.com</url>
    </developer>
    <developer>
      <id>jedesah</id>
      <name>Jean-Remi Desjardins</name>
      <email>jeanremi.desjardins@gmail.com</email>
      <url>https://github.com/jedesah</url>
    </developer>
  </developers>
)
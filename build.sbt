name := "persist-json"

organization := "com.persist"

version := "0.19"

scalaVersion := "2.11.1"

libraryDependencies ++= Seq(
        "org.scala-lang" % "scala-reflect" % scalaVersion.value,
        "com.chuusai" %% "shapeless" % "2.0.0",
        "org.scalatest" %% "scalatest" % "2.2.1" % "test",
        "org.specs2" %% "specs2" % "2.4.1" % "test",
        "junit" % "junit" % "4.11" % "test"
)

publishTo <<= version { v: String =>
  val nexus = "https://oss.sonatype.org/"
  if (v.trim.endsWith("SNAPSHOT"))
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases" at nexus + "service/local/staging/deploy/maven2")
}

publishMavenStyle := true

publishArtifact in Test := false

pomIncludeRepository := { _ => false }

licenses := Seq("Apache 2" -> url("http://www.apache.org/licenses/LICENSE-2.0.html"))

homepage := Some(url("https://github.com/nestorpersist/json"))

pomExtra := (
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
    <developer>
      <id>jedesah</id>
      <name>Jean-Remi Desjardins</name>
      <email>jeanremi.desjardins@gmail.com</email>
      <url>https://github.com/jedesah</url>
    </developer>
  </developers>
)
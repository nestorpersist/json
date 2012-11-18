import sbtassembly.Plugin._

import AssemblyKeys._

name := "persist-json"

version := "0.5"

scalaVersion := "2.9.1"

resolvers += "typesafe0" at "http://repo.typesafe.com/typesafe/releases"

libraryDependencies ++=Seq(
        "org.scalatest" %% "scalatest" % "1.7.2",
        "junit" % "junit" % "4.10" % "test"
)

assemblySettings

test in assembly := {}

jarName in assembly := "persist-json-0.1.jar"



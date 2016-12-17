

lazy val commonSettings = Seq(
  organization := "nl.about42",
  version := "0.0.1-SNAPSHOT",
  scalaVersion := "2.11.1"
)

name := "azpc_poly"
version := "1.0"

val akkaVersion = "2.4.14"
val akkaHttpVersion = "10.0.0"

val dependencies: Seq[ModuleID] = Seq(
  "com.typesafe.akka" % "akka-actor_2.11" % akkaVersion,
  "com.typesafe.akka" % "akka-http_2.11" % akkaHttpVersion
)

lazy val frontend = project

lazy val generator = project.
  settings( commonSettings: _*).
  settings( name := "PolyGenerator").
  settings( libraryDependencies ++= dependencies)

lazy val validator = project.
  settings( commonSettings: _*).
  settings( name := "PolyValidator").
  settings( libraryDependencies ++= dependencies)

lazy val root = project.in(file(".")).aggregate(frontend, generator, validator)

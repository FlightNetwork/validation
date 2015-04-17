organization := "com.flightnetwork"

name := "validation"

version := "0.0.2"

scalaVersion := "2.11.6"

scalacOptions ++= Seq("-deprecation", "-feature")

libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-reflect" % scalaVersion.value,
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.3",
  "org.specs2" %% "specs2" % "2.3.12" % "test"
)

credentials += Credentials("Artifactory Realm", "lib.fnlocal.net", System.getProperty("artifactory.user"), System.getProperty("artifactory.password"))

val buildTime = new java.util.Date().getTime

publishTo := Some("Artifactory Realm" at "http://lib.fnlocal.net/artifactory/libs-snapshot-local;build.timestamp=" + buildTime)
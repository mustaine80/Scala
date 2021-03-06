name := "localDeploy"

version := "1.0"

organization := "lignex1.com"

scalaVersion := "2.12.2"

enablePlugins(JavaAppPackaging)

scriptClasspath += "../conf"

lazy val akkaVersion = "2.5.7"

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-actor" % akkaVersion,
  "com.typesafe.akka" %% "akka-remote" % akkaVersion,
  "com.typesafe.akka" %% "akka-testkit" % akkaVersion,
  "com.typesafe.akka" %% "akka-multi-node-testkit" % akkaVersion,
  "org.scalatest" %% "scalatest" % "3.0.1" % "test",
  "io.suzaku" %% "boopickle" % "1.2.6"
)

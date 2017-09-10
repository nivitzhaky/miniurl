name := """miniurl"""

version := "1.0-SNAPSHOT"

lazy val root = (project in file(".")).enablePlugins(PlayScala)


scalaVersion := "2.11.6"
lazy val mongoVersion = "3.1.0"
lazy val json4sVersion = "3.3.0"


scalacOptions ++= Seq("-feature", "-language:implicitConversions", "-language:postfixOps")

libraryDependencies ++= Seq(
  jdbc,
  cache,
  ws,
  specs2 % Test,
	"org.specs2" %% "specs2-matcher-extra" % "3.6" % Test,
  "org.specs2" %% "specs2-core" % "3.6" % "test",
  "com.github.tototoshi" %% "play-json4s-native" % "0.4.2",
  "org.mongodb" %% "casbah" % mongoVersion,
  "org.json4s" %% "json4s-native" % json4sVersion,
  "org.json4s" %% "json4s-ext" % json4sVersion,
  "org.json4s" %% "json4s-mongo" % json4sVersion ,
  "com.typesafe.scala-logging" % "scala-logging_2.11" % "3.5.0",
  "org.scalacheck" %% "scalacheck" % "1.13.4" % "test",
  "org.scalatest" %% "scalatest" % "3.0.1" % "test"

)

resolvers += "scalaz-bintray" at "http://dl.bintray.com/scalaz/releases"

// Play provides two styles of routers, one expects its actions to be injected, the
// other, legacy style, accesses its actions statically.
routesGenerator := InjectedRoutesGenerator

scalariformSettings


fork in run := true

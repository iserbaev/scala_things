name := """scala sbt"""

version := "1.0"

scalaVersion := "2.12.0"

lazy val akkaVersion = "2.4.12"

libraryDependencies ++= Seq(
    "com.typesafe.akka" %% "akka-actor" % akkaVersion,
    "com.typesafe.akka" %% "akka-testkit" % akkaVersion,
    "org.scalatest" %% "scalatest" % "3.0.0" % "test",
    "junit" % "junit" % "4.12" % "test",
    "com.novocode" % "junit-interface" % "0.11" % "test"
)

testOptions += Tests.Argument(TestFrameworks.JUnit, "-v")

lazy val actorLearn = (project in file("actorLearn"))

lazy val scalaLearn = (project in file("scalaLearn"))

lazy val coursera = (project in file("coursera"))

lazy val root = (project in file("."))
  .aggregate(actorLearn, scalaLearn, coursera)
  .dependsOn(actorLearn, scalaLearn, coursera)

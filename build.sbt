name := """scala sbt"""

version := "1.0"

scalaVersion := "2.12.0"

libraryDependencies ++= Seq(
    "org.scalatest" %% "scalatest" % "3.0.0" % "test",
    "junit" % "junit" % "4.12" % "test",
    "com.novocode" % "junit-interface" % "0.11" % "test"
)

testOptions += Tests.Argument(TestFrameworks.JUnit, "-v")

lazy val scalaLearn = project in file("scalaLearn")

lazy val coursera = project in file("coursera")

lazy val sparkLearn = project in file("sparkLearn")

lazy val root = (project in file("."))
  .aggregate(scalaLearn, coursera)
  .dependsOn(scalaLearn, coursera)

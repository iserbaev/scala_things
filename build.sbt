import Deps._
import Settings._

name := """scala learn"""

lazy val scalaLearn = project in file("scalaLearn")

lazy val coursera = project in file("coursera")

lazy val algorithms = project
  .in(file("algorithms"))
  .settings(commonSettings2_12())
  .settings(
    version := "0.1",
    name    := "algorithms",
    libraryDependencies ++= algsProjectDeps
  )

lazy val root = (project in file("."))
  .aggregate(scalaLearn, coursera, algorithms)

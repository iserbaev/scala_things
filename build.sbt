import Deps._
import org.typelevel.scalacoptions.ScalacOptions

lazy val coursera = project in file("coursera")

lazy val commonSettings = Seq(
  tpolecatScalacOptions ++= Set(
    ScalacOptions.release("11"),
    ScalacOptions.warnNonUnitStatement,
  )
)

inThisBuild(
  Seq(
    addCompilerPlugin(Deps.sbtBetterMonadicFor),
    addCompilerPlugin(Deps.sbtKindProjector.cross(CrossVersion.full)),
    testFrameworks += new TestFramework("weaver.framework.CatsEffect"),
    scalafixDependencies ++= Deps.sbtScalafix.value,
  )
)

lazy val algorithms = project
  .in(file("algorithms"))
  .settings(
    scalaVersion := Deps.Versions.Scala,
    version      := "0.1",
    name         := "algorithms",
    libraryDependencies ++= algsProjectDeps,
    testFrameworks += new TestFramework("weaver.framework.CatsEffect")
  )

lazy val sparkStepik = project
  .in(file("spark_stepik"))
  .settings(
    scalaVersion := "2.12.15",
    version      := "0.1",
    name         := "spark_stepik",
    libraryDependencies ++= sparkStepikProjectDeps,
    testFrameworks += new TestFramework("weaver.framework.CatsEffect"),
    idePackagePrefix := Some("spark_stepik.cluster")
  )

lazy val sparkStreaming = project
  .in(file("spark_streaming"))
  .settings(
    scalaVersion := Deps.Versions.Scala,
    version      := "0.1",
    name         := "spark_streaming",
    libraryDependencies ++= sparkStepikProjectDeps,
    testFrameworks += new TestFramework("weaver.framework.CatsEffect")
  )

lazy val root = (project in file("."))
  .aggregate(algorithms, sparkStepik, sparkStreaming)
  .settings(name := """scala learn""")

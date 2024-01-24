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
    scalaVersion := Deps.Versions.Scala,
    addCompilerPlugin(Deps.sbtBetterMonadicFor),
    addCompilerPlugin(Deps.sbtKindProjector.cross(CrossVersion.full)),
    testFrameworks += new TestFramework("weaver.framework.CatsEffect"),
    scalafixDependencies ++= Deps.sbtScalafix.value,
  )
)

lazy val algorithms = project
  .in(file("algorithms"))
  .settings(
    version      := "0.1",
    name := "algorithms",
    libraryDependencies ++= algsProjectDeps,
    testFrameworks += new TestFramework("weaver.framework.CatsEffect")
  )

lazy val sparkStepik = project
  .in(file("spark_stepik"))
  .settings(
    version      := "0.1",
    name := "spark_stepik",
    libraryDependencies ++= sparkStepikProjectDeps,
    testFrameworks += new TestFramework("weaver.framework.CatsEffect"),
    idePackagePrefix := Some("spark_stepik.cluster")
  )

lazy val root = (project in file("."))
  .aggregate(algorithms, sparkStepik)
  .settings(name := """scala learn""")

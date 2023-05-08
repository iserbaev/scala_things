import Deps._

inThisBuild(
  Seq(
    name := """scala learn""",
    versionScheme := Some("early-semver"),
  )
)

lazy val coursera = project in file("coursera")

lazy val algorithms = project
  .in(file("algorithms"))
  .settings(
    version := "0.1",
    scalaVersion  := Deps.Versions.Scala,
    addCompilerPlugin(Deps.sbtBetterMonadicFor),
    addCompilerPlugin(Deps.sbtKindProjector.cross(CrossVersion.full)),
    name    := "algorithms",
    libraryDependencies ++= algsProjectDeps,
    testFrameworks += new TestFramework("weaver.framework.CatsEffect")
  )

lazy val root = (project in file("."))
  .aggregate(coursera, algorithms)

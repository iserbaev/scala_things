import sbt._

object Deps {
  object Versions {
    val Scala       = "2.13.12"
    val cats        = "2.10.0"
    val catsEffect  = "3.5.2"
    val catsTagless = "0.14.0"
    val shapeless   = "2.3.10"

    // Compiler Plugins
    val BetterMonadicFor = "0.3.1"
    val KindProjector = "0.13.2"
    val SemanticDB = "4.8.12"
    val ScalafixTypelevel = "0.2.0"

    val scalaLogging = "3.9.2"
    val logback      = "1.4.5"

    val fs2 = "3.9.2"

    val circe         = "0.14.6"
    val http4sVersion = "0.23.18"

    val doobieVersion = "1.0.0-RC4"

    val fs2_kafka = "3.0.1"

    val spark ="3.5.0"

    val spire  = "0.18.0"
    val breeze = "2.1.0"

    val scalaTest             = "3.0.8"
    val testcontainers        = "0.40.14"
    val kafkaTestContainer    = "1.12.3"
    val postgresTestcontainer = "1.12.3"
    val Weaver                = "0.8.2"

    val quickLens = "1.9.3"
  }

  private lazy val lens = Seq(
    "com.softwaremill.quicklens" %% "quicklens" % Versions.quickLens
  )

  private lazy val logging = Seq(
    "com.typesafe.scala-logging" %% "scala-logging"   % Versions.scalaLogging,
    "ch.qos.logback"              % "logback-core"    % Versions.logback,
    "ch.qos.logback"              % "logback-classic" % Versions.logback
  )

  private lazy val testDeps = Seq(
    "org.scalatest"     %% "scalatest"                      % Versions.scalaTest,
    "com.dimafeng"      %% "testcontainers-scala"           % Versions.testcontainers,
    "com.dimafeng"      %% "testcontainers-scala-scalatest" % Versions.testcontainers,
    "org.testcontainers" % "postgresql"                     % Versions.postgresTestcontainer,
    "org.testcontainers" % "kafka"                          % Versions.kafkaTestContainer
  ).map(_ % Test)

  private lazy val cats = Seq(
    "org.typelevel" %% "cats-core" % Versions.cats,
    "org.typelevel" %% "cats-free" % Versions.cats
  )

  private lazy val catsEffect = Seq(
    "org.typelevel" %% "cats-effect" % Versions.catsEffect
  )
  private lazy val catsTagless = Seq(
    "org.typelevel" %% "cats-tagless-macros" % Versions.catsTagless
  )

  private lazy val circe = Seq(
    "io.circe" %% "circe-core",
    "io.circe" %% "circe-generic",
    "io.circe" %% "circe-parser",
    "io.circe" %% "circe-refined",
    "io.circe" %% "circe-generic-extras"
  ).map(_ % Versions.circe)

  private lazy val conf = Seq(
    "com.typesafe" % "config" % "1.3.4"
  )

  private lazy val shapeless = Seq(
    "com.chuusai" %% "shapeless" % Versions.shapeless
  )

  private lazy val http4s = Seq(
    "org.http4s" %% "http4s-dsl"          % Versions.http4sVersion,
    "org.http4s" %% "http4s-blaze-server" % Versions.http4sVersion,
    "org.http4s" %% "http4s-blaze-client" % Versions.http4sVersion
  )

  private lazy val doobie = Seq(
    "org.tpolecat" %% "doobie-core"     % Versions.doobieVersion,
    "org.tpolecat" %% "doobie-h2"       % Versions.doobieVersion,
    "org.tpolecat" %% "doobie-hikari"   % Versions.doobieVersion, // HikariCP transactor.
    "org.tpolecat" %% "doobie-postgres" % Versions.doobieVersion, // Postgres driver 42.2.8 + type mappings
    "org.tpolecat" %% "doobie-scalatest" % Versions.doobieVersion % "test" // ScalaTest support for typechecking statements.
  )

  private lazy val fs2 = Seq(
    "co.fs2" %% "fs2-core"             % Versions.fs2, // For cats 2 and cats-effect 2
    "co.fs2" %% "fs2-io"               % Versions.fs2, // optional I/O library
    "co.fs2" %% "fs2-reactive-streams" % Versions.fs2  // optional reactive streams interop
  )
  private lazy val fs2_kafka = Seq(
    "com.github.fd4s" %% "fs2-kafka" % Versions.fs2_kafka
  )

  private lazy val spire = Seq(
    "org.typelevel" %% "spire" % Versions.spire
  )

  val weaver = Seq(
    "com.disneystreaming" %% "weaver-cats"       % Versions.Weaver,
    "com.disneystreaming" %% "weaver-discipline" % Versions.Weaver,
    "com.disneystreaming" %% "weaver-scalacheck" % Versions.Weaver,
  )

  val sbtBetterMonadicFor = "com.olegpy"   %% "better-monadic-for" % Versions.BetterMonadicFor
  val sbtKindProjector    = "org.typelevel" % "kind-projector"     % Versions.KindProjector
  val sbtSemanticDB       = "org.scalameta" % "semanticdb-scalac"  % Versions.SemanticDB

  val sbtScalafixTypelevel = Seq(
    "org.typelevel" %% "typelevel-scalafix-cats"        % Versions.ScalafixTypelevel,
    "org.typelevel" %% "typelevel-scalafix-cats-effect" % Versions.ScalafixTypelevel,
    "org.typelevel" %% "typelevel-scalafix-fs2"         % Versions.ScalafixTypelevel
  )

  val sbtScalafix = Def.setting(sbtScalafixTypelevel)

  val breeze = Seq(
    "org.scalanlp" %% "breeze" % Versions.breeze
  )

  val spark = Seq(
    "org.apache.spark" %% "spark-core" % Versions.spark,
    "org.apache.spark" %% "spark-sql" % Versions.spark
  )

  lazy val algsProjectDeps: Seq[ModuleID] =
    breeze ++ logging ++ testDeps ++ cats ++ catsEffect ++ catsTagless ++ conf ++ fs2 ++ shapeless ++ spire ++ lens ++ weaver

  lazy val sparkStepikProjectDeps: Seq[ModuleID] =
    spark ++ logging ++ testDeps ++ cats ++ catsEffect ++ catsTagless ++ conf ++ fs2 ++ weaver
}

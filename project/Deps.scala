import sbt.*

object Deps {
  object Versions {
    val Scala = "2.13.14"

    // Compiler Plugins
    val BetterMonadicFor  = "0.3.1"
    val KindProjector     = "0.13.3"
    val SemanticDB        = "4.8.12"
    val ScalafixTypelevel = "0.2.0"

    val breeze = "2.1.0"
    val spire  = "0.18.0"

    val cats        = "2.10.0"
    val catsEffect  = "3.5.4"
    val catsMtl     = "1.2.1"
    val catsTagless = "0.16.0"

    val derevo = "0.12.5"

    val doobie = "1.0.0-RC5"

    val fs2       = "3.10.2"
    val fs2_kafka = "3.0.1"

    val quickLens = "1.9.3"

    val monocle = "3.0.0"

    val newtype = "0.4.4"

    val refined = "0.9.25"

    val circe         = "0.14.7"
    val http4sVersion = "0.23.27"

    val shapeless = "2.3.10"
    val spark     = "3.5.0"

    val SttpApispec = "0.8.0"
    val SttpClient3 = "3.9.5"
    val SttpModel   = "1.7.10"
    val SttpShared  = "1.3.18"
    val SttpTapir   = "1.10.7"

    val tofu = "0.13.0"

    val scalaLogging = "3.9.2"
    val logback      = "1.4.11"

    val scalaTest             = "3.0.8"
    val testcontainers        = "0.41.3"
    val kafkaTestContainer    = "1.12.3"
    val postgresTestcontainer = "1.12.3"
    val Weaver                = "0.8.4"

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

  private lazy val catsMtl = Seq(
    "org.typelevel" %% "cats-mtl" % Versions.catsMtl
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

  val derevo = Seq(
     "tf.tofu" %% "derevo-cats"% Versions.derevo,
     "tf.tofu" %% "derevo-cats-tagless"% Versions.derevo,
     "tf.tofu" %% "derevo-circe-magnolia" % Versions.derevo,
  )

  private lazy val http4s = Seq(
    "org.http4s" %% "http4s-dsl"          % Versions.http4sVersion,
    "org.http4s" %% "http4s-blaze-server" % Versions.http4sVersion,
    "org.http4s" %% "http4s-blaze-client" % Versions.http4sVersion
  )

  private lazy val doobie = Seq(
    "org.tpolecat" %% "doobie-core"      % Versions.doobie,
    "org.tpolecat" %% "doobie-h2"        % Versions.doobie,
    "org.tpolecat" %% "doobie-hikari"    % Versions.doobie, // HikariCP transactor.
    "org.tpolecat" %% "doobie-postgres"  % Versions.doobie, // Postgres driver 42.2.8 + type mappings
    "org.tpolecat" %% "doobie-scalatest" % Versions.doobie % "test" // ScalaTest support for typechecking statements.
  )

  private lazy val fs2 = Seq(
    "co.fs2" %% "fs2-core"             % Versions.fs2, // For cats 2 and cats-effect 2
    "co.fs2" %% "fs2-io"               % Versions.fs2, // optional I/O library
    "co.fs2" %% "fs2-reactive-streams" % Versions.fs2  // optional reactive streams interop
  )
  private lazy val fs2_kafka = Seq(
    "com.github.fd4s" %% "fs2-kafka" % Versions.fs2_kafka
  )

  val monocle = Seq(
    "dev.optics" %% "monocle-core"  % Versions.monocle,
    "dev.optics" %% "monocle-macro" % Versions.monocle,
  )

  val newtype = Seq(
    "io.estatico" %% "newtype" % Versions.newtype
  )

  val refined = Seq(
    "eu.timepit" %% "refined"      % Versions.refined,
    "eu.timepit" %% "refined-cats" % Versions.refined,
  )

  private lazy val shapeless = Seq(
    "com.chuusai" %% "shapeless" % Versions.shapeless
  )

  private lazy val spire = Seq(
    "org.typelevel" %% "spire" % Versions.spire
  )

  val tofu = Seq(
    "tf.tofu" %% "tofu-core-higher-kind" % Versions.tofu
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
    "org.apache.spark" %% "spark-core"      % Versions.spark,
    "org.apache.spark" %% "spark-sql"       % Versions.spark,
    "org.apache.spark" %% "spark-streaming" % Versions.spark,
  )

  val tapirCore = Seq(
    "com.softwaremill.sttp.model" %% "core"             % Versions.SttpModel,
    "com.softwaremill.sttp.tapir" %% "tapir-core"       % Versions.SttpTapir,
    "com.softwaremill.sttp.tapir" %% "tapir-json-circe" % Versions.SttpTapir,
  )

  val tapirMetrics = Seq("com.softwaremill.sttp.tapir" %% "tapir-prometheus-metrics" % Versions.SttpTapir)

  val tapirServer = Seq(
    "com.softwaremill.sttp.apispec" %% "openapi-model"           % Versions.SttpApispec,
    "com.softwaremill.sttp.shared"  %% "fs2"                     % Versions.SttpShared,
    "com.softwaremill.sttp.tapir"   %% "tapir-http4s-server"     % Versions.SttpTapir,
    "com.softwaremill.sttp.tapir"   %% "tapir-openapi-docs"      % Versions.SttpTapir,
    "com.softwaremill.sttp.tapir"   %% "tapir-server"            % Versions.SttpTapir,
    "com.softwaremill.sttp.tapir"   %% "tapir-swagger-ui"        % Versions.SttpTapir,
    "com.softwaremill.sttp.tapir"   %% "tapir-swagger-ui-bundle" % Versions.SttpTapir,
  )

  lazy val algsProjectDeps: Seq[ModuleID] =
    breeze ++ logging ++ testDeps ++ cats ++ catsEffect ++ catsTagless ++ conf ++ fs2 ++ shapeless ++ spire ++ lens ++ weaver

  lazy val sparkStepikProjectDeps: Seq[ModuleID] =
    spark ++ logging ++ testDeps ++ cats ++ catsEffect ++ catsTagless ++ conf ++ fs2 ++ weaver
}

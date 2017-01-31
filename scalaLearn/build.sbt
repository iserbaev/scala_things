name := "scalaLearn"

version := "1.0"

scalaVersion := "2.12.0"

val akkaVersion = "2.4.12"

val kamonVersion = "0.6.3"

// https://mvnrepository.com/artifact/javax.persistence/persistence-api
libraryDependencies += "javax.persistence" % "persistence-api" % "1.0"

// https://mvnrepository.com/artifact/org.scala-lang/scala-reflect
libraryDependencies += "org.scala-lang" % "scala-reflect" % "2.12.0"

// https://mvnrepository.com/artifact/javax.inject/javax.inject
libraryDependencies += "javax.inject" % "javax.inject" % "1"



libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-actor" % akkaVersion,
  "com.typesafe.akka" %% "akka-testkit" % akkaVersion,
  "org.scalatest" %% "scalatest" % "3.0.0" % "test",
  "junit" % "junit" % "4.12" % "test",
  "com.novocode" % "junit-interface" % "0.11" % "test",
  "joda-time" % "joda-time" % "2.9.4",
  "org.joda" % "joda-convert" % "1.8.1",
  "org.scala-lang.modules" %% "scala-xml" % "1.0.6"
)
    
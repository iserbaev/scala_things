name := "coursera"

version := "1.0"

scalaVersion := "2.12.2"

val akkaVersion = "2.4.12"

val kamonVersion = "0.6.3"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.0.0" % "test",
  "junit" % "junit" % "4.12" % "test",
  "com.novocode" % "junit-interface" % "0.11" % "test",
  "joda-time" % "joda-time" % "2.9.4",
  "org.joda" % "joda-convert" % "1.8.1"
)

resolvers += "Sonatype OSS Snapshots" at
  "https://oss.sonatype.org/content/repositories/releases"

libraryDependencies += "com.storm-enroute" %% "scalameter" % "0.8.2"

testFrameworks += new TestFramework("org.scalameter.ScalaMeterFramework")

parallelExecution in Test := false
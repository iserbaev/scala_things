libraryDependencies += "ch.qos.logback"     % "logback-classic"  % "1.4.5"
libraryDependencies += "org.apache.commons" % "commons-compress" % "1.21"
libraryDependencies += "org.slf4j"          % "slf4j-nop"        % "1.7.21"

addSbtPlugin("ch.epfl.scala"             % "sbt-scalafix"        % "0.9.34")
addSbtPlugin("ch.epfl.scala"             % "sbt-version-policy"  % "2.1.0")
addSbtPlugin("com.eed3si9n"              % "sbt-buildinfo"       % "0.11.0")
addSbtPlugin("com.github.sbt"            % "sbt-release"         % "1.1.0")
addSbtPlugin("com.typesafe"              % "sbt-mima-plugin"     % "1.1.2")
addSbtPlugin("com.github.sbt"            % "sbt-git"             % "2.0.1")
addSbtPlugin("io.github.davidgregory084" % "sbt-tpolecat"        % "0.4.1")
addSbtPlugin("org.typelevel"             % "sbt-fs2-grpc"        % "2.5.5")
addSbtPlugin("org.scalameta"             % "sbt-scalafmt"        % "2.5.0")
addSbtPlugin("com.github.sbt"            % "sbt-native-packager" % "1.9.16")

// sbt dependencyBrowseTreeHTML - will generate dependency tree in html/json formats
addDependencyTreePlugin
libraryDependencies += "ch.qos.logback"     % "logback-classic"  % "1.4.8"
libraryDependencies += "org.apache.commons" % "commons-compress" % "1.23.0"
libraryDependencies += "org.slf4j"          % "slf4j-nop"        % "1.7.36"

addSbtPlugin("ch.epfl.scala"  % "sbt-scalafix"        % "0.11.1")
addSbtPlugin("ch.epfl.scala"  % "sbt-version-policy"  % "2.1.3")
addSbtPlugin("com.eed3si9n"   % "sbt-buildinfo"       % "0.11.0")
addSbtPlugin("com.github.sbt" % "sbt-release"         % "1.1.0")
addSbtPlugin("com.typesafe"   % "sbt-mima-plugin"     % "1.1.3")
addSbtPlugin("com.github.sbt" % "sbt-git"             % "2.0.1")
addSbtPlugin("org.typelevel"  % "sbt-tpolecat"        % "0.5.0")
addSbtPlugin("org.typelevel"  % "sbt-fs2-grpc"        % "2.7.11")
addSbtPlugin("org.scalameta"  % "sbt-mdoc"            % "2.4.0")
addSbtPlugin("org.scalameta"  % "sbt-scalafmt"        % "2.5.2")
addSbtPlugin("com.github.sbt" % "sbt-native-packager" % "1.9.16")

// sbt dependencyBrowseTreeHTML - will generate dependency tree in html/json formats
addDependencyTreePlugin
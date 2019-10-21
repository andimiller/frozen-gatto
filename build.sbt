name := "FrozenGatto"

version := "0.1"

scalaVersion := "2.12.10"

addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.11.0" cross CrossVersion.full)

scalacOptions += "-Ypartial-unification"

libraryDependencies ++= List(
  "org.typelevel"     %% "cats-effect" % "2.0.0",
  "io.circe"          %% "circe-yaml"  % "0.11.0-M1",
  "io.circe"          %% "circe-rs"    % "0.12.2",
  "io.circe"          %% "circe-parser"    % "0.12.2",
  "com.monovore"      %% "decline"     % "1.0.0",
  "io.higherkindness" %% "droste-core" % "0.7.0",
  "co.fs2"            %% "fs2-io"      % "2.0.1",
  "org.tpolecat"      %% "atto-core"   % "0.7.1"
)

scalafmtConfig in ThisBuild := Some(file("scalafmt.conf"))

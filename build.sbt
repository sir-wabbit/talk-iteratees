val testLibraries = List(
  "org.scalacheck" %% "scalacheck" % "1.13.4" % "test",
  "org.typelevel" %% "discipline" % "0.7.2" % "test",
  "org.scalatest" %% "scalatest" % "3.0.1" % "test")

val effLibrary = List(
  "org.atnos" %% "eff-cats" % "2.0.0-RC18")

val catsLibraries = List(
  "org.typelevel" %% "algebra" % "0.5.1",
  "org.typelevel" %% "cats" % "0.8.1")

val iterateeLibraries = ((version: String) => List(
  "io.iteratee"  %%  "iteratee-core" % version,
  "io.iteratee"  %%  "iteratee-files" % version))
  .apply("0.7.1")

lazy val commonSettings = List(
  addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.3"),
  addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full),
  organization := "sir-wabbit",
  version := "0.1-SNAPSHOT",
  scalaOrganization := "org.typelevel",
  scalaVersion := "2.11.8",
  scalacOptions ++= List(
    "-deprecation", "-unchecked", "-feature",
    "-encoding", "UTF-8",
    "-language:existentials", "-language:higherKinds",
    "-Yno-adapted-args", "-Ywarn-dead-code",
    "-Ywarn-numeric-widen", "-Xfuture",
    "-Ypartial-unification", "-Yliteral-types"),
  resolvers ++= List(
    Resolver.sonatypeRepo("snapshots"),
    Resolver.sonatypeRepo("releases")),
  libraryDependencies ++= testLibraries,
  wartremoverWarnings ++= Warts.all)

lazy val motivation1 = (project in file("motivation1")).
  settings(name := "motivation1").
  settings(commonSettings: _*).
  settings(libraryDependencies ++=
    catsLibraries ++
      effLibrary ++
      iterateeLibraries)

lazy val motivation2 = (project in file("motivation2")).
  settings(name := "motivation2").
  settings(commonSettings: _*)

lazy val basic1 = (project in file("basic1")).
  settings(name := "basic1").
  settings(commonSettings: _*).
  settings(libraryDependencies ++= catsLibraries)

lazy val basic2 = (project in file("basic2")).
  settings(name := "basic2").
  settings(commonSettings: _*).
  settings(libraryDependencies ++= catsLibraries)

lazy val iteratee1 = (project in file("iteratee1")).
  settings(name := "iteratee1").
  settings(commonSettings: _*).
  settings(libraryDependencies ++=
    catsLibraries ++
    effLibrary ++
    iterateeLibraries)

lazy val root = (project in file(".")).
  settings(name := "iteratee-presentation").
  settings(commonSettings: _*).
  aggregate(motivation1, motivation2, basic1, basic2, iteratee1)

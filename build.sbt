val scala2Version = "2.13.6"
val scala3Version = "3.1.0"

lazy val root = project
  .in(file("."))
  .settings(
    name := "Advent_2021 scala3-cross",
    version := "0.1.0",

    libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.10",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.10" % "test",

    // To make the default compiler and REPL use Dotty
    scalaVersion := scala3Version,

    // To cross compile with Scala 3 and Scala 2
    crossScalaVersions := Seq(scala3Version, scala2Version)
  )

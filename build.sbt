ThisBuild / organization := "io.forward"
ThisBuild / scalaVersion := "3.8.4"
ThisBuild / version := "0.1.0-SNAPSHOT"

lazy val root = (project in file("."))
  .settings(
    name := "shapes",
    description := "Small functional programming shapes for Scala",
    libraryDependencies += "org.scalameta" %% "munit" % "1.3.3" % Test,
    testFrameworks += new TestFramework("munit.Framework"),
    scalacOptions ++= Seq(
      "-deprecation",
      "-feature",
      "-unchecked"
    )
  )

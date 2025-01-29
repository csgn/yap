import Dependencies._

/* Project settings */
ThisBuild / name := "yap"
ThisBuild / scalaVersion := "2.13.15"
ThisBuild / version := "1.0.0-SNAPSHOT"
ThisBuild / description := "Data structures & algorithms implementation in Scala."
ThisBuild / licenses := List(("MIT", url("https://opensource.org/license/mit")))
ThisBuild / developers ++= List(
  Developer(
    id = "csgn",
    name = "Sergen Cepoglu",
    email = "dev.csgn@gmail.com",
    url = url("https://github.com/csgn")
  )
)

/* Test settings */
ThisBuild / testFrameworks += new TestFramework("munit.Framework")

/* Scalafix settings */
ThisBuild / semanticdbEnabled := true
ThisBuild / semanticdbVersion := scalafixSemanticdb.revision

/* Publish settings */
ThisBuild / publishMavenStyle := true

/* Compiler settings */
ThisBuild / scalacOptions ++= Seq(
  "-Wunused",
)

lazy val benchmarks = project
  .in(file("benchmarks"))
  .dependsOn(core)
  .settings(
    name := "benchmarks",
    publish / skip := true,
    libraryDependencies ++= {
      Seq(
        scalameter,
      )
    }
  )
  .enablePlugins(ScalafixPlugin)

lazy val tests = project
  .in(file("tests"))
  .dependsOn(core)
  .settings(
    name := "tests",
    publish / skip := true,
    libraryDependencies ++= {
      Seq(
        munit % Test,
      )
    }
  )
  .enablePlugins(ScalafixPlugin)

lazy val core = project
  .in(file("core"))
  .settings(
    name := "core",
    moduleName := "yap-core",
    libraryDependencies ++= {
      Seq(
        cats,
      )
    }
  )
  .enablePlugins(ScalafixPlugin)

lazy val yap = project
  .in(file("."))
  .settings(
    name := "yap",
  )
  .aggregate(core, tests)

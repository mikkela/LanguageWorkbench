ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / scalaVersion := "3.6.2"
ThisBuild / organization := "org.mikadocs"

val scalaTestDeps = Seq(
  "org.scalameta" %% "munit" % "1.0.3" % Test
)

lazy val root = (project in file("."))
  .settings(
    name := "LanguageWorkbench",
    idePackagePrefix := Some("org.mikadocs.language"),
    libraryDependencies ++= scalaTestDeps,
  )
  .aggregate(
    workbench, kamin, lox
  )
  .dependsOn(workbench, kamin, lox)

lazy val workbench = (project in file("workbench"))
  .settings(
    name := "Workbench",
    idePackagePrefix := Some("org.mikadocs.language.workbench"),
    libraryDependencies ++= scalaTestDeps,
  )

lazy val lox = (project in file("lox"))
  .settings(
    name := "Lox",
    idePackagePrefix := Some("org.mikadocs.language.lox"),
    libraryDependencies ++= scalaTestDeps,
  )
  .dependsOn(workbench)

lazy val kamin = (project in file("kamin"))
  .settings(
    name := "Kamin",
    idePackagePrefix := Some("org.mikadocs.language.kamin"),
    libraryDependencies ++= scalaTestDeps,
  )
  .dependsOn(workbench)

libraryDependencies += "org.jline" % "jline" % "3.27.1"
libraryDependencies += "org.scalameta" %% "munit" % "1.0.3" % Test

ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / scalaVersion := "3.6.2"
ThisBuild / organization := "org.mikadocs"

lazy val root = (project in file("."))
  .settings(
    name := "LanguageWorkbench",
    idePackagePrefix := Some("org.mikadocs.language")
  )
  .aggregate(
    workbench, lox
  )
  .dependsOn(workbench)

lazy val workbench = (project in file("workbench"))
  .settings(
    name := "Workbench",
    idePackagePrefix := Some("org.mikadocs.language.workbench")
  )

lazy val lox = (project in file("lox"))
  .settings(
    name := "Lox",
    idePackagePrefix := Some("org.mikadocs.language.lox")
  )
  .dependsOn(workbench)

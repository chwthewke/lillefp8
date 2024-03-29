import Dependencies._

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "fr.thomasdufour",
      scalaVersion := "2.12.4",
      version      := "0.1.0-SNAPSHOT"
    )),
    name := "lillefp8",
    libraryDependencies ++= Seq(scalatest, scalacheck, cats, catsKernelLaws)
  )

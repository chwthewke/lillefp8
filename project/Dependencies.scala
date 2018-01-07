import sbt._

object Dependencies {
  lazy val scalatest = "org.scalatest" %% "scalatest" % "3.0.3"
  lazy val scalacheck = "org.scalacheck" %% "scalacheck" % "1.13.4"
  lazy val cats = "org.typelevel" %% "cats-core" % "1.0.0"
  lazy val catsKernelLaws = "org.typelevel" %% "cats-kernel-laws" % "1.0.0"
}

import BuildHelper._
import sbt._

ThisBuild / description  := "Morphir Scala"
ThisBuild / organization := "finos"

lazy val docs = project
  .settings(
    moduleName := "morphir-scala",
    mdocOut    := (LocalRootProject / baseDirectory).value / "website" / "docs"
  )
  .enablePlugins(MdocPlugin, WebsitePlugin)

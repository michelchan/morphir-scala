import mdoc.MdocPlugin.autoImport.{mdocIn, mdocOut}
import sbt.Keys._
import sbt._

object BuildHelper {
  def mdocSettings(docsDir: String, outDir: String) = Seq[sbt.Def.Setting[_]](
    mdocIn  := baseDirectory.value / docsDir,
    mdocOut := (LocalRootProject / baseDirectory).value / outDir
  )
}

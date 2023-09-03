import mill._
import mill.scalalib._
import mill.util.Jvm
import mill.define.Target
import os.Path

trait Docusaurus2Module extends Module {

  def docusaurusSources: Sources
  def compiledMdocs: Sources

  def pnpmInstall: T[PathRef] = T {
    val baseDir = T.dest

    docusaurusSources().foreach { pr =>
      os.list(pr.path)
        .foreach(p =>
          os.copy.into(
            p,
            baseDir,
            followLinks = true,
            replaceExisting = true,
            copyAttributes = true,
            createFolders = true,
            mergeFolders = false
          )
        )
    }

    val process = Jvm.spawnSubprocess(
      commandArgs = Seq(
        "pnpm",
        "install",
      ),
      envArgs = Map.empty,
      workingDir = T.dest
    )
    process.join()
    T.log.info(new String(process.stdout.bytes))
    PathRef(T.dest)
  }

  def installedDocusaurusSources = T.source(pnpmInstall().path)

  def docusaurusBuild: T[PathRef] = T {
    val workDir                = T.dest
    val docusaurusInstallation = installedDocusaurusSources()
    val pnpmSetup              = docusaurusInstallation.path

    val proc = Jvm.spawnSubprocess(
      commandArgs = Seq(
        "sbt",
        "compileDocs"
      ),
      envArgs = Map.empty,
      workingDir = millbuild.build.millSourcePath
    )
    proc.join()

    os.copy(
      pnpmSetup,
      workDir,
      followLinks = true,
      replaceExisting = true,
      copyAttributes = true,
      createFolders = true,
      mergeFolders = false
    )

//
//    val docsDir = workDir / "docs"
//    os.makeDir.all(docsDir)
//    os.list(docsDir).foreach(os.remove.all)
//
//    Seq(docusaurusInstallation).foreach { pr =>
//      val bd = pr.path
//      os.walk(pr.path / "docs").foreach { p =>
//        val relPath = p.relativeTo(bd / "docs")
//        T.log.info(relPath.toString())
//        if (p.toIO.isFile) {
//          val target = docsDir / relPath
//          os.makeDir.all(target)
//          os.copy.over(p, docsDir / relPath)
//        }
//      }
//    }
//
//    compiledMdocs().foreach { pr =>
//      os.list(pr.path)
//        .foreach(p =>
//          os.copy.into(
//            p,
//            docsDir,
//            followLinks = true,
//            replaceExisting = true,
//            copyAttributes = true,
//            createFolders = true,
//            mergeFolders = true
//          )
//        )
//    }

    val process = Jvm.spawnSubprocess(
      commandArgs = Seq(
        "pnpm",
        "install"
      ),
      envArgs = Map.empty,
      workingDir = T.dest
    )

    val p2 = Jvm.spawnSubprocess(
      commandArgs = Seq(
        "pnpm",
        "build"
      ),
      envArgs = Map.empty,
      workingDir = workDir
    )

    p2.join()

    PathRef(workDir)
  }

  def docusaurusServe() = T.command {
    val workDir = docusaurusBuild().path
    Jvm.runSubprocess(
      commandArgs = Seq(
        "pnpm",
        "start"
      ),
      envArgs = T.env,
      workingDir = workDir
    )
  }
}

package info.kwarc.mmt.test.testers

import info.kwarc.mmt.api.archives.Archive
import info.kwarc.mmt.api.utils.MMTSystem

import scala.util.Try

/** trait implementing testing for archives */
trait ArchiveTester extends BaseTester with ActionTester {
  lazy protected val useArchiveDevel: Boolean = {

    // look into the environment
    // and use that version if defined
    val develEnv = Try(sys.env("TEST_USE_DEVEL")).toOption
    if(develEnv.contains("0")){
      false
    } else if(develEnv.contains("1")){
      true
    // else use the current git branch
    } else {
      MMTSystem.gitVersion.contains("devel")
    }
  }

  /** set of archives to be installed */
  val archives: Seq[String]

  /** gets an [[Archive]] instance given an ID */
  protected def getArchive(id: String) : Archive = {
    if(!archives.contains(id)){
      testWarn(s"Archive missing from test specification: $id is missing from Test")
    }

    controller.backend.getArchive(id).getOrElse(
      throw testError(s"Archive missing from controller: $id")
    )
  }

  /** check that a given archive gets installed properly */
  private def installArchive(archive: String): Unit = {
    val lmhInstallArchive = archive + (if(useArchiveDevel) "@devel" else "")

    test(s"get archive $archive", {
      // and install the archive (optionally a development version)
      handleLine(s"lmh install $lmhInstallArchive")

      // and it should not be null
      assert(getArchive(archive) != null)
    })
  }

  /** check that all the archives get installed properly */
  def shouldInstallArchives() : Unit = {
    controller.report.withGroups(/* "lmh" */) {
      archives.foreach(installArchive)
    }
  }
}
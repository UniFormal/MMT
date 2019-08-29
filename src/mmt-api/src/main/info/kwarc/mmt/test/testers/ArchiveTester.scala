package info.kwarc.mmt.test.testers

import info.kwarc.mmt.api.archives.Archive
import info.kwarc.mmt.api.utils.MMTSystem

import scala.util.Try

/** trait implementing testing for archives */
trait ArchiveTester extends BaseTester with ActionTester {
  lazy protected val testBranch: Option[String] = {

    // look into the environment and check if the TEST_USE_BRANCH
    // environment variable is set
    val envBranch = Try(sys.env("TEST_USE_BRANCH")).toOption
    if(envBranch.nonEmpty) {
      envBranch
    // else use the current git branch
    } else {
      MMTSystem.gitVersion
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
    val lmhInstallArchive = archive + testBranch.map("@" + _).getOrElse("")

    test(s"get archive $archive", {
      // and install the archive (optionally a special version)
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
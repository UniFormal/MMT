package info.kwarc.mmt.api.test.utils.testers

import info.kwarc.mmt.api.archives.Archive
// import info.kwarc.mmt.api.archives.lmh.StandardVersioning

/** trait implementing testing for archives */
trait ArchiveTester extends BaseTester with ActionTester {
  /** set of archives to be installed */
  val archives: List[String]

  /** gets an [[Archive]] instance given an ID */
  protected def getArchive(id: String) : Archive = {
    if(!archives.contains(id)){
      testWarn(s"Archive missing from test specification: $id is missing from Test")
    }
/* TODO
    if(!StandardVersioning.all.exists(_._1 == id)){
      testWarn(s"Archive not versioned by default: $id does not have a fixed version")
    } */

    controller.backend.getArchive(id).getOrElse(
      throw testError(s"Archive missing from controller: $id")
    )
  }

  /** check that a given archive gets installed properly */
  private def installArchive(archive: String): Unit = {
    it should s"get archive $archive" in {
      controller.report.groups += "lmh"
      handleLine(s"lmh install $archive")
      controller.report.groups -= "lmh"

      assert(getArchive(archive) != null)
    }
  }

  /** check that all the archives get installed properly */
  def shouldInstallArchives() : Unit = {
    archives.foreach(installArchive)
  }
}

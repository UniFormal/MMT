package info.kwarc.mmt.api.test.utils.testers

import info.kwarc.mmt.api.archives.Archive
import info.kwarc.mmt.api.test.utils.ArchiveSpec

/** trait implementing testing for archives */
trait ArchiveTester extends BaseTester with ActionTester {
  /** set of archives to be installed */
  val archives: List[ArchiveSpec]

  /** gets an [[Archive]] instance given an ID */
  protected def getArchive(id: String) : Archive = {
    if(!archives.exists(_.id == id)){
      testWarn(s"Archive missing from test specification: $id is missing from Test")
    }

    controller.backend.getArchive(id).getOrElse(
      throw testError(s"Archive missing from controller: $id")
    )
  }

  /** check that a given archive gets installed properly */
  private def installArchive(archive: ArchiveSpec): Unit = {
    it should s"get archive ${archive.id}" in {
      controller.report.groups += "oaf"
      handleLine(archive.toAction.toParseString)
      controller.report.groups -= "oaf"

      assert(getArchive(archive.id) != null)
    }
  }

  /** check that all the archives get installed properly */
  def shouldInstallArchives() : Unit = {
    archives.foreach(installArchive)
  }
}
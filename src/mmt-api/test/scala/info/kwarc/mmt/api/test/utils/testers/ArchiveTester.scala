package info.kwarc.mmt.api.test.utils.testers

import info.kwarc.mmt.api.archives.Archive

import scala.util.Try
// import info.kwarc.mmt.api.archives.lmh.StandardVersioning

/** trait implementing testing for archives */
trait ArchiveTester extends BaseTester with ActionTester {
  lazy protected val useArchiveDevel = Try(sys.env("TEST_USE_DEVEL")).toOption.contains("1")

  /** set of archives to be installed */
  val archives: List[TestArchive]

  /** gets an [[Archive]] instance given an ID */
  protected def getArchive(id: String) : Archive = {
    if(!archives.exists(_.id == id)){
      testWarn(s"Archive missing from test specification: $id is missing from Test")
    }
    /*
    // TODO: Ignore this for now
    if(!StandardVersioning.all.exists(_._1 == id)){
      testWarn(s"Archive not versioned by default: $id does not have a fixed version")
    } */

    controller.backend.getArchive(id).getOrElse(
      throw testError(s"Archive missing from controller: $id")
    )
  }

  /** check that a given archive gets installed properly */
  private def installArchive(archive: TestArchive): Unit = {
    it should s"get archive ${archive.id}" in {
      // and install the archive (optionally a development version)
      // controller.report.groups += "lmh"
      handleLine(s"lmh install ${archive.toLMHString(useArchiveDevel)}")
      // controller.report.groups -= "lmh"

      // and it should not be null
      assert(getArchive(archive.id) != null)
    }
  }

  /** check that all the archives get installed properly */
  def shouldInstallArchives() : Unit = {
    archives.foreach(installArchive)
  }
}

/** represents a single archive to be installed */
case class TestArchive(id: String, standardVersion: Option[String] = None, develVersion: Option[String] = None) {
  /** turns this test archive into a string that can be used with 'lmh install' */
  def toLMHString(useArchiveDevel: Boolean): String = (if(useArchiveDevel) develVersion else standardVersion) match {
    case Some(v) => s"$id@$v"
    case None => id
  }
}


object TestArchive {
  /** creates a new Archive that always installs the default version */
  implicit def apply(id: String) : TestArchive = new TestArchive(id, None, None)

  /** creates a new Archive that installs distinct master and devel versions */
  def apply(id: String, standardVersion: String, develVersion: String): TestArchive = new TestArchive(id, Some(standardVersion), Some(develVersion))

  /** creates a new Archive that has a seperate devel branch */
  def apply(id: String, hasDevel: Boolean): TestArchive = if(hasDevel){
    new TestArchive(id, None, Some("devel"))
  } else {
    new TestArchive(id, None, None)
    new TestArchive(id, None, None)
  }
}
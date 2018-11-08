package info.kwarc.mmt.api.test

import info.kwarc.mmt.api.test.testers._
import info.kwarc.mmt.api.utils.MMTSystem.{Classes, DeployRunStyle, ThinJars}
import info.kwarc.mmt.api.utils.{File, MMTSystem}
import info.kwarc.mmt.doc.Setup

/**
  * A class used for MMT Integration Tests which require a completly setup MMT environment to work
  * @param neededArchives List of archives that should be automatically installed
  * @param neededExtensions List of extensions that should be needed
  */
abstract class MMTIntegrationTest(neededArchives : TestArchive*)(neededExtensions : ExtensionSpec*) extends MMTUnitTest
  with ExtensionTester with ArchiveTester with CheckTester{


  /** the root folder to use for all test data */
  lazy val rootFolder: File = {
    val root = MMTSystem.runStyle match {
      case d: DeployRunStyle => d.deploy / ".."
      case _ => File("")
    }
    (root / "test" / this.getClass.getCanonicalName / "target").canonical
  }
  lazy val contentFolder: File = rootFolder / "content"
  lazy val systemFolder: File = rootFolder / "system"

  /** runs the setup routine inside the MMT controller */
  private def runSetup(): Unit = {

    // configure folders we want to use for setup
    // this is put into .../mmt-subproject/test/<test-class>/target
    // and is thus automatically .gitignored

    // create a setup instance
    val setup = new Setup {
      override val log: String => Unit = s => report("setup", s.trim)
    }
    controller.extman.addExtension(setup)

    // wipe anything old
    if (rootFolder.exists()){
      rootFolder.children foreach (_.deleteDir)
    }

    log("Running automated setup")
    logGroup {
      // report.groups += "setup"
      setup.setup(systemFolder, contentFolder, None, installContent = false)
      // report.groups -= "setup"
    }

    /*
    // this used to be the old flag to setup archive versions from HEAD
    if(Try(sys.env("TEST_USE_ARCHIVE_HEAD")).toOption.contains("1")){
      log("TEST_USE_ARCHIVE_HEAD=1 was set, using newest archive versions")
      logGroup {
        handleLine("lmh versioning disable")
      }
    }
    */

    // simply sho
    if(useArchiveDevel){
      log("Using devel branch of selected archives")
    }

    // and show some information about MMT itself
    handleLine("show mmt", showLog = true)
    handleLine("show lmh", showLog = true)
  }

  /** initialize the test class */
  override def init(): Unit = {
    super.init()

    runSetup()
    shouldLoadExtensions()
    shouldInstallArchives()
  }

  val extensions: List[ExtensionSpec] = List(
    ExtensionSpec("info.kwarc.mmt.api.ontology.AlignmentsServer"),
    ExtensionSpec("info.kwarc.mmt.api.web.JSONBasedGraphServer")
  ) ::: neededExtensions.toList

  val archives: List[TestArchive] = neededArchives.toList
}
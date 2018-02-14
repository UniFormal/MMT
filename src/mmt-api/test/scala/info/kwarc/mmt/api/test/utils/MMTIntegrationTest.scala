package info.kwarc.mmt.api.test.utils

import info.kwarc.mmt.api.test.utils.testers._
import info.kwarc.mmt.api.utils.File
import info.kwarc.mmt.doc.Setup

import scala.util.Try

/**
  * A class used for MMT Integration Tests which require a completly setup MMT environment to work
  * @param neededArchives List of archives that should be automatically installed
  * @param neededExtensions List of extensions that should be needed
  */
abstract class MMTIntegrationTest(neededArchives : String*)(neededExtensions : ExtensionSpec*) extends MMTUnitTest
  with ExtensionTester with ArchiveTester with CheckTester {

  lazy val rootFolder = File(s"test/${this.getClass.getCanonicalName}/target").canonical
  lazy val contentFolder = rootFolder / "content"
  lazy val systemFolder = rootFolder / "system"

  /** runs the setup routine inside the MMT controller */
  private def runSetup(): Unit = {

    // configure folders we want to use for setup
    // this is put into .../mmt-subproject/test/<test-class>/target
    // and is automatically .gitignored

    // create a setup instance
    val setup = new Setup// TODO (Some(s => report("setup", s.trim)))
    controller.extman.addExtension(setup)

    // wipe anything old
    if (rootFolder.exists()){
      rootFolder.children foreach (_.deleteDir)
    }

    log("Running automated setup")
    logGroup {
      report.groups += "setup"
      setup.setup(systemFolder, contentFolder, None, installContent = false)
      report.groups -= "setup"
    }

    if(Try(sys.env("TEST_USE_ARCHIVE_HEAD")).toOption.contains("1")){
      log("TEST_USE_ARCHIVE_HEAD=1 was set, using newest archive versions")
      logGroup {
        handleLine("lmh versioning disable")
      }
    }

    // and show some information about MMT itself
    handleLine("show mmt", showLog = true)
    handleLine("show lmh", showLog = true)
  }

  /** run various setup code for tests, this method should be called to setup all the tests */
  def bootstrapTests(): Unit = it should "bootstrap the controller" in {
    runSetup()
  }

  val extensions: List[ExtensionSpec] = List(
    ExtensionSpec("info.kwarc.mmt.api.ontology.AlignmentsServer"),
    ExtensionSpec("info.kwarc.mmt.api.web.JSONBasedGraphServer")
  ) ::: neededExtensions.toList

  val archives: List[String] = neededArchives.toList
}

package info.kwarc.mmt.test

import info.kwarc.mmt.test.testers._
import info.kwarc.mmt.api.utils.MMTSystem.{Classes, DeployRunStyle, ThinJars}
import info.kwarc.mmt.api.utils.{File, MMTSystem}
import info.kwarc.mmt.doc.Setup

/**
  * A class used for MMT Integration Tests which require a completly setup MMT environment to work
  * @param archives List of archives that should be automatically installed
  * @param neededExtensions List of extensions that should be needed
  */
abstract class MMTIntegrationTest(override val archives : String*)(neededExtensions : ExtensionSpec*)
  extends MMTUnitTest with ExtensionTester with ArchiveTester with CheckTester {

  // the extensions to be installed */
  val extensions: List[ExtensionSpec] = List(
    ExtensionSpec("info.kwarc.mmt.api.ontology.AlignmentsServer"),
    ExtensionSpec("info.kwarc.mmt.api.web.JSONBasedGraphServer")
  ) ::: neededExtensions.toList

  /** the test program */
  def main(): Unit

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
    report.withGroups(/* "setup" */) {
      logGroup {
        setup.setup(systemFolder, contentFolder, None, installContent = None)
      }
    }


    if(testBranch.nonEmpty){
      log(s"Using ${testBranch.get} version of selected archives")
    } else {
      log("Using default version of all archives")
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
}
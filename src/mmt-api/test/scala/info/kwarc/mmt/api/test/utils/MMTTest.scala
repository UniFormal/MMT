package info.kwarc.mmt.api.test.utils

import info.kwarc.mmt.api.archives.BuildQueue
import info.kwarc.mmt.api.frontend.{Controller, ReportHandler, Run}
import info.kwarc.mmt.api.test.utils.testers._
import info.kwarc.mmt.api.utils.File
import info.kwarc.mmt.doc.Setup
import org.scalatest.{BeforeAndAfterAll, FlatSpec, Matchers}

/**
  * Base class for Tests run within MMT Projects
  * @param neededArchives
  * @param neededExtensions
  */
abstract class MMTTest(neededArchives : ArchiveSpec*)(neededExtensions : ExtensionSpec*) extends FlatSpec with Matchers with BeforeAndAfterAll
  with ExtensionTester with ArchiveTester with CheckTester {

  lazy val controller: Controller = Run.controller

  /** setups up the controller */
  private def bootstrapController(): Unit = {
    report.addHandler(new ReportHandler("test-console") {
      override def apply(ind: Int, caller: => String, group: String, msgParts: List[String]) {
        msgParts.foreach { msg =>
          val m = indentString(ind) + group + ": " + msg
          info(m)
        }
      }
    })

    controller.report.groups += "test"
    controller.report.groups += "test-warn"
    controller.report.groups += "test-error"

    controller.extman.get(classOf[BuildQueue]).foreach(controller.extman.removeExtension)
  }

  /** runs the setup routine inside the MMT controller */
  private def runSetup(): Unit = {
    // configure the folders and prepare setup
    val rootFolder = File("test/target").canonical
    val contentFolder = rootFolder / "content"
    val systemFolder = rootFolder / "system"

    // create a setup instance
    val setup = new Setup
    controller.extman.addExtension(setup)

    // wipe anything old
    if (rootFolder.exists()){
      rootFolder.children foreach (_.deleteDir)
    }

    // and run the setup
    setup.setup(systemFolder, contentFolder, None, installContent = false, logger = Some(s => log(s.trim)))

    // and print an overview
    log(s"Configured MMT Testing root in $rootFolder")
  }

  /** run various setup code for tests */
  def bootstrapTests(): Unit = it should "bootstrap the controller" in {
    bootstrapController()
    runSetup()
  }

  val extensions: List[ExtensionSpec] = List(
    ExtensionSpec("info.kwarc.mmt.api.ontology.AlignmentsServer"),
    ExtensionSpec("info.kwarc.mmt.api.web.JSONBasedGraphServer")
  ) ::: neededExtensions.toList

  val archives: List[ArchiveSpec] = neededArchives.toList
}
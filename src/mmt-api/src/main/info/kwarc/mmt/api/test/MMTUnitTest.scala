package info.kwarc.mmt.api.test

import info.kwarc.mmt.api.archives.BuildQueue
import info.kwarc.mmt.api.frontend.{ConsoleHandler, Controller, ReportHandler, Run}
import info.kwarc.mmt.api.test.testers.BaseTester
/**
  * A class used for MMT Unit Tests that only need a controller instance to work properly
  */
abstract class MMTUnitTest extends BaseTester {
  /** controller we are using during tests */
  lazy val controller: Controller = Run.controller

  /** sets up the controller for tests */
  override def init(): Unit = {
    controller.report.addHandler(ConsoleHandler)

    // add all the log groups that we are only using during tests
    controller.report.groups += "test"
    controller.report.groups += "testcase"
    controller.report.groups += "test-warn"
    controller.report.groups += "test-error"

    // remove the [BuildQueue] Extension
    controller.extman.get(classOf[BuildQueue]).foreach(controller.extman.removeExtension)
  }

}

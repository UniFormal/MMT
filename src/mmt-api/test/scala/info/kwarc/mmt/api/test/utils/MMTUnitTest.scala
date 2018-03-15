package info.kwarc.mmt.api.test.utils

import info.kwarc.mmt.api.archives.BuildQueue
import info.kwarc.mmt.api.frontend.{Controller, ReportHandler, Run}
import info.kwarc.mmt.api.test.utils.testers.BaseTester
import org.scalatest.{BeforeAndAfterAll, FlatSpec, Matchers}

/**
  * A class used for MMT Unit Tests that only need a controller instance to work properly
  */
abstract class MMTUnitTest extends FlatSpec with Matchers with BeforeAndAfterAll with BaseTester {
  /** controller we are using during tests */
  lazy val controller: Controller = Run.controller

  /** sets up the controller for tests */
  override def beforeAll(): Unit = {
    // create a new handler that can print to the test-console
    report.addHandler(new ReportHandler("test") {
      override def apply(ind: Int, caller: => String, group: String, msgParts: List[String]) {
        msgParts.foreach { msg =>
          val m = indentString(ind) + group + ": " + msg
          info(m)
        }
      }
    })

    // add all the log groups that we are only using during tests
    controller.report.groups += "test"
    controller.report.groups += "test-warn"
    controller.report.groups += "test-error"

    // remove the [BuildQueue] Extension
    controller.extman.get(classOf[BuildQueue]).foreach(controller.extman.removeExtension)
  }

}

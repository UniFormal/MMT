package info.kwarc.mmt.test

import info.kwarc.mmt.api.archives.BuildQueue
import info.kwarc.mmt.api.frontend._
import info.kwarc.mmt.api.objects.Term
import info.kwarc.mmt.api.utils
import info.kwarc.mmt.test.testers.BaseTester

/**
  * A class used for MMT Unit Tests that only need a controller instance to work properly
  */
abstract class MMTUnitTest extends BaseTester {

  /** controller we are using during tests */
  lazy val controller: Controller = new Controller(new Report {
      private var count = 0

      override def apply(prefix: => String, msg: => String): Unit = {
        count += 1
        super.apply(prefix, msg)
        val prefixList = utils.stringToList(prefix, "#")
        if (prefixList.forall(p => groups.contains(p)) || groups.contains("all")) {
          count = 0
        }
        if (count >= 5000) {
          count = 0
          super.apply("test", "Still running!")
        }
      }
    })

  /** sets up the controller for tests */
  override def init {
    controller.report.addHandler(ConsoleHandler)

    // add all the log groups that we are only using during tests
    controller.report.groups += "test"
    controller.report.groups += "testcase"
    controller.report.groups += "test-warn"
    controller.report.groups += "test-error"

    // remove the [BuildQueue] Extension
    controller.extman.get(classOf[BuildQueue]).foreach(controller.extman.removeExtension)
  }

  def assertTermEqual(expected: Term, actual: Term, msg: Option[String] = None): Unit = {
    if (expected != actual) {
      testError(s"Term equality failed${msg.map(" (" + _ + ")").getOrElse("")}:")
      testError("Expected term: " + expected)
      testError("Actual term: " + actual)
    }
  }

  def assertSetEqual[T](expected: Set[T], actual: Set[T], msg: Option[String] = None): Unit = {
    if (expected != actual) {
      testError(s"Set equality failed${msg.map(" (" + _ + ")").getOrElse("")}:")
      testError("Expected set: " + expected)
      testError("Actual set: " + actual)
    }
  }

}

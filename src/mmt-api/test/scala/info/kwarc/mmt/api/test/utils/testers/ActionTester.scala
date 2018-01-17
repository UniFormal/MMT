package info.kwarc.mmt.api.test.utils.testers

import info.kwarc.mmt.api.frontend.ReportHandler

/** implements the checking of Actions */
trait ActionTester extends BaseTester {
  /** handle a single line in the controller */
  def handleLine(s : String, showLog: Boolean = true): Unit = controller.handleLine(s, showLog=showLog)

  /** handle a line and assert that it should properly run */
  def shouldHandleLine(s : String): Unit = it should s in handleLine(s)
}

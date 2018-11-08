package info.kwarc.mmt.api.test.testers

/** implements the checking of Actions */
trait ActionTester extends BaseTester {

  /** handle a single line in the controller */
  def handleLine(s : String, showLog: Boolean = true): Unit = controller.handleLine(s, showLog=showLog)

  /** handle a line and assert that it should properly run */
  def shouldHandleLine(s : String): Unit = test(s, handleLine(s))
}

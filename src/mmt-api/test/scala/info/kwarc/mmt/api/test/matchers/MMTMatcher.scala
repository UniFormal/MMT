package info.kwarc.mmt.api.test.matchers

import info.kwarc.mmt.api
import info.kwarc.mmt.api.frontend.ConsoleHandler.indentString
import info.kwarc.mmt.api.frontend.{Controller, Logger, Report, ReportHandler}
import org.scalatest.FlatSpec

/**
  * A basic matcher for anything MMT related
  */
trait MMTMatcher extends FlatSpec with Logger {
  info("test")

  // create a controller to be used during testing
  val controller: Controller

  /** handle a single line in the controller */
  def handleLine(s : String): Unit = controller.handleLine(s)

  /** handle a line and assert that it should properly run */
  def shouldHandleLine(s : String): Unit = it should s in handleLine(s)

  /** configure the controller for various test details */
  def configureController: Unit = {
      report.addHandler(new ReportHandler("console") {
        override def apply(ind: Int, caller: => String, group: String, msgParts: List[String]) {
          msgParts.foreach { msg =>
            val m = indentString(ind) + group + ": " + msg
            // TODO: Split this up via seperate reporters
            info(m)
          }
        }
      })
      handleLine("log+ test")
      handleLine("log+ test-warn")
      handleLine("log+ test-error")
  }

  // this Matcher has a logger that can be used to log extensions
  def logPrefix: String = "test"
  def report: Report = controller.report

  /** logs a warning */
  def testWarn(msg: => String): Unit = {
    log(msg, Some("warn"))
  }

  /** logs an error */
  def testError(message: String, causedBy: Option[Throwable] = None): TestError = {
    val error = TestError(message)
    val errorWithCause = causedBy.map(error.setCausedBy).getOrElse(error)
    log(errorWithCause.toStringLong, Some("error"))
    errorWithCause
  }
}

case class TestError(s : String) extends api.Error(s)
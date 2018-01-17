package info.kwarc.mmt.api.test.utils.testers

import info.kwarc.mmt.api
import info.kwarc.mmt.api.frontend._
import org.scalatest.FlatSpec

/** abstract class for MMT Testing components */
trait BaseTester extends FlatSpec with Logger {
  val controller: Controller

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
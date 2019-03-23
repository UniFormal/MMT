package info.kwarc.mmt.test.testers

import info.kwarc.mmt.api
import info.kwarc.mmt.api.frontend._

/** abstract class for MMT Testing components */
trait BaseTester extends Logger {
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

  /** runs a test or aborts if needed */
  def test[T](s: String, test: => T): T = {
    try {
      logGroup {
        report("testcase", s"'$s' starting")
        val t = logGroup {
          test
        }
        report("testcase", s"'$s' ok")
        t
      }

    } catch {
      case te: TestError => throw te
      case t: Throwable =>
        throw testError(s, Some(t))
    }
  }

  /** sets up the test */
  def init(): Unit = {}
  /** runs the test properly, to be implemented by test class */
  def main(): Unit

  /** runs the test safely */
  def run(): Boolean = {
    try {
      init()
      main()
      true
    } catch {
      case te: TestError => false
      case t: Throwable =>
        testError(this.getClass.getName, Some(t))
        false
    }
  }

  /** main entry point when running this test in a stand-alone fashion */
  def main(args: Array[String]): Unit = {
    if(run()){
      System.exit(0)
    } else {
      System.exit(-1)
    }
  }
}

case class TestError(s : String) extends api.Error(s)

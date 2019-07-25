package info.kwarc.mmt.test

import info.kwarc.mmt.test.testers.BaseTester

/**
  * The TestRunner class represents a single run on multiple Main-Classes
  * That Implement MMT-Unit Tests
  */
object TestRunner {

  def main(args: Array[String]): Unit = {
    if(run(args.toList)) {
      sys.exit(0)
    } else {
      sys.exit(1)
    }
  }

  /**
    * Main entry point
    * @return
    */
  def run(args: List[String]): Boolean = {
    logSection("Loading Tests")
    val tests = args.map(p => (p, loadAndInitializeTest(p)))

    logSection("Running Tests")
    val res = tests.map({
      case (cls, Some(inst)) => {
        logInfo(Some(cls), "Starting Test")
        val res = inst.run()
        if(res) {
          logSuccess(cls, "Test succeeded")
        } else {
          logError(cls, "Test failed")

        }
        (cls, Some(res))
      }
      case (cls, None) => {
        (cls, None)
      }
    })

    logSection("Summary")

    var loadFailureCount = 0
    var failureCount = 0
    var successCount = 0

    // produce a summary at the bottom
    res.foreach({
      case (cls, None) =>
        logError(cls, "Test failed to load")
        loadFailureCount += 1
      case (cls, Some(false)) =>
        logError(cls, "Test reported failure")
        failureCount += 1
      case (cls, Some(true)) =>
        logSuccess(cls, "Test reported success")
        successCount += 1
    })

    logInfo(None, "="*40)
    logInfo(None, s"Total: ${res.length}")
    logInfo(None, s"Success: $successCount / Failed: $failureCount / Load Failure: $loadFailureCount")

    // return true iff there were no errors
    failureCount == 0 && loadFailureCount == 0
  }

  private def logInfo(testName: Option[String], msg: String): Unit = {
    Console.err.println(testName.map("[" + Console.BLUE + _ + Console.RESET + "] ").getOrElse("") + msg)
  }
  private def logSuccess(testName: String, msg: String): Unit = {
    logInfo(Some(testName), Console.GREEN + msg + Console.RESET)
  }
  private def logError(testName: String, msg: String): Unit = {
    logInfo(Some(testName), Console.RED + msg + Console.RESET)
  }
  private def logSection(name: String): Unit = {
    logInfo(None, "="*80)
    logInfo(None, name)
    logInfo(None, "="*80)
  }

  /**
    * Loads and initializes a singleton test object
    * @return
    */
  private def loadAndInitializeTest(path: String): Option[BaseTester] = {
    // load the single
    val obj = try {
      loadCompanionObject(path)
    } catch {
      case e: Exception =>
        logError(path, s"Failed to load class: $e")
        return None
    }

    obj match {
      case bt: BaseTester =>
        logSuccess(path, "Loaded Test")
        Some(bt)
      case _ =>
        logError(path, "Singleton is not a BaseTester instance")
        None
    }
  }


  /** LoadCompanionObject loads the companion object from path */
  private def loadCompanionObject(path: String) = {
    val clz = Class.forName(path + "$")
    val field = clz.getField("MODULE$")
    field.get(null)
  }
}

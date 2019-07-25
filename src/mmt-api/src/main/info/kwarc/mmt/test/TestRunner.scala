package info.kwarc.mmt.test

import info.kwarc.mmt.api.utils.MMTSystem
import info.kwarc.mmt.api.utils.MMTSystem.IsFat

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
    * Gets the default set of tests
    * @return
    */
  private def getDefaultTests(): List[String] = {
    MMTSystem.getResourceAsString("tests/tests.txt").split("\n").map(_.trim).filter(_.nonEmpty).toList
  }

  /** Main TestRunner entry point */
  def run(args: List[String]): Boolean = {

    logSection("Test Overview")
    val tests = if(args.nonEmpty) args else getDefaultTests()
    tests.foreach({t => logInfo(None, t)})


    logSection("Test Run")
    val res = tests.map({ clz =>
      if(runTest(clz)) {
        logSuccess(clz, "Test reported success")
        (clz, true)
      } else {
        logError(clz, "Test reported failure")
        (clz, false)
      }
    })

    logSection("Summary")

    var successCount = 0
    var failureCount = 0

    // produce a summary at the bottom
    res.foreach({
      case (cls, false) =>
        logError(cls, "Test reported failure")
        failureCount += 1
      case (cls, true) =>
        logSuccess(cls, "Test reported success")
        successCount += 1
    })

    logInfo(None, "="*40)
    logInfo(None, s"Success: $successCount / Failed: $failureCount / Total: ${res.length}")

    // return true iff there were no errors
    failureCount == 0
  }

  /**
    * runTest runs a specific test
    * @param clz
    * @return
    */
  private def runTest(clz: String): Boolean = {

    // get the classpath, which is either the jar that is provided
    // or the jar (when run from a jar)
    val cp = MMTSystem.runStyle match {
      case fat: IsFat => fat.jar.toString
      case _ => System.getProperty("java.class.path")
    }

    // run the test in a subprocess
    val process = new ProcessBuilder(
      System.getProperty("java.home") + java.io.File.separator + "bin" + java.io.File.separator + "java",
      "-cp",
      cp,
      clz
    )

    logInfo(Some(clz), "Starting Test")
    logInfo(Some(clz), String.join(" ", process.command()))

    // start + wait for the process
    val proc = process.inheritIO().start()
    proc.waitFor()

    // return based on the exit code
    proc.exitValue() == 0
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
}

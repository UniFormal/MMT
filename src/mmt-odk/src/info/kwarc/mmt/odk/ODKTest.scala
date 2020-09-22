package info.kwarc.mmt.odk

import info.kwarc.mmt.test.MMTIntegrationTest
import info.kwarc.mmt.test.testers._
import info.kwarc.mmt.test.Orders

object ODKTest extends MMTIntegrationTest(
  "MMT/urtheories",
  "MMT/LFX",
  "Test/General",
  "MitM/smglom",
)(
  ExtensionSpec("info.kwarc.mmt.lf.Plugin"),
  ExtensionSpec("info.kwarc.mmt.odk.Plugin")
) {
  def main(): Unit = {
    shouldClearTarget("MMT/urtheories", "bin")
    shouldHandleLine("build MMT/urtheories scala-bin")

    shouldClearTarget("MMT/LFX", "bin")
    shouldHandleLine("build MMT/LFX scala-bin")
    shouldHandleLine("build MMT/LFX mmt-omdoc")

    shouldCheck("Test/General", Orders.testgeneral: _*)(mayfail = List(
      "http://test.kwarc.info/Structure?C?test2?definition"
    ))
  }

}


object MitMTest extends MMTIntegrationTest(
  "MMT/urtheories",
  "MMT/LFX",
  "MitM/Foundation",
  "MitM/Core"
)(
  ExtensionSpec("info.kwarc.mmt.lf.Plugin"),
  ExtensionSpec("info.kwarc.mmt.odk.Plugin")
) {
  def main(): Unit = {
    shouldClearTarget("MMT/urtheories", "bin")
    shouldHandleLine("build MMT/urtheories scala-bin")

    shouldClearTarget("MMT/LFX", "bin")
    shouldHandleLine("build MMT/LFX scala-bin")

    shouldClearTarget("MitM/Foundation", "bin")
    shouldHandleLine("build MitM/Foundation scala-bin")

    logGroup {
      shouldHandleLine("log+ structure-checker-simple")
      shouldCheck("MitM/Core",Orders.mitmcore:_*)(onlyfiles = true)
    }
  }
}

object LATIN2Test extends MMTIntegrationTest(
  "MMT/urtheories",
  "MMT/LFX",
  "MMT/LATIN2"
)(
  ExtensionSpec("info.kwarc.mmt.lf.Plugin"),
  ExtensionSpec("info.kwarc.mmt.odk.Plugin")
){
  def main(): Unit = {
    shouldClearTarget("MMT/urtheories", "bin")
    shouldHandleLine("build MMT/urtheories scala-bin")

    shouldClearTarget("MMT/LFX", "bin")
    shouldHandleLine("build MMT/LFX scala-bin")

    shouldClearTarget("MMT/LATIN2", "bin")
    shouldHandleLine("build MMT/LATIN2 scala-bin")

    logGroup {
      shouldHandleLine("log+ structure-checker-simple")
      shouldCheck("MMT/LATIN2",Orders.latin2:_*)(onlyfiles = true)
    }
  }
}
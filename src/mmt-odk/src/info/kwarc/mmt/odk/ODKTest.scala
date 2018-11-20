package info.kwarc.mmt.odk

import info.kwarc.mmt.api.test.MMTIntegrationTest
import info.kwarc.mmt.api.test.testers._
import info.kwarc.mmt.lf.Orders

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
  "MitM/smglom"
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
      shouldCheck("MitM/smglom",Orders.mitmsmglom:_*)(onlyfiles = true)
    }
  }
}
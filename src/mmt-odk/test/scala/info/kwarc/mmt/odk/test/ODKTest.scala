package info.kwarc.mmt.odk.test

import info.kwarc.mmt.api.archives.RedirectableDimension
import info.kwarc.mmt.api.archives.lmh.LMHHubArchiveEntry
import info.kwarc.mmt.api.test.utils.testers._
import info.kwarc.mmt.api.test.utils._
import info.kwarc.mmt.api.utils.File
import info.kwarc.mmt.lf.test.Orders

class ODKTest extends MMTIntegrationTest(
  TestArchive("MMT/urtheories", hasDevel = true),
  TestArchive("MMT/LFX", hasDevel = true),
  TestArchive("Test/General", hasDevel = true),
  TestArchive("MitM/smglom", hasDevel = true)
)(
  ExtensionSpec("info.kwarc.mmt.lf.Plugin"),
  ExtensionSpec("info.kwarc.mmt.odk.Plugin")
) {

  bootstrapTests()

  shouldLoadExtensions()
  shouldInstallArchives()

  shouldClearTarget("MMT/LFX", "bin")
  shouldHandleLine("build MMT/LFX scala-bin")
  shouldHandleLine("build MMT/LFX mmt-omdoc")

  shouldCheck("Test/General", Orders.testgeneral:_*)(mayfail = List(
    "http://test.kwarc.info/Structure?C?test2?definition"
  ))
  handleLine("log+ structure-checker")
  shouldCheck("MitM/smglom",Orders.mitmsmglom:_*)(onlyfiles = true)
}

/*
class MitMTest extends MMTTest("MMT/LFX","MitM/Foundation","MitM/smglom")("info.kwarc.mmt.lf.Plugin") {
  behavior of "MitM"
  // shouldhl("build MitM/Foundation mmt-omdoc")

  // shouldcheck("MitM/smglom",Orders.mitmsmglom:_*)()
}
*/

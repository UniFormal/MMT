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

  shouldClearTarget("MMT/urtheories", "bin")
  shouldHandleLine("build MMT/urtheories scala-bin")

  shouldClearTarget("MMT/LFX", "bin")
  shouldHandleLine("build MMT/LFX scala-bin")
  shouldHandleLine("build MMT/LFX mmt-omdoc")

  shouldCheck("Test/General", Orders.testgeneral:_*)(mayfail = List(
    "http://test.kwarc.info/Structure?C?test2?definition"
  ))
}


class MitMTest extends MMTIntegrationTest(
  TestArchive("MMT/urtheories", hasDevel = true),
  TestArchive("MMT/LFX", hasDevel = true),
  TestArchive("MitM/Foundation", hasDevel = true),
  TestArchive("MitM/smglom", hasDevel = true)
)(
  ExtensionSpec("info.kwarc.mmt.lf.Plugin"),
  ExtensionSpec("info.kwarc.mmt.odk.Plugin")
) {

  bootstrapTests()

  shouldLoadExtensions()
  shouldInstallArchives()


  shouldClearTarget("MMT/urtheories", "bin")
  shouldHandleLine("build MMT/urtheories scala-bin")

  shouldClearTarget("MMT/LFX", "bin")
  shouldHandleLine("build MMT/LFX scala-bin")

  shouldClearTarget("MitM/Foundation", "bin")
  shouldHandleLine("build MitM/Foundation scala-bin")

  handleLine("log+ structure-checker")
  shouldCheck("MitM/smglom",Orders.mitmsmglom:_*)(onlyfiles = true)
}


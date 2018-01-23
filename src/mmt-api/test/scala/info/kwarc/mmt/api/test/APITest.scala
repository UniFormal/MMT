package info.kwarc.mmt.api.test

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.test.utils._
import info.kwarc.mmt.api.utils.URI


class APITest extends MMTIntegrationTest(
  "MMT/urtheories"
)() {
  bootstrapTests()

  // extensions should load
  shouldLoadExtensions()
  shouldInstallArchives()

  it should "get a Constant" in {
    lazy val brackets = (DPath(URI.http colon "cds.omdoc.org") / "mmt") ? "mmt" ? "brackets"
    controller.getConstant(brackets)
  }
}

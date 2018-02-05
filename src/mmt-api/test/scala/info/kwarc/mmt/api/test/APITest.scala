package info.kwarc.mmt.api.test

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.test.utils._
import info.kwarc.mmt.api.utils.URI


class APITest extends MMTIntegrationTest(
  "MMT/urtheories"
  ,"MMT/LATIN"
  ,"MMT/LFX"
  ,"MitM/smglom"
  ,"MitM/interfaces"
  ,"alignments/Public"
  ,"Mizar/MML"
  ,"HOLLight/Basic"
  ,"PVS/Prelude"
  ,"PVS/NASA"
)() {
  bootstrapTests()

  // extensions should load
  shouldLoadExtensions()
  shouldInstallArchives()

  it should "get a Constant" in {
    lazy val brackets = (DPath(URI.http colon "cds.omdoc.org") / "mmt") ? "mmt" ? "brackets"
    controller.getConstant(brackets)
  }

  it should "check all alignments" in {
    handleLine("extension info.kwarc.mmt.api.ontology.AddAlignments " + (contentFolder / "alignments" / "Public").toString)
  }
}

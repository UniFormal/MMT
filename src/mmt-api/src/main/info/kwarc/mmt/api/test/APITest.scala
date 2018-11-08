package info.kwarc.mmt.api.test

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.test.testers.TestArchive
import info.kwarc.mmt.api.utils.URI


object APITest extends MMTIntegrationTest(
  TestArchive("MMT/urtheories", hasDevel = true),
//  ,"MMT/LATIN"
//  ,"MMT/LFX"
//  ,"MitM/smglom"
//  ,"MitM/interfaces"
//  ,"Mizar/MML"
//  ,"HOLLight/basic"
//  ,"PVS/Prelude"
//  ,"PVS/NASA"
)(){
  def main(): Unit = {
    test("get a Constant", {
      lazy val brackets = (DPath(URI.http colon "cds.omdoc.org") / "mmt") ? "mmt" ? "brackets"
      controller.getConstant(brackets)
    })

    /*
    shouldHandleLine("lmh clone alignments/Public")

    test("check all alignments", {
      handleLine("log+ archive")
      handleLine("extension info.kwarc.mmt.api.ontology.AddAlignments " + (contentFolder / "alignments" / "Public").toString)
    })
    */
  }
}

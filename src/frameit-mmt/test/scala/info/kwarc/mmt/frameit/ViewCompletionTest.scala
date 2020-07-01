package info.kwarc.mmt.frameit

import info.kwarc.mmt.test.MMTIntegrationTest

object ViewCompletionTest extends MMTIntegrationTest(
  "FrameIT/frameworld"
)(){
  def main() {
    /* test("get a Constant", {
      lazy val brackets = (DPath(URI.http colon "cds.omdoc.org") / "mmt") ? "mmt" ? "brackets"
      controller.getConstant(brackets)
    })*/

    /*
    shouldHandleLine("lmh clone alignments/Public")

    test("check all alignments", {
      handleLine("log+ archive")
      handleLine("extension info.kwarc.mmt.api.ontology.AddAlignments " + (contentFolder / "alignments" / "Public").toString)
    })
    */
  }
}

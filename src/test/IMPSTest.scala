// Helpful: debug, object-checker

object IMPSTest extends MagicTest("debug"){//, "object-checker") {
  override def doFirst: Unit = {
    hl("extension info.kwarc.mmt.imps.IMPSImporter") // Register extension
    //hl("build MMT/LATIN mmt-omdoc foundations/imps") // Build LUTINS theory
    hl("build imps imps-omdoc")                      // Build Archive
  }

  override def run : Unit = {}
}

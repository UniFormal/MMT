// Helpful: debug, object-checker

object IMPSTest extends MagicTest(prefixes = "debug"){//}, "object-checker") {
  override def doFirst: Unit = {
    hl("extension info.kwarc.mmt.imps.IMPSImporter") // Register extension

    // Comment out, depending on your desired level of information.
    hl("log+ imps-omdoc-structure")
    hl("log+ imps-omdoc-overview")
    hl("log+ imps-omdoc-specifics")
    hl("log+ imps-omdoc-details")

    hl("build MMT/LATIN mmt-omdoc foundations/imps") // Build LUTINS theory
    hl("build imps imps-omdoc")                      // Build Archive
  }

  override def run : Unit = {}
}

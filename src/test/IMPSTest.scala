// Helpful: debug, object-checker

object IMPSTest extends MagicTest(prefixes = "debug"){//}, "object-checker") {
  override def doFirst(): Unit = {
    hl("extension info.kwarc.mmt.imps.IMPSImporter") // Register extension

    // Comment out, depending on your desired level of information.

    hl("log+ imps-omdoc-structure")  /* Only the most high-level view on the translation process */
    hl("log+ imps-omdoc-overview")   /* Includes information on files and declarations being worked on */
    hl("log+ imps-omdoc-specifics")  /* Elaborates on certain additional relevant details */
    hl("log+ imps-omdoc-details")    /* The most detailed information in every little bit in the pipeline*/
    //hl("log+ imps-omdoc-steps")      /* Also lists steps and intermediate results from importer internals */

    hl("build MMT/LATIN mmt-omdoc foundations/imps") // Build LUTINS theory
    hl("build imps imps-omdoc")                      // Build Archive
  }

  override def run() : Unit = {}
}

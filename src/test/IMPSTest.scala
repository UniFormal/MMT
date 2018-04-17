// Helpful: debug, object-checker

object IMPSTest extends JonasTest("debug") {
  override def doFirst: Unit = {
    hl("extension info.kwarc.mmt.imps.IMPSImporter")
    hl("build imps imps-omdoc")
  }

  override def run : Unit = {}
}

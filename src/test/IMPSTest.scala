object IMPSTest extends JonasTest("") {
  override def doFirst: Unit = {
    hl("extension info.kwarc.mmt.imps.IMPSImporter")
    hl("build imps imps-omdoc")
  }

  override def run : Unit = {}
}

object IMPSTest extends JonasTest("") {
  override def run: Unit = {
    hl("extension info.kwarc.mmt.imps.IMPSImporter")
    hl("build imps imps-omdoc")
  }
}

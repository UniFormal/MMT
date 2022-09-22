object GfImporterTest extends MagicTest(){
  override def doFirst(): Unit = {
    hl("extension info.kwarc.mmt.glf.GfImporter") // Register extension
    hl("log+ gf-omdoc-progress")
  }

  override def run() : Unit = {
    hl("build COMMA/GLF gf-omdoc")
  }
}

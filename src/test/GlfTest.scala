object GlfTest extends MagicTest(){
  override def doFirst: Unit = {
    hl("extension info.kwarc.mmt.glf.GlfServer") // Register extension
  }

  override def run : Unit = {}
}

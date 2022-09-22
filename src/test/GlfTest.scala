object GlfTest extends MagicTest(){
  override def doFirst(): Unit = {
    hl("extension info.kwarc.mmt.glf.GlfConstructServer") // Register extension
    hl("extension info.kwarc.mmt.glf.GlfBuildServer") // Register extension
  }

  override def run() : Unit = {}
}

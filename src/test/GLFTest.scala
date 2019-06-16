object GLFTest extends MagicTest(){
  override def doFirst: Unit = {
    hl("extension info.kwarc.mmt.glf.GLFServer") // Register extension
  }

  override def run : Unit = {}
}

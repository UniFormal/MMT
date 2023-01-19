object MBGenTest extends MagicTest() {
  override val serverport = None
  override val gotoshell = true
  override def doFirst(): Unit = {
    hl("extension info.kwarc.mmt.sql.codegen.CodeGenerator")
    hl("mbgen /Users/katja/Versioned/MBGen")
  }
  def run(): Unit = {}
}
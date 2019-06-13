object MBGenTest extends MagicTest() {
  override val serverport = None
  override val gotoshell = true
  def run: Unit = {
    hl("extension info.kwarc.mmt.sql.codegen.CodeGenerator")
    // then run: mbgen arguments
  }
}
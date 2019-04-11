object MathHubTest extends MagicTest("mathhub") {
  override val serverport = Some(9000)
  override val gotoshell = true
  def run: Unit = {
    hl("extension info.kwarc.mmt.mathhub.Server")
  }
}
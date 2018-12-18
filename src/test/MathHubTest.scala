object MathHubTest extends MagicTest("mathhub") {
  override val gotoshell = true
  def run: Unit = {
    hl("extension info.kwarc.mmt.mathhub.Server")
  }
}
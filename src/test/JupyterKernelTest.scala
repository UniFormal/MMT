object JupyterKernelTest extends MagicTest() {
  override val gotoshell = true
  def run(): Unit = {
    hl("extension info.kwarc.mmt.python.Py4JGateway 25333")
  }
}
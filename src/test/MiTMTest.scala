import info.kwarc.mmt.api.Path
import info.kwarc.mmt.api.symbols.FinalConstant

object MiTMTest extends MagicTest("lmfdb", "mitm", "scscp") {
  def run : Unit = {
    hl("scscp on 26134") // turn on scscp
    hl("log+ backend")
  }
}

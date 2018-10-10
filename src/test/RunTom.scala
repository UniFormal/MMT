import info.kwarc.mmt.api.Path
import info.kwarc.mmt.api.symbols.FinalConstant

object RunTom extends TomTest("lmfdb", "mitm", "scscp") {
  def run : Unit = {
    hl("scscp on 26134")
    //val thing = controller.get(Path.parse("http://www.lmfdb.org/db?hmf_hecke?3.3.49.1-27.1-a")).asInstanceOf[FinalConstant]
    //print(thing)
  }
}

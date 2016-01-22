import info.kwarc.mmt.api.Path
import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.odk

/**
  * Created by raupi on 28.12.15.
  */
object Lmfdbtest {
  def main (args: Array[String]) {
    println("Start...")
    val controller = new Controller
    """log console
       mathpath archive ../../MathHub/
       extension info.kwarc.mmt.lf.Plugin
       extension info.kwarc.mmt.odk.Plugin
    """.split("\n").foreach(controller.handleLine(_))

    val c = controller.get(Path.parse("http://www.lmfdb.org/?elliptic_curves?11a1"))
    val d = controller.get(Path.parse("http://www.lmfdb.org/?elliptic_curves?35a2"))
    println(c)
    println(d)
  }
}

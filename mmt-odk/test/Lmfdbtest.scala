import info.kwarc.mmt.api.Path
import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.objects.Context
import info.kwarc.mmt.api.symbols.FinalConstant
import info.kwarc.mmt.lf.Apply
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

    val c = controller.get(Path.parse("http://www.lmfdb.org/db/elliptic_curves?curves?11a1")).asInstanceOf[FinalConstant]
    val d = controller.get(Path.parse("http://www.lmfdb.org/db/elliptic_curves?curves?35a2")).asInstanceOf[FinalConstant]
    println(c)
    println(d)
    val cond = controller.get(Path.parse("http://www.lmfdb.org/omf?conductor?conductor")).asInstanceOf[FinalConstant]
    val res1 = controller.simplifier.apply(Apply(cond.toTerm,c.toTerm),Context.empty)
    val res2 = controller.simplifier.apply(Apply(cond.toTerm,d.toTerm),Context.empty)
    println(res1)
    println(res2)
  }
}

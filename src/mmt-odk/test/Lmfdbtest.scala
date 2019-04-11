import info.kwarc.mmt.api.{NamespaceMap, Path}
import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.objects.Context
import info.kwarc.mmt.api.symbols.{FinalConstant, RuleConstant}
import info.kwarc.mmt.lf.Apply
import info.kwarc.mmt.odk

object Lmfdbtest {
  def main (args: Array[String]) {
    println("Start...")
    val controller = new Controller
    """log console
       log+ impl-rule-gen
       log+ simplifier

       mathpath archive ../../MathHub/

       server on 8080
       extension info.kwarc.mmt.odk.Plugin
    """.split("\n").foreach(controller.handleLine(_))

    val c = controller.get(Path.parse("http://www.lmfdb.org/db/elliptic_curves?curves?11a1")).asInstanceOf[FinalConstant]
    val d = controller.get(Path.parse("http://www.lmfdb.org/db/elliptic_curves?curves?35a2")).asInstanceOf[FinalConstant]
    println(c)
    println(d)

    val p = Path.parseM("http://www.lmfdb.org/schema/elliptic_curves?curves", NamespaceMap.empty)


    val cond = controller.get(Path.parse("http://www.lmfdb.org/schema/elliptic_curves?curves?conductor")).asInstanceOf[FinalConstant]
    val res1 = controller.simplifier.apply(Apply(cond.toTerm,c.toTerm),SimplificationUnit(Context(p), false, true))
    val res2 = controller.simplifier.apply(Apply(cond.toTerm,d.toTerm),SimplificationUnit(Context(p), false, true))
    println(res1)
    println(res2)

   // controller.handleLine("build ODK/GAP gap-omdoc gaptypes.json") //gaptypes.json")
  }
}

import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.libraries.Library
import info.kwarc.mmt.api.modules.{Theory, View}
import info.kwarc.mmt.api.objects.Context
import info.kwarc.mmt.api.{NamespaceMap, Path}
import info.kwarc.mmt.lf.Beta
import info.kwarc.mmt.lf.comptrans.{CompositionalTotalMorphism, CompositionalTranslation}

/**
  * @author Navid
  */
object CompTransTest extends MagicTest("debug") {
  override def run(): Unit = {
    implicit val ctrl: Controller = controller
    implicit val library: Library = controller.library

    val doc = Path.parseD("latin:/playground/comptrans", NamespaceMap.empty)
    val dom = controller.getTheory(doc ? "HTyped")
    val cod = controller.getTheory(doc ? "STyped")
    val view = controller.getModule(doc ? "v").asInstanceOf[View]

    val mor = new CompositionalTotalMorphism(view.toTerm)
    val logrel = new CompositionalTranslation(List(mor), p => {
      None
    })

    printExpTypes(dom, mor)
    printExpTypes(dom, logrel)
    sys.exit(0)
  }

  def printExpTypes(dom: Theory, trans: CompositionalTranslation)(implicit ctrl: Controller): Unit = {
    println("\n--------------------------------------")
    println(s"Comptrans ${trans}\n")
    dom.getConstants.foreach(c => {
      println(s"${c.name} : ${trans.getExpected(Context.empty, c.toTerm, c.tp.get).map(Beta.reduce).map(_.toStr(true))}")
    })
    println("\n--------------------------------------")
  }
}

import LMFDBTest.controller
import info.kwarc.mmt.api.ontology.{DeclarationTreeExporter, DependencyGraphExporter, PathGraphExporter}
import info.kwarc.mmt.api.web.JSONBasedGraphServer
//import info.kwarc.mmt.argsemcomp
//import MagicTest.home
//import info.kwarc.mmt.api.utils.File
//import info.kwarc.mmt.imps


object Diagramtester extends MagicTest() {
  override def doFirst(): Unit = {
    super.doFirst()
    controller.handleLine("extension info.kwarc.mmt.pvs.PVSImporter")
  }
  def run() : Unit = {
    //println(MagicTest.archiveRoot)
    //List(File(System.getProperty("user.home") / "MMT" / "myformalizations")find.(_exists).getOrElse(println("Does not exist"))
    hl("extension info.kwarc.mmt.argsemcomp.ArgumentationSemanticComputer")
    //val test = WebQuery("type=archivegraph&graphdata=MMT/urtheories&semantic=grounded")
    // println(test)
    //println( test("graphdata"))
    //val serve = new JSONBasedGraphServer()
  }
}

object Graphtester extends MagicTest("jgraph", "argcomp") {

  override def doFirst(): Unit = {
    super.doFirst()
    // Copied here because these lines were removed from MagicTest.
    // Please reevaluate if they are necessary. If in doubt, leave them. They are just slow.)
    controller.handleLine("extension info.kwarc.mmt.pvs.PVSImporter")
    controller.handleLine(("extension info.kwarc.mmt.api.ontology.AlignmentsServer " + alignmentspath).trim)
    controller.extman.addExtension(new DependencyGraphExporter)
    controller.extman.addExtension(new DeclarationTreeExporter)
    controller.extman.addExtension(new JSONBasedGraphServer)
    controller.extman.addExtension(new PathGraphExporter)
  }

  def run() : Unit = {
    //println(MagicTest.archiveRoot)
    //List(File(System.getProperty("user.home") / "MMT" / "myformalizations")find.(_exists).getOrElse(println("Does not exist"))
    hl("extension info.kwarc.mmt.argsemcomp.ArgumentationSemanticComputer")
    //val test = WebQuery("type=archivegraph&graphdata=MMT/urtheories&semantic=grounded")
    // println(test)
    //println( test("graphdata"))
    //val serve = new JSONBasedGraphServer()
  }
}

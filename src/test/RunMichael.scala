import Graphtester.controller
import info.kwarc.mmt.api.ontology.{DeclarationTreeExporter, DependencyGraphExporter, PathGraphExporter}
import info.kwarc.mmt.api.web.JSONBasedGraphServer
import info.kwarc.mmt.api.{NamespaceMap, Path}
import info.kwarc.mmt.got.GraphOptimizationTool
import info.kwarc.mmt.jedit.MMTOptimizationAnnotationReader

object RunMichael extends MagicTest {

  override def doFirst: Unit = {
    super.doFirst
    // Copied here because these lines were removed from MagicTest.
    // Please reevaluate if they are necessary. If in doubt, leave them. They are just slow.)
    controller.handleLine("extension info.kwarc.mmt.pvs.PVSImporter")
    controller.handleLine(("extension info.kwarc.mmt.api.ontology.AlignmentsServer " + alignmentspath).trim)
    controller.extman.addExtension(new DependencyGraphExporter)
    controller.extman.addExtension(new DeclarationTreeExporter)
    controller.extman.addExtension(new JSONBasedGraphServer)
    controller.extman.addExtension(new PathGraphExporter)
  }

  def run : Unit = {
    controller.extman.addExtension(new GraphOptimizationTool)
    val got : GraphOptimizationTool = controller.extman.get(classOf[GraphOptimizationTool]).head

    val list = Path.parseM("http://mydomain.org/testarchive/mmt-example?test_other",NamespaceMap.empty) :: Path.parseM("http://mydomain.org/testarchive/mmt-example?test_all",NamespaceMap.empty) :: Path.parseM("http://mydomain.org/testarchive/mmt-example?test_future",NamespaceMap.empty) :: Nil
    val starttime = System.currentTimeMillis()
    controller.handleLine("build testarchive got")
    controller.extman.addExtension(new MMTOptimizationAnnotationReader)
    val or : MMTOptimizationAnnotationReader = controller.extman.get(classOf[MMTOptimizationAnnotationReader]).head
    //println(list.head)
    //println(or(list.head))
    //println(got.toXML(got.findReplacements()))
    //println(got.toXML(got.findReplacements(list, true)))
    //println((System.currentTimeMillis()-starttime)/1000)
  }
}

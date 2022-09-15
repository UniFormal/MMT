import info.kwarc.mmt._
import MitM.MitM._
import odk._
import Sage._
import GAP._
import Graphtester.controller
import Singular._
import info.kwarc.mmt.api.ontology.{DeclarationTreeExporter, DependencyGraphExporter, PathGraphExporter}
import info.kwarc.mmt.api.web.JSONBasedGraphServer

object MitMTest extends MagicTest("lmfdb", "mitm", "scscp") {
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

  def run(): Unit = {
    // FR: systems are loaded ODK plugin, see file Config/Actions.scala, only warmup is needed
    // load the (default) configuration
    // hl("mitm use")
    hl("mitm warmup")
    
    // turn on scscp on localhost:26134
    hl("scscp on 26134")
    
    val gap = controller.extman.get(classOf[GAPSystem]).head
    val sage = controller.extman.get(classOf[SageSystem]).head
    val singular = controller.extman.get(classOf[SingularSystem]).head
    
    implicit val trace = new MitM.VRESystem.MitMComputationTrace(None)
    
    singular.call(tt)
    
    println(trace.toString(t => controller.presenter.asString(t)))
    sys.exit()
  }
}

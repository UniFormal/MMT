import Graphtester.controller
import LMFDBTest.controller
import info.kwarc.mmt.api.ontology.{DeclarationTreeExporter, DependencyGraphExporter, PathGraphExporter}
import info.kwarc.mmt.api.web.JSONBasedGraphServer

object MathHubTest extends MagicTest("mathhub") {
  override val serverport = Some(9000)
  override val gotoshell = true

  override def doFirst: Unit = {
    super.doFirst()
    // Copied here because these lines were removed from MagicTest.
    // Please reevaluate if they are necessary. If in doubt, leave them. They are just slow.)
    controller.handleLine("extension info.kwarc.mmt.pvs.PVSImporter")
    controller.handleLine("extension info.kwarc.mmt.api.archives.MWSHarvestExporter")
    controller.extman.addExtension(new DependencyGraphExporter)
    controller.extman.addExtension(new DeclarationTreeExporter)
    controller.extman.addExtension(new JSONBasedGraphServer)
    controller.extman.addExtension(new PathGraphExporter)
  }

  def run: Unit = {
    hl("extension info.kwarc.mmt.mathhub.Server")
  }
}
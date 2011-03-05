package info.kwarc.mmt.api.tntplugin
import jomdoc._
import jomdoc.frontend._
import jomdoc.backend._
import info.kwarc.tntbase.pluginapi._

/** TNT plugin that presents an MMT expressions using LF and MathML notations */
class TNTPresenter extends Controller(libraries.NullChecker, NullReport) with TntIntegrationPlugin[String] {
   def init(lookup : TntPluginDocumentResolver[String]) {
      backend.addStore(TNTLookup(lookup))
      base = DPath(utils.mmt.baseURI)
   }
   def performAction(s : String, rev : Long, con : TntPluginContext) : String = {
      val path = Path.parse(s, base)
      val nset = Path.parseM("foundations/lf/mathml.omdoc?mathml", base)
      Respond(Present(Get(path), nset), "").get(this).toString
   }
}

package info.kwarc.mmt.api.tntplugin
import info.kwarc.mmt.api._
import frontend._
import backend._
import utils._

import info.kwarc.tntbase.pluginapi._

import scala.xml.NodeSeq

//import scala.collection.jcl.Conversions.convertList

/** TNT plugin that validates an MMT document using the default foundation */
class TNTValidation extends Controller with TntIntegrationPlugin[String] {
   def init(lookup : TntPluginDocumentResolver[String]) {
      backend.addStore(TNTLookup(lookup))
   }
   def performAction(s : String, rev : Long, con : TntPluginContext) : String = {
      val p = Path.parse(s, base)
      val files = con.getValue("name").asInstanceOf[java.util.List[String]] // list of files in current transaction
      get(p)
      val deps = Respond(Deps(p.doc), "").get(this) //TODO does not work anymore as dependencies are not stored by default
      deps.toString
   }
}

/** implementation of Storage in terms of TntPluginDocumentResolver provided by TNTBase */
case class TNTLookup(lookup : TntPluginDocumentResolver[String]) extends Storage {
   val base = utils.URI("http", "cds.omdoc.org", Nil)
   def get(path : Path)(implicit cont: (URI,NodeSeq) => Unit) {
	   val content = lookup.resolveDocument("/" + Storage.getSuffix(base, path.doc.uri), null)
      if (content == null) throw BackendError(path)
	   val src = scala.io.Source.fromString(content)
	   val node = scala.xml.parsing.ConstructingParser.fromSource(src, false).document()
	   cont(path.doc.uri, node)
   }
}

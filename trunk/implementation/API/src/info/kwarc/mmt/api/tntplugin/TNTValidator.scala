package info.kwarc.mmt.api.tntplugin
import jomdoc._
import jomdoc.frontend._
import jomdoc.backend._
import info.kwarc.tntbase.pluginapi._
//import scala.collection.jcl.Conversions.convertList

/** TNT plugin that validates an MMT document using the default foundation */
class TNTValidation extends {
   val checker = new libraries.FoundChecker(libraries.DefaultFoundation)
} with Controller(checker, NullReport) with TntIntegrationPlugin[String] {
   def init(lookup : TntPluginDocumentResolver[String]) {
      backend.addStore(TNTLookup(lookup))
   }
   def performAction(s : String, rev : Long, con : TntPluginContext) : String = {
      val p = Path.parse(s, base)
      val files = con.getValue("name").asInstanceOf[java.util.List[String]] // list of files in current transaction
      get(p)
      val deps = Respond(Deps(p.doc, true), "").get(this)
      deps.toString
   }
}

/** implementation of Storage in terms of TntPluginDocumentResolver provided by TNTBase */
case class TNTLookup(lookup : TntPluginDocumentResolver[String]) extends Storage("http", "cds.omdoc.org", "/") {
   def get(path : Path, reader : Reader) {
	   val content = lookup.resolveDocument("/" + getSuffix(path.doc.uri), null)
       if (content == null) throw backend.NotFound(path)
	   val src = scala.io.Source.fromString(content)
	   val node = scala.xml.parsing.ConstructingParser.fromSource(src, false).document();
	   reader.readDocuments(path.doc, node);
   }
}

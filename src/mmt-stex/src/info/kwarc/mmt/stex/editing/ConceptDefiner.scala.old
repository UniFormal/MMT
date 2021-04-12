package info.kwarc.mmt.stex.editing
/*
import info.kwarc.mmt.api._
import web._
import utils._
import ontology._
import symbols._

/*
 * load this extension: extension  info.kwarc.mmt.stex.editing.ConceptDefiner
 * make MMT load all names in all known archives: extension info.kwarc.mmt.api.ontology.RelationalReader
 *
 */

class ConceptDefiner extends ServerExtension("define") {

  // optional initialization
  override def start(args: List[String]) {
    controller.handleLine("extension info.kwarc.mmt.api.ontology.RelationalReader")
  }

  def apply(request: ServerRequest): ServerResponse = {

     // get all archives and the constant names defined in them
     val nameLists = controller.backend.getArchives map {a =>
       val paths = controller.depstore.queryList(DPath(a.narrationBase), Transitive(+ Declares)*HasType(IsConstant))
       (a, paths)
     }

     // examples for how to retrieve constant declarations and inspects them
     nameLists.map {case (a,names) =>
       names.headOption map {p => controller.globalLookup.get(p) match {
         case c: Constant => c.df match {
           case Some(d) => println(d)
           case None =>
         }
         case _ =>
       }}
     }

     import JSONConversions._
     val json: JSONArray = nameLists map {case (a,names) =>
       JSONObject(
           "archive" -> JSONString(a.narrationBase.toString),
           "names"   -> JSONArray(names.map(n => JSONString(n.toString)) :_*)
        )
     }

     ServerResponse.JsonResponse(json)
  }
}


 */
package info.kwarc.mmt.glf
import info.kwarc.mmt.api.modules.{Theory, View}
import info.kwarc.mmt.api.symbols.Constant
import info.kwarc.mmt.api.uom.SimplificationUnit
import info.kwarc.mmt.api.{DPath, MPath}
import info.kwarc.mmt.api.utils.{JSON, JSONArray, JSONBoolean, JSONObject, JSONString, URI}
import info.kwarc.mmt.api.web.{ServerError, ServerExtension, ServerRequest, ServerResponse}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/*
  Technically, the GlfConstructServer simply applies a view to particular terms.
  This is used to apply the semantics construction (a view) to a GF parse tree (which can be seen as an LF term).
 */

class GlfConstructServer extends ServerExtension("glf-construct"){
  def apply(request: ServerRequest): ServerResponse = {
    val query: GlfConstructQuery = GlfConstructQuery.fromJSON(request.body.asJSON)

    val view: Option[View] = query.semanticsView.map(controller.getO(_) match {
      case Some(v: View) => controller.simplifier.apply(v); v
      case None => return errorResponse("Could not find view " + query.semanticsView)
      case _ => return errorResponse(query.semanticsView + " does not appear to be a view")
    })

    val langTheo = if (query.languageTheory.isEmpty) {
      view.getOrElse(return errorResponse("Neither language theory nor semantics view provided")).from.toMPath
    } else {
      query.languageTheory.get
    }

    val theory: Theory = controller.getO(langTheo) match {
      case Some(th: Theory) => th
      case None => return errorResponse("Could not find theory " + langTheo)
      case _ => return errorResponse(langTheo + " does not appear to be a theory")
    }

    val theoryMap: mutable.Map[String, Constant] = mutable.Map()
    val inclSet: mutable.Set[MPath] = mutable.Set() // already included theories
    def fillTheoryMap(mpath: MPath): Unit = {
      if (inclSet.contains(mpath)) return
      else inclSet.add(mpath)

      controller.get(mpath) match {
        case t: Theory =>
          for (incl <- t.getIncludesWithoutMeta) fillTheoryMap(incl)
          for (const <- t.getConstants) theoryMap.put(const.name.toString, const)
        case _ => new Exception(mpath.toString + "doesn't appear to be a theory")
      }
    }

    try {
      fillTheoryMap(theory.toTerm.toMPath)
    } catch {
      case ex: Exception => return errorResponse(ex.getMessage)
    }

    /* val theorymap : Map[String, Constant] = {
      controller.simplifier(theory)
      val consts = theory.getConstants ::: theory.getIncludes.map(controller.get).collect {
        case t : Theory =>
          controller.simplifier(t)
          t.getConstants
      }.flatten
      HashMap(consts.map(c => (c.name.toString,c)):_*)
    }
    */

    val trees = query.asts
      .map(GfAST.parseAST)
      .map(_.toOMDocRec(theoryMap.toMap))
      .map(t => view match {
        case Some(v) => controller.library.ApplyMorphs(t, v.toTerm)
        case None => t })
      .map(t => if (query.simplify) controller.simplifier(t,
        SimplificationUnit(theory.getInnerContext,expandDefinitions=query.deltaExpansion,fullRecursion=true)) else t)
      .distinct

    ServerResponse.JsonResponse(JSONArray(trees.map(t => JSONString(controller.presenter.asString(t))) : _*))
  }

  private def errorResponse(message : String) : ServerResponse = {
    ServerResponse.JsonResponse(
      JSONArray(JSONString(message))
    )
  }
}

class GlfConstructQuery(val asts : List[String],
                        val languageTheory : Option[MPath],
                        val semanticsView : Option[MPath],
                        val simplify : Boolean,
                        val deltaExpansion : Boolean)

object GlfConstructQuery {
  def fromJSON(json : JSON) : GlfConstructQuery = {
    var asts = ArrayBuffer[String]()
    var langTheo : Option[String] = None
    var semView : Option[String] = None
    var simplify = true
    var deltaExpansion = false

    json match {
      case JSONObject(map) =>
        for (entry <- map) {
          entry match {
            case (JSONString("ASTs"), array : JSONArray) =>
              for (jsonstr <- array) {
                jsonstr match {
                  case JSONString(s) => asts += s
                  case _ => ServerError("Invalid JSON: ASTs should be list of strings")
                }
              }
            case (JSONString("languageTheory"), JSONString(value)) => langTheo = Some(value)
            case (JSONString("semanticsView"), JSONString(value)) => semView = Some(value)
            case (JSONString("simplify"), JSONBoolean(value)) => simplify = value
            case (JSONString("deltaExpansion"), JSONBoolean(value)) => deltaExpansion = value
            case (key, _) => throw ServerError("Invalid JSON: can't handle entry '" + key.toFormattedString("") + "'" )
          }
        }
      case _ => throw ServerError("Invalid JSON: Expected object")
    }
    val semanticsView : Option[MPath] = semView.map(s => DPath(URI(s)).toMPath)
    val languageTheory = langTheo.map(p => DPath(URI(p)).toMPath)
    new GlfConstructQuery(asts.toList, languageTheory, semanticsView, simplify, deltaExpansion)
  }
}

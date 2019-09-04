package info.kwarc.mmt.glf
import info.kwarc.mmt.api.modules.{Theory, View}
import info.kwarc.mmt.api.symbols.Constant
import info.kwarc.mmt.api.uom.SimplificationUnit
import info.kwarc.mmt.api.{DPath, MPath}
import info.kwarc.mmt.api.utils.{JSON, JSONArray, JSONBoolean, JSONObject, JSONString, URI}
import info.kwarc.mmt.api.web.{ServerError, ServerExtension, ServerRequest, ServerResponse}

import scala.collection.immutable.HashMap
import scala.collection.mutable.ArrayBuffer

class GlfServer extends ServerExtension("glf"){
  def apply(request: ServerRequest): ServerResponse = {
    val query : Query = Query.fromJSON(request.body.asJSON)

    val theory : Theory = controller.getO(query.languageTheory) match {
      case Some(th : Theory) => th
      case None => throw ServerError("Could not find theory " + query.languageTheory)
      case _ => throw ServerError(query.languageTheory + " does not appear to be a theory")
    }

    val view : Option[View] = query.semanticsView.map(controller.getO(_) match {
      case Some(v : View) => { controller.simplifier.apply(v) ; v }
      case None => throw ServerError("Could not find view " + query.semanticsView)
      case _ => throw ServerError(query.semanticsView + " does not appear to be a view")
    })

    val theorymap : Map[String, Constant] = {
      controller.simplifier(theory)
      val consts = theory.getConstants ::: theory.getIncludes.map(controller.get).collect {
        case t : Theory =>
          controller.simplifier(t)
          t.getConstants
      }.flatten
      HashMap(consts.map(c => (c.name.toString,c)):_*)
    }

    val trees = query.asts
      .map(GfAST.parseAST)
      .map(_.toOMDocRec(theorymap))
      .map(t => view match {
        case Some(v) => controller.library.ApplyMorphs(t, v.toTerm)
        case None => t })
      .map(t => if (query.simplify) controller.simplifier(t,
        SimplificationUnit(theory.getInnerContext,expandDefinitions=query.deltaExpansion,fullRecursion=true)) else t)
      .distinct

    ServerResponse.JsonResponse(JSONArray(trees.map(t => JSONString(controller.presenter.asString(t))) : _*))
  }
}

class Query(val asts : List[String],
            val languageTheory : MPath,
            val semanticsView : Option[MPath],
            val simplify : Boolean,
            val deltaExpansion : Boolean)

object Query {
  def fromJSON(json : JSON) : Query = {
    var asts = ArrayBuffer[String]()
    var langTheo : Option[String] = None
    var semView : Option[String] = None
    var simplify = true
    var deltaExpansion = false

    json match {
      case JSONObject(map) =>
        for (entry <- map) {
          entry match {
            case (JSONString("ASTs"), array : JSONArray) => {
              for (jsonstr <- array) {
                jsonstr match {
                  case JSONString(s) => asts += s
                  case _ => ServerError("Invalid JSON: ASTs should be list of strings")
                }
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
    val languageTheory = DPath(URI(langTheo.getOrElse(throw ServerError("no language theory provided")))).toMPath
    val semanticsView : Option[MPath] = semView.map(s => DPath(URI(s)).toMPath)
    new Query(asts.toList, languageTheory, semanticsView, simplify, deltaExpansion)
  }
}

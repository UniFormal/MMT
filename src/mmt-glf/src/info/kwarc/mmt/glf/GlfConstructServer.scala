package info.kwarc.mmt.glf
import info.kwarc.mmt.api.modules.{Theory, View}
import info.kwarc.mmt.api.objects.{OMBIND, OMLIT, OMS, OMV, Term}
import info.kwarc.mmt.api.symbols.Constant
import info.kwarc.mmt.api.uom.SimplificationUnit
import info.kwarc.mmt.api.{DPath, LocalName, MPath}
import info.kwarc.mmt.api.utils.{JSON, JSONArray, JSONBoolean, JSONInt, JSONObject, JSONString, URI}
import info.kwarc.mmt.api.web.{ServerError, ServerExtension, ServerRequest, ServerResponse}
import info.kwarc.mmt.lf.elpi.{BaseConstantHandler, ELPIExporter}
import info.kwarc.mmt.lf.{ApplySpine, Arrow, Lambda, Pi}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/*
  Future TODO: Remove protocol version 1.
  This should only be done when the GLIF reimplementation is well-established
  and the original implementation has been retired.
 */

/*
  Technically, the GlfConstructServer simply applies a view to particular terms.
  This is used to apply the semantics construction (a view) to a GF parse tree (which can be seen as an LF term).
 */

class GlfConstructServer extends ServerExtension("glf-construct") {
  def apply(request: ServerRequest): ServerResponse = {
    val query: GlfConstructQuery = GlfConstructQuery.fromJSON(request.body.asJSON)

    val view: Option[View] = query.semanticsView.map(controller.getO(_) match {
      case Some(v: View) => controller.simplifier.apply(v); v
      case None => return errorResponse("Could not find view " + query.semanticsView, query.version > 1)
      case _ => return errorResponse(query.semanticsView + " does not appear to be a view", query.version > 1)
    })

    val langTheo = if (query.languageTheory.isEmpty) {
      view.getOrElse(return errorResponse("Neither language theory nor semantics view provided", query.version > 1)).from.toMPath
    } else {
      query.languageTheory.get
    }

    val theory: Theory = controller.getO(langTheo) match {
      case Some(th: Theory) => th
      case None => return errorResponse("Could not find theory " + langTheo, query.version > 1)
      case _ => return errorResponse(langTheo + " does not appear to be a theory", query.version > 1)
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
      case ex: Exception => return errorResponse(ex.getMessage, query.version > 1)
    }

    try {
      val trees = query.asts
        .map(GfAST.parseAST)
        .map(_.toOMDocRec(theoryMap.toMap))
        .map(t => view match {
          case Some(v) => controller.library.ApplyMorphs(t, v.toTerm)
          case None => t
        })
        .map(t => if (query.simplify) controller.simplifier(t,
          SimplificationUnit(theory.getInnerContext, expandVarDefs = query.deltaExpansion, expandConDefs = query.deltaExpansion, fullRecursion = true)) else t)
        .map(t => removeFakeLambdas(t, Set()))
        .distinct

      val elpiresult = JSONArray(trees.map(t => JSONString(ELPIExporter.translateTerm(t).toELPI())): _*)
      val mmtresult = JSONArray(trees.map(t => JSONString(controller.presenter.asString(t))): _*)
      if (query.version == 1) {
        if (query.toElpi)
          ServerResponse.JsonResponse(elpiresult)
        else
          ServerResponse.JsonResponse(mmtresult)
      } else {
        ServerResponse.JsonResponse(JSONObject(
          ("isSuccessful", JSONBoolean(true)),
          ("result", JSONObject(
            ("elpi", elpiresult),
            ("mmt", mmtresult),
          )),
          ("errors", JSONArray()),
        ))
      }
    } catch {
      case ex: LangTheoryIncomplete => return errorResponse(ex.getMessage, query.version > 1)
    }
  }

  private def errorResponse(message: String, newresponse: Boolean): ServerResponse = {
    if (newresponse) {
      ServerResponse.JsonResponse(JSONObject(
        ("isSuccessful", JSONBoolean(false)),
        ("errors", JSONArray(JSONString(message))),
      ))
    } else {
      ServerResponse.JsonResponse(
        JSONArray(JSONString(message))
      )
    }
  }

  private def removeFakeLambdas(t: Term, bound : Set[String]): Term = {
    t match {
      case OMS(p) =>
        if (bound.contains(p.toMPath.toGlobalName.toString)) {
          OMV(p.name.toStr(true))
        }
        else {
          OMS(p)
        }
      case OMV(n) =>
        OMV(n)
      case OMBIND(b, c, s) =>
        OMBIND(b, c, removeFakeLambdas(s, bound))
      case ApplySpine(OMS(p), List(tp1, _, OMS(v), b)) =>
        if (p.toMPath.toGlobalName.toString.equals("http://mathhub.info/COMMA/GLF?fakeLambda?fake_lambda")) {   // apparently, it's "[logic]/fake_lambda"
          val name = v.name.toStr(true)
          Lambda(LocalName(name), tp1, removeFakeLambdas(b, bound + v.toMPath.toGlobalName.toString))
        } else {
          t match {
            case ApplySpine(f, args) =>
              ApplySpine(removeFakeLambdas(f, bound), args.map(a => removeFakeLambdas(a, bound)): _*)
          }
        }
      case ApplySpine(f, args) =>
        ApplySpine(removeFakeLambdas(f, bound), args.map(a => removeFakeLambdas(a, bound)): _*)
      case Arrow(_, _) =>
        throw new GlfException("Didn't expect arrow in result of semantics construction")
      case Pi(_, _, _) =>
        throw new GlfException("Didn't expect pi in result of semantics construction")
      case OMLIT(v, rt) => OMLIT(v, rt)
      case _ => throw new GlfException("unknown term: " + t)
    }
  }
}

class GlfConstructQuery(val asts : List[String],
                        val languageTheory : Option[MPath],
                        val semanticsView : Option[MPath],
                        val simplify : Boolean,
                        val deltaExpansion : Boolean,
                        val toElpi : Boolean,
                        val version : Int)

object GlfConstructQuery {
  def fromJSON(json : JSON) : GlfConstructQuery = {
    var asts = ArrayBuffer[String]()
    var langTheo : Option[String] = None
    var semView : Option[String] = None
    var simplify = true
    var deltaExpansion = false
    var toElpi = false
    var version = 1

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
            case (JSONString("toElpi"), JSONBoolean(value)) => toElpi = value
            case (JSONString("version"), JSONInt(value)) => version = value.toInt
            case (key, _) => throw ServerError("Invalid JSON: can't handle entry '" + key.toFormattedString("") + "'" )
          }
        }
      case _ => throw ServerError("Invalid JSON: Expected object")
    }
    val semanticsView : Option[MPath] = semView.map(s => DPath(URI(s)).toMPath)
    val languageTheory = langTheo.map(p => DPath(URI(p)).toMPath)
    new GlfConstructQuery(asts.toList, languageTheory, semanticsView, simplify, deltaExpansion, toElpi, version)
  }
}

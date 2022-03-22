package info.kwarc.mmt.glf
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.checking.Solver
import info.kwarc.mmt.api.documents.InterpretationInstructionContext
import info.kwarc.mmt.api.modules.Theory
import info.kwarc.mmt.api.objects.{Context, OMS, Term}
import info.kwarc.mmt.api.parser.{Parser, ParsingUnit, SourceRef}
import info.kwarc.mmt.api.uom.SimplificationUnit
import info.kwarc.mmt.api.utils.{JSON, JSONArray, JSONBoolean, JSONInt, JSONObject, JSONString, URI}
import info.kwarc.mmt.api.web.{ServerError, ServerExtension, ServerRequest, ServerResponse}
import info.kwarc.mmt.lf.{ApplySpine, Lambda}

import scala.collection.mutable.ArrayBuffer

/*
  Highly experimental (and overly specific) code for accumulating expressions in a theory
 */


class GlfAccumulateServer extends ServerExtension("glf-accumulate") {

  def apply(request: ServerRequest): ServerResponse = {
    val query = GlfAccumulateQuery.fromJSON(request.body.asJSON)
//     val metatheory = controller.getO(query.metatheory) match { case Some(t: Theory) => t }

    val theory = Theory(query.theorypath.parent.toMPath.toDPath, query.theorypath.name, Some(query.metatheory),
      modules.Theory.noParams, modules.Theory.noBase)
    controller.add(theory)


    val parser = controller.extman.get(classOf[Parser], "mmt").get
    def parse(termstring: String): Term = {
      val pu = ParsingUnit(SourceRef.anonymous(termstring), Context(theory.path), termstring,
        InterpretationInstructionContext(NamespaceMap(query.metatheory)), None)
      parser(pu)(ErrorThrower).toTerm
    }

    def simplify(term: Term): Term = {
      controller.simplifier(term, SimplificationUnit(theory.getInnerContext, expandDefinitions = false, fullRecursion = true))
      // Solver.infer(controller, Context(theory.path), term, None).get
      term
    }

    if (query.mode == "default") {
      var counter = 0
      for (termstring <- query.terms) {
        val tm = simplify(parse(termstring))
        val c = symbols.Constant(theory.toTerm, LocalName("x" + counter.toString), Nil, Some(tm), None, None)
        theory.add(c)
        counter += 1
      }
    } else if (query.mode == "events") {
      val ded = parse("ded")
      var sentencecounter = 0
      var eventcounter = 0
      for (termstring <- query.terms) {
        var tm = simplify(parse(termstring))
        tm match {
          case ApplySpine(OMS(op), List(Lambda(x, tp, content))) =>
            sentencecounter += 7
            if (controller.globalLookup.getConstant(op).rl.contains("EventIntroduction")) {
              val newevent = symbols.Constant(theory.toTerm, LocalName("e" + eventcounter.toString), Nil, Some(tp), None, None)
              eventcounter += 1
              theory.add(newevent)
              sentencecounter += 20
              tm = simplify(ApplySpine(Lambda(x, tp, content), newevent.toTerm))
            }
          case _ => { }
        }
        val c = ApplySpine(ded, tm)
        theory.add(
          symbols.Constant(theory.toTerm, LocalName("s" + sentencecounter.toString), Nil, Some(c), None, None)
        )
        sentencecounter += 1
      }

    }

    val newtheory = controller.get(theory.path)
    controller.endAdd(theory)

    ServerResponse.JsonResponse(JSONObject(
      ("isSuccessful", JSONBoolean(true)),
      ("theorypath", JSONString(theory.path.toString)),
      ("theorypresentation", JSONString(newtheory.toString)),
      ("errors", JSONArray())
    ))
  }
}

class GlfAccumulateQuery(val terms : List[String],
                         val metatheory : MPath,
                         val theorypath : MPath,
                         val mode : String,
                         val version : Int)


object GlfAccumulateQuery {
  def fromJSON(json: JSON): GlfAccumulateQuery = {
    val terms = ArrayBuffer[String]()
    var metatheory: Option[String] = None
    var theorypath: Option[String] = None
    var mode: Option[String] = None
    var version = 1

    json match {
      case JSONObject(map) =>
        for (entry <- map) {
          entry match {
            case (JSONString("terms"), array : JSONArray) =>
              for (jsonstr <- array) {
                jsonstr match {
                  case JSONString(s) => terms += s
                  case _ => ServerError("Invalid JSON: terms should be list of strings")
                }
              }
            case (JSONString("metatheory"), JSONString(value)) => metatheory = Some(value)
            case (JSONString("theorypath"), JSONString(value)) => theorypath = Some(value)
            case (JSONString("mode"), JSONString(value)) => mode = Some(value)
            case (JSONString("version"), JSONInt(value)) => version = value.toInt
            case (key, _) => throw ServerError("Invalid JSON: can't handle entry '" + key.toFormattedString("") + "'")
          }
        }
      case _ => throw ServerError("Invalid JSON: Expected object")
    }
    new GlfAccumulateQuery(
      terms.toList,
      DPath(URI(metatheory.getOrElse(throw ServerError("No meta theory specified")))).toMPath,
      DPath(URI(theorypath.getOrElse(throw ServerError("No theory path specified")))).toMPath,
      mode.getOrElse("default"),
      version
    )
  }
}

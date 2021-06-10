package info.kwarc.mmt.glf

import info.kwarc.mmt.api.modules.Theory
import info.kwarc.mmt.api.symbols.{Constant, Declaration, PlainInclude, RuleConstant, Structure}
import info.kwarc.mmt.api.{DPath, MPath}
import info.kwarc.mmt.api.utils.{JSONArray, JSONBoolean, JSONInt, JSONObject, JSONString, URI}
import info.kwarc.mmt.api.web.{ServerError, ServerExtension, ServerRequest, ServerResponse}
import info.kwarc.mmt.lf.RuleMatcher
import info.kwarc.mmt.lf.elpi._

/*
  Future TODO: Remove protocol version 1.
  This should only be done when the GLIF reimplementation is well-established
  and the original implementation has been retired.
 */

class ElpiGenerationServer extends ServerExtension("glif-elpigen") {
  override def start(args: List[String]): Unit = {
    super.start(args)
  }

  private def errorResponse(message: String, version: Int): ServerResponse = {
    if (version == 1)
      ServerResponse.JsonResponse(JSONArray(JSONString(message)))
    else
      ServerResponse.JsonResponse(JSONObject(
        ("isSuccessful", JSONBoolean(false)),
        ("errors", JSONArray(JSONString(message))),
      ))
  }

  def apply(request: ServerRequest): ServerResponse = {
    var theoryPath : Option[MPath] = None
    var mode : String = "types"
    var followMeta : Boolean = false
    var followIncludes : Boolean = true
    var version : Int = 1
    request.body.asJSON match {
      case JSONObject(map) =>
        for (entry <- map) {
          entry match {
            case (JSONString("theory"), JSONString(value)) => theoryPath = Some(DPath(URI(value)).toMPath)
            case (JSONString("mode"), JSONString(value)) => mode = value
            case (JSONString("follow-meta"), JSONBoolean(value)) => followMeta = value
            case (JSONString("follow-includes"), JSONBoolean(value)) => followIncludes = value
            case (JSONString("version"), JSONInt(value)) => version = value.toInt
            case (key, _) => throw ServerError("Invalid JSON: can't handle entry '" + key.toFormattedString("") + "'")
          }
        }
      case _ => throw ServerError("Invalid JSON: Expected object")
    }

    val theory = theoryPath.map(controller.getO(_) match {
      case Some(t: Theory) => controller.simplifier.apply(t); t
      case None => return errorResponse("Could not find theory " + theoryPath, version)
      case _ => return errorResponse(theoryPath.toString + " does not appear to be a theory", version)
    })

    if (theory.isEmpty) return errorResponse("No theory specified", version)

    lazy val lup = controller.globalLookup
    lazy val ruleMatcher = new RuleMatcher(lup, List("Judgment", "TabMarker", "TabClosed"))

    val ctx = mode match {
      case "types" => new ElpiGenCtx(new ConstantHandlerSequence(List(new TypeHandler)), followMeta, followIncludes)
      case "simpleprover" =>
        new ElpiGenCtx(new ConstantHandlerSequence(List(
          new GeneratedFromHandler(controller),
          new MainJudgmentHandler(ruleMatcher),
          new ProductRuleHandler(ruleMatcher),
          new IterativeDeepeningHandler(ruleMatcher),
          new ProofTermHandler(ruleMatcher),
          new RuleUseHandler(ruleMatcher),
          new HandDownHandler(ruleMatcher, ""),
        )), followMeta, followIncludes)
      case _ => throw ServerError("Unkown mode '" + mode + "'")
    }
    val p = translateTheory(theory.get, ctx)
    if (version == 1)
      ServerResponse.JsonResponse(JSONString(ELPI.Program(p:_*).toELPI))
    else
      ServerResponse.JsonResponse(JSONObject(
        ("isSuccessful", JSONBoolean(true)),
        ("result", JSONString(ELPI.Program(p:_*).toELPI)),
        ("errors", JSONArray()),
      ))
  }

  private def translateTheory(thy: Theory, ctx:ElpiGenCtx): List[ELPI.Decl] = {
    val meta = if (ctx.followMeta && thy.meta.get.parent.toString != "http://cds.omdoc.org/urtheories") {
      translateTheory(controller.getO(thy.meta.get).get.asInstanceOf[Theory], ctx)
    } else Nil
    val decls = thy.getDeclarations flatMap (translateDeclaration(_, ctx))
    meta ::: decls
  }

  private def translateDeclaration(d: Declaration, ctx:ElpiGenCtx): List[ELPI.Decl] = {
    d match {
      case c: Constant => ctx.ch.handle(c)
      case PlainInclude(from,_) =>
        if (!ctx.followInclude) Nil
        else if (from.parent.toString == "http://cds.omdoc.org/urtheories") Nil
        else translateTheory(controller.getO(from).get.asInstanceOf[Theory], ctx)
      case _: RuleConstant =>
        Nil // ignored
      case _: Structure =>
        Nil // redundant after flattening
      case _ =>
        throw ELPIError("unknown declaration: " + d.path)
    }
  }
}

class ElpiGenCtx(val ch : ConstantHandler, val followMeta : Boolean, val followInclude : Boolean)


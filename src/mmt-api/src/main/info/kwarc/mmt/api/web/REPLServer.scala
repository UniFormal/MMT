package info.kwarc.mmt.api.web

import info.kwarc.mmt.api._
import frontend._
import documents._
import modules._
import symbols._
import objects._
import parser._
import checking._
import utils._

import scala.util.Try

/** stores the state of a content-inputing REPL session */
class REPLSession(val doc: Document, val id: String, interpreter: Interpreter) {
  private val path = doc.path
  override def toString = doc.toString
  private var currentScope: HasParentInfo = IsDoc(path)
  private val errorCont = ErrorThrower
  private var counter = 0

  /** parses a declaration in a specific point (default: current point) of the document associated with this session (also stores in in controller) */
  def parseStructure(s: String, scopeOpt: Option[HasParentInfo] = None): StructuralElement = {
    val buffer = ParsingStream.stringToReader(s)
    val scope = scopeOpt.getOrElse(currentScope)
    val ps = ParsingStream(path.uri, scope, NamespaceMap(doc.path), interpreter.format, buffer)
    val se = interpreter(ps)(errorCont)
    se match {
      case r: MRef => currentScope = IsMod(r.target, LocalName.empty)
      case m: DeclaredModule => currentScope = IsMod(m.path, LocalName.empty)
      case nm: NestedModule => currentScope = IsMod(nm.module.path, LocalName.empty)
      case _ =>
    }
    se
  }

  /** closes the current container element, e.g., a theory */
  def parseElementEnd {
    currentScope match {
      case IsMod(m,_) =>
        val newScope = if (m.name.length > 1) IsMod(m ^, LocalName.empty) else IsDoc(m ^^)
        currentScope = newScope
      case IsDoc(_) => throw GeneralError("no open module")
    }
  }

  /** like parseStructure but for objects; the object is stored it as the definiens of a [[Constant]] declaration */
  def parseObject(s: String, scopeOpt: Option[HasParentInfo] = None): Declaration = {
    val scope = scopeOpt.getOrElse(currentScope)
    val mpath = scope match {
      case IsMod(m, _) => m
      case _ => throw GeneralError("can only parse term inside a theory")
    }
    val sref = SourceRef(path.uri, SourceRegion.ofString(s))
    val context = Context(mpath)
    val pu = ParsingUnit(sref, context, s, NamespaceMap.empty)
    val cr = interpreter(pu)(errorCont)
    val term = cr.term
    val termS = interpreter.simplifier(term, context, true)
    val df = Some(termS)
    val tp = cr.solution.flatMap(_.getO(CheckingUnit.unknownType).flatMap(_.df))
    val name = LocalName("res" + counter)
    counter += 1
    Constant(OMMOD(mpath), name, Nil, tp, df, None)
  }
}

import ServerResponse._

class REPLServer extends ServerExtension("repl") {
  private lazy val presenter = controller.presenter

  private var sessions: List[REPLSession] = Nil

  def apply(request: ServerRequest): ServerResponse = {
    val response: REPLServerResponse = try {
      applyActual(request)
    } catch {
      case err: Error => REPLServerResponse(Some(err.toHTML), None, success = false)
      case e : Exception => REPLServerResponse(Some(ServerError("unknown error").setCausedBy(e).toHTML), None, success = false)
    }
    JsonResponse(response.toJSON)
  }

  // READING parameters from session
  private def sessionIDOpt(implicit request: ServerRequest)= request.headers.get("X-REPL-Session")
  private def sessionID(implicit request: ServerRequest) = sessionIDOpt.getOrElse(throw LocalError("Missing X-REPL-Session Header"))

  private def currentSessionOpt(implicit request: ServerRequest)= sessionIDOpt.flatMap(id => sessions.find(_.id == id))
  private def currentSession(implicit request: ServerRequest) = this.currentSessionOpt.getOrElse(throw LocalError("Unknown Session"))

  private def path(implicit request: ServerRequest) = DPath(mmt.baseURI) / "jupyter" / sessionID

  private def applyActual(implicit request: ServerRequest) : REPLServerResponse = request.query match {
    case "show" => getSessions
    case "clear" => clearSessions
    case "start" => startSession
    case "restart" => restartSession
    case "quit" => quitSession

    case _ => evalInSession
  }

  private def evalInSession(implicit request: ServerRequest) = {
    val input = request.body.asString.trim
    val firstPart = input.takeWhile(c => !c.isWhitespace)
    val rest = input.substring(firstPart.length)

    val message = firstPart match {
      case "end" =>
        // special case for closing the current container element (module etc.)
        currentSession.parseElementEnd
        Some("closed module")
      case "eval" =>
        val d = currentSession.parseObject(rest)
        controller.add(d)
        Some(presenter.asString(d))
      case "get" =>
        val p = Path.parse(rest, currentSession.doc.nsMap)
        val se = controller.get(p)
        Some(presenter.asString(se))
      case "content" | _ =>
        val toBeParsed = if (firstPart == "content") rest else input
        val se = currentSession.parseStructure(toBeParsed)
        Some(presenter.asString(se))
    }
    REPLServerResponse(message, None, session = Some(sessionID))
  }


  private def getSessions = REPLServerResponse(None, Some(sessions.map(_.id)))
  private def clearSessions = {
    sessions foreach deleteSession
    REPLServerResponse(Some("Sessions cleared"), None)
  }

  private def startSession(implicit request: ServerRequest) = {

    // no previous session of the same name should exist
    if(currentSessionOpt.nonEmpty){
      throw LocalError("Session already exists")
    }

    // create the session
    val id = sessionIDOpt.getOrElse(java.util.UUID.randomUUID().toString)
    createSession(path, id)

    // return the session id
    REPLServerResponse(Some(s"Created Session $id"), None, session = Some(id))
  }

  private def restartSession(implicit request: ServerRequest): REPLServerResponse = {
    currentSessionOpt foreach deleteSession
    createSession(path, sessionID)

    REPLServerResponse(Some(s"Restarted Session $sessionID"), None, session = Some(sessionID))
  }

  private def quitSession(implicit request: ServerRequest) = {
    sessions = sessions.filterNot(_.id == sessionID)
    controller.delete(path)

    REPLServerResponse(Some(s"Deleted session $sessionID"), None, session = Some(sessionID))
  }


  //
  // SESSION MANAGEMENT
  //

  private def createSession(path: DPath, id: String) : REPLSession = {
    val doc = new Document(path, root=true)
    controller.add(doc)
    val format = "mmt"
    val interpreter = controller.extman.get(classOf[Interpreter], format).getOrElse {
      throw LocalError("no parser found")
    }
    val s = new REPLSession(doc, id, interpreter)
    sessions ::= s
    s
  }

  private def deleteSession(s: REPLSession) {
    controller.delete(s.doc.path)
    sessions = sessions.filterNot(_.id == s.id)
  }
}

/** Response to a REPL Session */
case class REPLServerResponse(message: Option[String], sessions: Option[List[String]], session: Option[String] = None, success: Boolean = true) {
  def toJSON: JSON = REPLServerResponse.Converter.toJSON(this)
}

object REPLServerResponse {
  implicit object Converter extends JSONConverter[REPLServerResponse] {
    import JSONConverter._

    def toJSON(r: REPLServerResponse) = r match {
      case REPLServerResponse(message, sessions, session, success) =>
        val buffer = new JSONObjectBuffer

        buffer.add("message", message)
        buffer.add("sessions", sessions)
        buffer.add("session", session)
        buffer.add("success", success)

        buffer.result()
    }
    def fromJSONOption(j: JSON): Option[REPLServerResponse] = j match {
      case jo: JSONObject =>
        val r = new JSONObjectParser(jo)

        Try(
          REPLServerResponse(
            r.take[Option[String]]("message"),
            r.take[Option[List[String]]]("sessions"),
            r.take[Option[String]]("session"),
            r.take[Boolean]("success")
          )
        ).toOption
      case _ => None
    }
  }
}
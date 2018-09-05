package info.kwarc.mmt.api.web

import info.kwarc.mmt.api._
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

object REPLServer {
  abstract class Command
  // meta-commands managing the set of sessions
  case object Show extends Command
  case object Clear extends Command
  // meta-commands managing a single session
  case object Start extends Command
  case object Restart extends Command
  case object Quit extends Command
  // mathematically relevant commands
  case class Input(command: String) extends Command

  object Command {
    def parse(s: String): Command = {
      s match {
        case "start" => Start
        case "clear" => Clear
        case "show" => Show
        case "restart" => Restart
        case "quit" => Quit
        case s => Input(s)
      }
    }
  }

  /** Response by a REPL session after executing a [[Command]] */
  abstract class REPLResponse

  case class AdminResponse(message: String) extends REPLResponse

  abstract class ElementResponse extends REPLResponse {
    def element: StructuralElement
  }
  case class NewElement(element: StructuralElement) extends ElementResponse
  case class ExistingElement(element: StructuralElement) extends ElementResponse
}

import REPLServer._

class REPLServer extends ServerExtension("repl") {
  private lazy val presenter = controller.presenter

  private var sessions: List[REPLSession] = Nil

  def apply(request: ServerRequest): ServerResponse = {
    val response: REPLResponse = try {
      val input = (request.query + " " + request.body.asString).trim
      val command = Command.parse(input)
      val session = request.headers.get("x-repl-session")
      apply(session, command)
    } catch {
      case e : Exception => return ServerResponse.errorResponse(Error(e), "html")
    }
    TextResponse(response.toString)
  }

  def apply(session: Option[String], command: Command): REPLResponse = {
    applyActual(session, command)
  }

  private def getSessionOpt(id: String) = sessions.find(_.id == id)
  private def getSession(id: String) = getSessionOpt(id).getOrElse(throw LocalError("Unknown Session"))

  private def path(id: String): DPath = DPath(mmt.baseURI) / "jupyter" / id

  private def applyActual(idO: Option[String], command: Command) : REPLResponse = {
    implicit lazy val session = idO match {
      case None => throw LocalError("session needed")
      case Some(id) => getSession(id)
    }
    command match {
      case Show => getSessions
      case Clear => clearSessions
      case Start => startSession(idO.getOrElse(throw LocalError("No session provided")))
      case Restart => restartSession(session)
      case Quit => quitSession(session)
      case Input(s) => evalInSession(session, s)
    }
  }

  private def evalInSession(session: REPLSession, input: String) = {
    val firstPart = input.takeWhile(c => !c.isWhitespace)
    val rest = input.substring(firstPart.length).trim

    firstPart match {
      case "end" =>
        // special case for closing the current container element (module etc.)
        session.parseElementEnd
        AdminResponse("closed module")
      case "eval" =>
        val d = session.parseObject(rest)
        controller.add(d)
        NewElement(d)
      case "get" =>
        val p = Path.parse(rest, session.doc.nsMap)
        val se = controller.get(p)
        ExistingElement(se)
      case "content" | _ =>
        val toBeParsed = if (firstPart == "content") rest else input
        val se = session.parseStructure(toBeParsed)
        NewElement(se)
    }
  }


  private def getSessions = AdminResponse(sessions.map(_.id).mkString(", "))
  private def clearSessions = {
    sessions foreach deleteSession
    AdminResponse("Sessions cleared")
  }

  private def startSession(id: String) = {
    if (sessions.exists(_.id==id)){
      throw LocalError("Session already exists")
    }
    createSession(path(id), id)
    // return the session id
    AdminResponse(s"Created Session $id")
  }

  private def restartSession(session: REPLSession) = {
    val id = session.id
    deleteSession(session)
    createSession(path(id), id)
    AdminResponse(s"Restarted Session $id")
  }

  private def quitSession(session: REPLSession) = {
    val id = session.id
    deleteSession(session)
    controller.delete(session.doc.path)
    AdminResponse(s"Deleted session $id")
  }

  // SESSION MANAGEMENT

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

package info.kwarc.mmt.api.web

import info.kwarc.mmt.api._
import documents._
import modules._
import symbols._
import objects._
import parser._
import checking._
import info.kwarc.mmt.api.uom.SimplificationUnit
import utils._

import scala.util.Try

/** stores the state of a content-inputing REPL session */
class REPLSession(val doc: Document, val id: String, interpreter: TwoStepInterpreter, val errorCont: ErrorHandler) {
  private val path = doc.path
  override def toString = doc.toString
  private var currentScope: HasParentInfo = IsDoc(path)
  private var counter = 0

  /** parses a declaration in a specific point (default: current point) of the document associated with this session (also stores in in controller) */
  def parseStructure(s: String, scopeOpt: Option[HasParentInfo] = None): StructuralElement = {
    val buffer = ParsingStream.stringToReader(s)
    val scope = scopeOpt.getOrElse(currentScope)
    val ps = ParsingStream(path.uri, scope, doc.getNamespaceMap, interpreter.format, buffer)
    val se = interpreter(ps)(errorCont)
    se match {
      case r: MRef => currentScope = IsMod(r.target, LocalName.empty)
      case Include(_) =>
      case m: ModuleOrLink => currentScope = IsMod(m.modulePath, LocalName.empty)
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

  /** gets the current MPath or throws an error */
  def getMPath(scopeOpt: Option[HasParentInfo]): MPath  = scopeOpt.getOrElse(currentScope) match {
    case IsMod(m, _) => m
    case _ => throw GeneralError("can only get context inside of module")
  }

  /** like parseStructure but for objects; the object is stored it as the definiens of a [[Constant]] declaration */
  def parseObject(s: String, scopeOpt: Option[HasParentInfo] = None): Constant = {

    // get a context for parsing
    val mpath = getMPath(scopeOpt)
    val context = Context(mpath)

    // set up the parsing unit
    val sref = SourceRef(path.uri, SourceRegion.ofString(s))
    val pu = ParsingUnit(sref, context, s, doc.getIIContext)

    // parse the term
    val cr = interpreter(pu)(errorCont)
    val term = cr.term

    // simplify it and extract the types
    val termS = interpreter.simplifier(term, SimplificationUnit(context, true, true, true))
    val df = Some(termS)
    val tp = cr.solution.flatMap(_.getO(CheckingUnit.unknownType).flatMap(_.df))

    // store it as a local definition
    val name = LocalName("res" + counter)
    counter += 1
    Constant(OMMOD(mpath), name, Nil, tp, df, None)
  }
  /** like parseObject but does not store or check the term */
  def parseTerm(s: String, scopeOpt: Option[HasParentInfo] = None, ls : List[LocalName] = Nil): Term = {

    import objects.Conversions._
    // get a context for parsing
    val mcontext = Context(getMPath(scopeOpt))
    val context = if (ls.isEmpty) mcontext else ls.foldLeft(mcontext)((c,ln) => c ++ VarDecl(ln)) // ln % tp

    // setup the parsing unit
    val sref = SourceRef(path.uri, SourceRegion.ofString(s))
    val pu = ParsingUnit(sref, context, s, doc.getIIContext)

    // and return the term
    interpreter.parser(pu)(errorCont).term
  }
  /** simplifies a term in the current context */
  def simplifyTerm(t: Term, scopeOpt: Option[HasParentInfo]): Term = {
    val context = Context(getMPath(scopeOpt))
    interpreter.simplifier(t, SimplificationUnit(context, true,true, true))
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
  // return the OMDoc representation of the current session
  case object StoreOMDoc extends Command
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
        case "finalize" => StoreOMDoc
        case s => Input(s)
      }
    }
  }

  /** Response by a REPL session after executing a [[Command]] */
  abstract class REPLResponse

  /** @param messages pairs on type and object */
  case class MultiTypedResponse(messages: (String,String)*) extends REPLResponse
  /** shortcut for a response with a single message */
  object AdminResponse {
    def apply(message: String) = MultiTypedResponse("message" -> message)
  }

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
      apply(session, command, None)
    } catch {
      case e : Exception => return ServerResponse.errorResponse(Error(e), "html")
    }
    TextResponse(response.toString)
  }

  def apply(session: Option[String], command: Command, errorContO: Option[ErrorHandler]): REPLResponse = {
    applyActual(session,command,errorContO)
  }

  def getSessionOpt(id: String) = sessions.find(_.id == id)
  private def getSession(id: String) = getSessionOpt(id).getOrElse(throw LocalError("Unknown Session"))

  private def path(id: String): DPath = DPath(mmt.baseURI) / "jupyter" / id

  private def applyActual(idO: Option[String], command: Command, errorContO: Option[ErrorHandler]) : REPLResponse = {
    implicit lazy val session = idO match {
      case None => throw LocalError("session needed")
      case Some(id) => getSession(id)
    }
    command match {
      case Show => getSessions
      case Clear => clearSessions
      case Start => startSession(idO.getOrElse(throw LocalError("No session provided")), errorContO.getOrElse(ErrorThrower))
      case Restart => restartSession(session)
      case Quit => quitSession(session)
      case Input(s) => evalInSession(session, s)
      case StoreOMDoc => storeOMDoc(session)
    }
  }

  private def storeOMDoc(session: REPLSession) = {
    val doc = session.doc
    val msg = "stored document " + session.doc.path
    MultiTypedResponse("message" -> msg, "omdoc" -> doc.toNodeResolved(controller.globalLookup).toString)
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
        val p = Path.parse(rest, session.doc.getNamespaceMap)
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

  private def startSession(id: String, errorCont: ErrorHandler) = {
    if (sessions.exists(_.id==id)){
      throw LocalError("Session already exists")
    }
    createSession(path(id), id, errorCont)
    // return the session id
    AdminResponse(s"Created Session $id")
  }

  private def restartSession(session: REPLSession) = {
    val id = session.id
    deleteSession(session)
    createSession(path(id), id, session.errorCont)
    AdminResponse(s"Restarted Session $id")
  }

  private def quitSession(session: REPLSession) = {
    val id = session.id
    deleteSession(session)
    controller.delete(session.doc.path)
    AdminResponse(s"Deleted session $id")
  }

  // SESSION MANAGEMENT

  private def createSession(path: DPath, id: String, errorCont: ErrorHandler) : REPLSession = {
    val nsMap = controller.getNamespaceMap(path)
    val doc = new Document(path, level=FileLevel, initNsMap = nsMap)
    controller.add(doc)
    val format = "mmt"
    val interpreter = controller.extman.get(classOf[TwoStepInterpreter], format).getOrElse {
      throw LocalError("no parser found")
    }
    val s = new REPLSession(doc, id, interpreter, errorCont)
    sessions ::= s
    s
  }

  private def deleteSession(s: REPLSession) {
    controller.delete(s.doc.path)
    sessions = sessions.filterNot(_.id == s.id)
  }
}

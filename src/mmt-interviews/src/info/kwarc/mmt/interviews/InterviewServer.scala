package info.kwarc.mmt.interviews


import info.kwarc.mmt.api._
import info.kwarc.mmt.api.checking.{CheckingEnvironment, MMTStructureChecker, RelationHandler, TwoStepInterpreter}
import info.kwarc.mmt.api.modules.{DeclaredTheory, DeclaredView, Theory}
import info.kwarc.mmt.api.objects.{Context, OMMOD}
import info.kwarc.mmt.api.parser._
import info.kwarc.mmt.api.web.{ServerExtension, ServerRequest, ServerResponse}

class InterviewServer extends ServerExtension("interview") {
  override def logPrefix: String = "interview"
  override def apply(request: ServerRequest): ServerResponse = {
    val (path,query,body) = (request.path.tail,request.parsedQuery,request.body)
    log("Path: " + path.mkString("/"))
    log("Query: " + query)
    path match {
      case List("new") =>
        if (query("theory").isDefined) {
            val name = query("theory").get
            val meta = query("meta").map(Path.parseM(_,NamespaceMap.empty))
            val mp = Path.parseM(name,NamespaceMap.empty)
            val th = Theory.empty(mp.parent,mp.name,meta)
            controller add th
            return ServerResponse.TextResponse("OK")
        }
        if (query("view").isDefined && query("from").isDefined && query("to").isDefined) {
            val (name,froms,tos) = (query("view").get,query("from").get,query("to").get)
            val mp = Path.parseM(name,NamespaceMap.empty)
            val (from,to) = (Path.parseM(froms,NamespaceMap.empty),Path.parseM(tos,NamespaceMap.empty))
            val v = new DeclaredView(mp.parent,mp.name,OMMOD(from),OMMOD(to),false)
            controller add v
            return ServerResponse.TextResponse("OK")
        }
        if (query("decl").isDefined && query("cont").isDefined) {
            val mps = query("cont").get
            val th = controller.get(Path.parseM(mps,NamespaceMap.empty)) match {
              case ths : DeclaredTheory => ths
              case _ => return ServerResponse.errorResponse("Theory " + mps + " doesn't exit")
            }
            val errs = parseDecl(body.asString,th)
            return if(errs.nonEmpty) ServerResponse.errorResponse(errs.head,"html") else {
              ServerResponse.TextResponse("OK")
            }
        }
      case _ =>
    }
    (query("term"),query("cont")) match {
      case (Some(_),Some(mps)) =>
        val th = controller.get(Path.parseM(mps,NamespaceMap.empty)) match {
          case ths : DeclaredTheory => ths
          case _ => return ServerResponse.errorResponse("Theory " + mps + " doesn't exit")
        }
        val (tm,errs) = parseTerm(body.asString,th.path)
        return if(errs.nonEmpty) ServerResponse.errorResponse(errs.head,"html") else {
          ServerResponse.XmlResponse(tm.toTerm.toNode)
        }
      case _ =>
    }
    ServerResponse.errorResponse("Invalid request")
  }

  private lazy val twostep = controller.extman.getOrAddExtension(classOf[TwoStepInterpreter],"mmt").get
  private lazy val parser = twostep.parser.asInstanceOf[KeywordBasedParser]
  private lazy val checker = twostep.checker.asInstanceOf[MMTStructureChecker]

  private def parseTerm(s : String, mp : MPath, check : Boolean = true) = {
    implicit val errorCont = new ErrorContainer(None)
    val t = parser.apply(ParsingUnit(SourceRef.anonymous(""),Context(mp),s,NamespaceMap.empty))
    (t,errorCont.getErrors)
  }

  private def parseDecl(s : String, th : DeclaredTheory) = {
    val pstream = ParsingStream.fromString(s,th.parent,"mmt")
    val errorCont = new ErrorContainer(None)

    val cont = new StructureParserContinuations(errorCont) {
      val ce = new CheckingEnvironment(errorCont, RelationHandler.ignore, pstream)
      override def onElement(se: StructuralElement) {
        checker.applyElementBegin(se)(ce)
      }
      override def onElementEnd(se: ContainerElement[_]) {
        checker.applyElementEnd(se)(ce)
      }
    }
    implicit val pstate = new ParserState(Reader(s),pstream,cont)
    parser.readInModule(th,Context(th.path),parser.noFeatures)
    errorCont.getErrors
  }
}

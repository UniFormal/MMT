package info.kwarc.mmt.interviews


import info.kwarc.mmt.api._
import info.kwarc.mmt.api.checking._
import info.kwarc.mmt.api.modules.{Module, Theory, View}
import info.kwarc.mmt.api.objects.{Context, OMMOD}
import info.kwarc.mmt.api.parser._
import info.kwarc.mmt.api.presentation.{HTMLPresenter, Presenter}
import info.kwarc.mmt.api.web.{ServerExtension, ServerRequest, ServerResponse}

class InterviewServer extends ServerExtension("interview") {
  private lazy val twostep = controller.extman.getOrAddExtension(classOf[TwoStepInterpreter],"mmt").get
  private lazy val parser = twostep.parser.asInstanceOf[KeywordBasedParser]
  private lazy val checker = twostep.checker.asInstanceOf[MMTStructureChecker]

  override def logPrefix: String = "interview"
  override def apply(request: ServerRequest): ServerResponse = {
    implicit val errorCont = new ErrorContainer(None)
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
            checker.apply(th)(new CheckingEnvironment(controller.simplifier, errorCont,RelationHandler.ignore,MMTTask.generic))
            return ServerResponse.TextResponse("OK")
        }
        if (query("view").isDefined && query("from").isDefined && query("to").isDefined) {
            val (name,froms,tos) = (query("view").get,query("from").get,query("to").get)
            val mp = Path.parseM(name,NamespaceMap.empty)
            val (from,to) = (Path.parseM(froms,NamespaceMap.empty),Path.parseM(tos,NamespaceMap.empty))
            val v = View(mp.parent,mp.name,OMMOD(from),OMMOD(to),false)
            controller add v
            checker.apply(v)(new CheckingEnvironment(controller.simplifier, errorCont,RelationHandler.ignore,MMTTask.generic))
            return ServerResponse.TextResponse("OK")
        }
        if (query("decl").isDefined && query("cont").isDefined) {
            val mps = query("cont").get
            val th : Module = controller.get(Path.parseM(mps,NamespaceMap.empty)) match {
              case ths : Theory => ths
              case v : View => v
              case _ => return ServerResponse.errorResponse("Theory " + mps + " doesn't exit")
            }
            val errs = parseDecl(body.asString,th)
            return if(errs.nonEmpty) ServerResponse.errorResponse(errs.head,"html") else {
              response(th)
            }
        }
      case List("term") =>
        val mps = query("cont").getOrElse(return ServerResponse.errorResponse("No context for term given"))
        val th = controller.get(Path.parseM(mps,NamespaceMap.empty)) match {
          case ths : Theory => ths
          case _ => return ServerResponse.errorResponse("Theory " + mps + " doesn't exit")
        }
        val (tm,errs) = parseTerm(body.asString,th.path)
        return if(errs.nonEmpty) ServerResponse.errorResponse(errs.head,"html") else {
          response(tm.toTerm)
        }
      case List("infer") =>
        val mps = query("cont").getOrElse(return ServerResponse.errorResponse("No context for term given"))
        val th = controller.get(Path.parseM(mps,NamespaceMap.empty)) match {
          case ths : Theory => ths
          case _ => return ServerResponse.errorResponse("Theory " + mps + " doesn't exit")
        }
        val (tm,errs) = parseTerm(body.asString,th.path)
        if(errs.nonEmpty) return ServerResponse.errorResponse(errs.head,"html")
        val itp = checking.Solver.infer(controller, th.getInnerContext, tm.toTerm, None).getOrElse {
          return ServerResponse.errorResponse("Term parsed, but error occured during type inference")
        }
        return response(itp)
      case _ =>
    }

    ServerResponse.errorResponse("Invalid request:\nPath: " + path + "\nQuery: " + query)
  }

  private def parseTerm(s : String, mp : MPath, check : Boolean = true)(implicit errorCont : ErrorContainer) = {
    val t = parser.apply(ParsingUnit(SourceRef.anonymous(""),Context(mp),s,NamespaceMap.empty))
    (t,errorCont.getErrors.filter(_.level > Level.Warning))
  }

  private def parseDecl(s : String, th : Module)(implicit errorCont : ErrorContainer) = {
    val pstream = ParsingStream.fromString(s,th.parent,"mmt")

    val cont = new StructureParserContinuations(errorCont) {
      val ce = new CheckingEnvironment(controller.simplifier, errorCont, RelationHandler.ignore, pstream)
      override def onElement(se: StructuralElement) {
        checker.applyElementBegin(se)(ce)
      }
      override def onElementEnd(se: ContainerElement[_]) {
        checker.applyElementEnd(se)(ce)
      }
    }
    implicit val pstate = new ParserState(Reader(s),pstream,cont)
    val context = th.getInnerContext
    parser.readInModule(th,context,parser.noFeatures)
    errorCont.getErrors.filter(_.level > Level.Warning)
  }


  lazy val html = controller.extman.get(classOf[Presenter],"html").get.asInstanceOf[HTMLPresenter]
  private def response(se : StructuralElement) = {
    ServerResponse.HTMLResponse(html.asString(se))
  }
  private def response(o : objects.Obj) = {
    ServerResponse.HTMLResponse(html.objectPresenter.asString(o,None))
  }

}

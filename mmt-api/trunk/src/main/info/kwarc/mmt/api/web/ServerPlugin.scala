package info.kwarc.mmt.api.web
import info.kwarc.mmt.api._
import frontend._
import ontology._
import utils._
import tiscaf._

/**
 * An MMT extensions that handles certain requests in MMT's HTTP server.
 * 
 * It will be called on URIs of the form http://server:port/:CONTEXT/PATH?QUERY
 * 
 * @param cont the CONTEXT
 */
abstract class ServerExtension(context: String) extends Extension {
  /**
   * @param cont the context of the request
   * @return true if cont is equal to this.context
   */
  def isApplicable(cont : String) : Boolean = cont == context
  /**
   * handles the HTTP request
   * @param path the PATH from above (excluding CONTEXT)
   * @param query the QUERY from above
   * @param body the body of the request
   * @return the HTTP response
   * 
   * Implementing classes can and should use Server.XmlResponse etc to construct responses conveniently.
   * 
   * Errors thrown by this method are caught and sent back to the browser.
   */
  def apply(path: List[String], query: String, body: Body): HLet
}

/** execute the query as an MMT [[frontend.GetAction]] */
class ActionServer extends ServerExtension("mmt") {
    def apply(path: List[String], query: String, body: Body) = {
       val action = Action.parseAct(query, controller.getBase, controller.getHome)
        val resp: String = action match {
          case GetAction(a: ToWindow) =>
             a.make(controller)
             <done action={a.toString}/>.toString
          case GetAction(a: Respond) =>
             a.get(controller)
          case _ =>
             <notallowed action={action.toString}/>.toString
        }
        Server.XmlResponse(resp)
    }
}

/** interprets the query as an MMT document URI and returns the SVG representation of the theory graph */
class SVGServer extends ServerExtension("svg") {
   /**
    *  @param path ignored
    *  @param query the document path
    *  @param body ignored
    */
   def apply(path: List[String], query: String, body: Body) = {
      val path = Path.parse(query, controller.getBase)
      val (arch,inPath) = controller.backend.resolveLogical(path.doc.uri).getOrElse {
         throw LocalError("illegal path: " + query)
      }
      val inPathFile = archives.Archive.narrationSegmentsAsFile(inPath, "omdoc")
      val svgFile = (arch.root / "svg" / inPathFile).setExtension("svg")
      log("serving svg from " + svgFile)
      val node = utils.xml.readFile(svgFile)
      Server.XmlResponse(node)
   } 
}

/** interprets the body as a QMT [[ontology.Query]] and evaluates it */
class QueryServer extends ServerExtension("query") {
   /**
    *  @param path ignored
    *  @param httpquery ignored
    *  @param body the query as XML
    */
   def apply(path: List[String], httpquery: String, body: Body) = {
      val mmtquery = body.asXML
      log("qmt query: " + mmtquery)
      val q = Query.parse(mmtquery)(controller.extman.queryExtensions)
      //log("qmt query: " + q.toString)
      Query.infer(q)(Nil) // type checking
      val res = controller.evaluator.evaluate(q)
      val resp = res.toNode
      Server.XmlResponse(resp)
   }
}

/** HTTP frontend to the [[Search]] class */
class SearchServer extends ServerExtension("search") {
   private lazy val search = new Search(controller)
   private lazy val mmlpres = new presentation.MathMLPresenter(controller)
   /**
    *  @param path ignored
    *  @param httpquery search parameters
    *  @param body ignored
    */
   def apply(path: List[String], httpquery: String, body: Body) = {
      val wq = WebQuery.parse(httpquery)
      val base = wq("base")
      val mod = wq("module")
      val name = wq("name")
      val theory = wq("theory")
      val pattern = wq("pattern")
      val intype = wq.boolean("type")  
      val indef = wq.boolean("definition")
      val allcomps = List(TypeComponent, DefComponent)
      val comps = allcomps.zip(List(intype,indef)).filter(_._2).map(_._1)
      val pp = PathPattern(base, mod, name)
      val tp = (theory, pattern) match {
         case (Some(t), Some(p)) => Some(TermPattern.parse(controller, t, p))
         case (_, _) => None
      }
      val sq = SearchQuery(pp, comps, tp)
      val res = search(sq, true)
      val html = utils.HTML.builder
      import html._
      div(attributes = List("xmlns" -> xml.namespace("html"), "xmlns:jobad" -> utils.xml.namespace("jobad"))) {
         res.foreach {r =>
            div("result") {
               val CPath(par, comp) = r.cpath
               div {
                  text {comp.toString + " of "}
                  span("mmturi", attributes=List("jobad:href" -> par.toPath)) {text {par.last}}
               }
               r match {
                  case SearchResult(cp, pos, None) =>
                  case SearchResult(cp, pos, Some(term)) =>
                     def style(pc: presentation.PresentationContext) = if (pc.pos == pos) "resultmatch" else ""
                     div {mmlpres(term, Some(cp), style)(new presentation.HTMLRenderingHandler(html))}
               }
            }
         }
      }
      Server.XmlResponse(html.result)
   }
}

/** part of web browser */
class BreadcrumbsServer extends ServerExtension("breadcrumbs") {
   def apply(path: List[String], query: String, body: Body) = {
      val mmtpath = Path.parse(query, controller.getBase)
      val ancs = mmtpath.ancestors.reverse
      var mpathfound = false
      var spathfound = false
      val html = utils.HTML.builder
      import html._
      def gsep() = span {text {"?"}}
      def lsep() = span {text {"/"}}
      // strangely, the client somehow does not handle this right if the XML is given literally, might be due to namespaces
      div(attributes = List("xmlns" -> utils.xml.namespace("xhtml"), "xmlns:jobad" -> utils.xml.namespace("jobad"))) {
         ancs.foreach {p =>
            p match {
               case p : MPath if ! mpathfound => mpathfound = true; gsep()
               case p : GlobalName if ! spathfound => spathfound = true; gsep()
               case p if p.^! == p => Nil
               case _ => lsep()
            }
            span("mmturi", attributes=List("jobad:href" -> p.toPath)) {text {p.last}}
         }
      }
      Server.XmlResponse(html.result)
   }
}

/** interprets the query as an MMT [[frontend.Action]] and executes it */
class AdminServer extends ServerExtension("admin") {
   private val logCache = new CacheHandler("admin")
   override def start(args: List[String]) {
      report.addHandler(logCache)
   }
   override def destroy {
      report.removeHandler("admin")
   }
   def apply(path: List[String], query: String, body: Body) = {
      val c = query.replace("%20", " ")
      val act = frontend.Action.parseAct(c, controller.getBase, controller.getHome)
      logCache.clear
      controller.handle(act)
      val r = logCache.recall
      val html = utils.HTML.builder
      import html._
      div {r.reverse foreach {l => div {text {l}}}}
      Server.XmlResponse(html.result)
   }
}

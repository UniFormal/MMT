package info.kwarc.mmt.api.web
import info.kwarc.mmt.api._
import frontend._
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
        val node: scala.xml.Node = action match {
          case GetAction(a: ToWindow) =>
             a.make(controller)
             <done action={a.toString}/>
          case GetAction(a: Respond) =>
             a.get(controller)
          case _ =>
             <notallowed action={action.toString}/>
        }
        val textresponse = action match {
          case GetAction(Respond(p)) => p match {
             case Present(_, param) => param.startsWith("text") 
             case _ => false
          }
          case _ => false
        }
        if (textresponse)
            Server.TextResponse(node.toString)
        else
            Server.XmlResponse(node)
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
    *  @param query ignored
    *  @param body the query as XML
    */
   def apply(path: List[String], query: String, body: Body) = {
      val q = ontology.Query.parse(body.asXML)(controller.extman.queryExtensions)
      log("qmt query: " + q.toString)
      ontology.Query.infer(q)(Nil) // type checking
      val res = controller.evaluator.evaluate(q)
      val resp = res.toNode
      Server.XmlResponse(resp)
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
      Server.XmlResponse(Util.div(r reverseMap Util.div))
   }
}

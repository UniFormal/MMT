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
abstract class ServerPlugin(context: String) extends Extension {
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

/** a ServerPlugin that serves the svg file of a documents */
class SVGServer extends ServerPlugin("svg") {
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
      val svgFile = (arch.svgDir / inPathFile).setExtension("svg")
      log("serving svg from " + svgFile)
      val node = utils.xml.readFile(svgFile)
      Server.XmlResponse(node)
   } 
}

/** a ServerPlugin that serves the svg file of a documents */
class UOMServer extends ServerPlugin("uom") {
   /**
    *  @param path ignored
    *  @param query the expression as string
    *  @param body the expression as XML
    */
   def apply(path: List[String], query: String, body: Body) = {
      val input = objects.Obj.parseTerm(body.asXML, controller.getBase)
      val output = controller.uom.simplify(input)
      Server.XmlResponse(output.toNode)
   }
}

/** Plugin for QMT query requests */
class QueryServer extends ServerPlugin("query") {
   /**
    *  @param path ignored
    *  @param query ignored
    *  @param body the query as XML
    */
   def apply(path: List[String], query: String, body: Body) = {
      val q = ontology.Query.parse(body.asXML)(controller.extman.queryExtensions)
      ontology.Query.infer(q)(Nil) // type checking
      val res = controller.evaluator.evaluate(q)
      val resp = res.toNode
      Server.XmlResponse(resp)
   }
}


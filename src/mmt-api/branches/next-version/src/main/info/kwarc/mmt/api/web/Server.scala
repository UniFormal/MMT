package info.kwarc.mmt.api.web

import info.kwarc.mmt.api._
import frontend._
import backend._
import ontology._
import modules._
import utils.URI
import zgs.httpd._
import zgs.httpd.let._
import scala.util.parsing.json.{ JSONType, JSONArray, JSONObject }
import scala.xml._
import java.net.HttpURLConnection
import java.net._
import java.io._
import scala.util.parsing.json.JSONObject

case class ServerError(n: Node) extends java.lang.Throwable

/** An HTTP RESTful server. */
class Server(val port: Int, controller: Controller) extends HServer {

  override def name = "MMT rest server"
  override def writeBufSize = 16 * 1024
  override def tcpNoDelay = true // make this false if you have extremely frequent requests
  override def startStopListener = {} // prevents tiscaf from creating a "stop" listener
  protected def ports = List(port) // port to listen to
  protected def apps = List(new RequestHandler) // RequestHandler is defined below
  protected def talkPoolSize = 4
  protected def talkQueueSize = Int.MaxValue
  protected def selectorPoolSize = 2

  private def log(message: => String) { controller.report("server", message) }
  /**
   * Cross-Origin Resource Sharing
   * For cross website ajax queries
   */
  private def CORS_AllowOrigin(origin : String) = true //for now
  private def checkCORS(tk : HTalk) : HTalk = tk.req.header("Origin")  match {
    case None => tk
    case Some(s) => CORS_AllowOrigin(s) match {
      case true => tk.setHeader(" Access-Control-Allow-Origin", s)
      case false => tk
    }
  }
  
  private def bodyAsString(tk: HTalk): String = {
    val bodyArray: Array[Byte] = tk.req.octets.getOrElse(throw ServerError(<error message="no body found"/>))
    new String(bodyArray, "UTF-8")
  }

  private def bodyAsXML(tk: HTalk): Node = {
    val bodyString = bodyAsString(tk)
    val bodyXML = try {
      scala.xml.XML.loadString(bodyString).head
    } catch {
      case _ => throw ServerError(<error message="invalid XML"/>)
    }
    scala.xml.Utility.trim(bodyXML)
  }

  protected class RequestHandler extends HApp {
    //override def buffered = true
    override def chunked = true // Content-Length is not set at the beginning of the response, so we can stream info while computing/reading from disk
    def resolve(req: HReqHeaderData): Option[HLet] = {
      log("request: /" + req.uriPath + " " + req.uriExt.getOrElse("") + "?" + req.query)
      Util.getComponents(req.uriPath) match {
        case ":tree" :: _ => Some(XmlResponse(tree(req.query)))
        case ":query" :: _ => Some(QueryResponse)
        case ":graph" :: _ =>
          val mmtpath = Path.parse(req.query, controller.getBase)
          val json: JSONType = graph(mmtpath)
          Some(JsonResponse(json))
        case ":breadcrumbs" :: _ =>
          val mmtpath = Path.parse(req.query, controller.getBase)
          val node = scala.xml.Utility.trim(Util.breadcrumbs(mmtpath))
          Some(XmlResponse(node))
        case ":admin" :: _ =>
          try {
            controller.report.record
            val c = req.query.replace("%20", " ")
            val act = frontend.Action.parseAct(c, controller.getBase, controller.getHome)
            controller.handle(act)
            val r = controller.report.recall
            controller.report.clear
            Some(XmlResponse(Util.div(r reverseMap Util.div)))
          } catch {
            case e: Error =>
              controller.report(e)
              controller.report.clear
              Some(XmlResponse(Util.div("error: " + e.msg)))
          }
        case ":uom" :: _ => Some(UomResponse)
        case ":search" :: _ => Some(MwsResponse)
        case ":parse" ::_ => Some(ParserResponse)
        case ":mmt" :: _ => Some(MmtResponse)
        // empty path 
        case List("") | Nil => Some(resourceResponse("browse.html"))
        // HTML files in xhtml/ folder can be accessed without xhtml/ prefix
        //case List(s) if (s.endsWith(".html")) => {println("tt"); Some(resourceResponse("xhtml/" + s))}
        // other resources
        case _ => Some(resourceResponse(req.uriPath))
      }
    }
  }

  /** Response when the first path component is :query */
  private def QueryResponse: HLet = new HLet {
    def act(tk: HTalk) {
      try {
        val res = try {
          val q = ontology.Query.parse(bodyAsXML(tk))
          controller.evaluator.evaluate(q)
        } catch {
          case ParseError(s) => throw ServerError(<error message={ s }/>)
          case GetError(s) => throw ServerError(<error message={ s }/>)
        }
        val resp = res.toNode
        XmlResponse(resp).act(tk)
      } catch {
        case ServerError(n) => XmlResponse(n).act(tk)
      }
    }
  }

  /** Response when the first path component is :uom */
  private def UomResponse: HLet = new HLet {
    def act(tk: HTalk) {
      val resp = tk.req.query match {
        case "register" => <error message="not implemented yet"/>
        case "simplify" =>
          try {
            val input = objects.Obj.parseTerm(bodyAsXML(tk), controller.getBase)
            val output = controller.uom.simplify(input)
            output.toNode
          } catch {
            case e => <error>{ e.getMessage }</error>
          }
        case _ => <error message="illegal command"/>
      }
      XmlResponse(resp).act(tk)
    }
  }

  /** Response when the first path component is :search */
  private def MwsResponse: HLet = new HLet {
    def act(tk: HTalk) {
      val resp = try {
        //
        val offset = tk.req.header("Offset") match {
          case Some(s) => try s.toInt catch { case _ => 0 }
          case _ => 0
        }
        val size = tk.req.header("Size") match {
          case Some(s) => try s.toInt catch { case _ => 30 }
          case _ => 30
        }
        val query = tk.req.query
        val qt = controller.extman.getQueryTransformer(query).getOrElse(TrivialQueryTransformer)
        val (mwsquery, params) = query match {
          case "mizar" =>
            val bodyXML = bodyAsXML(tk)
            val mmlVersion = tk.req.header("MMLVersion") match {
              case Some(s) => s
              case _ => "4.166"
            }
            val currentAid = tk.req.header("Aid") match {
              case Some(s) => s
              case _ => "HIDDEN"
            }
            (bodyXML, List(currentAid, mmlVersion))
          case "tptp" => (scala.xml.Text(bodyAsString(tk)), Nil)
          case "lf" =>
             val scope = tk.req.header("scope") match {
               case Some(s) => 
                 Path.parse(s) match {
                   case mp : MPath =>  objects.OMMOD(mp)
                   case _ => throw ServerError(<error><message> expected mpath found : {s} </message></error>)
                 }
               case _ => throw ServerError(<error><message> expected a scope (mpath) passed in header </message></error>)
             }
             val termParser = controller.termParser
             val tm = try {
               termParser(parser.ParsingUnit(null, scope, objects.Context(), bodyAsString(tk)), true)
             } catch {
               case e : Throwable =>
                 println(e)
                 throw e
             }

             def genQVars(n : Node) : Node = n match {
               case a : scala.xml.Atom[_] => a
               case  <m:ci>{q}</m:ci> =>
                 if (q.toString.startsWith("?") || q.toString.startsWith("%3F")) {
                   <mws:qvar>{q}</mws:qvar>
                 } else {
                   n
                 }
               case _ => new scala.xml.Elem(n.prefix, n.label, n.attributes, n.scope, n.child.map(x => genQVars(x)) :_*)
             }
             val processedQuery = genQVars(tm.toCML)
             (<mws:expr>{processedQuery}</mws:expr>, Nil)
          case _ => (bodyAsXML(tk), Nil) // default: body is forwarded to MWS untouched
        }
        val tqs = qt.transformSearchQuery(mwsquery, params)
        def wrapMWS(n: Node): Node = <mws:query output="xml" limitmin={ offset.toString } answsize={ size.toString }>{ n }</mws:query>
        val mws = controller.extman.getMWS.getOrElse(throw ServerError(<error message="no MathWebSearch engine defined"/>))

        tqs.map(q => println(wrapMWS(q)))
        val res = tqs.map(q => utils.xml.post(mws.toJava.toURL, wrapMWS(q))) // calling MWS via HTTP post
        val total = res.foldRight(0)((r, x) => x + (r \ "@total").text.toInt)
        val totalsize = res.foldRight(0)((r, x) => x + (r \ "@size").text.toInt)
        val answrs = res.flatMap(_.child)
        <mws:answset total={ total.toString } size={ totalsize.toString } xmlns:mws="http://www.mathweb.org/mws/ns">{ answrs }</mws:answset>
      } catch {
        case ServerError(n) => n
        case e: ParseError => <error><message>{ e.getMessage }</message></error>
        case e => <error><message>error translating query : {e.getMessage()}</message></error>
      }
      XmlResponse(resp).act(tk)
    }
  }

  private def ParserResponse : HLet = new HLet {
    def act(tk : HTalk) {
      val text = tk.req.param("text").getOrElse(throw ServerError(<error><message>found no text to parse</message></error>))
      val save = tk.req.param("save").map(_ == "true").getOrElse(false) //if save parameter is "true" then save otherwise don't
      tk.req.query.split("\\?").toList match {
        case strDPath :: strThy :: Nil =>
          val dpath = DPath(URI(strDPath))
          val mpath = dpath ? LocalPath(strThy :: Nil)
          val ctrl = new Controller(controller.report)
          val reader = new TextReader(controller, ctrl.add)
          val res = reader.readDocument(text, dpath)(controller.termParser.apply)
          res._2.toList match {
            case Nil => //no error -> parsing successful
              try {
                val mod = ctrl.memory.content.getModule(mpath)
                val nset = DPath(URI("http://cds.omdoc.org/foundations/lf/mathml.omdoc")) ? "twelf"  //TODO get style from server js
                val rb = new presentation.XMLBuilder()
                val module = if(save) {
                  controller.get(mpath)
                } else {
                  mod
                }
                controller.presenter(module, presentation.GlobalParams(rb, nset))
                val thyXML = rb.get()
                val response = new collection.mutable.HashMap[String,Any]()
                response("success") = "true"                                      
                val sdiff = controller.detectChanges(List(mod))
                save match {
                  case false => //just detecting refinements
                    val refs = controller.detectRefinements(sdiff)
                    response("info") = JSONArray(refs)
                    response("pres") =  thyXML.toString
                  case true => //updating and returning list of done updates
                    val pchanges = tk.req.param("pchanges").map(_.split("\n").toList).getOrElse(Nil)       
                    val boxedPaths = controller.update(sdiff, pchanges)

                    def invPaths(p : Path, parents : Set[Path] = Nil.toSet) : Set[Path] = {
                    println("calling for path " + p + " with parents " + parents.mkString(", "))  
                    p match {
                      case d : DPath => 
                        controller.getDocument(d).getItems.flatMap(x => invPaths(x.target, parents + d)).toSet
                      case m : MPath => 
                        val affected = boxedPaths exists {cp => cp.parent match {
                          case gn : GlobalName => gn.module.toMPath == m
                          case mp : MPath => mp == p 
                          case _ => false
                        }}
                        if (affected)
                          parents + p
                        else 
                          Nil.toSet
                      case _ => Nil.toSet
                    }}
                    response("pres") = JSONArray(invPaths(controller.getBase).map(_.toString).toList)
                }
                JsonResponse(JSONObject(response.toMap)).act(tk)
              }  catch {
                case e : Throwable =>
                  TextResponse(e.toString).act(tk)
              }
            case l => //parsing failed -> returning errors 
              val response = new collection.mutable.HashMap[String,Any]()
              response("success") = "false"
              response("info") = JSONArray(Nil)
              response("pres") = l.map(e => (<p>{e.toString}</p>).toString).mkString("")
              JsonResponse(JSONObject(response.toMap)).act(tk)
          }
        case _ => throw ServerError(<error><message> invalid theory name in query : {tk.req.query}</message></error>)
      }
    }
  }


  /** Response when the first path component is :mmt */
  private def MmtResponse: HLet = new HLet {
    def act(tk: HTalk) {
      val comps = tk.req.query.split("\\?", -1)
      val (doc, mod, sym, act) = comps.length match {
        case 1 => (comps(0), "", "", "")
        case 2 => (comps(0), comps(1), "", "")
        case 3 => (comps(0), comps(1), comps(2), "")
        case _ =>
          val rest = comps.drop(3).mkString("", "?", "")
          (comps(0), comps(1), comps(2), rest.replace("_", " "))
      }
      val textresponse: Boolean = try {
        Action.parseAct(doc + "?" + mod + "?" + sym + " " + act, controller.getBase, controller.getHome) match {
          case DefaultGet(p) => p match {
            case frontend.Present(_, _) => tk.req.header("Accept") match {
              case Some("text/xml") => false
              case _ => false // TODO could be determined based on the presentation style
            }
            case ToNode(_) | Deps(_) => false
            case ToString(_) => true
          }
          case _ => false
        }
      } catch {
        case _ => false
      }
      try {
        val node = doGet(doc, mod, sym, act)
        if (textresponse)
          TextResponse(node.toString).act(tk)
        else
          XmlResponse(node).act(tk)
      } catch {
        case e: Error =>
           val ns = utils.xml.namespace("html")
           XmlResponse(<div xmlns={ns}>{e.msg.split("\\n").map(s => <p>{s}</p>)}</div>).act(tk)
      }
    }
  }

  /**
   * A resource response that the server sends back to the browser
   * @param path the path to the resource
   */
  private def resourceResponse(path: String): HLet = new HLet {
    def act(tk: HTalk) {
      val io = Util.loadResource(path.replace("//", "/"))
      if (io == null)
        (new ErrLet(HStatus.NotFound, path)).act(tk)
      else {
        val cType = HMime.exts.keysIterator.find(ext => path.toLowerCase.endsWith("." + ext)) match {
          case Some(e) => HMime.exts(e)
          case None => "text/plain"
        }
        tk.setContentType(cType)

        val buffer = new Array[Byte](4096) // buffer

        // read from disk and write to network simultaneously
        @scala.annotation.tailrec
        def step(wasRead: Int): Unit = if (wasRead > 0) {
          tk.write(buffer, 0, wasRead)
          step(io.read(buffer))
        }
        step(io.read(buffer))
        io.close
        tk.close
      }
    }
  }

  /**
   * A text response that the server sends back to the browser
   * @param text the message that is sent in the HTTP body
   */
  private def TextResponse(text: String): HLet = new HLet {
    def act(tk: HTalk) {
      val out = text.getBytes("UTF-8")
      checkCORS(tk).setContentLength(out.size) // if not buffered
        .setContentType("text/plain; charset=utf8")
        .write(out)
        .close
    }
  }

  /**
   * An XML response that the server sends back to the browser
   * @param node the XML message that is sent in the HTTP body
   */
  private def XmlResponse(node: scala.xml.Node): HLet = new HLet {
    def act(tk: HTalk) {
      val out: Array[Byte] = node.toString.getBytes("UTF-8")
      checkCORS(tk).setContentLength(out.size) // if not buffered
        .setContentType("text/xml; charset=utf8")
        .write(out)
        .close
    }
  }

  /**
   * A Json response that the server sends back to the browser
   * @param json the Json message that is sent in the HTTP body
   */
  private def JsonResponse(json: JSONType): HLet = new HLet {
    def act(tk: HTalk) {
      val out: Array[Byte] = json.toString.getBytes("UTF-8")
      checkCORS(tk).setContentLength(out.size) // if not buffered
        .setContentType("application/json; charset=utf8")
        .write(out)
        .close
    }
  }

  def doGet(doc: String, mod: String, sym: String, act: String) = {
    val action = frontend.Action.parseAct(doc + "?" + mod + "?" + sym + " " + act, controller.getBase, controller.getHome)
    log(action.toString)
    val ret: scala.xml.Node = action match {
      case frontend.DefaultGet(p) => frontend.Respond(p, "").get(controller)
	  case GetAction(a: frontend.ToWindow) =>
	     a.make(controller)
		 <done action={a.toString}/>
      case GetAction(a: frontend.Respond) => a.get(controller)
      case a => <error action={ a.toString }/>
    }
    log("done")
    ret
  }

  def tree(q: String): scala.xml.Node = {
    if (q == ":root")
      <root>{
        controller.backend.getArchives map { a => Util.item(DPath(a.narrationBase), "closed", Some(a.id)) }
      }</root>
    else {
      val path = Path.parse(q, controller.getBase)
      val role = controller.depstore.getType(path)
      path match {
        case p: DPath =>
          val doc = controller.getDocument(p)
          <root>{ doc.getItems.map { i => Util.item(i.target, "closed") } }</root>
        case p: MPath =>
          val rels: List[(String, RelationExp)] = role match {
            case Some(IsTheory) =>
              List(("meta for", -HasMeta), ("included into", -Includes),
                ("instantiated in", -RelationExp.HasStructureFrom),
                ("views out of", -HasDomain * HasType(IsView)), ("views into", -HasCodomain * HasType(IsView)))
            case Some(IsView) => List(("included into", -Includes), ("domain", +HasDomain), ("codomain", +HasCodomain))
            case _ => Nil // should be impossible
          }
          val results = rels map { case (desc, rel) => (desc, controller.depstore.queryList(path, rel)) }
          val resultsNonNil = results.filterNot(_._2.isEmpty)
          <root>{
            resultsNonNil map {
              case (desc, res) =>
                <item state="closed">
                  <content><name class="treerelation">{ desc }</name></content>
                  { res.map(Util.item(_, "closed")) }
                </item>
            }
          }</root>
        case _ => throw ImplementationError("only children of documents and modules can be taken")
      }
    }
  }

  def kindToColor(kind: String) : String = kind match {
      case "Meta" => "#0000ff"
      case "Include" => "#adad85"
      case "Struvture" => "#00ee00"
      case "View" => "#52527a"
    }
  
  /**
   * returns the JSON object representing a graph
   * @param p the MMT URI that is currently focused (ignored for now)
   */
  def graph(p: Path): JSONType = {
    val tg = new TheoryGraph(controller.depstore)
    JSONArray(
      tg.nodes.toList map { f =>
        JSONObject(Map(
          "id" -> f.toPath,
          "name" -> f.last,
          "data" -> JSONObject(Map(
              "$type" -> "ellipse",
              "$height" -> 15,
              "$width" -> 15,
              "$color" -> "#ff0000"
          )),
          "adjacencies" -> JSONArray(
            tg.edgesFrom(f) map {
              case (t, edges) =>               
                JSONObject(Map(
                  "nodeTo" -> t.toPath,
                  "nodeFrom" -> f.toPath,
                  "data" -> JSONObject(Map(
                    "$type" -> "multiple_arrow",
//                    "$color" -> kindToColor(Util.edgeKind(edges(0).edge)),
                    "$direction" -> JSONArray(
                        edges map {
                          case EdgeTo(_, e, backwards) =>
                        		JSONObject(Map(
                        				"from" -> (if (backwards) t else f).toPath,
                        				"to" -> (if (backwards) f else t).toPath,
                        				"backwards" -> backwards,
                        				"kind" -> Util.edgeKind(e),
                        				"uri" -> Util.edgeUri(e),
                        				"name" -> Util.edgeName(e),
                        				"inref" -> tg.nodes.toList.contains(t).toString()
                        		))})
                        )))

            )}))
      )})
  }
}

// Initial rewrites not implemented
/*                  
         // URI of the form CATALOG-AUTHORITY/;?doc?mod?sym?params
         // Ideally: URI of the form CATALOG-AUTHORITY?doc?mod?sym?params
         // But firefox transforms "path" to "/path" if path(0) != "/"
         // so the next best thing to an empty path is the path "/;"
          case RewriteRequest(ParsePath(List(";"), _, _, _), GetRequest, req) =>
               val initial = ReqHelpers.query(req)
               RewriteResponse(ParsePath(List("xhtml","browse"), "html", true, false), Map(("query", initial)), true)
*/
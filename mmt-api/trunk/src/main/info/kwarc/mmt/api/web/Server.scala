package info.kwarc.mmt.api.web

import info.kwarc.mmt.api._
import frontend._
import ontology._
import modules._

import info.kwarc.mmt.mizar.test.MwsService
import info.kwarc.mmt.tptp._

import zgs.httpd._  // tiscaf
import zgs.httpd.let._

import scala.util.parsing.json.{JSONType, JSONArray, JSONObject}
import scala.xml._

import java.net.HttpURLConnection;
import java.net._
import java.io._



/** An HTTP RESTful server. */  
class Server(val port : Int, controller : Controller) extends HServer {
    
  override def name = "mmt rest server"
  override def writeBufSize = 16*1024
  override def tcpNoDelay = true       // make this false if you have extremely frequent requests
  override def startStopListener = {}  // prevents tiscaf from creating a "stop" listener
  protected def ports = List(port)         // port to listen to
  protected def apps  = List(new RequestHandler) // RequestHandler is defined below
  protected def talkPoolSize = 4
  protected def talkQueueSize = Int.MaxValue
  protected def selectorPoolSize = 2
  
  private def log(message : =>String) {controller.report("server", message)}
  
  protected class RequestHandler extends HApp {
    //override def buffered = true
    override def chunked = true   // Content-Length is not set at the beginning of the response, so we can stream info while computing/reading from disk
    def resolve(req : HReqHeaderData) : Option[HLet] = {
      log("request: " + req.uriPath + "  " + req.uriExt + "   " + req.query);
      Util.getComponents(req.uriPath) match {
      case ":tree" :: _ => Some(XmlResponse(tree(req.query)))
      case ":query" :: _ => Some(QueryResponse)
      case ":graph" :: _ =>
          val mmtpath = Path.parse(req.query, controller.getBase)
          val json : JSONType = graph(mmtpath)
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
      case ":mmt" :: _ => Some(MmtResponse)
      // empty path 
      case List("") | Nil => Some(resourceResponse("browse.html"))
      // HTML files in xhtml/ folder can be accessed without xhtml/ prefix
      //case List(s) if (s.endsWith(".html")) => {println("tt"); Some(resourceResponse("xhtml/" + s))}
      // other resources
      case _ => Some(resourceResponse(req.uriPath))
    }
  }}
  
  
  /** Response when the first path component is :query */
  private def QueryResponse : HLet = new HLet {
    def act(tk : HTalk) {
           // default incoming edges
           if (tk.req.query == "") {
              val mmtpath = Path.parse(tk.req.query, controller.getBase)
              val node = incoming(mmtpath)
              XmlResponse(node).act(tk)
           } else {
              case class Error(n: Node) extends java.lang.Throwable
              try {
                 val bodyWS : Array[Byte] = tk.req.octets.getOrElse(throw Error(<error message="no body found"/>))
                 var bodyXML : scala.xml.Node = null
                 try {
                   bodyXML = scala.xml.XML.loadString(bodyWS.mkString).head
                 } catch {
                   case _ => throw Error(<error message="invalid XML"/>)
                 }
                 val body = scala.xml.Utility.trim(bodyXML)
                 val q = ontology.Query.parse(body)
                 val res = try {controller.evaluator.evaluate(q)}
                           catch {
                              case ParseError(s) => throw Error(<error message={s}/>)
                              case GetError(s) => throw Error(<error message={s}/>)
                           }
                 val resp = res.toNode
                 XmlResponse(resp).act(tk)
              }
              catch {case Error(n) => XmlResponse(n).act(tk)}
           }
    }
  }
  
  /** Response when the first path component is :uom */
  private def UomResponse : HLet = new HLet {
    def act(tk : HTalk) {
      val resp = tk.req.query match {
        case "register" => <error message="not implemented yet"/>
        case "simplify" =>
          tk.req.octets match {
            case None => <error message="no body found"/>
            case Some(body) => 
              //               val bodyNode = javax.xml.parsers.DocumentBuilderFactory
              //                .newInstance()
              //                .newDocumentBuilder()
              //                //.parse(new java.io.StringInputStream(body)
              //                .parse(new org.xml.sax.InputSource(new java.io.StringReader(body)))
              //                .getDocumentElement()
              try {
                val bodyXML = scala.xml.XML.loadString(body.mkString).head
                val input = objects.Obj.parseTerm(bodyXML, controller.getBase)
                val output = controller.uom.simplify(input)
                output.toNode
              } catch {
                case e => <error><message>{e.getMessage}</message><input>{body.mkString}</input></error>
              }
          }
        case _ => <error message="illegal command"/>
      }
      XmlResponse(resp).act(tk)
    }
  }
  
  /** Response when the first path component is :search */
  private def MwsResponse : HLet = new HLet {
    def act(tk : HTalk) {
      val resp = controller.backend.mws match {
            case None => <error message="no MathWebSearch backend defined"/>
            case Some(mws) =>
              tk.req.octets match {
                case None => <error message="empty query string"/>
                case Some(body) =>
                  val bodyXML = null
                  try {
                    val bodyXML = scala.xml.XML.loadString(body.mkString).head
                    // XML => Mizar
                    try {
                      val input = tk.req.query match {
                        case "mizar" => 
                          val mmlVersion = tk.req.header("MMLVersion") match {
                            case Some(s) => s
                            case _ => "4.166" 
                          }
                          val currentAid = tk.req.header("Aid") match {
                            case Some(s) => s
                            case _ => "HIDDEN"
                          }
                          val Offset = tk.req.header("Offset") match {
                            case Some(s) => try s.toInt catch {case _ => 0}
                            case _ => 0
                          }
                          val Size = tk.req.header("Size") match {
                            case Some(s) => try s.toInt catch {case _ => 30}
                            case _ => 30
                          }
                          val q = MwsService.parseQuery(bodyXML, currentAid, mmlVersion,Offset,Size) // translate mizar-xml query
                          val qlist = MwsService.applyImplicitInferences(q)
                          qlist
                        case _ => List(bodyXML) // assume content math query by default
                      }
                      val res = input.map(utils.xml.post(mws.toJava.toURL, _))
                      val total = res.foldRight(0)((r,x) => x + (r \ "@total").text.toInt)
                      val size = res.foldRight(0)((r,x) => x + (r \ "@size").text.toInt)
                            
                      val answrs = res.flatMap(_.child)
                      <mws:answset total={total.toString} size={size.toString} xmlns:mws="http://www.mathweb.org/mws/ns">{answrs}</mws:answset>  
                    }
                    catch {
                      case e: ParseError => <error><message>{e.getMessage}</message><input>{body.mkString}</input></error>
                      case _ => <error><message>Invalid MizXML, Translation Failed</message></error>
                    }  // end[Mizar]
                  }
                  catch { case _ => 
                    // text, not XML => TPTP
                    try {
                      val bodyString = body.mkString
                      tk.req.query match {
                        case "tptp" =>
                          val Offset = tk.req.header("Offset") match {
                            case Some(s) => try s.toInt catch {case _ => 0}
                            case _ => 0
                          }
                          val Size = tk.req.header("Size") match {
                            case Some(s) => try s.toInt catch {case _ => 30}
                            case _ => 30
                          }
                          val translator = new TptpTranslator()
                          val translated = translator.translateFormula(bodyString)
                          translated match {
                            case Some(x) =>
                              val node = TptpTranslator.toMwsQuery(x.toCML, Offset, Size)
//                              val res = node.map(utils.xml.post(new URL(mws.toJava.toURL), _))
                              val res = node.map(utils.xml.post(new URL("http://localhost:20000"), _))
                              val total = res.foldRight(0)((r,x) => x + (r \ "@total").text.toInt)
                              val size = res.foldRight(0)((r,x) => x + (r \ "@size").text.toInt)
                              val answrs = res.flatMap(_.child)
                              <mws:answset total={total.toString} size={size.toString} xmlns:mws="http://www.mathweb.org/mws/ns">{answrs}</mws:answset>
                            case None => <error><message>Error translating the given query to OMDoc</message></error>
                          }
                        case _ => <error><message>Only TPTP queries can be specified as string</message></error>
                      }
                    }
                    catch {
                      case e: ParseError => <error><message>{e.getMessage}</message></error>
                      case _ => <error><message>Translation Failed</message></error>
                    }
                  } // end[case Some(body)]
              } // end[tk.req.octets match]
      } // end[controller.backend.mws match]
      XmlResponse(resp).act(tk)
    }
  }
  
  
  /** Response when the first path component is :mmt */
  private def MmtResponse : HLet = new HLet {
    def act(tk : HTalk) {
        val comps = tk.req.query.split("\\?",-1)
        val (doc, mod, sym, act) = comps.length match {
          case 1 => (comps(0), "", "", "")
          case 2 => (comps(0), comps(1), "", "")
          case 3 => (comps(0), comps(1), comps(2), "")
          case _ =>
            val rest = comps.drop(3).mkString("","?","")
            (comps(0), comps(1), comps(2), rest.replace("_", " "))
        }
        val textresponse : Boolean = try {
          Action.parseAct(doc + "?" + mod + "?" + sym + " " + act, controller.getBase, controller.getHome) match {
            case DefaultGet(p) => p match {
              case Present(_,_) => tk.req.header("Accept") match {
                case Some("text/xml") => false    // TODO both false?
                case _ => false                   // TODO both false?
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
          case e => XmlResponse(<div>{"get error:\n\n" + e.getMessage}</div>).act(tk)
        }
    }
  }
  
      
  /** A text response that the server sends back to the browser
    * @param text the message that is sent in the HTTP body 
    */
  private def TextResponse(text : String) : HLet = new HLet {
    def act(tk : HTalk) {
      val out = text.getBytes("UTF-8")
      tk.setContentLength(out.size) // if not buffered
        .setContentType("text/plain; charset=utf8")
        .write(out)
        .close
    }
  }
  
  /** A resource response that the server sends back to the browser
    * @param path the path to the resource
    */
  private def resourceResponse(path : String) : HLet = new HLet {
    def act(tk : HTalk) {
        val io = Util.loadResource(path.replace("//", "/"))
        if (io == null)
          (new ErrLet(HStatus.NotFound, path)).act(tk)
        else {
          val cType = HMime.exts.keysIterator.find(ext => path.toLowerCase.endsWith("." + ext)) match {
            case Some(e) => HMime.exts(e)
            case None    => "text/plain"
          }
          tk.setContentType(cType)

          val buffer = new Array[Byte](4096) // buffer
          
          // read from disk and write to network simultaneously
          @scala.annotation.tailrec
          def step(wasRead : Int) : Unit = if (wasRead > 0) {
            tk.write(buffer, 0, wasRead)
            step(io.read(buffer))
          }
          step(io.read(buffer))
          io.close
          tk.close
        }
    }
  }

  /** An XML response that the server sends back to the browser
    * @param node the XML message that is sent in the HTTP body
    */
  private def XmlResponse(node : scala.xml.Node) : HLet = new HLet {
    def act(tk : HTalk) {
      val out : Array[Byte] = new scala.xml.PrettyPrinter(80,2).format(node).getBytes("UTF-8")
      tk.setContentLength(out.size) // if not buffered
            .setContentType("text/xml; charset=utf8")
            .write(out)
            .close
    }
  }

  /** A Json response that the server sends back to the browser
    * @param json the Json message that is sent in the HTTP body
    */
  private def JsonResponse(json : JSONType) : HLet = new HLet {
    def act(tk : HTalk) {
      val out : Array[Byte] = json.toString.getBytes("UTF-8")
      tk.setContentLength(out.size) // if not buffered
            .setContentType("application/json; charset=utf8")
            .write(out)
            .close
    }
  }

  def doGet(doc : String, mod : String, sym : String, act : String) = {
      val action = frontend.Action.parseAct(doc + "?" + mod + "?" + sym + " " + act, controller.getBase, controller.getHome)
      log(action.toString)
      val ret : scala.xml.Node = action match {
         case frontend.DefaultGet(p) => frontend.Respond(p,"").get(controller)
         case a : frontend.Respond => a.get(controller)
         case a => <error action={a.toString}/>
      }
      log("done")
      ret
   }
   
   
   def incoming(path: Path) : Node = {
      val deps = controller.depstore
      val meta = deps.queryList(path, - HasMeta)
      val imps = deps.queryList(path, - Includes)
      val strs = deps.queryList(path, - RelationExp.HasStructureFrom)
      val doms = deps.queryList(path, - HasDomain * HasType(IsView))
      val cods = deps.queryList(path, - HasCodomain * HasType(IsView))
      def refs(rel : String, subjs: List[Path]) : NodeSeq = {
        val lis = subjs map {p =>
          <li class="jstree-leaf">{Util.ahref(p)}</li>
        }
        <li class="jstree-leaf"><a href="">{rel}</a>{if (lis == Nil) Nil else <ul>{lis}</ul>}</li>
      }
      <ul xmlns={utils.mmt.namespace("xhtml")}>
        <li class="jstree-open">
          <a href="">known references</a>
          <ul>
           {refs("meta for", meta)}
           {refs("imported in",imps)}
           {refs("instantiated in",strs)}
           {refs("domain of",doms)}
           {refs("codomain of",cods)}
          </ul>
        </li>
      </ul>
   }
   
   def tree(q: String) : scala.xml.Node = {
      if (q == ":root")
         <root>{
            controller.backend.getArchives map {a => Util.item(DPath(a.narrationBase), "closed", Some(a.id))}
         }</root>
      else {
           val path = Path.parse(q, controller.getBase)
           val elem = controller.get(path)
           path match {
              case p: DPath => 
                 val children = controller.depstore.queryList(path, + Declares) 
                 <root>{children.map{c => Util.item(c, "closed")}}</root>
              case p:MPath =>
                 val rels : List[(String,RelationExp)] = elem match {
                    case t: Theory =>
                       List(("meta for",  - HasMeta), ("included into",  - Includes),
                            ("instantiated in",  - RelationExp.HasStructureFrom),
                            ("views out of", - HasDomain * HasType(IsView)), ("views into",  - HasCodomain * HasType(IsView)))
                    case v: View => List(("included into", - Includes), ("domain", + HasDomain), ("codomain", + HasCodomain))
                 }
                 val results = rels map {case (desc, rel) => (desc, controller.depstore.queryList(path, rel))}
                 val resultsNonNil = results.filterNot(_._2.isEmpty) 
                 <root>{
                    resultsNonNil map {case (desc,res) =>
                       <item state="closed">
                          <content><name class="treerelation">{desc}</name></content>
                          {res.map(Util.item(_, "closed"))}
                       </item>
                    }
                 }</root>
          }
      }
   }
   
   /**
    * returns the JSON object representing a graph
    * @param p the MMT URI that is currently focused (ignored for now)
    */
   def graph(p: Path) : JSONType = {
      val tg = new TheoryGraph(controller.depstore)
      JSONArray(
        tg.nodes.toList map { f =>
           JSONObject(Map(
              "id" -> f.toPath,
              "name" -> f.last,
              "adjacencies" -> JSONArray(tg.edgesFrom(f) map {case EdgeTo(t, e, backwards) =>
                  JSONObject(Map(
                     "nodeTo" -> t.toPath,
                     "nodeFrom" -> f.toPath,
                     "data" -> JSONObject(Map(
                        "$type" -> "multiple_arrow",
                        "direction" -> JSONObject(Map(
                           "from" -> (if (backwards) t else f).toPath,
                           "to" -> (if (backwards) f else t).toPath,
                           "kind" -> Util.edgeKind(e),
                           "uri" -> Util.edgeUri(e),
                           "name" -> Util.edgeName(e)
                        ))
                     ))
                  ))
              })
           ))
        }
      )
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
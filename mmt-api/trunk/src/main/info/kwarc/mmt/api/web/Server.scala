package info.kwarc.mmt.api.web

import info.kwarc.mmt.api._

import frontend._
import backend._
import ontology._
import modules._
import objects._
import utils.URI

import tiscaf._
import tiscaf.let._

import scala.util.parsing.json.{ JSONType, JSONArray, JSONObject }
import scala.xml._
import scala.concurrent._

import java.net.HttpURLConnection
import java.net._
import java.io._

case class ServerError(msg: String) extends Error(msg)

/** helper object for constructing HTTP responses */
object Server {
  /**
   * Cross-Origin Resource Sharing
   * For cross website ajax queries
   */
  private def CORS_AllowOrigin(origin : String) = true //for now
  private def checkCORS(tk : HTalk) : HTalk = tk.req.header("Origin")  match {
    case None => tk
    case Some(s) => CORS_AllowOrigin(s) match {
      case true => tk.setHeader("Access-Control-Allow-Origin", s)
      case false => tk
    }
  }
  /**
   * A text response that the server sends back to the browser
   * @param text the message that is sent in the HTTP body
   */
  def TypedTextResponse(text: String, tp: String): HLet = new HSimpleLet {
    def act(tk: HTalk) {
      val out = text.getBytes("UTF-8")
      checkCORS(tk).setContentLength(out.size) // if not buffered
        .setContentType(s"$tp; charset=utf8")
        .write(out)
    }
  }
  /**
   * A text response that the server sends back to the browser
   * @param text the message that is sent in the HTTP body
   */
  def TextResponse(text: String, tp: String = "plain"): HLet = TypedTextResponse(text, "text/"+tp)

  /**
   * An XML response that the server sends back to the browser
   * @param node the XML message that is sent in the HTTP body
   */
  def XmlResponse(s: String): HLet = TextResponse(s, "xml")

  /**
   * An XML response that the server sends back to the browser
   * @param node the XML message that is sent in the HTTP body
   */
  def XmlResponse(node: scala.xml.Node): HLet = TextResponse(node.toString, "xml")
  
  /**
   * A Json response that the server sends back to the browser 
   * @param json the Json message that is sent in the HTTP body
   */
  def JsonResponse(json: JSONType): HLet = new HSimpleLet {
    def act(tk: HTalk) {
      val out: Array[Byte] = json.toString.getBytes("UTF-8")
      checkCORS(tk).setContentLength(out.size) // if not buffered
        .setContentType("application/json; charset=utf8")
        .write(out)
    }
  }
  /** a response that sends an HTML error message to the browser */
  def errorResponse(msg: String): HLet = errorResponse(ServerError(msg))
  /** a response that sends an HTML error message to the browser */
  def errorResponse(error: Error): HLet = {
     val ns = utils.xml.namespace("html")
     val node = <div xmlns={ns}>{error.getLongMessage.split("\\n").map(s => <p>{s}</p>)}</div>
     XmlResponse(node)
  }
}

/** straightforward abstraction for web style key-value queries; no encoding, no duplicate keys */
case class WebQuery(pairs: List[(String,String)]) {
   def apply(key: String) : Option[String] = pairs.find(_._1 == key).map(_._2) 
   def string(key: String, default: String = ""): String = apply(key).getOrElse(default)
   def boolean(key: String, default: Boolean = false) = apply(key).getOrElse(default.toString) match {
      case "false" => false
      case "" | "true" => true
      case s => throw ParseError("boolean expected: " + s)
   }
   def int(key: String, default: Int = 0) = {
      val s = apply(key).getOrElse(default.toString) 
      try {s.toInt}
      catch {case _: Exception => throw ParseError("integer expected: " + s)}
   }
}

object WebQuery {
  /** parses k1=v1&...&kn=vn */
  def parse(query: String): WebQuery = {
     val kvs = utils.MyList.fromString(query, "&")
     val pairs = kvs map {s =>
        val i = s.indexOf("=")
        if (i == -1 || i == s.length-1) (s, "")
        else (s.substring(0,i), s.substring(i+1))
     }
     WebQuery(pairs)
  }
}

/** the body of an HTTP request
 *  
 *  This class abstracts from tiscaf's HTalk internal.
 */
class Body(tk: HTalk) {
  /** returns the body of a request as a string */
  def asString: String = {
    val bodyArray: Array[Byte] = tk.req.octets.getOrElse(throw ServerError("no body found"))
    new String(bodyArray, "UTF-8")
  }

  /** returns the body of a request as XML */
  def asXML: Node = {
    val bodyString = asString
    val bodyXML = try {
      scala.xml.XML.loadString(bodyString).head
    } catch {
      case _ : Throwable => throw ServerError("invalid XML")
       }
       scala.xml.Utility.trim(bodyXML)
     }
}


import Server._

/** An HTTP RESTful server. */
class Server(val port: Int, controller: Controller) extends HServer with Logger {

  override def name = "MMT rest server"
 // override def writeBufSize = 16 * 1024
  override def tcpNoDelay = true // make this false if you have extremely frequent requests
  override def startStopListener = {} // prevents tiscaf from creating a "stop" listener
  override def onMessage(s: String) {
     controller.report("tiscaf", s)
  }
  override def onError(e: Throwable) {
     logError("error in underlying server: " + e.getClass + ":" + e.getMessage + "\n" + e.getStackTrace.map(_.toString).mkString("","\n",""))
  }
  protected def ports = Set(port) // port to listen to
  protected def apps = List(new RequestHandler) // RequestHandler is defined below
  protected def talkPoolSize = 4
  protected def talkQueueSize = Int.MaxValue
  protected def selectorPoolSize = 2

  val logPrefix = "server"
  val report = controller.report
  
  protected class RequestHandler extends HApp {
    //override def buffered = true
    override def chunked = true // Content-Length is not set at the beginning of the response, so we can stream info while computing/reading from disk
    def resolve(req: HReqData): Option[HLet] = {
      log("request for /" + req.uriPath + " " + req.uriExt.getOrElse("") + "?" + req.query)
      req.uriPath.split("/").toList match {
        case ":change" :: _ => Some(ChangeResponse)
        case ":mws" :: _ => Some(MwsResponse)
        case ":parse" :: _ => Some(ParserResponse)
        case ":post" :: _ => Some(PostResponse)
        case hd::tl if hd.startsWith(":") =>
          controller.extman.getServerPlugin(hd.substring(1)) match {
            case Some(pl) =>
               val hlet = new HLet {
                  def aact(tk: HTalk)(implicit ec : ExecutionContext) : Future[Unit] = {
                     log("handling request via plugin " + pl.logPrefix)
                     val hl = try {
                        pl(tl, req.query, new Body(tk))
                     } catch {
                        case e: Error => errorResponse(e)
                     }
                     hl.aact(tk)
                  }
               }
               Some(hlet)
            case None => Some(errorResponse("no plugin registered for context " + hd))
          }
        // empty path 
        case List("") | Nil => Some(resourceResponse("browse.html"))
        // other resources
        case _ => Some(resourceResponse(req.uriPath))
      }
    }
  }

  /**
   * A resource response that the server sends back to the browser
   * @param path the path to the resource
   */
  private def resourceResponse(path: String): HLet = new HSimpleLet {
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
      }
    }
  }

  /** Response when the first path component is :search */
  private def MwsResponse: HLet = new HLet {
    def aact(tk: HTalk)(implicit ec : ExecutionContext) : Future[Unit] = {
      val body = new Body(tk)
      val resp = try {
        //
        val offset = tk.req.header("Offset") match {
          case Some(s) => try s.toInt catch { case _ : Throwable => 0 }
          case _ => 0
        }
        val size = tk.req.header("Size") match {
          case Some(s) => try s.toInt catch { case _ : Throwable => 30 }
          case _ => 30
        }
        val query = tk.req.query
        val qt = controller.extman.getQueryTransformer(query).getOrElse(TrivialQueryTransformer)
        val (mwsquery, params) = query match {
          case "mizar" =>
            val bodyXML = body.asXML
            val mmlVersion = tk.req.header("MMLVersion") match {
              case Some(s) => s
              case _ => "4.166"
            }
            val currentAid = tk.req.header("Aid") match {
              case Some(s) => s
              case _ => "HIDDEN"
            }
            (bodyXML, List(currentAid, mmlVersion))
          case "tptp" => (scala.xml.Text(body.asString), Nil)
          case "lf" =>
             val scope = tk.req.header("scope") match {
               case Some(s) => 
                 Path.parse(s) match {
                   case mp : MPath =>  mp
                   case _ => throw ServerError("expected mpath found : " + s)
                 }
               case _ => throw ServerError("expected a scope (mpath) passed in header")
             }
             val termParser = controller.textParser
             val tm = try {
               val str = body.asString
               termParser(parser.ParsingUnit(parser.SourceRef.anonymous(str), objects.Context(scope), str))(ErrorThrower)
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
               case _ => new scala.xml.Elem(n.prefix, n.label, n.attributes, n.scope, true, n.child.map(x => genQVars(x)) :_*)
             }
             val processedQuery = genQVars(tm.toCML)
             (<mws:expr>{processedQuery}</mws:expr>, Nil)
          case _ => (body.asXML, Nil) // default: body is forwarded to MWS untouched
        }
        val tqs = qt.transformSearchQuery(mwsquery, params)
        def wrapMWS(n: Node): Node = <mws:query output="xml" limitmin={ offset.toString } answsize={ size.toString }>{ n }</mws:query>
        val mws = controller.extman.mws.getOrElse(throw ServerError("no MathWebSearch engine defined")).url

        tqs.map(q => println(wrapMWS(q)))
        val res = tqs.map(q => utils.xml.post(mws, wrapMWS(q))) // calling MWS via HTTP post
        val total = res.foldRight(0)((r, x) => x + (r \ "@total").text.toInt)
        val totalsize = res.foldRight(0)((r, x) => x + (r \ "@size").text.toInt)
        val answrs = res.flatMap(_.child)
        val node = <mws:answset total={ total.toString } size={ totalsize.toString } xmlns:mws="http://www.mathweb.org/mws/ns">{ answrs }</mws:answset>
        XmlResponse(node)
      } catch {
        case e : Error => errorResponse(e)
      }
      resp.aact(tk)
    }
  }
  
  /** 
   * PostResponse is a response to a requests that posts content to MMT
   *  
   */
  private def PostResponse : HLet = new HLet {
    def aact(tk : HTalk)(implicit ec : ExecutionContext) : Future[Unit] = {
      try {
        val content = tk.req.param("body").getOrElse(throw ServerError("found no body in post req"))
        val format = tk.req.param("format").getOrElse("mmt")
        val dpathS = tk.req.param("dpath").getOrElse(throw ServerError("expected dpath"))
        val dpath = DPath(URI(dpathS))
        log("Received content : " + content)
        controller.textParser.readString(dpath.uri, dpath, content)(ErrorThrower)
        TextResponse("Success").aact(tk)
      } catch {
        case e : Error => errorResponse(e).aact(tk)
      }
    }    
  }
  
  
  private def ChangeResponse : HLet = new HLet {
    def aact(tk : HTalk)(implicit ec : ExecutionContext) : Future[Unit] = {
      try {
        val body = new Body(tk)
        val bodyString = body.asString
        val bodyXML = Utility.trim(XML.loadString(bodyString))
        val reader = new moc.DiffReader(controller)
        val diff = reader(bodyXML)
        moc.Patcher.patch(diff, controller)
        TextResponse("Success").aact(tk)
      } catch {
        case e : Error => errorResponse(e).aact(tk) 
      }
    }
  }


  private def ParserResponse : HLet = new HLet {
    def aact(tk : HTalk)(implicit ec : ExecutionContext) : Future[Unit] = {
      val text = tk.req.param("text").getOrElse(throw ServerError("found no text to parse"))
      val save = tk.req.param("save").map(_ == "true").getOrElse(false) //if save parameter is "true" then save otherwise don't
      tk.req.query.split("\\?").toList match {
        case strDPath :: strThy :: Nil =>
          val dpath = DPath(URI(strDPath))
          val mpath = dpath ? strThy
          val ctrl = new Controller(controller.report)
          val reader = new TextReader(controller, ctrl.add)
          val res = reader.readDocument(text, dpath)(pu => controller.textParser(pu)(ErrorThrower))
          res._2.toList match {
            case Nil => //no error -> parsing successful
              try {
                val mod = ctrl.memory.content.getModule(mpath)
                val nset = DPath(URI("http://cds.omdoc.org/styles/lf/mathml.omdoc")) ? "twelf"  //TODO get style from server js use ExtensionManager.getPresenter
                val rb = new presentation.StringBuilder()
                val module = if(save) {
                  controller.get(mpath)
                } else {
                  mod
                }
                val presenter = new archives.HTMLExporter() 
                presenter(module)(rb)
                val thyString = rb.get
                val response = new collection.mutable.HashMap[String,Any]()
                response("success") = "true"                                      
                val sdiff = controller.detectChanges(List(mod))
                save match {
                  case false => //just detecting refinements
                    val refs = controller.detectRefinements(sdiff)
                    response("info") = JSONArray(refs)
                    response("pres") =  thyString
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
                JsonResponse(JSONObject(response.toMap)).aact(tk)
              }  catch {
                case e : Throwable =>
                  e.printStackTrace()
                  TextResponse(e.getMessage()).aact(tk)
              }
            case l => //parsing failed -> returning errors 
              val response = new collection.mutable.HashMap[String,Any]()
              response("success") = "false"
              response("info") = JSONArray(Nil)
              response("pres") = l.map(e => (<p>{e.getStackTrace().toString}</p>).toString).mkString("")
              JsonResponse(JSONObject(response.toMap)).aact(tk)
          }
        case _ => errorResponse("invalid theory name in query : {tk.req.query}").aact(tk)
      }
    }
  }
}

  /*
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
  */
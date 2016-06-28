package info.kwarc.mmt.api.web

import info.kwarc.mmt.api._

import frontend._
import backend._
import utils._

import tiscaf._
import tiscaf.let._

import scala.xml._
import scala.concurrent._


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
    *
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
    *
    * @param text the message that is sent in the HTTP body
   */
  def TextResponse(text: String, tp: String = "plain"): HLet = TypedTextResponse(text, "text/"+tp)

  /**
   * An XML response that the server sends back to the browser
    *
    * @param s the XML message that is sent in the HTTP body
   */
  def XmlResponse(s: String): HLet = TextResponse(s, "xml")

  /**
   * A json response
    *
    * @param json the message that is sent in the HTTP body
   */
  def JsonResponse(json: JSON) = TypedTextResponse(json.toString, "application/json")

  /**
   * An XML response that the server sends back to the browser
    *
    * @param node the XML message that is sent in the HTTP body
   */
  def XmlResponse(node: scala.xml.Node): HLet = TextResponse(node.toString, "xml")
  
  /** a response that sends an HTML error message to the browser */
  def errorResponse(msg: String): HLet = errorResponse(ServerError(msg))
  /** a response that sends an HTML error message to the browser */
  def errorResponse(error: Error): HLet = {
     TextResponse(s"""<div xmlns="${xml.namespace("html")}"><div>"""+error.toHTML+"</div></div>", "html")
  }
}

/** straightforward abstraction for web style key-value queries; no encoding, no duplicate keys */
case class WebQuery(pairs: List[(String,String)]) {
   /** @return the value of the key, if present */
   def apply(key: String) : Option[String] = pairs.find(_._1 == key).map(_._2) 
   /** @return the string value of the key, default value if not present */
   def string(key: String, default: => String = ""): String = apply(key).getOrElse(default)
   /** @return the boolean value of the key, default value if not present */
   def boolean(key: String, default: => Boolean = false) = apply(key).getOrElse(default.toString).toLowerCase match {
      case "false" => false
      case "" | "true" => true
      case s => throw ParseError("boolean expected: " + s)
   }
   /** @return the integer value of the key, default value if not present */
   def int(key: String, default: => Int = 0) = {
      val s = apply(key).getOrElse(default.toString) 
      try {s.toInt}
      catch {case _: Exception => throw ParseError("integer expected: " + s)}
   }
}

object WebQuery {
  /** parses k1=v1&...&kn=vn */
  def parse(query: String): WebQuery = {
     val kvs = utils.stringToList(query, "&")
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
      case _: Exception => throw ServerError("invalid XML")
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
      lazy val reqString = "/" + req.uriPath + " " + req.uriExt.getOrElse("") + "?" + req.query
      log("request for " + reqString)
      req.uriPath.split("/").toList match {
        case ":change" :: _ => Some(ChangeResponse)
        case ":mws" :: _ => Some(MwsResponse)
        case hd::tl if hd.startsWith(":") =>
          val pl = controller.extman.getOrAddExtension(classOf[ServerExtension], hd.substring(1)) getOrElse {
             return Some(errorResponse("no plugin registered for context " + hd))
          }
          val hlet = new HLet {
            def aact(tk: HTalk)(implicit ec : ExecutionContext) : Future[Unit] = {
               log("handling request via plugin " + pl.logPrefix)
               val hl = try {
                  pl(tl, req.query, new Body(tk))
               } catch {
                  case e: Error =>
                     errorResponse(e)
                  case e: Exception =>
                     val le = pl.LocalError("unknown error while serving " + reqString).setCausedBy(e)
                     errorResponse(le)
               }
               hl.aact(tk)
            }
          }
          Some(hlet)
        // empty path 
        case List("") | Nil => Some(resourceResponse("browse.html"))
        // other resources
        case _ => Some(resourceResponse(req.uriPath))
      }
    }
  }

  /**
   * A resource response that the server sends back to the browser
    *
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
        val qt = controller.extman.get(classOf[QueryTransformer], query).getOrElse(TrivialQueryTransformer)
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
             val tm = try {
               val str = body.asString
               val pr = controller.objectParser(parser.ParsingUnit(parser.SourceRef.anonymous(str), objects.Context(scope), str, NamespaceMap(scope.doc)))(ErrorThrower)
               pr.toTerm
             } catch {
               case e : Throwable =>
                 throw e
             }

             def genQVars(n : Node) : Node = n match {
               case a : scala.xml.Atom[_] => a
               case  <ci>{q}</ci> =>
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
        val mws = controller.extman.get(classOf[ontology.MathWebSearch]).headOption.getOrElse(throw ServerError("no MathWebSearch engine defined")).url

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
}


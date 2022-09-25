package info.kwarc.mmt.api.web
/*
import info.kwarc.mmt.api._
import utils._

import java.io.{IOException, InputStream}
import tiscaf._

import scala.collection.{immutable, mutable}

/** Implements an HTTP Server using Tiscaf */
trait TiscafServerImplementation extends HServer with ServerImplementation {
  override def name : String = serverName
  override def bindHost : String = listenAddress

  // log all the things
  override protected val logInternals = true
  override def error(msg: String, t: Throwable): Unit = handleError(t)
  override def warning(msg: String): Unit = handleMessage(msg)
  override def info(msg: String): Unit = handleMessage(msg)

  // override def writeBufSize = 16 * 1024
  // make this false if you have extremely frequent requests
  override def tcpNoDelay = true
  /* it seems tiscaf eagerly closes connections after 20 seconds of inactivity
   * so it can happen that the connections is already closed when we try to send the response
   * so we increase the timeout here
   */
  override def connectionTimeoutSeconds = 300
  // prevents tiscaf from creating a "stop" listener
  override def startStopListener = {}
  // port to listen to
  protected def ports = Set(listenPort)
  // handlers to call
  protected def apps = List(new RequestHandler)

  protected class RequestHandler extends HApp {
    //override def buffered = true
    override def chunked = true
    // Session tracking config
    override def sessionTimeoutMinutes = 1
    override def tracking = HTracking.Cookie
    override def cookieKey = "MMT_SESSIONID"
    override def sidKey = "MMT_SESSIONID"

    def resolve(req: HReqData): Option[HLet] = {
      Some(new HSimpleLet {
        def act(tk: HTalk) = {
          val response = handleRequest(ServerTiscafAdapter.tiscaf2Request(tk))
          // set the status code
          var tiscafRef = tk.setStatus(ServerTiscafAdapter.code2Tiscaf(response.statusCode))
          // set all the headers
          response.headers.foreach(fv => {
            tiscafRef = tiscafRef.setHeader(fv._1, fv._2)
          })
          response.output match {
            // send the byte array as a whole right away
            case Left(ary : Array[Byte]) => {
              tiscafRef.setContentLength(ary.length).write(ary)
            }
            // read input and send at the same time
            case Right(io : InputStream) => {
              val buffer = new Array[Byte](4096)
              // read from disk and write to network simultaneously
              @scala.annotation.tailrec
              def step(wasRead: Int): Unit = {if (wasRead > 0) {
                tiscafRef.write(buffer, 0, wasRead)
                step(io.read(buffer))
              }}
              step(io.read(buffer))
              io.close()
            }
          }
        }
      })
    }
  }

  /** stop the server; for some reason this throws IOException which we catch here */
  override def stop: Unit = {
    try {
      super.stop
    } catch {
      case io: IOException =>
    }
  }
}

/** contains all functions adapting tiscaf objects into external objects */
object ServerTiscafAdapter {

  /** cleans up a url */
  private def cleanupURL(url : String) : String = {
    // a stack of path components
    val stack = mutable.Stack[String]()
    // iterate over the components
    url.split("/").foreach {
      case ".." =>
        // one directory up => remove last element from the stack
        if(stack.nonEmpty){
          stack.pop()
        }
      // empty path or current path => do nothing
      case "." | "" =>
      // everything else => add path
      case s =>
          stack.push(s)
    }
    // add all the components back
    val cleanPath = stack.reverse.mkString("/")
    // and add back a final slash if needed
    if(url.endsWith("/")) cleanPath + "/" else cleanPath
  }

  /** creates a new request object from an internal tiscaf HReqData object */
  def tiscaf2Request(data: HReqData): ServerRequest = {
    val method = tiscaf2Method(data.method)
    val headers = data.headerKeys.map(k => (k, data.header(k).get)).toMap
    val session = None
    val pathStr = cleanupURL(data.uriPath.toString)
    val path = utils.stringToList(pathStr,"/")
    val query = data.query
    val body = new Body(None)
    ServerRequest(method, headers, session, path, query, body)
  }

  /** creates a new request object from a tiscaf HTalk */
  def tiscaf2Request(tk: HTalk): ServerRequest = {
    val body = tiscaf2Body(tk)
    val session = Some(Session(tk.ses.id))
    tiscaf2Request(tk.req).copy(body = body, session = session)
  }

  /** parses a tiscaf method type object into a request method */
  def tiscaf2Method(method: HReqType.Value): RequestMethod.Value = method match {
    case HReqType.Get => RequestMethod.Get
    case HReqType.PostData => RequestMethod.Post
    case HReqType.PostOctets => RequestMethod.Post
    case HReqType.PostMulti => RequestMethod.Post
    case HReqType.Delete => RequestMethod.Delete
    case HReqType.Options => RequestMethod.Options
    case HReqType.Head => RequestMethod.Head
    case HReqType.Invalid => RequestMethod.Head
  }

  /** parses a tiscaf object into a body */
  def tiscaf2Body(tk: HTalk) = {
    val map = immutable.Map.from(tk.req.paramsKeys.collect{
      case s if tk.req.param(s).isDefined => (s,tk.req.param(s).get)
    })
    new Body(tk.req.octets,map)
  }

  /** turns a statusCode into a Tiscaf StatusCode */
  def code2Tiscaf(code : Int) : HStatus.Value = {
    HStatus.strings.find(_._2._1 == code.toString).map(_._1).get
  }
}
*/

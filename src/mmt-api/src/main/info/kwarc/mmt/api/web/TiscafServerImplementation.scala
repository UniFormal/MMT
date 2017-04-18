package info.kwarc.mmt.api.web

import java.io.InputStream
import java.net.Authenticator.RequestorType

import tiscaf._

/** Implements an HTTP Server using Tiscaf */
trait TiscafServerImplementation extends HServer with ServerImplementation {

  override def name : String = serverName
  override def hostname : String = listenAddress

  override def onMessage(s: String): Unit = {
    handleMessage(s)
  }

  override def onError(e: Throwable) {
    handleError(e)
  }

  // override def writeBufSize = 16 * 1024
  // make this false if you have extremely frequent requests
  override def tcpNoDelay = true

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
    override def sessionTimeoutMinutes = 60
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
              tiscafRef
                .setContentLength(ary.length)
                .write(ary)
            }

            // read input and send at the same time
            case Right(io : InputStream) => {
              val buffer = new Array[Byte](4096)

              // buffer
              // read from disk and write to network simultaneously
              @scala.annotation.tailrec
              def step(wasRead: Int): Unit = if (wasRead > 0) {
                tiscafRef.write(buffer, 0, wasRead)
                step(io.read(buffer))
              }

              step(io.read(buffer))
              io.close()
            }
          }

        }
      })
    }
  }
}

/** contains all functions adapting tiscaf objects into external objects */
object ServerTiscafAdapter {

  /** parses a Tiscaf object into a body */
  def tiscaf2Body(tk: HTalk): Body = new Body(tk.req.octets)

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

  /** creates a new request object from an internal Tiscaf HReqData object */
  def tiscaf2Request(data: HReqData): ServerRequest = {
    val method = tiscaf2Method(data.method)
    val headers = data.headerKeys.map(k => (k, data.header(k).get)).toMap[String, String]
    val sessionID = None
    val pathStr = data.uriPath.toString
    val queryStr = data.query
    val body = new Body(None)
    ServerRequest(method, headers, sessionID, pathStr, queryStr, body)
  }

  /** creates a new request object from a tiscaf HTalk */
  def tiscaf2Request(tk: HTalk): ServerRequest = {
    val body = tiscaf2Body(tk)
    val sessionID = Some(Session(tk.ses.id))

    tiscaf2Request(tk.req).copy(body = body, sessionID = sessionID)
  }

  /** turns a statusCode into a Tiscaf StatusCode */
  def code2Tiscaf(code : Int) : HStatus.Value = {
    HStatus.strings.find(_._2._1 == code.toString).map(_._1).get
  }
}


package info.kwarc.mmt.api.web

import akka.http.scaladsl.model.HttpEntity.Chunked
import akka.http.scaladsl.model.HttpHeader.ParsingResult.Ok
import akka.http.scaladsl.model.{ContentType, ContentTypes, HttpEntity, HttpHeader, HttpRequest, HttpResponse, ResponseEntity}
import akka.util.ByteString
import info.kwarc.mmt.api.utils
import info.kwarc.mmt.server.AkkaServer

import java.io.IOException

trait AkkaServerImplementation extends AkkaServer with ServerImplementation {
  override def name: String = serverName

  override def bindHost: String = listenAddress

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

  override protected def process(request: HttpRequest): HttpResponse = {
    response2akka(handleRequest(akka2Request(request)))
  }

  def response2akka(res:ServerResponse): HttpResponse = {
    val ret = HttpResponse(
      res.statusCode,
      res.headers.filter(_._1 != "Content-Type").map(p =>HttpHeader.parse(p._1,p._2)).collect{
        case Ok(h,_) => h
      }.toSeq,
      res.output match {
        case utils.Left(value) => HttpEntity(value)
        case utils.Right(value) => HttpEntity(value.readAllBytes()) // TODO
      }
    )
    ret.withEntity(ret.entity.withContentType(ContentType.parse(res.contentType) match {
      case Right(value) => value
      case _ => throw ServerError("Unknown content type " + res.contentType)
    }))
  }

  def akka2Request(req:HttpRequest): ServerRequest = {
    import scala.jdk.CollectionConverters._
    ServerRequest(
      RequestMethod.withName(req.method.value),
      req.headers.map(h => h.name() -> h.value()).toMap,
      None,
      req.getUri().pathSegments().asScala.toList,
      req.getUri().asScala().queryString().getOrElse(""),
      new Body(None) // TODO
    )//,req.headers.map(_.))
  }

  protected class RequestHandler {

  } /*{
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
            case Left(ary: Array[Byte]) => {
              tiscafRef.setContentLength(ary.length).write(ary)
            }
            // read input and send at the same time
            case Right(io: InputStream) => {
              val buffer = new Array[Byte](4096)

              // read from disk and write to network simultaneously
              @scala.annotation.tailrec
              def step(wasRead: Int): Unit = {
                if (wasRead > 0) {
                  tiscafRef.write(buffer, 0, wasRead)
                  step(io.read(buffer))
                }
              }

              step(io.read(buffer))
              io.close()
            }
          }
        }
      })
    }
  } */
}

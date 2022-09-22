package info.kwarc.mmt.api.web

import akka.actor.typed.ActorSystem
import akka.http.scaladsl.Http
import info.kwarc.mmt.api.utils.{Left, Right}

import java.io.{IOException, InputStream}

trait AkkaServerImplementation extends ServerImplementation {
  // port to listen to
  protected def ports = Set(listenPort)

  override def bindHost: String = listenAddress
  // handlers to call
  protected def apps = List(new RequestHandler)

  protected class RequestHandler {} /*{
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

  //private var akkaserver : Option[]

  def start = {
    val system = ActorSystem(akka.actor.typed.scaladsl.Behaviors.empty,"MMT_SESSIONID")
    val akka_server = Http()(system).newServerAt(listenAddress,listenPort)
      .bindSync({req => ???})
  }

  /** stop the server; for some reason this throws IOException which we catch here */
  def stop: Unit = {
    try {
      ???//super.stop
    } catch {
      case io: IOException =>
    }
  }
}

package info.kwarc.mmt.server

import akka.actor.typed.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.{HttpRequest, HttpResponse}

import java.io.IOException
import java.util.concurrent.TimeUnit
import scala.concurrent.{Await, Future}
import scala.concurrent.duration.{Duration, FiniteDuration}

trait AkkaServer {
  protected def ports: Set[Int]

  //------------------------- to override ------------------------------

  /** The host the server binds to */
  protected def bindHost = "127.0.0.1"

  /** Returns the server name, used in response headers. */
  protected def name = "akkaserver"

  /** Returns the port number listened to for a stop message. */
  def stopPort: Int = 8911

  /** Returns the host to listen to for stop message. */
  def stopHost: String = "localhost"

  protected def tcpNoDelay: Boolean = false // use 'true' for benchmarking only!

  /** Sets it to true to have internal information from tiscaf logged */
  protected val logInternals: Boolean = false

  def connectionTimeoutSeconds: Int = 20 // is used for Keep-Alive mode (if turned on)
  // *and* in multiplexer to dispose dead selector's keys

  /** Called to log a tiscaf internal error.
    * '''Note''': This methods is not intended to be called by applications using tiscaf.
    * Override it to log using your favorite logging library.
    * By default, logs the message and the stack trace to stderr.
    */
  def error(msg: String, t: Throwable): Unit = if (logInternals) {
    Console.err.println(msg)
    t.printStackTrace
  }

  /** Called to log a tiscaf internal warning.
    * '''Note''': This methods is not intended to be called by applications using tiscaf.
    * Override it to log using your favorite logging library.
    * By default, logs the message to stdout.
    */
  def warning(msg: String): Unit = if (logInternals) {
    println(msg)
  }

  /** Called to log a tiscaf internal information.
    * '''Note''': This methods is not intended to be called by applications using tiscaf.
    * Override it to log using your favorite logging library.
    * By default, logs the message to stdout.
    */
  def info(msg: String): Unit = if (logInternals) {
    println(msg)
  }

  /** Executed at the end of the server is started */
  protected def onStart(): Unit = {}

  /** Executed before the server is stopped */
  protected def onStop(): Unit = {}

  /** Starts the stop listener.
    * Override if you want more elaborated shutdown procedure
    * (and replace tiscaf.HStop)
    */
  protected def startStopListener: Unit = {
    print("")
  }/* sync.Sync.spawnNamed("StopListener") {
    // only bind to localhost for stop message
    val serverSocket =
      new java.net.ServerSocket(stopPort, 0, java.net.InetAddress.getByName(stopHost))
    info(name + " stop listener is listening to port " + stopPort)
    val dataSocket = serverSocket.accept
    val ar = new Array[Byte](256)
    dataSocket.getInputStream.read(ar)
    if (new String(ar, "ISO-8859-1") startsWith ("stop")) {
      dataSocket.close; serverSocket.close; stop
    }
    else sys.error(name + ": invalid stop sequence")
  } */
  private var akkaserver : Set[Http.ServerBinding] = Set.empty

  protected def process(request:HttpRequest) : HttpResponse

  def start = {
    val system = ActorSystem(akka.actor.typed.scaladsl.Behaviors.empty, name)
    val akka_servers = ports.map { prt =>
      Http()(system).newServerAt(bindHost, prt)
        .bindSync(process).andThen(_.foreach(akkaserver += _))(scala.concurrent.ExecutionContext.global)
    }
  }

  /** stop the server; for some reason this throws IOException which we catch here */
  def stop: Unit = {
    try {
      akkaserver.foreach(s => Await.result(s.terminate(FiniteDuration.apply(1,TimeUnit.SECONDS)),Duration.Inf)) //super.stop
    } catch {
      case io: IOException =>
    }
  }
}

/** *****************************************************************************
 *  This file is part of tiscaf.
 *
 *  tiscaf is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU Lesser General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  Foobar is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public License
 *  along with tiscaf.  If not, see <http://www.gnu.org/licenses/>.
 *  ****************************************************************************
 */
package tiscaf

import javax.net.ssl.SSLEngine

import scala.concurrent._

/** A server provides:
 *   - few common settings,
 *   - list of [[tiscaf.HApp]]lications,
 *   - start/stop methods
 */
trait HServer extends HLoggable {

  self =>

  //------------------ to implement -----------------------------------

  /** Returns the list of available applications.
   *  @see [[tiscaf.HApp]]
   */
  protected def apps: Seq[HApp]

  /** Returns the list of ports, the server listens to. */
  protected def ports: Set[Int]

  //------------------------- to override ------------------------------

  /** The host the server binds to */
  protected def bindHost = "127.0.0.1"

  /** Returns the server name, used in response headers. */
  protected def name = "tiscaf"

  /** Returns the port number listened to for a stop message. */
  def stopPort: Int = 8911

  /** Returns the host to listen to for stop message. */
  def stopHost: String = "localhost"

  protected def tcpNoDelay: Boolean = false // use 'true' for benchmarking only!

  /** Returns the executor pool size. */
  protected def poolSize: Int = Runtime.getRuntime.availableProcessors * 2

  /** Returns the executor queue size. */
  protected def queueSize: Int = Int.MaxValue

  /** Sets it to true to have internal information from tiscaf logged */
  protected val logInternals: Boolean = false

  /** Returns the NIO buffer size. */
  def bufferSize: Int = 4096 // public to be used in, say, FsLet

  /** Returns the connection timeout. It has to purposes:
   *   - a connection without any socket activity during this period will be closed
   *   - if you use (and client wants) 'keep-alive' connection, this period is declared in response header
   */
  def connectionTimeoutSeconds: Int = 20 // is used for Keep-Alive mode (if turned on)
  // *and* in multiplexer to dispose dead selector's keys

  /** Returns the time a shutdown process let the HLets a chance to finish properly. */
  def interruptTimeoutMillis: Int = 1000 // at shutdown process take HLets a chance to finish 

  /** The list of defaults headers that are set by the server for each request.
   *  Each let may override (or remove) them later */
  def defaultHeaders: Map[String, String] = Map()

  /** Called to log a tiscaf internal error.
   *  '''Note''': This methods is not intended to be called by applications using tiscaf.
   *  Override it to log using your favorite logging library.
   *  By default, logs the message and the stack trace to stderr.
   */
  def error(msg: String, t: Throwable): Unit = if(logInternals) {
    Console.err.println(msg)
    t.printStackTrace
  }

  /** Called to log a tiscaf internal warning.
   *  '''Note''': This methods is not intended to be called by applications using tiscaf.
   *  Override it to log using your favorite logging library.
   *  By default, logs the message to stdout.
   */
  def warning(msg: String): Unit = if(logInternals) {
    println(msg)
  }

  /** Called to log a tiscaf internal information.
   *  '''Note''': This methods is not intended to be called by applications using tiscaf.
   *  Override it to log using your favorite logging library.
   *  By default, logs the message to stdout.
   */
  def info(msg: String): Unit = if(logInternals) {
    println(msg)
  }

  /** Returns the maximum upload size allowed. */
  protected def maxPostDataLength: Int = 65536 // for POST other than multipart/form-data

  /** Executed at the end of the server is started */
  protected def onStart(): Unit = {}

  /** Executed before the server is stopped */
  protected def onStop(): Unit = {}

  /** Starts the stop listener.
   *  Override if you want more elaborated shutdown procedure
   *  (and replace tiscaf.HStop)
   */
  protected def startStopListener: Unit = sync.Sync.spawnNamed("StopListener") {
    // only bind to localhost for stop message
    val serverSocket =
      new java.net.ServerSocket(stopPort, 0, java.net.InetAddress.getByName(stopHost))
    info(name + " stop listener is listening to port " + stopPort)
    val dataSocket = serverSocket.accept
    val ar = new Array[Byte](256)
    dataSocket.getInputStream.read(ar)
    if (new String(ar, "ISO-8859-1") startsWith ("stop")) { dataSocket.close; serverSocket.close; stop }
    else sys.error(name + ": invalid stop sequence")
  }

  /** Returns the SSL settings if any. */
  def ssl: List[HSslContext] = Nil

  //-------------------------- user API ---------------------------------

  /** Starts the server. */
  def start: Unit = synchronized {
    if (isStopped.get) {
      talksExe.prepare()
      plexer.start
      ports.foreach { port => plexer.addListener(peerFactory, bindHost, port) }
      // listen to SSL ports if any configured
      val sslPorts = ssl.map { ssl =>
        plexer.addSslListener(peerFactory, bindHost, ssl)
        ssl.port
      }
      startStopListener
      onStart
      isStopped.set(false)
      info(name + " server was started on port(s) " + (ports.toList ++ sslPorts).sortBy(x => x).mkString(", "))
    } else sys.error("the server is already started")
  }

  /** Stops the server. */
  def stop: Unit = synchronized {
    if (!isStopped.get) {
      isStopped.set(true)
      onStop
      talksExe.stopAccepting
      Thread.sleep(interruptTimeoutMillis)
      talksExe.shutdown
      plexer.stop
      info(name + " server stopped")
    } else sys.error("the server is already stopped")
  }

  //--------------------------- internals -------------------------------

  // nothing must be started in init, so using few objects and lazy vals

  private lazy val talksExe = new sync.SyncQuExecutionContext(poolSize, queueSize, "ServerExecutorPool") {
    // HLoggable API
    def error(msg: String, t: Throwable) = self.error(msg, t)
    def warning(msg: String) = self.warning(msg)
    def info(msg: String) = self.info(msg)
  }
  private val timeoutMillis = connectionTimeoutSeconds * 1000L
  private val isStopped = new sync.SyncBool(true)

  private object plexer extends HPlexer {
    def timeoutMillis: Long = self.timeoutMillis
    def tcpNoDelay: Boolean = self.tcpNoDelay

    // HLoggable API
    def error(msg: String, t: Throwable) = self.error(msg, t)
    def warning(msg: String) = self.warning(msg)
    def info(msg: String) = self.info(msg)

    def ssl = self.ssl
  }

  import java.nio.channels.SelectionKey

  // key place
  private def peerFactory(aKey: SelectionKey, engine: Option[SSLEngine]): HPeer =
    engine match {
      case Some(sslEngine) =>
        // SSL connection
        new HSslPeer { self =>

          def plexer: HPlexer = HServer.this.plexer
          def key: SelectionKey = aKey

          def bufferSize: Int = session.getApplicationBufferSize

          // HLoggable API
          def error(msg: String, t: Throwable) = HServer.this.error(msg, t)
          def warning(msg: String) = HServer.this.warning(msg)
          def info(msg: String) = HServer.this.info(msg)

          def engine = sslEngine

          implicit def talksExe = HServer.this.talksExe

          val acceptor =
            new HAcceptor(
              new HWriter(self),
              apps,
              HServer.this.connectionTimeoutSeconds,
              self,
              maxPostDataLength,
              defaultHeaders)

          def submit(toRun: =>Unit): Unit = if (!isStopped.get) {
            info("Task submitted to SSL peer")
            Future(toRun)
          }
        }
      case None =>
        // non SSL connection
        new HSimplePeer { self =>

          def plexer: HPlexer = HServer.this.plexer
          def key: SelectionKey = aKey

          def bufferSize: Int = HServer.this.bufferSize

          // HLoggable API
          def error(msg: String, t: Throwable) = HServer.this.error(msg, t)
          def warning(msg: String) = HServer.this.warning(msg)
          def info(msg: String) = HServer.this.info(msg)

          implicit def talksExe = HServer.this.talksExe

          val acceptor =
            new HAcceptor(
              new HWriter(self),
              apps,
              HServer.this.connectionTimeoutSeconds,
              self,
              maxPostDataLength,
              defaultHeaders)

          def submit(toRun: =>Unit): Unit = if (!isStopped.get) {
            info("Task submitted to plain peer")
            Future(toRun)
          }
        }
    }

}


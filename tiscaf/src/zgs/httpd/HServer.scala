package zgs.httpd


trait HServer {

  //------------------ to implement -----------------------------------
  
  protected def apps : Seq[HApp]
  
  protected def ports : Seq[Int]
  
  protected def talkPoolSize : Int  // threads
  protected def talkQueueSize : Int // tasks
  
  protected def selectorPoolSize : Int // threads
  
  //------------------------- to override ------------------------------
  
  protected def name = "tiscaf" // ti-ny Sca-la f-ramework
  
  protected def stopPort : Int = 8911
  
  protected def tcpNoDelay : Boolean = false // use 'true' for benchmarking
  
  protected def selectorQueueSize : Int = Int.MaxValue // limit nio selector events queue size

  protected def readBufSize : Int  = 512  // nio read...
  protected def writeBufSize : Int = 4096 // ... and write buffers sizes

  protected def connectionTimeoutSeconds : Int  = 30  // is used for Keep-Alive mode (if turned on)
                                                      // *and* in multiplexer to dispose dead channels
  
  protected def interruptTimeoutMillis : Int = 1000 // for stopping: take HLets a chance to finish 

  protected def onError(e : Throwable) : Unit = e.printStackTrace

  //FR: make message handling overridable to avoid printing
  protected def onMessage(s:String): Unit = {println(s)}


  // override if you want more elaborated shutdown procedure (and replace zgs.httpd.HStop)
  protected def startStopListener : Unit = zgs.sync.Sync.spawn {
    val serverSocket = new java.net.ServerSocket(stopPort)
    val dataSocket  = serverSocket.accept
    val ar = new Array[Byte](256)
    dataSocket.getInputStream.read(ar)
    if (new String(ar, "ISO-8859-1") startsWith("stop")) { dataSocket.close; serverSocket.close; stop }
    else onMessage(name + ": invalid stop sequence")
    dataSocket.close; serverSocket.close; stop 
  }

  //-------------------------- user API ---------------------------------
  
  final def start : Unit = synchronized {
    if (stopped.get) {
      plexer.start
      ports.foreach { port => plexer.addServer(peerFactory, port) }
      //startStopListener
      stopped.set(false)
      onMessage(name + " server was started on port(s) " + ports.mkString(", "))
    }
    else sys.error("the server is already started")
  }
  
  final def stop : Unit = synchronized {
    if (!stopped.get) {
      stopped.set(true)
      talksExe.shutdown
      Thread.sleep(interruptTimeoutMillis)
      talksExe.shutdownNow 
      plexer.stop
      onMessage(name + " server stopped")
    }
    else sys.error("the server is already stopped")
  }
  
  //--------------------------- internals -------------------------------
  
  // nothing must be started in init, so using few objects and lazy vals
  
  private lazy val talksExe = new zgs.sync.SyncExe(talkPoolSize, talkQueueSize, "Talks")
  
  private val connTimeoutMillis : Long = connectionTimeoutSeconds * 1000
  
  private val stopped = new zgs.sync.SyncBool(true)
  
  private object plexer extends HPlexer {
    def connectionTimeout : Long      = HServer.this.connTimeoutMillis
    def selectorPoolSize : Int        = HServer.this.selectorPoolSize
    def selectorQueueSize : Int       = HServer.this.selectorQueueSize
    def tcpNoDelay : Boolean          = HServer.this.tcpNoDelay
    def onError(e : Throwable) : Unit = HServer.this.onError(e)
  }

  import java.nio.channels. { SelectionKey, SocketChannel }
  
  // key place
  private def peerFactory(key : SelectionKey) : HPeer = new HPeer { self =>

    def readBufSize : Int                         = HServer.this.readBufSize
    def writeBufSize : Int                        = HServer.this.writeBufSize
    def queueWakeup(want : PeerWant.Value) : Unit = HServer.this.plexer.toWake(key, want)
    def channel : SocketChannel         = key.channel.asInstanceOf[SocketChannel]
    def onError(e : Throwable) : Unit   = HServer.this.onError(e)
    def connectionTimeout : Long        = HServer.this.connTimeoutMillis

    private val connection =
      new HConn(new HStream(self), apps, HServer.this.connectionTimeoutSeconds, HServer.this.onError)

    def submit : Unit = if (!stopped.get)
      talksExe.submit(new Runnable { def run { try { connection.process } catch { case e => onError(e) } } })
  }

}

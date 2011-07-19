package zgs.httpd

//-------------------- !!! ---------------------------------
// almost all things which may be overriden are overriden:
// just for illustration purposes
//-------------------- !!! ---------------------------------


protected object HomeServer {
  val outBufSize = 4096 // used both for selector output buffers AND FsLet read buffers
}


class HomeServer extends HServer {

  private val appsList = List(new TheApp)
  
  //--------------- implementing
  
  protected def ports = List(8910) // ports to listen to
  protected def apps = appsList

  // executor in which HLet.act methods are executing
  protected def talkPoolSize  = 4 // may be there isn't any reason to make more rather cpuCount * 2
  protected def talkQueueSize = Int.MaxValue 
  
  //---------------- overriding

  override protected def stopPort = 8911
  override protected def tcpNoDelay = true // use 'true' for testing only
    
  // increase to speed up big files uploading
  override protected def readBufSize = 512 
  // increase to speed up big files downloading
  override protected def writeBufSize = HomeServer.outBufSize 
  
  // ATTENTION - this parameter has dual unsing:
  // 1. connections whos sockets were dead during this period, will be reset
  // 2. also is used for persistent connections timing out
  override protected def connectionTimeoutSeconds : Int  = 30
  
  // delay before stopping to take a chance to HLets to finish their talks
  override protected def interruptTimeoutMillis : Int = 1000

  // increase if you have plenty clients with poor reading from server
  override protected def selectorPoolSize : Int = 2
  
  // you can redirect it to your favourite logging system 
  override protected def onError(e : Throwable) : Unit = e.printStackTrace
  
  
  //------------ the only HApp
  
  protected class TheApp extends HApp {
  
    override def keepAlive = true
    override def chunked = false
    override def buffered = false
    override def gzip = true
  
    
    // turn sessions-tracking off
    override def tracking = HTracking.NotAllowed
    // override def sessionTimeoutMinutes = 30
  
    // dispatching
    def resolve(req : HReqHeaderData) = Some(homeLet) // the only handler 
  
    // browse over home dir
    private def homeLet = new let.FsLet {
      
      protected def dirRoot : String                  = System.getProperty("user.home")
      
      override protected def uriRoot : String         = "" // i.e. localhost:8910/
      override protected def allowLs                  = true // show directory listing
      override protected def indexes : List[String]   = Nil
      override protected def bufSize : Int            = HomeServer.outBufSize
      // treating unknown MIMEs as text/plain
      override protected def plainAsDefault : Boolean = false
    }
  }
}

// start/stop the server

object HomeServerStart { def main(args : Array[String]) = new HomeServer start }
object HomeServerStop  { def main(args : Array[String]) = new zgs.httpd.HStop(8911) stop }


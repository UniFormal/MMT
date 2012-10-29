package zgs.httpd

import scala.collection. { mutable => mute }


protected object HReqState extends Enumeration {
  val WaitsForHeader, WaitsForData, WaitsForOctets, WaitsForPart, IsInvalid, IsReady = Value
}


final private[httpd] class HConn(
  stream : HStream, apps : Seq[HApp], connectionTimeout : Int, onError : Throwable => Unit) {
  
  //----------------- connection state (too stateful) --------------
  
  private var state : HReqState.Value   = _

  private var tail : Array[Byte] = _
  private var acceptedTotalLength : Long = _

  private var header : Option[HReqHeader] = _
  private val parMap : mute.Map[String, Seq[String]] = new mute.HashMap[String, Seq[String]]
  
  private var appLet : Option[(HApp, HLet)] = _
  private var octetStream : Option[Array[Byte]] = _
  private var parts :  Option[HParts] = _
  
  def reset : Unit = {
    state  = HReqState.WaitsForHeader
    
    tail                = new Array[Byte](0)
    acceptedTotalLength = 0L
    
    header = None
    parMap.clear 
    
    appLet = None
    octetStream = None
    parts  = None
  }
  
  reset // prepare to new request
  
  //------------- SPI --------------------------
  
  // why synchronized? there is a gap between 'talk' and 'reset' at 'IsReady' case.
  // during this gap peer may get new data to process and calls HPeer.submit
  // before 'reset' is finished:
  // 'talk' put data to peer's outQ. This data is read by thread from another pool.
  // those threads read new data from socket and put it to inQ. HPeer.read calls 'submit'.
  // 'process' is the only entry (and synchronization point) from peer's threads - we
  // don't need to synchronize anything else: all other code is executed in a 'single
  // virtual connection thread' - for peers and for connection.
    
  def process : Unit = synchronized {
    tail ++= stream.readQueue
    // process tail
    state = state match {
      case HReqState.WaitsForHeader => inHeader
      case HReqState.WaitsForData   => inData
      case HReqState.WaitsForOctets => inOctets
      case HReqState.WaitsForPart   => inParts
      case x => x // don't change
    }
    // decide what to do further
    state match {
      case HReqState.IsReady   => tail = new Array[Byte](0); talk; reset // be ready for next alive talk
      case HReqState.IsInvalid => stream.dispose // Opera only listens to server during requesting
      case _ /* WaitsForXxx */ => stream.queueWakeup(PeerWant.Read)
    }
  }
  
  //---------------- delegatig here from user API in HTalk --------------
  
  def paramKeys : scala.collection.Set[String] = parMap.keySet

  def params(key : String) : Seq[String] = parMap.getOrElse(key, Nil)
  
  def param(key : String) : Option[String] = params(key) match {
    case Seq(x, _*) => Some(x.toString)
    case _          => None
  }

  def headerData : HReqHeaderData = header.getOrElse(sys.error("HReqHeaderData isn't defined yet"))
  
  def octets : Option[Array[Byte]] = octetStream

  //------------------------ internals -------------------------------
  
  //--------------------- header
  
  private def maxHeaderLength   = 8192
  private def maxPostDataLength = 65536
  
  private def inHeader : HReqState.Value = if (tail.size < 28) HReqState.WaitsForHeader else {
    
    val till = tail.size - 4
    
    def findEol(idx : Int) : Option[Int] = (0 to till).find { idx =>
      tail(idx)     == 13 &&
      tail(idx + 1) == 10 &&
      tail(idx + 2) == 13 && 
      tail(idx + 3) == 10
    }
    
    findEol(24) match {
      case None        => HReqState.WaitsForHeader
      case Some(shift) => if (shift >= maxHeaderLength) HReqState.IsInvalid else {
        val ar = tail.take(shift)
        tail = tail.slice(shift + 4, tail.size)
        val streamStrings = new String(ar, "ISO-8859-1") split("\r\n")
        header = Some(new HReqHeader(streamStrings))
        header.get.reqType match {
          case HReqType.Get        => parseParams(header.get.query); HReqState.IsReady
          case HReqType.PostData   => parseParams(header.get.query); inData
          case HReqType.PostOctets => parseParams(header.get.query); inOctets
          case HReqType.PostMulti  => parseParams(header.get.query); inParts
          case HReqType.Invalid    => HReqState.IsInvalid
        }
      }
    }
  }
  
  //---------------- post, form data
 
  private def inData : HReqState.Value = {
    val length = header.get.contentLength.get.toInt
    if (length > maxPostDataLength) HReqState.IsInvalid
    else if (length > tail.size) HReqState.WaitsForData else {
      parseParams(new String(tail.take(length), "ISO-8859-1"))
      HReqState.IsReady 
    }
  }
  
  //---------------- post, octets (unknown content type, falling back)
 
  private def inOctets : HReqState.Value = {
    val length = header.get.contentLength.get.toInt
    if (length > maxPostDataLength) HReqState.IsInvalid
    else if (length > tail.size) HReqState.WaitsForOctets else {
      val ar = new Array[Byte](length)
      (0 until length).foreach { i => ar(i) = tail(i)}
      octetStream = Some(ar)
      HReqState.IsReady 
    }
  }
  
  //---------------- post, multipart
 
  private def inParts : HReqState.Value = {
    
    def acceptResult(finalState : HReqState.Value) : HReqState.Value = {
      acceptedTotalLength += tail.size
      if (acceptedTotalLength > header.get.contentLength.get) HReqState.IsInvalid
      else finalState
    }
    
    if (appLet.isEmpty) appLet = Some(HResolver.resolve(apps, header.get))
    
    appLet.get._2.partsAcceptor(headerData) match {
      case None           => HReqState.IsInvalid
      case Some(acceptor) => 
        if (parts.isEmpty) {
          // at this point tail must have at least --boundary\r\n... or wait for bytes further
          val minSize = HParts.toBytes("--" + headerData.boundary.get + "\r\n").size
          if (tail.size >= minSize) parts = Some(new HParts(acceptor, headerData))
        }
        if (parts.isEmpty) HReqState.WaitsForPart else parts.get.processNextBytes(tail) match {
          case HReqState.IsInvalid => 
            HReqState.IsInvalid
          case x => // HReqState.IsReady or HReqState.WaitsForPart
            val result = acceptResult(x)
            tail = new Array[Byte](0) // parts eat all the tail
            result
        }
    }
    
  }
  
  //----------------- common
  
  private def parseParams(s : String) : Unit = s.split("&").foreach { tok => if (tok.length >= 2) {
    val items = tok.split("=", 2)
    val key = items(0)
    val value = if (items.size == 2) items(1).trim else ""
    if (key.size != 0) parMap.get(key) match {
      case None => 
        val buf = new mute.ArrayBuffer[String]
        buf += value
        parMap(key) = buf
      case Some(buf) => buf + value 
    }
  }}
  
  private def talk : Unit = {
    // we already have header here
    if (appLet.isEmpty) appLet = Some(HResolver.resolve(apps, header.get))
    val (app, let) = appLet.get
    val tk = new HTalk(app, this, stream)

    if (HSess.count(app) > app.maxSessionsCount)
      try {
        new zgs.httpd.let.ErrLet(HStatus.ServiceUnavailable) act(tk)
        sys.error("too many sessions for HApp " + app.getClass.getName)
      } catch { case e => onError(e) }
    else {
      trackConnection(app, tk)
      try { if (let.before.forall( be => { be.act(tk); !tk.isClosed })) let.act(tk) }
      catch { case e => onError(e); if (!tk.isClosed) new zgs.httpd.let.ErrLet(HStatus.NotFound) act(tk) }
    }
    
    tk.close // if user didn't
  }
  
  private def trackConnection(app : HApp, tk : HTalk) : Unit = 
    if (app.keepAlive && header.get.isPersistent)
      tk.setHeader("Connection", "keep-alive")
        .setHeader("Keep-Alive", "timeout=" + connectionTimeout + ", max=100")
    else 
      tk.setHeader("Connection", "close")
        .removeHeader("Keep-Alive")  
  
}


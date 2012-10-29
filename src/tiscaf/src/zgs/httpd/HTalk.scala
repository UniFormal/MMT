package zgs.httpd

import java.util.Date

final class HTalk(private val app : HApp, 
                  private val connection : HConn, 
                  private val stream : HStream) {
  
  //----------------------------------------------------------------
  //---------------------------- main user API ---------------------
  //----------------------------------------------------------------
  
  //------------ request info -----------

  object req {
    // common
    def method : HReqType.Value  = connection.headerData.reqType
    def host : Option[String]    = connection.headerData.host
    def port : Option[String]    = connection.headerData.port
    def uriPath : String         = decode(connection.headerData.uriPath)
    def uriExt : Option[String]  = connection.headerData.uriExt // no decoding needed
    def query : String           = decode(connection.headerData.query)
    def remoteIp : String        = stream.remoteIp 
  
    // header
    def header(key : String): Option[String]      = connection.headerData.header(key)
    def headerKeys : scala.collection.Set[String] = connection.headerData.headerKeys
  
    // parameters
    def paramKeys : Seq[String]              = connection.paramKeys.map(decode).toSeq
    def params(key : String) : Seq[String]   = connection.params(encode(key)).map(decode)
    def param(key : String) : Option[String] = connection.param(encode(key)).map(decode)
    def paramEncoding : String               = app.paramsEncoding

    // param(key) helpers
    def asByte(key : String) : Option[Byte]     = try { Some(param(key).get.toByte) } catch { case _ => None }
    def asShort(key : String) : Option[Short]   = try { Some(param(key).get.toShort) } catch { case _ => None }
    def asInt(key : String) : Option[Int]       = try { Some(param(key).get.toInt) } catch { case _ => None }
    def asLong(key : String) : Option[Long]     = try { Some(param(key).get.toLong) } catch { case _ => None }
    def asFloat(key : String) : Option[Float]   = try { Some(param(key).get.toFloat) } catch { case _ => None }
    def asDouble(key : String) : Option[Double] = try { Some(param(key).get.toDouble) } catch { case _ => None }
    
    // POST/application/octet-stream case
    def octets : Option[Array[Byte]] = connection.octets
  }

  // ------------- setting status / response header -----------

  def setStatus(code : HStatus.Value) : HTalk               = { checkHead; resp.setStatus(code); this }
  def setStatus(code : HStatus.Value, msg : String) : HTalk = { checkHead; resp.setStatus(code, msg); this }
  
  def setHeader(name : String, value : String) : HTalk = { checkHead; resp.setHeader(name, value); this }
  def removeHeader(name : String) : Option[String]     = { checkHead; resp.removeHeader(name) }
  def getHeader(name : String) : Option[String]        = { checkHead; resp.getHeader(name) }
  
  def setContentLength(length : Long) : HTalk        = setHeader("Content-Length", length.toString)
  def setCharacterEncoding(charset : String) : HTalk = setHeader("Character-Encoding", charset)
  def setContentType(cType : String) : HTalk         = setHeader("Content-Type", cType)
  
  //------------------- output/close -------------------------
  
  def write(ar : Array[Byte], offset : Int, length : Int) : HTalk = { 
    getOut.write(ar, offset, length) 
    this
  }
  
  def write(ar : Array[Byte]) : HTalk = write(ar, 0, ar.length)
  
  def close : Unit = getOut.close // it is safe to call multiple close
  def isClosed : Boolean = if (out.isEmpty) false else out.get.isClosed

  
  //------------------------ session-related -------------------
  
  object ses extends scala.collection.mutable.Map[Any,Any] {

    // implementing mutable.Map

    def get(key: Any): Option[Any]     = if (isAllowed) session.get.get(key) else None
    def iterator: Iterator[(Any, Any)] = if (isAllowed) session.get.iterator else Nil.iterator
    def += (kv: (Any, Any)): this.type = if (isAllowed) { session.get += kv; this } else this
    def -= (key: Any): this.type       = if (isAllowed) { session.get -= key; this } else this

    override def size : Int   = if (isAllowed) session.get.size else 0
    override def clear : Unit = if (isAllowed) session.get.clear
    
    override def foreach[U](f : Tuple2[Any,Any] => U) : Unit =
      if (isAllowed) session.get.foreach[U] { t => f(t) }

    // Map-related helpers

    def asString(key : Any) : Option[String]   = get(key).collect { case x : String => x }
    def asBoolean(key : Any) : Option[Boolean] = get(key).collect { case x : Boolean => x }
    def asByte(key : Any) : Option[Byte]       = get(key).collect { case x : Byte => x }
    def asShort(key : Any) : Option[Short]     = get(key).collect { case x : Short => x }
    def asInt(key : Any) : Option[Int]         = get(key).collect { case x : Int => x }
    def asLong(key : Any) : Option[Long]       = get(key).collect { case x : Long => x }
    def asFloat(key : Any) : Option[Float]     = get(key).collect { case x : Float => x }
    def asDouble(key : Any) : Option[Double]   = get(key).collect { case x : Double => x }
    def asDate(key : Any) : Option[Date]       = get(key).collect { case x : Date => x }

    def clearKeeping(keysToKeep : Any*) : Unit =
      if (isAllowed) (this.keySet -- keysToKeep.toSet).foreach(remove(_))

    // session-specific

    def tracking : HTracking.Value = app.tracking
    def isAllowed : Boolean        = tracking != HTracking.NotAllowed
    def isValid : Boolean          = isAllowed && session.get.isValid
    def invalidate : Unit          = if (isAllowed) session.get.invalidate
    
    def idKey : String    = HApp.sidKey
    def id : String       = if (isAllowed) session.get.sid else ""
    def idPhrase : String = if (isAllowed) (";" + idKey + "=" + id) else ""
  }
  
  //---------------------- internals -----------------------------
  
  private val resp    = new HResponse
  private val session : Option[HSess] = 
    if (app.tracking == HTracking.NotAllowed) None
    else Some(new HSess(app, connection, resp))
  
  // we can not define 'out' before header's Content-Type is defined (if it will)
  private var out : Option[HOut] = None
  
  private def checkHead = if (out.isDefined && out.get.isHeaded) {
    stream.dispose
    sys.error("header is already sent")
  }

  private def getOut : HOut = out.getOrElse {
    def aliveReq = try { connection.headerData.isPersistent } catch { case _ => false }
    val o =
      if (gzipable)          new HGzipOut(aliveReq, resp, app.chunked, stream, session)
      else if (app.buffered) new HBufferedOut(aliveReq, resp, app.chunked, stream, session)
      else                   new HIdentOut(aliveReq, resp, app.chunked, stream, session)
    out = Some(o)
    o
  }

  private def gzipable = {
    def accepted = req.header("Accept-Encoding") match {
      case None    => false
      case Some(x) => x.contains("gzip")
    }
    def mimeOk = resp.getHeader("Content-Type") match {
      case None    => false
      case Some(x) => HMime.gzipable.contains(x.split(";")(0).trim)
    }
    app.gzip && accepted && mimeOk
  }
  
  private def encode(s : String) = try { java.net.URLEncoder.encode(s, app.paramsEncoding) } catch { case _ => s }
  private def decode(s : String) = try { java.net.URLDecoder.decode(s, app.paramsEncoding) } catch { case _ => s }
}


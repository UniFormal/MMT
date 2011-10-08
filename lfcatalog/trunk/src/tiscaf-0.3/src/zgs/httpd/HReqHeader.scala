package zgs.httpd


trait HReqHeaderData {
  def reqType : HReqType.Value
  def host    : Option[String]
  def port    : Option[String]
  def uriPath : String
  def uriExt  : Option[String]
  def query   : String
  
  def header(key : String): Option[String]
  def headerKeys : scala.collection.Set[String]
  
  def contentLength : Option[Long]
  def isPersistent : Boolean
  def boundary : Option[String]
}


object HReqType extends Enumeration {
  val Invalid    = Value("Invalid")
  val Get        = Value("GET")
  val PostData   = Value("POST/application/x-www-form-urlencoded")
  val PostOctets = Value("POST/application/octet-stream")
  val PostMulti  = Value("POST/multipart/form-data")
}


final private class HReqHeader(streamStrings : Seq[String]) extends HReqHeaderData {
  
  // need vars to keep state for long request
  
  private var aReqType : HReqType.Value = HReqType.Invalid
  
  private var aHost : Option[String]    = None
  private var aPort : Option[String]    = None
  private var anUriPath : String        = _
  private var anUriExt : Option[String] = None
  private var aQuery : String           = _
  
  private var aContentLength : Option[Long]   = None
  private var aPersistence   : Boolean        = false
  private var aBoundary      : Option[String] = _
  
  private val pairs = new scala.collection.mutable.HashMap[String, String]
  
  //------------- init: fill in all state data -------------
  
  // there may be wrapped strings in header
  private val strings : Seq[String] = {
    val unwrapped = new scala.collection.mutable.ArrayBuffer[String]
    streamStrings.foreach { raw =>
      if (raw.startsWith("\t") || raw.startsWith(" ")) {
        if (unwrapped.size == 0) unwrapped.append(raw.trim)
        else unwrapped(unwrapped.size - 1) = unwrapped.last + " " + raw.trim
      }
      else unwrapped.append(raw.trim)
    }
    unwrapped
  }
  
  if (parseFirstPhase) pairs.get("host") match {
    case None    => 
      aHost = None
      aPort = None
    case Some(h) =>
      val hostParts = h.split(":", 2)
      aHost = Some(hostParts(0))
      aPort = if (hostParts.size == 1) None else Some(hostParts(1))
  }
  
  aPersistence = pairs.get("connection") match {
    case None      => false
    case Some(con) => con.toLowerCase.contains("keep-alive")
  }
  
  //--------------------- internals ---------------------
  
  private def parseFirstPhase : Boolean = if (strings.size == 0) false else {
    val s = strings(0)
    val parts = s.split(" ")
    if (parts.size != 3 /* method uri protocol */) false else {
      val method = parts(0).trim
      aReqType = method match {
        case "GET"  => fillInPairs; HReqType.Get
        case "POST" => fillInPairs; parsePostMethod
        case _      => HReqType.Invalid
      }
      if (aReqType != HReqType.isInstanceOf) { parseUri(parts(1).trim); true } else false
    }
  }
  
  private def parsePostMethod : HReqType.Value = fillInContentLength match {
    case None     => HReqType.Invalid
    case Some(le) => pairs.get("content-type") match {
      case None    => HReqType.Invalid
      case Some(x) => x.toLowerCase match {
        case da if (da.contains("application/x-www-form-urlencoded")) =>
          HReqType.PostData 
        case mu if (mu.contains("multipart/form-data")) =>
          x.split(';').map(_.trim).find(_.toLowerCase.startsWith("boundary")) match {
            case None => HReqType.Invalid
            case Some(bnd) => try {
              aBoundary = Some(bnd.split("=", 2)(1).trim)
              HReqType.PostMulti 
            } catch { case _ => HReqType.Invalid }
          }
        case _ => HReqType.PostOctets // falling back to 
      }
    }
  }
  
  private def parseUri(s : String) : Unit = {
    val parts = s.split("\\?", 2)
    // anUriPath = java.net.URLDecoder.decode(parts(0).replace('\\', '/'), "UTF-8") // ANLI ???
    anUriPath = parts(0).replace('\\', '/')
    if (anUriPath.startsWith("/")) anUriPath = anUriPath.substring(1)
    if (parts.size == 2) aQuery = parts(1) else aQuery = ""
    val extParts = anUriPath.split(";", 2)
    anUriPath = extParts(0) // like index.html;sessionid=12312839183
    if (extParts.size == 2) anUriExt = Some(extParts(1).trim)
  }

  private def fillInPairs : Unit = {
    val tail = strings.drop(1)
    for(s <- tail) {
      val idx = s.indexOf(":")
      if (idx > 0) (s.trim.toLowerCase, "")
        pairs(s.substring(0, idx).trim.toLowerCase) = s.substring(idx + 1).trim
    }
  }
  
  private def fillInContentLength : Option[Long] = { 
    try { aContentLength = Some(pairs("content-length").toLong) }
    catch { case _ => aContentLength = None }
    aContentLength
  }
 
  //--------------- implementing HReqHeaderData API --------------
  
  def reqType = aReqType
  def host    = aHost
  def port    = aPort
  def uriPath = anUriPath
  def uriExt  = anUriExt
  def query   = aQuery
  
  def header(key : String): Option[String]      = pairs.get(key.toLowerCase)
  def headerKeys : scala.collection.Set[String] = pairs.keySet
  
  def contentLength = aContentLength
  def isPersistent  = aPersistence
  def boundary      = aBoundary
  
}

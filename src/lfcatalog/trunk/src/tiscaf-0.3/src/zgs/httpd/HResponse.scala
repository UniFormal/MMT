package zgs.httpd


object HResponse {
  private val stdDateFormat = new java.text.SimpleDateFormat("EEE, dd MMM yyyy HH:mm:ss")
  private val offset = java.util.TimeZone.getDefault.getRawOffset

  def stdDateString(mills : Long) = stdDateFormat.synchronized { 
    stdDateFormat.format(new java.util.Date(mills - offset)) + " GMT"
  }

  // 1 second caching ('format' is very costly!)
  private var lastMillis : Long  = 0L
  private var lastStdDate = ""
  
  private def headerDateString = stdDateFormat.synchronized {
    val now = System.currentTimeMillis
    if (now > lastMillis + 1000) {
      lastMillis = now
      lastStdDate = stdDateFormat.format(new java.util.Date(now - offset)) + " GMT"
    }
    lastStdDate
  } 

  private val defaultStatus = HStatus.asString(HStatus.OK, "listen to Jazz")
}


// deals with header only
final private class HResponse {
  
  private var status : String = HResponse.defaultStatus
  private val map = new scala.collection.mutable.HashMap[String, String]
  
  setHeader("Content-Type", "text/html") // default if user forget
  setHeader("Server", "tiscaf httpd") // default
  
  //---------------
    
  def setStatus(code : HStatus.Value) : Unit               = { status = HStatus.asString(code) }
  def setStatus(code : HStatus.Value, msg : String) : Unit = { status = HStatus.asString(code, msg) }
  
  def setHeader(key : String, value : String)     = { map(key.trim.toLowerCase) = value }
  def getHeader(key : String) : Option[String]    = map.get(key.trim.toLowerCase)
  def removeHeader(key : String) : Option[String] = map.remove(key.trim.toLowerCase)
  
  def  getBytes = {
    val buf = new StringBuilder
    buf.append("HTTP/1.1 ").append(status).append("\r\n")
    
    setHeader("date", HResponse.headerDateString)

    // write known standard fields in given order...
    for { 
      f <- HFields.list
      v <- map.remove(f)
    } buf.append(HFields.toCap(f)).append(": ").append(v).append("\r\n")

    // ... and append all others in alphabetical order
    map.keysIterator
       .toSeq.sortWith(_<_)
       .foreach { f => buf.append(HFields.toCap(f)).append(": ").append(map(f)).append("\r\n") }

    buf.append("\r\n").toString.getBytes("ISO-8859-1")
  }
}


package zgs.httpd


private abstract class HOut(aliveReq : Boolean,
                            resp : HResponse, 
                            chunked : Boolean, 
                            stream : HStream, 
                            session : Option[HSess]) {
  
  //-------- to implement ------------
  
  def write(ar : Array[Byte], offset : Int, length : Int) : Unit
  def close : Unit
  def isHeaded : Boolean
  def isClosed : Boolean

  //----------- common ---------------

  protected def closeStream : Unit = if (reallyAlive) stream.switchToRead else stream.close
  
  protected def restamp : Unit            = if (session.isDefined) session.get.restamp
  protected def writeSessionCookie : Unit = if (session.isDefined) session.get.writeSessionCookie

  private val reallyAlive = aliveReq && { resp.getHeader("Connection") match {
    case Some(x) => x.toLowerCase.indexOf("close") < 0
    case None    => true
  }}

}


final private class HIdentOut(aliveReq : Boolean,
                              resp : HResponse, 
                              chunked : Boolean, 
                              stream : HStream, 
                              session : Option[HSess]) 
  extends HOut(aliveReq, resp, chunked, stream, session) {
    
  private var headed = false // single thread
  private var closed = false // single thread

  def isHeaded : Boolean = headed
  def isClosed : Boolean = closed
  
  def write(ar : Array[Byte], offset : Int, length : Int) : Unit = {
    
    writeHeader 

    if (chunked) {
      stream.write((length.toHexString.toLowerCase + "\r\n").getBytes("ISO-8859-1"))
      stream.write(ar, offset, length)
      stream.write("\r\n".getBytes("ISO-8859-1"))
    }
    else stream.write(ar, offset, length)   
  }
  
  private def writeHeader : Unit = if (!headed) {
    
    if (!aliveReq) resp.setHeader("Connection", "Close") /* else - done in HConn */
    
    if (chunked) {
      resp.removeHeader("Content-Length")
      resp.setHeader("Transfer-Encoding", "chunked")
    }
    else if (resp.getHeader("content-length").isEmpty) {
      stream.dispose
      sys.error("Content-Length is not set")
    }
    
    writeSessionCookie
    headed = true
    stream.write(resp.getBytes)
  }
  
  def close : Unit = if (!closed) {
    closed = true
    writeHeader // for case with empty content
    restamp
    if (chunked) stream.write("0\r\n\r\n".getBytes("ISO-8859-1"))
    closeStream
  }
  
}
  

private class HBufferedOut(aliveReq : Boolean,
                           resp : HResponse, 
                           chunked : Boolean, 
                           stream : HStream, 
                           session : Option[HSess]) 
  extends HOut(aliveReq, resp, chunked, stream, session) {

  final protected object bos extends java.io.ByteArrayOutputStream {
    def getBuf : Array[Byte] = buf
    def getLength : Int = count
  }

  private var closed = false // single thread
    
  final def isHeaded : Boolean = false
  final def isClosed : Boolean = closed
    
  def write(ar : Array[Byte], offset : Int, length : Int) : Unit = bos.write(ar, offset, length)
  
  final def close : Unit = if (!closed) {
    closed = true
    closeMe
    
    // 1. set and write header
    if (!aliveReq) resp.setHeader("Connection", "Close") // else - done in HConn
    setContentEncoding
    
    if (chunked) {
      resp.removeHeader("Content-Length")
      resp.setHeader("Transfer-Encoding", "chunked")
    }
    else resp.setHeader("Content-Length", bos.getLength.toString)

    writeSessionCookie
    stream.write(resp.getBytes)
    
    // 2. write content and real close
    if (chunked) {
      stream.write((bos.getLength.toHexString.toLowerCase + "\r\n").getBytes("ISO-8859-1"))
      stream.write(bos.getBuf, 0, bos.getLength)
      stream.write("\r\n".getBytes("ISO-8859-1"))
      stream.write("0\r\n\r\n".getBytes("ISO-8859-1"))
    }
    else stream.write(bos.getBuf, 0, bos.getLength)   
    
    restamp
    closeStream
  }
  
  protected def setContentEncoding : Unit = resp.removeHeader("Content-Encoding") // gzip is supported only
  protected def closeMe = bos.close
}
  
  
final private class HGzipOut(aliveReq : Boolean,
                             resp : HResponse, 
                             chunked : Boolean, 
                             stream : HStream, 
                             session : Option[HSess]) 
  extends HBufferedOut(aliveReq, resp, chunked, stream, session) {

  private val zos = new java.util.zip.GZIPOutputStream(bos)
  override def write(ar : Array[Byte], offset : Int, length : Int) : Unit = zos.write(ar, offset, length)
  override protected def setContentEncoding = resp.setHeader("Content-Encoding", "gzip")
  override protected def closeMe = zos.close
}


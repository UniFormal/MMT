/*******************************************************************************
 * This file is part of tiscaf.
 * 
 * tiscaf is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * Foobar is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with tiscaf.  If not, see <http://www.gnu.org/licenses/>.
 ******************************************************************************/
package tiscaf

private abstract class HOut(data: HTalkData, resp:  HResponse, session: Option[HSess]) {
  
  //-------- to implement ------------
  
  def write(ar: Array[Byte], offset: Int, length: Int): Unit
  def close: PeerWant.Value
  def isHeaded: Boolean

  //----------- common ---------------

  protected def closeStream: PeerWant.Value = if (reallyAlive) PeerWant.Read else PeerWant.Close
  
  protected def restamp: Unit            = if (session.isDefined) session.get.restamp
  protected def writeSessionCookie: Unit = if (session.isDefined) session.get.writeCookie

  // client can force connection closing via header
  private val reallyAlive = data.aliveReq && { resp.getHeader("Connection") match {
    case Some(x) => x.toLowerCase.indexOf("close") < 0
    case None    => true
  }}

}


final private class HIdentOut(data: HTalkData, resp:  HResponse, session: Option[HSess]) 
  extends HOut(data, resp, session) {
    
  private var headed = false

  def isHeaded: Boolean = headed
  
  def write(ar: Array[Byte], offset: Int, length: Int): Unit = {
    
    if (data.app.chunked) {
      data.writer.writeSeq(List(
        headerBytes,
        new HOutArray((length.toHexString.toLowerCase + "\r\n").getBytes("ISO-8859-1")),
        new HOutArray(ar, offset, length),  
        new HOutArray("\r\n".getBytes("ISO-8859-1"))   
      ))
    }
    else data.writer.writeSeq(List(
      headerBytes,
      new HOutArray(ar, offset, length)   
    ))
  }
  
  private def headerBytes: HOutArray = if (headed) new HOutArray(new Array[Byte](0)) else {
    
    if (!data.aliveReq) resp.setHeader("Connection", "Close") /* else - done in HConn */
    
    if (data.app.chunked) {
      resp.removeHeader("Content-Length")
      resp.setHeader("Transfer-Encoding", "chunked")
    }
    else if (resp.getHeader("content-length").isEmpty) {
      data.writer.dispose
      sys.error("Content-Length is not set")
    }
    
    writeSessionCookie
    headed = true
    new HOutArray(resp.getBytes)
  }
  
  def close: PeerWant.Value = {
    data.writer.write(headerBytes)
    restamp
    if (data.app.chunked) data.writer.write("0\r\n\r\n".getBytes("ISO-8859-1"))
    closeStream
  }
  
}
  

private class HBufferedOut(data: HTalkData, resp:  HResponse, session: Option[HSess]) 
  extends HOut(data, resp, session) {

  final object bos extends java.io.ByteArrayOutputStream {
    def getBuf: Array[Byte] = buf
    def getLength: Int = count
  }

  final def isHeaded: Boolean = false
    
  def write(ar: Array[Byte], offset: Int, length: Int): Unit = bos.write(ar, offset, length)
  
  final def close: PeerWant.Value = {
    flushMe
    
    // 1. set and write header
    if (!data.aliveReq) resp.setHeader("Connection", "Close") // else - done in HAcceptor
    setContentEncoding
    
    if (data.app.chunked) {
      resp.removeHeader("Content-Length")
      resp.setHeader("Transfer-Encoding", "chunked")
    }
    else resp.setHeader("Content-Length", bos.getLength.toString)

    writeSessionCookie
    
    // 2. write content and real close
    if (data.app.chunked) data.writer.writeSeq(List(
      new HOutArray(resp.getBytes),
      new HOutArray((bos.getLength.toHexString.toLowerCase + "\r\n").getBytes("ISO-8859-1")),
      new HOutArray(bos.getBuf),
      new HOutArray("\r\n".getBytes("ISO-8859-1")),
      new HOutArray("0\r\n\r\n".getBytes("ISO-8859-1"))
    ))
    else data.writer.writeSeq(List(
      new HOutArray(resp.getBytes),
      new HOutArray(bos.getBuf, 0, bos.getLength)
    ))
  
    restamp
    closeStream
  }
  
  protected def setContentEncoding: Unit = resp.removeHeader("Content-Encoding") // gzip is supported only
  protected def flushMe = bos.close
}
  
  
final private class HGzipOut(data: HTalkData, resp:  HResponse, session: Option[HSess]) 
  extends HBufferedOut(data, resp, session) {

  private val zos = new java.util.zip.GZIPOutputStream(bos)
  override def write(ar: Array[Byte], offset: Int, length: Int): Unit = zos.write(ar, offset, length)
  override protected def setContentEncoding = resp.setHeader("Content-Encoding", "gzip")
  override protected def flushMe = zos.close
}

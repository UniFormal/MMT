package zgs.httpd

import java.nio.ByteBuffer
import java.nio.channels. { Selector, SelectionKey, SocketChannel }


private object PeerWant extends Enumeration { val Close, Read, Write = Value }


private trait HPeer {
  
  //------------------- to implement ------------------------------
  
  def submit : Unit // on new data read puts HConn.process to peers queue
  
  def writeBufSize : Int
  def readBufSize  : Int
  
  def queueWakeup(want : PeerWant.Value) : Unit

  def onError(e : Throwable) : Unit
  
  def connectionTimeout : Long
  
  def channel : SocketChannel
  
  //-------------------------------------------------------------------
  
  final val inQ  = new zgs.sync.SyncField[ByteBuffer]
  final val outQ = new zgs.sync.SyncField[(ByteBuffer, PeerWant.Value)]
  
  var stamp = System.currentTimeMillis // the only (selector's) thread read/write
  
  final def dispose = { 
    queueWakeup(PeerWant.Close)
    inQ.close
    outQ.close
  }
  
  //---------------------------- reading --------------------------------------
  
  final def read = try {
    val buf = ByteBuffer.allocate(readBufSize)
    if (channel.read(buf) == -1) dispose // eof!
    else { submit; buf.flip; inQ.put(buf) }
  } catch { case e => dispose; onError(e) }

  //---------------------------- writing --------------------------------------
  
  final def write = try {
    val bufWant = outQ.take
    val buf = bufWant._1
    if (buf.remaining != 0) {
      channel.write(buf)
      if (buf.hasRemaining && !writeAside(buf, channel)) dispose
    }
    queueWakeup(bufWant._2) // HStream always wants some next operation after writing
  } catch { case e => dispose; onError(e) }

  // for me it happens with big (say, >= 64KB) buffers only
  private def writeAside(buf : ByteBuffer, sc : SocketChannel) : Boolean = {
    // println("aside")
    val tmpSelector = Selector.open
    val theKey = sc.register(tmpSelector, SelectionKey.OP_WRITE)
    theKey.attach(System.currentTimeMillis)

    @scala.annotation.tailrec
    def nextSelect : Boolean = if (buf.hasRemaining) {
      if (theKey.attachment.asInstanceOf[Long] + connectionTimeout < System.currentTimeMillis)  {
        tmpSelector.close
        false
      }
      else {
        tmpSelector.select(200)
        val keys = tmpSelector.selectedKeys
        val it = keys.iterator
        if (it.hasNext) {
          val key = it.next
          it.remove
          sc.write(buf)
          key.attach(System.currentTimeMillis)
        }
        nextSelect
      }
    }
    else { tmpSelector.close; true }
    
    nextSelect
  }
  
  //----------
  
  final def remoteIp = channel.socket.getInetAddress.getHostAddress
  
}

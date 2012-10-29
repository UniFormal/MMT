package zgs.httpd

import java.nio.ByteBuffer


private object HStream {
  val emptyBuf = ByteBuffer.allocate(0) // for queueing != Peer.WantWrite
}


final private class HStream(peer : HPeer) {
  
  def dispose                            = peer.dispose
  def remoteIp                           = peer.remoteIp
  def queueWakeup(want : PeerWant.Value) = peer.queueWakeup(want)

  def switchToRead : Unit = if (!closed) {
    toQueue(HStream.emptyBuf, PeerWant.Read) // for Keep-Alive
    onWrite = false     // it means on next write to socket we must wake up
                        // a key to write, i.e. to read from outQ
  }

  //-------------------------- intput --------------------------------
  
  def readQueue : Array[Byte] = {
    val buf = peer.inQ.take
    val ar = new Array[Byte](buf.remaining)
    buf.get(ar)
    ar
  }
  
  //------------------------ output ----------------------------------
  
  def write(ar : Array[Byte]) : Unit = write(ar, 0, ar.length)

  // *every* 'write' is adding to outQ one by one
  @scala.annotation.tailrec
  def write(ar : Array[Byte], offset : Int, length : Int) : Unit = if (length > 0) {
    if (closed)                      throwError("stream peer is already closed")
    if (length < 0 || offset < 0)    throwError("invalid arguments: length < 0 || offset < 0")
    if (length + offset > ar.length) throwError("invalid bounds: length + offset > ar.length")

    val writeLength = math.min(peer.writeBufSize, length)
    toQueue(ByteBuffer.allocate(writeLength).put(ar, offset, writeLength), PeerWant.Write)
    write(ar, offset + writeLength, length - writeLength)
  }

  def close : Unit = if (!closed) {
    closed = true
    toQueue(HStream.emptyBuf, PeerWant.Close)
  }

  // internals ------------------------------------------------

  private var closed = false
  private var onWrite = false
    
  // throws exception to take to HLet.act a chance to terminate
  // on network error: on HPeer dispose outQ is closed and
  // HTalk.anyOutMethod will throw such exception 
  private def toQueue(buf : ByteBuffer, want : PeerWant.Value) : Unit = {
    buf.flip
    peer.outQ.put(buf, want) // blocks
    if (!onWrite) {
      onWrite = true // new write sequence
      peer.queueWakeup(PeerWant.Write) // to fire peer to read from outQ
    }
  }
  
  private def throwError(msg : String) = { dispose; sys.error(msg) }
}

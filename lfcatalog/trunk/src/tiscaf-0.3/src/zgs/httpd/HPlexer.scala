package zgs.httpd

import java.net.InetSocketAddress
import java.nio.channels. { SelectionKey, Selector, ServerSocketChannel }

import zgs.sync._


private trait HPlexer {
  
  //---------------------- to implement ------------------------------

  def selectorPoolSize : Int
  def selectorQueueSize : Int
  def connectionTimeout : Long
  def tcpNoDelay : Boolean
  def onError(e : Throwable) : Unit
  
  //---------------------- SPI ------------------------------------------
  
  final def start = synchronized {
    if (!doWork.get ) {
      doWork.set(true)
      Sync.spawn { try { plex } catch { case e => onError(e) } }
    }
  }
  
  final def stop : Unit = synchronized { // close once only 
    if (doWork.get) {
      doWork.set(false)
      selector.close
      servers.foreach(_.asInstanceOf[ServerSocketChannel].close) 
      servers.clear
      workers.shutdownNow
    }
  }
  
  final def addServer(peerFactory : SelectionKey => HPeer, port : Int) : Unit = Sync.spawn {
    try { 
      val ssc = ServerSocketChannel.open
      servers += ssc
      ssc.configureBlocking(true)
      ssc.socket.bind(new InetSocketAddress(port))

      while (doWork.get) try {
        val sc = ssc.accept
        sc.socket.setTcpNoDelay(tcpNoDelay)
        sc.configureBlocking(false)
        val key = keySetGuard.synchronized {
          selector.wakeup
          sc.register(selector, 0)
        }
        key.attach(peerFactory(key))
        toWake(key, PeerWant.Read)
      } catch { 
        case e : java.nio.channels.AsynchronousCloseException =>
        case e => onError(e) 
      }
    } 
    catch {
      case e : java.nio.channels.AsynchronousCloseException =>
      case e => throw e
    }
  }
  
  final def toWake(key : SelectionKey, want : PeerWant.Value) : Unit = wakeService.consume(key, want)
  
  //------------------------- internals --------------------------------------

  private val workers : SyncExe = new zgs.sync.SyncExe(selectorPoolSize, selectorQueueSize, "HPlexer")
  
  private val doWork = new SyncBool(false)
  private val servers = new scala.collection.mutable.HashSet[ServerSocketChannel]
               with scala.collection.mutable.SynchronizedSet[ServerSocketChannel]
  private val wakes = new SynQu[(SelectionKey, PeerWant.Value)]
  private val selector = Selector.open
  
  //-- in accordance with Ron Hitchens (Ron, thanks for the trick!)
    
  private val keySetGuard = new AnyRef

  //---
  
  private def toWorker(code : => Unit) = workers.submit(new Runnable { def run { code } })

  //--

  private val wakeService = new zgs.sync.SyncService {
    override def name = "Plexer-wakeService"
    override protected def doConsume(what : Any) : Unit = keySetGuard.synchronized { try {
      selector.wakeup
      val keyWant = what.asInstanceOf[(SelectionKey, PeerWant.Value)]
      val key = keyWant._1
      keyWant._2 match {
        case PeerWant.Write => if (key.isValid) key.interestOps(SelectionKey.OP_WRITE)
        case PeerWant.Close => key.channel.close
        case PeerWant.Read  => if (key.isValid) key.interestOps(SelectionKey.OP_READ)
      }
    } catch { case e => onError(e) }} 
  }
    
  //--------- check expired connections - not too often
  
  private val expireDelta = math.max(connectionTimeout/10, 1000)
  private val lastExpire = new zgs.sync.SyncField[Long]
  lastExpire.set(System.currentTimeMillis)

  //-------- main multiplexer loop

  private def plex : Unit =  while (doWork.get) try {
    
    val now = System.currentTimeMillis
    if (now > lastExpire.get + expireDelta) {
      val timeX = now - connectionTimeout
      keySetGuard.synchronized {
        val it = selector.keys.iterator
        while (it.hasNext) {
          val key = it.next
          val att = key.attachment
          if (att != null && att.asInstanceOf[HPeer].stamp < timeX) key.channel.close
        }
        lastExpire.set(now)
      }
    } else keySetGuard.synchronized {}

    // timeout is used to close expired connections
    if (selector.select(expireDelta) != 0) {
      val it = selector.selectedKeys.iterator
      val now = System.currentTimeMillis
      while (it.hasNext) {
        val key = it.next
        it.remove
        if (key.isValid) {
          val peer = key.attachment.asInstanceOf[HPeer]
          peer.stamp = now
          if (key.isWritable)      { key.interestOps(0); toWorker(peer.write) }
          else if (key.isReadable) { key.interestOps(0); toWorker(peer.read) }
        }
      }
    }

  } catch { case e => onError(e) }  // plex (never saw this error)
  
}


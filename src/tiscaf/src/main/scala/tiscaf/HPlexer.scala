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

import java.net.InetSocketAddress
import java.nio.channels.{
  SelectionKey,
  Selector,
  ServerSocketChannel,
  SocketChannel
}
import java.nio.ByteBuffer
import javax.net.ssl._

import sync._
import scala.concurrent.SyncVar

import scala.collection.JavaConverters._

private trait HPlexer extends HLoggable {

  //---------------------- to implement ------------------------------

  def timeoutMillis: Long
  def tcpNoDelay: Boolean
  def ssl: List[HSslContext]

  //---------------------- SPI ------------------------------------------

  final def start = synchronized {
    if (!isWorking.get) {
      isWorking.set(true)
      Sync.spawnNamed("Plexer") {
        try {
          plex
        } catch {
          case e: Exception => error("An error occurred when starting the plexer", e)
        }
      }
    }
  }

  final def stop: Unit = synchronized { // close once only
    if (isWorking.get) {
      isWorking.set(false)
      selector.close
      for((socket, _) <- servers)
        socket.asInstanceOf[ServerSocketChannel].close
      servers.clear
    }
  }

  final def addListener(peerFactory: (SelectionKey, Option[SSLEngine]) => HPeer, bindHost: String, port: Int): Unit = Sync.spawnNamed("Acceptor-" + port) {
    try {
      val serverChannel = ServerSocketChannel.open
      servers(serverChannel) = ()
      serverChannel.configureBlocking(true)
      serverChannel.socket.bind(new InetSocketAddress(bindHost, port))

      while (isWorking.get) try {
        val socketCannel = serverChannel.accept
        socketCannel.socket.setTcpNoDelay(tcpNoDelay)
        socketCannel.configureBlocking(false)
        keySetGuard.synchronized {
          selector.wakeup
          val key = socketCannel.register(selector, 0)
          key.attach(new HKeyData(peerFactory(key, None)))
          needToRead(key)
        }
      } catch {
        case e: java.nio.channels.AsynchronousCloseException =>
        case e: Exception => error("Something wrong happened with acceptor on port " + port, e)
      }
    } catch {
      case e: java.nio.channels.AsynchronousCloseException =>
      case e: Exception => throw e
    }
  }

  final def addSslListener(peerFactory: (SelectionKey, Option[SSLEngine]) => HPeer, bindHost: String, sslData: HSslContext): Unit = Sync.spawnNamed("Acceptor-" + sslData.port) {
    try {
      val serverChannel = ServerSocketChannel.open
      servers(serverChannel) = ()
      serverChannel.configureBlocking(true)
      serverChannel.socket.bind(new InetSocketAddress(bindHost, sslData.port))

      while (isWorking.get) try {
        val socketChannel = serverChannel.accept
        val engine = {
          val host = socketChannel.socket.getInetAddress.getHostAddress
          val port = socketChannel.socket.getPort
          sslData.engine(host, port)
        }
        socketChannel.socket.setTcpNoDelay(tcpNoDelay)
        socketChannel.configureBlocking(false)
        // perform handshake
        HSsl.handshake(socketChannel, engine)
        // and continue
        keySetGuard.synchronized {
          selector.wakeup
          val key = socketChannel.register(selector, 0)
          key.attach(new HKeyData(peerFactory(key, Some(engine))))
          needToRead(key)
        }
      } catch {
        case e: java.nio.channels.AsynchronousCloseException =>
        case e: Exception =>
          error("Something wrong happened with acceptor on ssl port " + sslData.port, e)
      }
    } catch {
      case e: java.nio.channels.AsynchronousCloseException =>
      case e: Exception => throw e
    }
  }

  final def needToClose(key: SelectionKey): Unit = { wakeQu.put(key, PeerWant.Close); selector.wakeup }
  final def needToWrite(key: SelectionKey): Unit = { wakeQu.put(key, PeerWant.Write); selector.wakeup }
  final def needToRead(key: SelectionKey): Unit = { wakeQu.put(key, PeerWant.Read); selector.wakeup }

  //------------------------- internals --------------------------------------

  private val isWorking = new SyncBool(false)
  private val servers = new java.util.concurrent.ConcurrentHashMap[ServerSocketChannel, Unit].asScala
  private val selector = Selector.open
  //-- in accordance with Ron Hitchens (Ron, thanks for the trick!)
  private val keySetGuard = new AnyRef

  //-----------------------------------------------------------

  type KeyWant = (SelectionKey, PeerWant.Value)
  private val wakeQu = new SyncQu[KeyWant]

  private def processWakeQu: Unit = {
    @scala.annotation.tailrec
    def step: Unit = wakeQu.takeOpt match {
      case Some(kw) =>
        kw._2 match {
          case PeerWant.Write => if (kw._1.isValid) kw._1.interestOps(SelectionKey.OP_WRITE)
          case PeerWant.Close => keySetGuard.synchronized { selector.wakeup; kw._1.channel.close } // changes keySet
          case PeerWant.Read  => if (kw._1.isValid) kw._1.interestOps(SelectionKey.OP_READ)
        }
        step
      case _ =>
    }
    step
  }

  // check expired connections - not too often
  private val expireDelta = if (timeoutMillis > 10000L) 1000L else timeoutMillis / 10L
  private val lastExpire = new SyncVar[Long]
  lastExpire.put(System.currentTimeMillis)

  private def processExpiration: Unit = try {
    val now = System.currentTimeMillis
    if (now > lastExpire.get + expireDelta) {
      val timeX = now - timeoutMillis
      val it = selector.keys.iterator
      while (it.hasNext) {
        val key = it.next
        val att = key.attachment
        if (att == null || att.asInstanceOf[HKeyData].stamp < timeX) key.channel.close
      }
      // discard the old value
      lastExpire.take
      // and put the new one
      lastExpire.put(now)
    }
  } catch {
    case e: Exception => error("A problem occured while closing an expired connection", e)
  }

  // main multiplexer loop
  private def plex: Unit = while (isWorking.get) try {

    processWakeQu // these operations change keys' states only
    keySetGuard.synchronized { processExpiration } // these operation change keySet content

    if (selector.select > 0) {
      val it = selector.selectedKeys.iterator
      val now = System.currentTimeMillis
      while (it.hasNext) {
        val key = it.next
        it.remove
        if (key.isValid) {
          val data = key.attachment.asInstanceOf[HKeyData]
          data.stamp = now
          if (key.isWritable) try { key.interestOps(0); data.peer.proceedToWrite } catch { case _: Exception => /* can be canceled */ }
          else if (key.isReadable) try { key.interestOps(0); data.peer.readChannel } catch { case _: Exception => /* can be canceled */ }
        }
      }
    }

  } catch {
    case e: Exception => error("Something wrong happened in the main loop", e)
  }

}

private class HKeyData(val peer: HPeer) {
  var stamp = System.currentTimeMillis
}

private object PeerWant extends Enumeration {
  val Close = Value("Close")
  val Read = Value("Read")
  val Write = Value("Write")
}


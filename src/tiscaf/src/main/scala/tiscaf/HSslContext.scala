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

import javax.net.ssl._
import java.security._
import java.nio.channels._
import java.nio._

import scala.collection.{ mutable => mute }

/** tiscaf SSL context. It manages the session cache as well as SSL settings. */
trait HSslContext {

  //---------------------- to implement ------------------------------

  /** SSL ports. */
  def port: Int

  /** Keystore passphrase. */
  def passphrase: String

  /** Keystore containing server certificate and CA certificate(s). */
  def keystore: KeyStore

  //---------------------- to override ------------------------------

  /** Require client authentication. By default `false`. */
  def clientAuth: HClientAuth.Value = HClientAuth.None // client authentication

  /** Trusted client certificates and CA certificates. By default `None`. */
  def truststore: Option[KeyStore] = None // trusted client certificates and CA certificates

  /** Trusted client certificate depth according to CA certificate(s). By default `1`. */
  def trustDepth = 1 // trust depth for client certificate according to CA certificates in the truststore

  /** The protocol. By default `SSL` */
  def protocol = "SSL"

  /** Specific JCE provider name if one wants to use it.
   *  By default `None` which means that the default provider is used.
   */
  def provider: Option[String] = None

  /** SSL session timeout in minutes. By default `5`. */
  def sslSessionTimeoutMin: Int = 5

  //---------------------- internals ------------------------------

  private val keyManagers = {
    val factory = provider match {
      case Some(p) =>
        KeyManagerFactory.getInstance(KeyManagerFactory.getDefaultAlgorithm, p)
      case None =>
        KeyManagerFactory.getInstance(KeyManagerFactory.getDefaultAlgorithm)
    }
    factory.init(keystore, passphrase.toCharArray)
    factory.getKeyManagers
  }

  private val trustManagers =
    truststore match {
      case Some(ts) =>
        val factory = provider match {
          case Some(p) =>
            TrustManagerFactory.getInstance(TrustManagerFactory.getDefaultAlgorithm, p)
          case None =>
            TrustManagerFactory.getInstance(TrustManagerFactory.getDefaultAlgorithm)
        }
        factory.init(ts)
        factory.getTrustManagers
      case None => null
    }

  private[tiscaf] val sslContext = {
    val context = provider match {
      case Some(p) => SSLContext.getInstance(protocol, p)
      case None    => SSLContext.getInstance(protocol)
    }
    context.init(keyManagers, trustManagers, new SecureRandom)
    context
  }

  private[tiscaf] def engine(host: String, port: Int) = {
    val engine = sslContext.createSSLEngine(host, port)
    // we are on the server side
    engine.setUseClientMode(false)
    // configure client authentication
    clientAuth match {
      case HClientAuth.Accepted => engine.setWantClientAuth(true)
      case HClientAuth.Required => engine.setNeedClientAuth(true)
      case HClientAuth.None     => // ...
    }

    engine.setEnabledProtocols(Array("SSLv3", "TLSv1"))

    engine
  }

}

private[tiscaf] object HSsl {

  // do the SSL stuffs

  /* Performs the handshake and returns */
  def handshake(channel: SocketChannel, engine: SSLEngine) {

    // get the ssl session
    val session = engine.getSession
    // determine buffer sizes from session
    // Create byte buffers to use for holding application data
    val myAppData = ByteBuffer.allocate(session.getApplicationBufferSize)
    val myNetData = ByteBuffer.allocate(session.getPacketBufferSize)
    val peerAppData = ByteBuffer.allocate(session.getApplicationBufferSize)
    val peerNetData = ByteBuffer.allocate(session.getPacketBufferSize)

    // Begin handshake
    engine.beginHandshake
    var hs = engine.getHandshakeStatus

    // Process handshaking message
    import SSLEngineResult.HandshakeStatus._
    while (hs != FINISHED && hs != NOT_HANDSHAKING) {
      hs match {
        case NEED_UNWRAP =>
          // Receive handshaking data from peer
          if (channel.read(peerNetData) >= 0) {
            // Process incoming handshaking data
            peerNetData.flip
            val res = engine.unwrap(peerNetData, peerAppData)
            peerNetData.compact
            // set the new status
            hs = res.getHandshakeStatus

            // Check status
            res.getStatus match {
              case SSLEngineResult.Status.BUFFER_OVERFLOW =>
                // clear the buffer
                peerAppData.clear
              case _ => // on OK, BUFFER_UNDERFLOW or CLOSED, just continue
            }
          }

        case NEED_WRAP =>
          // Empty the local network packet buffer.
          myNetData.clear

          // Generate handshaking data
          val res = engine.wrap(myAppData, myNetData)
          hs = res.getHandshakeStatus

          // Check status
          res.getStatus match {
            case SSLEngineResult.Status.OK =>
              myNetData.flip

              // Send the handshaking data to peer
              while (myNetData.hasRemaining) {
                if (channel.write(myNetData) < 0) {
                  // TODO Handle closed channel
                }
              }

            case SSLEngineResult.Status.BUFFER_OVERFLOW =>
              // compact net buffer
              myNetData.compact

            case _ =>
              throw new Exception("Is it possible that this happens?")
          }

        case NEED_TASK =>
          // blocking tasks in another thread
          var runnable = engine.getDelegatedTask
          while (runnable != null) {
            runnable.run
            runnable = engine.getDelegatedTask
          }
          hs = engine.getHandshakeStatus

        case _ =>
          throw new Exception("Is it possible that this happens?")
      }
    }

  }

}

/** Indicates whether client authentication is accepted, required,
 *  or if none is needed.
 */
object HClientAuth extends Enumeration {
  val None, Accepted, Required = Value
}

package info.kwarc.mmt.python

import info.kwarc.mmt.api._
import frontend._

import py4j._
import GatewayServer._

import scala.collection.convert.ImplicitConversionsToJava._

/**
 * allows controlling MMT from Python
 * 
 * this only starts a Py4J server that handles the communication with python
 * See the file mmt.py for the Python counterpart and more documentation and examples.
 */
class Py4JGateway extends Extension {
  private var gatewayServer: GatewayServer = null
  
  override def start(args: List[String]): Unit = {
    val portS = args.headOption.getOrElse("25333")
    val port = utils.stringToInt(portS).getOrElse {
      throw LocalError("expected numeric argument")
    }
    // twiesing: this line depends on scala.collection.convert.ImplicitConversionsToJava
    // so can't remove the deprecation for now
    gatewayServer = new GatewayServer(controller, port, port+1, DEFAULT_CONNECT_TIMEOUT, DEFAULT_READ_TIMEOUT, Nil)
    gatewayServer.start()
  }
  
  override def destroy: Unit = {
    gatewayServer.shutdown()
  }
}

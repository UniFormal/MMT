package info.kwarc.mmt.python

import info.kwarc.mmt.api._
import frontend._

import py4j._
import GatewayServer._

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
    gatewayServer = new GatewayServer(controller, port, port+1, DEFAULT_CONNECT_TIMEOUT, DEFAULT_READ_TIMEOUT, null)
    gatewayServer.start()
  }
  
  override def destroy: Unit = {
    gatewayServer.shutdown()
  }
}

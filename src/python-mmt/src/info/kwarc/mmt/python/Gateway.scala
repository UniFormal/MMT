package info.kwarc.mmt.python

import info.kwarc.mmt.api.frontend._

import py4j._

/**
 * allows controlling MMT from Python
 * 
 * this only starts a Py4J server that handles the communication with python
 * See the file mmt.py for the Python counterpart and more documentation and examples.
 */
class Py4JGateway extends Extension {
  private var gatewayServer: GatewayServer = null
  
  override def start(args: List[String]) {
    gatewayServer = new GatewayServer(controller, args.headOption.map(_.toInt).getOrElse(25333))
    gatewayServer.start()
  }
  
  override def destroy {
    gatewayServer.shutdown()
  }
}

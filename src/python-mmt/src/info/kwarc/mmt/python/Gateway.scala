package info.kwarc.mmt.python

import info.kwarc.mmt.api.frontend._

import py4j._

class Py4JGateway extends Extension {
  private var gatewayServer: GatewayServer = null
  
  override def start(args: List[String]) {
    gatewayServer = new GatewayServer(controller)
    gatewayServer.start()  
  }
  
  override def destroy {
    gatewayServer.shutdown()
  }
}

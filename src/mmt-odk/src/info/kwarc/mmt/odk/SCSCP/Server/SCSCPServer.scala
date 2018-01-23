package info.kwarc.mmt.odk.SCSCP.Server

import java.io.InterruptedIOException
import java.net.{InetAddress, ServerSocket, Socket}

import info.kwarc.mmt.api.frontend.Extension
import info.kwarc.mmt.odk.OpenMath.OMSymbol
import info.kwarc.mmt.odk.SCSCP.CD.scscp2
import info.kwarc.mmt.odk.SCSCP.Protocol.ProtocolError

import scala.collection.mutable
import scala.concurrent.Future

/**
  * A (single threaded) implementation of the SCSCP protocol, version 1.3
  *
  * This is not technically conform to the standard, as we do not implement all the method outlined in the
  * specification.
  *
  * @param service_name       The name of the service offered offered by this server
  * @param service_version    The version of the service offered by this server
  * @param service_identifier The identifier of the service offered by this server.
  * @param socket             Socket to use for incoming connections
  * @param encoding           Encoding to use for incoming connections
  */
class SCSCPServer(val service_name: String, val service_version: String, val service_identifier: String, socket: ServerSocket, encoding: String = "UTF-8") {
  private val handlers = mutable.Map[OMSymbol, SCSCPHandler]()
  private val client_map = mutable.Map[String, SCSCPServerClient]()

  /** a list of clients connected to this server */
  def clients : List[SCSCPServerClient] = {
    client_map.toList.map(_._2)
  }

  /**
    * Registers a handler for a specific function call
    *
    * @param symbol  Symbol to handle
    * @param handler Handler to register
    */
  def register(symbol: OMSymbol, handler: SCSCPHandler) {
    // TODO: Figure out relative paths, etc.

    if (handlers.isDefinedAt(symbol)) {
      throw new Exception("Handler already defined for symbol: " + symbol)
    }

    handlers(symbol) = handler
  }

  // register the default handlers
  // TODO: Add more
  register(scscp2(scscp2.getAllowedHeads), new GetAllowedHeads(this))
  register(scscp2(scscp2.getSignature), new  GetSignature(this))

  /**
    * Gets the handler for a given symbol
    *
    * @param symbol
    * @return
    */
  def getHandler(symbol: OMSymbol): SCSCPHandler = {

    if (!handlers.isDefinedAt(symbol)) {
      throw new Exception("No handler defined for symbol: " + symbol)
    }

    handlers(symbol)
  }

  def getHandlerNames : List[OMSymbol] = handlers.toList.map(_._1)

  /**
    * Unregisters a handler for a symbol
    *
    * @param symbol Symbol to unregister
    */
  def unregister(symbol: OMSymbol) {

    if (!handlers.isDefinedAt(symbol)) {
      throw new Exception("No handler defined for symbol: " + symbol)
    }

    handlers -= symbol
  }

  /** Processes forever, blocking */
  def processForever() {

    // process stuff forever
    while(true){
      process()
      Thread.sleep(10)
    }
  }

  /** Represents one iteration of processing. Never blocks. */
  def process() {
    addClients()
    processClients()
    cleanupClients()
  }


  /** Adds all the clients to the pool of connections */
  private def addClients() {
    // make sure to wait at most 500 ms for a new connection
    socket.setSoTimeout(500)

    // accept clients until we have a timeout
    try {
      while (true) {
        this.addClient(socket.accept())
      }
    } catch {
      case io: InterruptedIOException =>
    }

    cleanupClients()
  }

  /**
    * Adds a new client to the pool of clients
    *
    * @param socket Socket that represents the new client
    */
  private def addClient(socket: Socket) {
    try {
      val client = new SCSCPServerClient(socket, this, encoding)
      client_map(client.identifier) = client
    } catch {
      case p: ProtocolError =>
    }
  }

  /** removes all non-connected clients from this server */
  private def cleanupClients() {
    clients.foreach(c => {
      // if the client is not connected, we can remove it
      if (!c.connected) {
        c.quit()
        client_map -= c.identifier
      }
    })

  }

  /** Processes all of the clients */
  private def processClients() {
    cleanupClients()
    clients.foreach(_.process())
  }

  /** Sends an info message to all clients attached to this server */
  def info(message: String) {
    cleanupClients()
    clients.foreach(_.info(message))
  }

  /** Quits all clients attached to this server */
  def quit(reason: Option[String]) {
    cleanupClients()
    clients.foreach(_.quit(reason))
  }
}

object SCSCPServer {
  def apply(service_name: String, service_version: String, service_identifier: String, host: String = "0.0.0.0", port: Int = 26133, encoding: String = "UTF-8") =
    new SCSCPServer(service_name, service_version, service_identifier, new java.net.ServerSocket(port, 0, InetAddress.getByName(host)), encoding)
}

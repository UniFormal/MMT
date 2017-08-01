package info.kwarc.mmt.odk.SCSCP.Server

import info.kwarc.mmt.odk.OpenMath.{OMApplication, OMExpression, OMString, OMSymbol}
import info.kwarc.mmt.odk.SCSCP.CD.SymbolSet
import info.kwarc.mmt.odk.SCSCP.Client.SCSCPClient
import info.kwarc.mmt.odk.SCSCP.Protocol.{SCSCPCall, SCSCPCallArguments, SCSCPReturnObject}

/**
  * SCSCP server that implements the MitM protocol.
  */
object MitMServer {
  val server = SCSCPServer("MitMServer", "1.0", "MitMServer")

  def run(): Unit = {
    // connect the handlers for registerServer and removeServer
    server.register(OMSymbol("registerServer", "mitm_transient", None, None), new RegisterServerHandler());
    server.register(OMSymbol("removeServer", "mitm_transient", None, None), new RemoveServerHandler());

    // and serve it forever
    server.processForever()
  }
}

/**
  * Database that stores and manages the function headers received from CAS clients.
  * Currently implemented in-memory, could in the long run be moved to the file system for extensibility.
  */
object MitMDatabase {
  final val SCSCP_PORT = 26134

  var mapOfServersToFunctions : Map[String, List[OMSymbol]] = Map()
  var mapOfServerNamesToClients : Map[String, SCSCPClient] = Map()

  def addServer(serverAddress: String): String = {
    val client : SCSCPClient = SCSCPClient(serverAddress, SCSCP_PORT)
    val serverID = client.service_id

    var listOfSymbols : List[OMSymbol] = List()
    mapOfServerNamesToClients += (serverID -> client)
    for (symbol <- client.getAllowedHeads) {
      MitMServer.server.register(symbol, new RemoteCallHandler(client, symbol))
      listOfSymbols ::= symbol
    }
    mapOfServersToFunctions += (serverID -> listOfSymbols)
    serverID
  }

  def removeServer(serverID: String): String = {
    val client = mapOfServerNamesToClients.get(serverID).get
    val listOfFunctions = mapOfServersToFunctions.get(serverID)
    for (symbol : OMSymbol <- listOfFunctions.get) {
      MitMServer.server.unregister(symbol)
    }
    client.quit()
    mapOfServerNamesToClients -= serverID
    mapOfServersToFunctions -= serverID
    serverID
  }
}

/**
  * Registers the server in the MitM proxy and returns the id of the server.
  */
class RegisterServerHandler() extends SCSCPHandler {
  override val min : Int = 1
  override val max : Int = 2 // TODO: the second argument could be the port number if it isn't 26133
  override val signature = SymbolSet(Nil) // TODO:
  def handle(client: SCSCPServerClient, arguments : SCSCPCallArguments, parameters: OMExpression* ) : OMExpression = {
    OMString(MitMDatabase.addServer(parameters.toList.head.toString), None)
  }
}

class RemoveServerHandler() extends SCSCPHandler {
  override val min: Int = 1
  override val max: Int = 1
  override val signature = SymbolSet(Nil) // TODO:
  def handle(client: SCSCPServerClient, arguments: SCSCPCallArguments, parameters: OMExpression*): OMExpression =
    OMString(MitMDatabase.removeServer(parameters.toList.head.toString), None)
}

// TODO: pass the min and max no of args as args to the ctor, but there seems to be no method of retrieving them from CAS
class RemoteCallHandler(CAS: SCSCPClient, symbol: OMSymbol) extends SCSCPHandler {
  override val min: Int = 1
  override val max: Int = -1
  override val signature: OMExpression = SymbolSet(Nil) // TODO:

  override def handle(client: SCSCPServerClient, arguments: SCSCPCallArguments, parameters: OMExpression*): OMExpression =
    CAS(new SCSCPCall(symbol, SCSCPCallArguments(CAS.newCallId, Some(SCSCPReturnObject), null), parameters: _*)).fetch().get
}
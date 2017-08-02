package info.kwarc.mmt.odk.SCSCP.Server

import info.kwarc.mmt.odk.OpenMath._
import info.kwarc.mmt.odk.SCSCP.CD.{SymbolSet, scscp1, scscp2}
import info.kwarc.mmt.odk.SCSCP.Client.SCSCPClient
import info.kwarc.mmt.odk.SCSCP.Protocol.{SCSCPCall, SCSCPCallArguments, SCSCPReturnObject}

/**
  * SCSCP server that implements the MitM protocol.
  */
object MitMServer {
  val server = SCSCPServer("MitMServer", "1.0", "MitMServer")

  def main(args: Array[String]): Unit = {
    // connect the handlers for registerServer and removeServer
    server.register(OMSymbol("registerServer", "mitm_transient", None, None), new RegisterServerHandler())
    server.register(OMSymbol("removeServer", "mitm_transient", None, None), new RemoveServerHandler())
    server.register(OMSymbol("getAllServers", "mitm_transient", None, None), new GetAllServersHandler())

    // and serve it forever
    server.processForever()
  }
}

/**
  * Database that stores and manages the function headers received from CAS clients.
  * Currently implemented in-memory, could in the long run be moved to the file system for extensibility.
  */
object MitMDatabase {
  var mapOfServersToFunctions : Map[String, List[OMSymbol]] = Map()
  var mapOfServerNamesToClients : Map[String, SCSCPClient] = Map()

  def addServer(serverAddress: String, port: Int): OMString = {
    val client : SCSCPClient = SCSCPClient(serverAddress, port)
    val serverID : String = client.service_id
    val signatureReqSym = OMSymbol("get_signature", "scscp2", None, None)
    val signatureSym = OMSymbol("signature", "scscp2", None, None)
    val infinity = OMSymbol("infinity", "nums1", None, None)
    val unboundDomain = scscp2(scscp2.symbolSetAll)

    var listOfSymbols : List[OMSymbol] = List()
    mapOfServerNamesToClients += (serverID -> client)

    for (symbol <- client.getAllowedHeads) {
      val signatureRequest = OMApplication(signatureReqSym, List(symbol), None, None)
      val signature = client(signatureRequest).fetch().get

      signature match {
        case OMApplication(`signatureSym`, args, None, None) =>
          args match {
              // bound number of args + bound domain
            case List(`symbol`, OMInteger(min, None), OMInteger(max, None), sig:OMApplication) =>
              MitMServer.server.register(symbol, new RemoteCallHandler(client, min.toInt, max.toInt, sig, symbol))
              // unbound number of args + bound domain
            case List(`symbol`, OMInteger(min, None), `infinity`, sig:OMApplication) =>
              MitMServer.server.register(symbol, new RemoteCallHandler(client, min.toInt, -1, sig, symbol))
              // bound number of args + unbound domain
            case List(`symbol`, OMInteger(min, None), OMInteger(max, None), `unboundDomain`) =>
              MitMServer.server.register(symbol, new RemoteCallHandler(client, min.toInt, max.toInt, SymbolSet(Nil), symbol))
              // unbound number of args + unbound domain
            case List(`symbol`, OMInteger(min, None), `infinity`, `unboundDomain`) =>
              MitMServer.server.register(symbol, new RemoteCallHandler(client, min.toInt, -1, SymbolSet(Nil), symbol))

            case _ =>
              throw new RemoteServerException
          }
        case _ =>
          throw new RemoteServerException
      }
      listOfSymbols ::= symbol
    }

    mapOfServersToFunctions += (serverID -> listOfSymbols)
    OMString(serverID, None)
  }

  def removeServer(serverID: String): OMSymbol = {
    val client = mapOfServerNamesToClients(serverID)
    val listOfFunctions = mapOfServersToFunctions.get(serverID)
    for (symbol : OMSymbol <- listOfFunctions.get) {
      MitMServer.server.unregister(symbol)
    }
    client.quit()
    mapOfServerNamesToClients -= serverID
    mapOfServersToFunctions -= serverID
    scscp1(scscp1.procedureCompleted)
  }
}

/**
  * Registers the server in the MitM proxy and returns the id of the server.
  */
class RegisterServerHandler() extends SCSCPHandler {
  override val min : Int = 1
  override val max : Int = 2
  // i.e. the signature is address: string, port: integer
  override val signature = SymbolSet(List(OMSymbol("string", "omtypes", None, None), OMSymbol("integer", "omptypes", None, None)))

  def handle(client: SCSCPServerClient, arguments : SCSCPCallArguments, parameters: OMExpression* ) : OMExpression = {
    val listOfArgs = parameters.toList
    val address = getArgAsString(listOfArgs, 0)
    if (listOfArgs.length > 1) {
      val port = getArgAsInt(parameters.toList, 1)
      MitMDatabase.addServer(address.text, port.int.toInt)
    }
    else {
      MitMDatabase.addServer(address.text, 26133)
    }
  }
}

class RemoveServerHandler() extends SCSCPHandler {
  override val min: Int = 1
  override val max: Int = 1
  override val signature = SymbolSet(List(OMSymbol("string", "omtypes", None, None)))
  def handle(client: SCSCPServerClient, arguments: SCSCPCallArguments, parameters: OMExpression*): OMExpression = {
    val server = getArgAsString(parameters.toList, 0)
    MitMDatabase.removeServer(server.text)
  }
}

class GetAllServersHandler() extends SCSCPHandler {
  override val min: Int = 0
  override val max: Int = 0
  override val signature = SymbolSet(Nil)

  override def handle(client: SCSCPServerClient, arguments: SCSCPCallArguments, parameters: OMExpression*): OMExpression = {
    var listOfServers : List[OMString] = List()
    for (server <- MitMDatabase.mapOfServerNamesToClients.keys) {
      listOfServers ::= OMString(server, None)
    }
    SymbolSet(listOfServers)
  }
}

class RemoteCallHandler(CAS: SCSCPClient, minArgs: Int, maxArgs: Int, sig: OMApplication, symbol: OMSymbol) extends SCSCPHandler {
  override val min: Int = minArgs
  override val max: Int = maxArgs
  override val signature = sig

  override def handle(client: SCSCPServerClient, arguments: SCSCPCallArguments, parameters: OMExpression*): OMExpression =
    CAS(new SCSCPCall(symbol, SCSCPCallArguments(CAS.newCallId, Some(SCSCPReturnObject), null), parameters: _*)).fetch().get
}
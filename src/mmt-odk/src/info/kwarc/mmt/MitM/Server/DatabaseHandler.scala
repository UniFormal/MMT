/* FR: This looks like dead code - taken out.

package info.kwarc.mmt.MitM.Server


import info.kwarc.mmt.odk.OpenMath._
import info.kwarc.mmt.odk.SCSCP.CD.{SymbolSet, scscp1, scscp2}
import info.kwarc.mmt.odk.SCSCP.Client.SCSCPClient
import info.kwarc.mmt.odk.SCSCP.Protocol.{SCSCPCall, SCSCPCallArguments, SCSCPReturnObject}
import info.kwarc.mmt.odk.SCSCP.Server.{RemoteServerException, SCSCPHandler, SCSCPServer, SCSCPServerClient}

abstract class DatabaseHandler(val symbol: OMSymbol) extends MitMExtension with SCSCPHandler 

/**
  * a database of mitm systems
  * @param scscpServer
  */
class MitMDatabase(scscpServer: => SCSCPServer) {
  var mapOfServersToFunctions : Map[String, List[OMSymbol]] = Map()
  var mapOfServerNamesToClients : Map[String, SCSCPClient] = Map()

  def addServer(serverAddress: String, port: Int): OMString = {
    val client : SCSCPClient = SCSCPClient(serverAddress, port)
    val serverID : String = client.service_id
    val name = client.service_name
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
              scscpServer.register(symbol, new RemoteCallHandler(client, min.toInt, max.toInt, sig, symbol))
            // unbound number of args + bound domain
            case List(`symbol`, OMInteger(min, None), `infinity`, sig:OMApplication) =>
              scscpServer.register(symbol, new RemoteCallHandler(client, min.toInt, -1, sig, symbol))
            // bound number of args + unbound domain
            case List(`symbol`, OMInteger(min, None), OMInteger(max, None), `unboundDomain`) =>
              scscpServer.register(symbol, new RemoteCallHandler(client, min.toInt, max.toInt, SymbolSet(Nil), symbol))
            // unbound number of args + unbound domain
            case List(`symbol`, OMInteger(min, None), `infinity`, `unboundDomain`) =>
              scscpServer.register(symbol, new RemoteCallHandler(client, min.toInt, -1, SymbolSet(Nil), symbol))
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
      scscpServer.unregister(symbol)
    }
    client.quit()
    mapOfServerNamesToClients -= serverID
    mapOfServersToFunctions -= serverID
    scscp1(scscp1.procedureCompleted)
  }

  /**
    * Registers the server in the MitM proxy and returns the id of the server.
    */
  object RegisterServerHandler extends DatabaseHandler(OMSymbol("registerServer", "mitm_transient", None, None)) {
    override val min: Int = 1
    override val max: Int = 2
    // i.e. the signature is address: string, port: integer
    override val signature = Some(SymbolSet(List(OMSymbol("string", "omtypes", None, None), OMSymbol("integer", "omptypes", None, None))))

    def handle(client: SCSCPServerClient, arguments: SCSCPCallArguments, parameters: OMExpression*): OMExpression = {
      val listOfArgs = parameters.toList
      val address = getArgAsString(listOfArgs, 0)
      val port = if (listOfArgs.length > 1) getArgAsInt(listOfArgs, 1).int.toInt else 26133
      addServer(address.text, port)
    }
  }

  object RemoveServerHandler extends DatabaseHandler(OMSymbol("removeServer", "mitm_transient", None, None)) {
    override val min: Int = 1
    override val max: Int = 1
    override val signature = Some(SymbolSet(List(OMSymbol("string", "omtypes", None, None))))

    def handle(client: SCSCPServerClient, arguments: SCSCPCallArguments, parameters: OMExpression*): OMExpression = {
      val server = getArgAsString(parameters.toList, 0)
      removeServer(server.text)
    }
  }

  object GetAllServersHandler extends DatabaseHandler(OMSymbol("getAllServers", "mitm_transient", None, None)) {
    override val min: Int = 0
    override val max: Int = 0
    override val signature = Some(SymbolSet(Nil))

    override def handle(client: SCSCPServerClient, arguments: SCSCPCallArguments, parameters: OMExpression*): OMExpression = {
      var listOfServers: List[OMString] = List()
      for (server <- mapOfServerNamesToClients.keys) {
        listOfServers ::= OMString(server, None)
      }
      SymbolSet(listOfServers)
    }
  }
}

class RemoteCallHandler(CAS: SCSCPClient, minArgs: Int, maxArgs: Int, sig: OMApplication, symbol: OMSymbol) extends SCSCPHandler {
  override val min: Int = minArgs
  override val max: Int = maxArgs
  override val signature = Some(sig)

  override def handle(client: SCSCPServerClient, arguments: SCSCPCallArguments, parameters: OMExpression*): OMExpression =
    CAS(new SCSCPCall(symbol, SCSCPCallArguments(CAS.newCallId, Some(SCSCPReturnObject), null), parameters: _*)).fetch().get
}

*/
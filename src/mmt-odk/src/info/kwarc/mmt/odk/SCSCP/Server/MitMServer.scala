package info.kwarc.mmt.odk.SCSCP.Server

import info.kwarc.mmt.odk.OpenMath._
import info.kwarc.mmt.odk.SCSCP.CD.SymbolSet
import info.kwarc.mmt.odk.SCSCP.Client.SCSCPClient
import info.kwarc.mmt.odk.SCSCP.Protocol.{SCSCPCall, SCSCPCallArguments, SCSCPReturnObject}

/**
  * SCSCP server that implements the MitM protocol.
  */
object MitMServer {
  val server = SCSCPServer("MitMServer", "1.0", "MitMServer")

  def main(args: Array[String]): Unit = {
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
  var mapOfServersToFunctions : Map[String, List[OMSymbol]] = Map()
  var mapOfServerNamesToClients : Map[String, SCSCPClient] = Map()

  def addServer(serverAddress: String, port: Int): String = {
    val client : SCSCPClient = SCSCPClient(serverAddress, port)
    val signatureReqSym = OMSymbol("get_signature", "scscp2", None, None)
    val signatureSym = OMSymbol("signature", "scscp2", None, None)
    val infinity = OMSymbol("infinity", "nums1", None, None)

    var listOfSymbols : List[OMSymbol] = List()
    mapOfServerNamesToClients += (serverAddress -> client)
    for (symbol <- client.getAllowedHeads) {
      val signatureRequest = OMApplication(signatureReqSym, List(symbol), None, None)
      val signature = client(signatureRequest).fetch().get
      signature match {
        case OMApplication(`signatureSym`, args, None, None) =>
          args match {
            case List(`symbol`, OMInteger(min, None), OMInteger(max, None), sig) =>
              MitMServer.server.register(symbol, new RemoteCallHandler(client, min.toInt, max.toInt, sig, symbol))
            case List(`symbol`, OMInteger(min, None), `infinity`, sig) =>
              MitMServer.server.register(symbol, new RemoteCallHandler(client, min.toInt, -1, sig, symbol))
            case _ => throw new SignatureMismatchException()
          }
        case _ =>
          throw new SignatureMismatchException()
      }
      listOfSymbols ::= symbol
    }
    mapOfServersToFunctions += (serverAddress -> listOfSymbols)
    serverAddress
  }

  def removeServer(serverID: String): String = {
    val client = mapOfServerNamesToClients(serverID)
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
  override val max : Int = 2
  // i.e. the signature is address: string, port: integer
  override val signature = SymbolSet(List(OMSymbol("string", "omtypes", None, None), OMSymbol("integer", "omptypes", None, None)))
  def handle(client: SCSCPServerClient, arguments : SCSCPCallArguments, parameters: OMExpression* ) : OMExpression = {
    val args = parameters.toList
    args match {
      case List(address: OMString, port: OMInteger) =>
        OMString(MitMDatabase.addServer(address.text, port.int.toInt), None)
      case _ => throw new SignatureMismatchException()
    }
  }
}

class RemoveServerHandler() extends SCSCPHandler {
  override val min: Int = 1
  override val max: Int = 1
  override val signature = SymbolSet(List(OMSymbol("string", "omtypes", None, None)))
  def handle(client: SCSCPServerClient, arguments: SCSCPCallArguments, parameters: OMExpression*): OMExpression = {
    val arg = parameters.toList.head
    arg match {
      case arg: OMString =>
        OMString(MitMDatabase.removeServer(arg.text), None)
      case _ => throw new SignatureMismatchException()
    }
  }
}

class RemoteCallHandler(CAS: SCSCPClient, minArgs: Int, maxArgs: Int, sig: OMExpression, symbol: OMSymbol) extends SCSCPHandler {
  override val min: Int = minArgs
  override val max: Int = maxArgs
  override val signature = sig

  override def handle(client: SCSCPServerClient, arguments: SCSCPCallArguments, parameters: OMExpression*): OMExpression =
    CAS(new SCSCPCall(symbol, SCSCPCallArguments(CAS.newCallId, Some(SCSCPReturnObject), null), parameters: _*)).fetch().get
}
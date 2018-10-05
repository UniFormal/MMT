package info.kwarc.mmt.odk.SCSCP.Server

import info.kwarc.mmt.api.frontend.Extension
import info.kwarc.mmt.api.objects.OMID
import info.kwarc.mmt.api.utils.URI
import info.kwarc.mmt.odk.OpenMath.Coding.OMMMTCoding
import info.kwarc.mmt.odk.{GAPSystem, SageSystem, SingularSystem, Systems}
import info.kwarc.mmt.odk.OpenMath._
import info.kwarc.mmt.odk.SCSCP.CD.{SymbolSet, scscp1, scscp2}
import info.kwarc.mmt.odk.SCSCP.Client.SCSCPClient
import info.kwarc.mmt.odk.SCSCP.Protocol.{SCSCPCall, SCSCPCallArguments, SCSCPReturnObject}

import scala.concurrent.Future

/**
  * SCSCP server that implements the MitM protocol.
  */
class MitMServer extends Extension {
  override def logPrefix: String = "mitm"

  var server : Option[SCSCPServer] = None //

  implicit val ec = scala.concurrent.ExecutionContext.global

  override def start(args: List[String]): Unit = {
    // connect the handlers for registerServer and removeServer

    // and serve it forever
    Future {
      try {
        val theServer = SCSCPServer("MitMServer", "1.0", "MitMServer", {s => log(s)})
        log("Starting SCSCP Server on 0.0.0.0:26133")
        server = Some(theServer)
        theServer.register(OMSymbol("mitmEval", "mitm_transient", None, None), MiTMEvalHandler)
        theServer.register(OMSymbol("registerServer", "mitm_transient", None, None), RegisterServerHandler)
        theServer.register(OMSymbol("removeServer", "mitm_transient", None, None), RemoveServerHandler)
        theServer.register(OMSymbol("getAllServers", "mitm_transient", None, None), GetAllServersHandler)
        theServer.processForever()
        log("SCSCP Server stopped")
      } catch {
        case e : java.net.BindException =>
          log("SCSCP Server already running")
          controller.extman.removeExtension(this)
        case f: Exception =>
          log(s"got exception: $f")
      }
    }
  }

  override def destroy: Unit = {
    server.foreach(_.quit(None))
  }

  object MiTMEvalHandler extends SCSCPHandler {
    override val min: Int = 1
    override val max: Int = 1

    /** a coding to decode / encode MMT terms */
    protected val coding = new OMMMTCoding(URI(controller.getBase.toPath))

    override val signature = None

    /** the odk plugin, so that we can interact and call it */
    protected lazy val odkPlugin: info.kwarc.mmt.odk.Plugin  = controller.extman.get(classOf[info.kwarc.mmt.odk.Plugin]).head

    def handle(client: SCSCPServerClient, arguments: SCSCPCallArguments, parameters: OMExpression*): OMExpression = {
      val theTerm = OMID(Systems.evalSymbol)(parameters.map(coding.encode): _*)
      log(s"client ${client.identifier} -> MiTM: ${theTerm.toString}")
      val theResult = odkPlugin.simplify(theTerm, None)
      log(s"MitM -> client ${client.identifier}: ${theResult.toString}")
      coding.decode(theResult).asInstanceOf[OMExpression]
    }
  }


  /**
    * Registers the server in the MitM proxy and returns the id of the server.
    */
  object RegisterServerHandler extends SCSCPHandler {
    override val min: Int = 1
    override val max: Int = 2
    // i.e. the signature is address: string, port: integer
    override val signature = Some(SymbolSet(List(OMSymbol("string", "omtypes", None, None), OMSymbol("integer", "omptypes", None, None))))

    def handle(client: SCSCPServerClient, arguments: SCSCPCallArguments, parameters: OMExpression*): OMExpression = {
      val listOfArgs = parameters.toList
      val address = getArgAsString(listOfArgs, 0)
      val port = if (listOfArgs.length > 1) getArgAsInt(listOfArgs, 1).int.toInt else 26133
      MitMDatabase.addServer(address.text, port)
    }
  }

  object RemoveServerHandler extends SCSCPHandler {
    override val min: Int = 1
    override val max: Int = 1
    override val signature = Some(SymbolSet(List(OMSymbol("string", "omtypes", None, None))))
    def handle(client: SCSCPServerClient, arguments: SCSCPCallArguments, parameters: OMExpression*): OMExpression = {
      val server = getArgAsString(parameters.toList, 0)
      MitMDatabase.removeServer(server.text)
    }
  }

  object GetAllServersHandler extends SCSCPHandler {
    override val min: Int = 0
    override val max: Int = 0
    override val signature = Some(SymbolSet(Nil))

    override def handle(client: SCSCPServerClient, arguments: SCSCPCallArguments, parameters: OMExpression*): OMExpression = {
      var listOfServers : List[OMString] = List()
      for (server <- MitMDatabase.mapOfServerNamesToClients.keys) {
        listOfServers ::= OMString(server, None)
      }
      SymbolSet(listOfServers)
    }
  }

  /**
    * Database that stores and manages the function headers received from CAS clients.
    * Currently implemented in-memory, could in the long run be moved to the file system for extensibility.
    */
  val mitm = this

  object MitMDatabase {
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
                mitm.server.get.register(symbol, new RemoteCallHandler(client, min.toInt, max.toInt, sig, symbol))
              // unbound number of args + bound domain
              case List(`symbol`, OMInteger(min, None), `infinity`, sig:OMApplication) =>
                mitm.server.get.register(symbol, new RemoteCallHandler(client, min.toInt, -1, sig, symbol))
              // bound number of args + unbound domain
              case List(`symbol`, OMInteger(min, None), OMInteger(max, None), `unboundDomain`) =>
                mitm.server.get.register(symbol, new RemoteCallHandler(client, min.toInt, max.toInt, SymbolSet(Nil), symbol))
              // unbound number of args + unbound domain
              case List(`symbol`, OMInteger(min, None), `infinity`, `unboundDomain`) =>
                mitm.server.get.register(symbol, new RemoteCallHandler(client, min.toInt, -1, SymbolSet(Nil), symbol))
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
        mitm.server.foreach(_.unregister(symbol))
      }
      client.quit()
      mapOfServerNamesToClients -= serverID
      mapOfServersToFunctions -= serverID
      scscp1(scscp1.procedureCompleted)
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

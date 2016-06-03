package info.kwarc.mmt.odk.SCSCP.Client

import java.net.Socket

import info.kwarc.mmt.odk.OpenMath.{OMApplication, OMExpression, OMSymbol}
import info.kwarc.mmt.odk.SCSCP.CD.scscp2
import info.kwarc.mmt.odk.SCSCP.Lowlevel.{ProtocolError, SCSCPPi, SCSCPSocket}
import info.kwarc.mmt.odk.SCSCP.Protocol.{SCSCPCall, SCSCPResult, SCSCPCallArguments, SCSCPReturnObject}
import info.kwarc.mmt.odk.SCSCP._

/**
  * Implement an Symbolic Computation Software Composability Protocol Client
  *
  * @param host Hostname of server to connect to
  * @param port Port number to connect to. Defaults to 26133.
  * @param versions Version(s) to be used by this SCSCP client. Will be prioritised in order.
  */
class SCSCPClient(host : String, port : Int = SCSCPClient.default_port, versions : List[String] = "1.3" :: Nil) {

  //
  // INITIALISATION
  //

  /**
    * The underlying SCSCPSocket used by this SCSCPClient.
    */
  private val socket : SCSCPSocket = new SCSCPSocket(new Socket(host, port))

  /**
    * Connection code
    */
  val (service_name : String, service_version : String, service_id : String, scscp_version : String) =
  {
    // expect a processing instruction with none
    val vpi = socket.expectAnyInstWith(None, "service_name", "service_version", "service_id")

    //find the supported server versions
    val server_versions = vpi("scscp_versions").split(" ")

    // and select the one we can use or quit
    val used_version = versions.filter(server_versions.contains(_)) match {
      case Nil =>
        quit(Some("version not supported"))
        throw new ProtocolError("Server does not support SCSCP version")
      case h :: _ => h
    }

    // make a PI for the version
    val version_pi = SCSCPPi(Map(("version", used_version)))

    // send it an expect it back
    socket.write(version_pi)
    socket.expectThisInst(version_pi)

    // return the service name + version + id
    (vpi("service_name"), vpi("service_version"), vpi("service_id"), used_version)
  }

  /**
    * Checks if this socket is still connected
    *
    * @return
    */
  def isConnected : Boolean = socket.socket.isConnected && !socket.socket.isClosed

  //
  // BASIC FUNCTIONALITY
  //

  /**
    * Quits the session with the SCSCP server
    *
    * @param reason Reason for quitting. Optional.
    */
  def quit(reason : Option[String] = None) : Unit = {
    // build the reson
    val mp : Map[String, String] = reason match {
      case Some(r) => Map(("reason", r))
      case None => Map()
    }

    // send the quit message
    socket.write(SCSCPPi(Some("quit"), mp))

    // and close the connection
    socket.socket.close()
  }

  /**
    * Sends an info message to the server
    *
    * @param message Message to send
    */
  def info (message : String): Unit = {
    socket.write(SCSCPPi(None, Map(("info", message))))
  }


  //
  // INTERNAL STATE VARIABLES
  //

  /**
    * A list of results from the server
    */
  private val results : scala.collection.mutable.Map[String, SCSCPResult] = scala.collection.mutable.Map()

  /**
    * A list of information messages from the server
    */
  private val infos : scala.collection.mutable.ArrayBuffer[String] = scala.collection.mutable.ArrayBuffer()


  //
  // UPDATING INTERNAL STATE
  //

  /**
    * Fetches the next item from the internal socket
    * and adds it to the internal Queue
    */
  def fetch() : Unit = {
    socket.read match {
      // in case of an info message
      case Left(SCSCPPi(None, a)) =>
        if(a.contains("info")){
          infos += a("info")
        }
      // in case of a quit message
      case Left(q @ SCSCPPi(Some("quit"), _)) =>
        if(q.attributes.contains("reason")) {
          throw new ProtocolError("Unable to read from Server: Server has quit: " + q.attributes("reason"))
        } else {
          throw new ProtocolError("Unable to read from Server: Server has quit. ")
        }

      case Left(_) =>
        throw new ProtocolError("Unknown processing instruction")

      // everything else is a result
      case Right(omobj) =>
        val result = SCSCPResult(omobj)
        results(result.call_id) = result
    }
  }

  //
  // READING INTERNAL STATE
  //

  /**
    * Gets the list of information messages received by this Client.
    *
    * @return
    */
  def info : List[String] = infos.toList

  /**
    * Tries to get the result with the given id
    *
    * @return
    */
  def get(call_id : String) : Option[SCSCPResult] = results.lift(call_id)

  /**
    * Fetches the result with the given id. Blocks until the result is available.
    *
    * @param call_id Id of Call to fetch
    * @return
    */
  def fetch(call_id : String) : SCSCPResult = {
    while(true) {
      get(call_id) match {
        case None => fetch()
        case Some(r) => return r
      }
    }

    throw new IllegalStateException("Exited a non-breaking while(true) loop")
  }


  //
  // MAKING PROCEDURE CALLS
  //



  /**
    * Generates a new Call Id to be used with this SCSCPClient
    *
    * @return
    */
  def newCallId : String = {

    // grab the current time in milliseconds
    val time = java.lang.System.currentTimeMillis().toString

    // and just to be safe, add a random integer
    val rn = scala.util.Random.nextInt().toString

    // and concatinate with the service id
    service_id + ":" + time + ":" + rn
  }

  /**
    * Makes a remote procedure call on the Server
    *
    * @param req Request of function to call
    * @return
    */
  def apply(req : SCSCPCall) : SCSCPComputation = {

    // grab the call id
    val call_id = req.arguments.call_id

    // Send it as an OpenMath object
    socket.write(req.toOMObject)

    // and return
    new SCSCPComputation(this, call_id)
  }

  /**
    * Makes a remote procedure call on the server
    *
    * @param procedure Symbol pointing to procedure to call
    * @param parameters Parameters for the procedure call
    * @return
    */
  def apply(procedure : OMSymbol, parameters: OMExpression*) : SCSCPComputation = {
    val req = new SCSCPCall(procedure, SCSCPCallArguments(newCallId, Some(SCSCPReturnObject), null), parameters : _*)
    apply(req)
  }

  /**
    * Sends an Interrupt Calls for the given call id
    *
    * @param call_id
    */
  def interrupt(call_id : String) : Unit = {
    socket.write(SCSCPPi(Some("terminate"), Map(("call_id", call_id))))
  }


  //
  // SPECIAL procedure calls
  //

  /**
    * Get the list of allowed heads
    * (a.k.a the list of allowed procedure calls)
    *
    * @return
    */
  def getAllowedHeads : List[OMSymbol] = {
    val ss = scscp2(scscp2.symbolSet)

    apply(scscp2(scscp2.getAllowedHeads)).fetchExpression() match {
      case OMApplication(`ss`, args, _, _) =>
        args.map(_.asSymbol)
    }
  }

}

object SCSCPClient {
  val default_port: Int = 26133
}
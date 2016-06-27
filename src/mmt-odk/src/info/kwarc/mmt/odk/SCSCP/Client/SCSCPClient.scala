package info.kwarc.mmt.odk.SCSCP.Client

import java.net.Socket

import info.kwarc.mmt.odk.OpenMath.{OMApplication, OMExpression, OMObject, OMSymbol}
import info.kwarc.mmt.odk.SCSCP.CD.scscp2
import info.kwarc.mmt.odk.SCSCP.Lowlevel.Readers.SCSCPReader
import info.kwarc.mmt.odk.SCSCP.Lowlevel.SCSCPPi
import info.kwarc.mmt.odk.SCSCP.Lowlevel.Writers.SCSCPWriter
import info.kwarc.mmt.odk.SCSCP.Protocol.{SCSCPCall, SCSCPCallArguments, SCSCPResult, SCSCPReturnObject}

import scala.collection.mutable

/**
  * Represents a client to an SCSCP
  *
  * @param socket
  * @param encoding
  */
class SCSCPClient(socket : Socket, encoding : String = "UTF-8"){

  private final val INFO_MESSAGE : String = "info"
  private final val QUIT_MESSAGE_KEY : String = "quit"
  private final val QUIT_REASON_KEY : String = "reason"
  private final val VERSIONS : List[String] = "1.3" :: Nil

  // create reader and writer instances
  val reader : SCSCPReader = new SCSCPReader(socket.getInputStream, encoding)
  val writer : SCSCPWriter = new SCSCPWriter(socket.getOutputStream, encoding)

  /**
    * Connection code
    */
  val (service_name : String, service_version : String, service_id : String, scscp_version : String) =
  {
    // expect a processing instruction with none
    val vpi = reader.getBlock match {
      case Left(pi: SCSCPPi) => pi
      case Right(_) => throw new ProtocolError("Excpected a processing instruction, got an OpenMathObject")
    }

    //find the supported server versions
    val server_versions = vpi("scscp_versions").split(" ")

    // and select the one we can use or quit
    val used_version = VERSIONS.filter(server_versions.contains(_)) match {
      case Nil =>
        quit(Some("This client only supports version 1.3"))
        throw new ProtocolError("Server does not support SCSCP version 1.3, exiting")
      case h :: _ => h
    }

    // make a PI for the version
    val version_pi = SCSCPPi(Map(("version", used_version)))

    // send a version
    writer.write(version_pi)

    // and expect it back
    reader.getBlock match {
      case Left(pi) => null
      case _ => throw new ProtocolError("Server did not echo back the SCSCP version")
    }

    // return the service name + version + id
    (vpi("service_name"), vpi("service_version"), vpi("service_id"), used_version)
  }


  private var hasQuit = false

  /**
    * Checks if this SCSCPClient is still connected to the server
    *
    * @return
    */
  def connected = socket.isConnected && !hasQuit


  // The list information messages
  private val info : mutable.Queue[String] = new mutable.Queue[String]

  // list of cached results
  private val results : mutable.Map[String, SCSCPResult] = mutable.Map()

  /**
    * Gets all information messages
    *
    * @return
    */
  def getInfoMessages : List[String] = info.toList

  /**
    * Protected function that gets called on information messages
    *
    * @param message
    */
  protected def onInfo(message : String) = {}

  /**
    * Protected function that gets called on Quit messages
    *
    * @param reason
    */
  protected def onQuit(reason : Option[String]) = {}

  /**
    * Handles a processing instruction
    *
    * @param pi
    */
  private def handle(pi : SCSCPPi) : Unit = {

    // read info messages
    if(pi.attributes.contains(INFO_MESSAGE)) {
      val msg = pi(INFO_MESSAGE)
      info.enqueue(msg)
      onInfo(msg)
    } else if (pi.key.contains(QUIT_MESSAGE_KEY)){
      val reason = pi.attributes.lift(QUIT_REASON_KEY)
      hasQuit = true
      onQuit(reason)
    } else {
      throw new ProtocolError("Unknown Processing Ins")
    }
  }

  private def handle(obj : OMObject) : Unit = {

    // parse a result from the OMObject
    val result = try {
      SCSCPResult(obj)
    } catch {
      case e : Exception => throw new ProtocolError("Unable to parse OpenMath")
    }

    // store it in the result map
    results(result.call_id) = result
  }

  /**
    * Handles all items available locally
    */
  private def handleAll() : Unit = {
    // read data
    var hasData = true

    while(hasData){
      reader.get() match {
        case Some(a) => handle(a)
        case None =>
          hasData = false
      }
    }

  }

  /**
    * Handles an incoming messages
    *
    * @param message Message to handle
    */
  private def handle(message : Either[SCSCPPi,OMObject]) : Unit = message match {
    case Left(pi : SCSCPPi) => handle(pi)
    case Right(obj : OMObject) => handle(obj)
  }

  /**
    * Gets a result for the given message it it is available
    *
    * @param call_id
    * @return
    */
  def getResult(call_id : String) : Option[SCSCPResult] = {
    // read all
    handleAll()

    // and return if it is available
    results.lift(call_id)
  }

  /**
    * Gets
    *
    * @param call_id
    * @return
    */
  def fetchResult(call_id : String) : SCSCPResult = {

    // handle everything
    handleAll()

    // while you don't have it get the next one
    while(!results.contains(call_id)){
      handle(reader.getBlock)
    }

    // and return it
    results(call_id)
  }

  /**
    * Quits the session with the SCSCP server
    *
    * @param reason Reason for quitting. Optional.
    */
  def quit(reason : Option[String] = None) : Unit = {
    // build the reason
    val mp : Map[String, String] = reason match {
      case Some(r) => Map(("reason", r))
      case None => Map()
    }
    // send the quit message and notify
    writer.write(SCSCPPi(Some("quit"), mp))
    onQuit(reason)

    // and close the connection
    socket.close()
  }

  /**
    * Sends an info message to the server
    *
    * @param message Message to send
    */
  def info (message : String): Unit = {
    writer.write(SCSCPPi(None, Map(("info", message))))
  }

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
  def apply(req : SCSCPCall) : SCSCPClientComputation = {

    // grab the call id
    val call_id = req.arguments.call_id

    // Send it as an OpenMath object
    writer.write(req.toOMObject)

    // and return
    new SCSCPClientComputation(this, call_id)
  }

  /**
    * Makes a remote procedure call on the server
    *
    * @param procedure Symbol pointing to procedure to call
    * @param parameters Parameters for the procedure call
    * @return
    */
  def apply(procedure : OMSymbol, parameters: OMExpression*) : SCSCPClientComputation = {
    val req = new SCSCPCall(procedure, SCSCPCallArguments(newCallId, Some(SCSCPReturnObject), null), parameters : _*)
    apply(req)
  }

  /**
    * Makes a remote procedure call on the server
 *
    * @param app
    * @return
    */
  def apply(app : OMApplication): SCSCPClientComputation = app match {
    case OMApplication(s : OMSymbol, arguments, _, _) => apply(s, arguments : _*)
  }

  /**
    * Sends an Interrupt Calls for the given call id
    *
    * @param call_id
    */
  def interrupt(call_id : String) : Unit = {
    writer.write(SCSCPPi(Some("terminate"), Map(("call_id", call_id))))
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

object SCSCPClient{
  def apply(host : String, port : Int = 26133, encoding : String = "UTF-8") =
    new SCSCPClient(new java.net.Socket(host, port), encoding)
}


class ProtocolError(message : String) extends Exception("Protocol Error: "+message)
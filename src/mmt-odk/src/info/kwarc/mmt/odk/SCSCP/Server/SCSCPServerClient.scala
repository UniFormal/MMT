package info.kwarc.mmt.odk.SCSCP.Server

import info.kwarc.mmt.odk.SCSCP.Protocol._
import info.kwarc.mmt.odk.SCSCP.Lowlevel.Readers.SCSCPReader
import info.kwarc.mmt.odk.SCSCP.Lowlevel.Writers.SCSCPWriter
import java.net.Socket

import info.kwarc.mmt.api.utils.URI
import info.kwarc.mmt.odk.OpenMath._
import info.kwarc.mmt.odk.SCSCP.CD.scscp1
import info.kwarc.mmt.odk.SCSCP.Lowlevel.SCSCPPi

import scala.collection.mutable

/** Represents a single client connected to the server */
class SCSCPServerClient(socket: Socket, server: SCSCPServer, encoding: String = "UTF-8") {

  /** logs a debug message */
  def debug(s: String): Unit = server.debug(s"client $identifier: $s")

  /** An identifier for this client, which is unique within the server */
  // TODO: Do not rely on the port and remote IP to be given by the string
  val identifier: String = s"${socket.getInetAddress.getHostName}:${socket.getPort}"
  // create reader and writer instances
  val reader: SCSCPReader = new SCSCPReader(socket.getInputStream, encoding)
  val writer: SCSCPWriter = new SCSCPWriter(socket.getOutputStream, encoding)

  /** a boolean indicating if the server has quit the connection */
  private var hasQuit = false

  /** List of results of function calls */
  private val results = mutable.Map[String, OMAny]()


  // The list information messages
  private val info: mutable.Queue[String] = new mutable.Queue[String]

  /**
    * Checks if this SCSCPClient is still connected to the server
    *
    * @return
    */
  def connected: Boolean = !socket.isClosed && !hasQuit

  // Initial connection
  writer.write(SCSCPPi(None, Map(
    (SCSCPAttributes.SCSCP_VERSIONS, SCSCPConstants.VERSIONS.mkString(" ")),
    (SCSCPAttributes.SERVICE_ID, server.service_identifier),
    (SCSCPAttributes.SERVICE_VERSION, server.service_version),
    (SCSCPAttributes.SERVICE_NAME, server.service_name)
  )))

  // Version negotiation
  private val version = reader.getBlock match {
    case Left(pi: SCSCPPi) => {
      // make sure the client sent us a version at all
      if (!pi.attributes.isDefinedAt(SCSCPAttributes.VERSION)) {
        quit(Some("expected a version from the client"))
        throw new ClientVersionExpected()
      }

      // did it also send a supported version
      val client_version = pi(SCSCPAttributes.VERSION)
      if (!SCSCPConstants.VERSIONS.contains(client_version)) {
        debug("got unsupported scscp version")
        quit(Some("unsupported version"))
        throw new UnsupportedProtocolVersion()
      }

      // send back that supported version
      // make a PI for the version
      val version_pi = SCSCPPi(Map((SCSCPAttributes.VERSION, client_version)))
      writer.write(version_pi)

      debug(s"negotiated scscp version $client_version")

      // and return it
      client_version
    }
    case _ => throw new ClientVersionExpected()
  }

  /**
    * Quits the session with the SCSCP server
    *
    * @param reason Reason for quitting. Optional.
    */
  def quit(reason: Option[String] = None): Unit = {
    debug(s"quitting: ${reason.getOrElse("")}")

    // build the reason
    val mp: Map[String, String] = reason match {
      case Some(r) => Map((SCSCPAttributes.REASON, r))
      case None => Map()
    }

    if (!socket.isClosed) {
      // send the quit message and notify
      writer.write(SCSCPPi(Some(SCSCPMessageKeys.QUIT), mp))
    }

    onQuit(reason)

    // and close the connection
    socket.close()
  }

  /** processes one instruction from the client if it is available. Will block until the computation is complete. */
  def process(): Unit = {

    // if we are not connected, we have nothing to do
    if (!connected) {
      return
    }

    // read data and update the OM Coding that has been used
    val data = reader.get()
    writer.codingState = reader.codingState

    data match {
      case Some(Left(pi: SCSCPPi)) => processPi(pi)
      case Some(Right(om: OMObject)) => processOM(om)
      case _ =>
    }
  }

  /** handles a single processing instruction */
  private def processPi(pi: SCSCPPi): Unit = {
    // read info messages
    if (pi.attributes.contains(SCSCPAttributes.INFO)) {
      debug(s"handling client INFO")

      val msg = pi(SCSCPAttributes.INFO)
      info.enqueue(msg)
      onInfo(msg)

      // when the client has quit
      // we should just end it all
    } else if (pi.key.contains(SCSCPMessageKeys.QUIT)) {
      debug(s"handling client QUIT")

      val reason = pi.attributes.get(SCSCPAttributes.REASON)
      socket.close()

      hasQuit = true
      onQuit(reason)
    } else if (pi.key.contains(SCSCPMessageKeys.TERMINATE)) {
      debug(s"handling client TERMINATE")
      // As per spec, we can compute the computation either way
      // for simplicity of implementation we actually will
      // and just ignore this instruction
    } else {
      debug(s"got unknown processing instruction")
      // as per spec, we ignore unknown processing instructions
      // throw new UnknownProcessingInstruction()
    }
  }

  /** handles a single procedure call */
  private def processOM(om: OMObject): Unit = {

    debug(s"handling object (procedure call): $om")

    // figure out what call to make
    val call = SCSCPCall.parse(om)

    // the return parameters just contain the call id
    val returnparams = OMAttributionPairs(List((scscp1(scscp1.callId), OMString(call.arguments.call_id, None))), None, None)

    // TODO: Go into the call and replace

    // TODO: Parse previous objects
    val retvat: SCSCPResult = try {
      // get the handler for the right procedure
      val handler = server.getHandler(call.procedure)


      // make the computation and store the result
      // we do this by
      debug(s"calling handler $handler")
      val result = handler.handle(this, call.arguments, call.parameters: _*)
      debug(s"handler returned result $result")

      // and prepare a response
      call.arguments.return_method.getOrElse(SCSCPReturnObject) match {
        case SCSCPReturnObject =>
          // simply return the result
          SCSCPObjectReturned(result, returnparams)
        case SCSCPReturnCookie =>
          // store the coookie
          results(call.arguments.call_id) = result
          val ref = OMReference(URI(None, None, List(call.arguments.call_id), false, None, None), None)
          SCSCPObjectStored(ref, returnparams)
        case SCSCPReturnNothing =>
          SCSCPNothingReturned(returnparams)
      }
    } catch {
      case e :SignatureMismatchException =>
        debug(s"caught signature mismatch in call: $e")
        SCSCPTerminated(OMError(scscp1(scscp1.errorSystemSpecific),
          OMString("expected signature: " + e.getExpected + ", actual: " + e.getActual, None) :: Nil, None, None), returnparams)
      case e: Exception =>
        debug(s"caught exception in call: $e")
        SCSCPTerminated(OMError(scscp1(scscp1.errorSystemSpecific), OMString(e.getClass.getCanonicalName, None) :: Nil, None, None), returnparams)
    }

    // and write the result
    debug(s"writing call result: $retvat")
    writer.write(retvat.toObject)
  }


  /**
    * Protected function that gets called on information messages
    *
    * @param message Info message that was sent by the server
    */
  protected def onInfo(message: String): Unit = {}

  /**
    * Protected function that gets called on Quit messages
    *
    * @param reason Reason that was given for quitting by the server
    */
  protected def onQuit(reason: Option[String]): Unit = {}


  /**
    * Sends an info message to the client
    *
    * @param message Message to send
    */
  def info(message: String): Unit = {
    writer.write(SCSCPPi(None, Map((SCSCPAttributes.INFO, message))))
  }
}

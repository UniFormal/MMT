package info.kwarc.mmt.odk.SCSCP

import java.net.Socket
import java.io.{BufferedReader, InputStreamReader}

import info.kwarc.mmt.odk.OpenMath.{OMXMLEncoding, OMObject}


/**
  * Represents a socket that can be used for SCSP communication
  * @param socket Underlying socket object to use
  */
class SCSCPSocket (val socket: Socket) {

  /**
    * A parser to read / write OpenMath XML
    */
  private val om_parser = new OMXMLEncoding()


  /**
    * Processing instruction used to indicate the start of an OpenMath Element
    */
  private val start_pi = SCSCPPi(Some("start"), Map())

  /**
    * Processing instruction used to indicate the end of an OpenMath Element
    */
  private val end_pi = SCSCPPi(Some("end"), Map())

  /**
    * Processing instruction used to indicate the cancellation of an OpenMath Element
    */
  private val cancel_pi = SCSCPPi(Some("cancel"), Map())

  /**
    * Processing instruction to indicate the quitting of the server / client
    */
  private val quit_pi = SCSCPPi(Some("quit"), Map())

  /**
    * Input stream
    */
  private val input = new BufferedReader(new InputStreamReader(socket.getInputStream, "UTF-8"))

  /**
    * Output Stream
    */
  private val output = socket.getOutputStream

  /**
    * Waits and reads a single line from the input
    * @return
    */
  private def read_socket_line : String = {
    if(! socket.isConnected){
      throw new SocketDisconnected
    }
    input.readLine
  }


  /**
    * Tries to read either a processing instruction or an OMObject
    * from the socket.
    * @return
    */
  def try_read_socket : Option[Either[SCSCPPi, OMObject]] = {
    try {
      // read the processing instruction
      val inst = SCSCPPi(read_socket_line)

      // for the start instruction, continue
      if(inst != start_pi){
        return Some(Left[SCSCPPi, OMObject](inst))
      }
    } catch {
      case e:Exception =>
        return None
    }

    // create a buffer for the openMath object
    val buffer = new StringBuilder


    while(true){

      // read a line from the socket
      val line = read_socket_line

      try {
        // read the processing instruction
        val inst = SCSCPPi(line)

        // if we have an end node
        if (inst == end_pi){
          val node : OMObject = om_parser(scala.xml.XML.loadString(buffer.toString))

          return Some(Right[SCSCPPi, OMObject](node))
        } else if (inst == cancel_pi){

          // we cancelled the object
          return None
        }
      } catch {
        case e:Exception =>
          // add the line to the buffer
          buffer ++= line + "\n"
      }
    }

    null
  }

  /**
    * Reads a processing instruction from the socket or throws an exception.
    * @return
    */
  def read_pi : SCSCPPi = try_read_socket match {case Some(Left(a)) => a}

  /**
    * Reads an OpenMath element from the socket or throws an exception.
    * @return
    */
  def read_om = try_read_socket match {case Some(Right(a)) => a}

  /**
    * Writes a string to the socket
    * @param s
    * @param flush
    */
  private def write_string (s : String, flush : Boolean = true): Unit = {
    if(! socket.isConnected){
      throw new SocketDisconnected
    }

    output.write(s.getBytes)

    if(flush){
      output.flush()
    }
  }

  /**
    * Writes a processing instruction to the client
    * @param pi
    */
  def write_pi(pi : SCSCPPi) : Unit = {
    write_string(pi.toString)
  }

  /**
    * Writes an OmDoc element to the socket.
    * @param om
    */
  def write_om(om : OMObject) : Unit = {
    write_pi(start_pi)
    write_string(om_parser(om).toString)
    write_pi(end_pi)
  }

  def apply(pi : SCSCPPi) : Unit = write_pi(pi)
  def apply(om : OMObject) : Unit = write_om(om)
  def apply() : Option[Either[SCSCPPi, OMObject]] = try_read_socket
}

/**
  * Exception that is thrown when an SCSCP Socket is disconnected and the user tries to read / write from it
  */
class SocketDisconnected extends Exception("SCSCPSocket instance is no longer connected")
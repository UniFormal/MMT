package info.kwarc.mmt.odk.SCSCP

import java.net.Socket
import java.io.{BufferedReader, InputStreamReader}

import info.kwarc.mmt.odk.OpenMath.{OMXMLEncoding, OMObject}


/**
  * Represents a socket that can be used for SCSP communication
  *
  * @param socket Underlying socket object to use
  */
class SCSCPSocket (val socket: Socket) {

  // Coder
  private val coder = new OMXMLEncoding()

  // INPUT / OUTPUT Streams
  private val input = new BufferedReader(new InputStreamReader(socket.getInputStream, "UTF-8"))
  private val output = socket.getOutputStream

  /**
    * Waits and reads a single line from the input
    *
    * @return
    */
  private def read_socket_line : String = {
    if(! socket.isConnected){
      throw new ProtocolError("Unable to read from the socket")
    }
    input.readLine
  }


  /**
    * Reads the next object from this socket or returns None.
    * Never throws exceptions.
    *
    * @return
    */
  private def read : Option[Either[SCSCPPi, OMObject]] = {
    try {
      // read the processing instruction
      val inst = SCSCPPi(read_socket_line)

      // for the start instruction, continue
      if(inst != SCSCPSocket.START_INST){
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
        if (inst == SCSCPSocket.END_INST){
          val node : OMObject = coder(scala.xml.XML.loadString(buffer.toString))

          return Some(Right[SCSCPPi, OMObject](node))
        } else if (inst == SCSCPSocket.CANCEL_INST){

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
    * Writes a String to this socket
    * @param s
    * @param flush
    */
  private def write (s : String, flush : Boolean = true): Unit = {
    if(! socket.isConnected){
      throw new ProtocolError("Unable to write to the socket")
    }

    output.write(s.getBytes)

    if(flush){
      output.flush()
    }
  }

  /**
    * Writes a processing Instruction to this socket
    *
    * @param pi
    */
  def provide(pi : SCSCPPi) : Unit = {
    write(pi.toString)
  }

  /**
    * Writes an OpenMath Element to this socket
    * @param om
    */
  def provide(om : OMObject) : Unit = {
    provide(SCSCPSocket.START_INST)
    write(coder(om).toString)
    provide(SCSCPSocket.END_INST)
  }

  /**
    * Reads any processing instruction form the command line or throws
    * a [[ProtocolError]].
    * @return the processing instruction being read.
    */
  def expect_any_inst() : SCSCPPi = {
    (try {
      read
    } catch {
      case e: Exception =>
        throw new ProtocolError("Unable to read from the socket. ")
    }) match {
      case None => throw new ProtocolError("Unable to read from the socket. ")
      case Some(Right(om)) =>
        throw new ProtocolError("Expected a processing instruction, got an OpenMath block. ")
      case Some(Left(p)) => p
    }
  }

  /**
    * Reads the exact processing instruction from the command line or throws
    * a [[ProtocolError]].
    *
    * @param pi Processing Instruction to read
    */
  def expect_this_inst(pi : SCSCPPi) : Unit = {

    // expect any processing instruction
    val got = expect_any_inst()

    if(got != pi){
      throw new ProtocolError("Expected " + pi + ", got " + got)
    }
  }

  /**
    * Reads any processing instruction with the the given key and attributes or
    * throws a [[ProtocolError]].
    * @param key Key that is expected in the processing instruction
    * @param attributes List of attributes that is expected
    * @return
    */
  def expect_any_inst_with(key : Option[String], attributes : String*) : SCSCPPi = {

    // read a processing instruction
    val got = expect_any_inst()

    if(got.key != key){
      throw new ProtocolError("Expected a processing instruction with key " + key +
        ", got a processing instruction with key " + got.key)
    }

    attributes.foreach({
      a =>
        if(!got.attributes.contains(a)){
          throw new ProtocolError("Expected a processing instruction with attribute " + a)
        }
    })

    // return the processing instruction
    got
  }

  /**
    * Reads any OpenMath object from this socket or throws a [[ProtocolError]]
    */
  def expect_any_openmath() : OMObject = {
    (try {
      read
    } catch {
      case e: Exception =>
        throw new ProtocolError("Unable to read from the socket. ")
    }) match {
      case None => throw new ProtocolError("Protocol error: Unable to read from the socket. ")
      case Some(Left(r)) =>
        throw new ProtocolError("Expected an OpenMath Block, got a processing instruction. ")
      case Some(Right(om)) => om
    }
  }

  /**
    * Reads an OpenMath object that can be defined at the given partial the given partial function applies or
    * @param one PatialFunction to use
    * @tparam A
    * @return
    */
  def expect_this_openmath[A <: OMObject](one : PartialFunction[OMObject, A]) : A = {

    // read any object
    val got = expect_any_openmath()

    // try applying the partial function
    one.lift(got) match {
      case None =>
        throw new ProtocolError("OpenMath block does not match expectation")
      case Some(x) => x
    }
  }
}

object SCSCPSocket {
  /**
    * Processing instruction used to indicate the start of an OpenMath Element
    */
  val START_INST = SCSCPPi(Some("start"), Map())

  /**
    * Processing instruction used to indicate the end of an OpenMath Element
    */
  val END_INST = SCSCPPi(Some("end"), Map())

  /**
    * Processing instruction used to indicate the cancellation of an OpenMath Element
    */
  val CANCEL_INST = SCSCPPi(Some("cancel"), Map())

  /**
    * Processing instruction to indicate the quitting of the server / client
    */
  val QUIT_INST = SCSCPPi(Some("quit"), Map())
}

/**
  * Exception that is thrown when an SCSCP Socket is disconnected and the user tries to read / write from it
  */
class SocketDisconnected extends Exception("SCSCPSocket instance is no longer connected")

/**
  * Represents an SCSCP Protocol Error
  *
  * @param message
  */
class ProtocolError(val message : String) extends Exception(message)
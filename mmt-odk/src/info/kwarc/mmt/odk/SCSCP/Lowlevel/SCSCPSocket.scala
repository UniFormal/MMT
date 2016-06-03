package info.kwarc.mmt.odk.SCSCP.Lowlevel

import java.io.{BufferedReader, InputStreamReader}
import java.net.Socket

import info.kwarc.mmt.odk.OpenMath.Coding.OMXMLCoding
import info.kwarc.mmt.odk.OpenMath.OMObject


/**
  * Represents a socket that can be used for SCSP communication
  *
  * @param socket Underlying socket object to use
  */
class SCSCPSocket (val socket: Socket) {

  // Coder for OpenMath
  private val coder = new OMXMLCoding()

  // INPUT / OUTPUT Streams
  private val input = new BufferedReader(new InputStreamReader(socket.getInputStream, "UTF-8"))
  private val output = socket.getOutputStream


  //
  // LOW-LEVEL FUNCTIONS
  //

  /**
    * Low-level function that reads a single line from the socket
    */
  private def readSocketLine : String = {
    if(! socket.isConnected || socket.isClosed){
      throw new ProtocolError("Unable to read from the socket")
    }
    input.readLine
  }

  /**
    * Low-level function that writes a single line to this Socket
    *
    * @param s Line to write to this socket
    * @param flush Optional boolean indicating if we should flush the output or not.
    */
  private def writeSocketLine(s : String, flush : Boolean = true): Unit = {
    if(! socket.isConnected || socket.isClosed ){
      throw new ProtocolError("Unable to writeSocketLine to the socket: Socket not connected. ")
    }

    output.write((s+"\n").getBytes)

    if(flush){
      output.flush()
    }
  }

  //
  // HIGH-LEVEL READING
  //

  /**
    * High-Level function that reads the next object from the socket
    */
  def read : Either[SCSCPPi, OMObject] = {
    // read the processing instruction
    val inst = SCSCPPi(readSocketLine)

    // for the start instruction, continue
    if(inst != SCSCPSocket.START_INST){
      return Left(inst)
    }

    // create a buffer for the openMath object
    val buffer = new StringBuilder


    while(true){

      // read a line from the socket
      val line = readSocketLine

      try {
        // read the processing instruction
        val inst = SCSCPPi(line)

        // if we have an end node
        if (inst == SCSCPSocket.END_INST){
          val node : OMObject = coder(scala.xml.XML.loadString(buffer.toString))

          return Right(node)
        } else if (inst == SCSCPSocket.CANCEL_INST){

          // object has been cancelled by the server => nothing read
          return null
        }
      } catch {
        case e:Exception =>
          // add the line to the buffer
          buffer ++= line + "\n"
      }
    }

    throw new IllegalStateException("Exited a non-breaking while(true) loop")
  }



  /**
    * High-level function that writes a processing instruction to the socket
    *
    * @param pi
    */
  def write(pi : SCSCPPi) : Unit = {
    writeSocketLine(pi.toString)
  }

  /**
    * High-level function that writes an OpenMath Object to the socket
    *
    * @param om
    */
  def write(om : OMObject) : Unit = {
    write(SCSCPSocket.START_INST)
    writeSocketLine(coder(om).toString)
    write(SCSCPSocket.END_INST)
  }

  //
  // EXPECTING TO READ CERTAIN OBJECTS
  //

  /**
    * Reads any processing instruction form the command line or throws
    * a [[ProtocolError]].
    *
    * @return the processing instruction being read.
    */
  def expectAnyInst(): SCSCPPi = {
    (try {
      read
    } catch {
      case e: Exception =>
        throw new ProtocolError("Unable to read from the socket. ")
    }) match {
      case Right(om) =>
        throw new ProtocolError("Expected a processing instruction, got an OpenMath block. ")
      case Left(p) => p
    }
  }

  /**
    * Reads the exact processing instruction from the command line or throws
    * a [[ProtocolError]].
    *
    * @param pi Processing Instruction to read
    */
  def expectThisInst(pi : SCSCPPi) : Unit = {

    // expect any processing instruction
    val got = expectAnyInst()

    if(got != pi){
      throw new ProtocolError("Expected " + pi + ", got " + got)
    }
  }

  /**
    * Reads any processing instruction with the the given key and attributes or
    * throws a [[ProtocolError]].
 *
    * @param key Key that is expected in the processing instruction
    * @param attributes List of attributes that is expected
    * @return
    */
  def expectAnyInstWith(key : Option[String], attributes : String*) : SCSCPPi = {

    // read a processing instruction
    val got = expectAnyInst()

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
  def expectAnyOpenMath(): OMObject = {
    (try {
      read
    } catch {
      case e: Exception =>
        throw new ProtocolError("Unable to read from the socket. ")
    }) match {
      case Left(r) =>
        throw new ProtocolError("Expected an OpenMath Block, got a processing instruction. ")
      case Right(om) => om
    }
  }

  /**
    * Reads an OpenMath object that can be defined at the given partial the given partial function applies or
 *
    * @param one PatialFunction to use
    * @tparam A
    * @return
    */
  def expectThisOpenMath[A <: OMObject](one : PartialFunction[OMObject, A]) : A = {

    // read any object
    val got = expectAnyOpenMath()

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
  * Exception that is thrown when an SCSCP Socket is disconnected and the user tries to read / writeSocketLine from it
  */
class SocketDisconnected extends Exception("SCSCPSocket instance is no longer connected")

/**
  * Represents an SCSCP Protocol Error
  *
  * @param message
  */
class ProtocolError(val message : String) extends Exception(message)
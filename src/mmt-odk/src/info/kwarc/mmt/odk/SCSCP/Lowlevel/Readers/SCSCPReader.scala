package info.kwarc.mmt.odk.SCSCP.Lowlevel.Readers

import java.io.InputStream

import info.kwarc.mmt.odk.OpenMath.Coding.OMXMLCoding
import info.kwarc.mmt.odk.OpenMath.OMObject
import info.kwarc.mmt.odk.SCSCP.Lowlevel.SCSCPPi

import scala.collection.mutable.ListBuffer

/**
  * Represents a reader that can read an SCSCP input stream
  *
  * @param stream   InputStream to read / write SCSCP data from
  * @param encoding Encoding to use. Defaults to UTF-8.
  */
class SCSCPReader(val stream: InputStream, val encoding: String = "UTF-8") {

  // hard coded instructions
  final private val START_INST = SCSCPPi(Some("start"), Map())
  final private val END_INST = SCSCPPi(Some("end"), Map())
  final private val CANCEL_INST = SCSCPPi(Some("cancel"), Map())

  // open a new reader and a new encoding object
  private val reader = new BufferedLineReader(stream, encoding)
  private val coder = new OMXMLCoding()

  /**
    * Reads the next OpenMath object or Processing instruction from the socket.
    * Only blocks in the case of unfinished transactions.
    *
    * @return
    */
  def get(): Option[Either[SCSCPPi, OMObject]] = {
    reader.readLine.map(SCSCPPi.apply).map(pi => {
      if (pi != START_INST) {
        Left(pi)
      } else {
        val o = getOMObject
        Right(o)
      }
    })
  }

  /**
    * Reads the next OpenMath object or Processing instruction from the socket.
    * Always blocks until the next object is available.
    *
    * @return
    */
  def getBlock: Either[SCSCPPi, OMObject] = {
    val pi = SCSCPPi(reader.readLineBlock)

    if (pi != START_INST) {
      Left(pi)
    } else {
      Right(getOMObject)
    }

  }

  /**
    * Gets all available lines in the buffer
    *
    * @return
    */
  def getAll: List[Either[SCSCPPi, OMObject]] = {

    // we want to store a list of things
    val l = new ListBuffer[Either[SCSCPPi, OMObject]]

    // get all the items
    while (true) {
      get() match {
        case Some(d) =>
          l += d
        case None =>
          return l.toList
      }
    }

    throw new IllegalStateException("Exited a non-breaking while(true) loop")
  }

  /**
    * Reads the end of an OpenMath transaction after the first starting instruction.
    * Always blocks until the end of the transaction.
    *
    * @return
    */
  private def getOMObject: OMObject = {

    // a buffer to cache stuff in
    val buffer = new StringBuilder

    while (true) {
      // read a line from the socket
      val line = reader.readLineBlock

      try {
        // try parsing a processing instruction
        val inst = SCSCPPi(line)

        // if it is the end, return
        if (inst == END_INST) {
          try {
            return coder(scala.xml.XML.loadString(buffer.toString))
          } catch {
            case e: Exception => throw new OpenMathParsingFailure(e)
          }

          // if it has been canceled, return null
        } else if (inst == CANCEL_INST) {
          return null

          // else it is not a processing instruction
        } else {
          throw new Exception("Move on to exception case")
        }

        // so just add it to the buffer
      } catch {
        case e: OpenMathParsingFailure => throw e.inner
        case e: Exception =>
          buffer ++= line + "\n"
      }
    }

    throw new IllegalStateException("Exited a non-breaking while(true) loop")
  }
}

class OpenMathParsingFailure(val inner : Exception) extends Exception

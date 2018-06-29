package info.kwarc.mmt.odk.SCSCP.Lowlevel.Writers

import java.io.OutputStream
import java.nio.charset.Charset

import info.kwarc.mmt.odk.OpenMath.Coding.{OMJSONCoding, OMXMLCoding}
import info.kwarc.mmt.odk.OpenMath.OMObject
import info.kwarc.mmt.odk.SCSCP.Lowlevel._

class SCSCPWriter(stream: OutputStream, val encoding: String = "UTF-8", var codingState: OMCodingState = AutoState) {

  // hard coded instructions
  final private val START_INST = SCSCPPi(Some("start"), Map())
  final private val END_INST = SCSCPPi(Some("end"), Map())
  final private val CANCEL_INST = SCSCPPi(Some("cancel"), Map())

  // read write OpenMath XML
  private val xmlCoder = new OMXMLCoding()
  private val jsonCoder = new OMJSONCoding()

  /**
    * Writes a string to the stream
    *
    * @param s
    */
  private def write(s: String): Unit = {
    stream.write(
      (s + "\n").getBytes(Charset.forName(encoding))
    )

    stream.flush()
  }

  /**
    * Writes a processing instruction to the stream
    */
  def write(pi: SCSCPPi): Unit = write(pi.toString)

  /**
    * Writes an OpenMath object to the stream
    */
  def write(om: OMObject): Unit = codingState match {
    case XMLState => writeXML(om)
    case JSONState => writeJSON(om)
    case _ => writeXML(om) // fallback to XML
  }

  private def writeXML(om: OMObject): Unit = {
    write(START_INST)
    write(xmlCoder(om).toString)
    write(END_INST)
  }

  private def writeJSON(om: OMObject): Unit = {
    write(START_INST)
    write(jsonCoder(om).toCompactString)
    write(END_INST)
  }
}

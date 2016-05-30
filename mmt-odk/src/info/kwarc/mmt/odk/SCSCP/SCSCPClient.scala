package info.kwarc.mmt.odk.SCSCP

import java.net.URL
import javax.xml.parsers.DocumentBuilderFactory
import org.w3c.dom.{Node, NodeList, ProcessingInstruction, NamedNodeMap}
import java.io.ByteArrayInputStream

class SCSCPClient(url : String, port : Int) {
  private val connection = new URL("http://" + url + ":" + port)


  private def sendMessage(input: Node) : String = {
    ""
  }
}
package info.kwarc.mmt.odk.SCSCP.Example

import info.kwarc.mmt.odk.OpenMath.Coding.GAPEncoding
import info.kwarc.mmt.odk.OpenMath.{OMInteger, OMSymbol}
import info.kwarc.mmt.odk.SCSCP.Client.SCSCPClient

/**
  * Created by twiesing on 27/06/16.
  */
object LocalClientExample {
  def main(args: Array[String]): Unit = {

    // create a new client to the SCSCP protocol
    val client = SCSCPClient("localhost")

    println(client.getAllowedHeads)
    client.quit(Some("end of session"))
    return

    val DihedralGroup = OMSymbol("DihedralGroup", "scscp_transient_1", None, None)
    val DihedralGroupAsPermGroup = OMSymbol("DihedralGroupAsPermGroup", "scscp_transient_1", None, None)

    val example = client(DihedralGroup(OMInteger(10, None))).fetchExpression()
    val example2 = client(DihedralGroupAsPermGroup(OMInteger(10, None))).fetchExpression()
    println(example)
    println(example2)
    println(GAPEncoding(example))
    println(GAPEncoding(example2))
  }
}

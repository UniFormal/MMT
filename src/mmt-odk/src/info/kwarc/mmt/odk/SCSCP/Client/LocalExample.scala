package info.kwarc.mmt.odk.SCSCP.Client

import info.kwarc.mmt.odk.OpenMath.{OMApplication, OMInteger, OMSymbol}

/**
  * Created by twiesing on 27/06/16.
  */
object LocalExample {
  def main(args: Array[String]): Unit = {

    // create a new client to the SCSCP protocol
    val client = SCSCPClient("localhost")

    println(client.getAllowedHeads)

    val DihedralGroup = OMSymbol("DihedralGroup", "scscp_transient_1", None, None)

    val example = client(DihedralGroup(OMInteger(10, None))).fetchExpression()
    println(example)

  }
}

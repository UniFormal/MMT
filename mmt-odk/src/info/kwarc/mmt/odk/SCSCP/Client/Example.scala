package info.kwarc.mmt.odk.SCSCP.Client

import info.kwarc.mmt.odk.OpenMath._

/**
  * An example SCSCPClient case
  *
  * Computes 1 + 1 on chrystal.mcs.st-andrews.ac.uk
  */
object Example {
  def main(args: Array[String]): Unit = {

    // create a new client to the SCSCP protocol
    val client = new SCSCPClient("chrystal.mcs.st-andrews.ac.uk", SCSCPClient.default_port)

    // get the allowed symbol names
    val heads = client.getAllowedHeads
    println(heads)

    // this should have the addition symbol
    var additionSymbol = OMSymbol("addition", "scscp_transient_1", None, None)
    println(heads.contains(additionSymbol))

    // apply the addition symbol to 1 + 1
    val computation = client(additionSymbol, OMInteger(1, None), OMInteger(1, None))

    // fetch the result
    val result = computation.fetch()
    println(result)

    // and get this as an actual number (hopefully 2)
    val oneplusone = result.get.asInteger.int
    println(oneplusone)

    // and finally quit the server
    client.quit(Some("Goodbye and thanks for all the fish"))
  }
}
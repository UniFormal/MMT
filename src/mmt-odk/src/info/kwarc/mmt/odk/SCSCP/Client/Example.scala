package info.kwarc.mmt.odk.SCSCP.Client

import info.kwarc.mmt.odk.OpenMath.Coding.OMMMTCoding
import info.kwarc.mmt.odk.OpenMath._

/**
  * An example SCSCPClient case
  *
  * Computes 1 + 1 on chrystal.mcs.st-andrews.ac.uk
  */
object Example {
  def main(args: Array[String]): Unit = {

    // create a new client to the SCSCP protocol
    val client = SCSCPClient("chrystal.mcs.st-andrews.ac.uk")

    // get the allowed symbol names
    val heads = client.getAllowedHeads
    println(heads)

    // this should have the addition symbol
    var additionSymbol = OMSymbol("addition", "scscp_transient_1", None, None)
    println("Can the server do addition? "+heads.contains(additionSymbol))

    // method 1 of computating : Make an actual expression and call the client with it
    val expression = OMApplication(additionSymbol, OMInteger(1, None) :: OMInteger(1, None) :: Nil, None, None)
    val oneplusonecomputation = client(expression)

    // wait while we have results
    while(oneplusonecomputation.get().isEmpty){
      // result is empty
      println("No results yet, waiting another few milliseconds")
      Thread.sleep(100)
    }
    val coding = new OMMMTCoding
    println(coding.encode(oneplusonecomputation.get().get.get))

    // once we have it, turn it into an integer
    println("1 + 1 = "+oneplusonecomputation.get().get.get.asInteger.int)

    // method two: we give arguments directly
    val zeropluszerocomputation = client(additionSymbol, OMInteger(0, None), OMInteger(0, None))

    // and we can fetch it (i.e. wait for it to arrive)
    val result = zeropluszerocomputation.fetch().get.asInteger.int
    println("0 + 0 = "+result)

    // and finally quit the server
    client.quit(Some("Goodbye and thanks for all the fish"))
  }
}
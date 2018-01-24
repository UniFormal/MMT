package info.kwarc.mmt.odk.SCSCP.Example

import info.kwarc.mmt.odk.OpenMath.Coding.GAPEncoding
import info.kwarc.mmt.odk.OpenMath._
import info.kwarc.mmt.odk.SCSCP.CD.SymbolSet
import info.kwarc.mmt.odk.SCSCP.Protocol.SCSCPCallArguments
import info.kwarc.mmt.odk.SCSCP.Server.{SCSCPHandler, SCSCPServer, SCSCPServerClient}

/**
  * An example SCSCPClient case
  *
  * Computes 1 + 1 on chrystal.mcs.st-andrews.ac.uk
  */
object ServerExample {
  def main(args: Array[String]): Unit = {

    // create a server instance
    val server = SCSCPServer("example", "1.0", "example")

    // register the add function to it
    server.register(OMSymbol("addition", "scscp_transient_1", None, None), new AdditionHandler())

    // and serve it forever
    server.processForever()
  }
}

/**
  * Implements a handler for addition
  */
class AdditionHandler() extends SCSCPHandler {
  val min : Int = 0
  val max : Int = -1
  val signature = SymbolSet(Nil) // TODO:
  def handle(client: SCSCPServerClient, arguments : SCSCPCallArguments, parameters: OMExpression* ) : OMExpression = {
    OMInteger(parameters.toList.map({
      case i:OMInteger => i.int
    }).sum, None)
  }


}

package info.kwarc.mmt.odk.SCSCP.Client

import info.kwarc.mmt.odk.OpenMath.{OMExpression, OMReference}
import info.kwarc.mmt.odk.SCSCP.CD.scscp2
import info.kwarc.mmt.odk.SCSCP.Protocol._

/**
  * Represents a (possibly ongoing) computation on the server
  *
  * @param client Client on which the computation is going on
  */
class SCSCPClientComputation(client : SCSCPClient, val call_id : String) {

  /**
    * Interrupts this computation on the Server
    */
  def interrupt() = client.interrupt(call_id)

  /**
    * Checks if the result of this computation has been finished on the client side.
 *
    * @return
    */
  def finished() : Boolean = get().nonEmpty

  /**
    * Gets the result of this computation if it is available locally or returns null.
 *
    * @return
    */
  def get() : Option[SCSCPResult] = client.getResult(call_id)

  /**
    * Fetches the result of the computation from the server. Blocks until a result is available.
 *
    * @return
    */
  def fetch(): SCSCPResult = client.fetchResult(call_id)

  def fetchExpression() : OMExpression = {
    fetch() match {
      case SCSCPObjectReturned(expr, _) => expr
      case SCSCPObjectStored(c, _) =>
        client(scscp2(scscp2.retrieve), c).fetchExpression()
      case SCSCPTerminated(_, _) => null
      case SCSCPNothingReturned(_) => null
    }
  }
}

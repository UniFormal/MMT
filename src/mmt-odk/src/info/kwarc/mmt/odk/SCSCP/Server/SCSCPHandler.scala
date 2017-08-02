package info.kwarc.mmt.odk.SCSCP.Server

import info.kwarc.mmt.odk.OpenMath.{OMExpression, OMSymbol}
import info.kwarc.mmt.odk.SCSCP.CD.SymbolSet
import info.kwarc.mmt.odk.SCSCP.Protocol.SCSCPCallArguments

/** A handler for SCSCP based functions */
abstract class SCSCPHandler {
  /** Minimal number of arguments to be passed to this handler. */
  val min : Int
  /** Maximal number of arguments to be passed to this handler. If < 0 assume Infinity */
  val max : Int
  /** The signature of this function, each being allowed to be a set */
  val signature : OMExpression // TODO: Allow either signature
  def handle(client: SCSCPServerClient, arguments: SCSCPCallArguments, parameters: OMExpression*): OMExpression
}

/**
  * A handler that consists of a single function
  * @param h
  */
class LambdaHandler(h : (SCSCPCallArguments, Seq[OMExpression]) => OMExpression, val min : Int = 0, val max : Int = -1, val signature : OMExpression) extends SCSCPHandler {
  def handle(client: SCSCPServerClient, arguments : SCSCPCallArguments, parameters: OMExpression* ) : OMExpression = h(arguments, parameters)
}

/**
  * Implements the getAllowedHeads method
  * @param server
  */
class GetAllowedHeads(server : SCSCPServer) extends SCSCPHandler {
  val min = 0
  val max = 0
  val signature = SymbolSet(Nil)
  def handle(client: SCSCPServerClient, arguments : SCSCPCallArguments, parameters: OMExpression* ) : OMExpression =
    SymbolSet(server.getHandlerNames)
}

/**
  * Implements the get_signature standard procedure specified by the SCSCP spec.
  * @param server the server of the procedure.
  */
class GetSignature(server : SCSCPServer) extends SCSCPHandler {
  override val min: Int = 1
  override val max: Int = 1
  override val signature: OMExpression = SymbolSet(server.getHandlerNames)

  override def handle(client: SCSCPServerClient, arguments: SCSCPCallArguments, parameters: OMExpression*): OMExpression = {
    val symbol = parameters.toList.head
    symbol match {
      case symbol : OMSymbol =>
        server.getHandler(symbol).signature
      case _ =>
        throw new SignatureMismatchException(signature, parameters)
    }
  }
}

class SignatureMismatchException(expected: OMExpression, actual: OMExpression) extends Exception {
  def getExpected = expected
  def getActual = actual
}
package info.kwarc.mmt.odk.SCSCP.Protocol

import info.kwarc.mmt.odk.OpenMath._
import info.kwarc.mmt.odk.SCSCP.CD.scscp1

/**
  * Represents a result returned by an SCSCPConnection
  *
  * @param attributes Attributes representing this result
  */
sealed abstract class SCSCPResult(val attributes: OMAttributionPairs) {
  /**
    * Gets an attribute of the attribution object or null
    *
    * @param attribute OMSymbol referencing attribute to get
    * @return
    */
  def getAttribute(attribute: OMSymbol): OMAnyVal = attributes(attribute).orNull

  /**
    * Gets an attribute within the content dictionary or None
    *
    * @param name Name of the Symbol within the scscp1CD to get
    * @return
    */
  def getAttribute(name: String): OMAnyVal = getAttribute(scscp1(name))

  /**
    * Gets the expression representing this SCSCPResult
    *
    * @return
    */
  def get: OMExpression = data.get

  /**
    * The data contained in this SCSCPResult (if any)
    *
    * @return
    */
  def data: Option[OMExpression]

  /**
    * The OpenMath body of this SCSCPResult
    */
  protected val body: OMApplication

  /**
    * Turns this SCSCPResult into an OpenMath object
    *
    * @return
    */
  def toObject: OMObject = {
    OMObject(
      OMAttribution(attributes, body, None, None)
      , None, None, None)
  }

  /**
    * The call id of this SCSCPResult
    */
  val call_id: String = getAttribute(scscp1.callId) match {
    case OMString(text, _) => text
  }
}

object SCSCPResult {
  /**
    * The symbol referencing completion of a procedure
    */
  val SYMBOL_COMPLETION = scscp1(scscp1.procedureCompleted)

  /**
    * The symbol referencing abortion of a procedure
    */
  val SYMBOL_TERMINATION = scscp1(scscp1.procedureTerminated)

  /**
    * Parses an SCSCPResult from an OpenMath object
    */
  def apply(om: OMObject): SCSCPResult = om.omel match {
    /* computation completed, but we stored it on the server side only */
    case OMAttribution(pairs, OMApplication(`SYMBOL_COMPLETION`, (omr: OMReference) :: Nil, _, _), _, _) =>
      SCSCPObjectStored(omr, pairs)

    /* computation completed and returned */
    case OMAttribution(pairs, OMApplication(`SYMBOL_COMPLETION`, (ome: OMExpression) :: Nil, _, _), _, _) =>
      SCSCPObjectReturned(ome, pairs)

    /* not completed, something went wrong */
    case OMAttribution(pairs, OMApplication(`SYMBOL_TERMINATION`, (ome: OMError) :: Nil, _, _), _, _) =>
      SCSCPTerminated(ome, pairs)

    /* ok, but nothing to be returned */
    case OMAttribution(pairs, OMApplication(`SYMBOL_COMPLETION`, Nil, _, _), _, _) => SCSCPNothingReturned(pairs)
    case OMAttribution(pairs, OMApplication(`SYMBOL_TERMINATION`, Nil, _, _), _, _) => SCSCPNothingReturned(pairs)

    /* else we couldn't parse it*/
    case _ => throw new OpenMathError("")
  }
}

/**
  * A Result that returns the object computed
  *
  * @param result     Expression that has been returned
  * @param attributes Attributes representing this result
  */
case class SCSCPObjectReturned(result: OMExpression, override val attributes: OMAttributionPairs) extends SCSCPResult(attributes) {
  protected val body = OMApplication(SCSCPResult.SYMBOL_COMPLETION, result :: Nil, None, None)

  def data: Option[OMExpression] = Some(result)
}

/**
  * A Result that stores the result object on the server
  *
  * @param omr        Reference to the object on the server
  * @param attributes Attributes representing this result
  */
case class SCSCPObjectStored(omr: OMReference, override val attributes: OMAttributionPairs) extends SCSCPResult(attributes) {
  protected val body = OMApplication(SCSCPResult.SYMBOL_COMPLETION, omr :: Nil, None, None)

  def data: Option[OMExpression] = Some(omr)
}

/**
  * A Result that indicates that the computation has been aborted prematurely
  *
  * @param error      OMError object describing the error in more detail
  * @param attributes Attributes representing this result
  */
case class SCSCPTerminated(error: OMError, override val attributes: OMAttributionPairs) extends SCSCPResult(attributes) {
  protected val body = OMApplication(SCSCPResult.SYMBOL_TERMINATION, error :: Nil, None, None)

  def data: Option[OMExpression] = Some(error)
}

/**
  * A result that indicates that the computation was completed, but there is nothing (to be) returned
  *
  * @param attributes Attributes representing this result
  */
case class SCSCPNothingReturned(override val attributes: OMAttributionPairs) extends SCSCPResult(attributes) {
  protected val body = OMApplication(SCSCPResult.SYMBOL_TERMINATION, Nil, None, None)

  def data: Option[OMExpression] = None
}

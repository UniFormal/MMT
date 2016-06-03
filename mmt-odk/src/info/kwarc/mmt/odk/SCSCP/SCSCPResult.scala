package info.kwarc.mmt.odk.SCSCP

import info.kwarc.mmt.odk.OpenMath._

/**
  * Represents a result of an SCSCP Computation
  */
sealed abstract class SCSCPResult(val attributes : OMAttributionPairs){
  def getAttribute(attribute : OMSymbol): OMNode = attributes(attribute).orNull

  protected val body : OMApplication

  /** Turns this SCSCPResult into an OpenMath object **/
  def toObject : OMObject = {
    OMObject(
      OMAttribution(attributes, body, None, None)
    , None, None, None)
  }

  val call_id : String = getAttribute(scscp1CD(scscp1CD.call_id)).asString.text
}

object SCSCPResult {
  val SYMBOL_COMPLETION = scscp1CD(scscp1CD.procedure_completed)
  val SYMBOL_TERMINATION = scscp1CD(scscp1CD.procedure_terminated)

  def apply(om : OMObject) : SCSCPResult = om.asAttribution match {
    case att @ OMAttribution(pairs, OMApplication(s : OMSymbol, h :: Nil, _, _), _, _) =>
      if(s === SYMBOL_COMPLETION){
        h match {
          case omr : OMReference =>
            SCSCPObjectStored(omr, pairs)
          case omo : OMObject =>
            SCSCPObjectReturned(omo, pairs)
        }
      } else if (s === SYMBOL_TERMINATION) {
        h match {
          case e : OMError => SCSCPTerminated(e, pairs)
        }
      } else {
        throw new MatchError(att)
      }
  }
}

/**
  * A result returning an SCSCP object
  */
case class SCSCPObjectReturned(result : OMObject, override val attributes : OMAttributionPairs) extends SCSCPResult(attributes) {
  protected val body = OMApplication(SCSCPResult.SYMBOL_COMPLETION, result :: Nil, None, None)
}
case class SCSCPObjectStored(omr : OMReference, override val attributes : OMAttributionPairs) extends SCSCPResult(attributes) {
  protected val body = OMApplication(SCSCPResult.SYMBOL_COMPLETION, omr :: Nil, None, None)
}
case class SCSCPTerminated(error : OMError, override val attributes : OMAttributionPairs) extends SCSCPResult(attributes) {
  protected val body = OMApplication(SCSCPResult.SYMBOL_TERMINATION,  error :: Nil, None, None)
}
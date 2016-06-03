package info.kwarc.mmt.odk.SCSCP.Protocol

import info.kwarc.mmt.odk.OpenMath._
import info.kwarc.mmt.odk.SCSCP.CD.scscp1


sealed case class SCSCPCall(procedure : OMSymbol, arguments : SCSCPCallArguments, parameters : OMExpression*){
  def toOMObject : OMObject = {
    // procedure call symbol
    val pcs = scscp1(scscp1.procedureCall)

    // make the function call object
    val function_call = OMApplication(procedure, parameters.toList, None, None)

    // make the procedure call object
    val procedure_call = OMApplication(pcs, function_call :: Nil, None, None)

    // make the arrtributation
    val procdure_attribution = OMAttribution(arguments.toPairs, procedure_call, None, None)

    // and finally wrap it in an object
    OMObject(procdure_attribution, None, None, None)
  }
}

/**
  * Represents options that can be parsed to a computation request
  *
  * @param call_id Call Identifier
  * @param return_method Method of return
  * @param others Other system-specific options captured by an OMAttribution call
  */
sealed case class SCSCPCallArguments(call_id : String, return_method : Option[SCSCPReturnMethod], others : OMAttributionPairs) {

  def toPairs : OMAttributionPairs = {
    val cid_others = (scscp1(scscp1.callId), OMString(call_id, None)) :: (if(others == null){Nil} else {others.pairs})

    OMAttributionPairs(if(return_method.isDefined){
      (scscp1(return_method.get.name), OMString("", None)) :: cid_others
    } else {
      cid_others
    }, None, None)
  }
}


sealed abstract class SCSCPReturnMethod {
  def name : String
}
case object SCSCPReturnObject extends SCSCPReturnMethod {
  def name = scscp1.optionReturnObject
}
case object SCSCPReturnCookie extends SCSCPReturnMethod {
  def name = scscp1.optionReturnNothing
}
case object SCSCPReturnNothing extends SCSCPReturnMethod {
  def name = scscp1.optionReturnNothing
}
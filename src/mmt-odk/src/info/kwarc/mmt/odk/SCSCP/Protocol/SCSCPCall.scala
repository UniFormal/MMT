package info.kwarc.mmt.odk.SCSCP.Protocol

import info.kwarc.mmt.api.ImplementationError
import info.kwarc.mmt.odk.OpenMath._
import info.kwarc.mmt.odk.SCSCP.CD.scscp1


sealed case class SCSCPCall(procedure: OMSymbol, arguments: SCSCPCallArguments, parameters: OMExpression*) {
  def toOMObject: OMObject = {
    // procedure call symbol
    val pcs = scscp1(scscp1.procedureCall)

    // make the function call object
    val function_call = OMApplication(procedure, parameters.toList, None, None)

    // make the procedure call object
    val procedure_call = OMApplication(pcs, function_call :: Nil, None, None)

    // make the arrtributation
    val procedure_attribution = OMAttribution(arguments.toPairs, procedure_call, None, None)

    // and finally wrap it in an object
    OMObject(procedure_attribution, None, None, None)
  }
}

object SCSCPCall {
  def parse(node: OMAny): SCSCPCall = {
    node match {
      case OMObject(
        OMAttribution(
          arguments,
          OMApplication(
            pcs,
            OMApplication(
              procedure : OMSymbol,
              parameters,
              _,
              _
            )
            :: Nil,
            _,
            _
          ),
          _,
          _
        )
      , _, _, _) if pcs == scscp1(scscp1.procedureCall) => SCSCPCall(
        procedure,
        SCSCPCallArguments.parse(arguments),
        parameters : _*
      )
      case _ => throw new Exception()
    }
  }
}

/**
  * Represents options that can be parsed to a computation request
  *
  * @param call_id       Call Identifier
  * @param return_method Method of return
  * @param others        Other system-specific options captured by an OMAttribution call
  */
sealed case class SCSCPCallArguments(call_id: String, return_method: Option[SCSCPReturnMethod], others: OMAttributionPairs) {

  def toPairs: OMAttributionPairs = {
    val cid_others = (scscp1(scscp1.callId), OMString(call_id, None)) :: (if (others == null) {
      Nil
    } else {
      others.pairs
    })

    OMAttributionPairs(if (return_method.isDefined) {
      (scscp1(return_method.get.name), OMString("", None)) :: cid_others
    } else {
      cid_others
    }, None, None)
  }
}

object SCSCPCallArguments {
  def parse(node: OMAny): SCSCPCallArguments = node match {
    case pairs: OMAttributionPairs =>
      // the id of the call
      val call_id = pairs(scscp1(scscp1.callId)) match {
        case Some(OMString(s, _)) => s
        case _ => throw ImplementationError("invalid call arguments: must contain a string")
      }

      // the return method
      val return_method = {
        if (pairs(scscp1(SCSCPReturnObject.name)).isDefined) {
          Some(SCSCPReturnObject)
        } else if (pairs(scscp1(SCSCPReturnCookie.name)).isDefined) {
          Some(SCSCPReturnCookie)
        } else if (pairs(scscp1(SCSCPReturnNothing.name)).isDefined) {
          Some(SCSCPReturnNothing)
        } else {
          None
        }
      }

      // and return the call arguments
      SCSCPCallArguments(call_id, return_method, OMAttributionPairs(pairs.pairs, None, None))
    case _ => throw new Exception()
  }
}


sealed abstract class SCSCPReturnMethod {
  def name: String
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

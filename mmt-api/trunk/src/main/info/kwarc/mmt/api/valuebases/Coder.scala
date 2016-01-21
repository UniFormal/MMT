package info.kwarc.mmt.api.valuebases

import info.kwarc.mmt.api._
import objects._

/**
 * encodes/decodes terms using a set of [[Codec]]s and [[CodecOperator]]s 
 */
class Coder[Code](codecs: List[Codec[Code]], operators: List[CodecOperator[Code]]) {
   
   def buildCodec(codecExp: Term): Codec[Code] = codecExp match {
      case OMA(OMS(op), pars) =>
         val cop = operators.find(_.id == op).getOrElse {
            throw GeneralError("codec not found: " + op)
         }
         if (pars.length != cop.numberOfTypeParameters)
            throw CodecNotApplicable
         val parsC = pars map buildCodec
         cop(parsC:_*)
      case OMS(op) =>
         codecs.find(_.exp == OMS(op)).getOrElse {
            throw GeneralError("codec not found: " + op)
         }
   }
}


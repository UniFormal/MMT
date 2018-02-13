package info.kwarc.mmt.api.valuebases

import info.kwarc.mmt.api._
import objects._

/**
 * encodes/decodes terms using a set of [[Codec]]s and [[CodecOperator]]s
 */
class Coder[Code](codecs: List[Codec[Code]], operators: List[CodecOperator[Code]]) {

   def destruct(t: Term) : Option[(GlobalName,List[Term])] = t match {
      case OMA(OMS(op), pars) => Some((op, pars))
      case _ => None
   }

   private object Matcher {
      def unapply(t: Term) = destruct(t)
   }

   def buildCodec(codecExp: Term): Codec[Code] = codecExp match {
      case Matcher(op, pars) =>
         val cop = operators.find(_.id == op).getOrElse {
            throw GeneralError("codec not found: " + op)
         }
         val numPars = pars.length
         val tpars = cop.typeParameterPositions map {i =>
            if (i < 1 || i > numPars)
               throw CodecNotApplicable
            pars(i-1)
         }
         val tparsC = tpars map buildCodec
         cop(tparsC:_*)
      case OMS(op) =>
         codecs.find(_.exp == OMS(op)).getOrElse {
            throw GeneralError("codec not found: " + op)
         }
      case t => throw GeneralError("codec not found: " + t)
   }
}


package  info.kwarc.mmt.sequences

import info.kwarc.mmt.lf._
import NatRules._

import info.kwarc.mmt.api._
import uom._
import frontend._
import objects._

/** matches argument list without sequences preceded by a NatLit */
object NoSeqs {
  def unapply(as: List[Term]) = as match {
    case NatLit(n) :: as if as.length == n => Some(as)
    case _ => None
  }
  def apply(as: List[Term]) = NatLit(as.length) :: as
}


/** takes flexary composition operator op: {n} a^n -> a  and implements associativity and related simplifications */
class Association(monoid: Boolean, semilattice: Boolean) extends ParametricRule {
  def apply(controller: Controller, home: Term, args: List[Term]) = {
    val (op,neutOpt) = args match {
      case OMS(o) :: Nil if !monoid => (o,None)
      case OMS(o) :: n :: Nil if monoid => (o,Some(n))
      case _ => throw ParseError("arguments must be identifier and (if monoid) term")
    }
    // TODO check type of op
    new SimplificationRule(op) {
      def apply(c: Context, t: Term) = t match {
        case ApplySpine(OMS(o), NoSeqs(l)) =>
            var lF = l.flatMap {
              case t if neutOpt contains t => Nil // neutrality
              case ApplySpine(OMS(op), NoSeqs(xs)) => xs // associativity
              case x => List(x)
            }
            if (semilattice)
              lF = lF.distinct // idempotence (in the presence of commutativity)
            val nF = lF.length
            if (neutOpt.isDefined && nF == 0) Simplify(neutOpt.get)
            else if (nF == 1) Simplify(lF.head)
            else if (nF != l.length) Simplify(ApplySpine(OMS(op), NoSeqs(lF) :_*))
            else Recurse
        case _ =>
          //TODO we can still do something in this case
          Simplifiability.NoRecurse
      }
    }
  }
}

/** additionally takes a neutral element */
object Monoid extends Association(true, false)

/** eliminates duplicates among the arguments */
object Semilattice extends Association(false, true)

/** combines monoid and semilattice laws */
object BoundedSemilattice extends Association(true, true)

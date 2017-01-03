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


/** takes flexary composition operator op: {n} a^n -> a and its neutral element and yields BreadthRule that applies monoid laws */
object Monoidal extends ParametricRule {
  def apply(controller: Controller, home: Term, args: List[Term]) = {
    val (op,neut) = args match {
      case OMS(o) :: n :: Nil => (o,n)
      case _ => throw ParseError("arguments must be identifier and term") 
    }
    // TODO check type of op
    new BreadthRule(op) {
      val apply: Rewrite = as => {
        as match {
          case NoSeqs(l) =>
            val lF = l.flatMap {
              case t if t == neut => Nil // neutrality
              case ApplySpine(OMS(this.head), NoSeqs(xs)) => xs // associativity
              case x => List(x)
            }
            val nF = lF.length
            if (nF == 0) GlobalChange(neut)
            else if (nF == 1) GlobalChange(lF.head)
            else if (nF != l.length) LocalChange(NoSeqs(lF))
            else NoChange
          case _ => NoChange
        }
      }
    }
  }
}
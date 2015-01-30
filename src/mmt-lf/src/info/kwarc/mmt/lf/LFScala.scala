package info.kwarc.mmt.lf

import info.kwarc.mmt.api._
import uom._
import objects._

class UnaryLFConstantScala(val parent: MPath, val name: String) extends ConstantScala {
   def apply(arg: Term) = Apply(OMS(path), arg)
   def unapply(t: Term) = t match {
      case ApplySpine(OMS(this.path), List(a)) => Some(a)
      case _ => None
   }
}
class BinaryLFConstantScala(val parent: MPath, val name: String) extends ConstantScala {
   def apply(arg1: Term, arg2: Term) = ApplySpine(OMS(path), arg1, arg2)
   def unapply(t: Term) = t match {
      case ApplySpine(OMS(this.path), List(a1, a2)) => Some((a1,a2))
      case _ => None
   }
}


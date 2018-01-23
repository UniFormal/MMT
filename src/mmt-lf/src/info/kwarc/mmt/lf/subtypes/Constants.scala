package info.kwarc.mmt.lf.subtypes

import info.kwarc.mmt.lf._
import info.kwarc.mmt.api._
import utils._
import objects._

object Subtyping {
   val _base = Typed._base
   val _path = _base ? "Subtyping"

   object Sub {
      val path = _path ? "Sub"
      def apply(a: Term, b: Term) = ApplySpine(OMS(path), a, b)
      def unapply(t: Term) = t match {
         case ApplySpine(OMS(this.path), List(a, b)) => Some(a, b)
         case _ => None
      }
   }
}

object PredicateSubtypes {
   val _base = Typed._base
   val _path = _base ? "PredicateSubtypes"

   object Sub {
      val path = _path ? "Sub"
      def apply(a: Term, x: LocalName, p: Term) = ApplySpine(OMS(path), a, Lambda(x, OMS(Typed.ktype), p))
      def unapply(t: Term) = t match {
         case ApplySpine(OMS(this.path), List(a, Lambda(x, OMS(Typed.ktype), p))) => Some(a, x, p)
         case _ => None
      }
   }
}

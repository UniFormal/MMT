package info.kwarc.mmt.lf.inhabitation

import info.kwarc.mmt.lf._
import info.kwarc.mmt.api._
import utils._
import objects._

object Inhabitation {
   val _base = Typed._base
   val _path = _base ? "Inhabitation"
   
   object Inh {
      val path = _path ? "Inh"
      def apply(a: Term) = Apply(OMS(path), a)
      def unapply(t: Term) = t match {
         case Apply(OMS(this.path), a) => Some(a)
         case _ => None
      }
   }
}
package info.kwarc.mmt.sequences

import info.kwarc.mmt.lf._
import info.kwarc.mmt.api._
import utils._
import objects._

object LFS {
   val _base = Typed._base
   val _path = _base ? "LFS"
   
   val nat   = _path ? "nat"
   val zero  = _path ? "zero"
   val one   = _path ? "one"
   //val plus = lfssymbol("plus")
   //val minus = lfssymbol("minus")
   //val times = lfssymbol("times")
   
   object succ {
      val path = _path ? "succ"
      def apply(n: Term) = OMA(OMS(path), List(n))
      def unapply(t: Term) = t match {
         case OMA(OMS(this.path), List(n)) => Some(n)
         case _ => None
      }
   }
   object leq {
      val path = _path ? "leq"
      def apply(m: Term, n: Term) = OMA(OMS(path), List(m,n))
      def unapply(t: Term) = t match {
         case OMA(OMS(this.path), List(m,n)) => Some((m,n))
         case _ => None
      }
   }
   
   object ntype {
      val path = _path ? "ntype"
      def apply(n: Term) = OMA(OMS(path), List(n))
      def unapply(t: Term) = t match {
         case OMA(OMS(this.path), List(n)) => Some(n)
         case _ => None
      }
   }
   
   object index {
      val path = _path ? "index"
      def apply(s: Term, n: Term) = OMA(OMS(path), List(s,n))
      def unapply(t: Term) = t match {
         case OMA(OMS(this.path), List(s,n)) => Some((s,n))
         case _ => None
      }
   }
   
   object ellipsis {
      val path = _path ? "ellipsis"
      def apply(from: Term, to: Term, index: LocalName, body: Term): Term = ???
      def unapply(t: Term): Option[(Term, Term, LocalName, Term)] = ???
   }
}
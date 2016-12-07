package info.kwarc.mmt.sequences

import info.kwarc.mmt.lf._
import info.kwarc.mmt.api._
import utils._
import objects._
import uom._

import Nat._

object TypeSequences {
   val _base = Typed._base
   val _path = _base ? "TypeSequences"
   
   object ntype extends UnaryConstantScala(_path, "ntype")
}

object LFS {
   val _base = Typed._base
   val _path = _base ? "LFS"
   
   object index extends BinaryConstantScala(_path, "index")

   object ellipsis {
      val path = _path ? "ellipsis"
      def apply(from: Term, to: Term, index: LocalName, body: Term): Term =
         ComplexTerm(path, Substitution(OMV("from")/from, OMV("to")/to), Context(OMV(index) % OMS(nat)), List(body))
      def unapply(t: Term): Option[(Term, Term, LocalName, Term)] = t match {
         case ComplexTerm(this.path,
                Substitution(Sub(_,from),Sub(_,to)),
                Context(VarDecl(index, Some(OMS(nat)), None, _)),
                List(body)
              ) => Some((from, to, index, body))
         case _ => None
      }
   }
   
   object flatseq extends FlexaryConstantScala(_path, "flatseq")
}
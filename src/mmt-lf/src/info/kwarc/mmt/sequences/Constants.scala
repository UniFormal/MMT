package info.kwarc.mmt.sequences

import info.kwarc.mmt.lf._
import info.kwarc.mmt.api._
import utils._
import objects._
import uom._

object LFS {
   val _base = Typed._base
   val _path = _base ? "LFS"
   
   val nat   = _path ? "nat"
   val zero  = _path ? "zero"
   val one   = _path ? "one"
   
   object succ  extends UnaryConstantScala(_path, "succ")
   object leq   extends BinaryConstantScala(_path, "leq")
   object plus  extends BinaryConstantScala(_path, "plus")
   object minus extends BinaryConstantScala(_path, "minus")
   object ntype extends UnaryConstantScala(_path, "ntype")
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
   
   object natlit extends info.kwarc.mmt.api.objects.StandardInt
   natlit.init(nat, DPath(URI("http","real.omdoc.org")) ? "StandardNat")
}
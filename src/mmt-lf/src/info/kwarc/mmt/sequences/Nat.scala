package info.kwarc.mmt.sequences

import info.kwarc.mmt.api._
import checking._
import objects._
import objects.Conversions._
import utils.URI

import info.kwarc.mmt.lf._

object Bool {
   val _base = Typed._base
   val _boolpath = _base ? "DHOL"
   val _dedpath = _base ? "Ded"
   object ded   extends UnaryLFConstantScala(_dedpath, "DED")
   val trueo = _base ? "Bool" ? "TRUE"
   val falseo = _base ? "Bool" ? "FALSE"
}

object Nat {
   val _base = Typed._base
   val _sympath = _base ? "NatSymbols"
   val _arithpath = _base ? "NatArith"
   val _relpath = _base ? "NatRels"

   val nat   = _sympath ? "NAT"
   val zero  = _sympath ? "zero"
   val one   = _arithpath ? "one"

   object succ  extends UnaryLFConstantScala(_arithpath, "succ")
   object leq   extends BinaryLFConstantScala(_relpath, "LEQ")

   /** a < b : type */
   def lessType(a: Term, b: Term) = Bool.ded(leq(succ(a),b))
}

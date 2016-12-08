package info.kwarc.mmt.sequences

import info.kwarc.mmt.api._
import checking._
import objects._
import objects.Conversions._
import utils.URI

import info.kwarc.mmt.lf._

object Nat {
   val _base = Typed._base
   val _path = _base ? "NatSymbols"

   val nat   = _path ? "nat"
   val zero  = _path ? "zero"
   val one   = _path ? "one"
   
   object succ  extends UnaryLFConstantScala(_path, "succ")
   object leq   extends BinaryLFConstantScala(_path, "leq")
   val natlit = new uom.RepresentedRealizedType(OMS(nat), uom.StandardNat)
}
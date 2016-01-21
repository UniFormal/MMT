package info.kwarc.mmt.sequences

import info.kwarc.mmt.api._
import checking._
import objects._
import objects.Conversions._
import utils.URI

import info.kwarc.mmt.lf._

object Nat {
   val _base = DPath(utils.URI("http", "cds.omdoc.org") / "examples")
   val _path = _base ? "Nat"

   val nat   = _path ? "nat"
   val zero  = _path ? "zero"
   val one   = _path ? "one"
   
   object succ  extends UnaryLFConstantScala(_path, "succ")
   object leq   extends BinaryLFConstantScala(_path, "leq")
   object plus  extends BinaryLFConstantScala(_path, "plus")
   object minus extends BinaryLFConstantScala(_base ? "NatMinus", "minus")
   val natlit = new uom.RepresentedRealizedType(OMS(nat), uom.StandardNat)
}
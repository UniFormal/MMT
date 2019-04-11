package info.kwarc.mmt.itp

import info.kwarc.mmt.lf._
import info.kwarc.mmt.api._
import utils._
import objects._
import uom._

object ITP {
   val _base = Typed._base / ".." / "examples"
   val _path = _base ? "ITP"

   object proof extends FlexaryConstantScala(_path, "proof")

   object assume extends BinaryConstantScala(_path, "assume")   
}

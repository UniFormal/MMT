package info.kwarc.mmt.pvs

import info.kwarc.mmt.api._
import utils._

object PVSTheory {
   val dpath = DPath(URI.http colon "pvs.csl.sri.com")
   val name = "PVS"
   val path = dpath ? name
   def sym(s: String) = path ? s
   val fun = sym("fun")
   val prod = sym("prod")
   val tuple = sym("tuple")
   val predsub = sym("predsub")
   val union = sym("union")
   val asType = sym("asType")
}
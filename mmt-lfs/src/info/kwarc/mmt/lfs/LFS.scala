package info.kwarc.mmt.lfs

import info.kwarc.mmt.api._
import utils._

object LFS {
   val baseURI = URI("http", "cds.omdoc.org")
   val lfsbase = DPath(baseURI / "omdoc" / "seq.omdoc")
   val lfscd = lfsbase ? "mmt"
   def lfssymbol(name : String) = lfscd ? name
   //val jokerbinder = lfssymbol("joker")
   //val ellipsis = lfssymbol("ellipsis")
   val index = lfssymbol("index")
   val seqmap = lfssymbol("seqmap")   
   val seq = lfssymbol("seq")
   val nat = lfssymbol("nat")
   val plus = lfssymbol("plus")
   val minus = lfssymbol("minus")
   val times = lfssymbol("times")   
}
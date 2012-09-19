package info.kwarc.mmt.api.utils
import info.kwarc.mmt.api._

object mmt {
   val baseURI = URI("http", "cds.omdoc.org")
   val mmtbase = DPath(baseURI / "omdoc" / "mmt.omdoc")
   val mmtcd = mmtbase ? "mmt"
   def mmtsymbol(name : String) = mmtcd ? name
   val mmttype = mmtsymbol("type")
   val mmtdef = mmtsymbol("definiens")
   val identity = mmtsymbol("identity")
   val composition = mmtsymbol("composition")
   val morphismapplication = mmtsymbol("morphismapplication")
   val substitutionapplication = mmtsymbol("substitutionapplication")
   val tempty = mmtsymbol("theory-empty")
   val mempty = mmtsymbol("morphism-empty")
   val tunion = mmtsymbol("theory-union")
   val munion = mmtsymbol("morphism-union")
   val unknowns = mmtsymbol("unknowns")
/* val poii = mmtsymbol("theory-union")
   val poiim = mmtsymbol("morphism-union")
   val poi = mmtsymbol("theory-with")
   val poim = mmtsymbol("morphism-with")
   val poiw = mmtsymbol("morphism-extend") */
/*
   val jokerbinder = mmtsymbol("joker")
   val ellipsis = mmtsymbol("ellipsis")
   val index = mmtsymbol("index")
   val seqmap = mmtsymbol("seqmap")   
   val seq = mmtsymbol("seq")
   val nat = mmtsymbol("nat")
   val plus = mmtsymbol("plus")
   val minus = mmtsymbol("minus")
   val times = mmtsymbol("times")*/
   val explmorph = mmtsymbol("explicit morphism")
   val functorapplication = mmtsymbol("functorapplication")
   val mimeBase = DPath(URI("http://www.iana.org/assignments/media-types/"))
}

object OpenMath {
   val base = DPath(URI("http", "www.openmath.org") / "cd")
   val arith1CD = base ? "arith1"
   object arith1 {
      val plus = arith1CD ? "plus"
      val minus = arith1CD ? "minus"
      val times = arith1CD ? "times"
   }
}
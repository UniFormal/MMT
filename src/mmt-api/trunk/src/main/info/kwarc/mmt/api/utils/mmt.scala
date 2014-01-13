package info.kwarc.mmt.api.utils
import info.kwarc.mmt.api._

object mmt {
   val baseURI = URI("http", "cds.omdoc.org")
   val mmtbase = DPath(baseURI / "mmt")
   val mmtcd = mmtbase ? "mmt"
   def mmtsymbol(name : String) = mmtcd ? name
   val mmttype = mmtsymbol("type")
   val mmtdef = mmtsymbol("definiens")
   val brackets = mmtsymbol("brackets")
   val context = mmtsymbol("context")
   val identity = mmtsymbol("identity")
   val composition = mmtsymbol("composition")
   val inferedTypeTag = mmtsymbol("infered")
   val morphismapplication = mmtsymbol("morphismapplication")
   val substitutionapplication = mmtsymbol("substitutionapplication")
   val mempty = mmtsymbol("morphism-empty")
   val tunion = mmtsymbol("theory-union")
   val munion = mmtsymbol("morphism-union")
   val unknowns = mmtsymbol("unknowns")
/* val poii = mmtsymbol("theory-union")
   val poiim = mmtsymbol("morphism-union")
   val poi = mmtsymbol("theory-with")
   val poim = mmtsymbol("morphism-with")
   val poiw = mmtsymbol("morphism-extend") */
   val explmorph = mmtsymbol("explicitmorphism")
   val label = mmtbase ? "label"
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
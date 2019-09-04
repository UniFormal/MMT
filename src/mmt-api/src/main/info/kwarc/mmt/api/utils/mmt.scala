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
   val andrewsDot = mmtsymbol("andrewsDot")
   val andrewsDotRight = mmtsymbol("andrewsDotRight")
   val context = mmtsymbol("context")
   val unknowns = mmtsymbol("unknowns")
   val label = mmtbase ? "label"
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

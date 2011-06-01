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
   val emptymorphism = mmtsymbol("emptymorphism")
/* val poii = mmtsymbol("theory-union")
   val poiim = mmtsymbol("morphism-union")
   val poi = mmtsymbol("theory-with")
   val poim = mmtsymbol("morphism-with")
   val poiw = mmtsymbol("morphism-extend") */
   val jokerbinder = mmtsymbol("joker")
   val repetition = mmtsymbol("repetition")
   val index = mmtsymbol("index")
   val ellipsis = mmtsymbol("ellipsis")
   val nat = mmtsymbol("nat")
   val seq = mmtsymbol("seq")
   val mimeBase = DPath(URI("http://www.iana.org/assignments/media-types/"))
}
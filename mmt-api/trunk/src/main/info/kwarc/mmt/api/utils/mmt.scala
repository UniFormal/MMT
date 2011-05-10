package info.kwarc.mmt.api.utils
import info.kwarc.mmt.api._

object mmt {
   val baseURI = new xml.URI("http", "cds.omdoc.org", "/", null)
   val mmtbase = DPath(baseURI.resolve("omdoc/mmt.omdoc"))
   val mmtcd = mmtbase ? "mmt"
   def mmtsymbol(name : String) = mmtcd ? name
   val mmttype = mmtsymbol("type")
   val mmtdef = mmtsymbol("definiens")
   val identity = mmtsymbol("identity")
   val composition = mmtsymbol("composition")
   val morphismapplication = mmtsymbol("morphismapplication")
   val emptymorphism = mmtsymbol("emptymorphism")
/* val poii = mmtsymbol("theory-union")
   val poiim = mmtsymbol("morphism-union")
   val poi = mmtsymbol("theory-with")
   val poim = mmtsymbol("morphism-with")
   val poiw = mmtsymbol("morphism-extend") */
   val jokerbinder = mmtsymbol("joker")
   val repetition = mmtsymbol("repetition")
   val index = mmtsymbol("index")
   val seq = mmtsymbol("seq")
   val nat = mmtsymbol("nat")
   val mimeBase = DPath(new xml.URI("http://www.iana.org/assignments/media-types/"))
}
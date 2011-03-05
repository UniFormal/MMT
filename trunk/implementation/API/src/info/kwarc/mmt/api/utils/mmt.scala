package info.kwarc.mmt.api.utils
import jomdoc._
import jomdoc.objects.{OMS}

object mmt {
   val baseURI = new xml.URI("http", "cds.omdoc.org", "/", null)
   val mmtbase = DPath(baseURI.resolve("omdoc/mmt.omdoc"))
   val mmtcd = mmtbase ? "mmt"
   def mmtsymbol(name : String) = OMS(mmtcd ? name)
   val mmttype = mmtsymbol("type")
   val mmtdef = mmtsymbol("definiens")
   val identity = mmtsymbol("identity")
   val composition = mmtsymbol("composition")
   val morphismapplication = mmtsymbol("morphismapplication")
   val jokerbinder = mmtsymbol("joker")
   val repetition = mmtsymbol("repetition")
   val index = mmtsymbol("index")
   val mimeBase = DPath(new xml.URI("http://www.iana.org/assignments/media-types/"))
}
package info.kwarc.mmt.LFX.Records

import info.kwarc.mmt.api.DPath
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.utils.URI
import info.kwarc.mmt.api.utils._
import info.kwarc.mmt.LFX.LFX

object Records {
  val baseURI = LFX.ns / "Records"
  val thname = "Symbols"
  val path = baseURI ? thname
  def lfrecsymbol(name : String) = path ? name
}

class LFRecSymbol(name:String) {
  val path = Records.path ? name
  val term = OMS(path)
}

object Rectype extends LFRecSymbol("Rectype") {
  def apply(v:OML*) = OMA(this.term, v.toList)
  def apply(con: Context) = OMA(this.term, con map {v => v.toOML})
  def unapply(t : Term) : Option[List[OML]] = t match {
    case OMA(this.term,ls) => Some(ls map {_ match {case o:OML => o case _ => return None}})
    case this.term => Some(Nil)
    case _ => None
  }
}

object Recexp extends LFRecSymbol("Recexp") {
  def apply(v:OML*) = OMA(this.term, v.toList)
  def apply(con: Context) = OMA(this.term, con map {v => v.toOML})
  def unapply(t : Term) : Option[List[OML]] = t match {
    case OMA(this.term,ls) => Some(ls map {_ match {case o:OML => o case _ => return None}})
    case this.term => Some(Nil)
    case _ => None
  }
}

object Getfield extends LFRecSymbol("Getfield") {
  def apply(t:Term,v:OML) = OMA(this.term,List(t,v))
  def unapply(t : Term) : Option[(Term,OML)] = t match {
    case OMA(this.term,List(tm, v)) => Some((tm,v match {case o:OML => o case _ => return None}))
    case _ => None
  }
}

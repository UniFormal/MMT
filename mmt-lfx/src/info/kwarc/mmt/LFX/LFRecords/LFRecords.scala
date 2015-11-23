package info.kwarc.mmt.LFX.LFRecords

import info.kwarc.mmt.api.DPath
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.utils.URI
import info.kwarc.mmt.api.utils._

object LFRecords {
  val baseURI = DPath(URI.http colon "cds.omdoc.org") / "LFX"
  val thname = "Records"
  val path = baseURI ? thname
  def lfrecsymbol(name : String) = path ? name
}

class LFRecSymbol(name:String) {
  val path = LFRecords.path ? name
  val term = OMS(path)
}

object Rectype extends LFRecSymbol("Rectype") {
  def apply(v:Term*) = OMA(this.term, v.toList)
  def apply(con: Context) = OMA(this.term, con map {v => OML(v)})
  def unapply(t : Term) : Option[List[OML]] = t match {
    case OMBIND(this.term,con,_) => Some(con map {v => OML(v)})
    case _ => None
  }
}

object Recexp extends LFRecSymbol("Recexp") {
  def apply(v:OML*) = OMA(this.term, v.toList)
  def apply(con: Context) = OMA(this.term, con map {v => OML(v)})
  def unapply(t : Term) : Option[List[OML]] = t match {
    case OMBIND(this.term,con,_) => Some(con map {v => OML(v)})
    case _ => None
  }
}

object Getfield extends LFRecSymbol("Getfield") {
  def apply(t:Term,v:OML) = OMA(this.term,List(t,v))
  def unapply(t : Term) : Option[(Term,OML)] = t match {
    case OMBIND(this.term,Context(v),tm) => Some((tm,OML(v)))
    case _ => None
  }
}
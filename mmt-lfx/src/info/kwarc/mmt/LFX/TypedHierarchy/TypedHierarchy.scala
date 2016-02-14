package info.kwarc.mmt.LFX.TypedHierarchy

import info.kwarc.mmt.LFX.NatLiterals
import info.kwarc.mmt.api.objects.{Term, OMA, OMS}
import info.kwarc.mmt.api.{LocalName, utils, DPath}
import info.kwarc.mmt.lf.{Typed, LF}

/**
  * Created by raupi on 08.02.16.
  */
object TypedHierarchy {
  val _base = DPath(utils.URI("http", "cds.omdoc.org") / "LFX")
  val path = _base ? "TypedHierarchy"
}

class Symbol(s:String) {
  val path = TypedHierarchy.path ? s
  val term = OMS(path)
}

object TypeLevel extends Symbol("TypeLevel") {
  def apply(i : BigInt) = {
    require(i>=0)
    OMA(this.term,List(NatLiterals(i)))
  }
  def unapply(t:Term) : Option[BigInt] = t match {
    case OMA(this.term,List(NatLiterals(i))) => i match {
      case i:BigInt => Some(i)
      case _ => None
    }
    case _ => None
  }
}

object TypeUniverse extends Symbol("universe") {
  val altpath = Typed.ktype
  def unapply(t:Term) : Boolean = t match {
    case this.term => true
    case OMS(p) if p == altpath => true
    case _ => false
  }
}
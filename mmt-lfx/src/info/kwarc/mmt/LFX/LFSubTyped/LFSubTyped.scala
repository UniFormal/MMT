package info.kwarc.mmt.LFX.LFSubTyped

import info.kwarc.mmt.api.symbols.FinalConstant
import info.kwarc.mmt.api.{StructuralElement, DPath}
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.parser.{ParserState, KeywordBasedParser, ParserExtension}
import info.kwarc.mmt.api.utils.URI

object LFSubTyped {
  val baseURI = DPath(URI.http colon "cds.omdoc.org") / "LFX"
  val thname = "Sub"
  val path = baseURI ? thname
  def lfsubsymbol(name : String) = path ? name
}

class LFSubSymbol(name:String) {
  val path = LFSubTyped.path ? name
  val term = OMS(path)
}

object subtypeOf extends LFSubSymbol("subtypeOf") {
  def apply(t : Term) = OMA(OMID(path),List(t))
  def unapply(t : Term) : Option[Term] = t match {
    case OMA(OMID(this.path), List(a)) => Some(a)
    case _ => None
  }
}
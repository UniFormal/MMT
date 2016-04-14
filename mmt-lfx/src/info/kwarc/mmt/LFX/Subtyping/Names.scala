package info.kwarc.mmt.LFX.Subtyping

import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.LFX.LFX

object SubTyped {
  val baseURI = LFX.ns / "Subtyping"
  val thname = "SubSymbol"
  val path = baseURI ? thname
  def lfsubsymbol(name : String) = path ? name
  val judgpath = baseURI ? "JudgmentSymbol"
  val predpath = baseURI ? "PredSubSymbols"
}

class LFSubSymbol(name:String) {
  val path = SubTyped.path ? name
  val term = OMS(path)
}

class LFSubJudgSymbol(name:String) {
  val path = SubTyped.judgpath ? name
  val term = OMS(path)
}

class LFPredSubSymbol(name:String) {
  val path = SubTyped.predpath ? name
  val term = OMS(path)
}

object subtypeOf extends LFSubSymbol("subtypeOf") {
  def apply(t : Term) = OMA(OMID(path),List(t))
  def unapply(t : Term) : Option[Term] = t match {
    case OMA(OMID(this.path), List(a)) => Some(a)
    case _ => None
  }
}

object subtypeJudg extends LFSubJudgSymbol("subtypeJudge") {
  def apply(t1 : Term, t2 : Term) = OMA(term,List(t1,t2))
  def unapply(t : Term) : Option[(Term,Term)] = t match {
    case OMA(this.term,List(t1,t2)) => Some(t1,t2)
    case _ => None
  }
}

object predsubtp extends  LFPredSubSymbol("predsubtp") {
  def apply(tp : Term, pred:Term) = OMA(this.term,List(tp,pred))
  def unapply(t : Term) : Option[(Term,Term)] = t match {
    case OMA(this.term,List(a,b)) => Some((a,b))
    case _ => None
  }
}

object PredOf extends LFPredSubSymbol("PredOf") {
  def apply(tm : Term) = OMA(this.term,List(tm))
  def unapply(t : Term) : Option[Term] = t match {
    case OMA(this.term,List(a)) => Some(a)
    case _ => None
  }
}

package info.kwarc.mmt.LFX.Coproducts

import info.kwarc.mmt.api.{LocalName, DPath}
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.LFX.LFX

object Coproducts {
  val baseURI = LFX.ns / "Coproducts"
  val thSymbols = "Symbols"
  //val th2name = "LFCoprod"
  val path = baseURI ? thSymbols
  def symbol(name : String) = path ? name
}

class CoprodSymbol(name:String) {
  val path = Coproducts.path ? name
  val term = OMS(path)
}

object Coprod extends CoprodSymbol("Coprod") {
  def apply(tp1:Term,tp2:Term) = OMA(this.term,List(tp1,tp2))
  def apply(t:Term*) = OMA(this.term,t.toList)
  def unapply(t:Term) : Option[(Term,Term)] = t match {
    case OMA(this.term,List(t1,t2)) => Some((t1,t2))
    case OMA(this.term,ls:List[Term]) if ls.length>2 => Some((ls.head,OMA(this.term,ls.tail)))
    case _ => None
  }
}


object inl extends CoprodSymbol("inl") {
  def apply(tm:Term,tp2:Term) = OMA(this.term,List(tm,tp2))
  def unapply(t:Term) : Option[(Term,Term)] = t match {
    case OMA(this.term,List(tm,tp2)) => Some((tm,tp2))
    case _ => None
  }
}

object inr extends CoprodSymbol("inr") {
  def apply(tm:Term,tp1:Term) = OMA(this.term,List(tm,tp1))
  def unapply(t:Term) : Option[(Term,Term)] = t match {
    case OMA(this.term,List(tm,tp1)) => Some((tm,tp1))
    case _ => None
  }
}
object cmatch extends CoprodSymbol("coprodmatch") {
  def apply(t:Term, x: LocalName, left : Term, right : Term, to : Term) = OMBINDC(this.term,List(VarDecl(x,None,None,None,None)),List(t,left,right,to))
  def unapply(tm : Term) : Option[(Term, LocalName, Term, Term, Term)] = tm match {
    case OMBINDC(this.term,Context(VarDecl(x,None,_,None,_), rest @ _*),tms) =>
      if (rest.nonEmpty) return None
      if (tms.length!=4) return None
      Some((tms.head,x,tms(1),tms(2),tms(3)))
    case _ => None
  }
}

object Addfunc {
  val name = "Addfunc"
  val path = (Coproducts.baseURI ? "FuncaddSymbol") ? name
  val term = OMS(path)

  def apply(f1:Term,f2:Term) = OMA(this.term,List(f1,f2))
  def apply(t:Term*) = OMA(this.term,t.toList)
  def unapply(t:Term) : Option[(Term,Term)] = t match {
    case OMA(this.term,List(f1,f2)) => Some((f1,f2))
    case OMA(this.term,ls:List[Term]) if ls.length>2 => Some((ls.head,OMA(this.term,ls.tail)))
    case _ => None
  }
}
package info.kwarc.mmt.LFX.LFCoprod

import info.kwarc.mmt.api.{LocalName, DPath}
import info.kwarc.mmt.api.checking.{History, Solver, TypingRule}
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.utils.URI
import info.kwarc.mmt.lf.Apply

object LFCoprod {
  val baseURI = DPath(URI.http colon "cds.omdoc.org") / "LFX"
  val thname = "Coprod"
  val th2name = "LFCoprod"
  val path = baseURI ? thname
  def symbol(name : String) = path ? name
}

class CoprodSymbol(name:String) {
  val path = LFCoprod.path ? name
  val term = OMS(path)
}

class LFCoprodSymbol(name:String) {
  val path = (LFCoprod.baseURI ? LFCoprod.th2name) ? name
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
/*
object ccase extends CoprodSymbol("coprodcase") {
  def apply(x:LocalName,tp:Term,t:Term) = OMBIND(this.term,OMV(x)%tp,t)
  def unapply(t:Term) : Option[(LocalName,Term,Term)] = t match {
    case OMBIND(this.term,Context(VarDecl(n,Some(tp),_,_)),bd) => Some((n,tp,bd))
    case _ => None
  }
}

object cto extends CoprodSymbol("coprodto") {
  def apply(v:LocalName,tp:Term,t:Term) = OMBIND(this.term,OMV(v)%tp,t)
  def unapply(t:Term) : Option[(LocalName,Term,Term)] = t match {
    case OMBIND(this.term,Context(VarDecl(n,Some(tp),_,_)),bd) => Some((n,tp,bd))
    case _ => None
  }
}

object cmatch extends CoprodSymbol("coprodmatch") {
  def apply(t1:Term,left:Term,right:Term,v:LocalName,cotp:Term,to:Term) : OMA = OMA(this.term,List(t1,left,right,cto(v,cotp,to)))//List(t1,left,right))
  def apply(t:Term,v1:LocalName,tp1:Term,left:Term,v2:LocalName,tp2:Term,right:Term,v:LocalName,cotp:Term,to:Term) : OMA
    = apply(t,ccase(v1,tp1,left),ccase(v2,tp2,right),v,cotp,to)
  def unapply(tm:Term) : Option[(Term,Term,Term,LocalName,Term,Term)] = tm match {
    case OMA(this.term,List(t1,left,right,cto(v,cotp,to))) => Some((t1,left,right,v,cotp,to))
    case _ => None
  }
}
*/
// 	coprodmatch # 2 match V1T either 3 or 4 to 5 
object cmatch extends CoprodSymbol("coprodmatch") {
  def apply(t:Term, x: LocalName, left : Term, right : Term, to : Term) = OMBINDC(this.term,List(VarDecl(x,None,None,None)),List(t,left,right,to))
  def unapply(tm : Term) : Option[(Term, LocalName, Term, Term, Term)] = tm match {
    case OMBINDC(this.term,Context(VarDecl(x,_,None,_), rest @ _*),tms) =>
      if (rest.nonEmpty) return None
      if (tms.length!=4) return None
      Some((tms.head,x,tms(1),tms(2),tms(3)))
    case _ => None
  }
}

object Addfunc {
  val name = "Addfunc"
  val path = (LFCoprod.baseURI ? "Funcadd") ? name
  val term = OMS(path)

  def apply(f1:Term,f2:Term) = OMA(this.term,List(f1,f2))
  def apply(t:Term*) = OMA(this.term,t.toList)
  def unapply(t:Term) : Option[(Term,Term)] = t match {
    case OMA(this.term,List(f1,f2)) => Some((f1,f2))
    case OMA(this.term,ls:List[Term]) if ls.length>2 => Some((ls.head,OMA(this.term,ls.tail)))
    case Apply(Apply(this.term,f1),f2) => Some((f1,f2))
    case _ => None
  }
}
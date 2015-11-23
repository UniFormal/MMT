package info.kwarc.mmt.LFX.LFSigma

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.utils._

object LFSigma {
  val baseURI = DPath(URI.http colon "cds.omdoc.org") / "LFX"
  val thname = "Sigma"
  val path = baseURI ? thname
  def lfssymbol(name : String) = path ? name
}

class LFSigmaSymbol(name:String) {
  val path = LFSigma.path ? name
  val term = OMS(path)
}

object Sigma extends LFSigmaSymbol("Sigma") {
  def apply(name : LocalName, tp : Term, body : Term) = OMBIND(this.term, OMV(name) % tp, body)
  def apply(con: Context, body : Term) = OMBIND(this.term, con, body)
  def unapply(t : Term) : Option[(LocalName,Term,Term)] = t match {
    case OMBIND(OMS(this.path), Context(VarDecl(n,Some(a),None,_), rest @ _*), s) =>
      val newScope = if (rest.isEmpty)
        s
      else
        apply(Context(rest:_*), s)
      Some(n,a,newScope)
    case OMA(Product.term,args) if args.length >= 2 =>
      val name = OMV.anonymous
      if (args.length > 2)
        Some((name, args(0), OMA(Product.term, args.tail)))
      else
        Some((name,args(0),args(1)))
    case _ => None
  }
}

object Product extends LFSigmaSymbol("Product") {
  def apply(t1 : Term, t2 : Term) = OMA(this.term,List(t1,t2))
  def apply(in: List[Term], out: Term) = if (in.isEmpty) out else OMA(this.term, in ::: List(out))
  def unapply(t : Term) : Option[(Term,Term)] = t match {
    case OMA(this.term, hd :: tl) if tl.nonEmpty => Some((hd, apply(tl.init, tl.last)))
    case _ => None
  }
}

object Tuple extends LFSigmaSymbol("Tuple") {
  def apply(t1 : Term, t2 : Term) = OMA(this.term,List(t1,t2))
  def apply(in: List[Term]) = OMA(this.term, in)
  def unapply(t : Term) : Option[(Term,Term)] = t match {
    case OMA(this.term,ls) if ls.length==2 => Some(ls.head,ls(1))
    case OMA(this.term, hd :: tl) if tl.nonEmpty => Some((hd, apply(tl)))
    case _ => None
  }
}

/* case class Proj(i:BigInt) extends LFSigmaSymbol("Proj") {
  def apply(a: Term) = OMA(this.term, List(OMI(i), a))
  def unapply(t: Term) : Option[(BigInt,Term)] = t match {
    case OMA(this.term, List(OMI(j),a)) => Some((j,a))
    case _ => None
  }
} */

object Proj1 extends LFSigmaSymbol("Projl") {
  def apply(a: Term) = OMA(this.term, List(a))
  def unapply(t: Term) : Option[Term] = t match {
    case OMA(this.term, a) =>
      if (a.length == 1) Some(a.head)
      else None
    case _ => None
  }
}

object Proj2 extends LFSigmaSymbol("Projr") {
  def apply(a: Term) = OMA(this.term, List(a))
  def unapply(t: Term) : Option[Term] = t match {
    case OMA(this.term, a) =>
      if (a.length == 1) Some(a.head)
      else None
    case _ => None
  }
}

object Proj extends LFSigmaSymbol("Proj") {
  def apply(i:Int,a:Term) = if (i==1) Proj1(a) else if (i==2) Proj2(a) else None
  def unapply(t:Term) : Option[(Int,Term)] = t match {
    case Proj1(a) => Some(1,a)
    case Proj2(a) => Some(2,a)
    case _ => None
  }
}
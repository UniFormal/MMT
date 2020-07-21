package info.kwarc.mmt.frameit.mitm.Foundation

import info.kwarc.mmt.api.{DPath, GlobalName}
import info.kwarc.mmt.api.checking.{CheckingCallback, ComputationRule, History}
import info.kwarc.mmt.api.objects.{OMA, OMS, Stack, Term}
import info.kwarc.mmt.api.uom.{RecurseOnly, Simplifiability, Simplify}
import info.kwarc.mmt.api.utils.URI
import info.kwarc.mmt.lf.{Apply, ApplySpine}
import info.kwarc.mmt.lf.coercions.LFTypeCoercionRule

object ZF {
  val set = (MitM.path / "sets") ? "Sets" ? "set"
  object Elem {
    val path = (MitM.path / "sets") ? "SetTypeConversions" ? "elem"
    def apply(tm : Term) = Apply(OMS(path),tm)
    def unapply(tm : Term) =tm match {
      case Apply(OMS(`path`),t) => Some(t)
      case _ => None
    }
  }
  object SetProduct {
    val path = (MitM.path / "sets") ? "CartesianProduct" ? "product"
    def apply(l: Term,r : Term) = ApplySpine(OMS(path),l,r)
    def unapply(tm : Term) = tm match {
      case ApplySpine(OMS(`path`),List(l,r)) => Some((l,r))
      case _ => None
    }
  }
}

object LFX {
  val ns = DPath(URI.http colon "gl.mathhub.info") / "MMT" / "LFX"
  object SigmaTypes {
    val baseURI = LFX.ns / "Sigma"
    val thname = "Symbols"
    val path = baseURI ? thname
    def lfssymbol(name : String) = path ? name
  }
  class LFSigmaSymbol(name:String) {
    val path = SigmaTypes.path ? name
    val term = OMS(path)
  }
  object Product extends LFSigmaSymbol("Product") {
    def apply(t1 : Term, t2 : Term) = OMA(this.term,List(t1,t2))
    def apply(in: List[Term], out: Term) = if (in.isEmpty) out else OMA(this.term, in ::: List(out))
    def unapply(t : Term) : Option[(Term,Term)] = t match {
      case OMA(this.term, hd :: tl) if tl.nonEmpty => Some((hd, apply(tl.init, tl.last)))
      case _ => None
    }
  }
}

import ZF._
// object SetCoercionRule extends LFTypeCoercionRule(ZF.set,MitM.under,ZF.elem)
object LiftProductType extends ComputationRule(Elem.path) {
  override def alternativeHeads: List[GlobalName] = Apply.path :: Nil
  override def apply(check: CheckingCallback)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History): Simplifiability = tm match {
    case Elem(SetProduct(l,r)) => Simplify(LFX.Product(Elem(l),Elem(r)))
    case Elem(i) =>
      RecurseOnly(2 :: Nil)
    case _ => Simplifiability.NoRecurse
  }
}

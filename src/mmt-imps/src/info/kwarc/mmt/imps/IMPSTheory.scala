package info.kwarc.mmt.imps

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.lf.{Apply, ApplySpine}
import utils._

object IMPSTheory
{
  val rootdpath  = DPath(URI.http colon "latin.omdoc.org") / "foundations" / "lutins"
  val lutinsPath = rootdpath ? "Lutins"

  val lutinsPropType = lutinsPath ? "boolType"
  val lutinsIndType  = lutinsPath ? "indType"

  class Sym(s: String)
  {
    val path : GlobalName = lutinsPath ? s
    val term = OMS(path)
  }

  object tp extends Sym("tp")

  object FunType extends Sym("funType")
  {
    def apply(tp1 : Term, tp2 : Term) = ApplySpine(this.term,tp1,tp2)
  }

  object exp extends Sym("exp")
  {
    def apply(t:Term) = Apply(this.term,t)
  }

  object Sort extends Sym("sort")
  {
    def apply(t : Term) : Term = {
      Apply(this.term, t)
    }
  }

  object FunSort extends Sym("fun")
  {
    def apply(tpA : Term, tpB : Term, srtA : Term, srtB : Term) : Term =
    {
      ApplySpine(this.term, tpA, tpB, srtA, srtB)
    }
  }

  object Thm extends Sym("thm")
  {
    def apply(t : Term) : Term = {
      Apply(this.term, t)
    }
  }

  /* LOGIC */

  object Truth     extends Sym("thetrue")
  object Falsehood extends Sym("thefalse")

  object Negation extends Sym("not") {
    def apply(t : Term) : Term = {
      Apply(this.term, t)
    }
  }

  object Or extends Sym("or") {
    def apply(ls : List[Term]) : Term = {
      assert (ls.length >= 2)
      ls.init.foldRight(ls.last)((t,r) => ApplySpine(this.term,t,r))
    }
  }

  object And extends Sym("and"){
    def apply(ls : List[Term]) : Term = {
      assert (ls.length >= 2)
      ls.init.foldRight(ls.last)((t,r) => ApplySpine(this.term,t,r))
    }
  }

  object Equals extends Sym("equals") {
    def apply(p : Term, q : Term) : Term = {
      ApplySpine(this.term, p, q)
    }
  }

  object If extends Sym("if") {
    def apply(p : Term, t1 : Term, t2 : Term) : Term = {
      ApplySpine(this.term, p, t1, t2)
    }
  }

  object Iff extends Sym("iff") {
    def apply(p : Term, q : Term) : Term = {
      ApplySpine(this.term, p, q)
    }
  }

  object If_Form extends Sym("ifform") {
    def apply(p1 : Term, p2 : Term, p3 : Term) : Term = {
      ApplySpine(this.term, p1, p2, p3)
    }
  }

  object Implies extends Sym("implies") {
    def apply(p : Term, q : Term) : Term = {
      ApplySpine(this.term, p, q)
    }
  }

  object Lambda extends Sym("lambda")
  {
    def apply(ls : List[(LocalName,Option[Term])], t : Term) : Term = {
      assert(ls.nonEmpty)
      ls match {
        case _ => ls.foldRight(t)((tm,p) => ApplySpine(this.term,info.kwarc.mmt.lf.Lambda(tm._1, tm._2.get, p)))
      }
    }
  }

  // TODO: Correct?
  object IMPSApply extends Sym("apply")
  {
    def apply(f : Term, as : List[Term]) : Term = {
      as.foldLeft(f)((zw, b) => ApplySpine(this.term, zw, b))
    }
  }

  object Forall extends Sym("forall")
  {
    def apply(ls : List[(LocalName,Option[Term])], t : Term) : Term = {
      assert(ls.nonEmpty)
      ls match
      {
        case _ => ls.foldRight(t)((tm,p) => ApplySpine(this.term,info.kwarc.mmt.lf.Lambda(tm._1, tm._2.get, p)))
      }
    }
  }

  object Forsome extends Sym("forsome")
  {
    def apply(ls : List[(LocalName,Option[Term])], t : Term) : Term = {
      assert(ls.nonEmpty)
      ls match
      {
        case _ => ls.foldRight(t)((tm,p) => ApplySpine(this.term,info.kwarc.mmt.lf.Lambda(tm._1, tm._2.get, p)))
      }
    }
  }

  object Iota extends Sym("iota")
  {
    def apply(v1 : LocalName, s1 : Term, p : Term) : Term = {
      ApplySpine(this.term,info.kwarc.mmt.lf.Lambda(v1, s1, p))
    }
  }

  object IotaP extends Sym("iota_p")
  {
    def apply(v1 : LocalName, s1 : Term, p : Term) : Term = {
      ApplySpine(this.term,info.kwarc.mmt.lf.Lambda(v1, s1, p))
    }
  }

  object IsDefined extends Sym("isdefined")
  {
    def apply(t : Term) : Term = {
      Apply(this.term, t)
    }
  }

  object IsDefinedIn extends Sym("definedin")
  {
    def apply(t : Term, s : Term) : Term = {
      ApplySpine(this.term, t, s)
    }
  }

  object Undefined extends Sym("undefined")
  {
    def apply(t : Term) : Term = {
      Apply(this.term, t)
    }
  }
}

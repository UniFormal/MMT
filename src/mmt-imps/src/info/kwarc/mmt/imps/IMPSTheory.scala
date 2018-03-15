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

  val anIndividual = lutinsPath ? "anIndividual"

  val lutinsTP = lutinsPath ? "tp"

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
    def apply(tp : Term, srt : Term) = ApplySpine(this.term, tp, srt)
  }

  object Sort extends Sym("sort")
  {
    def apply(t : Term) : Term = {
      Apply(this.term, t)
    }
  }

  object Sets extends Sym("sets")
  {
    def apply(a : Term, alpha : Term) : Term = {
      ApplySpine(this.term, a, alpha)
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
    def apply(a : Term, alpha : Term, beta : Term, p : Term, q : Term) : Term = {
      ApplySpine(this.term, a, alpha, beta, p, q)
    }
  }

  object If extends Sym("ifthenelse") {
    def apply(a : Term, alpha : Term, p : Term, t1 : Term, t2 : Term) : Term = {
      ApplySpine(this.term, a, alpha, p, t1, t2)
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
    def apply(a : Term, b : Term, alpha : Term, beta : Term, f : Term) : Term =
    {
      ApplySpine(this.term, a, b, alpha, beta, f)
    }
  }

  object IMPSApply extends Sym("apply")
  {
    def apply(a : Term, b : Term, alpha : Term, gamma : Term, beta : Term, f : Term, target : Term) : Term =
    {
      ApplySpine(this.term, a, b, alpha, gamma, beta, f, target)
    }
  }

  object Forall extends Sym("forall")
  {
    // forall : {A, α : sort A} (exp α → exp bool) → exp bool# ∀ 3 prec 20
    def apply(a : Term, alpha : Term, p : Term) : Term =
    {
      ApplySpine(this.term, a, alpha, p)
    }
  }

  object Forsome extends Sym("forsome")
  {
    // forsome  : {A, α : sort A} (exp α → exp bool) → exp bool# ∃ 3 prec 20
    def apply(a : Term, alpha : Term, p : Term) : Term =
    {
      ApplySpine(this.term, a, alpha, p)
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
    def apply(a : Term, alpha : Term, t : Term) : Term = {
      ApplySpine(this.term, a, alpha, t)
    }
  }

  object IsDefinedIn extends Sym("isdefinedin")
  {
    def apply(a : Term, alpha : Term, t : Term, s : Term) : Term = {
      ApplySpine(this.term, t, s)
    }
  }

  object Undefined extends Sym("undefined")
  {
    def apply(a : Term, t : Term) : Term = {
      ApplySpine(this.term, a, t)
    }
  }

  object Total extends Sym("total")
  {
    def apply(f : Term, bs : List[Term]) : Term = {
      ApplySpine(this.term, f) // TODO: FIXME
    }
  }

  object Nonvacuous extends Sym("nonvacuous")
  {
    def apply(p : Term) : Term = {
      Apply(this.term, p) // TODO: FIXME
    }
  }

  object Quasiequals extends Sym("quasiequals")
  {
    def apply(p1 : Term , p2 : Term) : Term = {
      ApplySpine(this.term, p1, p2) // TODO: FIXME
    }
  }

}

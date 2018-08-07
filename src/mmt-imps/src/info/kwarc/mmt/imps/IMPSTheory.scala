package info.kwarc.mmt.imps

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.uom.{RSubtype, RepresentedRealizedType, StandardInt, StandardRat, StandardNat}
import info.kwarc.mmt.lf.{Apply, ApplySpine}
import utils._

object ImpsOctet extends RSubtype(StandardNat) {
  override def asString = "octet"
  def by(u: Any) : Boolean = StandardInt.unapply(u).get >= 0 && StandardInt.unapply(u).get <= 255
}

object IntLiterals extends
  RepresentedRealizedType(IMPSTheory.exp(OMS(IMPSTheory.lutinsIndType),OMS(IMPSTheory.lutinsPath?"integerType")),StandardInt)

object RatLiterals extends
  RepresentedRealizedType(IMPSTheory.exp(OMS(IMPSTheory.lutinsIndType),OMS(IMPSTheory.lutinsPath?"rationalType")),StandardRat)

object OctLiterals extends
  RepresentedRealizedType(IMPSTheory.exp(OMS(IMPSTheory.lutinsIndType),OMS(IMPSTheory.lutinsPath?"octetType")),ImpsOctet)

object IMPSTheory
{
  val rootdpath  : DPath = DPath(URI.http colon "latin.omdoc.org") / "foundations" / "lutins"
  val lutinsPath : MPath = rootdpath ? "Lutins"

  val lutinsPropType : GlobalName = lutinsPath ? "boolType"
  val lutinsIndType  : GlobalName = lutinsPath ? "indType"
  val anIndividual   : GlobalName = lutinsPath ? "anIndividual"
  val lutinsTP       : GlobalName = lutinsPath ? "tp"

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

  object Iota extends Sym("description_i")
  {
    def apply(a : Term, alpha : Term, iota : Term) : Term = {
      ApplySpine(this.term,a,alpha,iota)
    }
  }

  object IotaP extends Sym("description_p")
  {
    def apply(alpha : Term, iota_p : Term) : Term = {
      ApplySpine(this.term,alpha,iota_p)
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
      ApplySpine(this.term, a, alpha, t, s)
    }
  }

  object Undefined extends Sym("undefined")
  {
    def apply(a : Term, t : Term) : Term = {
      ApplySpine(this.term, a, t)
    }
  }

  /* Quasi-Constructors in LUTINS */

  // Not used.
  object Falselike extends Sym("falselike")
  {
    def apply : Term = ???
  }

  object Total extends Sym("total")
  {
    def apply(a : Term, b : Term, alpha : Term, beta : Term, f : Term) : Term = {
      ApplySpine(this.term, a, b, alpha, beta, f)
    }
  }

  object Nonvacuous extends Sym("nonvacuous")
  {
    def apply(a : Term, alpha : Term, p : Term) : Term = {
      ApplySpine(this.term, a, alpha, p)
    }
  }

  object Quasiequals extends Sym("quasiequals")
  {
    def apply(a : Term, alpha : Term, beta : Term, p1 : Term , p2 : Term) : Term = {
      ApplySpine(this.term, a, alpha, beta, p1, p2)
    }
  }

  object Domain extends Sym("domain")
  {
    def apply(a : Term, b : Term, alpha : Term, beta : Term, f : Term) : Term = {
      ApplySpine(this.term, a, b, alpha, beta, f)
    }
  }

  // Subtheory for User-Defined Quasi-Constructors
  object QCT
  {
    val quasiLutinsPath : MPath = rootdpath ? "QuasiLutins"

    class UDQC(s: String)
    {
      val path : GlobalName = quasiLutinsPath ? s
      val term = OMS(path)
    }

    object pred2indicQC extends UDQC("pred2indicQC") {
      def apply (u : Term, uu : Term, s : Term) : Term = {
        ApplySpine(this.term, u, uu, s)
      }
    }

    object sort2indicQC extends UDQC("sort2indicQC") {
      def apply(u : Term, e : Term) : Term = {
        ApplySpine(this.term, u, e)
      }
    }

    object inQC extends UDQC("inQC") {
      def apply(a : Term, alpha : Term, x : Term, aa : Term) : Term = {
        ApplySpine(this.term, a, alpha, x, aa)
      }
    }

    object subseteqQC extends UDQC("subseteqQC") {
      def apply(u : Term, uu : Term, a : Term, b : Term) : Term = {
        ApplySpine(this.term, u, uu, a, b)
      }
    }

    object subsetQC extends UDQC("subsetQC") {
      def apply (u : Term, uu : Term, a : Term, b : Term) : Term = {
        ApplySpine(this.term, u, uu, a, b)
      }
    }

    object emptyIndicQC extends UDQC("emptyIndicQC") {
      def apply (u : Term, e : Term) : Term = {
        ApplySpine(this.term, u, e)
      }
    }

    object nonEmptyIndicQQC extends UDQC("nonEmptyIndicQQC") {
      def apply (u : Term, uu : Term, a : Term) : Term = {
        ApplySpine(this.term, u, uu, a)
      }
    }

    object emptyIndicQQC extends UDQC("emptyIndicQQC") {
      def apply (u : Term, uu : Term, a : Term) : Term = {
        ApplySpine(this.term, u, uu, a)
      }
    }

    object complementQC extends UDQC("complementQC") {
      def apply (u : Term, uu : Term, s : Term) : Term = {
        ApplySpine(this.term, u, uu, s)
      }
    }

    object unionQC extends UDQC("unionQC") {
      def apply (u : Term, uu : Term, s : Term, t : Term) : Term = {
        ApplySpine(this.term, u, uu, s, t)
      }
    }

    object intersectionQC extends UDQC("intersectionQC") {
      def apply (u : Term, uu : Term, s : Term, t : Term) : Term = {
        ApplySpine(this.term, u,uu,s,t)
      }
    }

    object differenceQC extends UDQC("differenceQC") {
      def apply (u : Term, uu : Term, s : Term, t : Term) : Term = {
        ApplySpine(this.term, u, uu, s, t)
      }
    }

    object symDifferenceQC extends UDQC("symDifferenceQC") {
      def apply (u : Term, uu : Term, s : Term, t : Term) : Term = {
        ApplySpine(this.term, u,uu,s,t)
      }
    }

    object disjointQC extends UDQC("disjointQC") {
      def apply (u : Term, uu : Term, s : Term, t : Term) : Term = {
        ApplySpine(this.term, u, uu, s, t)
      }
    }

    object partitionQQC extends UDQC("partitionQQC") {
      def apply (u : Term, uu : Term, w : Term, s : Term) : Term = {
        ApplySpine(this.term, u,uu,w,s)
      }
    }

    object singletonQC extends UDQC("singletonQC") {
      def apply (u : Term, uu : Term, a : Term) : Term = {
        ApplySpine(this.term, u, uu, a)
      }
    }

    object bigUnionQC extends UDQC("bigUnionQC") {
      def apply(a : Term, i : Term, aa : Term, ii : Term, f : Term) : Term = {
        ApplySpine(this.term, a, i, aa, ii, f)
      }
    }

    object bigIntersectionQC extends UDQC("bigIntersectionQC") {
      def apply(a : Term, i : Term, aa : Term, ii : Term, f : Term) : Term = {
        ApplySpine(this.term, a, i, aa, ii, f)
      }
    }

    object mcompositionQC extends UDQC("mcompositionQC") {
      def apply(a : Term, b : Term, c : Term, alpha : Term, beta : Term, gamma : Term, f : Term, g : Term) : Term = {
        ApplySpine(this.term, a, b, c, alpha, beta, gamma, f, g)
      }
    }

    object mdomainQC extends UDQC("mdomainQC") {
      def apply(a : Term, b : Term, alpha : Term, beta : Term, f : Term) : Term = {
        ApplySpine(this.term, a, b, alpha, beta, f)
      }
    }

    object mrangeQC extends UDQC("mrangeQC") {
      def apply(a : Term, b : Term, alpha : Term, beta : Term, f : Term) : Term = {
        ApplySpine(this.term, a, b, alpha, beta, f)
      }
    }

    object mimageQC extends UDQC("mimageQC") {
      def apply(a : Term, b : Term, alpha : Term, beta : Term, f : Term, aset : Term) : Term = {
        ApplySpine(this.term, a, b, alpha, beta, f, aset)
      }
    }

    object minverseimageQC extends UDQC("minverseimageQC") {
      def apply(a : Term, b : Term, alpha : Term, beta : Term, f : Term, bset : Term) : Term = {
        ApplySpine(this.term, a, b, alpha, beta, f, bset)
      }
    }

    object minverseQC extends UDQC("minverseQC") {
      def apply(a : Term, b : Term, alpha : Term, beta : Term, f : Term) : Term = {
        ApplySpine(this.term, a, b, alpha, beta, f)
      }
    }

    object midQC extends UDQC("midQC") {
      def apply(a : Term, alpha : Term, aset : Term) : Term = {
        ApplySpine(this.term, a, alpha, aset)
      }
    }

    object mrestrictQC extends UDQC("mrestrictQC") {
      def apply(a : Term, b : Term, alpha : Term, beta : Term, f : Term, aset : Term) : Term = {
        ApplySpine(this.term, a, b, alpha, beta, f, aset)
      }
    }

    object mrestrict2QC extends UDQC("mrestrict2QC") {
      def apply(a : Term, b : Term, c : Term, alpha : Term, beta : Term, gamma : Term, f : Term, aset : Term, bset : Term) : Term = {
        ApplySpine(this.term, a, b, c, alpha, beta, gamma, f, aset, bset)
      }
    }

    object msurjectiveQQC extends UDQC("msurjectiveQQC") {
      def apply(a : Term, b : Term, alpha : Term, beta : Term, f : Term) : Term = {
        ApplySpine(this.term, a, b, alpha, beta, f)
      }
    }

    object minjectiveQQC extends UDQC("minjectiveQQC") {
      def apply(a : Term, b : Term, alpha : Term, beta : Term, f : Term) : Term = {
        ApplySpine(this.term, a, b, alpha, beta, f)
      }
    }

    object mbijectiveQQC extends UDQC("mbijectiveQC") {
      def apply(a : Term, b : Term, alpha : Term, beta : Term, f : Term) : Term = {
        ApplySpine(this.term, a, b, alpha, beta, f)
      }
    }

    object msurjectiveonQQC extends UDQC("msurjectiveonQQC") {
      def apply(a : Term, b : Term, alpha : Term, beta : Term, f : Term, aset : Term, bset : Term) : Term = {
        ApplySpine(this.term, a, b, alpha, beta, f, aset, bset)
      }
    }

    object minjectiveonQQC extends UDQC("minjectiveonQQC") {
      def apply(a : Term, b : Term, alpha : Term, beta : Term, f : Term, aset : Term) : Term = {
        ApplySpine(this.term, a, b, alpha, beta, f, aset)
      }
    }

    object mbijectiveonQQC extends UDQC("mbijectiveonQQC") {
      def apply(a : Term, b : Term, alpha : Term, beta : Term, f : Term, aset : Term, bset : Term) : Term = {
        ApplySpine(this.term, a, b, alpha, beta, f, aset, bset)
      }
    }


  }

}



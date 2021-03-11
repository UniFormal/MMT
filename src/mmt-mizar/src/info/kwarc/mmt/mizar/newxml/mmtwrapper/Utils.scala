package info.kwarc.mmt.mizar.newxml.mmtwrapper

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.uom.ConstantScala
import info.kwarc.mmt.lf.{BinaryLFConstantScala, _}
import info.kwarc.mmt.mizar.newxml.mmtwrapper.MizSeq._

object MizarPrimitiveConcepts {
  val mmlBase = utils.URI("http", "oaff.mathweb.org") / "MML"
  val mathHubBase = "http://gl.mathhub.info/Mizar/MML/blob/master"
  // private val mizarBase =  DPath(utils.URI("http", "latin.omdoc.org") / "foundations"/ "mizar")

  private val latinBase =  DPath(utils.URI("latin:/"))
  val MizarPatternsTh = latinBase ? "MizarPatterns"
  val MizarTh = latinBase ? "Mizar"
  val TermsTh = latinBase ? "Terms"
  val PropositionsTh = latinBase ? "Propositions"
  val TypesTh = latinBase ? "Types"
  val ProofsTh = latinBase ? "Proofs"
  val softTypedTermsTh = latinBase ? "SoftTypedTerms"
  val ConjunctionTh = latinBase ? "Conjunction"
  val DisjunctionTh = latinBase ? "SoftTypedDefinedFOL"
  val EqualityTh = latinBase ? "UntypedEquality"
  val TruthTh = latinBase ? "Truth"
  val FalsityTh = latinBase ? "Falsity"
  val NegationTh = latinBase ? "Negation"
  val ImplicationTh = latinBase ? "SoftTypedDefinedFOL"
  val EquivalenceTh = latinBase ? "SoftTypedDefinedFOL"

  val HiddenTh = latinBase ? "HIDDEN"
  //TODO
  val MizarInformal = latinBase ? "mizar-informal"

  def by : Term = OMID(MizarInformal ? "by")
  def from : Term = OMID(MizarInformal ? "from")

  def constantName(name : String) : GlobalName = {
    name match {
      case "any" => TermsTh ? "term"
      case "set" => HiddenTh ? name
      case "sethood" => HiddenTh ? "sethood_property"
      case "in" => HiddenTh ? "in"
      case "prop"=> PropositionsTh ? name
      case "mode" => TypesTh ? "tp"
      case "proof" => ProofsTh ? "ded"
      case "is" => softTypedTermsTh ? "of"
      case "and" => ConjunctionTh ? name
      case "or" => DisjunctionTh ? name
      case "eq" => EqualityTh ? "equal"
      case "neq" => HiddenTh ? "inequal"
      case "true" => TruthTh ? name
      case "false" => FalsityTh ? name
      case "not" => NegationTh ? name
      case "implies" => ImplicationTh ? "impl"
      case "iff" => EquivalenceTh ? "equiv"
      case _ => MizarTh ? name
    }
  }
  object constant {
    def apply(name: String): OMID = OMID(constantName(name))

    def unapply(tm: Term) = tm match {
      case OMID(gn:GlobalName) if (gn == constantName(gn.name.toString)) => Some(gn.name.toString)
      case _ => None
    }
  }

  def compact(t : Term) : Term = {
    t
  }

  def apply(f : Term, args : Term*) = ApplyGeneral(f, args.toList)

  def prop : Term = constant("prop")
  def tp : Term = constant("tp")
  def set = constant("set")
  def in = constant("in")
  def any =constant("any")

  object is extends BinaryLFConstantScala(softTypedTermsTh, "of")
  object be extends BinaryLFConstantScala(MizarTh, "be")

  def andCon = constantName("and")
  object naryAndSym extends BinaryLFConstantScala(MizarTh, "nary_and")
  object And {
    def apply(tms : List[Term]) : Term = naryAndSym(OMI(tms.length), Sequence(tms))
    def unapply(t: Term) = t match {
      case ApplyGeneral(OMS(gn), conjs@List(conj1, conj2)) if (gn == andCon) => Some(conjs)
      case naryAndSym(OMI(n), Sequence(tms)) if (tms.length == n) => Some(tms)
      case _ => None
    }
  }
  def binaryAnd(a:Term, b:Term) : Term = And(List(a,b))
  def orCon = constantName("or")
  object naryOrSym extends BinaryLFConstantScala(MizarTh, "nary_or")
  object Or {
    def apply(tms: List[Term]) = naryOrSym(OMI(tms.length), Sequence(tms))
    def unapply(t: Term) = t match {
      case ApplyGeneral(OMS(gn), disjs@List(disj1, disj2)) if (gn == orCon) => Some(disjs)
      case naryOrSym(OMI(n), Sequence(tms)) if (n == tms.length) => Some(tms)
      case _ => None
    }
  }
  def binaryOr(a:Term, b:Term) : Term = Or(List(a,b))

  def trueCon = constant("true")
  def falseCon = constant("false")

  object implies extends BinaryLFConstantScala(MizarTh, "implies")
  object iff extends BinaryLFConstantScala(MizarTh, "iff")
  object not extends UnaryLFConstantScala(MizarTh, "not")
  def eqCon = constantName("eq")
  object eq extends BinaryLFConstantScala(eqCon.module, eqCon.name.toString)
  def neqCon = constantName("neq")
  object neq extends BinaryLFConstantScala(neqCon.module, neqCon.name.toString)
  def thesis = constantName("thesis")

  class Quantifier(n: String) {
    def apply(v : OMV, univ : Term, p : Term): Term = ApplySpine(OMS(constantName(n)), univ, Lambda(v % any, p))
    def apply(v: VarDecl, p: Term): Term = apply(v.toTerm, v.tp.get, p)
    def apply(v : String, univ : Term, p : Term): Term = apply(OMV(v), univ, p)
    def unapply(t: Term): Option[(OMV,Term,Term)] = t match {
      case ApplySpine(OMS(q), List(a, Lambda(x, _, p))) if q == constantName(n) => Some((OMV(x), a, p))
      case _ => None
    }
  }
  object forall extends Quantifier("for")
  object exists extends Quantifier("ex")

  object proof extends UnaryLFConstantScala(ProofsTh, "ded")
  object Uses extends TernaryLFConstantScala(MizarTh, "using")
  object ProofByExample extends BinaryLFConstantScala(MizarTh, "proof_by_example")
  def uses(claim: Term, usedFacts: List[Term]) = Uses(claim, OMI(usedFacts.length), Sequence(usedFacts))
  def zeroAryAndPropCon = constant("0ary_and_prop")
  object oneAryAndPropCon extends UnaryLFConstantScala(MizarTh, "1ary_and_prop")
  object andInductPropCon extends TernaryLFConstantScala(MizarTh, "and_induct_prop")
  def consistencyTp(argTps: List[Term], cases: List[Term], caseRes: List[Term], direct: Boolean, resKind: String) = {
    val suffix = if (direct) "Dir" else "Indir" + resKind
    ApplyGeneral(OMS(MizarPatternsTh ? LocalName("consistencyTp"+suffix )), List(OMI(argTps.length), Sequence(argTps), OMI(cases.length), Sequence(cases), Sequence(caseRes)))
  }

  def attr(t : Term) = apply(constant("attr"), t)
  def adjective(cluster : Term, typ : Term) = apply(constant("adjective"), typ, cluster)
  def cluster(a1 : Term, a2 : Term) = apply(constant("cluster"), a1, a2)
  def choice(tp : Term) = apply(constant("choice"), tp)
  def fraenkel(v : String, t : Term, p : Term, f : Term) =
    apply(constant("fraenkel"), t, Lambda(LocalName(v), any, p), Lambda(LocalName(v), any, f))

  /**
   * invoking specification axiom for sets
   * corresponds to the set specified in mizar by
   * {expression where args is Element of universe : condition }
   *
   * @param expression any term, may use the variables from args
   * @param args some free variables that may be used in expression and condition
   * @param universe a set, the args must be elements of
   * @param condition the condition which argument choices to consider
   * @return
   */
  def fraenkelTerm(expression: Term, args: List[OMV], universe:Term, condition:Term) = {
    val argsCont = Context(args.map(_.%(universe)):_*)
    val cond = info.kwarc.mmt.lf.Pi(argsCont, condition)
    val expr = info.kwarc.mmt.lf.Pi(argsCont, expression)
    apply(constant("fraenkelTerm"), List(universe,OMI(args.length),cond,expr):_*)
  }
  def simpleFraenkelTerm(expression: Term, args: List[OMV], universe:Term) = {
    val argsCont = Context(args.map(_.%(universe)):_*)
    val expr = info.kwarc.mmt.lf.Pi(argsCont, expression)
    apply(constant("simpleFraenkelTerm"), List(universe,OMI(args.length),expr):_*)
  }

  val numRT = new uom.RepresentedRealizedType(any, uom.StandardInt)
  def num(i: Int) = numRT(i)

  object SimpleTypedAttrAppl {
    def apply(baseTp: Term, attrs: List[Term]) = {
      val attrApplSym = constant("adjective")
      attrs.foldRight[Term](baseTp)((tp:Term, attr:Term) => ApplyGeneral(attrApplSym, List(tp,attr)))
    }
    def unapply(tm: Term) : Option[(Term, List[Term])] = tm match {
      case ApplyGeneral(constant("adjective"), tp::attr) => Some((tp, attr))
      case _ => None
    }
  }
  object depTypedAttrAppl {
    def apply(n: Int, nArgsDepType: Term, attrs: List[Term]) = {
      val m = attrs.length
      val attributes = MMTUtils.flatten(attrs)
      val attrApplSym = constant("attr_appl")
      val (nTm, mTm) = (OMI(n), OMI(m))
      ApplyGeneral(attrApplSym, List(nTm, mTm, nArgsDepType, attributes))
    }
    def unapply(tm: Term) : Option[(Int, Term, List[Term])] = tm match {
      case ApplyGeneral(constant("attr_appl"), List(nTm, mTm, nArgsDepType, attributes)) =>
        val MizSeq.OMI(n) = nTm
        val Sequence(attrs) = attributes
        Some((n, nArgsDepType, attrs))
      case _ => None
    }
  }
}

object MMTUtils {
  val mainPatternName = OMV.anonymous

  def Lam(name: String, tp: Term, body: Term): Term = {
    Lambda(LocalName(name), tp, body)
  }

  def freeVarContext(varTps:List[Term]): Context =
    varTps.zipWithIndex.map {case (tp:Term,i:Int) => OMV(LocalName("x_"+i)) % tp }
  def freeVars(varTps:List[Term], nm:Option[String]=None): Context =
  varTps.zipWithIndex.map {case (tp:Term,i:Int) => OMV(LocalName(nm.getOrElse("x_")+i)) % MizarPrimitiveConcepts.any }
  def freeAlternatingVars(varTps:List[Term], nm:List[String]): List[OMV] =
    varTps.zipWithIndex.flatMap {case (tp:Term,i:Int) => nm map {s => OMV(LocalName(s+i))} }
  def flatten(tms:List[Term]) : Term = MizSeq.Sequence.apply(tms:_*)
}
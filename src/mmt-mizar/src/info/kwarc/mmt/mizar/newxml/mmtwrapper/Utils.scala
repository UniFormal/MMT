package info.kwarc.mmt.mizar.newxml.mmtwrapper

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.lf._
import info.kwarc.mmt.mizar.newxml.mmtwrapper.MizSeq._
import info.kwarc.mmt.mizar.newxml.translator.TranslationController


object Mizar {
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
  val EqualityTh = latinBase ? "Equality"
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
      case "prop"=> PropositionsTh ? name
      case "mode" => TypesTh ? "tp"
      case "proof" => ProofsTh ? "ded"
      case "is" => softTypedTermsTh ? "of"
      case "and" => ConjunctionTh ? name
      case "or" => DisjunctionTh ? name
      case "eq" => EqualityTh ? "equal"
      case "ineq" => HiddenTh ? "inequal"
      case "true" => TruthTh ? name
      case "false" => FalsityTh ? name
      case "not" => NegationTh ? name
      case "implies" => ImplicationTh ? "impl"
      case "iff" => EquivalenceTh ? "equiv"
      case _ => MizarTh ? name
    }
  }
  def constant(name : String) : Term = OMID(constantName(name))

  def compact(t : Term) : Term = {
    t
  }

  def apply(f : Term, args : Term*) = ApplyGeneral(f, args.toList)

  def prop : Term = constant("prop")
  //val any : Term = constant("any")
  def tp : Term = constant("tp")
  def set = constant("set")
  // TODO: make sure this really gives us what we want
  def any =constant("any")

  def is(t1 : Term, t2 : Term) = apply(constant("is"), t1, t2)
  def be(t1 : Term, t2 : Term) = apply(constant("be"), t1, t2)

  def andCon = constantName("and")
  def naryAndCon = constantName("nary_and")

  def and(tms : List[Term]) : Term = apply(OMS(naryAndCon), (OMI(tms.length)::tms):_*)
  def binaryAnd(a:Term, b:Term) : Term = apply(OMS(andCon),List(a,b):_*)
  def orCon = constantName("or")
  def naryOrCon = constantName("nary_or")
  def or(tms : List[Term]) : Term = apply(OMS(naryOrCon), (OMI(tms.length)::tms):_*)
  def binaryOr(a:Term, b:Term) : Term = apply(OMS(orCon),List(a,b):_*)

  // Special function for 'and' and 'or' applied to an sequence (e.g. Ellipsis or sequence variable)
  def seqConn(connective : String, length : Term, seq : Term) : Term =
    apply(constant(connective), length, seq)


  def trueCon = constant("true")
  def falseCon = constant("false")

  object implies extends BinaryLFConstantScala(MizarTh, "implies")
  object iff extends BinaryLFConstantScala(MizarTh, "iff")
  object not extends UnaryLFConstantScala(MizarTh, "not")
  def eqCon = constantName("eq")
  object eq extends BinaryLFConstantScala(eqCon.module, "eq")

  class Quantifier(n: String) {
    def apply(v : OMV, univ : Term, prop : Term) = ApplySpine(OMS(constantName(n)), univ, Lambda(v % univ, prop))
    def unapply(t: Term): Option[(OMV,Term,Term)] = t match {
      case ApplySpine(OMS(q), List(a, Lambda(x, _, prop))) if q == constantName(n) => Some((OMV(x), a, prop))
      case _ => None
    }
  }
  object forall extends Quantifier("for")
  object exists extends Quantifier("ex")

  object proof extends UnaryLFConstantScala(MizarTh, "proof")


  //     OMBIND(apply(Mizar.constant("for"), tp),Context(VarDecl(LocalName(v), Some(Mizar.any), None, None)), prop)

  def attr(t : Term) = apply(Mizar.constant("attr"), t)
  def adjective(cluster : Term, typ : Term) = apply(Mizar.constant("adjective"), typ, cluster)
  def cluster(a1 : Term, a2 : Term) = apply(Mizar.constant("cluster"), a1, a2)
  def choice(tp : Term) = apply(Mizar.constant("choice"), tp)
  def fraenkel(v : String, t : Term, p : Term, f : Term) =
    apply(Mizar.constant("fraenkel"), t, Lambda(LocalName(v), Mizar.any, p), Lambda(LocalName(v), Mizar.any, f))

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
    apply(Mizar.constant(name="fraenkelTerm"), List(universe,OMI(args.length),cond,expr):_*)
  }
  def simpleFraenkelTerm(expression: Term, args: List[OMV], universe:Term) = {
    val argsCont = Context(args.map(_.%(universe)):_*)
    val expr = info.kwarc.mmt.lf.Pi(argsCont, expression)
    apply(Mizar.constant(name="simpleFraenkelTerm"), List(universe,OMI(args.length),expr):_*)
  }

  val numRT = new uom.RepresentedRealizedType(any, uom.StandardInt)
  def num(i: Int) = numRT(i)

  def simpleTypedAttrAppl(baseTp: Term, attrs: List[Term]) = {
    val attrApplSym = constant("adjective")
    attrs.foldRight[Term](baseTp)((tp:Term, attr:Term) => ApplyGeneral(attrApplSym, List(tp,attr)))
  }
  def depTypedAttrAppl(n: Int, nArgsDepType: Term, attrs: List[Term]) = {
    val m = attrs.length
    val attributes = MMTUtils.flatten(attrs)
    val attrApplSym = constant("attr_appl")
    val (nTm, mTm) = (OMI(n), OMI(m))
    ApplyGeneral(attrApplSym, List(nTm, mTm, nArgsDepType, attributes))
  }
}

object MMTUtils {
  val mainPatternName = OMV.anonymous

  def getTheoryPath(aid: String): MPath = {
    if (aid == TranslationController.currentAid)
      TranslationController.currentTheoryPath
    else aid match {
      case "HIDDEN" => Mizar.HiddenTh
      case _ => DPath(Mizar.mmlBase) ? aid
    }
  }

  def getPath(aid: String, kind: String, absnr: Int): GlobalName = {
    getTheoryPath(aid) ? (aid + "_" + kind + "_" + absnr.toString)

  }
  def getPath(aid: String, name: String): GlobalName = {
    getTheoryPath(aid) ? name
  }
  def getPath(aid: String, name: LocalName): GlobalName = {
    getTheoryPath(aid) ? name
  }
  def getPath(aid: String, name: List[String]): GlobalName = {
    getTheoryPath(aid) ? LocalName(name.map(SimpleStep))
  }

  def Lam(name: String, tp: Term, body: Term): Term = {
    Lambda(LocalName(name), tp, body)
  }

  def freeVarContext(varTps:List[Term]): Context =
    varTps.zipWithIndex.map {case (tp:Term,i:Int) => OMV(LocalName("x_"+i)) % tp }
  def freeVars(varTps:List[Term], nm:Option[String]=None): Context =
  varTps.zipWithIndex.map {case (tp:Term,i:Int) => OMV(LocalName(nm.getOrElse("x_")+i)) % Mizar.any }
  def freeAlternatingVars(varTps:List[Term], nm:List[String]): List[OMV] =
    varTps.zipWithIndex.flatMap {case (tp:Term,i:Int) => nm map {s => OMV(LocalName(s+i))} }
  def flatten(tms:List[Term]) : Term = MizSeq.Sequence.apply(tms:_*)
}
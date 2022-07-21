package info.kwarc.mmt.lf.elpi

import info.kwarc.mmt.api._
import archives._
import symbols._
import modules._
import objects._
import documents._
import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.lf._
import info.kwarc.mmt.sequences.NatRules.NatLit
import ELPIExporter.translateTerm
import info.kwarc.mmt.api.LocalName.toList

object HelpCons {
  def apply(path: GlobalName)                 : ELPI.Variable = ELPI.Variable(LocalName("help") / path.name)
  def apply(path: GlobalName, suffix: String) : ELPI.Variable = ELPI.Variable(LocalName("help") / path.name / suffix)
}

case class  ELPIError(msg: String) extends Error(msg)

object ELPIExporter {
  /** straightforward translation of an LF terms to a lambda-Prolog term */

  def translateTerm(t: Term): ELPI.Expr = {
    t match {
      case OMS(p) =>
        if (p.toString == "http://cds.omdoc.org/urtheories?NatSymbols?NAT")
          ELPI.Variable(LocalName("int"))
        else {
          ELPI.Variable(p.name)
        }
      case OMV(n) =>
        ELPI.Variable(elpiRename(n))
      case Lambda(x, _, t) =>
        ELPI.Lambda(elpiRename(x), translateTerm(t))
      case ApplySpine(f, args) =>
        val fE = translateTerm(f)
        val argsE = args map translateTerm
        fE(argsE: _*)
      case Arrow(a, b) =>
        val aE = translateTerm(a)
        val bE = translateTerm(b)
        ELPI.Arrow(aE, bE)
      case Pi(x, _, b) =>
        ELPI.Forall(elpiRename(x), translateTerm(b))
      case OMLIT(v, NatLit) =>
        ELPI.Integer(v.toString.toInt)
      case _ => throw ELPIError("unknown term: " + t)
    }
  }

  // We want Variables to start with capital letters, so we rename things like "t" to "T/t".
  // "I don't anticipate any clashes here because MMT names can't contain slashes." -- Jonas, March 2021
  // We also need to change variable names to ASCII only, ELPI seems to have a problem with Unicode.
  def elpiRename(ln : LocalName) : LocalName = {
    var s : String = ""
    for (ch : Char <- ln.toString) {
      // Check for ascii-ness
      if ((ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z')) {
        s = s + ch.toString
      } else {
        val hex = Integer.toHexString(ch).toUpperCase.reverse.padTo(4, '0').reverse
        s = s + hex
      }
    }
    // Make sure everything still starts with a letter
    if (!(s.head.isLetter)) {
      s = "E" + s
    }
    val nln : LocalName = if (s.head.isLower) { LocalName(s.head.toUpper + "/" + s) } else { LocalName(s) }
    nln
  }
}

class ELPIExporter extends Exporter {
  val key = "lf-elpi"
  override def outExt = "elpi"

  private lazy val lup = controller.globalLookup
  private lazy val ruleMatcher = new RuleMatcher(lup, List("Judgment", "TabMarker", "TabClosed"))

  private lazy val constantHandler = new ConstantHandlerSequence(List(
    new GeneratedFromHandler(controller),
    // new TypeHandler(),   // not good enough (what do we do with dependent types?)
    new MainJudgmentHandler(ruleMatcher),
    new ProductRuleHandler(ruleMatcher),
    new IterativeDeepeningHandler(ruleMatcher),
    new ProofTermHandler(ruleMatcher),
    new RuleUseHandler(ruleMatcher),
    new HandDownHandler(ruleMatcher, name = ""),
    new HandDownHandler(ruleMatcher, name = "2"),
    new BackChainingHandler(ruleMatcher)
  ))

  private def translateTheory(thy: Theory): ELPI.Program = {
    val cons = thy.getDeclarations
    val consE = cons flatMap translateDeclaration
    ELPI.Program(consE:_*)
  }

  private def translateDeclaration(d: Declaration): List[ELPI.Decl] = {
    d match {
      case c: Constant => constantHandler.handle(c)
      case PlainInclude(from,_) =>
        // we generate one elpi file per MMT theory; MMT includes become lambda-Prolog file includes
        getOutFileForModule(from) match {
          case Some(f) => List(ELPI.Accumulate(f.stripExtension))
          case None => throw ELPIError("no ELPI file known for theory " + from)
        }
      case _: RuleConstant =>
        Nil // ignored
      case _: Structure =>
        Nil // redundant after flattening
      case _ =>
        throw ELPIError("unknown declaration: " + d.path)
    }
  }

  def exportTheory(thy: Theory, bf: BuildTask): Unit = {
    val thyE = translateTheory(thy)
    rh << thyE.toELPI
  }

  def exportView(view: View, bf: BuildTask): Unit = {}
  def exportDocument(doc: Document, bf: BuildTask): Unit = {}
  def exportNamespace(dpath: DPath, bd: BuildTask, namespaces: List[BuildTask], modules: List[BuildTask]): Unit = {}
}

private class VarCounter {
  private var i = 0
  def next(upper: Boolean) : LocalName = {
    i += 1
    val base = if (upper) "X" else "x"
    LocalName(base + i.toString)
  }
}

trait ConstantHandler {
  /** called on every constant. */
  def handle(c : Constant) : List[ELPI.Decl]
  /** may be used to generate boilerplate code before processing.
    * depending on the exporter, this may not get called. */
  def setup() : List[ELPI.Decl] = List()
  /** may be used to generate boilerplate code after processing.
    * depending on the exporter, this may not get called. */
  def finish() : List[ELPI.Decl] = List()

  def elpiRename(ln : LocalName) : LocalName = ELPIExporter.elpiRename(ln)
}

class ConstantHandlerSequence(handlers : List[ConstantHandler]) extends ConstantHandler {
  def handle(c : Constant) : List[ELPI.Decl] = handlers.flatMap(_.handle(c))
  override def setup()     : List[ELPI.Decl] = handlers.flatMap(_.setup())
  override def finish()    : List[ELPI.Decl] = handlers.flatMap(_.finish())
}

class GeneratedFromHandler(controller : Controller) extends ConstantHandler {
  def handle(c : Constant) : List[ELPI.Decl] = {
    println(">>> genfromhan constant: " + c.name + " / " + c.toString)
    List(ELPI.Comment(s"generated from ${c.name} : ${controller.presenter.asString(c.tp.get)}"))
  }
}

abstract class BaseConstantHandler(handlerName : String) extends ConstantHandler {
  def fail(c: Constant, msg: String): ELPI.Decl = {
    ELPI.Comment(c.path + ": " + handlerName + ": skipping due to error: " + msg)
  }
}

class IfElseHandler(a : ConstantHandler, b : ConstantHandler, useA : Constant => Boolean) extends ConstantHandler() {
  override def setup(): List[ELPI.Decl] = a.setup() ++ b.setup()
  override def finish(): List[ELPI.Decl] = a.setup() ++ b.setup()
  def handle(c : Constant) : List[ELPI.Decl] =
    if (useA(c)) a.handle(c) else b.handle(c)
}

class TypeHandler() extends BaseConstantHandler("th") {
  def handle(c : Constant) : List[ELPI.Decl] = {
    c.tp match {
      case Some(t) =>
        val tE = translateTerm(t)
        val isKind = t match {
          case FunType(_, OMS(Typed.ktype)) => true
          case _ => false
        }
        val decl = ELPI.Data(c.name, tE, isKind)
        List(decl) // TODO check how to write dependent types in ELPI
      case _ => List()
    }
  }
}

abstract class JudgmentHandler(handlerName : String, ruleMatcher : RuleMatcher) extends BaseConstantHandler(handlerName) {
  /** called when a new judgment is introduced.
    * e.g. in "ded : type -> prop | role Judgment ||" */
  def onIntro(c : Constant, vc : VarCounter) : List[ELPI.Decl] = List()

  /** called on rules.
    * e.g. on "andI : {A,B} ded A -> ded B -> ded (and A B)" */
  def onRule(c : Constant, dr : DeclarativeRule, vc : VarCounter) : List[ELPI.Decl]

  def handle(c: Constant) : List[ELPI.Decl] = {
    val vc = new VarCounter
    if (c.rl contains "Judgment") {
      onIntro(c, vc)
    } else {
      c.tp match {
        case Some(t) =>
          t match {
            case ruleMatcher.Rule(dr) => onRule(c, dr, vc)
            case _ => List()
          }
        case _ => List()
      }
    }
  }

  /* HELPER METHODS */
  def V(n: LocalName) : ELPI.Variable = ELPI.Variable(n)
  val hypSuffix = "hyp"

  def getArgVars(c : Constant, vc : VarCounter) : List[ELPI.Variable] = {
    c.tp match {
      case Some(FunType(args, _)) =>
        (1 to args.length).toList.map(_ => ELPI.Variable(vc.next(upper = true)))
    }
  }

  def cap(dr : DeclarativeRule) : DeclarativeRule = {
    return dr
  }

  /** translates a complex judgment to the corresponding lambda-Prolog predicate
    * Example: translateComplex(ded A -> ded B) should become pi x1 \ ded/hyp x1 A => ded (X2 x1) B
    */
  def translateComplex(cj: ComplexJudgement)(implicit vc: VarCounter) : (LocalName, ELPI.Expr) = {
    // for parameters: get the name, ignoring the type; for assumptions: translate the judgment and generate a name
    val parNames = cj.parameters.map {vd => vd.name}
    val (hypNames, hypEs) = cj.hypotheses.map {a => translateAtomic(a, Nil, hypothesis = true)}.unzip
    val names = parNames ::: hypNames
    // translate the conclusion, return the generated name as the name for the entire complex judgment
    val (thesisName, thesisE) = translateAtomic(cj.thesis, names, hypothesis = false)
    // quantify over all names, hypothesis implies conclusion
    val cjE = ELPI.Forall(names, ELPI.Impl(hypEs, thesisE))
    (thesisName, cjE)
  }

  /** translates an atomic judgment to the corresponding lambda-Prolog predicate
    *  @param aj the judgment
    *  @param hypNames if conclusion of hypothetical judgment: the names of the hypotheses
    *  @param hypothesis this is a hypothesis of a complex judgment
    *
    * Example: translateAtomic(ded A, [x], false) should become (X1, "ded (X1 x) A")
    * Example: translateAtomic(ded A, [], true) should become (x1, "ded/hyp x1 A")
    */
  def translateAtomic(aj: AtomicJudgement, hypNames: List[LocalName], hypothesis: Boolean)(implicit vc: VarCounter) : (LocalName, ELPI.Expr) = {
    val name = vc.next(!hypothesis)
    val argsE = aj.arguments map translateTerm
    val nameExpr = V(name)(hypNames)
    val opName = aj.operator.name
    // for theses/conclusions: judgment symbol name applied to hypothesis names
    // technical modification for a hypothetis: apply the -hyp predicate of the judgment symbol instead
    // rules for -hyp predicate are generated when the judgment symbol is exported
    val opNameH = if (hypothesis) opName / hypSuffix else opName
    val e = V(opNameH)(nameExpr :: argsE :_*)
    (name, e)
  }

  /* Like translateAtomic without hypotheses etc., but replaces Variable applications.
  *  example: For forall-elimination we want to have
  * `ded X (forall P) :- ..., P = F T, ...` instead of `ded X (forall (F T)) :- ...
  */
  def translateConclusion(aj : AtomicJudgement)(implicit vc: VarCounter) : (List[LocalName], ELPI.Expr, List[ELPI.Expr]) = {
    val cert = vc.next( upper = true)
    var names : List[LocalName] = List()
    val (argsE, extra) : (List[ELPI.Expr], List[List[ELPI.Expr]]) = aj.arguments.map {
      case ApplySpine(OMV(f), a) =>
        val v = vc.next(upper = true)
        names = v :: names
        (V(v), List(ELPI.Equal(V(v), translateTerm(ApplySpine(OMV(f), a :_*)))))
      case x => (translateTerm(x), List())
    }.unzip

    val e = V(aj.operator.name)(V(cert) :: argsE :_*)
    (cert :: names, e, extra.flatten)
  }

  def getParNames(dr: DeclarativeRule)(implicit vc: VarCounter): (List[LocalName], List[ELPI.Expr]) = {
    val parNames = dr.arguments.collect {
      case RuleParameter(n,_) => elpiRename(n)
    }
    // have to add extra ones (for case covered by translateConclusion)
    val (names, _, extras) = translateConclusion(dr.conclusion)
    (names.drop(1) ::: parNames, extras)
  }

}

class MainJudgmentHandler(ruleMatcher : RuleMatcher) extends JudgmentHandler(handlerName = "mj", ruleMatcher) {
  override def onIntro(c : Constant, vc : VarCounter) : List[ELPI.Decl] = {
    val argNames = getArgVars(c, vc)
    val certName = vc.next(upper = true)
    val cert = ELPI.Variable(certName)
    val hypName = ELPI.Variable(vc.next(upper = true))
    val right = ELPI.Variable(c.name)(cert :: argNames :_*)
    val left1 = ELPI.Variable(c.name / hypSuffix)(hypName :: argNames :_*)
    val left2 = HelpCons(c.path)(argNames ::: List(hypName, cert) :_*)
    val ruleE = ELPI.Impl(List(left1,left2),right)
    List(ELPI.Rule(ruleE))
  }

  /** translates an LF rule into the corresponding lambda-Prolog rule with an additional helper predicate as a side condition
    *  the helper predicate takes all inputs and the output of the rule as arguments and can be used to
    *  - store the proot term (if query variable), supply a proof term to check (if argument given)
    *  - guide the proof search by controlling when a rule is applicable
    *  - control the proof search, e.g., by providing the search depth
    */
  def onRule(c: Constant, dr: DeclarativeRule, vc: VarCounter) : List[ELPI.Decl] = {
    // for parameters: just the given name, ignoring the type; for assumptions: a generated name and the judgment
    val (argNames,assOs) = dr.arguments.map {
      case RuleParameter(n,_) =>
        (elpiRename(n),None) // TODO make sure all parNames start with upper case letter (because printer drops outermost pi's)
      case RuleAssumption(cj) =>
        val (n, e) = translateComplex(cj)(vc)
        (elpiRename(n), Some(e))
    }.unzip
    val assEs = assOs.flatMap(_.toList)
    // the conclusion and a generated name for it
    val (concNames, concE, concExtra) = translateConclusion(dr.conclusion)(vc)
    val names = (concNames ::: argNames)
    // helper judgment: c/help applied to all names; providing rules for this judgment allows guiding the prover
    val help = HelpCons(c.path)(names)
    // quantify over all names, assumptions imply conclusion, with helper judgment as side condition
    val r = ELPI.Forall(names, ELPI.Impl(help::(concExtra:::assEs), concE))
    List(ELPI.Rule(r))
  }
}


class ProductRuleHandler(ruleMatcher : RuleMatcher) extends JudgmentHandler(handlerName = "prod", ruleMatcher) {
  private object ProdCertCons extends ELPI.BuiltInConstant(name = "prodcert")
  override def onIntro(c : Constant, vc : VarCounter) : List[ELPI.Decl] = {
    val argNames = getArgVars(c, vc)
    val certName = vc.next(upper = true)
    val hypName = ELPI.Variable(vc.next(upper = true))
    val cert1 = ELPI.Variable(certName / "1")
    val cert2 = ELPI.Variable(certName / "2")
    val rightProd = HelpCons(c.path)(argNames ::: List(hypName, ProdCertCons(cert1, cert2)): _*)
    val leftProd1 = HelpCons(c.path)(argNames ::: List(hypName, cert1): _*)
    val leftProd2 = HelpCons(c.path)(argNames ::: List(hypName, cert2): _*)
    List(ELPI.Rule(ELPI.Impl(List(leftProd1, leftProd2), rightProd)))
  }

  override def onRule(c: Constant, dr: DeclarativeRule, vc: VarCounter): List[ELPI.Decl] = {
    // for parameters: just the name; for assumptions: the lambda-Prolog judgment and two generated names
    val (parNames, _) = getParNames(dr)(vc)

    val (assNamePairs, assExprs) = dr.arguments.collect {
      case RuleAssumption(cj) =>
        val (namePair, e) = productRuleConc(cj, vc)
        (namePair, e)
    }.unzip

    val (assNames1,assNames2) = assNamePairs.unzip
    val certName = vc.next(upper = true)
    // e1: first helper predicate applies to a list of inputs with output certName1
    val certName1 = certName / "1"
    // val e1 = HelpCons(c.path, "1")(parNames ::: assNames1 ::: List(certName1))
    val e1 = HelpCons(c.path)(certName1 :: parNames ::: assNames1)
    // e2: second helper predicate applies to another list of inputs with output certName2
    val certName2 = certName / "2"
    // val e2 = HelpCons(c.path, "2")(parNames ::: assNames2 ::: List(certName2))
    val e2 = HelpCons(c.path)(certName2 :: parNames ::: assNames2)
    // e: product helper predicate applies to the pair of certName1 and certName2
    val e =  HelpCons(c.path)(ProdCertCons(List(certName1,certName2)) :: parNames.map(V) ::: assExprs :_*)
    val r = ELPI.Forall(parNames ::: assNames1 ::: assNames2 ::: List(certName1,certName2), ELPI.Impl(List(e1,e2),e))
    List(ELPI.Rule(r))
  }

  /** auxiliary function of productRule: translates the conclusions and generates names for them */
  private def productRuleConc(cj: ComplexJudgement, vc: VarCounter) : ((LocalName,LocalName), ELPI.Expr) = {
    val parNames = cj.parameters.map {vd => vd.name}
    val hypNames = cj.hypotheses.map {_ => vc.next(upper = false)}
    val certName = vc.next(upper = true)
    val certName1 = certName / "1"
    val certName2 = certName / "2"
    val names = parNames:::hypNames
    val res = ELPI.Lambda(names,ProdCertCons(V(certName1)(names),V(certName2)(names)))
    ((certName1,certName2), res)
  }
}

class IterativeDeepeningHandler(ruleMatcher : RuleMatcher) extends JudgmentHandler(handlerName = "id", ruleMatcher) {
  object IdCertCons extends ELPI.BuiltInConstant(name = "idcert")
  override def onIntro(c : Constant, vc : VarCounter) : List[ELPI.Decl] = {
    val argNames = getArgVars(c, vc)
    val hypName = ELPI.Variable(vc.next(upper = true))
    val rightID = HelpCons(c.path)(argNames:::List(hypName,IdCertCons(V(vc.next(upper = true)))) :_*)
    List(ELPI.Rule(ELPI.Impl(List(), rightID)))
  }

  override def onRule(c: Constant, dr: DeclarativeRule, vc: VarCounter): List[ELPI.Decl] = {
    val (parNames, _) = getParNames(dr)(vc)

    val assCertName = vc.next(upper = true)
    val (assNames, assExprs) = dr.arguments.collect {
      case RuleAssumption(cj) =>
        val parNames = cj.parameters.map { vd => vd.name }
        val hypNames = cj.hypotheses.map { a => vc.next(upper = false) }
        val names = parNames ::: hypNames
        // val res = ELPI.Lambda(names, IdCertCons(V(assCertName)(names)))
        val res = ELPI.Lambda(names, IdCertCons(V(assCertName)))
        (assCertName, res)
    }.unzip
    val certName = vc.next(upper = true)
    val res = HelpCons(c.path)(IdCertCons(List(certName)) :: parNames.map(V) ::: assExprs :_*)
    val cond1 = ELPI.GreaterThan(ELPI.Variable(certName), ELPI.Integer(0))
    val cond2 = ELPI.Is(ELPI.Variable(assCertName), ELPI.Minus(ELPI.Variable(certName), ELPI.Integer(1)))
    val r = ELPI.Forall(parNames ::: assNames ::: List(certName), ELPI.Impl(List(cond1, cond2),res))
    List(ELPI.Rule(r))
  }
}

class BackChainingHandler(ruleMatcher : RuleMatcher) extends JudgmentHandler("bc", ruleMatcher) {

  object BcCertCons extends ELPI.BuiltInConstant(name = "bccert")

  override def onIntro(c: Constant, vc: VarCounter): List[ELPI.Decl] = {
    val argNames = getArgVars(c, vc)
    val hypName = ELPI.Variable(vc.next(upper = true))
    val cert = ELPI.Variable(vc.next(upper = true))
    val rightBC = HelpCons(c.path)(argNames:::List(hypName,BcCertCons(cert)) :_*)
    List(ELPI.Rule(ELPI.Impl(List(), rightBC)))
  }

  override def onRule(c: Constant, dr: DeclarativeRule, vc: VarCounter): List[ELPI.Decl] = {

    val isForward = c.rl.contains("ForwardRule")
    val (parNames, extras) = getParNames(dr)(vc)

    // val conclVars = getVars(dr.conclusion.arguments.head)
    val needBC = isForward || c.rl.contains("EliminationRule") // parNames.exists(n => !conclVars.contains(n))

    val assCertName = vc.next(upper = true)
    var isFirstArg = true
    var firstExpr : Option[ELPI.Expr] = None
    val (assNames, assExprs) = dr.arguments.collect {
      case RuleAssumption(cj) =>
        val parNames = cj.parameters.map { vd => vd.name }
        val hypNames = cj.hypotheses.map { a => vc.next(upper = false) }
        val names = parNames ::: hypNames
        val res = if (isFirstArg && needBC) {
          firstExpr = Some(translateTerm(cj.thesis.arguments.head))
          ELPI.Lambda(names, BcCertCons(V(LocalName("bc/fwdLocked"))(V(assCertName))))
        } else {
          ELPI.Lambda(names, BcCertCons(V(assCertName)))
        }
        isFirstArg = false
        (assCertName, res)
    }.unzip
    val certName = vc.next(upper = true)
    val res = HelpCons(c.path)(BcCertCons(List(certName)) :: parNames.map(V) ::: assExprs  :_*)
    val certval = ELPI.Variable(vc.next(upper = true))
    val conds = if (needBC) {
      val cond1 = V(LocalName("bc/val"))(V(certName), certval)   // TODO: bc/pos suffices if not ForwardRule
      val cond2 = ELPI.GreaterThan(certval, ELPI.Integer(0))
      val cond3 = ELPI.Is(ELPI.Variable(assCertName), ELPI.Minus(certval, ELPI.Integer(1)))
      val cond4 = V(LocalName("bc/fwdable"))(firstExpr.get)
      List(cond1, cond2, cond3, cond4)
    } else {
      // val cond1 = ELPI.GreaterThan(V(certName), ELPI.Integer(0))
      val cond1 = V(LocalName("bc/pos"))(V(certName))
      val cond2 = ELPI.Is(ELPI.Variable(assCertName), ELPI.Minus(V(certName), ELPI.Integer(1)))
      List(cond1, cond2)
    }
    val r = ELPI.Forall(parNames ::: assNames ::: List(certName), ELPI.Impl(conds ::: extras,res))
    if (isForward) {
      val auxres = ELPI.Variable(vc.next(upper = true))
      val concE = translateTerm(dr.conclusion.arguments.head)
      val aux = ELPI.Forall(parNames, ELPI.Impl(V(LocalName("bc/aux"))(concE, auxres),
        V(LocalName("bc/aux"))(firstExpr.get, auxres)))
      List(ELPI.Rule(r), ELPI.Rule(aux))
    } else {
      List(ELPI.Rule(r))
    }
  }
}


class ProofTermHandler(ruleMatcher : RuleMatcher) extends JudgmentHandler("pt", ruleMatcher) {
  object PtCertCons extends ELPI.BuiltInConstant("ptcert")

  override def onIntro(c : Constant, vc : VarCounter) : List[ELPI.Decl] = {
    val argNames = getArgVars(c, vc)
    val hypName = ELPI.Variable(vc.next(upper = true))
    val rightPT = HelpCons(c.path)(argNames:::List(hypName,PtCertCons(V(LocalName("i"))(argNames :_*))) :_*)
    List(ELPI.Rule(ELPI.Impl(List(), rightPT)))
  }

  override def onRule(c: Constant, dr: DeclarativeRule, vc: VarCounter): List[ELPI.Decl] = {
    val (parNames, _) = getParNames(dr)(vc)

    val (assNames, assExprs) = dr.arguments.collect {
      case RuleAssumption(cj) =>
        val parNames = cj.parameters.map { vd => vd.name }
        val hypNames = cj.hypotheses.map { a => vc.next(upper = false) }
        val names = parNames ::: hypNames
        val assCertName = vc.next(upper = true)
        val res = ELPI.Lambda(names, PtCertCons(V(assCertName)))
        (assCertName, res)
    }.unzip
    val newCert = PtCertCons(V(c.path.name)(parNames.map(V) ::: assNames.map(V): _*))
    val res = HelpCons(c.path)(newCert :: parNames.map(V) ::: assExprs: _*)
    val r = ELPI.Forall(parNames ::: assNames, ELPI.Impl(List(), res))
    List(ELPI.Rule(r))
  }
}

/**
  * Counts how often a rule has been applied
  */
class RuleUseHandler(ruleMatcher: RuleMatcher) extends JudgmentHandler("u", ruleMatcher) {
  override def onRule(c: Constant, dr: DeclarativeRule, vc: VarCounter): List[ELPI.Decl] = {
    val (parNames, _) = getParNames(dr)(vc)
    val certName = vc.next(upper = true)
    var uExpr : Option[ELPI.Expr] = None
    val assExprs = dr.arguments.collect {
      case RuleAssumption(cj) =>
        val parNames = cj.parameters.map { vd => vd.name }
        val hypNames = cj.hypotheses.map { a => vc.next(upper = false) }
        val names = parNames ::: hypNames
        if (uExpr.isEmpty) {
          uExpr = Some(translateComplex(cj)(vc)._2)
        }
        val ucert = V(LocalName("ucert"))(V(LocalName("prep"))(uExpr.get, V(certName)))
        ELPI.Lambda(names, ucert)
    }
    if (uExpr.isEmpty) {
      // return ELPI.Comment("Can't create helper for ucert: no hypotheses found")
      return List(fail(c, "can't create helper: no hypotheses found"))
    }
    val res = HelpCons(c.path)(V(LocalName("ucert"))(V(certName)) :: parNames.map(V) ::: assExprs :_*)
    val cond = V(LocalName("occatmost"))(uExpr.get, ELPI.Integer(if (c.rl.contains("ApplyRepeatedly")) 4 else 1), V(certName))
    val r = ELPI.Forall(parNames, ELPI.Impl(List(uExpr.get, cond),res))
    List(ELPI.Rule(r))
  }
}

/**
  * simply hands something down. Can e.g. be used to return errors.
  */
class HandDownHandler(ruleMatcher: RuleMatcher, name : String) extends JudgmentHandler(handlerName = "hd" + name, ruleMatcher) {
  override def onRule(c: Constant, dr: DeclarativeRule, vc: VarCounter): List[ELPI.Decl] = {
    val (parNames, _) = getParNames(dr)(vc)

    val hdcert = V(LocalName("hd" + name + "cert"))(V(vc.next(upper = true)))
    val assExprs = dr.arguments.collect {
      case RuleAssumption(cj) =>
        val parNames = cj.parameters.map { vd => vd.name }
        val hypNames = cj.hypotheses.map { _ => vc.next(upper = false) }
        val names = parNames ::: hypNames
        ELPI.Lambda(names, hdcert)
    }
    val res = HelpCons(c.path)(hdcert :: parNames.map(V) ::: assExprs :_*)
    val r = ELPI.Forall(parNames, ELPI.Impl(List(),res))
    List(ELPI.Rule(r))
  }
}

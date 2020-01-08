package info.kwarc.mmt.lf.elpi

import info.kwarc.mmt.api._
import archives._
import symbols._
import modules._
import objects._
import documents._
import info.kwarc.mmt.lf._

case class ELPIError(msg: String) extends Error(msg)

object HelpCons {
  def apply(path: GlobalName) = ELPI.Variable(LocalName("help") / path.name)
  def apply(path: GlobalName, suffix: String) = ELPI.Variable(LocalName("help") / path.name / suffix)
}

object ProdCertCons extends ELPI.Constant("prodcert")
object IdCertCons extends ELPI.Constant("idcert")
object PtCertCons extends ELPI.Constant("ptcert")
object BcCertCons extends ELPI.Constant("bccert")

class ELPIExporter extends Exporter {
  val key = "lf-elpi"
  override def outExt = "elpi"

  private lazy val lup = controller.globalLookup
  private lazy val ruleMatcher = new RuleMatcher(lup, List("Judgment","Judgement"))

  private def translateTheory(thy: Theory): ELPI.Program = {
    val cons = thy.getDeclarations
    val consE = cons flatMap translateDeclaration
    ELPI.Program(consE:_*)
  }

  private def translateDeclaration(d: Declaration): List[ELPI.Decl] = {
    def fail(msg: String) = {
      println(msg)
      List(ELPI.Comment("skipping due to error: " + d.path + "\n" + msg))
    }
    d match {
      case c: Constant =>
        val comment = ELPI.Comment(s"generated from ${c.name} : ${controller.presenter.asString(c.tp.get)}")
        if (c.rl contains "Judgment") {
          c.tp match {
            case Some(FunType(args,_)) =>
              implicit val varCounter = new VarCounter
              val argNames = (1 to args.length).toList.map(_ => ELPI.Variable(varCounter.next(true)))
              val certName = varCounter.next(true)
              val cert = ELPI.Variable(certName)
              val hypName = ELPI.Variable(varCounter.next(true))
              val right = ELPI.Variable(c.name)(cert :: argNames :_*)
              val left1 = ELPI.Variable(c.name / hypSuffix)(hypName :: argNames :_*)
              val left2 = HelpCons(c.path)(argNames ::: List(hypName, cert) :_*)
              val ruleE = ELPI.Impl(List(left1,left2),right)
              val rule = ELPI.Rule(ruleE)
              // helper rule for iterative deepening
              val cert1 = ELPI.Variable(certName / "1")
              val cert2 = ELPI.Variable(certName / "2")
              val rightProd = HelpCons(c.path)(argNames:::List(hypName,ProdCertCons(cert1, cert2)) :_*)
              val leftProd1 = HelpCons(c.path)(argNames:::List(hypName, cert1) :_*)
              val leftProd2 = HelpCons(c.path)(argNames:::List(hypName, cert2) :_*)
              val ruleProd = ELPI.Rule(ELPI.Impl(List(leftProd1, leftProd2), rightProd))
              // helper rule for iterative deepening
              val rightID = HelpCons(c.path)(argNames:::List(hypName,IdCertCons(cert)) :_*)
              val ruleID = ELPI.Rule(ELPI.Impl(List(), rightID))
              // helper rule for proof terms
              val rightPT = HelpCons(c.path)(argNames:::List(hypName,PtCertCons(V(LocalName("i"))(argNames :_*))) :_*)
              val rulePT = ELPI.Rule(ELPI.Impl(List(), rightPT))
              List(comment, rule, ruleID, rulePT, ruleProd)
          }
        } else {
          c.tp match {
            case Some(t) =>
              t match {
                case ruleMatcher.Rule(dr) =>
                  try {
                    val mainRule = translateRule(c, dr)(new VarCounter)
                    val pr = productRule(c, dr)(new VarCounter) // boilerplate for taking the cartesian product of two helper judgment definitions
                    // helper for iterative deepening
                    val idr = idRule(c, dr)(new VarCounter)
                    // helper for proof terms
                    val ptr = ptRule(c, dr)(new VarCounter)
                    // helper for backchaining
                    // val bcr = bcRule(c, dr)(new VarCounter)
                    List(comment, mainRule, pr, idr, ptr)
                  } catch {case ELPIError(msg) =>
                    fail(msg)
                  }
                case _ =>
                  val tE = translateTerm(t)
                  val isKind = t match {
                    case FunType(_, OMS(Typed.ktype)) => true
                    case _ => false
                  }
                  val decl = ELPI.Data(c.name, tE, isKind)
                  List(comment, ELPI.Comment(decl.toELPI)) // TODO check how to write dependent types in ELPI
              }
            case None =>
              throw ELPIError("cannot translate untyped constant: " + c.path)
          }
        }
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

  /** translates an LF rule into the corresponding lambda-Prolog rule with an additional helper predicate as a side condition
   *  the helper predicate takes all inputs and the output of the rule as arguments and can be used to
   *  - store the proot term (if query variable), supply a proof term to check (if argument given)
   *  - guide the proof search by controlling when a rule is applicable
   *  - control the proof search, e.g., by providing the search depth
   */
  private def translateRule(c: Constant, dr: DeclarativeRule)(implicit vc: VarCounter) : ELPI.Rule = {
    // for parameters: just the given name, ignoring the type; for assumptions: a generated name and the judgment
    val (argNames,assOs) = dr.arguments.map {
      case RuleParameter(n,_) => (n,None) // TODO make sure all parNames start with upper case letter (because printer drops outermost pi's)
      case RuleAssumption(cj) =>
        val (n, e) = translateComplex(cj)
        (n, Some(e))
    }.unzip
    val assEs = assOs.flatMap(_.toList)
    // the conclusion and a generated name for it
    val (concName, concE) = translateAtomic(dr.conclusion, Nil, false)
    val names = argNames ::: List(concName)
    // helper judgment: c/help applied to all names; providing rules for this judgment allows guiding the prover
    val help = HelpCons(c.path)(names)
    // quantify over all names, assumptions imply conclusion, with helper judgment as side condition
    val r = ELPI.Forall(names, ELPI.Impl(help::assEs, concE))
    ELPI.Rule(r)
  }

   /** translates a complex judgment to the corresponding lambda-Prolog predicate */
   private def translateComplex(cj: ComplexJudgement)(implicit vc: VarCounter) : (LocalName, ELPI.Expr) = {
    // for parameters: get the name, ignoring the type; for assumptions: translate the judgment and generate a name
    val parNames = cj.parameters.map {vd => vd.name}
    val (hypNames, hypEs) = cj.hypotheses.map {a => translateAtomic(a, Nil, true)}.unzip
    val names = parNames ::: hypNames
    // translate the conclusion, return the generated name as the name for the entire complex judgment
    val (thesisName, thesisE) = translateAtomic(cj.thesis, names, false)
    // quantify over all names, hypothesis implies conclusion
    val cjE = ELPI.Forall(names, ELPI.Impl(hypEs, thesisE))
    (thesisName, cjE)
  }

  /** translates an atomic judgment to the corresponding lambda-Prolog predicate
   *  @param aj the judgment
   *  @param hypNames if conclusion of hypothetical judgment: the names of the hypotheses
   *  @param hypothesis this is a hypothesis of a complex judgment
   */
  private def translateAtomic(aj: AtomicJudgement, hypNames: List[LocalName], hypothesis: Boolean)(implicit vc: VarCounter) : (LocalName, ELPI.Expr) = {
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

  /** back chaining helper (doesn't work yet) */
  private def bcRule(c: Constant, dr: DeclarativeRule)(implicit vc: VarCounter) : ELPI.Rule = {
    val isForward = c.rl.map(_ == "ForwardRule").getOrElse(false)
    val parNames = dr.arguments.collect {
      case RuleParameter(n,_) => n
    }
    val assCertName = vc.next(true)
    var isFirstArg = true
    val (assNames, assExprs) = dr.arguments.collect {
      case RuleAssumption(cj) =>
        val parNames = cj.parameters.map { vd => vd.name }
        val hypNames = cj.hypotheses.map { a => vc.next(false) }
        val names = parNames ::: hypNames
        val res = if (isFirstArg && isForward) {
          ELPI.Lambda(names, BcCertCons(V(LocalName("bc/xx"))(V(assCertName))))
        } else {
          ELPI.Lambda(names, BcCertCons(V(assCertName)))
        }
        isFirstArg = false
        (assCertName, res)
    }.unzip
    val certName = vc.next(true)
    val res = HelpCons(c.path)(parNames.map(V) ::: assExprs ::: List(BcCertCons(List(certName))) :_*)
    val certval = ELPI.Variable(vc.next(true))
    val cond1 = V(LocalName("bc/val"))(V(certName), certval)
    val cond2 = ELPI.GreaterThan(certval, ELPI.Integer(0))
    val cond3 = ELPI.Is(ELPI.Variable(assCertName), ELPI.Minus(certval, ELPI.Integer(1)))
    val backchainingcond = if (isForward) {
      List()  // TODO: call backchaining predicate with correct formula
    } else {
      List()
    }
    val r = ELPI.Forall(parNames ::: assNames ::: List(certName), ELPI.Impl(List(cond1, cond2, cond3):::backchainingcond,res))
    ELPI.Rule(r)
  }

  /** proof term helper */
  private def ptRule(c: Constant, dr: DeclarativeRule)(implicit vc: VarCounter) : ELPI.Rule = {
    val parNames = dr.arguments.collect {
      case RuleParameter(n,_) => n
    }
    val assCertName = vc.next(true)
    val (assNames, assExprs) = dr.arguments.collect {
      case RuleAssumption(cj) =>
        val parNames = cj.parameters.map { vd => vd.name }
        val hypNames = cj.hypotheses.map { a => vc.next(false) }
        val names = parNames ::: hypNames
        val res = ELPI.Lambda(names, PtCertCons(V(assCertName)))
        (assCertName, res)
    }.unzip
    val newCert = PtCertCons(V(c.path.name)(parNames.map(V) ::: assNames.map(V) :_*))
    val res = HelpCons(c.path)(parNames.map(V) ::: assExprs ::: List(newCert) :_*)
    val r = ELPI.Forall(parNames ::: assNames, ELPI.Impl(List(),res))
    ELPI.Rule(r)
  }

  /** iterative deepening helper */
  private def idRule(c: Constant, dr: DeclarativeRule)(implicit vc: VarCounter) : ELPI.Rule = {
    val parNames = dr.arguments.collect {
      case RuleParameter(n,_) => n
    }
    val assCertName = vc.next(true)
    val (assNames, assExprs) = dr.arguments.collect {
      case RuleAssumption(cj) =>
        val parNames = cj.parameters.map { vd => vd.name }
        val hypNames = cj.hypotheses.map { a => vc.next(false) }
        val names = parNames ::: hypNames
        // val res = ELPI.Lambda(names, IdCertCons(V(assCertName)(names)))
        val res = ELPI.Lambda(names, IdCertCons(V(assCertName)))
        (assCertName, res)
    }.unzip
    val certName = vc.next(true)
    val res = HelpCons(c.path)(parNames.map(V) ::: assExprs ::: List(IdCertCons(List(certName))) :_*)
    val cond1 = ELPI.GreaterThan(ELPI.Variable(certName), ELPI.Integer(0))
    val cond2 = ELPI.Is(ELPI.Variable(assCertName), ELPI.Minus(ELPI.Variable(certName), ELPI.Integer(1)))
    val r = ELPI.Forall(parNames ::: assNames ::: List(certName), ELPI.Impl(List(cond1, cond2),res))
    ELPI.Rule(r)
  }


  /** boilerplate rule for taking the Cartesian product of two helper predicates for a rule */
  private def productRule(c: Constant, dr: DeclarativeRule)(implicit vc: VarCounter) : ELPI.Rule = {
    // for parameters: just the name; for assumptions: the lambda-Prolog judgment and two generated names
    val parNames = dr.arguments.collect {
      case RuleParameter(n,_) => n
    }
    val (assNamePairs, assExprs) = dr.arguments.collect {
      case RuleAssumption(cj) =>
        val (namePair, e) = productRuleConc(cj)
        (namePair, e)
    }.unzip

    val (assNames1,assNames2) = assNamePairs.unzip
    val certName = vc.next(true)
    // e1: first helper predicate applies to a list of inputs with output certName1
    val certName1 = certName / "1"
    // val e1 = HelpCons(c.path, "1")(parNames ::: assNames1 ::: List(certName1))
    val e1 = HelpCons(c.path)(parNames ::: assNames1 ::: List(certName1))
    // e2: second helper predicate applies to another list of inputs with output certName2
    val certName2 = certName / "2"
    // val e2 = HelpCons(c.path, "2")(parNames ::: assNames2 ::: List(certName2))
    val e2 = HelpCons(c.path)(parNames ::: assNames2 ::: List(certName2))
    // e: product helper predicate applies to the pair of certName1 and certName2
    val e =  HelpCons(c.path)(parNames.map(V) ::: assExprs ::: List(ProdCertCons(List(certName1,certName2))) :_*)
    val r = ELPI.Forall(parNames ::: assNames1 ::: assNames2 ::: List(certName1,certName2), ELPI.Impl(List(e1,e2),e))
    ELPI.Rule(r)
  }

  /** auxiliary function of productRule: translates the conclusions and generates names for them */
  private def productRuleConc(cj: ComplexJudgement)(implicit vc: VarCounter) : ((LocalName,LocalName), ELPI.Expr) = {
    val parNames = cj.parameters.map {vd => vd.name}
    val hypNames = cj.hypotheses.map {a => vc.next(false)}
    val certName = vc.next(true)
    val certName1 = certName / "1"
    val certName2 = certName / "2"
    val names = parNames:::hypNames
    val res = ELPI.Lambda(names,ProdCertCons(V(certName1)(names),V(certName2)(names)))
    ((certName1,certName2), res)
   }

  private class VarCounter {
    private var i = 0
    def next(upper: Boolean) = {
      i += 1
      val base = if (upper) "X" else "x"
      LocalName(base + i.toString)
    }
  }
  
  
  private val hypSuffix = "hyp"
  
  private def V(n: LocalName) = ELPI.Variable(n)
  
  /** straightforward translation of an LF terms to a lambda-Prolog term */
  private def translateTerm(t: Term): ELPI.Expr = {
    t match {
      case OMS(p) =>
        // also works for "type" because it is called the same in ELPI
        ELPI.Variable(p.name)
      case OMV(n) =>
        ELPI.Variable(n)
      case Lambda(x,_,t) =>
        ELPI.Lambda(x, translateTerm(t))
      case ApplySpine(f,args) =>
        val fE = translateTerm(f)
        val argsE = args map translateTerm
        fE(argsE :_*)
      case Arrow(a,b) =>
        val aE = translateTerm(a)
        val bE = translateTerm(b)
        ELPI.Arrow(aE,bE)
      case Pi(x,_,b) =>
        ELPI.Forall(x, translateTerm(b))
      case _ => throw ELPIError("unknown term: " + t)
    }
  }  
  
  def exportTheory(thy: Theory, bf: BuildTask) {
    val thyE = translateTheory(thy)
    rh << thyE.toELPI
  }

  def exportView(view: View, bf: BuildTask) {}
  def exportDocument(doc: Document, bf: BuildTask) {}
  def exportNamespace(dpath: DPath, bd: BuildTask, namespaces: List[BuildTask], modules: List[BuildTask]) {}
}
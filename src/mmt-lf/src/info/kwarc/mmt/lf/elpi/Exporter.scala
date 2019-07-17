package info.kwarc.mmt.lf.elpi

import info.kwarc.mmt.api._
import archives._
import libraries._
import symbols._
import modules._
import objects._
import documents._

import info.kwarc.mmt.lf._

case class ELPIError(msg: String) extends Error(msg)

object HelpCons {
  def apply(path: GlobalName) = ELPI.Variable(LocalName("help") / path.name)
}

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
  
  private def translateDeclaration(d: Declaration): Option[ELPI.Decl] = {
    def fail(msg: String) = {
      println(msg)
      ELPI.Comment("skipping due to error: " + d.path + "\n" + msg)
    }
    d match {
      case c: Constant =>
        implicit val varCounter = new VarCounter
        def addComment(r: ELPI.Rule) = r.copy(comment = Some(s"generated from ${c.name} : ${controller.presenter.asString(c.tp.get)}"))
        if (c.rl contains "Judgment") {
          c.tp match {
            case Some(FunType(args,_)) =>
              val argNames = (1 to args.length).toList.map(_ => ELPI.Variable(varCounter.next(true)))
              val certName = ELPI.Variable(varCounter.next(true))
              val hypName = ELPI.Variable(varCounter.next(true))
              val right = ELPI.Variable(c.name)(certName :: argNames :_*)
              val left1 = ELPI.Variable(c.name / hypSuffix)(hypName :: argNames :_*)
              val left2 = HelpCons(c.path)(argNames ::: List(hypName, certName) :_*)
              val ruleE = ELPI.Impl(List(left1,left2),right)
              val rule = ELPI.Rule(ruleE, None)
              Some(addComment(rule))
          }
        } else {
          c.tp match {
            case Some(t) =>
              t match {
                case ruleMatcher.Rule(dr) =>
                  val r = try {
                    addComment(translateRule(c, dr))
                  } catch {case ELPIError(msg) =>
                    fail(msg)
                  }
                  Some(r)
                case _ =>
                  val msg = "not a rule: " + controller.presenter.asString(t)
                  Some(fail(msg))
              }
            case None =>
              throw ELPIError("cannot translate untyped constant: " + c.path)
          }
        }
      case PlainInclude(from,_) =>
        getOutFileForModule(from) match {
          case Some(f) => Some(ELPI.Accumulate(f))
          case None => throw ELPIError("no ELPI file known for theory " + from)
        }
      case _: RuleConstant => None
      case _: Structure => None
      case _ =>
        throw ELPIError("unknown declaration: " + d.path)
    }
  }
  
  private def translateAtomic(aj: AtomicJudgement, hypNames: List[LocalName], hypothetical: Boolean)(implicit vc: VarCounter) : (LocalName, ELPI.Expr) = {
    val name = vc.next(!hypothetical)
    val argsE = aj.arguments map translateTerm
    val nameExpr = ELPI.Variable(name)(hypNames.map(ELPI.Variable(_)) :_*)
    val opName = if (hypothetical) aj.operator.name / hypSuffix else aj.operator.name 
    val e = ELPI.Apply(ELPI.Variable(opName), nameExpr :: argsE :_*)
    (name, e)
  }
  
  private def translateComplex(cj: ComplexJudgement)(implicit vc: VarCounter) : (LocalName, ELPI.Expr) = {
    if (cj.parameters.nonEmpty)
      throw ELPIError("can't handle parameters in complex judgments: " + cj)
    
    val (hypNames, hypEs) = cj.hypotheses.map {a => translateAtomic(a, Nil, true)}.unzip
    val (thesisName, thesisE) = translateAtomic(cj.thesis, hypNames, false)
    val cjE = ELPI.Forall(hypNames, ELPI.Impl(hypEs, thesisE))
    (thesisName, cjE)
  }

  private def translateRule(c: Constant, dr: DeclarativeRule)(implicit vc: VarCounter) : ELPI.Rule = {
    val (assNames, assEs) = dr.assumptions.map {cj => 
      translateComplex(cj)
    }.unzip
    val (concName, concE) = translateAtomic(dr.conclusion, Nil, false)
    val drNames = dr.parameters map (_.name)
    val names = drNames ::: assNames ::: List(concName)
    val help = HelpCons(c.path)(names.map {n => ELPI.Variable(n)} :_*)
    val r = ELPI.Forall(names, ELPI.Impl(help::assEs, concE))
    ELPI.Rule(r, None)
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
  
  private def translateTerm(t: Term): ELPI.Expr = {
    t match {
      case OMS(p) =>
        ELPI.Variable(p.name)
      case OMV(n) =>
        ELPI.Variable(n)
      case Lambda(x,_,t) =>
        ELPI.Lambda(x, translateTerm(t))
      case Pi(x,_,b) =>
        ELPI.Forall(x, translateTerm(b))
      case ApplySpine(f,args) =>
        val fE = translateTerm(f)
        val argsE = args map translateTerm
        fE(argsE :_*)
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
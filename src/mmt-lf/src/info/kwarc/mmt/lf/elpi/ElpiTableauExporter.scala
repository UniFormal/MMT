package info.kwarc.mmt.lf.elpi

import info.kwarc.mmt.api._
import archives._
import symbols._
import modules._
import objects._
import documents._

import info.kwarc.mmt.lf._

class ElpiTableauExporter extends Exporter {
  val key = "lf-elpi-tab"
  override def outExt = "elpi-tab"

  private lazy val lup = controller.globalLookup
  private lazy val ruleMatcher = new RuleMatcher(lup, List("TabClosed", "TabMarker"))

  private def translateTheory(thy: Theory): ELPI.Program = {
    val cons = thy.getDeclarations
    val consE = cons flatMap translateDeclaration
    ELPI.Program(consE:_*)
  }

  private class VarCounter {
    private var i = 0
    def next(upper: Boolean) = {
      i += 1
      val base = if (upper) "X" else "x"
      LocalName(base + i.toString)
    }
  }


  private def translateDeclaration(d: Declaration): List[ELPI.Decl] = {
    def fail(msg: String) = {
      println(msg)
      List(ELPI.Comment("% skipping due to error: " + d.path + "\n% " + msg))
    }
    d match {
      case c: Constant =>
        val comment = ELPI.Comment(s"% generated from ${c.name} : ${controller.presenter.asString(c.tp.get)}")
        c.tp match {
          case Some(t) =>
            t match {
              case ruleMatcher.Rule(dr) =>
                println(s"generated from ${c.name} : ${controller.presenter.asString(c.tp.get)}")
                println(s"Matches ${dr}")
                try {
                  val rules = translateRule(c, dr)(new VarCounter())
                  List(comment) ++ rules
                } catch {
                  case ELPIError(msg) => fail(msg)
                  case CantHandleRule(msg) => fail(msg)
                }
              case _ =>
                val msg = "not a rule: " + controller.presenter.asString(t)
                fail(msg)
            }
          case None =>
            throw ELPIError("cannot translate untyped constant: " + c.path)
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

  private def sV(s : String) = ELPI.Variable(LocalName(s))

  private def translateRule(c: Constant, dr: DeclarativeRule)(implicit vc : VarCounter) : List[ELPI.Decl] = {
    println("C: " + c)
    val args = dr.arguments.collect {
      case RuleParameter(_,_) => None
      case RuleAssumption(cj) => Some(cj)
    }.flatten
    val parameternames = dr.arguments.collect {
      case RuleParameter(n,_) => Some(n)
      case _ => None
    }.flatten

    if (args.head.hypotheses.nonEmpty) {
      throw CantHandleRule("Hypotheses for head argument: " + args.head.hypotheses)
    }
    if (dr.conclusion.rl != "TabClosed") {
      throw CantHandleRule("Conclusion is not a closed tableau " + dr.conclusion)
    }
    val inpcore = ELPI.Variable(args.head.thesis.operator.name)(args.head.thesis.arguments map translateTerm :_*)
    val inp = sV("tab/hyp")(inpcore)
    val rest = args.drop(1).map(
      cj =>
      {
        if (cj.thesis.rl != "TabClosed") {
          throw CantHandleRule("Thesis is not a closed tableau " + cj.thesis)
        }
        translrest(cj.hypotheses)
      }
    )
    println("inp: " + inp.toELPI())
    println("rest: " + rest)
    List(ELPI.Rule(ELPI.Forall(parameternames, ELPI.Impl(List(
      inp,
      ELPI.Not(sV("tab/processed")(inpcore)),
      ELPI.Impl(List(ELPI.Variable(LocalName("tab/processed"))(inpcore)), ELPI.And(rest)))
      , sV("tab/close")))))
  }

  def translrest(aj : List[AtomicJudgement])(implicit vc : VarCounter) : ELPI.Expr = {
    if (aj.isEmpty) {
      ELPI.Variable(LocalName("tab/close"))
    } else {
      ELPI.Impl(
        List(ELPI.Variable(LocalName("tab/hyp"))(ELPI.Variable(aj.head.operator.name)(aj.head.arguments map translateTerm :_*))),
        translrest(aj.drop(1)))
    }
  }

  def exportTheory(thy: Theory, bf: BuildTask) {
    val thyE = translateTheory(thy)
    rh << thyE.toELPI
  }

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

  def exportView(view: View, bf: BuildTask) {}
  def exportDocument(doc: Document, bf: BuildTask) {}
  def exportNamespace(dpath: DPath, bd: BuildTask, namespaces: List[BuildTask], modules: List[BuildTask]) {}
}

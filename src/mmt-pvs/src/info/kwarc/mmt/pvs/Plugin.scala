package info.kwarc.mmt.pvs

import info.kwarc.mmt.api._
import checking._
import modules._
import uom._
import notations.{HOAS, NestedHOASNotation}
import objects._
import objects.Conversions._
import notations._
import symbols._
import info.kwarc.mmt.lf._

/*
class Plugin extends frontend.Plugin {
  val theory = PVSTheory.thpath
  val dependencies = List("info.kwarc.mmt.lf.Plugin")
  override def start(args: List[String]) {
    val em = controller.extman
    // content enhancers
    em.addExtension(new LambdaPiInclude)
    em.addExtension(new PVSImporter)
    em.addExtension(new PVSServer)
    em.addExtension(PVSTheory.preproc)
  }
}
*/

import PVSTheory._

// Notation Extension

object PVSNotation extends NotationExtension {
  val theory = PVSTheory.thpath
  override val priority = 3
  def isApplicable(t: Term): Boolean = t match {
    case ComplexTerm(Apply.path,sub,con,List(pvsapply.term,_,_,OMS(fun),tuple_expr(List(a,b)))) =>
      true
    case ComplexTerm(Apply.path,sub,con,List(pvsapply.term,_,_,Apply(OMS(fun),ls),tuple_expr(List(a,b)))) =>
      true // implicit type arguments
    case ComplexTerm(Apply.path,sub,con,List(pvsapply.term,_,_,parambind(fun,ls),tuple_expr(List(a,b)))) =>
      true // implicit type arguments
    case _ => false
  }
  /** called to construct a term after a notation produced by this was used for parsing */
  def constructTerm(op: GlobalName, subs: Substitution, con: Context, args: List[Term], not: TextNotation)
                   (implicit unknown: () => Term): Term = {
    require(args.length == 2)
    ???
  }
  def constructTerm(fun: Term, args: List[Term]): Term = ApplySpine(fun,args:_*)
  /** called to deconstruct a term before presentation */
  def destructTerm(t: Term)(implicit getNotations: GlobalName => List[TextNotation]): Option[PragmaticTerm] = {
    val (fun,sub,con,a,b) = t match {
      case ComplexTerm(Apply.path,sub2,con2,List(pvsapply.term,_,_,OMS(fun2),tuple_expr(List((a2,_),(b2,_))))) => (fun2,sub2,con2,a2,b2)
      case ComplexTerm(Apply.path,sub2,con2,List(pvsapply.term,_,_,Apply(OMS(fun2),ls),tuple_expr(List((a2,_),(b2,_))))) => (fun2,sub2,con2,a2,b2)
      case ComplexTerm(Apply.path,sub2,con2,List(pvsapply.term,_,_,parambind(fun2,ls),tuple_expr(List((a2,_),(b2,_))))) =>
        (fun2,sub2,con2,a2,b2) // implicit type arguments
      case _ => return None
    }
    val delim = Delim(fun.name match {
      case ComplexStep(p) / s => p.name + "?" + s
      case _ => fun.name.toString
    })
    val fixity = Mixfix(List(SimpArg(1),Delim("%w"),delim,Delim("%w"),SimpArg(2)))
    val notation = TextNotation(fixity,Precedence.integer(0),None)
    Some(PragmaticTerm(fun,sub,con,List(a,b),notation,Position.positions(OMA(OMS(fun),List(a,b)))))
  }
}

// Structural Features

class LambdaPiInclude extends StructuralFeature("BoundInclude") with IncludeLike {
  def elaborate(parent: ModuleOrLink, dd: DerivedDeclaration)(implicit env: Option[ExtendedSimplificationEnvironment] = None): Elaboration = {
    val dom = getDomain(dd)
    val body = controller.simplifier.getBody(Context.empty, dom) match {
      case t : Theory => t.path
      case _ => throw LocalError("not a declared theory: " + dom)
    }
    new Elaboration {
      override def domain: List[LocalName] = List(LocalName(body))

      override def getO(name: LocalName): Option[Declaration] = name match {
        case n if n == LocalName(body) => Some(PlainInclude(body,dd.parent))
      }
    }
  }

  override def processHeader(header: Term) = header match {
    case OMA(OMMOD(`mpath`), (t @ OMPMOD(p,_))::Nil) => (LocalName("Param_" + p.name), t)
  }

  override def makeHeader(dd: DerivedDeclaration) = OMA(OMMOD(`mpath`), dd.tpC.get.get :: Nil)

  def check(dd: DerivedDeclaration)(implicit env: ExtendedCheckingEnvironment): Unit = {}
}//BoundTheoryParameters(BoundInclude.feature,Pi.path,Lambda.path,Apply.path)

object BoundInclude {
  val mpath = SemanticObject.javaToMMT("info.kwarc.mmt.pvs.LambdaPiInclude")
  val feature = "BoundInclude"
  def apply(top: MPath, from: MPath) = {
    val tpC = TermContainer(OMMOD(from))
    new DerivedDeclaration(OMMOD(top), LocalName(from), feature, tpC, new NotationContainer())
  }
}

object BoundIncludeRule extends StructuralFeatureRule(classOf[LambdaPiInclude], BoundInclude.feature)

// Literals

object NatLiterals extends RepresentedRealizedType(expr(OMS(PVSTheory.thpath ? "NatLit")),StandardNat) {
  val pvstp = OMS(PVSTheory.thpath ? "NatLit")
}
object StringLiterals extends RepresentedRealizedType(expr(OMS(PVSTheory.thpath ? "StringLit")),StandardString) {
  val pvstp = OMS(PVSTheory.thpath ? "StringLit")
}
object RationalLiterals extends RepresentedRealizedType(expr(OMS(PVSTheory.thpath ? "RatLit")),StandardRat) {
  val pvstp = OMS(PVSTheory.thpath ? "RatLit")
}

// Other Rules

object CurryingPiRule extends ComputationRule(PVSTheory.pvspi.path) {
  def apply(check: CheckingCallback)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History) = tm match {
    case pvspi(v0,sigmaspine(ls,b0),c) =>
      var x = Context.pickFresh(stack.context,"x")
      val ret1 = c ^? (v0 / tuple_expr(ls.map(p => (OMV(p._1),p._2)) ::: (OMV(x._1),b0) :: Nil)._1)
      Simplify(ls.foldRight(pvspi(x._1,b0,ret1))((p,t) => pvspi(p._1,p._2,t)))
      // var ret = pvspi()

    case _ => Recurse
  }
  override def priority: Int = 10
}
object CurryingLambdaRule extends ComputationRule(PVSTheory.pvslambda.path) {
  def apply(check: CheckingCallback)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History) = tm match {
    case pvslambda(v0,sigmaspine(ls,b0),tpc,bd) =>
      var x = Context.pickFresh(stack.context,"x")
      val tp1 = tpc ^? (v0 / tuple_expr(ls.map(p => (OMV(p._1),p._2)) ::: (OMV(x._1),b0) :: Nil)._1)
      val bd1 = bd ^? (v0 / tuple_expr(ls.map(p => (OMV(p._1),p._2)) ::: (OMV(x._1),b0) :: Nil)._1)
      val (retbd,rettp) = ls.foldRight((pvslambda(x._1,b0,tp1,bd1),pvspi(x._1,b0,tp1)))((p,t) =>
        (pvslambda(p._1,p._2,t._2,t._1),pvspi(p._1,p._2,t._2))
      )
      Simplify(retbd)
    case _ => Recurse
  }
  override def priority: Int = 10
}

object CurryingEqualityPiRule extends TermBasedEqualityRule {
  val head = pvspi.path

  def applicable(tm1: Term, tm2: Term): Boolean = (tm1, tm2) match {
    case (pvspi(v1, pvssigma(v2, a, b), target), pvspi(v12, a2, pvspi(v22, b2, target2))) => true
    case (pvspi(v12, a2, pvspi(v22, b2, target2)), pvspi(v1, pvssigma(v2, a, b), target)) => true
    case _ => false
  }

  def apply(check: CheckingCallback)(tm1: Term, tm2: Term, tp: Option[Term])
           (implicit stack: Stack, history: History): Option[Continue[Boolean]] = (tm1, tm2) match {
    case (pvspi(v1, sig @ pvssigma(v2, a, b), target),
          pvspi(v12, a2, pvspi(v22, b2, target2))) =>
      val ret = CurryingPiRule(check)(tm1,true)
      if (ret.get.isDefined) Some(Continue(check.check(Equality(stack,ret.get.get,tm2,None))(history)))
      else None
    case (pvspi(v12, a2, pvspi(v22, b2, target2)), pvspi(v1, pvssigma(v2, a, b), target)) =>
      apply(check)(tm2, tm1, tp)
    case _ => None
  }
}

object CurryingEqualityLambdaRule extends TermBasedEqualityRule {
  val head = pvslambda.path

  def applicable(tm1: Term, tm2: Term): Boolean = (tm1, tm2) match {
    case (pvslambda(v1, pvssigma(v2, a, b),_, target), pvslambda(v12, a2, _, pvslambda(v22, b2, _, target2))) => true
    case (pvslambda(v12, a2, _, pvslambda(v22, b2, _, target2)), pvslambda(v1, pvssigma(v2, a, b), _, target)) => true
    case _ => false
  }

  def apply(check: CheckingCallback)(tm1: Term, tm2: Term, tp: Option[Term])
           (implicit stack: Stack, history: History): Option[Continue[Boolean]] = (tm1, tm2) match {
    case (pvslambda(v1, sig @ pvssigma(v2, a, b), retp1, target),
    pvslambda(v12, a2, _, pvslambda(v22, b2, retp2, target2))) =>
      val ret = CurryingLambdaRule(check)(tm1,true)
      if (ret.get.isDefined) Some(Continue(check.check(Equality(stack,ret.get.get,tm2,None))(history)))
      else None
    case (pvslambda(v12, a2, _, pvslambda(v22, b2, _, target2)), pvslambda(v1, pvssigma(v2, a, b), _, target)) =>
      apply(check)(tm2, tm1, tp)
    case _ => None
  }
}
/*
object CurryingLambdaRule extends ComputationRule(PVSTheory.expr_as_type.path) {
  def apply(check: CheckingCallback)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History): Option[Term] = tm match {
    case expr_as_type(expr,pred(a)) =>

    case _ => None
  }
}
*/

object NatLitSubtype extends SubtypingRule {
  val head = PVSTheory.natlit
  private val prelude = PVSTheory.rootdpath / "Prelude"
  private val numberlist = List(
    prelude ? "numbers" ? "number",
    prelude ? "number_fields" ? "number_field",
    prelude ? "number_fields" ? "numfield"
  )
  private def isNumber(p : GlobalName) : Boolean =
    numberlist.contains(p) || isNumber(p.name)
  private def isNumber(p : LocalName) : Boolean =
    numberlist.exists(n => p == (LocalName(n.module) / n.name))
  def applicable(tp1: Term, tp2: Term): Boolean = (tp1,tp2) match {
    case (expr(OMS(PVSTheory.natlit)),expr(OMS(p))) if isNumber(p)   => true
    case (expr(OMS(PVSTheory.natlit)),expr(OMV(p))) if isNumber(p)   => true
    case _ => false
  }
  /**
    * pre all arguments covered
    * @return Some(b) if the judgment was proved/disproved, None if the result is inconclusive
    */
  def apply(solver: Solver)(tp1: Term, tp2: Term)(implicit stack: Stack, history: History) : Option[Boolean]
  = if (applicable(tp1,tp2)) Some(true) else None
}


object PVSHOAS extends NestedHOASNotation(HOAS(pvsapply.path,pvslambda.path),LF.hoas)

import PVSTheory._

object SetsubRule extends ComputationRule(PVSTheory.expr.path) {
  def apply(check: CheckingCallback)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History) = tm match {
    case expr(setsub(tp,prop)) => Simplify(LFX.predsubtp(expr(tp),proof("internal_judgment",
      Lambda(doName,expr(tp),pvsapply(prop,OMV(doName),expr(tp),bool.term)._1))))
    case _ => Recurse
  }

  def doName = Context.pickFresh(Context.empty,LocalName("Ipred")/"x")._1
}

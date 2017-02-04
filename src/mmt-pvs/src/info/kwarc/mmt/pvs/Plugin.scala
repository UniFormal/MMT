package info.kwarc.mmt.pvs

import info.kwarc.mmt.api._
import checking._
import info.kwarc.mmt.api.uom.{RepresentedRealizedType, StandardNat, StandardRat, StandardString}
import notations.{HOAS, NestedHOASNotation}
import objects._
import objects.Conversions._
import notations._
import symbols.{BoundTheoryParameters, DerivedDeclaration, StructuralFeatureRule, TermContainer}
import info.kwarc.mmt.lf._

class Plugin extends frontend.Plugin {
  val theory = PVSTheory.thpath
  val dependencies = List("info.kwarc.mmt.lf.Plugin")
  override def start(args: List[String]) {
    val em = controller.extman
    // content enhancers
    em.addExtension(new LambdaPiInclude)
    em.addExtension(new PVSImporter)
  }
}

import PVSTheory._

// Notation Extension

object PVSNotation extends NotationExtension {
  def isApplicable(t: Term): Boolean = t match {
    case pvsapply(fun,tuple_expr(List(a,b)),_) => true
    case _ => false
  }
  /** called to construct a term after a notation produced by this was used for parsing */
  def constructTerm(op: GlobalName, subs: Substitution, con: Context, args: List[Term], attrib: Boolean, not: TextNotation)
                   (implicit unknown: () => Term): Term = {
    println("constructTerm1")
    println(op)
    println(subs)
    println(con)
    println(args)
    println(attrib)
    println(not)
    OMA(OMS(op),args)
  }
  def constructTerm(fun: Term, args: List[Term]): Term = {
    println("constructTerm2")
    println(fun)
    println(args)
    OMA(fun,args)
  }
  /** called to deconstruct a term before presentation */
  def destructTerm(t: Term)(implicit getNotations: GlobalName => List[TextNotation]): Option[PragmaticTerm] = {
    println("destructTerm")
    println(t)
    ???
  }
}

// Structural Features

class LambdaPiInclude extends BoundTheoryParameters(BoundInclude.feature,Pi.path,Lambda.path,Apply.path)

object BoundInclude {
  val mpath = SemanticObject.javaToMMT("info.kwarc.mmt.pvs.LambdaPiInclude")
  val feature = "BoundInclude"
  def apply(top: MPath, from: MPath) = {
    val tpC = TermContainer(OMMOD(from))
    new DerivedDeclaration(OMMOD(top), LocalName(from), feature, tpC, new NotationContainer())
  }
}

object BoundIncludeRule extends StructuralFeatureRule(BoundInclude.feature)

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
  def apply(check: CheckingCallback)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History): Option[Term] = tm match {
    case pvspi(v0,sigmaspine(ls,b0),c) =>
      var x = Context.pickFresh(stack.context,"x")
      val ret1 = c ^? (v0 / tuple_expr(ls.map(p => (OMV(p._1),p._2)) ::: (OMV(x._1),b0) :: Nil)._1)
      Some(ls.foldRight(pvspi(x._1,b0,ret1))((p,t) => pvspi(p._1,p._2,t)))
      // var ret = pvspi()

    case _ => None
  }
  override def priority: Int = 10
}
object CurryingLambdaRule extends ComputationRule(PVSTheory.pvslambda.path) {
  def apply(check: CheckingCallback)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History): Option[Term] = tm match {
    case pvslambda(v0,sigmaspine(ls,b0),tpc,bd) =>
      var x = Context.pickFresh(stack.context,"x")
      val tp1 = tpc ^? (v0 / tuple_expr(ls.map(p => (OMV(p._1),p._2)) ::: (OMV(x._1),b0) :: Nil)._1)
      val bd1 = bd ^? (v0 / tuple_expr(ls.map(p => (OMV(p._1),p._2)) ::: (OMV(x._1),b0) :: Nil)._1)
      val (retbd,rettp) = ls.foldRight((pvslambda(x._1,b0,tp1,bd1),pvspi(x._1,b0,tp1)))((p,t) =>
        (pvslambda(p._1,p._2,t._2,t._1),pvspi(p._1,p._2,t._2))
      )
      Some(retbd)
    case _ => None
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
      if (ret.isDefined) Some(Continue(check.check(Equality(stack,ret.get,tm2,None))(history)))
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
      if (ret.isDefined) Some(Continue(check.check(Equality(stack,ret.get,tm2,None))(history)))
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


object PVSHOAS extends NestedHOASNotation(HOAS(pvsapply.path,pvslambda.path,expr.path),LF.hoas)

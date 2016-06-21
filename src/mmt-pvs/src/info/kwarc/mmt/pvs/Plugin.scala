package info.kwarc.mmt.pvs

import info.kwarc.mmt.api.{GlobalName, _}
import info.kwarc.mmt.api.checking.{Solver, SubtypingRule, _}
import info.kwarc.mmt.api.modules.DeclaredTheory
import info.kwarc.mmt.api.notations.{HOAS, NestedHOASNotation}
import info.kwarc.mmt.api.objects.{OMS, _}
import info.kwarc.mmt.api.symbols.{BoundTheoryParameters, DerivedDeclaration, StructuralFeatureRule, TermContainer}
import info.kwarc.mmt.lf.{Apply, LF, Lambda, Pi}
import info.kwarc.mmt.api.objects.Conversions._

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


class LambdaPiInclude extends BoundTheoryParameters("BoundInclude",Pi.path,Lambda.path,Apply.path)
case class BoundInclude(top:DeclaredTheory,from:MPath) extends DerivedDeclaration(top.toTerm,LocalName(from),"BoundInclude",
  List(DeclarationComponent(DomComponent,TermContainer(OMID(from)))))

object BoundIncludeRule extends StructuralFeatureRule("BoundInclude",List(DomComponent),hasname = false)

object CurryingRule extends ComputationRule(PVSTheory.pvspi.path) {
  def apply(check: CheckingCallback)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History): Option[Term] = tm match {
    case pvspi(v1,pvssigma(v2,a,Lambda(_,_,b)),Lambda(_,_,c)) =>
      Some(pvspi(v1,a,pvspi(v2,b,c)))//fun_type(PVSTheory.tuple_type(ls), c) =>
    //Some(ls.foldRight(c)((t, ret) => fun_type(t, ret)))
    case _ => None
  }
}
object CurryingApplyRule extends ComputationRule(PVSTheory.apply.path) {
  def apply(check: CheckingCallback)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History): Option[Term] = tm match {
    case PVSTheory.apply(f, PVSTheory.tuple_expr(ls), rettp) =>
      val stack = ls.foldRight(List(rettp))((p, ret) => fun_type(p._2, ret.head) :: ret)
      Some(ls.foldLeft((f, stack))((fun, p) => (PVSTheory.apply(fun._1, p._1, fun._2.head)(null)._1, fun._2.tail))._1)
  }
}
object CurryingEqualityRule extends TermBasedEqualityRule {
  val head = pvspi.path

  def applicable(tm1: Term, tm2: Term): Boolean = (tm1, tm2) match {
    case (pvspi(v1, pvssigma(v2, a, Lambda(_, _, b)), Lambda(_, _, target)), pvspi(v12, a2, Lambda(_, _, pvspi(v22, b2, Lambda(_, _, target2))))) => true
    case (pvspi(v12, a2, Lambda(_, _, pvspi(v22, b2, target2))), pvspi(v1, pvssigma(v2, a, b), target)) => true
    case _ => false
  }

  def apply(check: CheckingCallback)(tm1: Term, tm2: Term, tp: Option[Term])
           (implicit stack: Stack, history: History): Option[Continue[Boolean]] = (tm1, tm2) match {
    case (pvspi(v1, pvssigma(v2, a, Lambda(_, _, b)), Lambda(_, _, target)), pvspi(v12, a2, Lambda(_, _, pvspi(v22, b2, Lambda(_, _, target2))))) =>
      val ret = check.check(Equality(stack, a, a2, None)) &&
        check.check(Equality(stack ++ v2 % a, b, b2 ^? (v22 / OMV(v2)), None)) &&
        check.check(Equality(stack, target, target2, None))
      Some(Continue(ret))
    case (pvspi(v12, a2, Lambda(_, _, pvspi(v22, b2, target2))), pvspi(v1, pvssigma(v2, a, b), target)) =>
      apply(check)(tm2, tm1, tp)
    /*
  case (Lambda(v1,expr(tp1),target1),Lambda(v2,expr(tp2),target2)) =>
    apply(check)(pvspi(v1,tp1,target1),pvspi(v2,tp2,target2),tp)
    */
    case _ => None
  }
}
object NatLitSubtype extends SubtypingRule {
  val head = PVSTheory.natlit
  private val prelude = PVSTheory.rootdpath / "Prelude"
  private val numberlist = List(
    prelude ? "numbers" ? "number",
    prelude ? "number_fields" ? "number_field",
    prelude ? "number_fields" ? "numfield"
  )
  private def isNumber(p : GlobalName) : Boolean =
    numberlist.contains(p) || numberlist.exists(n => p.name == (LocalName(n.module) / n.name))
  def applicable(tp1: Term, tp2: Term): Boolean = (tp1,tp2) match {
    case (expr(OMS(PVSTheory.natlit)),expr(OMS(p))) if isNumber(p)   => true
    case _ => false
  }
  /**
    * pre all arguments covered
    * @return Some(b) if the judgment was proved/disproved, None if the result is inconclusive
    */
  def apply(solver: Solver)(tp1: Term, tp2: Term)(implicit stack: Stack, history: History) : Option[Boolean]
  = if (applicable(tp1,tp2)) Some(true) else None
}
/*
object NatLitTypeCast extends TermBasedEqualityRule {
  val head = PVSTheory.natlit

  def applicable(tp1: Term, tp2: Term): Boolean = (tp1,tp2) match {
    case (OMS(PVSTheory.natlit),OMS(p)) => true
    case (OMS(p),OMS(PVSTheory.natlit)) => true
    case _ => false
  }

  def apply(check: CheckingCallback)(tm1: Term, tm2: Term, tp: Option[Term])
           (implicit stack: Stack, history: History): Option[Continue[Boolean]] = (tm1,tm2) match {
    case (OMS(PVSTheory.natlit),OMS(p)) =>
      val list = List("number","number_field","numfield")
      if (list contains p.name.last.toString) Some(Continue(true)) else None
    case (OMS(p),OMS(PVSTheory.natlit)) => apply(check)(tm2,tm1,tp)
    case _ => None
  }
}
*/
object PVSHOAS extends NestedHOASNotation(HOAS(PVSTheory.apply.path,lambda.path,expr.path),LF.hoas)

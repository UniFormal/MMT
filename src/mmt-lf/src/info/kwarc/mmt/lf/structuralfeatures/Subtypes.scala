package info.kwarc.mmt.lf.structuralfeatures

import info.kwarc.mmt.api._
import objects._
import symbols._
import notations._
import checking._
import modules._
import frontend.Controller
import info.kwarc.mmt.lf._
import InternalDeclaration._
import InternalDeclarationUtil._

class Subtypes extends StructuralFeature("Subtype") with ParametricTheoryLike {
  override def check(dd: DerivedDeclaration)(implicit env: ExtendedCheckingEnvironment) {}

  def elaborate(parent: DeclaredModule, dd: DerivedDeclaration) = {
    implicit val parentTerm = dd.path
    val params = Type.getParameters(dd)
    val context = if (params.nonEmpty) Some(params) else None
    try {
      /*val eqRel = controller.getO(dd.getDeclarationsElaborated.head.parent / dd.getDeclarations.head.name) match {
        case Some(b) => b match {
          case d : DerivedDeclaration => if (d.feature != "equivalence_relation") throw LocalError("Equivalence relation expected.") 
            else d.getDeclarations.head match {case c: Constant => fromConstant(c, controller, Some(context))}
          case _ => throw LocalError("Equivalence relation expected.")
        }
      }*/ 
      val pred = dd.getDeclarations.last match {case c:Constant=>fromConstant(c, controller, context)}
      val dom = pred.args match {case List((_, d)) => d}
      val domain = TypeLevel(uniqueGN("A"), Nil, None, context)
            
      val subtype : TypeLevel = structureDeclaration(Some("S"), context)
      val injection = introductionDeclaration(subtype.path, List(domain), Some("injection"), context)
      val injInjective = injection.injDecl
      val lift = this.lift(domain.path, subtype.path, pred, Some("lift"), context)
      val inverse = this.inverse(domain.path, subtype.path, pred, injection.path, lift.path, Some("inverse"), context)
            
      val elabDecls = List(domain.toConstant, injection.toConstant, injInjective, lift, inverse) 
      elabDecls map {d => log(defaultPresenter(d)(controller))}
      new Elaboration {
        def domain = elabDecls map {d => d.name}
        def getO(n: LocalName) = {
          elabDecls.find(_.name == n)
        }
      }
    } catch {
      case e: Error => throw LocalError("Wrong argument: subtype expects a predicate as an argument: "+e.getMessage)
    }
  }
  
  /** Declares the existence of a lifting of any function out of the subtype to the domain, as long as the predicate holds on its argument */
  private def lift(domain: GlobalName, subtype: GlobalName, pred:InternalDeclaration, name: Option[String], ctx: Option[Context])(implicit parent: GlobalName): Constant = {
    val Ltp = () => {
      val T = newVar(uniqueLN("T"), Univ(1), ctx)
      val f = newVar(uniqueLN("f"), Arrow(OMS(subtype), T.toTerm), ctx)
      val y = newVar(uniqueLN("y"), OMS(domain), ctx)
      val p = pred.applyTo(y)
      Pi(List(T, f, y), Arrow(p, T.toTerm))
    }
    makeConst(uniqueLN(name getOrElse "invert"), Ltp)
  }
  
  /** Declares that the lifting function is inverse to the injection, if defined */
  private def inverse(domain: GlobalName, subtype: GlobalName, pred: InternalDeclaration, injection: GlobalName, lift: GlobalName, name: Option[String], ctx: Option[Context])(implicit parent: GlobalName): Constant = {
    val Ltp = () => {
      val T = newVar(uniqueLN("T"), Univ(1), ctx)
      val f = newVar(uniqueLN("f"), Arrow(OMS(subtype), T.toTerm), ctx)
      val x = newVar(uniqueLN("x"), OMS(subtype), ctx)
      val y = ApplyGeneral(OMS(injection), List(x.toTerm))
      val p = pred.applyTo(y)
      val lifted = ApplyGeneral(OMS(lift), List(T.toTerm, f.toTerm, y, p))
      Pi(List(T, f, x), Arrow(p, Eq(lifted, ApplyGeneral(f.toTerm, List(x.toTerm)))))
    }
    makeConst(uniqueLN(name getOrElse "inverse"), Ltp)
  }
}

object SubtypeRule extends StructuralFeatureRule(classOf[Subtypes], "Subtypes")
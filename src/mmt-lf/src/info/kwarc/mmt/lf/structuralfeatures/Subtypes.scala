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
      val injection = introductionDeclaration(subtype.path, List(domain), Some("inj"), context)
      val injInjective = injection.injDecl
      val lift = this.lift(domain.path, subtype.path, pred, Some("g"), context)
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
  
  /** 
   * Let S be the predicate subtype of the type D by the predicate p
   * Now given a function f: S -> T for any type T, there exists a unique lift g: D -> T for all y in D s.t. p y
   */
  private def lift(D: GlobalName, S: GlobalName, p:InternalDeclaration, name: Option[String], ctx: Option[Context])(implicit parent: GlobalName): Constant = {
    val Ltp = () => {
      val T = newVar(uniqueLN("T"), Univ(1), ctx)
      val f = newVar(uniqueLN("f"), Arrow(OMS(D), T.toTerm), ctx)
      val y = newVar(uniqueLN("y"), OMS(D), ctx)
      val proof = p.applyTo(y)
      Pi(List(T, f, y), Arrow(proof, T.toTerm))
    }
    makeConst(uniqueLN(name getOrElse "g"), Ltp)
  }
  
  /** Declares that the lifting function is inverse to the injection, if defined 
   * i.e. in the above situation and with canonical injection inj: S -> D, we have:
   * for all y in D s.t. p y => g(y) = f ° inj
   */
  private def inverse(D: GlobalName, S: GlobalName, p: InternalDeclaration, inj: GlobalName, g: GlobalName, name: Option[String], ctx: Option[Context])(implicit parent: GlobalName): Constant = {
    val Ltp = () => {
      val T = newVar(uniqueLN("T"), Univ(1), ctx)
      val f = newVar(uniqueLN("f"), Arrow(OMS(D), T.toTerm), ctx)
      val x = newVar(uniqueLN("x"), OMS(S), ctx)
      val y = ApplyGeneral(OMS(inj), List(x.toTerm))
      val proof = p.applyTo(y)
      val y_lifted = ApplyGeneral(OMS(g), List(T.toTerm, f.toTerm, y, proof))
      val y_mapped = ApplyGeneral(f.toTerm, List(x.toTerm))
      Pi(List(T, f, x), Arrow(proof, Eq(y_lifted, y_mapped)))
    }
    makeConst(uniqueLN(name getOrElse "inverse"), Ltp)
  }
}

object SubtypeRule extends StructuralFeatureRule(classOf[Subtypes], "Subtypes")
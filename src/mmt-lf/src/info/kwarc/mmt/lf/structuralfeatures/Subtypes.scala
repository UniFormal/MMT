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
import StructuralFeatureUtils._
import InternalDeclarationUtil._
import info.kwarc.mmt.api.utils.MMT_TODO

@deprecated("MMT_TODO: this is experimental and may still be removed", since="forever")
class Subtypes extends StructuralFeature("Subtype") with ParametricTheoryLike {
  override def check(dd: DerivedDeclaration)(implicit env: ExtendedCheckingEnvironment): Unit = {}

  def elaborate(parent: ModuleOrLink, dd: DerivedDeclaration)(implicit env: Option[uom.ExtendedSimplificationEnvironment] = None) = {
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
      val pred = dd.getDeclarations.last match {case c:Constant=>fromConstant(c, controller, Nil, context)}
      val dom = pred.args match {case List((_, d)) => d}
            
      val subtype = structureDeclaration(Some("S"), context)
      val injection = {
        val Ltp = () => {
          val tp = Arrow(dom, OMS(subtype.path))
          PiOrEmpty(params, if (!params.isEmpty) ApplyGeneral(tp, params.map(_.toTerm)) else tp)
        }
        makeConst(uniqueLN("inj"), Ltp)
      }
      val injInjective = injDecl(injection, controller, context)
      val lift = this.lift(dom, pred, Some("g"), context)
      val inverse = this.inverse(dom, subtype.path, pred, injection.path, lift.path, Some("inverse"), context)
            
      val elabDecls = List(subtype, injection)++injInjective++List(lift, inverse) 
      //elabDecls map {d => log(defaultPresenter(d)(controller))}
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
  private def lift(dom: Term, p:InternalDeclaration, name: Option[String], ctx: Option[Context])(implicit parent: GlobalName): Constant = {
    val Ltp = () => {
      val T = newVar("T", Univ(1), ctx)
      val f = newVar("f", Arrow(dom, T.toTerm), ctx)
      val y = newVar("y", dom, ctx)
      val proof = p.applyTo(y)
      Pi(List(T, f, y), Arrow(proof, T.toTerm))
    }
    makeConst(uniqueLN(name getOrElse "g"), Ltp)
  }
  
  /** Declares that the lifting function is inverse to the injection, if defined 
   * i.e. in the above situation and with canonical injection inj: S -> D, we have:
   * for all y in D s.t. p y => g(y) = f ° inj
   */
  private def inverse(dom: Term, S: GlobalName, p: InternalDeclaration, inj: GlobalName, g: GlobalName, name: Option[String], ctx: Option[Context])(implicit parent: GlobalName): Constant = {
    val Ltp = () => {
      val sub = if (! ctx.isEmpty) ApplyGeneral(OMS(S), ctx.get map (_.toTerm)) else OMS(S)
      val T = newVar("T", Univ(1), ctx)
      val f = newVar("f", Arrow(dom, T.toTerm), ctx)
      val x = newVar("x", sub, ctx)
      val y = ApplyGeneral(OMS(inj), List(x.toTerm))
      val proof = p.applyTo(y)
      val y_lifted = ApplyGeneral(OMS(g), List(T.toTerm, f.toTerm, y, proof))
      val y_mapped = ApplyGeneral(f.toTerm, List(x.toTerm))
      Pi(List(T, f, x), Arrow(proof, Eq(y_lifted, y_mapped,T.toTerm)))
    }
    makeConst(uniqueLN(name getOrElse "inverse"), Ltp)
  }
}

object SubtypeRule extends StructuralFeatureRule(classOf[Subtypes], "Subtypes")
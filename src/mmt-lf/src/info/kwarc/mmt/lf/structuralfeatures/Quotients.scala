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

@MMT_TODO("this is experimental and may still be removed")
class Quotients extends StructuralFeature("quotient") with ParametricTheoryLike {
  override def check(dd: DerivedDeclaration)(implicit env: ExtendedCheckingEnvironment): Unit = {}

  def elaborate(parent: ModuleOrLink, dd: DerivedDeclaration)(implicit env: Option[uom.ExtendedSimplificationEnvironment] = None) = {
    val name = LocalName(dd.path.last)
    implicit val parentTerm = dd.path
    val params = Type.getParameters(dd)
    val context = if (params.nonEmpty) Some(params) else None
    try {
      val eqRel = dd.getDeclarations.last match {case c:Constant=>fromConstant(c, controller, Nil, context)}
      val dom = eqRel.args match {
        case List((_, d), (_,_)) => d
        case tp => throw ImplementationError("Unexpectedly found: . ")
      }
      val rel = eqRel.toConstant
      val structure = structureDeclaration(Some("Q"), context)
      
      val quotientMap = {
        val Ltp = () => {
          val tp = Arrow(dom, structure.tp.get)
          PiOrEmpty(params, if (!params.isEmpty) ApplyGeneral(tp, params.map(_.toTerm)) else tp)
        }
        makeConst(uniqueLN("quotient_map"), Ltp)
      }
      val quotMapSurj = surjDecl(quotientMap, controller, context)
      
      val quotInvert = Pushforward(dom, structure.path, rel.path, quotientMap.path, Some("quotInvert"), context)
      val quotInverse = inverse(dom, structure.path, rel.path, quotientMap.path, quotInvert.path, Some("quotInverse"), context)
      
      val elabDecls = List(rel, structure, quotientMap, quotInvert, quotInverse, quotMapSurj)
      //elabDecls map {d => log(present(d))}
      new Elaboration {
        def domain = elabDecls map {d => d.name}
        def getO(n: LocalName) = {
          elabDecls.find(_.name == n)
        }
      }
    } catch {
      case e: Error => throw LocalError("Wrong argument: Quotient expects a relation as an argument: "+e.getMessage)
    }
  }
  def quotientEq(structure: TypeLevel, rel: InternalDeclaration, quot: TermLevel, name: Option[String], ctx: Option[Context])(implicit parent: GlobalName): TermLevel = {
    val a = rel.args match {
      case List((_, d), (_,_)) => d
      case tp => throw ImplementationError("Unexpected match case: "+tp)
    }
    val args = List(newVar("a1", a, ctx), newVar("a2", a, ctx))
    val ret : Term = PiOrEmpty(args, Arrow(rel.applyTo(args), Eq(quot.applyTo(args.init), quot.applyTo(args.tail), quot.ret)))
    new OutgoingTermLevel(uniqueGN(name getOrElse "quot"), args map (x => (Some(x.name), x.tp.get)), ret, None, None, ctx)
  }
  
  /** Declares the unique push-forward g of a function on the domain down to the quotient, whenever well-defined 
   *   Q  - - g - - >  Q
   *   ^               ^
   *   |               |
   *  quot            quot
   *   |               |
   *  dom ---- f ---> dom
   */
  def Pushforward(dom: Term, Quot:GlobalName, relat:GlobalName, quot: GlobalName, name: Option[String], ctx: Option[Context])(implicit parent: GlobalName): Constant = {
    val Ltp = () => {
      val (q, rel) = if (!ctx.isEmpty) (ApplyGeneral(OMS(Quot), ctx.get.map(_.toTerm)), ApplyGeneral(OMS(relat), ctx.get.map(_.toTerm))) else (OMS(Quot), OMS(relat))
      val f = newVar("f", Arrow(dom, dom), ctx)
      def F(t:Term)=ApplySpine(f.toTerm,t)
      val (x, y) = (newVar("x", dom, ctx), newVar("y", dom, ctx))
      val proof = Pi(List(x, y), Arrow(ApplyGeneral(rel, List(x.toTerm, y.toTerm)), Eq(F(x.toTerm), F(y.toTerm), dom)))
      
      Pi(ctx getOrElse Context.empty ++ f, Arrow(proof, Arrow(q, q)))
    }
    makeConst(uniqueLN(name getOrElse "g"), Ltp)
  }
  
  /**
   * States that the diagram commutes:
   *   Q  -- g -->  Q
   *   ^            ^
   *   |            |
   *  quot         quot
   *   |            |
   *  dom -- f --> dom 
   */
  def inverse(dom: Term, Quot:GlobalName, relat:GlobalName, quot: GlobalName, quotInv: GlobalName, name: Option[String], ctx: Option[Context])(implicit parent: GlobalName): Constant = {
    val Ltp = () => {
      val (q, rel) = if (!ctx.isEmpty) (ApplyGeneral(OMS(Quot), ctx.get.map(_.toTerm)), ApplyGeneral(OMS(relat), ctx.get.map(_.toTerm))) else (OMS(Quot), OMS(relat))
      val f = newVar("f", Arrow(dom, dom), ctx)
      def F(t:Term)=ApplySpine(f.toTerm,t)
      val (x, y, z) = (newVar("x", dom, ctx), newVar("y", dom, ctx), newVar("z", dom, ctx))
      val proof = Pi(List(x, y), Arrow(ApplyGeneral(rel, List(x.toTerm, y.toTerm)), Eq(F(x.toTerm), F(y.toTerm), dom)))
      
      val g_o_quot_z = ApplyGeneral(OMS(quotInv), (ctx getOrElse Context.empty).map(_.toTerm) ++ List(f.toTerm, proof, ApplySpine(OMS(quot),z.toTerm)))
      val quot_o_f_z = ApplySpine(OMS(quot), ApplySpine(f.toTerm, z.toTerm))
      Pi((ctx getOrElse Context.empty) ++ f ++ z, Eq(g_o_quot_z, quot_o_f_z, dom))
    }
  makeConst(uniqueLN(name getOrElse "quotInverse"), Ltp)
  }
}

object QuotientRule extends StructuralFeatureRule(classOf[Quotients], "Quotients")
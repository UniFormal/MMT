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

class Quotients extends StructuralFeature("quotient") with ParametricTheoryLike {
  override def check(dd: DerivedDeclaration)(implicit env: ExtendedCheckingEnvironment) {}

  def elaborate(parent: DeclaredModule, dd: DerivedDeclaration) = {
    val name = LocalName(dd.path.last)
    implicit val parentTerm = dd.path
    val params = Type.getParameters(dd)
    val context = if (params.nonEmpty) Some(params) else None
    try {
      val eqRel = dd.getDeclarations.last match {case c:Constant=>fromConstant(c, controller, context)}
      val dom = eqRel.args match {
        case List((_, d), (_,_)) => d
        case tp => log("Unexpectedly found: . ")
      }
      val domain = TypeLevel(uniqueGN("A"), Nil, None, context)
            
      val structure : TypeLevel = structureDeclaration(Some("Q"), context)
      val quotientMap = introductionDeclaration(structure.path, List(domain), Some("quotient_map"), context)
      val quotMapSurj = quotientMap.surjDecl
      val quotInvert = Pushout(domain.path, structure.path, eqRel.path, quotientMap.path, Some("quotInvert"), context)
      val quotInverse = inverse(domain.path, structure.path, eqRel.path, quotientMap.path, quotInvert.path, Some("quotInverse"), context)
      
      val elabDecls = List(structure, quotientMap).map(_.toConstant) ++ List(quotInvert, quotInverse, quotMapSurj)
      //elabDecls map {d => log(defaultPresenter(d)(controller))}
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
      case tp => println(tp); throw ImplementationError("Unexpected match case. ")
    }
    val args = List(newVar(uniqueLN("a1"), a, None), newVar(uniqueLN("a2"), a, None))
    val ret : Term = PiOrEmpty(args, Arrow(rel.applyTo(args), Eq(quot.applyTo(args.init), quot.applyTo(args.tail))))
    TermLevel(uniqueGN(name getOrElse "quot"), args map (x => (Some(x.name), x.tp.get)), ret, None, None, ctx)
  }
  
  /** Declares the unique push-out of a function on the domain down to the quotient, whenever well-defined */
  def Pushout(D:GlobalName, Q:GlobalName, rel:GlobalName, quot: GlobalName, name: Option[String], ctx: Option[Context])(implicit parent: GlobalName): Constant = {
    val Ltp = () => {
      val f = newVar(uniqueLN("f"), Arrow(OMS(D), OMS(D)), ctx)
      val (x, y) = (newVar(uniqueLN("x"), OMS(D), ctx), newVar(uniqueLN("y"), OMS(D), ctx))
      val proof = Pi(List(x, y), ApplyGeneral(OMS(rel), List(x.toTerm, y.toTerm)))
      
      Pi(f, Arrow(proof, Arrow(OMS(Q), OMS(Q))))
    }
    makeConst(uniqueLN(name getOrElse "g"), Ltp)
  }
  
  def inverse(D:GlobalName, Q:GlobalName, rel:GlobalName, quot: GlobalName, quotInv: GlobalName, name: Option[String], ctx: Option[Context])(implicit parent: GlobalName): Constant = {
    val Ltp = () => {
      val f = newVar(uniqueLN("f"), Arrow(OMS(D), OMS(D)), ctx)
      val (x, y, z) = (newVar(uniqueLN("x"), OMS(D), ctx), newVar(uniqueLN("y"), OMS(D), ctx), newVar(uniqueLN("z"), OMS(D), ctx))
      val proof = Pi(List(x, y), ApplyGeneral(OMS(rel), List(x.toTerm, y.toTerm)))
      
      val g_z = ApplyGeneral(OMS(quotInv), (ctx getOrElse Context.empty).map(_.toTerm) ++ List(f.toTerm, proof, z.toTerm))
      val f_o_quot_z = ApplySpine(OMS(quot), ApplySpine(f.toTerm, z.toTerm))
      PiOrEmpty(List(f, z), Eq(g_z, f_o_quot_z))
    }
  makeConst(uniqueLN(name getOrElse "quotInverse"), Ltp)
  }
}

object QuotientRule extends StructuralFeatureRule(classOf[Quotients], "Quotients")
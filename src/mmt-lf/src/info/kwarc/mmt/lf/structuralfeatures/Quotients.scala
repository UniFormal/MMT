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
      val (b, f, p, quotInvert) = Lift(domain, structure, eqRel, quotientMap, Some("quotInvert"), context)
      val quotInverse = inverse(domain, structure, eqRel, quotientMap, b, f, p, quotInvert.path, Some("quotInverse"))
      
      val elabDecls = List(structure, quotientMap).map(_.toConstant) ++ List(quotInvert, quotInverse, quotMapSurj)
      elabDecls map {d => log(defaultPresenter(d)(controller))}
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
  
  /** Declares the lifting of a function on the domain to the quotient, whenever well-defined */
  def Lift(domain:TypeLevel, structure:TypeLevel, rel:InternalDeclaration, quot: TermLevel, name: Option[String], ctx: Option[Context])(implicit parent: GlobalName): (TypeLevel, TermLevel, TermLevel, Constant) = {
    val B = TypeLevel(uniqueGN("B"), Nil, None, ctx)
    val f = TermLevel(uniqueGN("f"), List((None, domain.toTerm)), B.toTerm, None, None, None)
    val xy @ List(x, y) = List(domain.makeVar("x", Context.empty), domain.makeVar("y", Context.empty))
    val p = TermLevel(uniqueGN("p"), xy, Arrow(rel.applyTo(xy), Eq(f.applyTo(x.tp.get), f.applyTo(y.tp.get))), None, None, None)
    val quotInv = makeConst(uniqueLN(name getOrElse "quotInvert"), () => {
      FunType(List(B, f, p).map(x => (Some(x.name), x.tp)) :+ (Some(structure.name), structure.toTerm), B.toTerm)
    }, () => None)
    (B, f, p, quotInv)
  }
  
  def inverse(domain:TypeLevel, structure:TypeLevel, rel:InternalDeclaration, quot: TermLevel, B: TypeLevel, F:TermLevel, P:TermLevel, quotInv: GlobalName, name: Option[String])(implicit parent: GlobalName): Constant = {
    val Ltp = () => {
      val ctx = B.context
      val (b, f, p, arg) = (B.makeVar("b", ctx), newVar(uniqueLN("f"), F.tp, Some(ctx)), newVar(uniqueLN("p"), P.tp, Some(ctx)), domain.makeVar("x", ctx))
      val im = OMV(uniqueLN("image_point")) % quot.applyTo(arg.toTerm)
      PiOrEmpty(List(b, f, p, arg), Eq(ApplyGeneral(OMS(quotInv), List(b, f, p).map(_.toTerm) :+ quot.applyTo(arg.toTerm)), F.applyTo(arg.toTerm)))
    }
  makeConst(uniqueLN(name getOrElse "quotInverse"), Ltp)
  }
}

object QuotientRule extends StructuralFeatureRule(classOf[Quotients], "Quotients")
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
    val context = Type.getParameters(dd)
    try {
      /*val eqRel = controller.getO(dd.getDeclarationsElaborated.head.parent / dd.getDeclarations.head.name) match {
        case Some(b) => b match {
          case d : DerivedDeclaration => if (d.feature != "equivalence_relation") throw LocalError("Equivalence relation expected.") 
            else d.getDeclarations.head match {case c: Constant => fromConstant(c, controller, Some(context))}
          case _ => throw LocalError("Equivalence relation expected.")
        }
      }*/ 
      val eqRel = dd.getDeclarations.last match {case c:Constant=>fromConstant(c, controller, Some(context))}
      val dom = eqRel.args match {
        case List((_, d), (_,_)) => d
        case tp => log("Unexpectedly found: . ")
      }
      val domain = TypeLevel(uniqueGN("A"), Nil, None, None, Some(context))
            
      val structure : TypeLevel = structureDeclaration(Some("Q"))
      val quotientMap = introductionDeclaration(structure, List(domain), Some("quotient_map"))
      val quotMapSurj = quotientMap.surjDecl
      val (b, f, p, quotInvert) = quotientInvert(domain, structure, eqRel, quotientMap, Some("quotInvert"))
      val quotInverse = quotientInverse(domain, structure, eqRel, quotientMap, b, f, p, quotInvert, Some("quotInverse"))
      
      val elabDecls = List(structure, quotientMap, quotInvert).map(_.toConstant) ++ List(quotInverse, quotMapSurj)
      new Elaboration {
        def domain = elabDecls map {d => log(controller.presenter.asString(d)); d.name}
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
  def quotientInvert(domain:TypeLevel, structure:TypeLevel, rel:InternalDeclaration, quot: TermLevel, name: Option[String])(implicit parent: GlobalName) = {
    val B = structureDeclaration(Some("B"))
    val f = TermLevel(uniqueGN("f"), List((None, domain.toTerm)), B.toTerm, None, None, None)
    val xy @ List(x, y) = List(domain.makeVar("x", Context.empty), structure.makeVar("y", Context.empty))
    val p = TermLevel(uniqueGN("p"), xy map (x => (Some(x.name), x.tp.get)), Arrow(rel.applyTo(xy), Eq(f.applyTo(x.tp.get), f.applyTo(y.tp.get))), None, None, None)
    (B, f, p, TermLevel(uniqueGN(name getOrElse "quotInvert"), List(B, f, p).map(x => (Some(x.name), x.tp)) :+ (Some(structure.name), structure.toTerm), B.toTerm, None, None, None))
  }
  def quotientInverse(domain:TypeLevel, structure:TypeLevel, rel:InternalDeclaration, quot: TermLevel, B: TypeLevel, F:TermLevel, P:TermLevel, quotInv: TermLevel, name: Option[String])(implicit parent: GlobalName): Constant = {
    val ctx = B.context
    val (b, f, p, arg) = (B.makeVar("b", ctx), newVar(uniqueLN("f"), F.tp, Some(ctx)), newVar(uniqueLN("p"), P.tp, Some(ctx)), domain.makeVar("x", ctx))
    val im = OMV(uniqueLN("image_point")) % quot.applyTo(arg.toTerm)
    val tp = PiOrEmpty(List(b, f, p, arg), Eq(quotInv.applyTo(List(b, f, p).map(_.toTerm) :+ quot.applyTo(arg.toTerm)), F.applyTo(arg.toTerm)))
    makeConst(uniqueLN(name getOrElse "quotInverse"), tp)
  }
}

object QuotientRule extends StructuralFeatureRule(classOf[Quotients], "Quotients")
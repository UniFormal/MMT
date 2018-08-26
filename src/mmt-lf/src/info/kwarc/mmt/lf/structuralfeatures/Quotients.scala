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
}

object QuotientRule extends StructuralFeatureRule(classOf[Quotients], "Quotients")
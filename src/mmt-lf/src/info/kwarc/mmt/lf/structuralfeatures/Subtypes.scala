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
            
      val structure : TypeLevel = structureDeclaration(Some("S"))
      val injection = introductionDeclaration(structure, List(domain), Some("injection"))
      val injInjective = injection.injDecl
      //val test = Constant(parent, uniqueLN("test"), Nil, Some())
      //val (b, f, p, injInvert) = invertInj(domain, structure, eqRel, injection, Some("injInvert"))
      //val subInverse = injInverse(domain, structure, eqRel, injection, b, f, p, injInvert, Some("quotInverse"))
      
      val elabDecls = List(structure, injection).map(_.toConstant) :+ injInjective
      new Elaboration {
        def domain = elabDecls map {d => log(controller.presenter.asString(d)); d.name}
        def getO(n: LocalName) = {
          elabDecls.find(_.name == n)
        }
      }
    } catch {
      case e: Error => throw LocalError("Wrong argument: subtype expects a relation as an argument: "+e.getMessage)
    }
  }
}

object SubtypeRule extends StructuralFeatureRule(classOf[Subtypes], "Subtypes")
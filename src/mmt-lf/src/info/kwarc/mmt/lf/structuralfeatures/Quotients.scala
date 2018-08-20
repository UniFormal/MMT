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
    implicit val parentTerm = OMID(parent.path / name)
    val context = Type.getParameters(dd)
    try {
      /*val eqRel = controller.getO(dd.getDeclarationsElaborated.head.parent / dd.getDeclarations.head.name) match {
        case Some(b) => b match {
          case d : DerivedDeclaration => if (d.feature != "equivalence_relation") throw LocalError("Equivalence relation expected.") 
            else d.getDeclarations.head match {case c: Constant => fromConstant(c, controller)}
          case _ => throw LocalError("Equivalence relation expected.")
        }
      }*/ 
      val eqRel = dd.getDeclarations.head match {case c:Constant=>fromConstant(c, controller)}
      val FunType(List((_, dom)), _) = eqRel.tp
      val domain = TypeLevel(uniqueGN("A"), Nil, None, None)
            
      val structure : TypeLevel = structureDeclaration(Some("q"))
      val quotientMap = introductionDeclaration(structure, Nil, Some("quotient_map"))
      val quotEq : TermLevel = quotientEq(structure, eqRel, quotientMap, None)
      
      val elabDecls : List[Constant]= List(structure, quotientMap, quotEq) map(_.toConstant)
      new Elaboration {
        def domain = elabDecls map {d => d.name}
        def getO(n: LocalName) = {
          elabDecls.find(_.name == n)
        }
      }
    } catch {
      case _ => throw LocalError("Wrong argument: Equivalence relation expects a relation as an argument. ")
    }
  }
}

object QuotientRule extends StructuralFeatureRule(classOf[Quotients], "Quotients")
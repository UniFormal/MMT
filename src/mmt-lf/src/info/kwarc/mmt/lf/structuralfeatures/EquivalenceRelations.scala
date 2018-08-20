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

class EquivalenceRelation extends StructuralFeature("equivalence_relation") with ParametricTheoryLike {
  override def check(dd: DerivedDeclaration)(implicit env: ExtendedCheckingEnvironment) {}

  def elaborate(parent: DeclaredModule, dd: DerivedDeclaration) = { 
    val name = LocalName(dd.path.last)
    implicit val parentTerm = OMID(parent.path / name)
    val context = Type.getParameters(dd)
    try {
      val relat = context.last
      val Arrow(dom, codom) = relat.tp.get
      val Arrow(a2, target) = codom
      if (dom != a2 || target != Univ(1))
        throw LocalError("Wrong argument: Equivalence relation expects a relation (A -> A -> Type) as an argument. ")
      val eqRel = fromVarDecl(relat, Some(context.init), controller)
      def rel(a: VarDecl, b: VarDecl) = eqRel.applied(List(a, b))
      
      val (a, b, c) = (InternalDeclarationUtil.newVar(uniqueLN("a"), dom), InternalDeclarationUtil.newVar(uniqueLN("b"), dom), InternalDeclarationUtil.newVar(uniqueLN("c"), dom))
      val transTp = OMV(uniqueLN("trans_"+eqRel.name)) % Pi(context++a++b++c, Arrow(rel(a,b), Arrow(rel(a, c), rel(b, c))))
      val symmTp = OMV(uniqueLN("symm_"+eqRel.name)) % Pi(context++a++b, Arrow(rel(a, b), rel(b, a)))
      val reflTp = OMV(uniqueLN("refl_"+eqRel.name)) % Pi(context++a, rel(a, a))
      
      val elabDecls = eqRel.toConstant::(List(transTp, symmTp, reflTp) map {x => Constant(parentTerm, x.name, Nil, x.tp, None, None)})
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

object EquivalenceRelationRule extends StructuralFeatureRule(classOf[EquivalenceRelation], "EquivalenceRelation")
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

class EquivalenceRelation extends StructuralFeature("equivRel") with ParametricTheoryLike {
  override def check(dd: DerivedDeclaration)(implicit env: ExtendedCheckingEnvironment) {}

  def elaborate(parent: DeclaredModule, dd: DerivedDeclaration) = { 
    //val name = LocalName(dd.path.last)
    implicit val parentTerm = dd.path
    val context = Type.getParameters(dd)
    try {
      /*val relat = context.last
      /val Arrow(dom, codom) = relat.tp.get
      val Arrow(a2, target) = codom
      if (dom != a2 || target != Univ(1))
        throw LocalError("Wrong argument: Equivalence relation expects a relation (A -> A -> Type) as an argument. ")*/
      val eqRel = dd.getDeclarations.head match {case c:Constant=>fromConstant(c, controller, Some(context))}
      val List((_, dom), (_, dom2)) = eqRel.args
      val target = eqRel.ret
      if (dom != dom2 || target != Univ(1))
        throw LocalError("Wrong argument: Equivalence relation expects a relation (A -> A -> Type) as an argument. ")
      
      def rel(a: VarDecl, b: VarDecl) = eqRel.applyTo(List(a, b))
      
      val (a, b, c) = (InternalDeclarationUtil.newVar(uniqueLN("a"), dom), InternalDeclarationUtil.newVar(uniqueLN("b"), dom), InternalDeclarationUtil.newVar(uniqueLN("c"), dom))
      val transTp = OMV(uniqueLN("trans_"+eqRel.name)) % Pi(context++a++b++c, Arrow(rel(a,b), Arrow(rel(a, c), rel(b, c))))
      val symmTp = OMV(uniqueLN("symm_"+eqRel.name)) % Pi(context++a++b, Arrow(rel(a, b), rel(b, a)))
      val reflTp = OMV(uniqueLN("refl_"+eqRel.name)) % Pi(context++a, rel(a, a))
      
      val elabDecls = eqRel.toConstant::(List(transTp, symmTp, reflTp) map {x => makeConst(x.name, () => {x.tp.get})})
      //elabDecls map {d => log(controller.presenter.asString(d))}
      new Elaboration {
        def domain = elabDecls map {d => d.name}
        def getO(n: LocalName) = {
          elabDecls.find(_.name == n)
        }
      }
    } catch {
      case e: Throwable  => val le = LocalError("Wrong argument: Equivalence relation expects a relation as an argument. ")
        e match {
        case err: Error => log("Error while elaborating the equivalence Relation"+dd.name+": "+err.getMessage)
        case _ => log("Throwable while elaborating the equivalence Relation "+dd.name+": "+e.getMessage)
      }
      throw le
    }
  }
}

object EquivalenceRelationRule extends StructuralFeatureRule(classOf[EquivalenceRelation], "EquivalenceRelation")
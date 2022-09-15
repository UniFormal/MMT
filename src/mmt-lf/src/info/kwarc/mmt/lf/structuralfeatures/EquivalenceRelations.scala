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
import info.kwarc.mmt.api.utils.MMT_TODO
import info.kwarc.mmt.api.uom.ExtendedSimplificationEnvironment

@deprecated("MMT_TODO: this is experimental and may still be removed", since="forever")
class EquivalenceRelation extends StructuralFeature("equivRel") with ParametricTheoryLike {
  override def check(dd: DerivedDeclaration)(implicit env: ExtendedCheckingEnvironment): Unit = {}

  def elaborate(parent: ModuleOrLink, dd: DerivedDeclaration)(implicit env: Option[uom.ExtendedSimplificationEnvironment] = None) = { 
    //val name = LocalName(dd.path.last)
    implicit val parentTerm = dd.path
    val context = Type.getParameters(dd)
    val params = if (context.isEmpty) None else Some(context)
    try {
      /*val relat = context.last
      /val Arrow(dom, codom) = relat.tp.get
      val Arrow(a2, target) = codom
      if (dom != a2 || target != Univ(1))
        throw LocalError("Wrong argument: Equivalence relation expects a relation (A -> A -> Type) as an argument. ")*/
      val eqRel = dd.getDeclarations.head match {case c:Constant=>fromConstant(c, controller, Nil, Some(context))}
      val List((_, dom), (_, dom2)) = eqRel.args
      val target = eqRel.ret
      if (dom != dom2 || target != Univ(1))
        throw LocalError("Wrong argument: Equivalence relation expects a relation (A -> A -> Type) as an argument. ")
      
      def rel(a: VarDecl, b: VarDecl) = eqRel.applyTo(List(a, b))
      
      val (a, b, c) = (InternalDeclarationUtil.newVar("a", dom, params), InternalDeclarationUtil.newVar("b", dom, params), InternalDeclarationUtil.newVar("c", dom, params))
      val transTp = OMV(uniqueLN("trans_"+eqRel.name, params)) % Pi(context++a++b++c, Arrow(rel(a,b), Arrow(rel(a, c), rel(b, c))))
      val symmTp = OMV(uniqueLN("symm_"+eqRel.name, params)) % Pi(context++a++b, Arrow(rel(a, b), rel(b, a)))
      val reflTp = OMV(uniqueLN("refl_"+eqRel.name, params)) % Pi(context++a, rel(a, a))
      
      val elabDecls = eqRel.toConstant(false)::(List(transTp, symmTp, reflTp) map {x => makeConst(x.name, () => {x.tp.get})})
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
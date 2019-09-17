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
import StructuralFeatureUtils._
import StructuralFeatureUtil._
import inductiveUtil._
import structuralfeatures.InductiveTypes._

/** theories as a set of types of expressions */ 
class Reflections extends StructuralFeature("reflect") with TypedParametricTheoryLike {
  
  /**
   * Checks the validity of the inductive type(s) to be constructed
   * @param dd the derived declaration from which the inductive type(s) are to be constructed
   */
  override def check(dd: DerivedDeclaration)(implicit env: ExtendedCheckingEnvironment) {}

  /**
   * Elaborates an declaration of one or multiple mutual inductive types into their declaration, 
   * as well as the corresponding no confusion and no junk axioms
   * Constructs a structure whose models are exactly the (not necessarily initial) models of the declared inductive types
   * @param parent The parent module of the declared inductive types
   * @param dd the derived declaration to be elaborated
   */
  def elaborate(parent: ModuleOrLink, dd: DerivedDeclaration)(implicit env: Option[uom.ExtendedSimplificationEnvironment] = None) = {

    val (indDefPathGN, context, indParams) = ParamType.getParams(dd)
    val indDefPath = indDefPathGN.toMPath
    // TODO: figure out how to work with theory parameters
    val (indCtx, consts) = controller.getO(indDefPath) match {
      case Some(str) => str match {
        case t: Theory => (Context.empty, getConstants(t.getDeclarations, controller))
        case t: StructuralElement if (t.feature =="theory") => 
          val decls = t.getDeclarations map {case d: Declaration => d case _ => throw LocalError("unsupported structural element at "+t.path)}
          (Context.empty, getConstants(decls, controller))
        case m : ModuleOrLink => (Context.empty, getConstants(m.getDeclarations, controller))
        case t => throw GeneralError("reflection over unsupported target (expected module or link)"+t.path+" of feature "+t.feature + ": "+t)
      }
      case None => throw GeneralError("target of reflection not found at "+indDefPath)
    }
    
    val declsPre = readInternalDeclarations(consts, controller, Some(context))(indDefPathGN)
    val subst = indCtx zip indParams map {case (vd, param) => Sub(vd.name, param)}
    val tr = OMSReplacer({p:GlobalName => if (p.module == indDefPath.module/indDefPath.name) Some(OMS(dd.modulePath ? p.name)) else None})
    val decls = declsPre map (_.translate(ApplySubs(subst)).translate(TraversingTranslator(tr)))
    
    structuralfeatures.InductiveTypes.elaborateDeclarations(context, decls, controller, Some({c => log(defaultPresenter(c)(controller))}))(dd.path)
  }
}

object ReflectionRule extends StructuralFeatureRule(classOf[Reflections], "reflect")
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
    val (indDefPath, context, indParams) = ParamType.getParams(dd)
    val consts = controller.getO(indDefPath) match {
      case Some(m : ModuleOrLink) => getConstants(m.getDeclarations, controller)
      case Some(t) => throw GeneralError("reflection over unsupported target (expected module or link)"+t.path)
      case None => throw GeneralError("target of reflection not found at "+indDefPath)
    }
    val decls = readInternalDeclarations(consts, controller, Some(context))(indDefPath)
        
    
    structuralfeatures.InductiveTypes.elaborateDeclarations(context, decls)(dd.path)
  }
}

object ReflectionRule extends StructuralFeatureRule(classOf[Reflections], "reflect")
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

object ReflectionsUtil {
  def feature = {"reflect"}
}

/** theories as a set of types of expressions */ 
class Reflections extends StructuralFeature(ReflectionsUtil.feature) with ReferenceLikeTypedParametricTheoryLike {
   /**
   * Checks the validity of the inductive type(s) to be constructed
   * @param dd the derived declaration from which the inductive type(s) are to be constructed
   */
  override def check(dd: DerivedDeclaration)(implicit env: ExtendedCheckingEnvironment): Unit = {
    val (_, _, indCtx, indParams, context) = getContent(dd)
    checkParams(indCtx, indParams, Context(dd.parent)++context, env)
  }
  /**
   * Elaborates an declaration of one or multiple mutual inductive types into their declaration, 
   * as well as the corresponding no confusion and no junk axioms
   * Constructs a structure whose models are exactly the (not necessarily initial) models of the declared inductive types
   * @param parent The parent module of the declared inductive types
   * @param dd the derived declaration to be elaborated
   */
  def elaborate(parent: ModuleOrLink, dd: DerivedDeclaration)(implicit env: Option[uom.ExtendedSimplificationEnvironment] = None) = {
    val (indDefPathGN, consts, indCtx, indParams, context) = getContent(dd)
    val indDefPath = indDefPathGN.toMPath

    val declsPre = readInternalDeclarations(consts, controller, Some(context))(dd.path)
    val subst = indCtx zip indParams map {case (vd, param) => Sub(vd.name, param)}
    val tr = OMSReplacer({(p:GlobalName) => if (p.module == indDefPath) Some(OMS(dd.modulePath ? p.name)) else None})
    val decls = declsPre map (_.translate(ApplySubs(subst)).translate(TraversingTranslator(tr)))
    
    structuralfeatures.InductiveTypes.elaborateDeclarations(context, decls, controller, Some({c => log(defaultPresenter(c)(controller))}))(dd.path)
  }

  /**
   * parse the derived declaration into its components
   * @param dd the derived declaration
   * @return returns a pair containing the mpath of the derived declaration, the constants defined in the referenced theory,
   *         the argument context of this derived declaration, the arguments provided to the referenced theory and the outer context
   */
  def getContent(dd:DerivedDeclaration): (GlobalName, List[Constant], Context, List[Term], Context) = {
    val (indDefPathGN, decls, indCtx, indParams, context) = getDecls(dd)
    (indDefPathGN, getConstants(decls, controller)(dd.path.toMPath), indCtx, indParams, context)
  }
}

object ReflectionRule extends StructuralFeatureRule(classOf[Reflections], "reflect")
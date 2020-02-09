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
  override def check(dd: DerivedDeclaration)(implicit env: ExtendedCheckingEnvironment) {
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

    val declsPre = readInternalDeclarations(consts, controller, Some(context))(indDefPathGN)
    val subst = indCtx zip indParams map {case (vd, param) => Sub(vd.name, param)}
    val tr = OMSReplacer({p:GlobalName => if (p.module == indDefPath) Some(OMS(dd.modulePath ? p.name)) else None})
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
    val (indDefPathGN, context, indParams) = ParamType.getParams(dd)
    val (indCtx, consts) = controller.getO(indDefPathGN) match {
      case Some(str) => str match {
        case t: Theory => (t.parameters, getConstants(t.getDeclarations, controller))
        case t: StructuralElement if (t.feature =="theory") =>
          val decls = t.getDeclarations map {case d: Declaration => d case _ => throw LocalError("unsupported structural element at "+t.path)}
          //We shouldn't have to do something this ugly, for this match to work out
          //TODO:There should be a better method provided by the api
          val params = t.headerInfo match {
            case HeaderInfo("theory", _, List(_, params: Context)) => params
            case _ => Context.empty
          }
          (params, getConstants(decls, controller))
        case m : ModuleOrLink => (Context.empty, getConstants(m.getDeclarations, controller))
        case t => throw GeneralError("reflection over unsupported target (expected a theory)"+t.path+" of feature "+t.feature + ": "+t)
      }
      case None => throw GeneralError("target of reflection not found at "+indDefPathGN)
    }
    (indDefPathGN, consts, indCtx, indParams, context)
  }
}

object ReflectionRule extends StructuralFeatureRule(classOf[Reflections], "reflect")
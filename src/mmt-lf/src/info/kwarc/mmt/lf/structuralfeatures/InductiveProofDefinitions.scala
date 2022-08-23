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
import TermConstructingFeatureUtil._
import StructuralFeatureUtils._
import StructuralFeatureUtil._
import inductiveUtil._
import InductiveTypes._

/** theories as a set of types of expressions */ 
class InductiveProofDefinitions extends StructuralFeature("ind_proof") with TypedParametricTheoryLike {
  
  /**
   * Checks the validity of the inductive type(s) to be constructed
   * @param dd the derived declaration from which the inductive type(s) are to be constructed
   */
  override def check(dd: DerivedDeclaration)(implicit env: ExtendedCheckingEnvironment): Unit = {
    val (context, indParams, indD, indCtx) = parseTypedDerivedDeclaration(dd, Some(inductiveUtil.compatibleFeatures))
    checkParams(indCtx, indParams, Context(dd.parent)++context, env)
  }
  
  /**
   * Checks that each definien matches the expected type
   */
  override def expectedType(dd: DerivedDeclaration, c: Constant): Option[Term] = {
    val (_, _, indD, indCtx) = parseTypedDerivedDeclaration(dd, Some(inductiveUtil.compatibleFeatures))
    
    val intDecls = parseInternalDeclarationsSubstitutingDefiniens(indD, controller, Some(indCtx))
    val (constrdecls, tpdecls) = (constrs(intDecls), tpls(intDecls))
    val (_, _, indPfCases, _) = inductionHypotheses(tpdecls, constrdecls, indCtx)(indD.path)
    
    val intDecl = intDecls.find(_.name == c.name).getOrElse(throw ImplementationError("No declaration to give a definien for is found for "+c.path))
    val tp = utils.listmap(indPfCases, intDecl).map(_.tp.get)
		//Replace references to the inductive claims by their definitions read earlier
    val externalTp = tp map {tp =>
      val substs = indPfCases.filter(_._1.isTypeLevel).map({case (tpl: TypeLevel, claim: VarDecl) =>
        Sub(claim.name, dd.get(tpl.name).toTerm)})
      tp ^ substs
    }
    Some(externalTp.get)
  }

  /**
   * Elaborates an declaration of one or multiple mutual inductive types into their declaration, 
   * as well as the corresponding no confusion and no junk axioms
   * Constructs a structure whose models are exactly the (not necessarily initial) models of the declared inductive types
   * @param parent The parent module of the declared inductive types
   * @param dd the derived declaration to be elaborated
   */
  def elaborate(parent: ModuleOrLink, dd: DerivedDeclaration)(implicit env: Option[uom.ExtendedSimplificationEnvironment] = None) = {
    val (context, indParams, indD, indCtx) = parseTypedDerivedDeclaration(dd, Some(inductiveUtil.compatibleFeatures))
    implicit val parent = indD.path

    val intDecls = parseInternalDeclarations(indD, controller, None)
    var intTpls: List[TypeLevel] = intDecls.filter(_.isTypeLevel).collect{case t:TypeLevel => t}
    
    val intTplNames = intTpls map (_.name)
    
    var decls = parseInternalDeclarationsWithDefiniens(dd, controller, Some(context))
     
    val proof_paths = intTpls.map(t=>externalName(indD.path, proofName(t.name)))
    val (preds, indProofDeclMap, predsMap, _) = inductionHypotheses(intTpls, constrs(intDecls), indCtx)
    
    val modelDf = decls map (_.df.get)
    val indTplsArgs = intTpls map(_.argContext(None)._1)
    
    val intTplsDef = intTplNames map (nm => decls.find(_.name == nm).getOrElse(
        throw LocalError("No declaration found for the typelevel: "+nm)).df.get)
    val Tps = intTpls zip intTplsDef map {case (intTpl, indTplDef) => 
      val (indTplDefArgs, indTplDefBody) = unapplyLambdaOrEmpty(indTplDef)
      //val indTplDefArgsBar = rBar(indTplDefArgs, intTpls, indProofDeclMap)
      PiOrEmpty(context++indTplDefArgs, indTplDefBody)}
    val Dfs = indTplsArgs zip proof_paths map {case (indTplArgs, proof_path) => 
      LambdaOrEmpty(context++indTplArgs, ApplyGeneral(OMS(proof_path), indParams++modelDf++indTplArgs.map(_.toTerm)))}
    
    val inductDefs = (intTpls zip Tps zip Dfs) map {case ((tpl, tp), df) => 
      makeConst(tpl.name, ()=> {tp}, false, () => {Some(df)})(dd.path)}
    
    externalDeclarationsToElaboration(inductDefs, Some({c => log(defaultPresenter(c)(controller))}))
  }
}

object InductiveProofDefinitionRule extends StructuralFeatureRule(classOf[InductiveProofDefinitions], "ind_proof")

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

/** theories as a set of types of expressions */ 
class InductiveProofDefinitions extends StructuralFeature("ind_proof") with TypedParametricTheoryLike {
  
  /**
   * Checks the validity of the inductive type(s) to be constructed
   * @param dd the derived declaration from which the inductive type(s) are to be constructed
   */
  override def check(dd: DerivedDeclaration)(implicit env: ExtendedCheckingEnvironment) {
    val (context, indParams, indD, indCtx) = parseTypedDerivedDeclaration(dd, "inductive")
    checkParams(indCtx, indParams, Context(dd.parent)++context, env)
  }
  
  /**
   * Checks that each definien matches the expected type
   */
  override def expectedType(dd: DerivedDeclaration, con: Controller, c: Constant): Option[Term] = {
    val (_, _, indD, indCtx) = parseTypedDerivedDeclaration(dd, "inductive")
    
    val intDecls = parseInternalDeclarations(indD, con, Some(indCtx))
    val (constrdecls, tpdecls) = (constrs(intDecls), tpls(intDecls))
    val (_, _, indProofDeclMap, ctx) = InductiveTypes.inductionHypotheses(tpdecls, constrdecls, indCtx)(indD.path)
    
    val intDecl = intDecls.find(_.name == c.name)
    
    utils.listmap(indProofDeclMap, intDecl) map (_.tp.get)
  }

  /**
   * Elaborates an declaration of one or multiple mutual inductive types into their declaration, 
   * as well as the corresponding no confusion and no junk axioms
   * Constructs a structure whose models are exactly the (not necessarily initial) models of the declared inductive types
   * @param parent The parent module of the declared inductive types
   * @param dd the derived declaration to be elaborated
   */
  def elaborate(parent: ModuleOrLink, dd: DerivedDeclaration) = {
    val (context, _, indD, indCtx) = parseTypedDerivedDeclaration(dd, "inductive")
    implicit val parent = indD.path

    val intDecls = parseInternalDeclarations(indD, controller, None)
    var intTpls: List[TypeLevel] = intDecls.filter(_.isTypeLevel).collect{case t:TypeLevel => t}
    
    val intTplNames = intTpls map (_.name)
    
    var decls = parseInternalDeclarationsWithDefiniens(dd, controller, Some(context))
     
    // and whether we have all necessary declarations
    intDecls filter {d => decls forall (_.name != d.name)} map {d => 
      decls map (d => println(d.toString()))
      throw LocalError("No declaration found for the internal declaration "+d.name+" of "+indD.name+":\n The declarations (at "+dd.path+") are: \n"+decls.map(_.toString()).toString())
    }
    
    val proof_paths = intTpls.map(t=>t.path.copy(name=indD.name/proofName(t.name)))
    
    val modelDf = decls map (_.df.get)
    val indTplsArgs = intTpls map(_.argContext(None)._1)
    
    val intTplsDef = intTplNames map (nm => decls.find(_.name == nm).getOrElse(
        throw LocalError("No declaration found for the typelevel: "+nm)).df.get)
    val Tps = intTpls zip intTplsDef map {case (intTpl, indTplDef) => PiOrEmpty(context, Arrow(intTpl.toTerm, indTplDef))}
    val Dfs = indTplsArgs zip proof_paths map {case (indTplArgs, proof_path) => 
      LambdaOrEmpty(context++indTplArgs, ApplyGeneral(OMS(proof_path), context.map(_.toTerm)++intTplsDef++modelDf++indTplArgs.map(_.toTerm)))}
    
    val inductDefs = (intTpls zip Tps zip Dfs) map {case ((tpl, tp), df) => 
      makeConst(dd.name/tpl.name, ()=> {tp}, false, () => {Some(df)})}
    //inductDefs foreach (d => log(defaultPresenter(d)(controller)))
    
    externalDeclarationsToElaboration(inductDefs, Some({c => log(defaultPresenter(c)(controller))}))
  }
}

object InductiveProofDefinitionRule extends StructuralFeatureRule(classOf[InductiveProofDefinitions], "ind_proof")
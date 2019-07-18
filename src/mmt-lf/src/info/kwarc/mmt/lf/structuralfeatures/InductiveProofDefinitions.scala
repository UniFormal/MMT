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
  override def check(dd: DerivedDeclaration)(implicit env: ExtendedCheckingEnvironment) {}
  
  /**
   * Checks that each definien matches the expected type
   */
  override def expectedType(dd: DerivedDeclaration, con: Controller, c: Constant): Option[Term] = {
    val (intDeclsPath, _, indParams) = ParamType.getParams(dd)
    val (indD, indCtx) = controller.library.get(intDeclsPath) match {
      case indD: DerivedDeclaration if (indD.feature == "inductive") => (indD, Type.getParameters(indD))
      case _ => throw LocalError("Expected definition of corresponding inductively-defined types at "+intDeclsPath+" but none found at that location.")
    }
    //check the indParams match the indCtx at least in length
    // TODO: Check the types match as well
    if (indCtx.length != indParams.length) throw LocalError("Incorrect length of parameters for the derived declaration "+indD.name+".\nExpected "+indCtx.length+" parameters but found "+indParams.length+".")

    
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
    val (indDefPath, context, indParams) = ParamType.getParams(dd)
    val (indD, indCtx) = controller.library.get(indDefPath) match {
      case indD: DerivedDeclaration if (indD.feature == "inductive") => (indD, Type.getParameters(indD))
      case d: DerivedDeclaration => throw LocalError("the referenced derived declaration is not of the feature inductive but of the feature "+d.feature+".")
      case _ => throw LocalError("Expected definition of corresponding inductively-defined types at "+indDefPath.toString()
            +" but no derived declaration found at that location.")
    }
    implicit val parent = indD.path
    val intDecls = parseInternalDeclarations(indD, controller, None)
    var intTpls: List[TypeLevel] = intDecls.filter(_.isTypeLevel).collect{case t:TypeLevel => t}
    
    val indTplNames = intTpls map (_.name)
    
    var decls = parseInternalDeclarationsWithDefiniens(dd, controller, Some(context))
     
    // and whether we have all necessary declarations
    intDecls filter {d => decls forall (_.name != d.name)} map {d => 
      decls map (d => println(d.toString()))
      throw LocalError("No declaration found for the internal declaration "+d.name+" of "+indD.name+":\n The declarations (at "+dd.path+") are: \n"+decls.map(_.toString()).toString())
    }
    
    val proof_paths = intTpls.map(t=>t.path.copy(name=indD.name/proofName(t.name)))
    
    val modelDf = decls map (_.df.get)
    val indTplsArgs = intTpls map(_.argContext(None)._1)
    
    val indTplsDef = indTplNames map (nm => decls.find(_.name == nm).getOrElse(
        throw LocalError("No declaration found for the typelevel: "+nm)).df.get)
    val Tps = intTpls zip indTplsDef map {case (indTpl, indTplDef) => PiOrEmpty(context, Arrow(indTpl.toTerm, indTplDef))}
    val Dfs = indTplsArgs zip proof_paths map {case (indTplArgs, proof_path) => 
      LambdaOrEmpty(context++indTplArgs, ApplyGeneral(OMS(proof_path), indParams++indTplsDef++modelDf++indTplArgs.map(_.toTerm)))}
    
    val inductDefs = (intTpls zip Tps zip Dfs) map {case ((tpl, tp), df) => 
      makeConst(dd.name/tpl.name, ()=> {tp}, false, () => {Some(df)})}
    //inductDefs foreach (d => log(defaultPresenter(d)(controller)))
    
    externalDeclarationsToElaboration(inductDefs, Some({c => log(defaultPresenter(c)(controller))}))
  }
  
  /**
   * checks whether d.tp matches the type decl.externalTp
   * @param d the declaration whoose type to check
   * @param decl the corresponding declaration which is to be defined by d
   * @note this will return an error if d.tp doesn't match decl.externalTp
   */
  def checkDecl(d: InternalDeclaration, decl: InternalDeclaration) {
    //TODO: This should be implemented to provide more accurate error messages
    //TODO: It should set the tp.analize fields of the internal declarations to the expected types (and definitions if any)
  }
}

object InductiveProofDefinitionRule extends StructuralFeatureRule(classOf[InductiveProofDefinitions], "ind_proof")
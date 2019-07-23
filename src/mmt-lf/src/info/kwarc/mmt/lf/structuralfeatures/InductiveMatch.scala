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
import inductiveUtil._
import StructuralFeatureUtils._
import StructuralFeatureUtil._


/** theories as a set of types of expressions */ 
class InductiveMatch extends StructuralFeature("match") with TypedParametricTheoryLike {
  
  /**
   * Compute the expected type for an inductive definition case
   */
  def expectedType(dd: DerivedDeclaration, intDecls: List[InternalDeclaration], indDPath: GlobalName, c: Constant) : (Term, (Boolean, Option[GlobalName])) = {
    val tpdecls = tpls(intDecls)
    val tplPathMap = tpdecls map {tpl =>
      (tpl.externalPath(indDPath), correspondingDecl(dd, tpl.name).get.df.get)
    }
    
    val intC = intDecls.find(_.name == c.name).get
    val tr = TraversingTranslator(OMSReplacer(p => utils.listmap(tplPathMap, p)))
    val cons = intC match {
      case const : Constructor => 
        val intTpl = const.getTpl
        val tplC = dd.getDeclarations.find(_.name == intTpl.name).get.path
        (true, Some(tplC))
      case _ => (false, None)
    }
    (tr(Context.empty, intC.externalTp(indDPath)), cons)
  }  
  
   /**
   * Check that each definien matches the expected type
   */
  override def expectedType(dd: DerivedDeclaration, c: Constant): Option[Term] = {
    val (context, indParams, indD, indCtx) = parseTypedDerivedDeclaration(dd, "inductive")
    
    val intDecls = parseInternalDeclarations(indD, controller, Some(indCtx))
    Some(expectedType(dd, intDecls, indD.path, c)._1)
  }

  /**
   * Elaborates an declaration of one or multiple mutual inductive types into their declaration, 
   * as well as the corresponding no confusion and no junk axioms
   * Constructs a structure whose models are exactly the (not necessarily initial) models of the declared inductive types
   * @param parent The parent module of the inductive type to be elaborated
   * @param dd the derived declaration to be elaborated
   */
  def elaborate(parent: ModuleOrLink, dd: DerivedDeclaration) = {
    val (context, _, indD, indCtx) = parseTypedDerivedDeclaration(dd, "inductive")   
    implicit val parent = indD.path
    
    val intDecls = parseInternalDeclarations(indD, controller, Some(indCtx))
    var intTpls = tpls(intDecls)
    
    val indTplNames = intTpls map (_.name)
    
    var decls = parseInternalDeclarationsWithDefiniens(dd, controller, Some(context))
    
    val induct_paths = intTpls.map(t=>t.path.copy(name=inductName(t.name)))
    val unapply_paths = intDecls.map(t=>t.path.copy(name=unapplierName(t.name)))
    def defined(d: InternalDeclaration) = {decls.map(_.name) contains d.name}
    def definition(d: InternalDeclaration): Term = {
      val decl = decls.find(_.name == d.name).getOrElse(throw LocalError("The declaration "+d.name+" must also be defined."))
      decl.df.getOrElse(throw LocalError("No definien for declaration: "+decl.name))
    }
    val types = intTpls map (_.path)
    val mapDefs = intDecls map {
      case d: TypeLevel if defined(d) =>  
        PiOrEmpty(context++indCtx++d.argContext(None)._1, definition(d))
      case constr: Constructor if (defined(constr)) => 
        val tpl = constr.getTpl
        val Arrow(_, ret) = definition(tpl)
        PiOrEmpty(context++indCtx++constr.argContext(None)._1, MAP(constr.externalRet(indD.path), ret, definition(constr)))
      case d: Constructor => PiOrEmpty(context++indCtx++d.argContext(None)._1, NONE(d.externalRet))
      case d => PiOrEmpty(context, d.toTerm)
    }
    
    val modelDf = decls map (_.df.get)
    val indTplsArgs = intTpls map(_.argContext(None)._1)
    
    val indTplsDef = indTplNames map (nm => decls.find(_.name == nm).getOrElse(
        throw LocalError("No declaration found for the typelevel: "+nm)).df.get)
    val Tps = intTpls zip indTplsDef map {case (indTpl, indTplDef) => PiOrEmpty(context, Arrow(indTpl.toTerm, indTplDef))}
    val Dfs = indTplsArgs zip induct_paths map {case (indTplArgs, induct_path) => 
      LambdaOrEmpty(context++indTplArgs, ApplyGeneral(OMS(induct_path), context.map(_.toTerm)++modelDf++indTplArgs.map(_.toTerm)))}
    
    val inductDefs = (intTpls zip Tps zip Dfs) map {case ((tpl, tp), df) => 
      makeConst(dd.name/tpl.name, ()=> {tp}, false, () => {Some(df)})}
    //inductDefs foreach (d => log(defaultPresenter(d)(controller)))
    
    externalDeclarationsToElaboration(inductDefs, Some({c => log(defaultPresenter(c)(controller))}))
  }
}

object InductiveMatchRule extends StructuralFeatureRule(classOf[InductiveMatch], "match")
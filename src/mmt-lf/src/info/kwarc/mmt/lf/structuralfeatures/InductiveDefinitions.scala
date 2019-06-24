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
import TermConstructingFeatureUtil._
import inductiveUtil._

/** theories as a set of types of expressions */ 
class InductiveDefinitions extends StructuralFeature("inductive_definition") with TypedParametricTheoryLike {
  
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
  def elaborate(parent: ModuleOrLink, dd: DerivedDeclaration) = {
    val (indDefPath, context, indParams) = ParamType.getParams(dd)
    val (indD, indCtx) = controller.library.get(indDefPath) match {
      case indD: DerivedDeclaration if (indD.feature == "inductive") => (indD, Type.getParameters(indD))
      case d: DerivedDeclaration => throw LocalError("the referenced derived declaration is not of the feature inductive but of the feature "+d.feature+".")
      case _ => throw LocalError("Expected definition of corresponding inductively-defined types at "+indDefPath.toString()
            +" but no derived declaration found at that location.")
    }
    //The internal definitions we need to give definiens for
    val indDefs = parseInternalDeclarations(indD, controller, None)(indD.path) filterNot (_.isOutgoing)
    var (indTpls, indTmls) = (tpls(indDefs), tmls(indDefs))
    
    val (indTplNames, indTmlNames, indDeclsNames) = (indTpls map (_.name), indTmls map (_.name), indDefs map (_.name))
    
    var decls = parseInternalDeclarationsWithDefiniens(dd, controller, Some(context), indD.path)
    
    // check whether all declarations match their corresponding constructors
    decls foreach { 
      d => correspondingDecl(indD, d.name) map { decl => 
      checkDecl(d, fromConstant(decl, controller, indTpls.map(_.path), None)(indD.path))
      }
    }
    // and whether we have all necessary declarations
    indD.getDeclarations.map(_.name).find(n => !decls.map(_.name).contains(n)) foreach {
      n => throw LocalError("No declaration found for the internal declaration "+n+" of "+indD.name+".")
    }
    
    val induct_paths = indDefs map (t=>t.path.copy(name=indD.name/inductName(t.name)))
    
    val modelDf = decls map (_.df.get)
    val indDeclsArgs = indDefs map(_.argContext(None)(indD.path)._1)
    
    val indDeclsDef = indDeclsNames map (nm => decls.find(_.name == nm).getOrElse(
        throw LocalError("No declaration found for the typelevel: "+nm)).df.get)
    val Tps = indDefs zip indDeclsDef map {
      case (indTpl, indTplDef) if indTpl.isTypeLevel => PiOrEmpty(context, Arrow(indTpl.toTerm(indD.path), indTplDef))
      case (indTml: Constructor, indTmlDef) => 
        val indTpl = indTml.getTpl(indTpls)
        val tml = decls.find(_.name == indTml.name).get
        val (args, dApplied) = tml.argContext(None)(indD.path)
        PiOrEmpty(context++args, Eq(indTpl.df.get, indTpl.applyTo(dApplied)(dd.path), indTmlDef))
    }
    val Dfs = indDefs zip indDeclsArgs zip induct_paths.map(OMS(_)) map {case ((indDef, indDefArgs), tm) => 
        PiOrEmpty(context++indDefArgs, ApplyGeneral(tm, indParams++modelDf++indDefArgs.map(_.toTerm)))
    }
    
    
    var inductDefs = (indDefs zip Tps zip Dfs) map {case ((tpl, tp), df) => 
      makeConst(tpl.name, ()=> {tp}, false,() => {Some(df)})(dd.path)}
    
    // Add a more convenient version of the declarations for the tmls by adding application to the function arguments via a chain of Congs
    val indTmlVDDecls = (indDefs zip inductDefs) filter({case (d, _) => d.isConstructor}) map {case (d, c) => (OMV(c.name) % c.tp.get, d)}
    val tpInitials : List[Term] = indTmlVDDecls map {case (_, d:Constructor) => 
      val indTpl = d.getTpl(indTpls)
      Arrow(indTpl.externalTp(dd.path), externalizeNamesAndTypes(dd.path, indTpl.context)(
          PiOrEmpty(indTpl.context, FunType(indTpl.args.tail, indTpl.ret))))
    }
    
    val inductTmlsAppliedDfs : List[Term] = (tpInitials zip indTmlVDDecls) map {case (tpInitial, (vd, d)) => Cong(vd, tpInitial, d.argContext(None)(dd.path)._1)}
    val inductTmlsApplied = indTmls zip inductTmlsAppliedDfs map {
      case (c,df) => makeConst(c.name, () => {c.tp}, false, () => {Some(df)})(dd.path)
    }
    inductDefs ++ inductTmlsApplied
    inductDefs foreach (d => log(defaultPresenter(d)(controller)))
    
    new Elaboration {
      def domain = inductDefs map (_.name)
      def getO(n: LocalName) = {
        inductDefs find (_.name == n) foreach(d=>log(defaultPresenter(d)(controller)))
        inductDefs find (_.name == n)
      }
    }
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

object InductiveDefinitionRule extends StructuralFeatureRule(classOf[InductiveDefinitions], "inductive_definition")
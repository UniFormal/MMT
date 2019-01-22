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
  def elaborate(parent: Module, dd: DerivedDeclaration) = {
    val (indDefPath, context, indParams) = ParamType.getParams(dd)
    val (indD, indCtx) = controller.library.get(indDefPath) match {
      case indD: DerivedDeclaration if (indD.feature == "inductive") => (indD, Type.getParameters(indD))
      case d: DerivedDeclaration => throw LocalError("the referenced derived declaration is not of the feature inductive but of the feature "+d.feature+".")
      case _ => throw LocalError("Expected definition of corresponding inductively-defined types at "+indDefPath.toString()
            +" but no derived declaration found at that location.")
    }
    implicit val parent = indD.path
    val indDefs = controller.library.get(indDefPath).getDeclarations map {case c:Constant => 
      fromConstant(c, controller, Some(indCtx))}
    var indTpls: List[TypeLevel] = indDefs.filter{case tpl: TypeLevel => true case _ => false}.map{case t:TypeLevel => t}
    
    val indTplNames = indTpls map (_.name)
    def correspondingDecl(d: LocalName): Option[Constant] = {
      indD.getO(d) map {
        case c: Constant=> c
        case dec => throw LocalError("Expected a constant at "+dec.path+".")
      }
    }
    
    
    var decls:List[InternalDeclaration] = Nil
    
    // read in the declarations and replace all occurances of previously declared symbols by their definitions
    dd.getDeclarations foreach {
      case c: Constant  if (c.df.isDefined) =>
        val intDecl = fromConstant(c, controller, Some(context))
        def repDfs(df: GlobalName) = {utils.listmap(decls.map(t => (t.path, ApplyGeneral(t.df.get, context.map(_.toTerm)))), df)}
        def mpDf(df: Term):Term = OMSReplacer(repDfs)(df, Context.empty)
        val repDf = Some(mpDf(intDecl.df.get))
        
        intDecl match {
          case d : TermLevel => val repD = d.copy(df = repDf); decls :+= repD
          case d : TypeLevel => val repD = d.copy(df = repDf); decls :+= repD
          case d : StatementLevel => val repD = d.copy(df = repDf); decls :+= repD 
         }
      case c: Constant => throw LocalError("Unsupported corresponding declaration: Expected constant with definien at "+c.path)
      case _ => throw LocalError("unsupported declaration")
    }
        
    // check whether all declarations match their corresponding constructors
    decls foreach { 
      d => correspondingDecl(d.name) map { decl => 
      checkDecl(d, fromConstant(decl, controller, None))
      }
    }
    // and whether we have all necessary declarations
    indD.getDeclarations.map(_.name).find(n => !decls.map(_.name).contains(n)) foreach {
      n => throw LocalError("No declaration found for the internal declaration "+n+" of "+indD.name+".")
    }
    
    //val indParams = Type.getParameters(inductiveType)
    val induct_paths = indTpls.map(t=>t.path.copy(name=LocalName("induct_"+t.name)))
    
    val modelDf = decls map (_.df.get)
    val indTplsArgs = indTpls map(_.argContext(None)._1)
    
    val indTplsDef = indTplNames map (nm => decls.find(_.name == nm).getOrElse(
        throw LocalError("No declaration found for the typelevel: "+nm)).df.get)
    val Tps = indTpls zip indTplsDef map {case (indTpl, indTplDef) => PiOrEmpty(context, Arrow(indTpl.toTerm, indTplDef))}
    val Dfs = indTplsArgs zip induct_paths map {case (indTplArgs, induct_path) => 
      PiOrEmpty(context, PiOrEmpty(indTplArgs, ApplyGeneral(OMS(induct_path), indParams++modelDf++indTplArgs.map(_.toTerm))))}
    
    val inductDefs = (indTpls zip Tps zip Dfs) map {case ((tpl, tp), df) => 
      makeConst(dd.name/tpl.name, ()=> {tp}, () => {Some(df)})}
    //inductDefs foreach (d => log(defaultPresenter(d)(controller)))
    
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
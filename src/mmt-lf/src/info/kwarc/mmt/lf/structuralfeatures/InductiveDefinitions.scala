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
class InductiveDefinitions extends StructuralFeature("inductive_definition") with ParametricTheoryLike {
  object noLookupPresenter extends presentation.NotationBasedPresenter {
    override def getNotations(p: GlobalName) = if (! (p.doc.uri.path contains "urtheories")) Nil else super.getNotations(p)
    override def getAlias(p: GlobalName) = if (true) Nil else super.getAlias(p)
  }
  
  override def start(args: List[String]) {
    initOther(noLookupPresenter)
  }
  
  def defaultPresenter(c: Constant)(implicit con: Controller): String = c.name + ": " + noLookupPresenter.asString(c.tp.get) + (if (c.df != None) " = "+noLookupPresenter.asString(c.df.get) else "")

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
    implicit val parentTerm = dd.path
    val context = Type.getParameters(dd)
    val indTplPath : GlobalName = try {
      dd.getDeclarations.head match {case c: Constant if (c.name.toString() == "tpl") => c.df.get match {case OMS(p) => p}
      case _ => throw LocalError("the first declaration should be a constant. ")}
    } catch {
      case e: NoSuchElementException => throw LocalError("The first declaration should be defined as the TypeLevel for which type the inductively-defined function is to be defined. ")
      case e: Exception => throw LocalError("the definien of the first declaration should be the declaration of the TypeLevel for which type the inductively-defined function is to be defined. ")
    }
    val indTpl = controller.library.get(indTplPath) match {case c: Constant => c}
    
    val indTplName = LocalName(indTplPath.last)
    val indDerDeclPath = indTplPath.copy(name = LocalName(indTplPath.name.head))
    println("intDerDeclPath: "+indDerDeclPath)
    val inductiveTypeDecl = controller.library.getO(indDerDeclPath) getOrElse(
        throw LocalError("No corresponding inductively-defined type found at "+indDerDeclPath.toString()))
    val inductiveType = inductiveTypeDecl match {
        case dd : DerivedDeclaration if (dd.feature == "inductive") => dd
        case _ => throw LocalError("Expected corresponding inductively-defined type at "+indDerDeclPath.toString()
            +" but no derived declaration of deature inductive found.")
    }
    def correspondingDecl(d: LocalName): Constant = {
      inductiveType.getO(d).getOrElse(
        throw LocalError("No corresponding declaration found for "+d)) match {
        case c: Constant=> c
        case dec => throw LocalError("Expected a constant at "+dec.path+".")
      }
    }
    
    
    var decls:List[InternalDeclaration] = Nil
    
    // read in the declarations and replace all occurances of previously declared symbols by their definitions
    dd.getDeclarations.tail foreach {
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
    decls foreach { d => checkDecl(d, fromConstant(correspondingDecl(d.name),controller, None)) }
    
    val indParams = Type.getParameters(inductiveType)
    val induct_path = indDerDeclPath / LocalName("induct_"+indTplName)
    
    val modelDf = decls map (_.df.get)
    val indTplArgs = fromConstant(controller.library.get(indTplPath) match {case c: Constant => c}, controller, Some(context)).argContext(None)._1
    
    val indTplDef = decls.find(_.name == indTplName).getOrElse(
        throw LocalError("No declaration found for the typelevel of the type of the inductively-defined function.")).df.get
    val Tp = PiOrEmpty(context, Arrow(indTpl.toTerm, indTplDef))
    val Df = PiOrEmpty(context, PiOrEmpty(indTplArgs, ApplyGeneral(OMS(induct_path), indParams.map(_.toTerm)++modelDf++indTplArgs.map(_.toTerm))))
    
    val inductDef = makeConst(dd.name, ()=> {Tp}, () => {Some(Df)})(indDerDeclPath)
    log(defaultPresenter(inductDef)(controller))
    
    new Elaboration {
      def domain = List(inductDef.name)
      def getO(n: LocalName) = if (n == inductDef.name) Some(inductDef) else None
    }
  }
  
  /**
   * checks whether d.tp matches the type decl.externalTp
   * @param d the declaration whoose type to check
   * @param decl the corresponding declaration which is to be defined by d
   * @note this will return an error if d.tp doesn't match decl.externalTp
   */
  def checkDecl(d: InternalDeclaration, decl: InternalDeclaration) {}//TODO: This should be implemented to provide more accurate error messages
}

object InductiveDefinitionRule extends StructuralFeatureRule(classOf[InductiveDefinitions], "inductive_definition")
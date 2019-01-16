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
class RecordDefinitions extends StructuralFeature("record_term") with ParametricTheoryLike {
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
  
  object DefType {
    val mpath = SemanticObject.javaToMMT(getClass.getCanonicalName)

    def apply(p: GlobalName, params: Context, args: List[Term]) = OMBINDC(OMMOD(mpath), params, List(OMID(p)))
    def unapply(t: Term) = t match {
      case OMBINDC(OMMOD(this.mpath), params, List(OMID(p))) => Some(p, params, Nil)
      case _ => None
    }

    /** retrieves the parameters */
    def getParameters(dd: DerivedDeclaration) = {
      dd.tpC.get.getOrElse(throw LocalError("missing type of derived declaration")) match {
        case DefType(p, pars, args) => (p, pars, args)
      }
    }
  }

  override def getHeaderNotation = List(LabelArg(2, LabelInfo.none), Delim("("), Var(1, true, Some(Delim(","))), Delim(")"), Delim(":"),SimpArg(3))
      //SimpArg(3),Delim("("), SimpSeqArg(4, Delim(","), CommonMarkerProperties.noProps), Delim(")"))
  override def processHeader(header: Term) = header match {
    case OMA(OMMOD(`mpath`), List(OML(name,_,_,_,_),t)) => 
      val (p, args) = getInd(t)
      (name, DefType(p, Context.empty, args))
    case OMBINDC(OMMOD(`mpath`), cont, List(OML(name,_, _,_,_), t)) => 
      val (p, args) = getInd(t)
      (name, DefType(p, cont, args))
    case hdr => throw InvalidObject(header, "ill-formed header: "+hdr.toNode)
  }
  override def makeHeader(dd: DerivedDeclaration) = getParams(dd) match {
    case (p, cont, args) => DefType(p, cont, args)
  }
  def getInd(t: Term) : (GlobalName, List[Term]) = t match {
    case OMA(OMID(p), args) => (p.toMPath.copy(name=p.name.init) ? p.name.last, args)
    case OMID(p) => (p.toMPath.copy(name=p.name.init) ? p.name.last, Nil)
  }
  def getParams(dd: DerivedDeclaration): (GlobalName, Context, List[Term]) = {
    dd.tpC.get match {
      case Some(OMBINDC(_, params, args)) => val (p, ags) = getInd(args.last); (p, params, ags)
    }
  }
  
    
  /**
   * Elaborates an declaration of one or multiple mutual inductive types into their declaration, 
   * as well as the corresponding no confusion and no junk axioms
   * Constructs a structure whose models are exactly the (not necessarily initial) models of the declared inductive types
   * @param parent The parent module of the declared inductive types
   * @param dd the derived declaration to be elaborated
   */
  def elaborate(parent: Module, dd: DerivedDeclaration) = {
    implicit val parentTerm = dd.path
    val (indDefPath, context, indParams) = DefType.getParameters(dd)
    val (recD, indCtx) = controller.library.get(indDefPath) match {
      case indD: DerivedDeclaration if (indD.feature == "inductive") => (indD, Type.getParameters(indD))
      case d: DerivedDeclaration => throw LocalError("the referenced derived declaration is not of the feature inductive but of the feature "+d.feature+".")
      case _ => throw LocalError("Expected corresponding inductively-defined types at "+indDefPath.toString()
            +" but no derived declaration of feature inductive found at that location.")
    }
    val recDefs = controller.library.get(indDefPath).getDeclarations map {case c:Constant => fromConstant(c, controller, Some(indCtx))}
    
    def correspondingDecl(d: LocalName): Constant = {
      recD.getO(d).getOrElse(
        throw LocalError("No corresponding declaration found for "+d)) match {
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
    decls foreach { d => checkDecl(d, fromConstant(correspondingDecl(d.name),controller, None)) }
    // and whether we have all necessary declarations
    recDefs.map(_.name).find(n => !decls.map(_.name).contains(n)) foreach {n => throw LocalError("No declaration found for the internal declaration "+n+" of "+recD.name+".")}
    
    val Ltp = () => {
      val recType = recDefs.find(_.name.toString() == "type").getOrElse(throw LocalError("No type declaration found for the record "+recD.name))
      PiOrEmpty(context, ApplyGeneral(recType.applyTo(indParams), decls.filter({case d:TypeLevel => true case _ => false}).map(d => d.df.get)))
    }
    val Ldf = () => {
      val makeRec = recDefs.find(_.name.toString() == "make").getOrElse(throw LocalError("No introduction declaration found for the record "+recD.name))
      Some(LambdaOrEmpty(context, ApplyGeneral(makeRec.applyTo(indParams), decls.map(_.df.get))))
    }
    
    val make = makeConst(LocalName("make"), Ltp, Ldf)
    log(defaultPresenter(make)(controller))
    
    new Elaboration {
      def domain = List(make.name)
      def getO(n: LocalName) = List(make) find (_.name == n)
    }
  }
  
  /**
   * checks whether d.tp matches the type decl.externalTp
   * @param d the declaration whoose type to check
   * @param decl the corresponding declaration which is to be defined by d
   * @note this will return an error if d.tp doesn't match decl.externalTp
   */
  def checkDecl(d: InternalDeclaration, decl: InternalDeclaration) {//TODO: This should be implemented to provide more accurate error messages
    
  }
}

object RecordDefinitionRule extends StructuralFeatureRule(classOf[RecordDefinitions], "record_term")
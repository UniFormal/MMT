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
class Records extends StructuralFeature("record") with ParametricTheoryLike {

  /**
   * Checks the validity of the record to be constructed
   * @param dd the derived declaration from which the record is to be constructed
   */
  override def check(dd: DerivedDeclaration)(implicit env: ExtendedCheckingEnvironment) {
    //TODO: check for inhabitability
  }
  
  /**
   * Elaborates the declaration of a record into the derived declarations, 
   * as well as the corresponding no confusion and no junk axioms
   * Constructs a structure whose models are exactly the (not necessarily initial) models of the declared inductive types
   * @param parent The parent module of the declared inductive types
   * @param dd the derived declaration to be elaborated
   */
  def elaborate(parent: DeclaredModule, dd: DerivedDeclaration) = {
    val context = Type.getParameters(dd) 
    implicit val parentTerm = dd.path
    // to hold the result
    var elabDecls : List[Constant] = Nil
    implicit var tmdecls : List[TermLevel]= Nil
    implicit var statdecls : List[StatementLevel]= Nil
    implicit var tpdecls : List[TypeLevel] = Nil
    
    val structure : TypeLevel = structureDeclaration(None)
    elabDecls :+= structure.toConstant
    
    val origDecls : List[InternalDeclaration] = dd.getDeclarations map {
      case c: Constant => fromConstant(c, controller, Some(context))
      case _ => throw LocalError("unsupported declaration")
    }
    val decls : List[InternalDeclaration] = toEliminationDecls(origDecls, structure)
    
    decls foreach {
       case d @ TermLevel(_, _, _, _, _,_) => tmdecls :+= d
       case d @ TypeLevel(_, _, _, _,_) => tpdecls :+= d
       case d @ StatementLevel(_, _, _,_, _) => statdecls :+= d
    }
    
    val make : TermLevel = introductionDeclaration(structure, origDecls, None)
    // copy all the declarations
    decls foreach {d => elabDecls ::= d.toConstant}
    
    // the no junk axioms
    elabDecls = elabDecls.reverse ++ noJunksEliminationDeclarations(decls, context, make, origDecls)
    
    elabDecls :+= reprDeclaration(structure, decls)
    
    elabDecls foreach {d =>
      log(controller.presenter.asString(d))
    }
    new Elaboration {
      val elabs : List[Declaration] = Nil 
      def domain = elabDecls map {d => d.name}
      def getO(n: LocalName) = {
        elabDecls.find(_.name == n)
      }
    }
  }
  def reprDeclaration(structure:TypeLevel, decls:List[InternalDeclaration])(implicit parent: GlobalName) : Constant = {
    val arg = structure.makeVar("m", Context.empty)
    val ret : Term = Eq(structure.applyTo(decls.map(_.applyTo(arg.toTerm))), arg.toTerm)
    makeConst(uniqueLN("repr"), PiOrEmpty(List(arg), ret))
  }
  
  def toEliminationDecls(decls: List[InternalDeclaration], structure: InternalDeclaration)(implicit parent : GlobalName) : List[InternalDeclaration] = {
    var types : List[TypeLevel] = Nil
    decls map {
      case tpl @ TypeLevel(_, _, _, _, _) => val tplelim = tpl.toEliminationDecl(structure, types map (_.path)); types :+= (tplelim match {case t @ TypeLevel(_,_,_,_,_) => t}); tplelim
      case d => d.toEliminationDecl(structure, types map (_.path))
    }
  }
    
  /**
   * Generate no junk declaration for all the elimination form internal declarations
   * @param parent the parent declared module of the derived declaration to elaborate
   * @param decls all the elimination form declarations, used to construct the chain
   * @param tmdecls all term level declarations
   * @param tpdecls all type level declarations
   * @param context the inner context of the derived declaration
   * @param introductionDecl the introduction declaration of the structure
   * @param 
   * @returns returns one no junk (morphism) declaration for each type level declaration
   * then generates all the corresponding no junk declarations for the termlevel constructors of each declared type
   */    
  def noJunksEliminationDeclarations(decls : List[InternalDeclaration], context: Context, introductionDecl: InternalDeclaration, origDecls: List[InternalDeclaration])(implicit parent : GlobalName) : List[Constant] = {
    val (repls, modelCtx) = chain(origDecls, context)
    def makeApplNm = uniqueLN(introductionDecl.name+modelCtx.foldLeft("")((a, b)=>a +" "+b.name))
    def makeApplied = (introductionDecl.ret, introductionDecl, {(arg:VarDecl, chain:Context) => OMV(makeApplNm) % introductionDecl.applyTo(chain)})
    
    def makeAppl = makeApplied match {case (x, y, map) => (x, {t:VarDecl => map (t, modelCtx)})}
    
    def mapTerm(tm: VarDecl) : VarDecl = if (makeAppl._1 == tm.tp.get) makeAppl._2(tm) else tm
    
    origDecls zip origDecls.map(_.translate(TraversingTranslator(OMSReplacer(p => utils.listmap(repls, p))))) map {case (e, dDecl) => 
      val decl = e.toEliminationDecl(introductionDecl, decls map (_.path))
      val d = utils.listmap(repls, e.path).get
      
      //the result of applying m to the result of the constructor
      val (args, mappedRes) = dDecl.argContext(None)
    
      //the result of applying the image of the constructor (all types substituted) to the image of the arguments
      val mappedArgs : List[VarDecl] = decl.argContext(None)._1.getDeclarations.head::args map (mapTerm(_))
      //ensure everything went well and the argument is actually in the context
      val mappedConstr : Term = decl.applyTo(mappedArgs)
         
      def assert(x: Term, y: Term) : (Term, LocalName) = e match {
        case TypeLevel(_, _, _, _,_) => (Arrow(x, y), uniqueLN("induct_"+d.name))
        case _ => (Eq(x, y), uniqueLN("compute_"+d.name))
      }
      val ass = assert(mappedConstr, mappedRes)
      makeConst(ass._2, PiOrEmpty(modelCtx ++ context ++ args, ass._1))
    }
  }  
}

object RecordRule extends StructuralFeatureRule(classOf[Records], "record")
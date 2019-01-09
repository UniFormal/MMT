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
  object noLookupPresenter extends presentation.NotationBasedPresenter {
    override def getNotations(p: GlobalName) = if (! (p.doc.uri.path contains "urtheories")) Nil else super.getNotations(p)
    override def getAlias(p: GlobalName) = if (true) Nil else super.getAlias(p)
  }
  
  override def start(args: List[String]) {
    initOther(noLookupPresenter)
  }
  
  def defaultPresenter(c: Constant)(implicit con: Controller): String = c.name + ": " + noLookupPresenter.asString(c.tp.get) + (if (c.df != None) " = "+noLookupPresenter.asString(c.df.get) else "")


  /**
   * Checks the validity of the record to be constructed
   * @param dd the derived declaration from which the record is to be constructed
   */
  override def check(dd: DerivedDeclaration)(implicit env: ExtendedCheckingEnvironment) {}
  
  /**
   * Elaborates the declaration of a record into the derived declarations, 
   * as well as the corresponding no confusion and no junk axioms
   * Constructs a structure whose models are exactly the (not necessarily initial) models of the declared inductive types
   * @param parent The parent module of the declared inductive types
   * @param dd the derived declaration to be elaborated
   */
  def elaborate(parent: Module, dd: DerivedDeclaration) = {
    val params = Type.getParameters(dd)
    val context = if (params.nonEmpty) Some(params) else None
    implicit val parentTerm = dd.path
    // to hold the result
    var elabDecls : List[Constant] = Nil
    
    val structure = structureDeclaration(None, context)
    
    val origDecls : List[InternalDeclaration] = dd.getDeclarations map {
      case c: Constant => fromConstant(c, controller, context)
      case _ => throw LocalError("unsupported declaration")
    }
    val decls : List[Constant] = toEliminationDecls(origDecls, structure.path)
        
    val make : Constant = this.introductionDeclaration(structure.path, origDecls, None, context)
    val strDecl = makeConst(structure.name.tail, () => {PiOrEmpty(origDecls map(_.toVarDecl), structure.tp.get)})
    elabDecls ::= strDecl
    elabDecls ::= make

    // copy all the declarations
    decls foreach (elabDecls ::= _)
    
    // the no junk axioms
    elabDecls = elabDecls.reverse ++ noJunksDeclarations(decls map (_.path), params, structure.path, make.path, origDecls)
    
    elabDecls :+= reprDeclaration(structure.path, make.path, decls map (_.path), Some("repr"), context)
    
    elabDecls foreach {d => log(defaultPresenter(d)(controller))}
    new Elaboration {
      val elabs : List[Declaration] = Nil 
      def domain = elabDecls map {d => d.name}
      def getO(n: LocalName) = {
        elabDecls.find(_.name == n)
      }
    }
  }
  
  def introductionDeclaration(recType: GlobalName, decls: List[InternalDeclaration], nm: Option[String], context: Option[Context])(implicit parent : GlobalName) = {
    val Ltp = () => {
      val declCtx = decls.map(_.toVarDecl)
      PiOrEmpty(context getOrElse Context.empty ++declCtx++ decls.map(d => newVar(LocalName("x_"+d.name), OMV(LocalName(d.name)))), ApplyGeneral(OMS(recType), declCtx.map(_.toTerm)))
    }
    makeConst(uniqueLN(nm getOrElse "make"), Ltp)
  }
  
  def reprDeclaration(recordType: GlobalName, introDecl: GlobalName, recordFields:List[GlobalName], name: Option[String], ctx: Option[Context])(implicit parent: GlobalName) : Constant = {
    val Ltp = () => {
      val con = (ctx getOrElse Context.empty)
      val params = con  map (_.toTerm)
      val recType = if (con.isEmpty) OMS(recordType) else ApplyGeneral(OMS(recordType), params)
      val arg = newVar(uniqueLN(name getOrElse "m"), recType, ctx)
      val resStr = ApplyGeneral(OMS(introDecl), params ++ (recordFields map {f => ApplyGeneral(OMS(f), params :+ arg.toTerm)}))
      val ret : Term = Eq(resStr, arg.toTerm, OMS(recordType))
      Pi(con :+ arg, ret)
    }
    makeConst(uniqueLN("repr"), Ltp)
  }
  
  def toEliminationDecls(decls: List[InternalDeclaration], recType: GlobalName)(implicit parent : GlobalName) : List[Constant] = {
    var fields : List[GlobalName] = Nil
    decls map {
      case d =>
        val Ctx = decls.map(_.toVarDecl)
        val dE = toEliminationDecl(d, recType, Ctx, fields)
        fields ::= dE.path
        dE
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
   * @returns returns one no junk (morphism) declaration for each type level declaration
   * then generates all the corresponding no junk declarations for the termlevel constructors of each declared type
   */    
  def noJunksDeclarations(recordFields : List[GlobalName], context: Context, recType: GlobalName, recMake: GlobalName, origDecls: List[InternalDeclaration])(implicit parent : GlobalName) : List[Constant] = {
    val (repls, modelCtx) = chain(origDecls, context)
    (origDecls zip repls) map {case (d, (p, v)) =>
      val Ltp = () => {
        val dElim = toEliminationDecl(d, recType, origDecls.map(_.toVarDecl), recordFields)
        def assert(x: Term, y: Term): Term = d match {
          case TypeLevel(_, _, _, _,_) => Arrow(x, y)
          case _ => Eq(x, y, d.ret)
        } 
        PiOrEmpty(context++modelCtx, assert(ApplySpine(OMS(dElim.path), ApplyGeneral(OMS(recMake), modelCtx.map(_.toTerm))), v))
      }
      makeConst(uniqueLN("induct_"+d.name), Ltp)
    }
  }
  
  /**
   * for records: for a declaration c:A, this produces the declaration c: {r:R} A[r]
   * where A[r] is like A with every d declared in this record with d r
   */
  def toEliminationDecl(c:InternalDeclaration, recType: GlobalName, Ctx: Context, recordFields: List[GlobalName])(implicit parent: GlobalName): Constant = {
    val Ltp = () => {
      val r = LocalName("r")
      val tr = TraversingTranslator(OMSReplacer(p => if (recordFields contains p) Some(ApplyGeneral(OMS(p), (c.context++Ctx).map(_.toTerm):+OMV(r))) else None))
      PiOrEmpty(c.context++Ctx, Pi(r, OMS(recType), tr(c.context, c.tp)))
    }
    makeConst(c.name, Ltp)
  }
}

object RecordRule extends StructuralFeatureRule(classOf[Records], "record")
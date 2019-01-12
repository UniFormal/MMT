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
    val declCtx = origDecls map(_.toVarDecl)
       
    val makeType = makeConst(LocalName("makeType"), () => {PiOrEmpty(declCtx, structure.tp.get)})//, () => {Some(OMS(structure.path))})
    val decls : List[Constant] = toEliminationDecls(origDecls, declCtx, makeType.path)
    val make : Constant = this.introductionDeclaration(makeType.path, origDecls, None, context)
    
    //elabDecls ::= structure
    elabDecls ::= makeType
    elabDecls ::= make

    // copy all the declarations
    decls foreach (elabDecls ::= _)
    
    // the no junk axioms
    elabDecls = elabDecls.reverse ++ noJunksDeclarations(params, declCtx, makeType.path, make.path, origDecls)
    
    elabDecls :+= reprDeclaration(makeType.path, make.path, declCtx, decls map (_.path), Some("rec"), context)
    elabDecls :+= equalityDecl(makeType.path, make.path, declCtx, origDecls, context)
    
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
      val declsTm = decls.filter(isTypeLevel(_)).map(d=>OMV(LocalName(d.name)))
      PiOrEmpty(context getOrElse Context.empty ++declCtx, Arrow(declsTm, ApplyGeneral(OMS(recType), declCtx.map(_.toTerm))))
    }
    makeConst(uniqueLN(nm getOrElse "make"), Ltp)
  }
  
  def reprDeclaration(recordType: GlobalName, introDecl: GlobalName, declCtx:Context, recordFields:List[GlobalName], name: Option[String], ctx: Option[Context])(implicit parent: GlobalName) : Constant = {
    val Ltp = () => {
      val con = (ctx getOrElse Context.empty)
      val params = con .map(_.toTerm)++declCtx.map(_.toTerm)
      val recType = if (params.isEmpty) OMS(recordType) else ApplyGeneral(OMS(recordType), params)
      val arg = newVar(uniqueLN(name getOrElse "m"), recType, ctx)
      val resStr = ApplyGeneral(OMS(introDecl), params ++ (recordFields map {f => ApplyGeneral(OMS(f), params :+ arg.toTerm)}))
      val ret : Term = Eq(recType, arg.toTerm, resStr)
      Pi(con++declCtx :+ arg, ret)
    }
    makeConst(uniqueLN("repr"), Ltp)
  }
  
  def equalityDecl(makeType: GlobalName, make: GlobalName, declCtx:Context, decls:List[InternalDeclaration], ctx: Option[Context])(implicit parent: GlobalName) : Constant = {
    val Ltp = () => {
      val con = (ctx getOrElse Context.empty)
      val params = con.map(_.toTerm) ++ declCtx.map(_.toTerm)
      val recType = if (params.isEmpty) OMS(makeType) else ApplyGeneral(OMS(makeType), params)
      val args = decls.filter(isTypeLevel(_)).map(d=>newVar(uniqueLN("x_"+d.name), d.toVarDecl.toTerm, ctx))
      val argsP = decls.filter(isTypeLevel(_)).map(d=>newVar(uniqueLN("y_"+d.name), d.toVarDecl.toTerm, ctx))
      val res = ApplyGeneral(OMS(make), params ++ args.map(_.toTerm))
      val resP = ApplyGeneral(OMS(make), params ++ argsP.map(_.toTerm))
      val ret : Term = Arrow(args.zip(argsP) map {case (a, b) => Eq(a.tp.get, a.toTerm, b.toTerm)}, Eq(recType, res, resP))
      Pi(con++declCtx ++ args ++ argsP, ret)
    }
    makeConst(uniqueLN("equiv"), Ltp)
  }
  
  def toEliminationDecls(decls: List[InternalDeclaration], declCtx: Context, recMakeType: GlobalName)(implicit parent : GlobalName) : List[Constant] = {
    var fields : List[GlobalName] = Nil
    (decls zip declCtx).filter(_._1 match {case _: TypeLevel => true case _ => false}) map {
      case (decl, d) =>
        val Ltp = () => {
          val params = decl.context.map(_.toTerm)++declCtx.map(_.toTerm)
          val r = LocalName("r")
          val rec = if (params.isEmpty) OMV(r) % OMS(recMakeType) else OMV(r) % ApplyGeneral(OMS(recMakeType), params)
          PiOrEmpty(decl.context++declCtx++rec, d.toTerm)
        }
        makeConst(decl.name, Ltp)
    }
  }
    
  
  /*/**
   * for records: for a declaration c:A, this produces the declaration c: {r:R} A[r]
   * where A[r] is like A with every d declared in this record with d r
   */
  def toEliminationDecl(c:InternalDeclaration, recMakeType: GlobalName, declCtx: Context, recordFields: List[GlobalName])(implicit parent: GlobalName): Constant = {
    val Ltp = () => {
      val r = LocalName("r")
      val rec = if (c.context.isEmpty) OMV(r) % OMS(recMakeType) else OMV(r) % ApplyGeneral(OMS(recMakeType), c.context++declCtx map (_.toTerm))
      val tr = TraversingTranslator(OMSReplacer(p => if (recordFields contains p) Some(ApplyGeneral(OMS(p), (c.context).map(_.toTerm):+OMV(r))) else None))
      PiOrEmpty(c.context++declCtx, Pi(rec, tr(c.context, c.applyTo(rec.toTerm))))
    }
    makeConst(c.name, Ltp)
  }*/
  
  /**
   * Generate no junk declaration for all the elimination form internal declarations
   * @param parent the parent declared module of the derived declaration to elaborate
   * @param recordFields the paths of the internal declarations
   * @param tmdecls all term level declarations
   * @param tpdecls all type level declarations
   * @param context the inner context of the derived declaration
   * @param introductionDecl the introduction declaration of the structure
   * @returns returns one no junk (morphism) declaration for each type level declaration
   * then generates all the corresponding no junk declarations for the termlevel constructors of each declared type
   */    
  def noJunksDeclarations(context: Context, declCtx: Context, recMakeType: GlobalName, recMake: GlobalName, origDecls: List[InternalDeclaration])(implicit parent : GlobalName) : List[Constant] = {
    //val (_, declCtx) = chain(origDecls, context)
    //(origDecls zip repls) map {case (d, (p, v)) =>
    origDecls zip(declCtx) map {case (decl, d) =>
      val Ltp = () => {
        val dElim = OMS(externalName(parent, decl.name))
        val args = (origDecls.zip(declCtx)).filter(x => isTypeLevel(x._1)) map {case (_, d) => newVar(uniqueLN("x_"+d.name), d.toTerm, Some(context++declCtx))}
        val params = (context++declCtx) map (_.toTerm)
        val x_d = if (isTypeLevel(decl)) {val vd=newVar(uniqueLN("x_"+d.name), d.toTerm, Some(context++declCtx)); (vd.tp.get, vd.toTerm)} else (decl.tp, decl.toTerm)
        
        PiOrEmpty(context++declCtx++args, Eq(x_d._1, ApplyGeneral(dElim, params:+ApplyGeneral(OMS(recMake), params++args.map(_.toTerm))), x_d._2))
      }
      makeConst(uniqueLN("induct_"+decl.name), Ltp)
    }
  }
}

object RecordRule extends StructuralFeatureRule(classOf[Records], "record")
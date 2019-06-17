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
import StructuralFeatureUtils._
import InternalDeclarationUtil._

object RecordUtil {
    val recHome = DPath(utils.URI("http", "cds.omdoc.org") / "LFX") ? "Records"
    val recTypePath = recHome ? "Rectype"
    val recExpPath = recHome ? "Recexp"
}
import RecordUtil._

/** theories as a set of types of expressions */ 
class Records extends StructuralFeature("record") with ParametricTheoryLike {

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
  def elaborate(parent: ModuleOrLink, dd: DerivedDeclaration) = {
    val params = Type.getParameters(dd)
    implicit val parentTerm = dd.path

    if (declaresRecords(parent)) {elaborateToRecord(params)} else {
      val context = if (params.nonEmpty) Some(params) else None
      // to hold the result
      var elabDecls : List[Constant] = Nil
      
      val structure = structureDeclaration(Some("typeDecl"), context)
      
      val origDecls = parseInternalDeclarations(dd, controller, context)
      val declCtx = origDecls map(d => OMV(LocalName(d.name)) % d.internalTp)
      val TpDeclCtx = origDecls filter (_.isTypeLevel) map (_.toVarDecl)
         
      val recordType = makeConst(LocalName("type"), () => {PiOrEmpty(TpDeclCtx, structure.tp.get)})
      val decls : List[Constant] = toEliminationDecls(origDecls, declCtx, TpDeclCtx, recordType.path)
      val make : Constant = this.introductionDeclaration(recordType.toTerm, origDecls, None, context)
      
      //elabDecls ::= structure
      elabDecls ::= recordType
      elabDecls ::= make
  
      // copy all the declarations
      decls foreach (elabDecls ::= _)
      
      // the no junk axioms
      elabDecls = elabDecls.reverse ++ noJunksDeclarations(params, declCtx, TpDeclCtx, recordType.toTerm, make.path, origDecls)
      
      elabDecls :+= reprDeclaration(recordType.path, make.path, declCtx, TpDeclCtx, decls map (_.path), Some("rec"), context)
      elabDecls :+= equalityDecl(recordType.path, make.path, declCtx, origDecls, context)
      elabDecls ++= convEqualityDecls(recordType.path, make.path, declCtx, origDecls, context)
      
      //elabDecls foreach {d => log(defaultPresenter(d)(controller))}
      new Elaboration {
        val elabs : List[Declaration] = Nil
        def domain = elabDecls map {d => d.name}
        def getO(n: LocalName) = {
          elabDecls.find(_.name == n).foreach(d => log(defaultPresenter(d)(controller)))
          elabDecls.find(_.name == n)
        }
      }
    }
  }
  
  def introductionDeclaration(recType: Term, decls: List[InternalDeclaration], nm: Option[String], context: Option[Context])(implicit parent : GlobalName) = {
    val Ltp = () => {
      val declsCtx = decls.map(d => OMV(LocalName(d.name)) % d.internalTp)
      val declsTp = decls.filter(_.isTypeLevel).map(d => OMV(LocalName(d.name)) % d.internalTp)
      val params = context.getOrElse(Context.empty)++declsTp
      val declsTm = decls.filterNot(_.isTypeLevel).map(d => d.internalTp)
      val TpTmDecls = decls.filter(_.isTypeLevel).map(d => OMV(LocalName("x_"+d.name)) % OMV(d.name))
      PiOrEmpty(params++TpTmDecls, Arrow(declsTm, ApplyGeneral(recType, params.map(_.toTerm))))
    }
    makeConst(uniqueLN(nm getOrElse "make"), Ltp)
  }
  
  def reprDeclaration(recordType: GlobalName, introDecl: GlobalName, declCtx:Context, TpDeclCtx: Context, recordFields:List[GlobalName], name: Option[String], ctx: Option[Context])(implicit parent: GlobalName) : Constant = {
    val Ltp = () => {
      val con = (ctx getOrElse Context.empty)
      val params = con .map(_.toTerm)++TpDeclCtx.map(_.toTerm)
      val recType = if (params.isEmpty) OMS(recordType) else ApplyGeneral(OMS(recordType), params)
      val arg = newVar(name getOrElse "m", recType, ctx)
      val resStr = ApplyGeneral(OMS(introDecl), (con++declCtx).map(_.toTerm) ++ (recordFields map {f => ApplyGeneral(OMS(f), params :+ arg.toTerm)}))
      val ret : Term = Eq(recType, arg.toTerm, resStr)
      Pi(con++declCtx :+ arg, ret)
    }
    makeConst(uniqueLN("repr"), Ltp)
  }
  
  /* Design issue: 
  	Should the declaration also consider parameters?
  	If not the declarations will be useless for Florian's intended usage.
  	Otherwise, we would break with the idea that theory parameters have no other effect than being copied into the context of each external declaration
  */
  def equalityDecl(makeType: GlobalName, make: GlobalName, declCtx:Context, decls:List[InternalDeclaration], ctx: Option[Context])(implicit parent: GlobalName) : Constant = {
    val Ltp = () => {
      val con = (ctx getOrElse Context.empty)
      val cont = con ++ (decls zip declCtx).filter(_._1.isTypeLevel).map(_._2)
      val params = cont.map(_.toTerm)
      val recType = if (params.isEmpty) OMS(makeType) else ApplyGeneral(OMS(makeType), params)
      val args = decls.map(d=>newVar("x_"+d.name, if(d.isTypeLevel) d.toVarDecl.toTerm else d.internalTp, ctx))
      val argsP = decls.map(d=>newVar("y_"+d.name, if(d.isTypeLevel) d.toVarDecl.toTerm else d.internalTp, ctx))
      val res = ApplyGeneral(OMS(make), params ++ args.map(_.toTerm))
      val resP = ApplyGeneral(OMS(make), params ++ argsP.map(_.toTerm))
      val ret : Term = Arrow(args.zip(argsP) map {case (a, b) => Eq(a.tp.get, a.toTerm, b.toTerm)}, Eq(recType, res, resP))
      PiOrEmpty(cont ++ args ++ argsP, ret)
    }
    makeConst(uniqueLN("equiv"), Ltp)
  }
  
  def convEqualityDecls(makeType: GlobalName, make: GlobalName, declCtx:Context, decls:List[InternalDeclaration], ctx: Option[Context])(implicit parent: GlobalName) : List[Constant] = {
    decls.zipWithIndex map { case (_, i) =>
      val Ltp = () => {
        val con = (ctx getOrElse Context.empty)
        val cont = con ++ (decls zip declCtx).filter(_._1.isTypeLevel).map(_._2)
        val params = cont.map(_.toTerm)
        val recType = if (params.isEmpty) OMS(makeType) else ApplyGeneral(OMS(makeType), params)
        val args = decls.map(d=>newVar("x_"+d.name, if(d.isTypeLevel) d.toVarDecl.toTerm else d.internalTp, ctx))
        val argsP = decls.map(d=>newVar("y_"+d.name, if(d.isTypeLevel) d.toVarDecl.toTerm else d.internalTp, ctx))
        val res = ApplyGeneral(OMS(make), params ++ args.map(_.toTerm))
        val resP = ApplyGeneral(OMS(make), params ++ argsP.map(_.toTerm))
        val argsEq = args.zip(argsP) map {case (a, b) => Eq(a.tp.get, a.toTerm, b.toTerm)}
        val ret : Term = Arrow(Eq(recType, res, resP), argsEq.toArray.apply(i))
        Pi(cont ++ args ++ argsP, ret)
      }
      makeConst(uniqueLN("conv_equiv_"+i), Ltp)
    }
  }
  
  def toEliminationDecls(decls: List[InternalDeclaration], declCtx: Context, TpDeclCtx: Context, recMakeType: GlobalName)(implicit parent : GlobalName) : List[Constant] = {
    var fields : List[GlobalName] = Nil
    (decls zip declCtx) map {case (decl, d) =>
      decl match {
        case _ : TypeLevel =>
          val Ltp = () => {
            val params = decl.context.map(_.toTerm)++TpDeclCtx.map(_.toTerm)
            val r = LocalName("r")
            val rec = if (params.isEmpty) OMV(r) % OMS(recMakeType) else OMV(r) % ApplyGeneral(OMS(recMakeType), params)
            PiOrEmpty(decl.context++TpDeclCtx++rec, Univ(1))
          }
          makeConst(decl.name, Ltp)
          case _ => 
            val Ltp = () => {
            val params = decl.context.map(_.toTerm)++TpDeclCtx.map(_.toTerm)
            val r = LocalName("r")
            val rec = if (params.isEmpty) OMV(r) % OMS(recMakeType) else OMV(r) % ApplyGeneral(OMS(recMakeType), params)
            PiOrEmpty(decl.context++TpDeclCtx++rec, decl.internalTp)
          }
          makeConst(decl.name, Ltp)
      }
    }
  }

  
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
  def noJunksDeclarations(context: Context, declCtx: Context, TpDeclCtx: Context, recType: Term, recMake: GlobalName, origDecls: List[InternalDeclaration])(implicit parent : GlobalName) : List[Constant] = {
    //val (_, declCtx) = chain(origDecls, context)
    //(origDecls zip repls) map {case (d, (p, v)) =>
    
    origDecls zip(declCtx) map {case (decl, d) =>
      val Ltp = () => {
        val dElim = OMS(externalName(parent, decl.name))
        val args = (origDecls.zip(declCtx)).filter(x => x._1.isTypeLevel) map {case (_, d) => newVar("x_"+d.name, d.toTerm, Some(context++declCtx))}
        val params = (context++TpDeclCtx) map (_.toTerm)
        val x_d = if (isTypeLevel(decl)) {val vd=newVar("x_"+d.name, d.toTerm, Some(context++declCtx)); (vd.tp.get, vd.toTerm)} else (decl.tp, d.toTerm)
        
        PiOrEmpty(context++declCtx++args, Eq(x_d._1, ApplyGeneral(dElim, params:+ApplyGeneral(OMS(recMake), params++args.map(_.toTerm))), x_d._2))
      }
      makeConst(uniqueLN("induct_"+decl.name), Ltp)
    }
  }
  
  def declaresRecords(home: ModuleOrLink) = {
    val knownDecls = home.getDeclarations.map(_.path)
    knownDecls.contains(recExpPath)
  }
  
  def elaborateToRecord(ctx: Context)(implicit parent: GlobalName) = {
    val recordFields = ctx.filter(d=>isTypeLevel(d.tp.get)).map(_.toOML)
    Elaboration(List((makeConst(parent.name, () => {OMA(OMS(recTypePath), recordFields)}))))
  }
}

object RecordRule extends StructuralFeatureRule(classOf[Records], "record")
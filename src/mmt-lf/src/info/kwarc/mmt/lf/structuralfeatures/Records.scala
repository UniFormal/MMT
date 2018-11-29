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
  override def check(dd: DerivedDeclaration)(implicit env: ExtendedCheckingEnvironment) {}
  
  /**
   * Elaborates the declaration of a record into the derived declarations, 
   * as well as the corresponding no confusion and no junk axioms
   * Constructs a structure whose models are exactly the (not necessarily initial) models of the declared inductive types
   * @param parent The parent module of the declared inductive types
   * @param dd the derived declaration to be elaborated
   */
  def elaborate(parent: DeclaredModule, dd: DerivedDeclaration) = {
    val params = Type.getParameters(dd)
    val context = if (params.nonEmpty) Some(params) else None
    implicit val parentTerm = dd.path
    // to hold the result
    var elabDecls : List[Constant] = Nil
    
    val structure = structureDeclaration(None, context)
    elabDecls :+= structure
    
    val origDecls : List[InternalDeclaration] = dd.getDeclarations map {
      case c: Constant => fromConstant(c, controller, context)
      case _ => throw LocalError("unsupported declaration")
    }
    val decls : List[Constant] = toEliminationDecls(origDecls, structure.path)
        
    val make : Constant = this.introductionDeclaration(structure.path, origDecls, None, context)
    elabDecls :+= make
    // copy all the declarations
    decls foreach (elabDecls ::= _)
    
    // the no junk axioms
    elabDecls = elabDecls.reverse ++ noJunksDeclarations(decls map (_.path), params, structure.path, make.path, origDecls)
    
    elabDecls :+= reprDeclaration(structure.path, make.path, decls map (_.path), Some("repr"), context)
    
    //elabDecls foreach {d => log(defaultPresenter(d)(controller))}
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
      PiOrEmpty(context getOrElse Context.empty ++ decls.map(_.toVarDecl), OMS(recType))
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
        val dE = toEliminationDecl(d, recType, fields)
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
        val dElim = toEliminationDecl(d, recType, recordFields)
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
  def toEliminationDecl(c:InternalDeclaration, recType: GlobalName, recordFields: List[GlobalName])(implicit parent: GlobalName): Constant = {
    val Ltp = () => {
      val r = LocalName("r")
      val tr = TraversingTranslator(OMSReplacer(p => if (recordFields contains p) Some(Apply(OMS(p), OMV(r))) else None))
      Pi(r, OMS(recType), tr(c.context, c.externalTp(Nil)))
    }
    makeConst(c.name, Ltp)
  }
}

object RecordRule extends StructuralFeatureRule(classOf[Records], "record")
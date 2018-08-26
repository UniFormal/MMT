package info.kwarc.mmt.lf.structuralfeatures

import info.kwarc.mmt.api._
import objects._
import symbols._
import notations._
import checking._
import modules._
import frontend.Controller

import info.kwarc.mmt.lf._

/** utility functions for elaborating structural features from typed internal declarations */
private object InternalDeclarationUtil {
  /**
   * Make a new unique local name from the given string
   * @param nm the string from which the local name is generated
   */
  def uniqueLN(nm: String): LocalName = {
    LocalName(nm)
  }
  
  def uniqueGN(nm: String)(implicit parent: GlobalName): GlobalName = parent.module ? uniqueLN(nm)

  /**
    * Make a new variable declaration for a variable of type tp and fresh name (preferably name) in the given (optional) context
    * @param name the local name that will preferably be picked for the variable declaration
    * @param tp the type of the variable
    * @param the context in which to pick the a fresh variable (optional)
    * @note postcondition: the returned variable is free in the given context
    */
   def newVar(name: LocalName, tp: Term, con: Option[Context] = None) : VarDecl = {
     val c = con.getOrElse(Context.empty)
     val (freshName,_) = Context.pickFresh(c, uniqueLN(name.toString()))
     VarDecl(freshName, tp)
   }
  
   //much nicer to read output than the one by controller.presenter.asString
  /*def present(c: Constant)(implicit parent: OMID) : String = {
    c.path.name + ": " + present(c.tp.get)
  }
  def present(e: Term)(implicit parent: OMID) : String = {
    def flatStrList(l : List[String], sep : String) = l match {
      case Nil => ""
      case hd::tl => hd+tl.foldLeft("")((a, b) => a + sep + b)
    }
    def preCon(ctx: Context) = {
      val args = (ctx map (x => present(Constant(parent, x.name, Nil, x.tp, x.df, None)).replaceAll("\n", " ")))
      if (args == Nil) "" else " {"+flatStrList(args, ", ")+"}"
    }
    def iterPre(body: Term) : String = {
      body match {
        case OMBIND(Pi.term, con, body) => preCon(con) + " "+iterPre(body)
        case ApplyGeneral(f:Term, args @ hd::tl) => iterPre(f)+ " " + flatStrList(args map iterPre, " ")
        case Arrow(a, b) => "("+iterPre(a) + "-> " + iterPre(b)+")"
        case OMS(target) => "OMS("+target.name.toString()+")"
        case ApplySpine(t : Term, List(arg1: Term, arg2: Term)) => "("+iterPre(t) + " " + iterPre(arg1) + " " + iterPre(arg2)+")" // + iterPre(arg1) + " " 
        case OMV(n) => "OMV("+n.toString()+")"
      }
    }
    iterPre(e)
  }  */ 

  val theory: MPath = LF._base ? "Inductive"
  object Eq extends BinaryLFConstantScala(theory, "eq")
  object Neq extends BinaryLFConstantScala(theory, "neq")
  
  val Contra = OMS(theory ? "contra")
  def neg(tp: Term) : Term = Arrow(tp, Contra)
  
  def makeConst(name: LocalName, tp: Term, df:Option[Term], notC: Option[NotationContainer])(implicit parent: GlobalName): Constant = {
    val externalName = parent.name / name
    Constant(OMMOD(parent.module), externalName, Nil, Some(tp), df, None, notC getOrElse NotationContainer())
  }
  def makeConst(name: LocalName, tp: Term, df:Option[Term])(implicit parent: GlobalName): Constant = makeConst(name, tp, df, None)
  def makeConst(name: LocalName, tp: Term)(implicit parent:  GlobalName): Constant = makeConst(name, tp, None)
  
}

import InternalDeclarationUtil._
import info.kwarc.mmt.api.utils.URI
import scala.util.Random
import java.util.Random
import scala.util.Random

object InternalDeclaration {
  implicit def tp(d: InternalDeclaration)(implicit parent: GlobalName): Term = d.tp
  implicit def tm(d: InternalDeclaration): Option[Term] = d.df
  implicit def toVarDecl(d: InternalDeclaration)(implicit parent: GlobalName): VarDecl = d.toVarDecl

  //TODO: Implement this properly
  def isTypeLevel(tp: Term) : Boolean = false
  
  /**
   * convert the given constant into the appropriate internal declaration
   * @param c the constant to convert
   * @param con the controller
   */
  def fromConstant(c: Constant, con: Controller, ctx: Option[Context])(implicit parent : GlobalName) : InternalDeclaration = {
    val tp = c.tp getOrElse {throw InvalidElement(c, "missing type")}
    /*def parsePi(tp: Term) : (Context, Term) = tp match {
      case Pi(name, hd, tl) => val (ctx, tm) = parsePi(tl); (OMV(name) % hd ::ctx, tm)
      case tm => (Context.empty, tm)
    }*/
    val (retCtx, retTp) = (Context.empty, tp)//parsePi(tp)
    val FunType(args, ret) = retTp
    val context = Some(ctx getOrElse Context.empty ++ retCtx)
    val p = parent.module ? c.path.last
    if (JudgmentTypes.isJudgment(ret)(con.globalLookup)) {
      StatementLevel(p, args, c.df, Some(c.notC), context)
    } else {
      ret match {
        case Univ(1) => TypeLevel(p, args, c.df, Some(c.notC), context)
        case Univ(x) if x != 1 => throw ImplementationError("unsupported universe")
        case r => if (isTypeLevel(r)) TypeLevel(p, args, c.df, Some(c.notC), context) else TermLevel(p, args, ret, c.df, Some(c.notC), context)}
    }
  }
  
  /**
   * convert the given VarDecl into the appropriate internal declaration
   * @param vd the vd to convert
   * @param con the controller
   */
  def fromVarDecl(vd: VarDecl, ctx: Option[Context], con: Controller, context: Context)(implicit parent: GlobalName) : InternalDeclaration = {
    fromConstant(vd.toConstant(parent.module, ctx getOrElse Context.empty), con, Some(context))
  }
  
  private def chain(decls: List[InternalDeclaration], context: Context) : (List[(GlobalName, OMV)], List[VarDecl]) = {
    var repls: List[(GlobalName,OMV)] = Nil
    val tr = TraversingTranslator(OMSReplacer(p => utils.listmap(repls, p)))
    val modelContext = decls map {d =>
      val namePrimed = d.name / "'"
      val vdPrimed = VarDecl(namePrimed, tr(context, d.tp))
      repls ::= d.path -> OMV(namePrimed)
      vdPrimed
    }
    (repls, modelContext)
  }
  
   /**
   * Generate no junk declarations for an inductive type declaration I
   * @param decls all declarations in I
   * @param parent the URI of I
   * @param context the parameters of I
   * 
   * for type level declarations c: {G} type
   *   induct_c: {M,G} c g -> c_M (g map induct)
   * for term level declaration c: {G} A
   *   induct_c: {M,G} induct(c g) = c_M (g map induct)
   * where
   *  * M declares one (primed) variable for every declaration in I
   *  * c_M is the variable in M corresponding to c
   *  * g is the list of varibles declared in G
   *  * induct(t) is the inductive translation of t, defined in terms of the induct function corresponding to the type of t,
   *    e.g., induct_a(m,t) if t has type a for some a: type in I
   *       or induct_a(m,h,t) if t has type (a h) for some a: {H}type in I
   */
  def noJunks(decls : List[InternalDeclaration], context: Context)(implicit parent : GlobalName): List[Constant] = {
    val (repls, modelContext) : (List[(GlobalName, OMV)], List[VarDecl]) = chain(decls, context)
    val model = modelContext.map(_.toTerm)
    var inductNames : List[(GlobalName,GlobalName)] = Nil
    decls map {d =>
      val (argCon, dApplied) = d.argContext(None)
      val dAppliedInduct = induct(model, d.ret, dApplied, inductNames)
      val dPrimed = utils.listmap(repls, d.path).get
      val dAppliedPrimed = ApplyGeneral(dPrimed, argCon map {vd => induct(model, vd.tp.get, vd.toTerm, inductNames)})
      val ret = d match {
        case tl: TermLevel => Eq(dAppliedInduct, dAppliedPrimed)
        case tl: TypeLevel => Arrow(dAppliedInduct, dAppliedPrimed)
      }
      val tp = Pi(context ++ modelContext ++ argCon, ret)
      val name = inductName(d.name)
      val c = makeConst(name, tp)
      inductNames ::= d.path -> c.path
      c
    } 
  }
  /**
   * name of the declaration corresponding to n declared in noJunks
   */
  private def inductName(n: LocalName) = LocalName("induct_" + n.toPath)
  /**
   * applies the inductive translation needed in noJunks by constructing terms that apply the appropriate induction functions
   * @param model the model for which we need the inductive translation (as built by noJunks)
   * @param tp the type of tm
   * @param tm the term to translate
   * @param inductNames map of the declarations in the inductive type to the corresponding generated induction functions
   */
  private def induct(model: List[Term], tp: Term, tm: Term, inductNames: List[(GlobalName,GlobalName)]) = tp match {
    case ApplyGeneral(OMS(p), args) =>
      utils.listmap(inductNames, p) match {
        case Some(inductP) => ApplyGeneral(OMS(inductP), model:::args:::List(tm))
        case None => tm
      }
    case _ => throw ImplementationError("missing case")
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
      makeConst(ass._2, Pi(modelCtx ++ context ++ args, ass._1))
    }
  }
  
  def toEliminationDecls(decls: List[InternalDeclaration], structure: InternalDeclaration)(implicit parent : GlobalName) : List[InternalDeclaration] = {
    var types : List[TypeLevel] = Nil
    decls map {
      case tpl @ TypeLevel(_, _, _, _, _) => val tplelim = tpl.toEliminationDecl(structure, types map (_.path)); types :+= (tplelim match {case t @ TypeLevel(_,_,_,_,_) => t}); tplelim
      case d => d.toEliminationDecl(structure, types map (_.path))
    }
  }
  
  def structureDeclaration(name: Option[String])(implicit parent : GlobalName) = TypeLevel(uniqueGN(name getOrElse "M"), Nil, None, None, None)
  def introductionDeclaration(structure:TypeLevel, decls: List[InternalDeclaration], nm: Option[String])(implicit parent : GlobalName): TermLevel = {
    val (p, args) = (uniqueGN(nm getOrElse "make"), decls.map(x => (Some(x.name), x.toTerm)))
    TermLevel(p, args, structure.toTerm, None, None, None)
  }
  def reprDeclaration(structure:TypeLevel, decls:List[InternalDeclaration])(implicit parent: GlobalName) : Constant = {
    val arg = structure.makeVar("m", Context.empty)
    val ret : Term = Eq(structure.applyTo(decls.map(_.applyTo(arg.toTerm))), arg.toTerm)
    makeConst(uniqueLN("repr"), Pi(List(arg), ret))
  }
  def quotientEq(structure: TypeLevel, rel: InternalDeclaration, quot: TermLevel, name: Option[String], ctx: Option[Context])(implicit parent: GlobalName): TermLevel = {
    //val FunType(List((_, a), _), q) = structure.tp
    val a = rel.args match {
      case List((_, d), (_,_)) => d
      case tp => println(tp); throw ImplementationError("Unexpected match case. ")
    }
    val args = List(newVar(uniqueLN("a1"), a, None), newVar(uniqueLN("a2"), a, None))
    val ret : Term = Pi(args, Arrow(rel.applyTo(args), Eq(quot.applyTo(args.init), quot.applyTo(args.tail))))
    TermLevel(uniqueGN(name getOrElse "quot"), args map (x => (Some(x.name), x.tp.get)), ret, None, None, ctx)
  }
  def quotientInvert(domain:TypeLevel, structure:TypeLevel, rel:InternalDeclaration, quot: TermLevel, name: Option[String])(implicit parent: GlobalName) = {
    val B = structureDeclaration(Some("B"))
    val f = TermLevel(uniqueGN("f"), List((None, domain.toTerm)), B.toTerm, None, None, None)
    val xy @ List(x, y) = List(domain.makeVar("x", Context.empty), structure.makeVar("y", Context.empty))
    val p = TermLevel(uniqueGN("p"), xy map (x => (Some(x.name), x.tp.get)), Arrow(rel.applyTo(xy), Eq(f.applyTo(x.tp.get), f.applyTo(y.tp.get))), None, None, None)
    (B, f, p, TermLevel(uniqueGN(name getOrElse "quotInvert"), List(B, f, p).map(x => (Some(x.name), x.tp)) :+ (Some(structure.name), structure.toTerm), B.toTerm, None, None, None))
  }
  def quotientInverse(domain:TypeLevel, structure:TypeLevel, rel:InternalDeclaration, quot: TermLevel, B: TypeLevel, F:TermLevel, P:TermLevel, quotInv: TermLevel, name: Option[String])(implicit parent: GlobalName): Constant = {
    val ctx = B.context
    val (b, f, p, arg) = (B.makeVar("b", ctx), newVar(uniqueLN("f"), F.tp, Some(ctx)), newVar(uniqueLN("p"), P.tp, Some(ctx)), domain.makeVar("x", ctx))
    val im = OMV(uniqueLN("image_point")) % quot.applyTo(arg.toTerm)
    val tp = Pi(List(b, f, p, arg), Eq(quotInv.applyTo(List(b, f, p).map(_.toTerm) :+ quot.applyTo(arg.toTerm)), F.applyTo(arg.toTerm)))
    makeConst(uniqueLN(name getOrElse "quotInverse"), tp)
  }
}


/** helper class for the various declarations in an inductive type */ 
sealed abstract class InternalDeclaration {
  def path: GlobalName
  def name = path.name
  def args: List[(Option[LocalName], Term)]
  def ret: Term
  protected def ctx: Option[Context] // TODO do we need this
  def context = ctx getOrElse Context.empty
  def notation: NotationContainer // TODO do we need this
  def tp = Pi(context, FunType(args, ret))
  def df : Option[Term]

  /**
	 * Build a context quantifying over all arguments
	 *  and the term of this constructor applied to those arguments
	 * @param suffix (optional) a suffix to append to the local name of each variable declaration in the returned context
   */
  def argContext(suffix: Option[String]): (Context, Term) = { 
    val suf = suffix getOrElse ""
    val dargs = args.zipWithIndex map {
      case ((Some(loc), arg), _) => (loc, arg)
      case ((None, arg), i) => (uniqueLN("x_"+i), arg)
    }
    val con = dargs map {case (loc, tp) =>
      val n = uniqueLN(loc.toString()+suf)
      newVar(n, tp, None)
    }
    val tp = ApplyGeneral(OMS(path), con.map(_.toTerm))
    (con, tp)
  }
  
  def toVarDecl = VarDecl(name, tp)
  def toConstant(implicit parent : GlobalName) : Constant = makeConst(name, Pi(context, tp), df, Some(notation))
  def toTerm(implicit parent : GlobalName) : Term = OMS(path)
  
  /** apply the internal declaration to the given argument context */
  def applyTo(args: Context): Term = ApplyGeneral(OMS(path), args.map(_.toTerm))
  def applyTo(args: List[Term]):Term = ApplyGeneral(OMS(path), args)
  def applyTo(t: Term): Term = applyTo(List(t))
  
  /**
   * applies a term translator
   */
  def translate(tr: Translator) : InternalDeclaration = {
    val argsT = args map {case (nO, t) => (nO, tr.applyType(context, t))}
    this match {
      case tl: TermLevel => tl.copy(args = argsT, ret = tr.applyType(context, ret))
      case tl: TypeLevel => tl.copy(args = argsT)
      case sl: StatementLevel => sl.copy(args = argsT)
    }    
  }

  // auxiliary methods for specific strucutral features
  
  /**
   * for records: for a declaration c:A, this produces the declaration c: {r:R} A[r]
   * where A[r] is like A with every d declared in this record with d r
   */
  def toEliminationDecl(recType: InternalDeclaration, recordFields: List[GlobalName]): InternalDeclaration = {
    val r = LocalName("r")
    val tr = TraversingTranslator(OMSReplacer(p => if (recordFields contains p) Some(Apply(OMS(p), OMV(r))) else None))
    translate(tr)
  }
  
  def toString(implicit parent : OMID) = name+": "+tp+(if(df != None) " = "+df.get else "")
}

/** type declaration */
case class TypeLevel(path: GlobalName, args: List[(Option[LocalName], Term)], df: Option[Term], notC : Option[NotationContainer], ctx: Option[Context]) extends InternalDeclaration {
  def ret = Univ(1)
  def tm = df
  def notation = notC getOrElse NotationContainer()

  /** a var decl with a fresh name whose type is this one */
  def makeVar(name: String, con: Context) = newVar(uniqueLN(name), applyTo(con), None)
}

/** term constructor declaration */
case class TermLevel(path: GlobalName, args: List[(Option[LocalName], Term)], ret: Term, df: Option[Term], notC: Option[NotationContainer], ctx: Option[Context]) extends InternalDeclaration {
  def tm = df
  def notation = notC getOrElse NotationContainer()
  /** a var decl with a fresh name of the same type as this one */
  def makeVar(name: String) = newVar(uniqueLN(name), tp)

  /**
   * Generate injectivity declaration for the term constructor d
   * @param parent the parent declared module of the derived declaration to elaborate
   * @param d the term level for which to generate the injectivity axiom
   */
  def injDecl(implicit parent : GlobalName) : Constant = {
    val (aCtx, aApplied) = argContext(Some("_0"))
    val (bCtx, bApplied) = argContext(Some("_1"))
    
    val argEq = (aCtx zip bCtx) map {case (a,b) => Eq(a.toTerm,b.toTerm)}
    val resNeq = Neq(aApplied, bApplied)
    val body = Arrow(Arrow(argEq, Contra), resNeq)
    val inj = Pi(context++aCtx ++ bCtx,  body)
    
    makeConst(uniqueLN("injective_"+name.last.toString), inj)
  }
  
  /**
   * Generate surjectivity declaration for the term constructor d
   * @param parent the parent declared module of the derived declaration to elaborate
   * @param d the term level for which to generate the surjectivity axiom
   */
  def surjDecl(implicit parent : GlobalName) : Constant = {
    val im = newVar(uniqueLN("image_point"), ret, Some(context))
    val (aCtx, aApplied) = argContext(None)
    
    val surj = Pi(context++im,  neg(Pi(aCtx, neg(Eq(aApplied, im.toTerm)))))   
    makeConst(uniqueLN("surjective_"+name.last.toString), surj)
  }
   
   
  /**
   * Generate no confusion/injectivity declaration for term constructor d and all term constructors of the same return type
   * @param parent the derived declaration to elaborate
   * @param tmdecls all term level declarations
   */
  def noConf(tmdecls: List[TermLevel])(parent: GlobalName): List[Constant] = {
    var decls: List[Constant] = Nil
    tmdecls.takeWhile(_ != this) foreach {b => 
    if (b.ret == ret) {
        // TODO for dependently-typed, this can generate ill-typed declarations
        val newName = uniqueLN("no_conf_" + name.last+ "_" + b.name.last)
        val (aCtx, aApplied) = argContext(None)
        val (bCtx, bApplied) = b.argContext(None)
        val tp = Pi(context++aCtx ++ bCtx, Neq(aApplied, bApplied))
        decls ::= Constant(OMMOD(parent.module), parent.name/newName, Nil, Some(tp), None, None)
    }
  }
  decls = decls.reverse
  if(args.length > 0)
    decls ::= injDecl(parent)
  decls
  }
}


/** Rules and Judgment constructors */
case class StatementLevel(path: GlobalName, args: List[(Option[LocalName], Term)], df: Option[Term], notC: Option[NotationContainer], ctx: Option[Context]) extends InternalDeclaration {
  def ret = Univ(1)
  def tm = df
  def notation = notC getOrElse NotationContainer()
}
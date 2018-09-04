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
  
   /** a very detailed presenter, useful for debugging */
  def present(c: Constant)(implicit parent: GlobalName) : String = {
    c.path + ": " + present(c.tp.get)
  }
  def present(e: Term)(implicit parent: GlobalName) : String = {
    def flatStrList(l : List[String], sep : String): String = l match {
      case Nil => ""
      case List(hd) => hd
      case hd::tl => hd+sep+flatStrList(tl, sep)
    }
    def preCon(ctx: Context) = {
      val args = (ctx map (x => iterPre(x.toOML)))
      if (args == Nil) "" else " {"+flatStrList(args, ", ")+"}"
    }
    def iterPre(body: Term) : String = {
      body match {
        case OMBIND(Pi.term, con, body) => preCon(con) + " "+iterPre(body)
        case ApplyGeneral(f:Term, args @ hd::tl) => iterPre(f)+ " " + flatStrList(args map iterPre, " ")
        case Arrow(a, b) => "("+iterPre(a) + " -> " + iterPre(b)+")"
        case OMS(target) => "OMS("+target.toPath+")"
        case ApplySpine(t : Term, List(arg1: Term, arg2: Term)) => "("+iterPre(t) + " " + iterPre(arg1) + " " + iterPre(arg2)+")" // + iterPre(arg1) + " " 
        case OMV(n) => "OMV("+n.toString()+")"
        case OML(n, tp, df, _, _) => n.toString()+(if(tp != None) ": "+iterPre(tp.get) else "")+(if (df != None) " = "+iterPre(df.get) else "")
      }
    }
    iterPre(e)
  }  

  val theory: MPath = LF._base ? "Inductive"
  object Eq extends BinaryLFConstantScala(theory, "eq")
  object Neq extends BinaryLFConstantScala(theory, "neq")
  
  val Contra = OMS(theory ? "contra")
  /** negate the statement in the type */
  def neg(tp: Term) : Term = Arrow(tp, Contra)
  
  def externalName(parent: GlobalName, name: LocalName): LocalName = parent.name / name//uniqueLN(parent.name + "__"+name)
  
  /** produces a Constant derived declaration 
   *  @param name the local name of the constant
   *  @param tp the type of the constant
   *  @param df (optional) the definition of the constant
   *  @param parent (implicit) the inductive definition to elaborate
   */
  def makeConst(name: LocalName, tp: Term, df:Option[Term], notC: Option[NotationContainer])(implicit parent: GlobalName): Constant = {
    Constant(OMMOD(parent.module), externalName(parent, name), Nil, Some(tp), df, None, notC getOrElse NotationContainer())
  }
  def makeConst(name: LocalName, tp: Term, df:Option[Term])(implicit parent: GlobalName): Constant = makeConst(name, tp, df, None)
  def makeConst(name: LocalName, tp: Term)(implicit parent:  GlobalName): Constant = makeConst(name, tp, None)
  
  def PiOrEmpty(ctx: Context, body: Term) = if (ctx.isEmpty) body else Pi(ctx, body)
}

import InternalDeclarationUtil._

object InternalDeclaration {
  implicit def tp(d: InternalDeclaration): Term = d.tp
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
  
  def chain(decls: List[InternalDeclaration], context: Context) : (List[(GlobalName, OMV)], List[VarDecl]) = {
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
  
  def structureDeclaration(name: Option[String])(implicit parent : GlobalName) = TypeLevel(uniqueGN(name getOrElse "M"), Nil, None, None, None)
  def introductionDeclaration(structure:TypeLevel, decls: List[InternalDeclaration], nm: Option[String])(implicit parent : GlobalName): TermLevel = {
    val (p, args) = (uniqueGN(nm getOrElse "make"), decls map (x => (Some(x.name), x.toTerm)))
    TermLevel(p, args, structure.toTerm, None, None, None)
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
  def tp = PiOrEmpty(context, FunType(args, ret))
  def df : Option[Term]

  /**
	 * Build a context quantifying over all arguments
	 *  and the term of this constructor applied to those arguments
	 * @param suffix (optional) a suffix to append to the local name of each variable declaration in the returned context
   */
  def argContext(suffix: Option[String])(implicit parent: GlobalName): (Context, Term) = { 
    val suf = suffix getOrElse ""
    val dargs = args.zipWithIndex map {
      case ((Some(loc), arg), _) => (loc, arg)
      case ((None, arg), i) => (uniqueLN("x_"+i), arg)
    }
    val con = dargs map {case (loc, tp) =>
      val n = uniqueLN(loc.toString()+suf)
      newVar(n, tp, None)
    }
    val tp = ApplyGeneral(toTerm, con.map(_.toTerm))
    (con, tp)
  }
  
  def toVarDecl = VarDecl(name, tp)
  def toConstant(implicit parent: GlobalName): Constant = makeConst(name, PiOrEmpty(context, tp), df)(parent)
  def toTerm(implicit parent: GlobalName): Term = OMS(parent.module ? InternalDeclarationUtil.externalName(parent, name))
  
  /** apply the internal declaration to the given argument context */
  def applyTo(args: Context)(implicit parent: GlobalName): Term = ApplyGeneral(toTerm, args.map(_.toTerm))
  def applyTo(args: List[Term])(implicit parent: GlobalName): Term = ApplyGeneral(toTerm, args)
  def applyTo(tm: Term)(implicit parent: GlobalName): Term = applyTo(List(tm))
  
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
  
  override def toString = name+": "+tp+(if(df != None) " = "+df.get else "")
}

/** type declaration */
case class TypeLevel(path: GlobalName, args: List[(Option[LocalName], Term)], df: Option[Term], notC : Option[NotationContainer], ctx: Option[Context]) extends InternalDeclaration {
  def ret = Univ(1)
  def tm = df
  def notation = notC getOrElse NotationContainer()

  /** a var decl with a fresh name whose type is this one */
  def makeVar(name: String, con: Context)(implicit parent: GlobalName) = newVar(uniqueLN(name), applyTo(con), None)
}

object TypeLevel {
  def apply(path: GlobalName, args: Context, df: Option[Term], notC : Option[NotationContainer], ctx: Option[Context]): TypeLevel = {
    TypeLevel(path, args map {vd => (Some(vd.name), vd.toTerm)}, df, notC, ctx)
  }
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
  def injDecl(implicit parent: GlobalName): Constant = {
    val (aCtx, aApplied) = argContext(Some("_0"))
    val (bCtx, bApplied) = argContext(Some("_1"))
    
    val argEq = (aCtx zip bCtx) map {case (a,b) => Eq(a.toTerm, b.toTerm)}
    val resNeq = Neq(aApplied, bApplied)
    val body = Arrow(Arrow(argEq, Contra), resNeq)
    val inj = PiOrEmpty(context++aCtx ++ bCtx,  body)
    
    makeConst(uniqueLN("injective_"+name), inj)(parent)
  }
  
  /**
   * Generate surjectivity declaration for the term constructor d
   * @param parent the parent declared module of the derived declaration to elaborate
   * @param d the term level for which to generate the surjectivity axiom
   */
  def surjDecl(implicit parent: GlobalName): Constant = {
    val im = newVar(uniqueLN("image_point"), ret, Some(context))
    val (aCtx, aApplied) = argContext(None)
    
    val surj = PiOrEmpty(context++im,  neg(PiOrEmpty(aCtx, neg(Eq(aApplied, im.toTerm)))))   
    makeConst(uniqueLN("surjective_"+name), surj)(parent)
  }
}

object TermLevel {
  def apply(path: GlobalName, args: Context, ret: Term, df: Option[Term], notC : Option[NotationContainer], ctx: Option[Context]): TermLevel = {
      TermLevel(path, args map {vd => (Some(vd.name), vd.toTerm)}, ret, df, notC, ctx)
  }
  def apply(path: GlobalName, args: Context, ret: Term, df: Option[Term], ctx: Option[Context]): TermLevel = TermLevel(path, args, ret, df, None, ctx)
}

/** Rules and Judgment constructors */
case class StatementLevel(path: GlobalName, args: List[(Option[LocalName], Term)], df: Option[Term], notC: Option[NotationContainer], ctx: Option[Context]) extends InternalDeclaration {
  def ret = Univ(1)
  def tm = df
  def notation = notC getOrElse NotationContainer()
}

object StatementLevel {
  def apply(p: GlobalName, a: Context, df: Option[Term], n: Option[NotationContainer], c: Option[Context]): StatementLevel = StatementLevel(p, a.map(x => (Some(x.name), x.toTerm)), df, n, c)
  def apply(p: GlobalName, a: Context, df: Option[Term], c: Option[Context]): StatementLevel = StatementLevel(p, a.map(x => (Some(x.name), x.toTerm)), df, None, c)
  def apply(p: GlobalName, a: Context, df: Option[Term]): StatementLevel = StatementLevel(p, a, df, None)
}
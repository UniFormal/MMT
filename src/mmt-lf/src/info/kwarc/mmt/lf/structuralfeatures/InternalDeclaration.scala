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
  def present(c: Constant) : String = {
    c.path + ": " + present(c.tp.get, false)
  }
  /** a very detailed presenter, useful for debugging */
  def shortPresent(c: Constant) : String = {
    c.path + ": " + present(c.tp.get, true)
  }
  def present(e: Term, shortNames: Boolean) : String = {
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
        case OMS(target) => if (shortNames) target.name.toString() else "OMS("+target.name+")"
        case ApplySpine(t : Term, List(arg1: Term, arg2: Term)) => "("+iterPre(t) + " " + iterPre(arg1) + " " + iterPre(arg2)+")" // + iterPre(arg1) + " " 
        case OMV(n) => if (shortNames) n.toString() else "OMV("+n.toString()+")"
        case OML(n, tp, df, _, _) => n.toString()+(if(tp != None) ": "+iterPre(tp.get) else "")+(if (df != None) " = "+iterPre(df.get) else "")
      }
    }
    iterPre(e)
  }

  val theory: MPath = LF._base ? "DHOL"
  object Ded {
    val path = LF._base ? "Ded" ? "DED"
    def apply(t: Term) = ApplySpine(OMS(path), t)
  }
  object Eq {
    val path = theory ? "EQUAL"
    def apply(a: Term, s: Term, t: Term) = Ded(ApplySpine(OMS(path), a, s, t))
  }
  object Neq {
    val path = theory ? "NOTEQUAL"
    def apply(a: Term, s: Term, t: Term) = Ded(ApplySpine(OMS(path), a, s, t))
  }
  
  val Contra = OMS(theory ? "CONTRA")
  /** negate the statement in the type */
  def neg(tp: Term) : Term = Arrow(tp, Contra)
  
  def externalName(parent: GlobalName, name: LocalName): GlobalName = //(parent.module / parent.name, name)
    parent.module ? parent.name / name
 
  def externalizeNamesAndTypes(parent: GlobalName, params: Context):Term=>Term = { x=>
    externalizeNames(parent)(externalizeTypes(parent, params)(x, params), params)
  }
  
  def externalizeTypes(parent: GlobalName, params: Context) = {
    def r(p: GlobalName) = if (p.module == parent.toMPath) Some(ApplyGeneral(OMS(p), params.map(_.toTerm))) else None
    OMSReplacer(r _)
  }
  
  def externalizeNames(parent: GlobalName) = {
    def r(p: GlobalName) = if (p.module == parent.toMPath) Some(OMS(externalName(parent, p.name))) else None
    OMSReplacer(r _)
  }
  
  /** produces a Constant derived declaration 
   *  @param name the local name of the constant
   *  @param tp the type of the constant
   *  @param Lnot (optional) the parsing notation of the constant
   *  @param df (optional) the definition of the constant
   *  @param parent (implicit) the inductive definition to elaborate
   */
  def makeConst(name: LocalName, Ltp: () => Term, Ldf: () => Option[Term] = () => None, Lnot: () => Option[TextNotation] = () => None)(implicit parent:  GlobalName): Constant = {
    val p = externalName(parent, name)
    new SimpleLazyConstant(OMMOD(p.module), p.name) {
      otherDefined = true
      def onAccess {
        _tp = Some(Ltp())
        _df = Ldf()
        _not = Lnot()
      }
    }
  }
//  def makeConst(name: LocalName, Ltp: () => Term, Ldf: () => Option[Term] = () => None)(implicit parent:  GlobalName): Constant = {makeConst(name, Ltp, () => None, Ldf)}
  def makeConst(name: LocalName, Ltp: () => Term)(implicit parent: GlobalName):Constant = {makeConst(name, Ltp, ()=>{None})}
  def injDecl(c: Constant, con: Controller, ctx: Option[Context], types:List[TypeLevel])(implicit parent: GlobalName) = {
    val decl = InternalDeclaration.fromConstant(c, con, ctx)
    val tmdecl = decl match {case tmd: TermLevel => tmd case _ => throw ImplementationError("Termlevel declaration expected.")}
    tmdecl.injDecls
  }
  def surjDecl(c: Constant, con: Controller, ctx: Option[Context])(implicit parent: GlobalName) = {
    val decl = InternalDeclaration.fromConstant(c, con, ctx)
    val tmdecl = decl match {case tmd: TermLevel => tmd case _ => throw ImplementationError("Termlevel declaration expected.")}
    tmdecl.surjDecl
  }
  
  def PiOrEmpty(ctx: Context, body: Term) = if (ctx.isEmpty) body else Pi(ctx, body)
  def LambdaOrEmpty(ctx: Context, body: Term) = if (ctx.isEmpty) body else Lambda(ctx, body)
}

import InternalDeclarationUtil._

object InternalDeclaration {
  implicit def tp(d: InternalDeclaration): Term = d.tp
  implicit def tm(d: InternalDeclaration): Option[Term] = d.df
  implicit def toVarDecl(d: InternalDeclaration)(implicit parent: GlobalName): VarDecl = d.toVarDecl

  //TODO: Implement this properly (not required in LF)
  def isTypeLevel(tp: Term) : Boolean = {
    val FunType(args, ret) = tp
    ret == Univ(1)
  }
  
  /**
   * convert the given constant into the appropriate internal declaration
   * @param c the constant to convert
   * @param con the controller
   */
  def fromConstant(c: Constant, con: Controller, ctx: Option[Context])(implicit parent : GlobalName) : InternalDeclaration = {
    val tp = c.tp getOrElse {throw InvalidElement(c, "missing type")}
    val FunType(args, ret) = tp
    val context = Some(ctx getOrElse Context.empty)
    val p = c.path
    if (JudgmentTypes.isJudgment(ret)(con.globalLookup)) {
      StatementLevel(p, args, c.df, context, Some(c.notC))
    } else {
      ret match {
        case Univ(1) => TypeLevel(p, args, c.df, context, Some(c.notC))
        case Univ(x) if x != 1 => throw ImplementationError("unsupported universe")
        case r => TermLevel(p, args, ret, c.df, Some(c.notC), ctx)//if (isTypeLevel(r)) TypeLevel(p, args, c.df, context, Some(c.notC)) else TermLevel(p, args, ret, c.df, Some(c.notC), ctx)
      }
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
  
  /** build a dictionary from the declaration paths to OMV with their "primed" type, as well as a context with all the "primed" declarations*/
  def chain(decls: List[InternalDeclaration], context: Context)(implicit parent: GlobalName) : (List[(GlobalName, OMV)], Context) = {
    var repls: List[(GlobalName,OMV)] = Nil
    val tr = TraversingTranslator(OMSReplacer(p => utils.listmap(repls, p)))
    val modelContext = decls map {d =>
      val namePrimed = d.name / "'"
      val vdPrimed = VarDecl(namePrimed, tr(context, externalizeTypes(parent, context)(d.tp, context)))
      repls ::= d.path -> OMV(namePrimed)
      vdPrimed
    }
    (repls, modelContext)
  }
  
  def structureDeclaration(name: Option[String], context: Option[Context])(implicit parent : GlobalName) = {
    val Ltp = () => {
      PiOrEmpty(context getOrElse Context.empty, Univ(1))
    }
    makeConst(uniqueLN(name getOrElse "type"), Ltp)
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
  def internalTp = FunType(args, ret)
  def df : Option[Term]
  def isTypeLevel: Boolean
  
  /** like tp but with all names externalized */
  def externalTp(implicit parent: GlobalName) = externalizeNamesAndTypes(parent, context)(tp)
  /** like df but with all names externalized */
  def externalDf(implicit parent: GlobalName) = df map {t => externalizeNamesAndTypes(parent, context)(t)}
  def externalRet(implicit parent: GlobalName) = externalizeNamesAndTypes(parent, context)(ret)
 
  
  /** checks whether the type of the declaration is dependent on the given argument */
  def isIndependentArgument(arg: (Option[LocalName], Term)): Boolean = arg match {
    case (None, _) => true
    case (Some(name), tp) =>
        val remainingArgs = args.reverse.takeWhile(_ != arg).reverse
        !(remainingArgs:+(None,ret)).exists{argTp => argTp._2.freeVars contains name}
  }
  
  /** checks whether the declaration is simply typed */
  def isSimplyTyped(): Boolean = {
    (args.filterNot{_._2 == Univ(1)}) forall {case arg@(_, tp) => 
      isIndependentArgument(arg) || tp == Univ(1)
    }
  }
  
  /**
	 * Build a context quantifying over all arguments
	 *  and the term of this constructor applied to those arguments
	 * @param suffix (optional) a suffix to append to the local name of each independent variable declaration in the returned context
   */
  def argContext(suffix: Option[String])(implicit parent: GlobalName): (Context, Term) = { 
    val suf = suffix getOrElse ""
    val dargs = args.zipWithIndex map {
      case ((Some(loc), arg), _) => (loc, arg)
      case ((None, arg), i) => (uniqueLN("x_"+i), arg)
    }
    // In case of an OMV argument used in the type of a later argument
    var subs = Substitution()
    val con: Context = dargs zip args map {case ((loc, tp), arg) =>
      val locSuf = if (isIndependentArgument(arg)) uniqueLN(loc+suf) else loc
      if (loc != locSuf) subs = subs ++ OMV(loc) / OMV(locSuf)
      newVar(locSuf, externalizeNamesAndTypes(parent, context)(tp ^? subs), None)
    }
    val tp = ApplyGeneral(toTerm, con.map(_.toTerm))
    (con, tp)
  }
  
  def toVarDecl = VarDecl(name, tp)
  def toConstant(implicit parent: GlobalName): Constant = {
    // for n parameters, copy over notation but add n implicit arguments at beginning
    def not = notation.parsing.flatMap {n =>
      val numPars = context.length
      try {
        Some(n.copy(fixity = n.fixity.addInitialImplicits(numPars)))
      } catch { 
        case ImplementationError(m) => println(m+" in the notation for the externalized internal declaration: "+name); None
      }
    }
    makeConst(name, () => externalTp, () => externalDf, () => not)(parent)
  }
  def toTerm(implicit parent: GlobalName): Term = ApplyGeneral(OMS(externalName(parent, name)), context.map(_.toTerm))
  
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

  override def toString = name+": "+tp+(if(df != None) " = "+df.get else "")
}

/** type declaration */
case class TypeLevel(path: GlobalName, args: List[(Option[LocalName], Term)], df: Option[Term], ctx: Option[Context]=None, notC : Option[NotationContainer]=None) extends InternalDeclaration {
  def ret = Univ(1)
  def tm = df
  def notation = notC getOrElse NotationContainer()
  def isTypeLevel = {true}

  /** a var decl with a fresh name whose type is this one */
  def makeVar(name: String, con: Context)(implicit parent: GlobalName) = newVar(uniqueLN(name), applyTo(con), None)
}

object TypeLevel {
  def apply(path: GlobalName, args: Context, df: Option[Term], ctx: Option[Context], notC : Option[NotationContainer]): TypeLevel = TypeLevel(path, args map {vd => (Some(vd.name), vd.toTerm)}, df, ctx, notC)
  def apply(path: GlobalName, args: Context, df: Option[Term], ctx: Option[Context]): TypeLevel = TypeLevel(path, args map {vd => (Some(vd.name), vd.toTerm)}, df, ctx)
  def apply(path: GlobalName, args: Context, df: Option[Term]): TypeLevel = TypeLevel(path, args map {vd => (Some(vd.name), vd.toTerm)}, df)
  def apply(path: GlobalName, args: Context): TypeLevel = TypeLevel(path, args map {vd => (Some(vd.name), vd.toTerm)}, None)
}

/** term constructor declaration */
case class TermLevel(path: GlobalName, args: List[(Option[LocalName], Term)], ret: Term, df: Option[Term]=None, notC: Option[NotationContainer]=None, ctx: Option[Context]=None) extends InternalDeclaration {
  def tm = df
  def notation = notC getOrElse NotationContainer()
  /** a var decl with a fresh name of the same type as this one */
  def makeVar(name: String) = newVar(uniqueLN(name), tp)
  def isTypeLevel = {false}

  /**
   * Generate injectivity declaration for the term constructor d
   * @param parent the parent declared module of the derived declaration to elaborate
   * @param d the term level for which to generate the injectivity axiom
   */
  def injDecls(implicit parent: GlobalName): List[Constant] = {
    val (aCtx, aApplied) = argContext(Some("_0"))
    val (bCtx, bApplied) = argContext(Some("_1"))
    val abi = (aCtx zip bCtx) zipWithIndex
    val bis = abi filter {case ((a,b),i) => a != b}
    bis map {case _@((a, b),i) =>
      val Ltp = () => {
        val argNeq = Neq(a.tp.get, a.toTerm, b.toTerm) // TODO does not type-check if ret depends on arguments
        val resNeq = Neq(externalRet, aApplied, bApplied)  // TODO does not type-check if ret depends on arguments
        val res=PiOrEmpty(context++aCtx ++ bis.map(_._1._2),  Arrow(argNeq, resNeq))
        res
      }
      makeConst(uniqueLN("injective_"+name+"_"+i.toString), Ltp, ()=>{None})(parent)
    }
  }
  
  /**
   * Generate surjectivity declaration for the term constructor d
   * @param parent the parent declared module of the derived declaration to elaborate
   * @param d the term level for which to generate the surjectivity axiom
   */
  def surjDecl(implicit parent: GlobalName): Constant = {
    val Ltp = () => {
      val im = newVar(uniqueLN("image_point"), ret, Some(context))
      val (aCtx, aApplied) = argContext(None)
    
      PiOrEmpty(context++im,  neg(PiOrEmpty(aCtx, neg(Eq(ret, aApplied, im.toTerm)))))
    }
    makeConst(uniqueLN("surjective_"+name), Ltp, ()=>{None})(parent)
  }
}

object TermLevel {
  def apply(path: GlobalName, args: Context, ret: Term, df: Option[Term], notC : Option[NotationContainer], ctx: Option[Context]): TermLevel = {
      TermLevel(path, args map {vd => (Some(vd.name), vd.toTerm)}, ret, df, notC, ctx)
  }
  def apply(path: GlobalName, args: Context, ret: Term, df: Option[Term], ctx: Option[Context]): TermLevel = TermLevel(path, args, ret, df, None, ctx)
}

/** Rules and Judgment constructors */
case class StatementLevel(path: GlobalName, args: List[(Option[LocalName], Term)], df: Option[Term]=None, ctx: Option[Context]=None, notC: Option[NotationContainer]=None) extends InternalDeclaration {
  def ret = Univ(1)
  def tm = df
  def notation = notC getOrElse NotationContainer()
  def isTypeLevel = {false}
}

object StatementLevel {
  def apply(p: GlobalName, a: Context, df: Option[Term], c: Option[Context], n: Option[NotationContainer]): StatementLevel = StatementLevel(p, a.map(x => (Some(x.name), x.toTerm)), df, c, n)
  def apply(p: GlobalName, a: Context, df: Option[Term], c: Option[Context]): StatementLevel = StatementLevel(p, a, df, c, None)
  def apply(p: GlobalName, a: Context, df: Option[Term]): StatementLevel = StatementLevel(p, a, df, None)
  def apply(p: GlobalName, a: Context): StatementLevel = StatementLevel(p, a, None)
}
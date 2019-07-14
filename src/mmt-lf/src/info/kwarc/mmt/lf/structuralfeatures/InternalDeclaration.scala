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
   * Make a new unique local name from the given string which is unique in the given context
   * @param nm the string from which the local name is generated
   * @param ctx (optional) the context
   */
  def uniqueLN(nm: String, ctx: Option[Context] = None): LocalName = {
    val c = ctx.getOrElse(Context.empty)
     Context.pickFresh(c, LocalName(nm))._1
  }
  
  def uniqueGN(nm: String)(implicit parent: GlobalName): GlobalName = parent.module ? nm

  /**
    * Make a new variable declaration for a variable of type tp and fresh name (preferably name) in the given (optional) context
    * @param name the local name that will preferably be picked for the variable declaration
    * @param tp the type of the variable
    * @param con (optional) the context in which to pick the a fresh variable
    * @note postcondition: the returned variable is free in the given context
    */
   def newVar(name: String, tp: Term, con: Option[Context] = None) : VarDecl = {
     VarDecl(uniqueLN(name.toString, con), tp)
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
  def makeConst(name: LocalName, Ltp: () => Term, simplifyTag: Boolean = false, Ldf: () => Option[Term] = () => None, Lnot: () => Option[TextNotation] = () => None)(implicit parent:  GlobalName): Constant = {
    val p = externalName(parent, name)
    new SimpleLazyConstant(OMMOD(p.module), p.name) {
      otherDefined = true
      def onAccess {
        _tp = Some(Ltp())
        _df = Ldf()
        _not = Lnot()
        _rl = if (simplifyTag) {
          Some(SimplificationRuleGenerator.SimplifyTag)
        } else {
          None
        }
      }
    }
  }
//  def makeConst(name: LocalName, Ltp: () => Term, Ldf: () => Option[Term] = () => None)(implicit parent:  GlobalName): Constant = {makeConst(name, Ltp, () => None, Ldf)}
def makeConst(name: LocalName, Ltp: () => Term, simplifyTag: Boolean)(implicit parent: GlobalName):Constant = {
    makeConst(name, Ltp, simplifyTag, ()=>{None})
  }
  def makeConst(name: LocalName, Ltp: () => Term)(implicit parent: GlobalName):Constant = {
    makeConst(name, Ltp, false, ()=>{None})
  }
  def injDecl(c: Constant, con: Controller, ctx: Option[Context], simplifyTag: Boolean=false)(implicit parent: GlobalName) = {
    val decl = InternalDeclaration.fromConstant(c, con, Nil, ctx)
    val tmdecl = decl match {case tmd: TermLevel => tmd case _ => throw ImplementationError("Termlevel declaration expected.")}
    tmdecl.injDecls(simplifyTag)
  }
  def surjDecl(c: Constant, con: Controller, ctx: Option[Context], simplifyTag: Boolean=false)(implicit parent: GlobalName) = {
    val decl = InternalDeclaration.fromConstant(c, con, Nil, ctx)
    val tmdecl = decl match {case tmd: TermLevel => tmd case _ => throw ImplementationError("Termlevel declaration expected.")}
    tmdecl.surjDecl(simplifyTag)
  }
  
  def PiOrEmpty(ctx: Context, body: Term) = if (ctx.isEmpty) body else Pi(ctx, body)
  def unapplyPiOrEmpty(tm: Term) : (Context, Term) = tm match {
    case Pi(n, tp, x) => 
      val (ctx, body) = unapplyPiOrEmpty(x)
      (OMV(n) % tp::ctx, body)
    case t => (Context.empty, t)
  }
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
  def tpls(decls: List[InternalDeclaration]): List[TypeLevel] = {decls.map{case s: TypeLevel=> Some(s) case _ => None}.filter(_.isDefined).map(_.get)}
  def tmls(decls: List[InternalDeclaration]): List[TermLevel] = {decls.map{case s: TermLevel=> Some(s) case _ => None}.filter(_.isDefined).map(_.get)}
  def constrs(decls: List[InternalDeclaration]): List[Constructor] = {decls.map{case s: Constructor=> Some(s) case _ => None}.filter(_.isDefined).map(_.get)}
  def outs(decls: List[InternalDeclaration]): List[OutgoingTermLevel] = {decls.map{case s: OutgoingTermLevel=> Some(s) case _ => None}.filter(_.isDefined).map(_.get)}
  def stats(decls: List[InternalDeclaration]): List[StatementLevel] = {decls.map{case s: StatementLevel => Some(s) case _ => None}.filter(_.isDefined).map(_.get)}
  
  /**
   * convert the given constant into the appropriate internal declaration
   * @param c the constant to convert
   * @param con the controller
   */
  def fromConstant(c: Constant, con: Controller, types: List[GlobalName], ctx: Option[Context])(implicit parent : GlobalName) : InternalDeclaration = {
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
        case r => 
          val isConstr = ret match {case ApplyGeneral(OMS(p), _) => types contains p case _ => false}
          if (isConstr) new Constructor(p, args, ret, c.df, Some(c.notC), ctx) else new OutgoingTermLevel(p, args, ret, c.df, Some(c.notC), ctx)
      }
    }
  }
  
  
  /**
   * convert the given VarDecl into the appropriate internal declaration
   * @param vd the vd to convert
   * @param con the controller
   */
  def fromVarDecl(vd: VarDecl, ctx: Option[Context], con: Controller, types: List[GlobalName], context: Context)(implicit parent: GlobalName) : InternalDeclaration = {
    fromConstant(vd.toConstant(parent.module, ctx getOrElse Context.empty), con, types, Some(context))
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
    makeConst(uniqueLN(name getOrElse "type"), Ltp, false)
  }
}

import StructuralFeatureUtils._

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
  def isOutgoing = {InternalDeclaration.outs(List(this)) != Nil}
  def isConstructor = {InternalDeclaration.constrs(List(this)) != Nil}
  
  /** like tp but with all names externalized */
  def externalTp(implicit parent: GlobalName) = externalizeNamesAndTypes(parent, context)(tp)
  /** like df but with all names externalized */
  def externalDf(implicit parent: GlobalName) = df map {t => externalizeNamesAndTypes(parent, context)(t)}
  def externalRet(implicit parent: GlobalName) = externalizeNamesAndTypes(parent, context)(ret)
  /** like path but with externalized path */
  def externalPath(implicit parent: GlobalName): GlobalName = externalizeNames(parent)(OMS(path), Context.empty) match {case OMS(p) => p}
 
  
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
      case ((None, arg), i) => (uniqueLN("x_"+i, Some(context)), arg)
    }
    // In case of an OMV argument used in the type of a later argument
    var subs = Substitution()
    val con: Context = dargs zip args map {case ((loc, tp), arg) =>
      val locSuf = if (isIndependentArgument(arg)) uniqueLN(loc+suf, Some(context)) else loc
      if (loc != locSuf) subs = subs ++ OMV(loc) / OMV(locSuf)
      newVar(locSuf.toString, externalizeNamesAndTypes(parent, context)(tp ^? subs), None)
    }
    val tp = ApplyGeneral(toTerm, con.map(_.toTerm))
    (con, tp)
  }
  
  def toVarDecl = VarDecl(name, tp)
  def toConstant(simplifyTag: Boolean)(implicit parent: GlobalName): Constant = {
    // for n parameters, copy over notation but add n implicit arguments at beginning
    def not = notation.parsing.flatMap {n =>
      val numPars = context.length
      try {
        Some(n.copy(fixity = n.fixity.addInitialImplicits(numPars)))
      } catch { 
        case ImplementationError(m) => println(m+" in the notation for the externalized internal declaration: "+name); None
      }
    }
    makeConst(name, () => externalTp, simplifyTag, () => externalDf, () => not)(parent)
  }
  def toConstant(implicit parent: GlobalName): Constant = {toConstant(false)}
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
      case constr: Constructor => new Constructor(constr.path, argsT, tr.applyType(context, ret), constr.df, constr.notC, constr.ctx)
      case out: OutgoingTermLevel => new OutgoingTermLevel(out.path, argsT, tr.applyType(context, ret), out.df, out.notC, out.ctx)
      case tl: TypeLevel => tl.copy(args = argsT)
      case sl: StatementLevel => sl.copy(args = argsT)
      case _ => throw ImplementationError("invalid InternalDeclaration")
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

  /** a var decl with a fresh name whose type is this one 
   * @param name a suggestion for the name of the new variable
   * @param con a context of arguments to apply the tpl to (other than the context)
   */
  def makeVar(name: String, con: Context)(implicit parent: GlobalName) = newVar(name, applyTo(con), None)
}

object TypeLevel {
  def apply(path: GlobalName, args: Context, df: Option[Term], ctx: Option[Context], notC : Option[NotationContainer]): TypeLevel = TypeLevel(path, args map {vd => (Some(vd.name), vd.toTerm)}, df, ctx, notC)
  def apply(path: GlobalName, args: Context, df: Option[Term], ctx: Option[Context]): TypeLevel = TypeLevel(path, args map {vd => (Some(vd.name), vd.toTerm)}, df, ctx)
  def apply(path: GlobalName, args: Context, df: Option[Term]): TypeLevel = TypeLevel(path, args map {vd => (Some(vd.name), vd.toTerm)}, df)
  def apply(path: GlobalName, args: Context): TypeLevel = TypeLevel(path, args map {vd => (Some(vd.name), vd.toTerm)}, None)
}

/** term constructor declaration */
abstract case class TermLevel(path: GlobalName, args: List[(Option[LocalName], Term)], ret: Term, df: Option[Term]=None, notC: Option[NotationContainer]=None, ctx: Option[Context]=None) extends InternalDeclaration {
  def tm = df
  def notation = notC getOrElse NotationContainer()
  /** a var decl with a fresh name of the same type as this one */
  def makeVar(name: String) = newVar(name, tp)
  def isTypeLevel = {false}
  /** Check whether the TermLevel is a constructor or outgoing */
  def isConstructor:Boolean// = {throw ImplementationError("This should never be called. ")}

  /**
   * Generate injectivity declaration for the term constructor d
   * @param parent the parent declared module of the derived declaration to elaborate
   * @param d the term level for which to generate the injectivity axiom
   */
  def injDecls(simplifyTag: Boolean=false)(implicit parent: GlobalName): List[Constant] = {
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
      makeConst(uniqueLN("injective_"+name+"_"+i.toString), Ltp, simplifyTag, ()=>{None})(parent)
    }
  }
  
  /**
   * Generate surjectivity declaration for the term constructor d
   * @param parent the parent declared module of the derived declaration to elaborate
   * @param d the term level for which to generate the surjectivity axiom
   */
  def surjDecl(simplifyTag: Boolean=false)(implicit parent: GlobalName): Constant = {
    val Ltp = () => {
      val im = newVar("image_point", ret, Some(context))
      val (aCtx, aApplied) = argContext(None)
    
      PiOrEmpty(context++im,  neg(PiOrEmpty(aCtx, neg(Eq(ret, aApplied, im.toTerm)))))
    }
    makeConst(uniqueLN("surjective_"+name), Ltp, simplifyTag)(parent)
  }
}

class OutgoingTermLevel(path: GlobalName, args: List[(Option[LocalName], Term)], ret: Term, df: Option[Term]=None, notC: Option[NotationContainer]=None, ctx: Option[Context]=None) extends TermLevel(path, args, ret: Term, df, notC, ctx) {
  override def isConstructor = {false}
}
object OutgoingTermLevel {
  private def fromTml(tml: TermLevel): OutgoingTermLevel = new OutgoingTermLevel(tml.path, tml.args, tml.ret, tml.df, tml.notC, tml.ctx)
  def unapply(d: InternalDeclaration, types: List[GlobalName]) = d match {
    case tml: TermLevel  if (!tml.isConstructor) => Some(fromTml(tml))
    case _ => None
  }
  def unapply(d: InternalDeclaration) = d match {
    case tml: Constructor => Some(fromTml(tml))
    case _ => None
  }
}
class Constructor(path: GlobalName, args: List[(Option[LocalName], Term)], ret: Term, df: Option[Term]=None, notC: Option[NotationContainer]=None, ctx: Option[Context]=None) extends TermLevel(path, args, ret: Term, df, notC, ctx) {
  override def isConstructor = {true}
  def getTpl(tpdecls: List[TypeLevel]): TypeLevel = ret match {
    case ApplyGeneral(OMS(tpl), args) => tpdecls.find(_.path == tpl).get
  }
  def getTplArgs: List[Term] = ret match {
    case ApplyGeneral(_, args) => args
  }
}

object Constructor {
  private def fromTml(tml: TermLevel): Constructor = new Constructor(tml.path, tml.args, tml.ret, tml.df, tml.notC, tml.ctx)
  def unapply(d: InternalDeclaration): Option[Constructor] = d match {
    case tml: TermLevel if (tml.isConstructor) => Some(fromTml(tml))
    case _ => None
  }
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
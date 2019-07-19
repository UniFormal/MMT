package info.kwarc.mmt.lf.structuralfeatures

import info.kwarc.mmt.api._
import objects._
import symbols._
import notations._
import checking._
import modules._
import frontend.Controller

import info.kwarc.mmt.lf._
import InternalDeclarationUtil._
import InternalDeclaration._

object StructuralFeatureUtils {
  val theory: MPath = LF._base ? "DHOL"
  object Ded {
    val path = LF._base ? "Ded" ? "DED"
    def apply(t: Term) = ApplySpine(OMS(path), t)
    def unapply(t: Term) : Option[Term] = t match {
      case Apply(OMS(path),tm) => Some(tm)
      case t => throw ImplementationError("Unexpected term found in unapply method for Ded: "+t)
    }
  }
  object Eq {
    val path = theory ? "EQUAL"
    def apply(tp: Term, tm1: Term, tm2: Term) = Ded(ApplySpine(OMS(path), tp, tm1, tm2))
    def unapply(t: Term) : Option[(Term, Term, Term)] = t match {
      case Ded(ApplyGeneral(OMS(path), List(tp, tm1, tm2))) => Some((tp, tm1, tm2))
    }
  }
  object Neq {
    val path = theory ? "NOTEQUAL"
    def apply(tp: Term, tm1: Term, tm2: Term) = Ded(ApplySpine(OMS(path), tp, tm1, tm2))
    def unapply(t: Term) : Option[(Term, Term, Term)] = t match {
      case Ded(ApplyGeneral(OMS(path), List(tp, tm1, tm2))) => Some((tp, tm1, tm2))
    }
  }
  
  // Needed for the testers
  val Prop = Univ(1)
  val Bool = OMS(LF._base ? "Bool" ? "BOOL")
  val True = OMS(LF._base ? "Bool" ? "TRUE")
  val False = OMS(LF._base ? "Bool" ? "FALSE")
  
  // Needed for the unappliers
  val theoryPath = LF._base ? "Option"
  object OPTION {
    val path = theoryPath ? "OPTION"
    def apply(tp: Term) = ApplySpine(OMS(path), tp)
  }
  object NONE {
    val path = theoryPath ? "NONE"
    def apply(tp: Term) = ApplySpine(OMS(path), tp)
  }
  object SOME {
    val path = theoryPath ? "SOME"
    def apply(tp: Term, tm: Term) = ApplyGeneral(OMS(path), List(tp, tm))
  }
  object MAP {
    val path = theoryPath ? "MAP"
    def apply(a: Term, tp: Term, f: Term) = ApplyGeneral(OMS(path), List(a, tp, f))
  }
  
  val Contra = OMS(theory ? "CONTRA")
  /** negate the statement in the type */
  def neg(tp: Term) : Term = Arrow(tp, Contra)
  
  /** To allow for more user friendly inductive-proof declarations
   * Takes a predicate pr: ⊦ x ≐ y for x: A, y: A, a (initial) function f: A → B and returns a predicate ⊦ f a ≐ f b
   * @param pr the predicate : ⊦ x ≐ y
   * @param f the function f : ⊦ A → B
   */  
  object CONG {
    val path = theory ? "CONG"
    def apply(A: Term, B: Term, x: Term, y: Term, p_xeqy: Term, f: Term) = ApplyGeneral(OMS(path), List(A,B,f,x,y, p_xeqy))
    def unapply(tm: Term) : Option[(Term, Term, Term, Term, Term, Term)] = tm match {
      case ApplyGeneral(OMS(path), List(a, b, x, y, p, f)) => Some((a, b, x, y, p, f))
    }
  }
  
  /** More convenient version of CONG, which manually infers the first four arguments */
  def Cong(prTp: Term, prDf : Term, funcTp: Term, funcDf: Term) : Term = {
    val (tp, x, y) = prTp match {
      case Eq(tp, tm1, tm2) => (tp, tm1, tm2)
      case _ => throw ImplementationError("Unexpected type found for the proof. Can't apply Cong. ")  
    }
    val (a, b) = funcTp match {
      case Arrow(dom, codom) => (dom, codom)
      case _ => throw ImplementationError("Unexpected type found for the function. Can't apply Cong.")
    }
    CONG(a, b, x, y, prDf, funcDf)
  }
  
  def Cong(pr: VarDecl, func: VarDecl) : Term = {Cong(pr.tp.get, pr.toTerm, func.tp.get, func.toTerm)}
  
  /**
   * Takes a predicate pr: ⊦ x ≐ y for x: C → D, y: C → D and a term tm: C and returns a predicate ⊦ f x ≐ f y, where f x := x tm
   * @param pr the predicate : ⊦ x ≐ y
   * @param f the function f : ⊦ f x ≐ f y
   * @param tm the term tm: C
   * @return the type and and proof of the resulting predicate
   */
  def CongAppl(prTp: Term, prDf: Term, tm: Term) : (Term, Term) = {
    val Eq(tp, x, y) = prTp
    val (c, d) = tp match {case FunType(a::as, b) => (a._2, FunType(as, b)) case _ => throw ImplementationError("Expected terms of a function type in CongAppl for "+prTp.toStr(true)+", but found "+tp.toStr(true))}
    val f = newVar("f", tp)
    val df = Lambda(f, Apply(f.toTerm, tm))
    (Eq(d, Apply(df, x), Apply(df, y)), Cong(prTp, prDf, Arrow(tp, d), df))
  }
  
  /**
   * Folds a chain of Congs over a list of arguments, given the initial predicate prInitial and the type of the initial terms
   * @param prInitialTp the type of the initial predicate : ⊦ x ≐ y
   * @param prInitialDf the proof of the initial predicate : ⊦ x ≐ y
   * @param argCtx the chain of arguments to which the terms are to be applied
   * @return the type and and proof of the resulting predicate
   */
  def Congs(prInitialTp: Term, prInitialDf: Term, argCtx: Context) : (Term, Term) = {
      val start = (prInitialTp, prInitialDf)
      argCtx.map(_.toTerm).foldLeft(start)({
        case ((prcTp, prcDf), tm) => 
          val Eq(tp, x, y) = prcTp
          CongAppl(prcTp, prcDf, tm)
      })
  }
  
  
  def parseInternalDeclarationsSubstitutingDefiniens(decls: List[Constant], con: Controller, ctx: Option[Context])(implicit parent : GlobalName): List[InternalDeclaration] = {
    val context = ctx.getOrElse(Context.empty)
    var types: List[GlobalName] = Nil

    decls map {c =>
      val intDecl = fromConstant(c, con, types, ctx)
      def repDfs(df: GlobalName) = {utils.listmap(decls.map(t => (t.path, ApplyGeneral(t.df.get, context.map(_.toTerm)))), df)}
      def mpDf(df: Term):Term = OMSReplacer(repDfs)(df, Context.empty)
      val repDf = intDecl.df map (mpDf)
      
      val decl = intDecl match {
          case constr: Constructor => new Constructor(constr.path, constr.args, constr.ret, repDf, constr.notC, constr.ctx)
          case out: OutgoingTermLevel => new OutgoingTermLevel(out.path, out.args, out.ret, repDf, out.notC, out.ctx)
          case d : TypeLevel => d.copy(df = repDf)
          case d : StatementLevel => d.copy(df = repDf)
          case _ => throw ImplementationError("invalid InternalDeclaration")
         }
      if (decl.isTypeLevel) types +:= decl.path
      decl
    }
  }
  
  /**
   * Parse the internal declarations of dd expanding previous definitions
   * @param dd the derived declaration whoose internal declarations to parse
   * @param con the controller
   * @param ctx (optional) a context to parse the declarations in
   * @param parent (implicit) path of the derived declaration over which we want to define a term
   */
  def parseInternalDeclarations(dd: DerivedDeclaration, con: Controller, ctx: Option[Context]): List[InternalDeclaration] = {
    val consts = dd.getDeclarations map {case c: Constant=> c case _ => throw GeneralError("unsupported declaration")}
    parseInternalDeclarationsSubstitutingDefiniens(consts, con, ctx)(dd.path)
  }
}

import StructuralFeatureUtils._

object TermConstructingFeatureUtil {
    /**
     * Fetches the constant internal declaration of given local name in the given derived declaration
     * @param dd the derived declaration in which to fetch the constant
     * @param d the name of the constant to fetch
     */
    def correspondingDecl(dd: DerivedDeclaration, d: LocalName): Option[Constant] = {
      dd.getO(d) map {
        case c: Constant=> c
        case dec => throw GeneralError("Expected a constant at "+dec.path+".")
      }
    }
    /**
   * Parse the internal declarations of dd expanding previous definitions
   * @param dd the derived declaration whoose internal declarations to parse
   * @param con the controller
   * @param ctx (optional) a context to parse the declarations in
   * @param parent path of the derived declaration over which we want to define a term
   */ 
    def parseInternalDeclarationsWithDefiniens(dd: DerivedDeclaration, con: Controller, ctx: Option[Context]): List[InternalDeclaration] = {
      val decls = parseInternalDeclarations(dd, con, ctx)
      decls.map(d => if (d.df.isEmpty) throw GeneralError("Unsupported corresponding declaration: Expected constant with definien at "+d.path))
      decls
    }
}
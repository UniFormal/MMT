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
  
  /**
   * parse the declarations into constants, flattening PlainIncludes
   * @param decls the declarations to parse
   * @param con the controller
   */
  def getConstants(decls: List[Declaration], con: Controller): List[Constant] = {
    decls.flatMap { d: Declaration =>
      d match {
        case c: Constant => List(c)
        case PlainInclude(from, to) =>
          val target: Option[StructuralElement] = con.getO(from)
          target match {
            case Some(refDD: DerivedDeclaration) => parseInternalDeclarationsIntoConstants(refDD, con)
            case Some(m: ModuleOrLink) => getConstants(m.getDeclarations, con)
            case Some(t) => throw GeneralError("unsupported include of" + t.path)
            case None => throw GeneralError("found empty include")
          }
        case _ => throw GeneralError("unsupported declaration")
      }
    }
  }

  /**
   * Reads in the given constants and parses them into Internal declarations.
   * It also will expand any references to previous declarations in terms of their definiens, if existent
   * This is useful for e.g. inductive definitions, inductive matches and inductive proofs
   * @param decls the declarations to parse
   * @param con the controller, needed to parse the delarations
   * @param ctx (optional) the (outer) context of the declarations
   * @param intDeclsO (optional) the internal declarations for which to provide definiens
   * @param parent the parent module of the given declarations
   * @param expectedTypeO (optional) a function computing the expected type of a given constant declaration
   */
  def readInternalDeclarationsSubstitutingDefiniens(decls: List[Constant], con: Controller, ctx: Option[Context] = None, intDeclsO: Option[List[InternalDeclaration]] = None)(implicit parent : GlobalName): List[InternalDeclaration] = {
    val context = ctx.getOrElse(Context.empty)
    var types: List[TypeLevel] = Nil

    val isConstr = intDeclsO map {
      intDecls =>
        val tpdecls = tpls(intDecls)

        val isConstrs = decls map {c=>
          val intC = intDecls.find(_.name == c.name).getOrElse(
            throw GeneralError("Definien for declaration "+c.name+" is missing. "))
          val isConstrC = intC match {
            case const : Constructor =>
              val intTpl = const.getTpl
              val tplC = decls.find(_.name == intTpl.name).get.path
              (true, Some(tplC))
            case _ => (false, None)
          } //intC.path.module match {case a ? (b / c) => (a ? b) ? c}//intC.externalTp(c.path.copy(name=intC.name))
          (c, isConstrC)
        }
        def someIsConstr(c: Constant): (Boolean, Option[GlobalName]) = {utils.listmap(isConstrs, c).getOrElse(
          throw ImplementationError("No definien for declaration "+c.path+" is recognized. "+"\nThe known definiens are "+isConstrs)
        )}
        someIsConstr _
    }

		val declsDefMap = decls.map(t => (t.path, ApplyGeneral(t.df.get, context.map(_.toTerm))))

    decls map {c =>
      val intDecl = fromConstant(c, con, types, ctx, isConstr map (_(c)))
      //def repDfs(df: GlobalName) = {utils.listmap(declsDef, df)}
      def repDfs(df: GlobalName) = {utils.listmap(decls.map(t => (t.path, ApplyGeneral(t.df.get, context.map(_.toTerm)))), df)}
      def mpDf(df: Term):Term = OMSReplacer(repDfs)(df, Context.empty)
      val repDf = intDecl.df map (mpDf)

      val decl = intDecl match {
        case constr: Constructor => new Constructor(constr.path, constr.args, constr.ret, constr.getTpl, repDf, constr.notC, constr.ctx)
        case out: OutgoingTermLevel => new OutgoingTermLevel(out.path, out.args, out.ret, repDf, out.notC, out.ctx)
        case d : TypeLevel => d.copy(df = repDf)
        case d : StatementLevel => d.copy(df = repDf)
        case _ => throw ImplementationError("invalid InternalDeclaration")
      }
      decl match {case tpl: TypeLevel => types +:= tpl case _ =>}
      decl
    }
  }

  /*/**
   * Reads in the given constants and parses them into Internal declarations.
   * It also will expand any references to previous declarations in terms of their definiens, if existent
   * @param decls the declarations to parse
   * @param con the controller, needed to parse the delarations
   * @param ctx (optional) the (outer) context of the declarations
   * @param isConstructor (optional) computes whether the declaration is a constructor and if so its typelevel
   * Needs to be given for constructors over defined typelevels
   * @precondition if isConstructor is given for a constant and its first part is true, the second part must be defined and contain the corresponding typelevel
   */
  def readInternalDeclarationsSubstitutingDefiniens(decls: List[Constant], con: Controller, ctx: Option[Context] = None, isConstructor: Option[Constant => (Boolean, Option[GlobalName])] = None)(implicit parent : GlobalName): List[InternalDeclaration] = {
    val context = ctx.getOrElse(Context.empty)
    var types: List[TypeLevel] = Nil

    decls map {c =>
      val intDecl = fromConstant(c, con, types, ctx, isConstructor map (_(c)))
      def repDfs(df: GlobalName) = {utils.listmap(decls.map(t => (t.path, ApplyGeneral(t.df.get, context.map(_.toTerm)))), df)}
      def mpDf(df: Term):Term = OMSReplacer(repDfs)(df, Context.empty)
      val repDf = intDecl.df map (mpDf)
      
      val decl = intDecl match {
          case constr: Constructor => new Constructor(constr.path, constr.args, constr.ret, constr.getTpl, repDf, constr.notC, constr.ctx)
          case out: OutgoingTermLevel => new OutgoingTermLevel(out.path, out.args, out.ret, repDf, out.notC, out.ctx)
          case d : TypeLevel => d.copy(df = repDf)
          case d : StatementLevel => d.copy(df = repDf)
          case _ => throw ImplementationError("invalid InternalDeclaration")
         }
      decl match {case tpl: TypeLevel => types +:= tpl case _ =>}
      decl
    }
  } */
  
  /**
   * Reads in the internal declarations of the given derived declaration and parses them into internal declarations.
   * It also will expand any references to previous declarations in terms of their definiens, if existent
   * @param dd the derived declaration whoose declarations to parse
   * @param con the controller, needed to parse the delarations
   * @param ctx (optional) the (outer) context of the declarations
   * @param intDecls (optional) the declarations for which to provide definiens
   */
  def parseInternalDeclarationsSubstitutingDefiniens(dd:DerivedDeclaration, con: Controller, ctx: Option[Context] = None, intDecls: Option[List[InternalDeclaration]] = None): List[InternalDeclaration] = {
    val sf = con.extman.get(classOf[StructuralFeature], dd.feature).getOrElse(throw GeneralError("Structural feature "+dd.feature+" not found."))
    val consts: List[Constant] = sf match {
      case rldd : ReferenceLikeTypedParametricTheoryLike => parseReferenceLikeDerivedDeclaration(dd, con, rldd)
      case _ => getConstants(dd.getDeclarations, con)
    }
    readInternalDeclarationsSubstitutingDefiniens(consts, con, ctx, intDecls)(dd.path)
  }

  /**
   * Parse the internal declarations of dd
   * @param dd the derived declaration whoose internal declarations to parse
   * @param con the controller
   * @param sf the structural feature of the derived declaration
   */
  def parseReferenceLikeDerivedDeclaration(dd: DerivedDeclaration, con:Controller, sf: ReferenceLikeTypedParametricTheoryLike) : List[Constant] = {
    getConstants(sf.getDecls(dd)._2, con)
  }

  /**
   * Parse the internal declarations of dd into constants unfolding includes
   * @param dd the derived declaration whoose internal declarations to parse
   * @param con the controller
   * @param ctx (optional) a context to parse the declarations in
   * @param isConstructor (optional) computes whether the declaration is a constructor and if so its typelevel
   * Needs to be given for constructors over defined typelevels
   * @precondition if isConstructor is given for a constant and its first part is true, the second part must be defined and contain the corresponding typelevel
   */
  def parseInternalDeclarationsIntoConstants(dd: DerivedDeclaration, con: Controller): List[Constant] = {
    val sf = con.extman.get(classOf[StructuralFeature], dd.feature).getOrElse(throw GeneralError("Structural feature "+dd.feature+" not found."))
    sf match {
      case rldd : ReferenceLikeTypedParametricTheoryLike => parseReferenceLikeDerivedDeclaration(dd, con, rldd)
      case _ => getConstants(dd.getDeclarations, con)
    }
  }

  /**
   * Parse the internal declarations of dd into internal declarations
   * @param dd the derived declaration whoose internal declarations to parse
   * @param con the controller
   * @param ctx (optional) a context to parse the declarations in
   * @param isConstructor (optional) computes whether the declaration is a constructor and if so its typelevel
   * Needs to be given for constructors over defined typelevels
   * @precondition if isConstructor is given for a constant and its first part is true, the second part must be defined and contain the corresponding typelevel
   */
  def parseInternalDeclarations(dd: DerivedDeclaration, con: Controller, ctx: Option[Context] = None, isConstructor: Option[Constant => (Boolean, Option[GlobalName])] = None): List[InternalDeclaration] = {
    val consts: List[Constant] = parseInternalDeclarationsIntoConstants(dd,con)
    readInternalDeclarations(consts, con, ctx, isConstructor)(dd.path)
  }
  
  def readInternalDeclarations(consts: List[Constant], con: Controller, ctx: Option[Context] = None, isConstructor: Option[Constant => (Boolean, Option[GlobalName])] = None)(implicit parent : GlobalName): List[InternalDeclaration] = {
    val context = ctx.getOrElse(Context.empty)
    var types: List[TypeLevel] = Nil
    
    consts map {c =>
      val intDecl = fromConstant(c, con, types, ctx, isConstructor map (_(c)))
      
      intDecl match {case tpl: TypeLevel => types +:= tpl case _ =>}
      intDecl
    }
  }
 
}

import StructuralFeatureUtils._
import info.kwarc.mmt.api.uom.ExtendedSimplificationEnvironment

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
   * @param isConstructor (optional) computes whether the declaration is a constructor and if so its typelevel
   * Needs to be given for constructors over defined typelevels
   * @precondition if isConstructor is given for a constant and its first part is true, the second part must be defined and contain the corresponding typelevel
   */ 
    def parseInternalDeclarationsWithDefiniens(dd: DerivedDeclaration, con: Controller, ctx: Option[Context], intDecls: Option[List[InternalDeclaration]] = None): List[InternalDeclaration] = {
      val decls = parseInternalDeclarationsSubstitutingDefiniens(dd, con, ctx, intDecls)
      decls.map(d => if (d.df.isEmpty) throw GeneralError("Unsupported corresponding declaration: Expected constant with definien at "+d.path))
      decls
    }
    
    /**
     * Simplify the given term in the given context (if specified)
     * @param tm the term to simplify
     * @param ctx (optional) the context in which to simplify the term
     * @param expDef (optional) whether to use definition expansions in the simplification process (by default not)
     */
    def simplify(tm: Term, ctx: Context = Context.empty, expDef: Boolean=false)(implicit env: Option[uom.ExtendedSimplificationEnvironment] = None) : Term = env match {
      case None => tm
      case Some(en) =>
        en.objectSimplifier.toTranslator(en.rules, expDef)(ctx, tm)
    }
}

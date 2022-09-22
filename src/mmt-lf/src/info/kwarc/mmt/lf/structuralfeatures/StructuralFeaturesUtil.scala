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

  /** To allow for more user friendly inductive-proof declarations
   * Takes a predicate pr: ⊦ x ≐ y for x: A, y: A, a (initial) function f: A → B and returns a predicate ⊦ f a ≐ f b
   * @param pr the predicate : ⊦ x ≐ y
   * @param func the function func : ⊦ A → B
   */
  def Cong(pr: VarDecl, func: VarDecl) : Term = {Cong(pr.tp.get, pr.toTerm, func.tp.get, func.toTerm)}
  
  /**
   * Takes a predicate pr: ⊦ x ≐ y for x: C → D, y: C → D and a term tm: C and returns a predicate ⊦ f x ≐ f y, where f x := x tm
   * @param prTp type of the predicate : ⊦ x ≐ y
   * @param prDf definien of the predicate : ⊦ x ≐ y
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
   * @param parent (implicit) the (new) parent module of all read constants
   */
  def getConstants(decls: List[Declaration], con: Controller)(implicit parent: MPath): List[Constant] = {
    def getFromInclusion(from:MPath) = {
      val target: Option[StructuralElement] = con.getO(from)
      target match {
        case Some(refDD: DerivedDeclaration) => parseInternalDeclarationsIntoConstants(refDD, con)
        case Some(m: ModuleOrLink) => getConstants(m.getDeclarations, con)
        case Some(t) => throw GeneralError("unsupported include of " + t.path)
        case None => throw GeneralError("found empty include")
      }
    }
    val consts:List[Constant] = decls.flatMap { (d: Declaration) =>
      d match {
        case c: Constant => List (c)
        case PlainInclude(from, to) => getFromInclusion(from)
        case Include(inclData) => getFromInclusion(inclData.from)
        case _ => throw GeneralError("unsupported declaration of feature "+d.feature)
      }
    }

//    println("The constant declarations whoose module paths to fix: "+consts)
//    println("The parent module path to set them to: "+parent)

    val names = consts map(_.name)
    val repl = OMSReplacer(g => names.find(_ == g.name).map(_=>OMS(g.copy(module = parent))))
    val tr = TraversingTranslator(repl)

    val tpCf = consts.map(c => c.tpC.map(tr(Context.empty, _)))
//    println("The fixed types of the constants: "+tpCf)
    consts map {c =>
      Constant.apply(OMMOD(parent.module), c.name, c.alias, c.tpC.map(tr(Context.empty, _)), c.dfC.map(tr(Context.empty, _)), c.rl, c.notC)
    }
  }

  /**
   * Parse the internal declarations of dd into constants unfolding includes
   * @param dd the derived declaration whoose internal declarations to parse
   * @param con the controller
   * @precondition if isConstructor is given for a constant and its first part is true, the second part must be defined and contain the corresponding typelevel
   */
  def parseInternalDeclarationsIntoConstants(dd: DerivedDeclaration, con: Controller): List[Constant] = {
    val sf = con.extman.get(classOf[StructuralFeature], dd.feature).getOrElse(throw GeneralError("Structural feature "+dd.feature+" not found."))
    val consts = sf match {
      case rldd : ReferenceLikeTypedParametricTheoryLike => parseReferenceLikeDerivedDeclaration(dd, con, rldd)
      case _ => getConstants(dd.getDeclarations, con)(dd.modulePath)
    }
    consts
  }

  /**
   * Parse the internal declarations of dd
   * @param dd the derived declaration whoose internal declarations to parse
   * @param con the controller
   * @param sf the structural feature of the derived declaration
   */
  def parseReferenceLikeDerivedDeclaration(dd: DerivedDeclaration, con:Controller, sf: ReferenceLikeTypedParametricTheoryLike) : List[Constant] = {
    getConstants(sf.getDecls(dd)._2, con)(dd.modulePath)
  }


  /**
   * Reads in the internal declarations of the given derived declaration and parses them into internal declarations.
   * It also will expand any references to previous declarations in terms of their definiens, if existent
   * @param dd the derived declaration whoose declarations to parse
   * @param con the controller, needed to parse the delarations
   * @param ctx (optional) the (outer) context of the declarations
   * @param intDeclsO (optional) the declarations for which to provide definiens
   */
  def parseInternalDeclarationsSubstitutingDefiniens(dd:DerivedDeclaration, con: Controller, ctx: Option[Context] = None, intDeclsO: Option[List[InternalDeclaration]] = None): List[InternalDeclaration] = {
    val consts = parseInternalDeclarationsIntoConstants(dd, con)
    readInternalDeclarationsSubstitutingDefiniens(consts, con, ctx, intDeclsO)
  }

  /**
   * Reads in the given constants and parses them into Internal declarations.
   * It also will expand any references to previous declarations in terms of their definiens, if existent
   * This is useful for e.g. inductive definitions, inductive matches and inductive proofs
   * @param decls the declarations to parse
   * @param con the controller, needed to parse the declarations
   * @param ctx (optional) the (outer) context of the declarations
   * @param intDeclsO (optional) the internal declarations for which to provide definiens
   */
  def readInternalDeclarationsSubstitutingDefiniens(decls: List[Constant], con: Controller, ctx: Option[Context] = None, intDeclsO: Option[List[InternalDeclaration]] = None): List[InternalDeclaration] = {
    val context = ctx.getOrElse(Context.empty)
    var types: List[TypeLevel] = Nil
    val isConstr = isConstructor(decls, intDeclsO)

    decls map {c =>
      val intDecl = fromConstant(c, con, types, ctx, isConstr map (_(c)))
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

  /**
   * Construct a map computing for each declaration whether it is a constructor and if so its typelevel
   * @param decls the declarations to check
   * @param intDecls the derived declarations for which to provide definiens
   * @return A function Constant => (Boolean, Option[GlobalName])
   *         , defined by c |-> (true, Some(p)) if c is constructor of type OMS(p)
   *                  and c |-> (false, None)   if c is not a constructor
   *         The function throws an error if there is no corresponding declaration (i.e. of same name) to c in intDecls
   */
  def isConstructor(decls: List[Constant], intDecls: List[InternalDeclaration]) : Constant => (Boolean, Option[GlobalName]) = {
    val isConstrO:List[(Constant, (Boolean, Option[GlobalName]))] = decls map {c =>
      val intC = intDecls.find(_.name == c.name).getOrElse(
        throw GeneralError("Definien for declaration "+c.name+" is missing. \nOnly found the declarations:\n"+intDecls.map(_.name)))
      intC match {
        case const : Constructor => (c, (true, Some(decls.find(_.name == const.getTpl.name).get.path)))
        case _ => (c, (false, None))
      }
    }
    utils.listmap(isConstrO, _ : Constant).get
  }


  /**
   * Construct a map computing for each declaration whether it is a constructor and if so its typelevel
   *
   * @param decls the declarations to check
   * @param intDeclsO (optional) the derived declarations for which to provide definiens
   * @return if intDeclsO is given, then return Some function Constant => (Boolean, Option[GlobalName]),
   *         defined by c |-> (true, Some(p)) if c is constructor of type OMS(p)
   *         *      and c |-> (false, None) if c is not a constructor
   *         The function throws an error if there is no corresponding declaration (i.e. of same name) to c in intDecls
   */
  def isConstructor(decls: List[Constant], intDeclsO: Option[List[InternalDeclaration]] = None) : Option[Constant => (Boolean, Option[GlobalName])] = {
    intDeclsO map { intDecls => isConstructor(decls, intDecls) }
  }

  /**
   * Parse the internal declarations of dd into internal declarations
   * @param dd the derived declaration whoose internal declarations to parse
   * @param con the controller
   * @param ctx (optional) a context to parse the declarations in
   * @param intDeclsO (optional) the derived declarations for which to provide definiens
   * @precondition if isConstructor is given for a constant and its first part is true, the second part must be defined and contain the corresponding typelevel
   */
  def parseInternalDeclarations(dd: DerivedDeclaration, con: Controller, ctx: Option[Context] = None, intDeclsO: Option[List[InternalDeclaration]] = None): List[InternalDeclaration] = {
    val consts = parseInternalDeclarationsIntoConstants(dd,con)
    readInternalDeclarations(consts, con, ctx, intDeclsO)(dd.path)
  }

  /**
   * parse the given constants into internal declarations
   * the parent modules of all internal declarations is set to the mpath of the parsed derived declaration
   * @param consts the constants to parse
   * @param con the controller
   * @param ctx (optional) the context of the declarations
   * @param intDeclsO (optional) the derived declarations for which to provide definiens
   * @param parent (implicit) the path of the derived declaration that is parsed
   * @return a list of internal declarations containing the parsed constants
   */
  def readInternalDeclarations(consts: List[Constant], con: Controller, ctx: Option[Context] = None, intDeclsO: Option[List[InternalDeclaration]] = None)(implicit parent: GlobalName): List[InternalDeclaration] = {
    val context = ctx.getOrElse(Context.empty)
    var types: List[TypeLevel] = Nil
    
    consts map {c =>
      val intDecl = fromConstant(c, con, types, ctx, isConstructor(consts, intDeclsO).map(_(c)))
      intDecl match {case tpl: TypeLevel => types +:= tpl case _ =>}
      intDecl
    }
  }

  def externalName(parent: GlobalName, name: LocalName): GlobalName = //(parent.module / parent.name, name)
    parent.module ? parent.name / name
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
   * @param intDecls (optional) the internal declarations for which to parse the definiens
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

package info.kwarc.mmt.lf.induction

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
  def uniqueLN(nm: String)(implicit parent: OMID): LocalName = {
    LocalName(nm)
	 ////In case it is applied multiple times on the same object, change it only the first time
    //if (nm contains parent.path.name.toString.filterNot(_ == '/')) LocalName(nm) else parent.path.name / nm
  }
   
  /**
    * Make a new variable declaration for a variable of type tp and fresh name (preferably name) in the given (optional) context
    * @param name the local name that will preferably be picked for the variable declaration
    * @param tp the type of the variable
    * @param the context in which to pick the a fresh variable (optional)
    * @note postcondition: the returned variable is free in the given context
    */
   def newVar(name: LocalName, tp: Term, con: Option[Context] = None)(implicit parent: OMID) : VarDecl = {
     val c = con.getOrElse(Context.empty)
     val (freshName,_) = Context.pickFresh(c, uniqueLN(name.toString()))
     VarDecl(freshName, tp)
   }

  val theory: MPath = LF._base ? "Inductive"
  object Eq extends BinaryLFConstantScala(theory, "eq")
  object Neq extends BinaryLFConstantScala(theory, "neq")
  
  val Contra = OMS(theory ? "contra") 
  
  // TODO: Fix this hack
  def compTp(a: Term, b: Term): Boolean = a.toStr(true) == b.toStr(true)
  
  /**
   * get the induction axiom declarations for the type levels as well as the term level and constructor level part 
   * of the morphism mapping an (term or statement level) internal declaration to its image in the model
   * @param decls All internal declarations
   * @param tpdecls All type level internal declarations
   * @param context the inner context of the derived declaration
   */
  def getFullMorph(decls : List[InternalDeclaration], tpdecls: List[TypeLevel], context: Context)(implicit parent : OMID) : (List[Constant], InternalDeclaration => InternalDeclaration, VarDecl => Term, List[InternalDeclaration]) = {
    //A list of quadruples consisting of (context of arguments, x, x_, the sub replacing x with a free type x_) for each type declaration declaring a type x
    val types : List[(Context, VarDecl, (Term, Term))]= tpdecls map(_.getTypes(None))
    
    /** The morphism on constructors, it map c |-> c' for each constructor c */
    def mapConstr(con: InternalDeclaration): InternalDeclaration = con ^ (types.map(_._3))
    val chain : List[InternalDeclaration]= decls map mapConstr
    
    /** The morphism for terms, it maps all terms t: a = m |-> t' : a* = m' iff a is one of the earlier inductively defined types */
    def mapTerm(morphisms: List[(VarDecl, Constant, Term => Term)], tm: VarDecl) : Term = {
      morphisms.find(x => compTp(x._1.toTerm, tm.tp.get)) match {
        case Some(morph) => morph._3(tm.toTerm)
        case None => toOMS(tm)
      }
    }
    
    var morphisms : List[(VarDecl, Constant, Term => Term)] = Nil
    tpdecls zip types foreach {
      case (t:TypeLevel, (c, x, s)) => morphisms = morphisms :+ t.getMorph(t, chain, mapTerm (morphisms, _), mapConstr, c, x, context)
    }
    
    /** Now the declarations for the computation axioms */
    val tp_induct_decls = morphisms map(_._2)
    
    (tp_induct_decls, mapConstr, mapTerm (morphisms, _), chain)
  }
  def toOMS(vd: VarDecl)(implicit parent : OMID) : Term = OMS(GlobalName(parent.path.toMPath, vd.name))
  def toOMS(tp: Term)(implicit parent: OMID) : Term = OMS(Constant(parent, uniqueLN(tp.toStr(true)), Nil, Some(tp), None, None).path)
}

import InternalDeclarationUtil._
import info.kwarc.mmt.api.utils.URI
//import info.kwarc.mmt.quotation.Quotation.quote

/** helper class for the various declarations in an inductive type */ 
sealed abstract class InternalDeclaration {
  def path: GlobalName
  def name = path.name
  def args: List[(Option[LocalName], Term)]
  def ret: Term
  def notation: NotationContainer
  def tp = FunType(args, ret)
  def df : Option[Term]

  /**
	 * Build a context quantifying over all arguments
	 *  and the variable declaration for the result of this constructor applied to those arguments
	 * @param suffix (optional) a suffix to append to the local name of each variable declaration in the returned context
   */
  def argContext(suffix: Option[String])(implicit parent: OMID): (Context, VarDecl) = { 
    val suf = suffix getOrElse ""
    val dargs= args.zipWithIndex map {
      case ((Some(loc), arg), _) => (loc, arg)
      case ((None, arg), i) => (uniqueLN("x_"+i.toString()), arg)
    }
    val con = dargs map {case (loc, tp) =>
      val n = uniqueLN(loc.toString()+suf)
      newVar(n, tp, None)
    }
    val tp = ApplyGeneral(OMS(path), con.map(_.toTerm))
    val newName = if (tp == OMS(path)) name else {
      val appliedName = name+suf+" ("+con.foldLeft("")((a, b)=>a+" "+b.name)+")"//+"_res"
      if (con == Nil) uniqueLN(name+suf) else uniqueLN(appliedName)}
    (con, VarDecl(newName, None, Some(tp), None, None))
  }
    /**
     * Substitute the argument types of the inductive declaration according to the substitution sub
     * @param sub the substitution to apply
     * @param con the controller
     */
  def ^(sub : List[(Term, Term)])(implicit parent : OMID) : InternalDeclaration = {
    //For some reason substitutions don't seem to work properly here, hence this reinvention of the wheel
    def substitute(t: Term) : Term = {
        var res = t
        sub. foreach {case (a, b) =>
          if (compTp(a, t)) {
            res = b}
        }
        res
      }
    def subst (tp: Term) : Term = {
      def subs(tp: Term) : Term = {
        val FunType(ars, ret) = tp 
        FunType(ars map {case (x, y) => (x, substitute(y))}, substitute (tp))//substitute (ret))
      }
      var res = tp
      while (res != subs(tp)) {res = subs(res)}
      res
    }
    
    val subArgs : List[(Option[LocalName], Term)]= args map {
      case a @ (Some(loc), tp) => (Some(LocalName(loc.toString() + "'")), subst (tp))
      case (None, tp) => (None, subst (tp))
    }
    val p = path.module ? LocalName(path.name+"'")
    this match {
      case TermLevel(_, _, _, df, n) => TermLevel(p, subArgs, subst (ret), df, n)
      case TypeLevel(_, _, df, n) => TypeLevel(p, subArgs, df, n)
      case StatementLevel(_, _, df, n) => StatementLevel(p, subArgs, df, n)
    }
    }

  def noJunk(chain:Context, mapConstr: InternalDeclaration=>InternalDeclaration, mapTerm: VarDecl=>Term)(implicit parent: OMID) : Term = {
    val (args, orig) = this.argContext(None)
        
    //the result of applying m to the result of the constructor
    //redeclaring the type to preserve matches in mapTerm, despite more complicated type structure of orig
    //TODO: Can this hack be avoided
    val mappedOrig = mapTerm(VarDecl(orig.name, None, Some(ret), orig.df, None))
    
    //the result of applying the image of the constructor (all types substituted) to the image of the arguments
    val mappedArgs : Term = ApplyGeneral(mapConstr(this).toVarDecl.toTerm, args map mapTerm)
    
    //both results should be equal, if m is actually a homomorphism
    def assertion(assert:(Term, Term) => Term) = Pi(chain ++ args, assert(mappedOrig, mappedArgs))
    //Use Arrow or Eq depending on whether the results are term or type level
    this match {
      case TypeLevel(_, _, _, _) => assertion ((x, y) => Arrow(x, y))
      case _ => assertion ((x, y) => Eq(x, y))
    }
  }
  
  def toVarDecl : VarDecl = VarDecl(this.name, this.tp, OMS(this.path))
  def toConstant(implicit parent : OMID) : Constant = Constant(parent, name, Nil, Some(tp), df, None, notation)
}

object InternalDeclaration {
  implicit def tp(d: InternalDeclaration): Term = d.tp
  implicit def tm(d: InternalDeclaration): Option[Term] = d.df
  implicit def toVarDecl(d: InternalDeclaration): VarDecl = d.toVarDecl

  //TODO: Implement this properly
  def isTypeLevel(tp: Term) : Boolean = false
  
  /**
   * convert the given constant into the appropriate internal declaration
   * @param c the constant to convert
   * @param con the controller
   */
  def fromConstant(c: Constant, con: Controller)(implicit parent : OMID) : InternalDeclaration = {
    val tp = c.tp getOrElse {throw InvalidElement(c, "missing type")}        
      val FunType(args, ret) = tp
      val p = parent.path.module ? c.path.last
        if (JudgmentTypes.isJudgment(ret)(con.globalLookup)) {
        StatementLevel(p, args, c.df, c.notC)
      } else {
      ret match {
        case Univ(1) => TypeLevel(p, args, c.df, c.notC)
        case Univ(x) if x != 1 => throw ImplementationError("unsupported universe")
        case r => if (isTypeLevel(r)) TypeLevel(p, args, c.df, c.notC) else TermLevel(p, args, ret, c.df, c.notC) }
      }
  }
  
   /**
   * Generate no junk declaration for all term-- and typelevel inductive declarations
   * @param parent the parent declared module of the derived declaration to elaborate
   * @param decls all declarations
   * @param tmdecls all term level declarations
   * @param tpdecls all type level declarations
   * @param con the controller
   * @param context the inner context of the derived declaration
   * @returns returns one no junk (morphism) declaration for each type level declaration
   * then generates all the corresponding no junk declarations for the termlevel constructors of each declared type
   */    
  def noJunks(decls : List[InternalDeclaration], tpdecls: List[TypeLevel], tmdecls: List[TermLevel], statdecls: List[StatementLevel], context: Context)(implicit parent : OMID) : List[Constant] = {
    var derived_decls:List[Constant] = Nil
    val (tp_induct_decls, mapConstr, mapTerm, chain) = getFullMorph(decls, tpdecls, context)
    derived_decls ++=tp_induct_decls
    
    (tmdecls++statdecls) foreach {decl : InternalDeclaration => 
          val assertion = decl.noJunk(chain.map(_.toVarDecl), mapConstr, mapTerm)
          derived_decls :+= Constant(parent, uniqueLN("compute_"+decl.name.last), Nil, Some(assertion), None, None)
    }
    derived_decls
  } 
}

/** type declaration */
case class TypeLevel(path: GlobalName, args: List[(Option[LocalName], Term)], df: Option[Term], notC : NotationContainer)(implicit parent : OMID) extends InternalDeclaration {
  def ret = Univ(1)
  def tm = df
  def notation = notC
  def getTypes(suffix: Option[String]) : (Context, VarDecl, (Term, Term)) = {
    val (argsCon, x)=this.argContext(suffix)
    //TODO: Define in terms of Quotations, fix below try
    //val a = VarDecl(uniqueLN("quoted_return_type_of_"+x.name), None, Some(Univ(1)), Some(quote(x.toTerm, Nil)), None)
    val a = VarDecl(uniqueLN(x.name+"'"), None, Some(Univ(1)), None, None)
    (argsCon, x, (x.toTerm, a.toTerm))
  }
  
  /** 
   * Generate the domain type, the morphism declaration and the actual morphism for a type level declaration
   * @param chain all internal declarations
   * @param decls ctx the argument context of the type level declaration
   * @param x the declaration of the resulting type of the type level declaration
   * @param a the declaration of the resulting type of the image of this type level declaration
   * @param the substitution from the resulting type of this declaration, to the resulting type of its image
   * @param context the inner context of the derived declaration
   * @returns returns the domain type, the morphism declaration and the morphism itself
   */    
  def getMorph(t: InternalDeclaration, chain : List[InternalDeclaration], mapTerm: VarDecl => Term, mapConstr: InternalDeclaration => InternalDeclaration, ctx: Context, x: VarDecl, context: Context) = {
    val morphType : Term = t.noJunk(chain.map(_.toVarDecl)++context, mapConstr, mapTerm)
    // TODO: Define the morphism in terms of quotations
    val morphTerm : Option[Term] = None
    val morph = VarDecl(uniqueLN("induct_"+x.name), None, Some(morphType), morphTerm, None)
    val morphDecl = Constant(parent, morph.name, Nil, Some(toOMS(morph)), morph.df, None) //morph.toDeclaration(parent)
    def m(tm : Term) = {
      val tp = ApplyGeneral(morph.toTerm, chain.map(_.tp) ++ ctx.map(_.toTerm) :+ tm)
      OMS(Constant(parent, uniqueLN(morph.name+" M "+tm.toStr(true)), Nil, Some(tp), morph.df, None).path)
    }
    (x, morphDecl, m _)
  }
}

/** term constructor declaration */
case class TermLevel(path: GlobalName, args: List[(Option[LocalName], Term)], ret: Term, df: Option[Term], notC: NotationContainer)(implicit parent : OMID) extends InternalDeclaration {
  def tm = df
  def notation = notC
  /**
   * Generate injectivity declaration for the term constructor d
   * @param parent the parent declared module of the derived declaration to elaborate
   * @param d the term level for which to generate the injectivity axiom
   */
  def injDecl(context: Context)(implicit parent : OMID) : Constant = {
    val (aCtx, aApplied) = argContext(Some("_0"))
    val (bCtx, bApplied) = argContext(Some("_1"))
    
    val argEq = (aCtx zip bCtx) map {case (a,b) => Eq(a.toTerm,b.toTerm)}
    val resEq = Eq(toOMS(aApplied), toOMS(bApplied))
    val body = Arrow(Arrow(argEq, Contra), Arrow(resEq, Contra))
    val inj = Pi(context++aCtx ++ bCtx,  body)
    
    Constant(parent, uniqueLN("injective_"+name.last.toString), Nil, Some(inj), None, None)
  }
   
  /**
   * Generate no confusion/injectivity declaration for term constructor d and all term constructors of the same return type
   * @param parent the parent declared module of the derived declaration to elaborate
   * @param d the term level declaration for which to generate the no confusion declarations
   * @param tmdecls all term level declarations
   * @param context the inner context of the derived declaration
   */
  def noConf(tmdecls: List[TermLevel], context: Context)(implicit parent : OMID) : List[Constant] = {
  var decls:List[Constant] = Nil
  tmdecls.filter(_.ret == ret) foreach {b => 
    if (b == this) {
      if(args.length > 0)
        decls :+= injDecl(context)
      } else {
        val newName = uniqueLN("no_conf_" + name.last+ "_" + b.name.last)
        val (aCtx, aApplied) = argContext(None)
        val (bCtx, bApplied) = b.argContext(None)
        val tp = Pi(context++aCtx ++ bCtx, Neq(toOMS(aApplied), toOMS(bApplied)))
        decls :+= Constant(parent, newName, Nil, Some(tp), None, None)
    }
  }
  decls
  }
}

object TermLevel {
  /**
   * generate the no confusion axioms for the list of term level internal declarations
   * @param tmdecs the list of term levels
   * @param context the inner context of the derived declaration
   * @param parent the parent term (needed to generate unique local names)
   */
  def noConfs(tmdecs : List[TermLevel], context: Context)(implicit parent : OMID) : List[Constant] = tmdecs match {
    case Nil => Nil
    case l @ hd::tl => hd.noConf(l, context)++noConfs(tl, context)
  }
}

/** Rules and Judgment constructors */
case class StatementLevel(path: GlobalName, args: List[(Option[LocalName], Term)], df: Option[Term], notC: NotationContainer)(implicit parent : OMID) extends InternalDeclaration {
  def ret = Univ(1)
  def tm = df
  def notation = notC
}

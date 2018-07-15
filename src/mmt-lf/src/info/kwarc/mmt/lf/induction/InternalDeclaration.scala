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
    //In case it is applied multiple times on the same object, change it only the first time
    if (nm contains parent.path.name.toString.filterNot(_ == '/')) LocalName(nm) else parent.path.name / nm
  }
   
  /**
    * Make a new variable declaration for a variable of type tp and fresh name (preferably name) in the given (optional) context
    * @param name the local name that will preferably be picked for the variable declaration
    * @param tp the type of the variable
    * @param the context in which to pick the a fresh variable (optional)
    * @note postcondition: the returned variable is free in the given context
    */
   def newVar(name: LocalName, tp: Term, con: Option[Context] = None)(implicit parent: OMID) : VarDecl = {
     try {
      val c = con.getOrElse(Context.empty)
      val (freshName,_) = Context.pickFresh(c, uniqueLN(name.toString()))
      VarDecl(freshName, tp)
     } catch {
       case e : Throwable => println("Error in newVar:"+e.getMessage); throw e
     }
   }

  val theory: MPath = LF._base ? "Inductive"
  object Eq extends BinaryLFConstantScala(theory, "eq")
  object Neq extends BinaryLFConstantScala(theory, "neq")
  
  val Contra = OMS(theory ? "contra") 
  
  // TODO: Fix this hack
  def compTp(a: Term, b: Term, con: Controller): Boolean = con.presenter.asString(a) == con.presenter.asString(b)
  
  /**
   * get the induction axiom declarations for the type levels as well as the term level and constructor level part 
   * of the morphism mapping an (term or statement level) internal declaration to its image in the model
   * @param decls All internal declarations
   * @param tpdecls All type level internal declarations
   * @param cont the controller
   * @param context the inner context of the derived declaration
   */
  def getFullMorph(decls : List[InternalDeclaration], tpdecls: List[TypeLevel], cont: Controller, context: Context)(implicit parent : OMID) : (List[Constant], InternalDeclaration => InternalDeclaration, VarDecl => Term, List[InternalDeclaration]) = {
    //A list of quadruples consisting of (context of arguments, x, x_, the sub replacing x with a free type x_) for each type declaration declaring a type x
    val types : List[(Context, VarDecl, VarDecl, (Term, Term))]= tpdecls map(_.getTypes(None))
    
    def mapConstr(con: InternalDeclaration): InternalDeclaration = con ^ (types.map(_._4), cont)
    val chain : List[InternalDeclaration]= decls map mapConstr
    
    /** The morphisms x=>x' for each defined type x */
    val morphisms = tpdecls zip types map {case (t:TypeLevel, (c, x, x_, s)) => t.getMorph(chain, c, x, x_, s, context)}
    val tp_induct_decls = morphisms map(_._2)
    
    /** Now the declarations for the computation axioms */
    def M(tm: VarDecl) : Term = {
      morphisms.find(x => compTp(x._1.toTerm, tm.tp.get, cont)) match {
        case Some(morph) => morph._3(tm.toTerm)
        case None => tm.toTerm
      }
    }
    (tp_induct_decls, mapConstr, M, chain)
  }
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
    val con : List[VarDecl] = dargs map {case (loc, tp) =>
      val n = uniqueLN(loc.toString()+suf)
      newVar(n, tp, None)
    }
    val tp = ApplyGeneral(OMS(path), con.map(_.toTerm))
    val newName = if (tp == OMS(path)) name else uniqueLN(name+"_res"+suf)
    (con, VarDecl(newName, None, Some(tp), None, None))
  }
    /**
     * Substitute the argument types of the inductive declaration according to the substitution sub
     * @param sub the substitution to apply
     * @param con the controller
     */
  def ^(sub : List[(Term, Term)], con: Controller)(implicit parent : OMID) : InternalDeclaration = {
    //For some reason substitutions don't seem to work properly here, hence this reinvention of the wheel
    def substitute(t: Term) : Term = {
        var res = t
        sub. foreach {case (a, b) =>
          if (compTp(a, t, con)) {
            res = b}
        }
        res
      }
    def subst (tp: Term) : Term = {
      def subs(tp: Term) : Term = {
        val FunType(ars, ret) = tp 
        FunType(ars map {case (x, y) => (x, substitute(y))}, substitute (ret))
      }
      var res = tp
      while (res != subs(tp)) {res = subs(res)}
      res
    }
    
    val subArgs : List[(Option[LocalName], Term)]= args map {
      case a @ (Some(loc), tp) => (Some(LocalName(loc.toString() + "'")), subst (tp))
      case (None, tp) => (None, subst (tp))
    }
    this match {
      case TermLevel(_, _, _, df, n) => TermLevel(path.module.?(name.toString()+"'"), subArgs, subst (ret), df, n)
      case TypeLevel(_, _, df, n) => TypeLevel(path.module.?(name.toString()+"'"), subArgs, df, n)
      case StatementLevel(_, _, df, n) => StatementLevel(path.module.?(name.toString()+"'"), subArgs, df, n)
    }
    
  }
  
  def toVarDecl : VarDecl = VarDecl(this.name, this, OMS(this.path))
  def toConstant(implicit parent : OMID) : Constant = Constant(parent, name, Nil, Some(tp), df, None, notation)
}

object InternalDeclaration {
  implicit def tp(d: InternalDeclaration): Term = FunType(d.args,d.ret)
  implicit def tm(d: InternalDeclaration): Option[Term] = d.df
  implicit def toVarDecl(d: InternalDeclaration): VarDecl = VarDecl(d.name, d)

  /**
   * convert the given constant into the appropriate internal declaration
   * @param c the constant to convert
   * @param con the controller
   */
  def fromConstant(c: Constant, con: Controller)(implicit parent : OMID) : InternalDeclaration = {
    val tp = c.tp getOrElse {throw InvalidElement(c, "missing type")}        
      val FunType(args, ret) = tp
      if (JudgmentTypes.isJudgment(ret)(con.globalLookup)) {
        StatementLevel(c.path.module ? parent.path.name / c.path.name, args, c.df, c.notC)
      } else {
      ret match {
        case Univ(1) => TypeLevel(c.path.module ? parent.path.name / c.path.name, args, c.df, c.notC)
        case Univ(x) if x != 1 => throw ImplementationError("unsupported universe")
        case r =>
          //TODO: Check r:type
          TermLevel(c.path.module ? parent.path.name / c.path.name, args, r, c.df, c.notC)
        }
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
  def noJunk(decls : List[InternalDeclaration], tpdecls: List[TypeLevel], tmdecls: List[TermLevel], statdecls: List[StatementLevel], con: Controller, context: Context)(implicit parent : OMID) : List[Constant] = {
    var derived_decls:List[Constant] = Nil
    val (tp_induct_decls, mapConstr, mapTerm, chain) = getFullMorph(decls, tpdecls, con, context)
    derived_decls ++=tp_induct_decls
    
    (tmdecls++statdecls).reverse foreach {decl : InternalDeclaration => 
          val (args, orig) = decl.argContext(None)
            
          //the result of applying m to the result of the constructor
          //redeclaring the type to preserve matches in mapTerm, despite more complicated type structure of orig
          //TODO: Can this hack be avoided
          val mappedOrig = mapTerm(VarDecl(orig.name, None, Some(decl.ret), orig.df, None))
          
          //the result of applying the image of the constructor (all types substituted) to the image of the arguments
          val mappedArgs : Term = ApplyGeneral(mapConstr(decl).toVarDecl.toTerm, args map mapTerm)
          
          //both results should be equal, if m is actually a homomorphism
          val assertion = Pi(context++chain.map (_.toVarDecl) ++ args, Eq(mappedOrig, mappedArgs))
          derived_decls :+= Constant(parent, uniqueLN("compute_"+decl.name), Nil, Some(assertion), None, None)
    }
    derived_decls
  } 
}

/** type declaration */
case class TypeLevel(path: GlobalName, args: List[(Option[LocalName], Term)], df: Option[Term], notC : NotationContainer)(implicit parent : OMID) extends InternalDeclaration {
  def ret = Univ(1)
  def tm = df
  def notation = notC
  def getTypes(suffix: Option[String]) : (Context, VarDecl, VarDecl, (Term, Term)) = {
    val (argsCon, x)=this.argContext(suffix)
    //TODO: Define in terms of Quotations, fix below try
    //val a = VarDecl(uniqueLN("quoted_return_type_of_"+x.name), None, Some(Univ(1)), Some(quote(x.toTerm, Nil)), None)
    val a = VarDecl(uniqueLN(x.name+"*"), None, Some(Univ(1)), None, None)
    (argsCon, x, a, (x.toTerm, a.toTerm))
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
  def getMorph(chain : List[InternalDeclaration], ctx: Context, x: VarDecl, a: VarDecl, sub: (Term, Term), context: Context) = {
    val morphType : Term = Pi(context++chain.map(_.toVarDecl)++ctx.++(x), a.toTerm)
    // TODO: Define the morphism in terms of quotations
    val morphTerm : Option[Term] = None
    val morph = VarDecl(uniqueLN("induct_"+x.name), None, Some(morphType), morphTerm, None)
    val morphDecl = Constant(parent, morph.name, Nil, morph.tp, morph.df, None) //morph.toDeclaration(parent)
    def m(tm : Term) = ApplyGeneral(morph.toTerm, chain.map(_.toTerm) ++ ctx.map(_.toTerm) :+ tm)
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
    val resEq = Eq(aApplied.toTerm, bApplied.toTerm)
    val body = Arrow(Arrow(argEq, Contra), Arrow(resEq, Contra))
    val inj = Pi(context++aCtx ++ bCtx,  body)
    
    Constant(parent, uniqueLN("injective_"+name.toString), Nil, Some(inj), None, None)
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
        val newName = uniqueLN("no_conf_" + name+ "_" + b.name)
        val (aCtx, aApplied) = argContext(None)
        val (bCtx, bApplied) = b.argContext(None)
        val tp = Pi(context++aCtx ++ bCtx, Neq(aApplied.toTerm, bApplied.toTerm))
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
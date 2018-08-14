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
  def uniqueLN(nm: String)(implicit parent: OMID): LocalName = {
    LocalName(nm)
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
   
   //much nicer to read output than the one by controller.presenter.asString
  def present(c: Constant)(implicit parent: OMID) : String = {
    c.path.name + ": " + preTp (c.tp.get)
  }
  def preTp(e: Term)(implicit parent: OMID) : String = {
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
  }   

  val theory: MPath = LF._base ? "Inductive"
  object Eq extends BinaryLFConstantScala(theory, "eq")
  object Neq extends BinaryLFConstantScala(theory, "neq")
  
  val Contra = OMS(theory ? "contra") 
  
  // TODO: Fix this hack
  def compTp(a: Term, b: Term): Boolean = a.toStr(true) == b.toStr(true)

  def toOMS(vd: VarDecl)(implicit parent : OMID) : Term = OMS(GlobalName(parent.path.toMPath, vd.name))
  def toOMS(tp: Term)(implicit parent: OMID) : Term = OMS(Constant(parent, uniqueLN(tp.toStr(true)), Nil, Some(tp), None, None).path)
}

import InternalDeclarationUtil._
import info.kwarc.mmt.api.utils.URI
import scala.util.Random
import java.util.Random
import scala.util.Random

//import info.kwarc.mmt.quotation.Quotation.quote

/** helper class for the various declarations in an inductive type */ 
sealed abstract class InternalDeclaration {
  def path: GlobalName
  def name = path.name
  def args: List[(Option[LocalName], Term)]
  def ret: Term
  def notation: NotationContainer
  def tp(implicit parent: OMID) = FunType(args, ret)
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
    (con, VarDecl(newName, None, Some(ret), None, None))
  }
  
  /** apply the internal declaration to the given argument context */
  def applied(args: Context)(implicit parent: OMID):Term = toOMS(ApplyGeneral(OMS(path), args map toOMS))
  def applyTo(args: List[Term])(implicit parent: OMID):Term = applied(args map {x => OMV(toOMS(x) match {case OMS(p) => p.name}) % x})
  
  def ^^(sub: Substitution)(implicit parent: OMID) : InternalDeclaration = {
    def toOMV(tp: Term)(implicit parent: OMID): Term = toOMS(tp) match {case OMS(p) => OMV(p.name)}
    def mapTm(tp:Term) = if ((toOMV(tp)^sub) != toOMV(tp)) toOMV(tp) ^sub else tp
    val margs = args map({case (a, x) => (a, mapTm(x))})
    val mtp = FunType(margs, toOMV(ret)^sub)
    this match {
      case TermLevel(_, _, _, df, n) => TermLevel(path, margs, toOMV(ret)^sub, df, n)
      case TypeLevel(_, _, df, n) => TypeLevel(path, margs, df, n)
      case StatementLevel(_, _, df, n) => StatementLevel(path, margs, df, n)
    }
  }
  
  def rename(newName: LocalName)(implicit parent: OMID) : InternalDeclaration = {
    val p = path.module ? newName
    this match {
      case TermLevel(_, _, _, _, n) => TermLevel(p, args, ret, df, n)
      case TypeLevel(_, _, _, n) => TypeLevel(p, args, df, n)
      case StatementLevel(_, _, _, n) => StatementLevel(p, args, df, n)
    }
  }
    
  /**
   * Substitute the argument types of the inductive declaration according to the substitution sub
   * @param sub the substitution to apply
   */
  def ^(sub: Substitution)(implicit parent: OMID) : InternalDeclaration = {
    this ^^ sub rename(uniqueLN(this.name+"'"))
  }
  
  /**
   * generate the type of the no junk axiom for the internal declaration
   * @param chain all internal declarations
   * @param ctx the context of the axiom
   * @param mapConstr the declaration level part of the morphism
   * @param mapTerm the term devel part of the morphism
   * @param parent (implicit) the parent module
   * @return the term containing the type of the no junk axiom
   */
  def noJunk(chain:List[InternalDeclaration], ctx:Context, mapConstr: InternalDeclaration=>InternalDeclaration, mapTerm: VarDecl=>VarDecl)(implicit parent: OMID) : Term = {
    val (args, orig) = this.argContext(None)
    
    //the result of applying m to the result of the constructor
    val mappedRes = toOMS(mapTerm(orig))
    
    //the result of applying the image of the constructor (all types substituted) to the image of the arguments
    val mappedArgs : List[VarDecl] = args map mapTerm
    //ensure everything went well and the argument is actually in the context
    val mappedConstr : Term = mapConstr(this).applied(mappedArgs)
    
    //both results should be equal, if m is actually a morphism
    def assertion(assert:(Term, Term) => Term) = Pi(chain.map(_.toVarDecl) ++ ctx ++ args, assert(mappedRes, mappedConstr))
    //Use Arrow or Eq depending on whether the internal declaration is term or type level
    this match {
      case TypeLevel(_, _, _, _) => assertion ((x, y) => Arrow(x, y))
      case _ => assertion ((x, y) => Eq(x, y))
    }
  }
  
  def toVarDecl(implicit parent: OMID) : VarDecl = VarDecl(this.name, this.tp, OMS(this.path))
  def toConstant(implicit parent : OMID) : Constant = Constant(parent, name, Nil, Some(tp), df, None, notation)
  def ToOMS(implicit parent : OMID) : Term = OMS(toConstant.path)
  def toEliminationDecl(structure: InternalDeclaration, types:List[TypeLevel])(implicit parent : OMID) : InternalDeclaration = {
    val mapTypes = types map {tpl => OMV(tpl.path.name) / tpl.applied(structure.toVarDecl)}
    this match {
      case d @ TermLevel(p, args, ret, df, notC) => TermLevel(p, (Some(uniqueLN("m")), structure.ret)::args, ret, df, notC) ^^ mapTypes
      case d @ TypeLevel(p, args, df, notC) => TypeLevel(p, (None, structure.ret)::args, df, notC) ^^ mapTypes
      case d @ StatementLevel(p, args, df, notC) => StatementLevel(p, (Some(uniqueLN("m")), structure.ret)::args, df, notC) ^^ mapTypes
    }
  }
  def toString(implicit parent : OMID) = present(this.toConstant)
}

object InternalDeclaration {
  implicit def tp(d: InternalDeclaration)(implicit parent: OMID): Term = d.tp
  implicit def tm(d: InternalDeclaration): Option[Term] = d.df
  implicit def toVarDecl(d: InternalDeclaration)(implicit parent: OMID): VarDecl = d.toVarDecl

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
   * Generate no junk declaration for all internal declarations
   * @param parent the parent declared module of the derived declaration to elaborate
   * @param decls all declarations, used to construct the chain
   * @param tmdecls all term level declarations
   * @param tpdecls all type level declarations
   * @param context the inner context of the derived declaration
   * @returns returns one no junk (morphism) declaration for each type level declaration
   * then generates all the corresponding no junk declarations for the termlevel constructors of each declared type
   */    
  def noJunks(decls : List[InternalDeclaration], tpdecls: List[TypeLevel], tmdecls: List[TermLevel], statdecls: List[StatementLevel], context: Context)(implicit parent : OMID) : List[Constant] = {
    var derived_decls:List[Constant] = Nil
    val (tp_induct_decls, mapConstr, mapTerm, chain) = getFullMorph(decls, tpdecls, context, None)
    derived_decls ++=tp_induct_decls
    
    (tmdecls++statdecls) foreach {decl : InternalDeclaration => 
      val assertion = decl.noJunk(chain, context, mapConstr, mapTerm)
      derived_decls :+= Constant(parent, uniqueLN("compute_"+decl.name.last), Nil, Some(assertion), None, None)
    }
    derived_decls
  }
  
  /**
   * Generate no junk declaration for all the elimination form internal declarations
   * @param parent the parent declared module of the derived declaration to elaborate
   * @param decls all the elimination form declarations, used to construct the chain
   * @param tmdecls all term level declarations
   * @param tpdecls all type level declarations
   * @param context the inner context of the derived declaration
   * @returns returns one no junk (morphism) declaration for each type level declaration
   * then generates all the corresponding no junk declarations for the termlevel constructors of each declared type
   */    
  def noJunksEliminationDeclarations(decls : List[InternalDeclaration], tpdecls: List[TypeLevel], tmdecls: List[TermLevel], statdecls: List[StatementLevel], context: Context, introductionDecl: InternalDeclaration, origDecls: List[InternalDeclaration])(implicit parent : OMID) : List[Constant] = {
    val types : List[(Context, VarDecl, Sub)]= tpdecls map(_.getTypes(None))
    def mapConstr: InternalDeclaration => InternalDeclaration = {con => con ^ (types.map(_._3))}
    val chain : List[InternalDeclaration]= origDecls map mapConstr
    def makeApplNm = uniqueLN(introductionDecl.name+chain.foldLeft("")((a, b)=>a +" "+b.name))
    def makeApplied = (introductionDecl.ret, introductionDecl, {(arg:VarDecl, chain:Context, mapConstr: InternalDeclaration => InternalDeclaration) => OMV(makeApplNm) % introductionDecl.applied(chain)})
    
    def makeAppl = makeApplied match {case (x, y, map) => (x, {t:VarDecl => map (t, chain map(_.toVarDecl), mapConstr)})}
    
    def mapTerm(tm: VarDecl) : VarDecl = {
      println("makeAppl: "+makeAppl._1+", tm: "+tm.tp.get)
      List(makeAppl).find(x => compTp(x._1, tm.tp.get)) match {
        case Some(morph) => println(morph._2(tm)); morph._2(tm)
        case None => tm
      }
    }
    
    origDecls map {e => 
      val decl = e.toEliminationDecl(introductionDecl, tpdecls)
      val d = mapConstr(e)
      println("d: "+d.toString+", decl: "+decl.toString)
      
      //the result of applying m to the result of the constructor
      val (args, mappedRes) = d.argContext(None)
    
      //the result of applying the image of the constructor (all types substituted) to the image of the arguments
      val mappedArgs : List[VarDecl] = decl.argContext(None)._1.getDeclarations.head::args map (mapTerm(_))
      //ensure everything went well and the argument is actually in the context
      val mappedConstr : Term = decl.applied(mappedArgs)
      
      println("decl.args: "+decl.args)
      val assertion = Pi(chain.map(_.toVarDecl) ++ context ++ args, Eq(mappedConstr, toOMS(mappedRes)))
      Constant(parent, uniqueLN("compute_"+d.name.last), Nil, Some(assertion), None, None)
    }
    
 	  /*var derived_decls:List[Constant] = Nil
    val (tp_induct_decls, mapConstr, mapTerm, chain) = getFullMorph(decls, tpdecls, context, Some(List(applyMakeToArgs)))
    val makeApplied = applyMakeToArgs(mapConstr)
    
    
    /*derived_decls ++=tp_induct_decls
    
    (tmdecls++statdecls) foreach {decl : InternalDeclaration => 
      val assertion = decl.noJunk(chain, context, mapConstr, mapTerm)
      derived_decls :+= Constant(parent, uniqueLN("compute_"+decl.name.last), Nil, Some(assertion), None, None)
    }
    derived_decls*/*/
    
  }
  
  /**
   * get the induction axiom declarations for the type levels as well as the term level and constructor level part 
   * of the morphism mapping an (term or statement level) internal declaration to its image in the model
   * @param decls All internal declarations
   * @param tpdecls All type level internal declarations
   * @param context the inner context of the derived declaration
   */
  private def getFullMorph(decls : List[InternalDeclaration], tpdecls: List[TypeLevel], context: Context, morphs: Option[List[(InternalDeclaration=>InternalDeclaration)=>(Term, InternalDeclaration, (VarDecl, Context, InternalDeclaration => InternalDeclaration) => VarDecl)]])(implicit parent : OMID) : (List[Constant], InternalDeclaration => InternalDeclaration, VarDecl => VarDecl, List[InternalDeclaration]) = {
    //A list of quadruples consisting of (context of arguments, x, x_, the sub replacing x with a free type x_) for each type declaration declaring a type x
    val types : List[(Context, VarDecl, Sub)]= tpdecls map(_.getTypes(None))
    
    /** The morphism on constructors, it map c |-> c' for each constructor c */
    def mapConstr: InternalDeclaration => InternalDeclaration = {con => con ^ (types.map(_._3))}
    val chain : List[InternalDeclaration]= decls map mapConstr

    val initMorphs = morphs getOrElse(Nil) map (z => z(mapConstr)) map {case (x:Term, y:InternalDeclaration, map) => (OMV(uniqueLN(y.name+"_ret")) % x, y.toConstant, {t:VarDecl => map (t, chain map(_.toVarDecl), mapConstr)})}
    /** The morphism for terms, it maps all terms t: a = m |-> t' : a* = m' iff a is one of the earlier inductively defined types */
    def mapTerm(morphisms: List[(VarDecl, Constant, VarDecl => VarDecl)], tm: VarDecl) : VarDecl = {
      //initial mapping
      val tmpost = initMorphs.find(x => compTp(x._1.tp.get, tm.tp.get)) match {
        case Some(morph) => morph._3(tm)
        case None => tm
      }
      morphisms.find(x => compTp(x._1.toTerm, tmpost.tp.get)) match {
        case Some(morph) => morph._3(tmpost)
        case None => tmpost
      }
    }
    
    var morphisms : List[(VarDecl, Constant, VarDecl => VarDecl)] = Nil
    tpdecls zip types foreach {
      case (t:TypeLevel, (c, x, s)) => morphisms = morphisms :+ t.getMorph(t, chain, mapTerm (morphisms, _), mapConstr, c, x, context)
    }
    
    /** Now the declarations for the computation axioms */
    val tp_induct_decls = morphisms map(_._2)
    
    (tp_induct_decls, mapConstr, mapTerm (morphisms, _), chain)
  }
  def toEliminationDecls(decls: List[InternalDeclaration], structure: InternalDeclaration)(implicit parent : OMID) : List[InternalDeclaration] = {
    var types : List[TypeLevel] = Nil
    decls map {
      case tpl @ TypeLevel(_, _, _, _) => val tplelim = tpl.toEliminationDecl(structure, types); types :+= (tplelim match {case t @ TypeLevel(_,_,_,_) => t}); tplelim
      case d => d.toEliminationDecl(structure, types)
    }
  }
}

/** type declaration */
case class TypeLevel(path: GlobalName, args: List[(Option[LocalName], Term)], df: Option[Term], notC : NotationContainer)(implicit parent : OMID) extends InternalDeclaration {
  def ret = Univ(1)
  def tm = df
  def notation = notC
  def getTypes(suffix: Option[String]) : (Context, VarDecl, Sub) = {
    val (argsCon, x)=this.argContext(suffix)
    //TODO: Define in terms of Quotations, fix below try
    //val a = VarDecl(uniqueLN("quoted_return_type_of_"+x.name), None, Some(Univ(1)), Some(quote(x.toTerm, Nil)), None)
    val a = VarDecl(uniqueLN(x.name+"'"), None, Some(Univ(1)), None, None)
    (argsCon, x, (x.toTerm / a.toTerm))
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
  def getMorph(t: InternalDeclaration, chain : List[InternalDeclaration], mapTerm: VarDecl => VarDecl, mapConstr: InternalDeclaration => InternalDeclaration, ctx: Context, x: VarDecl, context: Context) = {
    val morphType : Term = t.noJunk(chain, context, mapConstr, mapTerm)
    //println("morphType: "+morphType.toNode.toString())
    // TODO: Define the morphism in terms of quotations
    val morphTerm : Option[Term] = None
    val morph = VarDecl(uniqueLN("induct_"+x.name), None, Some(morphType), morphTerm, None)
    val morphDecl = Constant(parent, morph.name, Nil, morph.tp, morph.df, None) //morph.toDeclaration(parent)
    //println("morphDecl: "+morphDecl.toNode.toString())
    def m(tm : VarDecl) = {
      val tp = ApplyGeneral(morph.toTerm, chain.map(_.tp) ++ ctx.map(_.toTerm) :+ tm.toTerm)
      val const = Constant(parent, uniqueLN(morph.name+" M "+tm.toStr(true)), Nil, Some(tp), morph.df, None)//val const = Constant(parent, uniqueLN(morph.name+" M "+tm.toStr(true)), Nil, Some(tp), morph.df, None)
      VarDecl(const.name, None, const.tp, const.df, None)
    }
    (x, morphDecl, m _)
  }
}

/** term constructor declaration */
case class TermLevel(path: GlobalName, args: List[(Option[LocalName], Term)], Ret: Term, df: Option[Term], notC: NotationContainer)(implicit parent : OMID) extends InternalDeclaration {
  def tm = df
  def ret = toOMS(Ret)
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

package info.kwarc.mmt.lf.induction

import info.kwarc.mmt.api._
import objects._
import symbols._
import notations._
import checking._
import modules._

import info.kwarc.mmt.lf._
//import scala.collection.parallel.ParIterableLike.Copy

private object InductiveTypes {
  def uniqueLN(nm: String): LocalName = {
    LocalName("") / nm
  }
   
  /**
    * Make a new variable declaration for a variable of type tp and fresh name (preferably name) in the given (optional) context
    * @param name the local name that will preferably be picked for the variable declaration
    * @param tp the type of the variable
    * @param the context in which to pick the a fresh variable (optional)
    * @note postcondition: the returned variable is free in the given context
    */
   def newVar(name: LocalName, tp: Term, con: Option[Context] = None) : VarDecl = {
      val c = con.getOrElse(Context.empty)
      val (freshName,_) = Context.pickFresh(c, name)
      VarDecl(freshName, tp)
   }

  val theory: MPath = ???
  object Eq extends BinaryLFConstantScala(theory, "eq")
  object Neq extends BinaryLFConstantScala(theory, "neq")
  
  val Contra = OMS(theory ? "contra") 
  
  def substituteByMorph(sub : (Term, Context, Term)) : Term = {
    val (source, con,  morph) = sub
    ApplyGeneral(morph, con.variables.toList.map(_.toTerm))
  }
}

import InductiveTypes._

/** helper class for the various declarations in an inductive type */ 
sealed abstract class InductiveDecl {
  def path: GlobalName
  def name = path.name
  def args: List[(Option[LocalName], Term)]
  def ret: Term
  def toTerm = FunType(args,ret)
  /** a context quantifying over all arguments and the variable declaration for the result of this constructor applied to those arguments */
  def argContext(suffix: Option[String]): (Context, VarDecl) = { 
      val dargs= args.zipWithIndex map {
      case ((Some(loc), arg), _) => (loc, arg)
      case ((None, arg), i) => (uniqueLN(i.toString), arg)
    }
    val con = dargs map {case (loc, tp) =>
      val n = suffix match {
        case Some(s) => loc / s
        case None => loc
      }
      newVar(n, tp, None)
    }
    val tm = ApplySpine(OMS(path), con.map(_.toTerm) :_*)
    (con, VarDecl(uniqueLN("return_type_of_"+name), None, Some(tm), None, None))
  }
  def toVarDecl : VarDecl = VarDecl(this.name, this.toTerm, OMS(this.path))
}

/** type declaration */
case class TypeLevel(path: GlobalName, args: List[(Option[LocalName], Term)]) extends InductiveDecl {
  def ret = Univ(1)
}

/** constructor declaration */
case class TermLevel(path: GlobalName, args: List[(Option[LocalName], Term)], ret: Term) extends InductiveDecl {
  def substituteOfType(sub : (Term, Context, Term)) : TermLevel = {
    val subArgs : List[(Option[LocalName], Term)]= args map {
      case (Some(loc), tp) => (Some(uniqueLN(loc + "substituted_"+sub.toString())), substituteByMorph(sub))
      case (None, tp) => (None, substituteByMorph(sub))
    }
    TermLevel(path./("term_substituted_"+sub.toString()), subArgs, ret)
  }
  def substitute(sub : Substitution) : TermLevel = {
    val subArgs = args map {
      case (Some(loc), tp) => (Some(uniqueLN(loc + "substituted_"+sub.toString())), tp ^ sub)
      case (None, tp) => (None, tp ^ sub)
    }
    TermLevel(path./("substituted_"+sub.toString()), subArgs, ret ^ sub)
  }
}

/* Rules and Judgments */
case class StatementLevel(path: GlobalName, args: List[(Option[LocalName], Term)]) extends InductiveDecl {
  def ret = Univ(1)
}

/** theories as a set of types of expressions */ 
class InductiveTypes extends StructuralFeature("inductive") with ParametricTheoryLike {

  override def check(dd: DerivedDeclaration)(implicit env: ExtendedCheckingEnvironment) {
    //TODO: check for inhabitability
  }

  def elaborate(parent: DeclaredModule, dd: DerivedDeclaration) = {
    // to hold the result
    var elabDecls : List[Declaration] = Nil
    var tmdecls : List[TermLevel]= Nil
    var tpdecls : List[TypeLevel]= Nil
    val decls = dd.getDeclarations map {
      case c: Constant =>
        val tp = c.tp getOrElse {throw LocalError("missing type")}        
          val FunType(args, ret) = tp
          if (JudgmentTypes.isJudgment(ret)(controller.globalLookup)) {
            StatementLevel(c.path, args)
          } else {
          ret match {
            case Univ(1) => {
              val tpdecl = TypeLevel(c.path, args)
              tpdecls ::= tpdecl 
              tpdecl
            }
            case Univ(x) if x != 1 => throw LocalError("unsupported universe")
            case r => {// TODO check that r is a type
              val tmdecl = TermLevel(c.path, args, r)
              tmdecls ::= tmdecl 
              tmdecl
            }
          }
        }
      case _ => throw LocalError("unsupported declaration")
    }
    // the type and term constructors of the inductive types
    var noConfDecls : List[Declaration] = Nil
    decls foreach {d =>
      val tp = d.toTerm
      val c = Constant(parent.toTerm, d.name, Nil, Some(tp), None, None)
      elabDecls ::= c
      d match {
        case TermLevel(loc, args, tm) =>
          noConfDecls ++= noConf(parent, TermLevel(loc, args, tm),tmdecls)
        case _ =>
      }
    }
    // build the result
    elabDecls = elabDecls.reverse ::: noConfDecls ::: noJunk(parent, decls, tpdecls, tmdecls, dd)
    
    new Elaboration {
      def domain = elabDecls map {d => d.name}
      def getO(n: LocalName) = {
        elabDecls.find(_.name == n)
      }
    }
  }

  /** declarations for injectivity of all term constructors */
  private def injDecl(parent : DeclaredModule, d : TermLevel) : Declaration = {
    if (d.args.length <= 0) 
      throw ImplementationError("Trying to assert injectivity of a constant function")
    val (aCtx, aApplied) = d.argContext(Some("1"))
    val (bCtx, bApplied) = d.argContext(Some("2"))
    
    val argEq = (aCtx zip bCtx) map {case (a,b) => Eq(a.toTerm,b.toTerm)}
    val resEq = Eq(aApplied.toTerm, bApplied.toTerm)
    val body = Arrow(Arrow(argEq, Contra), Arrow(resEq, Contra))
    val inj = Pi(aCtx ++ bCtx,  body)
    
    Constant(parent.toTerm, uniqueLN("injective_"+d.name.toString), Nil, Some(inj), None, None)
  }
  
  /** no confusion declarations for d and any other constructor */
  private def noConf(parent : DeclaredModule, d : TermLevel, tmdecls: List[TermLevel]) : List[Declaration] = {
    var decls:List[Declaration] = Nil
    tmdecls foreach {e => 
      if (e == d) {
        if(d.args.length > 0)
          decls ::= injDecl(parent, d)  
      } else {
          val name = uniqueLN("no_conf_axiom_for_" + d.name.toString + "_" + e.toString)
          val (dCtx, dApplied) = d.argContext(Some("1"))
          val (eCtx, eApplied) = e.argContext(Some("2"))
          val tp = Pi(dCtx ++ eCtx, Neq(dApplied.toTerm, eApplied.toTerm))
          decls ::= Constant(parent.toTerm, name, Nil, Some(tp), None, None)
      }
    }
    decls
  }
  
  private def noJunk(parent : DeclaredModule, decls : List[InductiveDecl], tpdecls: List[TypeLevel], tmdecls: List[TermLevel], dd: DerivedDeclaration) : List[Declaration] = {
    
    var derived_decls:List[Declaration] = Nil
    //A list of quadruples consisting of (context of arguments, a, the sub replacing x with a free type a, x) for each type declaration declaring a type x
    val quant : List[(Context, VarDecl, Sub, VarDecl)]= tpdecls map {case dec => 
      val (ctx, x) = dec.argContext(None)
      val a = newVar(LocalName(""), Univ(1), None)
      (ctx, a, Sub(x.name, a.toTerm), x)
    }
    
    val subst : Substitution = Substitution.list2substitution(quant.map(_._3))
    val chain : Context = Context.list2context(decls map {case x : InductiveDecl => x.toVarDecl}) ^ subst
    //The morphisms for each defined type x
    val morphisms : List[(VarDecl, Term)]= quant map {case x:(Context, VarDecl, Sub, VarDecl) => (x._4, Pi(x._1.++(chain).++(x._2), x._4.toTerm))}
    //The corresponding declarations
    derived_decls = morphisms map {case (x, t) => Constant(parent.toTerm, uniqueLN("no_junk_axiom_for_type_"+x.toString), Nil, Some(t), None, None)}
    val morphs : List[(VarDecl, Declaration)] = morphisms.map(_._1) zip derived_decls
    
    //Now we need to produce the noJunk axioms for the term constructors
    tmdecls zip morphs foreach {
      case (tmdecl : TermLevel, (x : VarDecl, t : Declaration)) =>
        val args = tmdecl.argContext(None)._1
        val sub : (Term, Context, Term) = (x.toTerm, args, ApplyGeneral(OMV(t.name), (chain ^ subst).map(_.toTerm)))
        
        val original = tmdecl.argContext(Some("no_junk_result_1"))
        val mappedDecl : TermLevel = tmdecl.substituteOfType(sub).substitute(subst)
        val image = mappedDecl.argContext(Some("no_junk_result_2"))
        val assertion = Pi(args++original._1++image._1, Eq(original._2.toTerm, image._2.toTerm))
        derived_decls ::= Constant(parent.toTerm, uniqueLN("no_junk_axiom_for_constructor_"+assertion.toString), Nil, Some(assertion), None, None)
    }
    derived_decls
  } 
}
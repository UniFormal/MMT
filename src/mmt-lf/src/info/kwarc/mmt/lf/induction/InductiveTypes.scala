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
  /**
    * Make a new unique local name from the given string
    * @param nm the string from which the local name is generated
    */
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
}

import InductiveTypes._
//import info.kwarc.mmt.quotation.Quotation.quote

/** helper class for the various declarations in an inductive type */ 
sealed abstract class InductiveDecl {
  def path: GlobalName
  def name = path.name
  def args: List[(Option[LocalName], Term)]
  def ret: Term

  /**
    * Build a context quantifying over all arguments
    *  and the variable declaration for the result of this constructor applied to those arguments
    * @param suffix (optional) a suffix to append to the local name of each variable declaration in the returned context
    */
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
    val tp = ApplySpine(OMS(path), con.map(_.toTerm) :_*)
    (con, VarDecl(uniqueLN("return_type_of_"+name), None, Some(tp), None, None))
  }
    /**
    * Substitute the argument types of the inductivel declaration according to the substitution sub
    * @param sub the substitution to apply
    */
  def ^(sub : Substitution) : InductiveDecl = {
    val subArgs = args map {
      case (Some(loc), tp) => (Some(uniqueLN(loc.toString() + "substituted_"+sub.toString())), tp ^ sub)
      case (None, tp) => (None, tp ^ sub)
    }
    this match {
      case TermLevel(_, _, _) => TermLevel(path./("substituted_"+sub.toString()), subArgs, ret ^ sub)
      case TypeLevel(_, _) => TypeLevel(path./("substituted_"+sub.toString()), subArgs)
      case StatementLevel(_, _) => StatementLevel(path./("substituted_"+sub.toString()), subArgs)
    }
    
  }
  def toVarDecl : VarDecl = VarDecl(this.name, this, OMS(this.path))
}

object InductiveDecl {
  implicit def toTerm(d: InductiveDecl): Term = FunType(d.args,d.ret)
  implicit def toVarDecl(d: InductiveDecl): VarDecl = VarDecl(d.name, d, OMS(d.path))
}

/** type declaration */
case class TypeLevel(path: GlobalName, args: List[(Option[LocalName], Term)]) extends InductiveDecl {
  def ret = Univ(1)
  def getTypes(suffix: Option[String]) : (Context, VarDecl, VarDecl, Sub) = {
    val (argsCon, x)=this.argContext(suffix)
    //TODO: Define in terms of Quotations, fix below try
    //val a = VarDecl(uniqueLN("quoted_return_type_of_"+x.name), None, Some(Univ(1)), Some(quote(x.toTerm, Nil)), None)
    val a = VarDecl(uniqueLN("quoted_return_type_of_"+x.name), None, Some(Univ(1)), None, None)
    (argsCon, x, a, x.toTerm / (a.toTerm))
  }
  
  /** 
  * Generate morphism declaration and the actual morphism for a type level declaration
  * @param parent the parent declared module of the derived declaration to elaborate
  * @param decls all declarations
  * @param tpdecls all type level declarations
  *  @returns returns the morphism declaration and the morphism itself
  */    
  def getMorph(chain : List[InductiveDecl], ctx: Context, x: VarDecl, a: VarDecl, sub: Sub)(implicit parent : Term) = {
    val morphType : Term = Pi.apply(chain.map(_.toVarDecl)++ctx.++(x), a.toTerm)
    // TODO: Define the morphism in terms of quotations
    val morphTerm : Term = ???
    val morph = VarDecl.apply(uniqueLN("no_junk_axiom_for_type_"+x.toString), morphType, morphTerm)
    val morphDecl = morph.toDeclaration(parent)
    def m(tm : Term) = ApplyGeneral(morph.toTerm, ctx.map(_.toTerm) :+ tm)
    (morphDecl, m _, x, a, sub)
  }
}

/** constructor declaration */
case class TermLevel(path: GlobalName, args: List[(Option[LocalName], Term)], ret: Term) extends InductiveDecl {
 
  /**
   * inversely apply a sub to the argument types of the term level
   */
  def invertSubstitution(sub : Sub) : InductiveDecl = {
    val Sub(x:LocalName, a:OMV) = sub
    this ^ ((a / OMV(x)))
  }
}


/* Rules and Judgments */
case class StatementLevel(path: GlobalName, args: List[(Option[LocalName], Term)]) extends InductiveDecl {
  def ret = Univ(1)
}

/** theories as a set of types of expressions */ 
class InductiveTypes extends StructuralFeature("inductive") with ParametricTheoryLike {

  /**
    * Checks the validity of the inductive type(s) to be constructed
    * @param dd the derived declaration from which the inductive type(s) are to be constructed
    */
  override def check(dd: DerivedDeclaration)(implicit env: ExtendedCheckingEnvironment) {
    //TODO: check for inhabitability
  }
  
  /**
   * Elaborates an declaration of one or multiple mutual inductive types into their declaration, 
   * as well as the corresponding no confusion and no junk axioms
   * Constructs a structure whose models are exactly the (not necessarily initial) models of the declared inductive types
   * @param parent The parent module of the declared inductive types
   * @param dd the derived declaration to be elaborated
   */
  def elaborate(parent: DeclaredModule, dd: DerivedDeclaration) = {
    //The only thing ever needed
    implicit val parentTerm = parent.toTerm
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
    // the type and term constructors of the inductive types and the no confusion axioms for the term constructors
    var noConfDecls : List[Declaration] = Nil
    decls foreach {d =>
      val tp = d
      val c = Constant(parentTerm, d.name, Nil, Some(tp), None, None)
      elabDecls ::= c
      d match {
        case TermLevel(loc, args, tm) =>
          noConfDecls ++= noConf(TermLevel(loc, args, tm),tmdecls)
        case _ =>
      }
    }
    // get the no junk axioms and build the result
    elabDecls = elabDecls.reverse ::: noConfDecls ::: noJunk(decls, tpdecls, tmdecls)
    
    new Elaboration {
      def domain = elabDecls map {d => d.name}
      def getO(n: LocalName) = {
        elabDecls.find(_.name == n)
      }
    }
  }

  /**
    * Generate injectivity declaration for term constructor d
    * @param parent the parent declared module of the derived declaration to elaborate
    * @param d the term level for which to generate the injectivity axiom
    */
  private def injDecl(d : TermLevel)(implicit parent : Term) : Declaration = {
    val (aCtx, aApplied) = d.argContext(Some("1"))
    val (bCtx, bApplied) = d.argContext(Some("2"))
    
    val argEq = (aCtx zip bCtx) map {case (a,b) => Eq(a.toTerm,b.toTerm)}
    val resEq = Eq(aApplied.toTerm, bApplied.toTerm)
    val body = Arrow(Arrow(argEq, Contra), Arrow(resEq, Contra))
    val inj = Pi(aCtx ++ bCtx,  body)
    
    Constant(parent, uniqueLN("injective_"+d.name.toString), Nil, Some(inj), None, None)
  }
  
  /**
  * Generate no confusion/injectivity declaration for term constructor d and all term constructors of the same return type
  * @param parent the parent declared module of the derived declaration to elaborate
  * @param d the term level declaration for which to generate the no confusion declarations
  * @param tmdecls all term level declarations
  */
  private def noConf(d : TermLevel, tmdecls: List[TermLevel])(implicit parent : Term) : List[Declaration] = {
  var decls:List[Declaration] = Nil
  tmdecls.filter(_.ret == d.ret) foreach {e => 
    if (e == d) {
      if(d.args.length > 0)
        decls ::= injDecl(d)  
    } else {
        val name = uniqueLN("no_conf_axiom_for_" + d.name.toString + "_" + e.toString)
        val (dCtx, dApplied) = d.argContext(Some("1"))
        val (eCtx, eApplied) = e.argContext(Some("2"))
        val tp = Pi(dCtx ++ eCtx, Neq(dApplied.toTerm, eApplied.toTerm))
        decls ::= Constant(parent, name, Nil, Some(tp), None, None)
    }
  }
  decls
  }
  
  
  /**
  * generate no junk declaration for all term-- and typelevel inductive declarations
  * @param parent the parent declared module of the derived declaration to elaborate
  * @param decls all declarations
  * @param tmdecls all term level declarations
  * @param tpdecls all type level declarations
  * @returns returns one no junk (morphism) declaration for each type level declaration
  * then generates all the corresponding no junk declarations for the termlevel constructors of each declared type
  */    
  private def noJunk(decls : List[InductiveDecl], tpdecls: List[TypeLevel], tmdecls: List[TermLevel])(implicit parent : Term) : List[Declaration] = {
    var derived_decls:List[Declaration] = Nil
    //A list of quadruples consisting of (context of arguments, x, a, the sub replacing x with a free type a) for each type declaration declaring a type x
    val types : List[(Context, VarDecl, VarDecl, Sub)]= tpdecls map(_.getTypes(None))
    
    val subst = types.map(_._4)
    val chain : List[InductiveDecl]= decls map (_ ^ subst)
    
    //The morphisms for each defined type x
    val morphisms = tpdecls zip types map {case (t:TypeLevel, (c, x, a, s)) => t.getMorph(chain, c, x, a, s)}
    derived_decls = morphisms map(_._1)
        
    morphisms foreach {case (t : Declaration, m: (Term => Term), x : VarDecl, a : VarDecl, s : Sub) =>
    //Now we need to produce the noJunk axioms for the constructors of the type x under the morphism with x |-> a
      tmdecls.filter(_.ret == x.tp.get) foreach {
        case tmdeclOrig : TermLevel => 
          //substitute the other types, (needed in case of mutual induction)
          val tmdecl = tmdeclOrig ^ (subst filterNot (_ == s))
          val args = tmdecl.argContext(None)._1
            
          //the result of applying m to the result of the constructor
          val orig = ApplyGeneral(tmdecl, args.variables.map(_.toTerm).toList)
          val mappedOrig = m(orig)
          
          //the image of the arguments of the constructor under m
          val argsMapped : List[Term]= args.variables.toList map {arg =>
            if (arg.tp.get == x) {
              m(arg.toTerm)
            } else
              arg.toTerm
          }
          //the result of applying the image of the constructor (all types substituted) to the image of the arguments
          val mappedArgs : Term = ApplyGeneral(tmdecl ^ s, argsMapped)
          
          //both results should be equal, if m is actually a homomorphism
          val assertion = Pi(args, Eq(mappedOrig, mappedArgs))
          derived_decls ::= Constant(parent, uniqueLN("no_junk_axiom_for_constructor_"+assertion.toString), Nil, Some(assertion), None, None)
        }
      }
    derived_decls
  } 
}
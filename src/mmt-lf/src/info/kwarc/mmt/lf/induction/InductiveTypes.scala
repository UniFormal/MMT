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

/** helper class for the various declarations in an inductive type */ 
sealed abstract class InductiveDecl {
  def path: GlobalName
  def name = path.name
  def args: List[(Option[LocalName], Term)]
  def ret: Term
  def toTerm = FunType(args,ret)
  /** a context quantifying over all arguments and this constructor applied to those arguments */
  def argContext(suffix: Option[String]): (Context, Term) = { 
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
    (con, tm)
  }
  def toVarDecl : VarDecl = VarDecl(this.name, this.toTerm, OMS(this.path))
}

/** type declaration */
case class TypeLevel(path: GlobalName, args: List[(Option[LocalName], Term)]) extends InductiveDecl {
  def ret = Univ(1)
}

/** constructor declaration */
case class TermLevel(path: GlobalName, args: List[(Option[LocalName], Term)], ret: Term) extends InductiveDecl

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
    /* val indArgs = dd.tpC.get match {
      case Some(OMA(_, args)) => args
    }
    val tpArgs = indArgs.foldLeft(Nil:List[Term])({(l:List[Term], tm:Term) => tm match {
      case Univ(1) => tm::l
      case _ => l
      }}) 
    val tpArgs = Nil
    var elabDecls:List[Declaration] = tpArgs map {tp:Term => VarDecl(LocalName(parent.name.toString()+tp.toString()), None, Some(Univ(1)),  Some(tp), None).toDeclaration(parent.toTerm)}
    tpdecls = elabDecls.zip(tpArgs) map {case (d, tp) => 
      val FunType(args, ret) = tp
      TypeLevel(d.name, args)
    } */
    // split declarations into term and type level
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
    elabDecls = elabDecls.reverse ::: noConfDecls ::: noJunk(parent, decls, tpdecls, dd)
    
    // TODO: Add the induction axioms for the term constructors (using the noJunk axioms for the type constructors)
    /*
     * Exam:
     * induct_N : (U:type) (u:U) (f:U -> U) (N -> U)
     * induct_z : |- induct_N (U:type) (u:U) (f:U -> U) z = u
     * induct_s : |- induct_N (U:type) (u:U) (f:U -> U) 
     * 
     * Now induct_N is functor from N to any (given) modell U
     * We want our elaborate to correspond (via applying Mod) to the initial modell of N, i.e.
     * inductive_N satisfies universal mapping property or equivalently our modell
     * is the initial object in the category of modells of N
     * 
     * Perhaps additionally uniqueness of functor induct_N from N to given modell U
     * 
     * Rec. def. func: like additional noConf axiom
     * Exam:
     * def m(n) = 
     * 	n match {
     * 	case z => u
     * 	case s(x) => f(m(x))
     * }
     * 
     * or more involved:
     * 
     * fact = (induct_N (N x N) (z, 1) ([f] [(n, n')] (s n, sn * n')) . Pi_2)
     */
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
    val resEq = Eq(aApplied, bApplied)
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
          val tp = Pi(dCtx ++ eCtx, Neq(dApplied, eApplied))
          decls ::= Constant(parent.toTerm, name, Nil, Some(tp), None, None)
      }
    }
    decls
  }
  
  private def noJunk(parent : DeclaredModule, decls : List[InductiveDecl], tpdecls: List[TypeLevel], dd: DerivedDeclaration) : List[Declaration] = {
    val quant : List[(Context, List[Sub], Term)]= tpdecls map {case dec => 
      val (ctx, x) = dec.argContext(None)
      val a = newVar(LocalName(""), x, None)
      (ctx++a, Context.pickFresh(ctx++a, a.name)._2.subs.toList, x)
    }
    val bla : List[Iterator[Sub]] = quant map(_._2.toIterator)
    val blabla = bla.flatten 
    
    val subs : List[Sub] = quant map {x:(Context, List[Sub], Term) => x._2.toIterator} flatten
    val subst : Substitution = Substitution.list2substitution(subs)
    val chain : Context = Context.list2context(decls map {case x : InductiveDecl => x.toVarDecl}) ^ subst
    val noJunks = quant map {case x:(Context, List[Sub], Term) => Pi(x._1.++(chain), x._3)}
    
    noJunks map {t => Constant(parent.toTerm, uniqueLN("no_junk_axiom_for_"+dd.name.toString), Nil, Some(t), None, None)}
  } 
}
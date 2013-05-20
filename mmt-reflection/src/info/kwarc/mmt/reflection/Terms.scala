package info.kwarc.mmt.reflection
import info.kwarc.mmt.api._
import libraries._
import modules._
import symbols._
import objects._
import utils._
import frontend._
import objects.Conversions._
/*import scala.collection.immutable.Stack */

//import info.kwarc.mmt.lf._

/* Meta-variable names
   s, t: Terms
   A, B: Types
   K, L: kinds
   G: contexts
*/

/* Operators:
   v % A  : a context in which v : OMV has type A : Term
   G ++ G': appending G' to G
   G(name): look up type of OMV(name) in G
   v / s  : the substitution that substitutes v : OMV  with s : Term
   s ^ S  : application of the substitution S to s : Term
*/

case class ReflectionError(msg : String) extends java.lang.Throwable(msg)

object Reflection {
   val base = new DPath(utils.URI("http", "cds.omdoc.org") / "urtheories" / "reflection")
   val theory = base ? "Terms"
   def constant(name : String) : GlobalName = theory ? name
   val rtype = constant("formation")
   val intro = constant("intro")
   val elim = constant("elim")
   val eval = constant("eval")
}

/* apply methods and extractor methods for Scala
   constructor and pattern matcher: Lambda("x", tp, scope)
   accordingly for Pi, Apply, Arrow
   Univ(1), Univ(2) are type and kind
 */

object TermRefl {
   def apply(thy: Term, term: Term) = OMA(OMS(Reflection.intro), List(thy, term))
   def unapply(t : Term) : Option[(Term,Term)] = t match {
	   case OMA(OMS(Reflection.intro), List(thy, tm)) => Some((thy, tm))
	   case _ => None
   }
}

object TermEval {
  def apply(thy: Term, term: Term) = OMA(OMS(Reflection.eval), List(thy, term))
  def unapply(t : Term) : Option[(Term,Term)] = t match {
    case OMA(OMS(Reflection.eval), List(thy, tm)) => Some((thy, tm))
    case _ => None
  }
}

object ReflType {
  def apply(thy: Term, tp: Term)= OMA(OMS(Reflection.rtype), List(thy, tp))
  def unapply(t : Term) : Option[(Term,Term)] = t match {
    case OMA(OMS(Reflection.rtype), List(thy, tp)) => Some((thy, tp))
    case _ => None
  }
}

object Elim {
  def apply(t: Term, mor: Term) = OMA(OMS(Reflection.elim), List(t, mor))
  def unapply(t : Term) : Option[(Term,Term)] = t match {
    case OMA(OMS(Reflection.elim), List(t, mor)) => Some((t, mor))
    case _ => None
  }
}

/* Typing Rules for Reflected Terms */

object TermReflectionRule extends InferenceRule(Reflection.intro) {
  def apply(solver: Solver)(tm: Term)(implicit stack: Stack): Option[Term] =
    tm match {
       case TermRefl(s,t) => solver.inferType(t)
       case _ => None
     }
 }

 object TypeReflectionRule extends InferenceRule(Reflection.rtype) {
   def apply(solver: Solver)(tm: Term)(implicit stack: Stack): Option[Term] =
     tm match {
       case ReflType(s,a) => solver.inferType(a)
       case _ => None
     }
 }

 object ReflTermEvalRule extends InferenceRule(Reflection.eval){
   def apply(solver: Solver)(tm: Term)(implicit stack: Stack): Option[Term] =
    tm match {
       case TermEval(s,q) =>
         if(!stack.frames.isEmpty) {
         solver.inferType(q)
         }
         else None
       case _ => None
     }
 }

object ElimReflectionRule extends InferenceRule(Reflection.elim){
  implicit def pCont(p:Path){}
  def apply(solver: Solver)(tm: Term)(implicit stack : Stack) : Option[Term] = {
     tm match {
       case Elim(q,mor) =>
         if(!stack.frames.isEmpty) {
           solver.inferType(q) match {
            case None => None
            case Some(ReflType(s,a)) => Some(solver.controller.checker.checkMorphism(mor,s,stack.theory))
            case _ => None
           }
         }
         else None
       case _ => None
     }
   }
 }

/* Computation Rules for Reflected Terms */

object ComputationReflectionRule extends ComputationRule(Reflection.elim){
 implicit def pCont(p:Path){}
 implicit val stack = Stack(List())
 def apply(solver: Solver)(tm: Term)(implicit stack : Stack) : Option[Term] = {
   tm match {
     case Elim(TermRefl(s,t),mor) =>
       if(!stack.frames.isEmpty) {
        try {
          solver.checkMorphism(mor,s)
        }
        catch {
         case e:Error => false
        }
       Some(OMA(mor,List(t)))
       }
       else None
     case _ => None
 }
 }
}

object SoundnessReflectionRule extends ComputationRule(Reflection.eval){
  def apply(solver: Solver)(tm: Term)(implicit stack: Stack) : Option[Term] = {
    tm match {
      case TermEval(s1,TermRefl(s2,t)) =>
        if (s1 == s2)
           Some(t)
        else
           None
      case _ => None
    }
  }
}

object CompletenessReflectionRule extends ComputationRule(Reflection.intro){
  def apply(solver: Solver)(tm: Term)(implicit stack: Stack) : Option[Term] = {
    tm match {
      case TermRefl(s1,TermEval(s2,t)) =>
        if (s1 == s2) {
          solver.inferType(tm) match {
            case Some(ReflType(s,a)) => s == s2
            case _ => false
        }
        Some(t)
        }
        else None
      case _ => None
    }
  }
}

/* Equality Rule for Reflected Terms */

object ExtensionalityReflectionRule extends EqualityRule(Reflection.intro){
  def apply(solver: Solver)(tm1: Term, tm2: Term, tp: Term)(implicit stack: Stack) : Boolean = {
    (tm1,tm2) match {
      case (TermRefl(s1,t1),TermRefl(s2,t2)) =>
        if (s1 == s2) {
          solver.checkEquality(TermEval(s1,t1),TermEval(s2,t2),None)
        }
        else false
      case (_,_) => false
    }
  }
}


/* Solution Rules for Reflected Terms */

object SolveEvalReflectionRule extends SolutionRule(Reflection.eval){
  def apply(solver: Solver)(tm1: Term, tm2: Term)(implicit stack : Stack) : Boolean = {
    tm1 match {
      case TermEval(s,x) => solver.checkEquality(x,TermRefl(s,tm2),None)
      case _ => false
    }
  }
}

object SolveReflReflectionRule extends SolutionRule(Reflection.intro){
  def apply(solver: Solver)(tm1: Term, tm2: Term)(implicit stack : Stack) : Boolean = {
    tm1 match {
      case TermRefl(s,x) => solver.checkEquality(x,TermEval(s,tm2),None)
      case _ => false
    }
  }
}

/* Type Checking Rules for Reflected Terms */

object ReflTypingRule extends TypingRule(Reflection.rtype){
  def apply(solver: Solver)(tm: Term, tp: Term)(implicit stack : Stack) : Boolean = {
    tp match {
      case ReflType(s,a) => solver.checkTyping(TermEval(s,tm),a)
      case _ => false
    }
  }
}

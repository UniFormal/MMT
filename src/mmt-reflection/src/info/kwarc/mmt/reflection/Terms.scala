package info.kwarc.mmt.reflection
import info.kwarc.mmt.api._
import libraries._
import modules._
import symbols._
import objects._
import utils._
import frontend._
import objects.Conversions._
import scala.collection.immutable.Stack

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
   val base = new DPath(utils.URI("http", "cds.omdoc.org") / "foundations" / "reflection" / "termReflection.omdoc")
   val theory = base ? "TermReflection"
   def constant(name : String) : GlobalName = theory ? name
   val intro = constant("introduction")
   val elim = constant("elimination")
   val rtype = constant("refltype")
   val eval = constant("evaluation")
   val ttype = constant("type")
}

/* apply methods and extractor methods for Scala
   constructor and pattern matcher: Lambda("x", tp, scope)
   accordingly for Pi, Apply, Arrow
   Univ(1), Univ(2) are type and kind
 */

object TermRefl {
   def apply(thy: MPath, term: Term) = OMA(OMS(Reflection.intro), List(OMMOD(thy), term))
   def unapply(t : Term) : Option[(MPath,Term)] = t match {
	   case OMA(Reflection.intro, List(OMMOD(thy), tm)) => Some((thy, tm))
	   case _ => None
   }
}

object TermEval {
  def apply(thy: MPath, term: Term) = OMA(OMS(Reflection.eval), List(OMMOD(thy), term))
  def unapply(t : Term) : Option[(MPath,Term)] = t match {
    case OMA(Reflection.elim, List(OMMOD(thy), tm)) => Some((thy, tm))
    case _ => None
  }
}

object ReflType {
  def apply(thy: MPath, tp: Term)= OMA(OMS(Reflection.rtype), List(OMMOD(thy), tp))
  def unapply(t : Term) : Option[(MPath,Term)] = t match {
    case OMA(Reflection.rtype, List(OMMOD(thy), tp)) => Some((thy, tp))
    case _ => None
  }
}

object Elim {
  def apply(t: Term, mor: Term) = OMA(OMS(Reflection.elim), List(t, mor))
  def unapply(t : Term) : Option[(Term,Term)] = t match {
    case OMA(Reflection.rtype, List(t, mor)) => Some((t, mor))
    case _ => None
  }
}

/* Typing Rules for Reflected Terms */

object TermReflectionRule extends InferenceRule(Reflection.intro) {
   def apply(solver: Solver, tm: Term, frameStack: Stack[Frame]): Option[Term] =
    tm match {
       case TermRefl(s,t) => solver.inferTypeRefl(t, frameStack.push(Frame(s, Context())))
       case _ => None
     }
 }

 object TypeReflectionRule extends InferenceRule(Reflection.rtype) {
   def apply(solver: Solver, tm: Term, frameStack: Stack[Frame]): Option[Term] =
     tm match {
       case ReflType(s,a) => solver.inferTypeRefl(a, frameStack.push(Frame(s,Context())))
       case _ => None
     }
 }

 object ReflTermEvalRule extends InferenceRule(Reflection.eval){
   def apply(solver: Solver, tm: Term, frameStack: Stack[Frame]): Option[Term] =
     tm match {
       case TermEval(s,q) => if (frameStack.nonEmpty) solver.inferTypeRefl(q, frameStack.pop)
         else None
       case _ => None
     }
 }

object ElimReflectionRule extends InferenceRule(Reflection.elim){
  implicit def pCont(p:Path){}
   def apply(solver: Solver, tm: Term, frameStack: Stack[Frame]) : Option[Term] = {
     tm match {
       case Elim(q,mor) => if (frameStack.nonEmpty)
         solver.inferTypeRefl(q, frameStack) match {
            case None => None
            case Some(ReflType(s,a)) => Some(solver.controller.checker.checkMorphism(mor,OMMOD(s),OMMOD(frameStack.last.theory)))
            case _ => None
         }
         else None
       case _ => None
     }
   }
 }

/* Computation Rules for Reflected Terms */

object ComputationReflectionRule extends ComputationRule(Reflection.elim){
 implicit def pCont(p:Path){}
 def apply(solver: Solver, tm: Term, frameStack: Stack[Frame]) : Option[Term] = {
   tm match {
     case Elim(TermRefl(s,t),mor) => if (frameStack.nonEmpty) {
       try {
         solver.controller.checker.checkMorphism(mor,OMMOD(s),OMMOD(frameStack.last.theory))
       } catch {
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
  def apply(solver: Solver, tm: Term, frameStack: Stack[Frame]) : Option[Term] = {
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
  def apply(solver: Solver, tm: Term, frameStack: Stack[Frame]) : Option[Term] = {
    tm match {
      case TermRefl(s1,TermEval(s2,t)) =>
        if (s1 == s2) {
          solver.inferTypeRefl(tm,frameStack) match {
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
  def apply(solver: Solver, tm1: Term, tm2: Term, tp: Term, frameStack: Stack[Frame]) : Boolean = {
    (tm1,tm2) match {
      case (TermRefl(s1,t1),TermRefl(s2,t2)) =>
        if (s1 == s2) {
          solver.checkEqualityRefl(TermEval(s1,t1),TermEval(s2,t2),None,frameStack)
        }
        else false
      case (_,_) => false
    }
  }
}


/* Solution Rules for Reflected Terms */

object SolveEvalReflectionRule extends SolutionRule(Reflection.eval){
  def apply(solver: Solver, tm1: Term, tm2: Term, frameStack: Stack[Frame]) : Boolean = {
    tm1 match {
      case TermEval(s,x) => solver.checkEqualityRefl(x,TermRefl(s,tm2),None,frameStack.pop)
      case _ => false
    }
  }
}

object SolveReflReflectionRule extends SolutionRule(Reflection.eval){
  def apply(solver: Solver, tm1: Term, tm2: Term, frameStack: Stack[Frame]) : Boolean = {
    tm1 match {
      case TermRefl(s,x) => solver.checkEqualityRefl(x,TermEval(s,tm2),None,frameStack.push(Frame(s,Context())))
      case _ => false
    }
  }
}

/* Type Checking Rules for Reflected Terms */

object ReflTypingRule extends TypingRule(Reflection.rtype){
  def apply(solver: Solver, tm: Term, tp: Term, frameStack: Stack[Frame]) : Boolean = {
    tp match {
      case ReflType(s,a) => solver.checkTypingRefl(TermEval(s,tm),a,frameStack.push(Frame(s,Context())))
      case _ => false
    }
  }
}

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

case class MorphReflectionError(msg : String) extends java.lang.Throwable(msg)

object ReflectionMorph {
  val base = new DPath(utils.URI("http", "cds.omdoc.org") / "foundations" / "reflection" / "morphismReflection.omdoc")
  val theory = base ? "MorphismReflection"
  def constant(name : String) : GlobalName = theory ? name
  val intro = constant("introduction")
  val elim = constant("elimination")
  val rtype = constant("refltype")
  val eval = constant("evaluation")
  val ttype = constant("type")
}

object MorphRefl {
  def apply(thy: Term, term: Term) = term match {
    case ExplicitMorph(rec,dom) => OMA(OMS(ReflectionMorph.intro), List(thy,term))
  }
  def unapply(t : Term) : Option[(Term,Term)] = t match {
    case OMA(OMS(ReflectionMorph.intro), List(thy, tm)) => Some((thy, tm))
    case _ => None
  }
}

object MorphEval {
  def apply(thy: Term, term: Term) = term match {
    case ExplicitMorph(rec,dom) => OMA(OMS(ReflectionMorph.eval), List(thy,term))
  }
  def unapply(t : Term) : Option[(Term,Term)] = t match {
    case OMA(OMS(ReflectionMorph.eval), List(thy, tm)) => Some((thy, tm))
    case _ => None
  }
}

object MorphReflType {
  def apply(thy: Term, tp: Term) = OMA(OMS(ReflectionMorph.rtype), List(thy, tp))
  def unapply(t : Term) : Option[(Term,Term)] = t match {
    case OMA(OMS(ReflectionMorph.rtype), List(thy, tp)) => Some((thy, tp))
    case _ => None
  }
}

object MorphElim {
  def apply(t: Term, mor: Term) = mor match {
    case ExplicitMorph(rec,dom) =>  t match {
      case OMID(path) => rec.fields.find(localName => Some(localName._1) == path.toTriple._3) match {
        case Some(pair) => pair._2
      }
    }
    case _ =>  OMA(OMS(ReflectionMorph.elim), List(t, mor))
  }
  def unapply(t : Term) : Option[(Term,Term)] = t match {
    case OMA(OMS(ReflectionMorph.elim), List(t, mor)) => Some((t, mor))
    case _ => None
  }
}

/* Typing Rules for Reflected Morphisms */

object MorphismReflectionRule extends InferenceRule(ReflectionMorph.intro) {
  def apply(solver: Solver)(tm: Term)(implicit stack: Stack): Option[Term] =
    tm match {
      case MorphRefl(s,t) => solver.inferType(t)
      case _ => None
    }
}

object MorphTypeReflectionRule extends InferenceRule(ReflectionMorph.rtype) {
  def apply(solver: Solver)(tm: Term)(implicit stack: Stack): Option[Term] =
    tm match {
      case MorphReflType(s,a) => solver.inferType(a)
      case _ => None
    }
}

object MorphReflTermEvalRule extends InferenceRule(ReflectionMorph.eval){
  def apply(solver: Solver)(tm: Term)(implicit stack: Stack): Option[Term] =
    tm match {
      case MorphEval(s,q) =>
        if(!stack.frames.isEmpty) {
          solver.inferType(q)
        }
        else None
      case _ => None
    }
}

object MorphElimReflectionRule extends InferenceRule(ReflectionMorph.elim){
  implicit def pCont(p:Path){}
  def apply(solver: Solver)(tm: Term)(implicit stack : Stack) : Option[Term] = {
    tm match {
      case MorphElim(q,mor) =>
        if(!stack.frames.isEmpty) {
          solver.inferType(q) match {
            case None => None
            case Some(MorphReflType(s,a)) => Some(solver.controller.checker.checkMorphism(mor,s,stack.theory))
            case _ => None
          }
        }
        else None
      case _ => None
    }
  }
}

/* Computation Rules for Reflected Morphisms */

/* Given W |- MorphRefl(s,m), it holds that  W |- MorphRefl(s,m).t = m(t) */

object ComputationMorphReflectionRule extends ComputationRule(ReflectionMorph.elim){
  implicit def pCont(p:Path){}
  implicit val stack = Stack(List())
  def apply(solver: Solver)(tm: Term)(implicit stack : Stack) : Option[Term] = {
    tm match {
      case MorphElim(t, MorphRefl(s,m)) =>
        if(!stack.frames.isEmpty) {
          try {
            solver.checkMorphism(MorphRefl(s,m),s)
          }
          catch {
            case e:Error => false
          }
          Some(OMA(MorphRefl(s,m),List(t)))
        }
        else None
      case _ => None
    }
  }
}

object SoundnessMorphReflectionRule extends ComputationRule(ReflectionMorph.eval){
  def apply(solver: Solver)(tm: Term)(implicit stack: Stack) : Option[Term] = {
    tm match {
      case MorphEval(s1,MorphRefl(s2,t)) =>
        if (s1 == s2)
          Some(t)
        else
          None
      case _ => None
    }
  }
}

object CompletenessMorphReflectionRule extends ComputationRule(ReflectionMorph.intro){
  def apply(solver: Solver)(tm: Term)(implicit stack: Stack) : Option[Term] = {
    tm match {
      case MorphRefl(s1,MorphEval(s2,t)) =>
        if (s1 == s2) {
          solver.inferType(tm) match {
            case Some(MorphReflType(s,a)) => s == s2
            case _ => false
          }
          Some(t)
        }
        else None
      case _ => None
    }
  }
}

/* Equality Rule for Reflected Morphisms */

object ExtensionalityMorphReflectionRule extends EqualityRule(ReflectionMorph.intro){
  def apply(solver: Solver)(tm1: Term, tm2: Term, tp: Term)(implicit stack: Stack) : Boolean = {
    (tm1,tm2) match {
      case (MorphRefl(s1,t1),MorphRefl(s2,t2)) =>
        if (s1 == s2) {
          solver.checkEquality(MorphEval(s1,t1),MorphEval(s2,t2),None)
        }
        else false
      case (_,_) => false
    }
  }
}


/* Solution Rules for Reflected Morphisms */

object SolveEvalMorphReflectionRule extends SolutionRule(ReflectionMorph.eval){
  def apply(solver: Solver)(tm1: Term, tm2: Term)(implicit stack : Stack) : Boolean = {
    tm1 match {
      case MorphEval(s,x) => solver.checkEquality(x,MorphRefl(s,tm2),None)
      case _ => false
    }
  }
}

object SolveReflMorphReflectionRule extends SolutionRule(ReflectionMorph.intro){
  def apply(solver: Solver)(tm1: Term, tm2: Term)(implicit stack : Stack) : Boolean = {
    tm1 match {
      case MorphRefl(s,x) => solver.checkEquality(x,MorphEval(s,tm2),None)
      case _ => false
    }
  }
}

/* Type Checking Rules for Reflected Morphisms */

object MorphReflTypingRule extends TypingRule(ReflectionMorph.rtype){
  def apply(solver: Solver)(tm: Term, tp: Term)(implicit stack : Stack) : Boolean = {
    tp match {
      case MorphReflType(s,a) => solver.checkTyping(MorphEval(s,tm),a)
      case _ => false
    }
  }
}
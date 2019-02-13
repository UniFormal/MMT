package info.kwarc.mmt.lf.externals

import info.kwarc.mmt.lf._
import info.kwarc.mmt.api._
import objects._
import checking._
import uom._

/**
 * Rules for LLF_P as introduced by Honsell, Liquori, Maksimovi, Scagnetto in
 * A logical framework for modeling external evidence, side conditions, and proof irrelevance using monads
 */

/** helper objects for identifiers */
object Locks {
   val _base = Typed._base
   val _path = _base ? "Locks"
}

/** LockType(k,a,b,T) is the term \mathcal{L}^k_{a,b}[T] from the paper for a type T
 *  We also write LockType(K,T) if K=Key(k,a,b) is known.
 */
object LockType extends FouraryConstantScala(Locks._path, "locktype")

/** LockTerm(k,a,b,t) is the term \mathcal{L}^k_{a,b}[t] from the paper for a term t
 *  We also write LockTerm(K,t) if K=Key(k,a,b) is known.
 */
object LockTerm extends FouraryConstantScala(Locks._path, "lockterm")

/** Unlock(t) is the term \mathcal{U}^k_{a,b}[T] from the paper for a terms t: LockType(k,a,b,T)
 *  The additional arguments used in the paper are not actually needed in the internal syntax.
 */
object Unlock extends UnaryConstantScala(Locks._path, "unlock")

/**
 * convenience operator to bundle k,a,b into a single object Key(k, a, b)
 */
object Key extends TernaryConstantScala(Locks._path, "key") {
  def makeKeyDecl(k: Term)(implicit stack: Stack) = {
    val (keyN, _) = Context.pickFresh(stack.context, LocalName("key"))
    VarDecl(keyN, k)
  }
}

/** the abstract type of rules that realize a side condition for a particular key */
abstract class ExternalConditionRule(val head: GlobalName) extends CheckingRule {
  /**
   * checks the condition Key(head,tm,tp)
   * return None if successful, error message otherwise
   */
  def apply(solver: Solver)(context: Context, tm: Term, tp: Term): Option[String]
}

/** K |- T:type  --->  |- LockType(K,T): type */
object InferLockType extends InferenceRule(LockType.path, OfType.path) {
  def apply(solver: Solver)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History) : Option[Term] = {
     val LockType(p, n, s, t) = tm
     if (!covered) {
       solver.check(Typing(stack, n, s))(history.branch)
     }
     solver.inferType(t, covered)(stack ++ Key.makeKeyDecl(Key(p,n,s)), history)
  }
}

/** K |- t:T  ---> |- LockTerm(K,t) : LockType(K,T) */
object InferLockTerm extends InferenceRule(LockTerm.path, OfType.path) {
  def apply(solver: Solver)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History) : Option[Term] = {
     val LockTerm(p, n, s, t) = tm
     if (!covered) {
       solver.check(Typing(stack, n, s))(history.branch)
     }
     solver.inferType(t, covered)(stack ++ Key.makeKeyDecl(Key(p,n,s)), history) map {tI =>
       LockType(p, n, s, tI)
     }
  }
}

/** G |- t: LockType(K,T)   and   K=Key(p,n,s) in G   and  p(a,b)   --->  U: T */
object InferUnlock extends InferenceRule(Unlock.path, OfType.path) {
  def apply(solver: Solver)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History) : Option[Term] = {
     val Unlock(l) = tm
     solver.inferType(l, covered) flatMap {
       case LockType(p,n,s,t) =>
         if (!covered) {
           val isKnown = stack.context.exists {vd =>
             vd.tp match {
               case Some(k @ Key(pC, nC, sC)) =>
                 // must be dry run
                 solver.check(Equality(stack, k, Key(p, n, s), None))
               case _ => false
             }
           }
           if (!isKnown) {
             p match {
               case OMS(pPath) =>
                 solver.rules.getByHead(classOf[ExternalConditionRule], pPath).headOption match {
                   case Some(r) =>
                     r(solver)(stack.context, n, s) match {
                       case None =>
                       case Some(msg) =>
                         solver.error("external condition failed: " + msg)
                     }
                   case None =>
                     solver.error("no rule for predicate " + pPath + " found")
                 }
               case _ => solver.error("predicate must be a symbol")
             }
           }
         }
         Some(t)
       case _ => None
     }
  }
}

/** K |- Unlock(t): T  --->  |- LockType(K,T) */
object TypingLock extends TypingRule(LockType.path) {
  def apply(solver: Solver)(tm: Term, tp: Term)(implicit stack: Stack, history: History) : Option[Boolean] = {
    val LockType(pT, nT, sT, a) = tp
    val key = Key(pT,nT,sT)
    Some(solver.check(Typing(stack ++ Key.makeKeyDecl(key), Unlock(tm), a)))
  }
}


/** K |- Unlock(s) = Unlock(t): T  --->  |- s = t : LockType(K,T) */
object EqualityLock extends ExtensionalityRule(Nil, LockType.path) {
  val introForm = LockTerm
  
  def apply(solver: Solver)(tm1: Term, tm2: Term, tp: Term)(implicit stack: Stack, history: History) = {
    val LockType(pT, nT, sT, a) = tp
    val r = solver.check(Equality(stack ++ Key.makeKeyDecl(Key(pT,nT,sT)), Unlock(tm1), Unlock(tm2), Some(a)))
    Some(r)
  }
}

/** Unlock(LockTerm(K,t) = t */
object UnlockLock extends ComputationRule(Unlock.path) {
  def apply(solver: CheckingCallback)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History) = {
     val Unlock(l) = tm
     l match {
       case LockTerm(p, n, s, t) => Simplify(t)
       case _ => Recurse
     }
  }
}

// *******************  Example (Section 5.1 of paper): call-by-value for lambda_v calculus

/** helper object for identifier */
object CallByValue {
   val _base = Locks._base
   val _path = _base ? "CallByValueExample"
   
   val Val = _path ? "Val"
   val lam = _path ? "lam" 
   val free = _path ? "free"
}

/** Key(Val, tm, tp) checks whether tm is Val N is an abstraction of of the form free(_) */
object ValRule extends ExternalConditionRule(CallByValue.Val) {
  def apply(solver: Solver)(context: Context, tm: Term, tp: Term) = {
    tm match {
      case Apply(OMS(CallByValue.lam),_) =>
        None
      case Apply(OMS(CallByValue.free),_) =>
        None
      case _ =>
        Some(solver.presentObj(tm) + " must be a value")
    }
  }
}
package info.kwarc.mmt.lf.externals

import info.kwarc.mmt.lf._
import info.kwarc.mmt.api._
import objects._
import checking._
import uom._

object External {
   val _base = Typed._base
   val _path = _base ? "External"
}

object LockType extends FouraryConstantScala(External._path, "locktype")

object LockTerm extends FouraryConstantScala(External._path, "lockterm")

object Unlock extends UnaryConstantScala(External._path, "unlock")

object Key extends TernaryConstantScala(External._path, "key") {
  def makeKeyDecl(k: Term)(implicit stack: Stack) = {
    val (keyN, _) = Context.pickFresh(stack.context, LocalName("key"))
    VarDecl(keyN, Some(k), None, None)
  }
}

abstract class ExternalKeyRule(val head: GlobalName) extends SyntaxDrivenRule {
  def apply(solver: Solver)(context: Context, tm: Term, tp: Term): Boolean
}

object TrivRule extends ExternalKeyRule(External._base ? "TrivialKey" ? "triv") {
  //val f = utils.File("C:\\incoming\\test.txt")
  def apply(solver: Solver)(context: Context, tm: Term, tp: Term): Boolean = {
     //utils.File.write(f, utils.File.read(f) + "\n" + solver.presentObj(tm))
     true
  }
}

object InferLockType extends InferenceRule(LockType.path, OfType.path) {
  def apply(solver: Solver)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History) : Option[Term] = {
     val LockType(p, n, s, t) = tm
     if (!covered) {
       solver.check(Typing(stack, n, s))(history.branch)
     }
     solver.inferType(t, covered)(stack ++ Key.makeKeyDecl(Key(p,n,s)), history)
  }
}

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

object InferUnlock extends InferenceRule(Unlock.path, OfType.path) {
  def apply(solver: Solver)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History) : Option[Term] = {
     val Unlock(l) = tm
     solver.inferType(l, covered) flatMap {
       case LockType(p,n,s,t) =>
         if (!covered) {
           val isKnown = stack.context.exists {
             case VarDecl(_, Some(k @ Key(pC, nC, sC)), _, _) =>
               // must be dry run
               solver.check(Equality(stack, k, Key(p, n, s), None))
             case _ => false
           }
           if (!isKnown) {
             p match {
               case OMS(pPath) =>
                 solver.rules.getByHead(classOf[ExternalKeyRule], pPath).headOption match {
                   case Some(r) => r(solver)(stack.context, n, s)
                   case None => solver.error("no rule for predicate " + pPath + " found")
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

object TypingLock extends TypingRule(LockType.path) {
  def apply(solver: Solver)(tm: Term, tp: Term)(implicit stack: Stack, history: History) : Boolean = {
    val LockType(pT, nT, sT, a) = tp
    val key = Key(pT,nT,sT)
    solver.check(Typing(stack ++ Key.makeKeyDecl(key), Unlock(tm), a))
  }
}

object EqualityLock extends TypeBasedEqualityRule(Nil, LockType.path) {
  def apply(solver: Solver)(tm1: Term, tm2: Term, tp: Term)(implicit stack: Stack, history: History) = {
    val LockType(pT, nT, sT, a) = tp
    val r = solver.check(Equality(stack ++ Key.makeKeyDecl(Key(pT,nT,sT)), Unlock(tm1), Unlock(tm2), Some(a)))
    Some(r)
  }
}

object UnlockLock extends ComputationRule(Unlock.path) {
  def apply(solver: CheckingCallback)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History) : Option[Term] = {
     val Unlock(l) = tm
     l match {
       case LockTerm(p, n, s, t) => Some(t)
       case _ => None
     }
  }
}

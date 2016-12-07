package info.kwarc.mmt.sequences

import info.kwarc.mmt.api._
import checking._
import objects._
import objects.Conversions._

import info.kwarc.mmt.lf._

import TypeSequences._
import LFS._
import Nat._

// TODO upgrade natural numbers from examples to official theory; use that instead of urtheory nats
// add proof rules for leq

/* TODO inferring the type of ApplySpine(f, a_1,...,a_n) must work with entire argument sequence if some a_i is a sequence
 * current rules work if sequence arguments always provided as sequence term
 * 
 * maybe an idea for more general treatment: use spines, sequences may not be split across spines
 * if length is
 *  literal: use rules as usual (add flexary ApplyTerm, Beta rules)
 *  unknown: solve using number of arguments in spine
 *  term: unclear
 */

/** |- type ^ n UNIVERSE */
object UniverseNType extends UniverseRule(ntype.path) {
   def apply(solver: Solver)(tm: Term)(implicit stack: Stack, history: History) : Boolean = tm match {
      case TypeSequences.ntype(n) =>
         solver.check(Typing(stack, n, OMS(nat))) //TODO already covered by precondition or universe rules?
   }
}

/** |- type ^ n : kind */
object NTypeTerm extends InferenceRule(ntype.path, OfType.path) {
   def apply(solver: Solver)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History) : Option[Term] = tm match {
      case TypeSequences.ntype(n) =>
         if (!covered)
            solver.check(Typing(stack, n, OMS(nat)))
         Some(OMS(Typed.kind))
      case _ => None
   }
}

/** 
 *  i, i/low, i/up |- t(i) : a_i : type --->  |- [t(i)] i=m ^ n : [a_i] i=m ^ n
 *  i, i/low, i/up |- t(i) : type       --->  |- [t(i)] i=m ^ n : type ^ (n-m+1)
 */
object EllipsisInfer extends InferenceRule(ellipsis.path, OfType.path) {
   private val low = "low"
   private val up  = "up"
   def apply(solver: Solver)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History) : Option[Term] = tm match {
      case LFS.ellipsis(m, n, i, t) =>
         val stackI = stack ++ i%OMS(nat)
         val stackILU = stackI ++ i/low%leq(m, OMV(i)) ++ i/up%leq(OMV(i), n)
         val aOpt = solver.inferType(t)(stackILU , history)
         aOpt flatMap {a =>
            if (a == OMS(Typed.ktype)) {
               val length = if (m == natlit(1)) n else succ(minus(n,m)) // optimize n-m+1 ---> n 
               Some(ntype(length))
            } else {
               val ok = covered || {
                  // generated variables i/low and i/up may not appear in inferred type
                  !a.freeVars.exists(x => x == i/low || x == i/up) &&
                  // inferred types must actually by types
                  solver.check(Typing(stackI, a, OMS(Typed.ktype)))
               }
               if (ok)
                  Some(ellipsis(m,n,i,a))
               else
                  // TODO error message?
                  None
            }
         }
      case _ => None
   }
}

/** 
 *  ... t_i : A_i ...  for i = 1,...,n
 *  ----------------------------------
 *  t_1,...,t_n : A_1,...,A_n
 */
object FlatSeqInfer extends InferenceRule(flatseq.path, OfType.path) {
   def apply(solver: Solver)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History) : Option[Term] = tm match {
      case LFS.flatseq(ts@_*) =>
         val tsI = ts map {t => solver.inferType(t, covered).getOrElse(return None)}
         Some(flatseq(tsI:_*))
      case _ => None
   }
}


/** 
 *  |- s : [t(i)] i=m ^ n : [a_i] i=m ^ n
 *  |- ! : m <= a
 *  |- ! : a <= n
 *  --------------------------------------
 *  |- s.a : t(a)
 *  
 *  |- s : type^n
 *  |- ! : 1 <= a
 *  |- ! : a <= n
 *  --------------------------------------
 *  |- s.a : type
 */
object IndexInfer extends InferenceRule(index.path, OfType.path) {
   def apply(solver: Solver)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History) : Option[Term] = tm match {
      case LFS.index(s, at) =>
         if (!covered)
           solver.check(Typing(stack, at, OMS(Nat.nat)))
         val sTOpt = solver.inferType(s)
         sTOpt flatMap {
            case TypeSequences.ntype(n) =>
               if (covered || Length.checkWithin(solver)(OMS(one), at, n))
                  Some(OMS(Typed.ktype))
               else
                  None
            case LFS.ellipsis(m,n,i,a) =>
               if (covered || Length.checkWithin(solver)(OMS(one), at, n))
                  Some(a ^? (i -> at))
               else
                  None
            case _ => None // TODO decomposition? error message?
         }
      case _ => None
   }
}

/** 
 *  |t| = n-m+1
 *  |- t.i : a(i)  for all i=m,...n
 *  -------------------------------
 *  |- t : [a(i)] i=m ^ n
 *  
 *  applicable only if m and n simplify to literals
 */
object EllipsisTypeCheck extends TypingRule(ellipsis.path) {
  def apply(solver: Solver)(tm: Term, tp: Term)(implicit stack: Stack, history: History) = {
    val equalLengthChecked = Length.checkEqual(solver, tm, tp)
    if (!equalLengthChecked) {
      // TODO delay
    }
    val LFS.ellipsis(m, n, i, a) = tp
    val mS = solver.simplify(m)
    val nS = solver.simplify(n)
    (mS,nS) match {
      case (Nat.natlit(mI),Nat.natlit(nI)) =>
        (mI to nI) forall {iI =>
          val iL = natlit(iI)
          solver.check(Typing(stack, index(tm, iL), a ^? (i -> iL)))
        }
      case _ => ???// TODO delay
    }
  }
}

/** 
 *  |s| = n-m+1
 *  |t| = n-m+1
 *  |- s.i = t.i  for all i=m,...n
 *  -------------------------------
 *  |- s=t : [a(i)] i=m ^ n
 *  
 *  applicable only if m and n simplify to literals
 */
object EllipsisEqualityCheck extends TypeBasedEqualityRule(Nil, ellipsis.path) {
  def apply(solver: Solver)(tm1: Term, tm2: Term, tp: Term)(implicit stack: Stack, history: History): Option[Boolean] = {
    val equalLengthChecked = Length.checkEqual(solver,tm1,tp) && Length.checkEqual(solver,tm2,tp)
    if (!equalLengthChecked) {
      ??? // TODO delay
    }
    // check type of each element
    val LFS.ellipsis(m, n, i, a) = tp
    val mS = solver.simplify(m)
    val nS = solver.simplify(n)
    (mS,nS) match {
      case (Nat.natlit(mI),Nat.natlit(nI)) =>
        val res = (mI to nI) forall {iI =>
          val iL = natlit(iI)
          solver.check(Equality(stack, index(tm1, iL), index(tm2, iL), Some(a ^? (i -> iL))))
        }
        Some(res)
      case _ => ??? // TODO delay
    }
  }
}

/**
 * ([a(i)] i=m ^ n).k  ----> a(m+k-1)
 */
object IndexCompute extends ComputationRule(index.path) {
  def apply(solver: CheckingCallback)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History) : Option[Term] = {
    val LFS.index(s,at) = tm
    at match {
      case Nat.natlit(n) =>
        s match {
          case LFS.ellipsis(m,n,i,t) =>
            if (!covered) {
               Length.checkWithin(solver)(m,at,n)
            }
            val tS = t ^? (i -> minus(plus(m,at), OMS(one)))
            Some(tS)
          case LFS.flatseq(as@_*) =>
            if (!covered) {
              if (n == 0 || n>as.length) {
                solver.error("index out of bounds: " + at)
                None
              }
            }
            Some(as(n.toInt-1))
          case _ => None
        }
      case _ => None
    }
  }
}

/**
 * the beta-style rule
 * [E(i)]_{i=1}^n  --->  E(1),...,E(n)
 * 
 * The rule is applied only in argument sequences and contexts.
 * In contexts, it is only applied if the sequence variable occurs only with literal indices,
 * in which case each x.1 is replaced with x/"1" etc.
 * If the sequence variable occurs in any other way (e.g., as x or x.n), it is not expanded.
 * 
 * It would be easier to use the substitution x->Sequence(x/"1",...,x/"n"), but that is not possible because we avoid concatenation here.
 * Typically, sequence variables occur with indices that are index variables of ellipses. Those ellipses must have been previously simplified. 
 */ 
class ExpandEllipsis(op: GlobalName) extends ComputationRule(op) {

   def apply(solver: CheckingCallback)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History) : Option[Term] = {
      implicit val s = solver
      tm match {
         // turn sequence arguments into argument sequences
         case ApplySpine(f, args) =>
            val argsE = applyList(args)
            if (argsE != args)
               Some(ApplySpine(f, argsE:_*))
            else
               None
         // turn sequence variables into variable sequences
         case OMBINDC(binder, con, args) =>
            val (conE,subs) = applyCont(con)
            val argsS = args map {a => a ^? subs}
            val argsE = applyList(argsS)
            if (conE != con || argsE != args) {
              Some(OMBINDC(binder, conE, argsE))
            } else
              None
      }
   }

   /** flattens an argument sequence */
   private def applyList(tms: List[Term])(implicit solver: CheckingCallback, stack: Stack, history: History): List[Term] = {
     tms.flatMap {
        case TypeSequences.ntype(n) =>
            val nS = solver.simplify(n)
            nS match {
              case Nat.natlit(nL) =>
                (1 to nL.toInt).toList.map(_ => OMS(Typed.ktype))
              case _ => List(ntype(nS)) 
            }
        case LFS.ellipsis(m, n, i, t) =>
            val mS = solver.simplify(m)
            val nS = solver.simplify(n)
            (mS,nS) match {
               case (Nat.natlit(mL),Nat.natlit(nL)) =>
                  ellipsisToList(mL,nL,i, t)
               case _ =>
                  List(ellipsis(mS,nS,i,t))
             }
        case LFS.flatseq(ts@_*) => ts
        case a => List(a)
      }
   }
   
   /** flattens a context and returns the substituion */
   private def applyCont(con: Context)(implicit solver: CheckingCallback, stack: Stack, history: History): (Context,Substitution) = {
      var subs = Substitution.empty
      val newCon = con.mapVarDecls {case (conPrefix, vd) =>
        val vdS = vd ^? subs
        vdS match {
          case VarDecl(name, Some(t), dfs, _) =>
            t match {
               case TypeSequences.ntype(Nat.natlit(n)) =>
                  val types = (1 to n.toInt).toList.map(_ => OMS(Typed.ktype))
                  val (vds,sub) = seqVarToList(name, 1, types, dfs)
                  subs = subs ++ sub
                  vds
               case LFS.ellipsis(Nat.natlit(m),Nat.natlit(n),i,a) =>
                  val types = ellipsisToList(m,n,i,a)
                  val (vds, sub) = seqVarToList(name, m, types, dfs)
                  subs = subs ++ sub
                  vds
               case LFS.flatseq(tps@_*) =>
                  val (vds, sub) = seqVarToList(name, 1, tps.toList, dfs)
                  subs = subs ++ sub
                  vds
               case _ =>
                 subs = subs ++ vdS.name.id
                 List(vdS)
            }
          case v =>
            subs = subs ++ v.name.id
            List(v)
        }
      }
      (newCon.flatten, subs)
   }

   /** turns an ellipsis whose bounds normalizes to a literal into a list of Terms */
   private def ellipsisToList(m: BigInt, n: BigInt, i: LocalName, t: Term): List[Term] =
      (m.toInt to n.toInt).toList.map(x => t ^? (i -> Nat.natlit(x)))

   /**
    * turns a sequence variable (whose type is a sequence of types) into a variable sequence
    * also returns the substitution that maps the sequence variable to the variable sequence
    */
   private def seqVarToList(x: LocalName, from: BigInt, tps: List[Term], df: Option[Term]): (List[VarDecl],Sub) = {
      val vds = (0 until tps.length).toList.map {i =>
         val name = x / (from+i).toString
         VarDecl(name, Some(tps(i)), None, None)
      }
      val vars = flatseq(vds.map(vd => OMV(vd.name)):_*) 
      (vds, x -> vars)
   }
}

/** expands ellipses in the arguments of a Pi */
object FlexaryPi extends ExpandEllipsis(Pi.path)
/** expands ellipses in the arguments of an arrow */
object FlexaryArrow extends ExpandEllipsis(Arrow.path)
/** expands ellipses in the arguments of a lambda */
object FlexaryLambda extends ExpandEllipsis(Lambda.path)
/** expands ellipses in the arguments of an apply */
object FlexaryApply extends ExpandEllipsis(Apply.path)
package info.kwarc.mmt.sequences

import info.kwarc.mmt.api._
import checking._
import objects._
import objects.Conversions._

import info.kwarc.mmt.lf._

import Sequences._
import Nat._

/* TODO inferring the type of ApplySpine(f, a_1,...,a_n) must work with entire argument sequence if some a_i is a sequence
 * current rules work if sequence arguments always provided as sequence term
 * 
 * maybe an idea for more general treatment: use spines, sequences may not be split across spines
 * if length is
 *  literal: use rules as usual (add flexary ApplyTerm, Beta rules)
 *  unknown: solve using number of arguments in spine
 *  term: unclear
 */


/**
 * |- type^n UNIV
 */
object UniverseNType extends UniverseRule(rep.path) {
   def apply(solver: Solver)(tm: Term)(implicit stack: Stack, history: History) : Boolean = {
     val Sequences.rep(t,n) = tm
     solver.check(Typing(stack, n, OMS(nat)))(history.branch) //TODO already covered by precondition or universe rules?
     t match {
       case OMS(Typed.ktype) => true
       case _ => solver.error("not a universe: " + tm)
     }
   }
}

/** |- type ^ n : kind */
object NTypeTerm extends InferenceRule(rep.path, OfType.path) {
   def apply(solver: Solver)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History) : Option[Term] = {
     val Sequences.rep(t,n) = tm
     if (!covered)
        solver.check(Typing(stack, n, OMS(nat)))(history.branch)
     t match {
      case OMS(Typed.ktype) => Some(OMS(Typed.kind))
      case t =>
        solver.inferType(t, covered) map {tI => rep(tI, n)}
     }
   }
}

/** 
 *  i, i/up |- t(i) : a_i : type --->  |- [t(i)] i=0^n : [a_i] i=0^n
 *  i, i/up |- t(i) : type       --->  |- [t(i)] i=0^n : type^n
 */
object EllipsisInfer extends InferenceRule(ellipsis.path, OfType.path) {
   private val up  = "up"
   def apply(solver: Solver)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History) : Option[Term] = tm match {
      case Sequences.ellipsis(n, i, t) =>
         val stackI = stack ++ i%OMS(nat)
         val stackILU = stackI ++ i/up%lessType(OMV(i), n)
         val aOpt = solver.inferType(t)(stackILU , history.branch)
         aOpt flatMap {a =>
            if (a == OMS(Typed.ktype)) {
               // sequences of types
               Some(rep(a,n))
            } else {
               // sequences of typed terms
               val ok = covered || {
                  // generated variables i/low and i/up may not appear in inferred type
                  !a.freeVars.contains(i/up) &&
                  // inferred types must actually by types
                  solver.check(Typing(stackI, a, OMS(Typed.ktype)))(history.branch)
               }
               if (ok)
                  Some(ellipsis(n,i,a))
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
      case Sequences.flatseq(ts@_*) =>
         val tsI = ts map {t => solver.inferType(t, covered)(stack, history.branch).getOrElse(return None)}
         if (tsI.distinct.length == 1)
           Some(rep(tsI.head, natlit(tsI.length))) // optimization if all types are the same
         else
           Some(flatseq(tsI:_*))
      case _ => None
   }
}


/** 
 *  |- s : a
 *  |- i < |a|
 *  -------------
 *  |- s.i : a.i
 */
object IndexInfer extends InferenceRule(index.path, OfType.path) {
   def apply(solver: Solver)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History) : Option[Term] = {
     val Sequences.index(s, at) = tm
     if (!covered)
        solver.check(Typing(stack, at, OMS(Nat.nat)))(history.branch)
     val sTOpt = solver.inferType(s)(stack, history + "inferring type of sequence")
     sTOpt match {
       case Some(sT) =>
          if (!covered) {
            Length.infer(solver, s) match {
              case None => return None
              case Some(sL) => Length.checkBelow(solver)(at, sL)
            }
          }
          if (sT == OMS(Typed.kind))
             // special case for (type^n).i (which should never actually occur but is conceivable) 
             Some(OMS(Typed.kind))
          else
            Some(index(sT, at))
       case _ => None // TODO decomposition? error message?
     }
   }
}

/**
 * ([a(i)] i=1^n).k  ----> a(k)
 * (a^n).i           ----> a
 * (a_1,...,a_n).k  -----> a_k    if k literal
 */
object IndexCompute extends ComputationRule(index.path) {
  def apply(solver: CheckingCallback)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History) : Option[Term] = {
    val Sequences.index(s,at) = tm
    val nO = Length.infer(solver, s)
    if (!covered) {
       val n = nO getOrElse {return None}
       Length.checkBelow(solver)(at,n)
    }
    s match {
      case Sequences.ellipsis(n,i,t) =>
        val tS = t ^? (i -> at)
        Some(tS)
      case Sequences.rep(a,n) =>
        Some(a)
      case Sequences.flatseq(as@_*) =>
        at match {
          case Nat.natlit(l) =>
            Some(as(l.toInt))
          case _ => None
        }
      case _ => None
    }
  }
}

/** 
 *  component-wise type-checking of a sequence
 *  
 *  |t| = |a|
 *  |- t.i : a.i  for all i=0,...,n-1
 *  ---------------------------------
 *  |- t : a
 *  
 *  applicable only if |a| simplifies to a literal
 */
class SequenceTypeCheck(op: GlobalName) extends TypingRule(op) {
  def apply(solver: Solver)(tm: Term, tp: Term)(implicit stack: Stack, history: History): Boolean = {
    val equalLength = Length.checkEqual(solver, tm, tp).getOrElse {
      throw DelayJudgment("length not known")
    }
    if (!equalLength) return false
    val n = Length.infer(solver, tp).get
    val nS = solver.simplify(n)
    nS match {
      case Nat.natlit(nI) =>
        (BigInt(0) until nI) forall {iI =>
          val iL = natlit(iI)
          solver.check(Typing(stack, index(tm, iL), index(tp, iL)))
        }
      case _ => throw DelayJudgment("length not known")
    }
  }
}

object EllipsisTypeCheck extends SequenceTypeCheck(ellipsis.path)
object RepTypeCheck extends SequenceTypeCheck(rep.path)

/**
 *  component-wise equality-checking of sequences
 *  
 *  |s| = |t| = |a|
 *  |- s.i = t.i : a.i for all i=1,...,n-1
 *  --------------------------------------
 *  |- s=t : a
 *  
 *  applicable only if |a| simplifies to a literal
 */
class SequenceEqualityCheck(op: GlobalName) extends TypeBasedEqualityRule(Nil, op) {
  def apply(solver: Solver)(tm1: Term, tm2: Term, tp: Term)(implicit stack: Stack, history: History): Option[Boolean] = {
    val equalLength = List(tm1,tm2).map {tm => Length.checkEqual(solver,tm,tp).getOrElse {
      throw DelayJudgment("length not known")
    }}
    if (equalLength contains false) {
      return Some(false)
    }
    // check type of each element
    val n = Length.infer(solver, tp).get
    val nS = solver.simplify(n)
    nS match {
      case Nat.natlit(nI) =>
        val res = (BigInt(0) until nI) forall {iI =>
          val iL = natlit(iI)
          solver.check(Equality(stack, index(tm1, iL), index(tm2, iL), Some(index(tp, iL))))
        }
        Some(res)
      case _ => throw DelayJudgment("length not a literal")
    }
  }
}

object EllipsisEqualityCheck extends SequenceEqualityCheck(ellipsis.path)
object RepEqualityCheck extends SequenceEqualityCheck(rep.path)


/** a^n ---> [a]i=0^n for fresh i */
object ExpandRep extends ComputationRule(rep.path) {
   def apply(solver: CheckingCallback)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History) : Option[Term] = {
     val Sequences.rep(t,n) = tm
     if (t == OMS(Typed.ktype))
       None
     else {
       val e = ellipsis(n, OMV.anonymous, t)
       Some(e)
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
         case _ => None
      }
   }

   /** flattens an argument sequence */
   private def applyList(tms: List[Term])(implicit solver: CheckingCallback, stack: Stack, history: History): List[Term] = {
     tms.flatMap {
        case Sequences.rep(t, Nat.natlit(n)) =>
           (BigInt(0) until n).toList.map(_ => t)
        case Sequences.ellipsis(Nat.natlit(n), i, t) =>
            ellipsisToList(n, i, t)
        case Sequences.flatseq(ts@_*) => ts
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
               case Sequences.rep(tp, Nat.natlit(n)) =>
                  val types = (BigInt(0) until n).toList.map(_ => tp)
                  val (vds,sub) = seqVarToList(name, types, dfs)
                  subs = subs ++ sub
                  vds
               case Sequences.ellipsis(Nat.natlit(n),i,a) =>
                  val types = ellipsisToList(n,i,a)
                  val (vds, sub) = seqVarToList(name, types, dfs)
                  subs = subs ++ sub
                  vds
               case Sequences.flatseq(tps@_*) =>
                  val (vds, sub) = seqVarToList(name, tps.toList, dfs)
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
   private def ellipsisToList(n: BigInt, i: LocalName, t: Term): List[Term] =
      (BigInt(0) until n).toList.map(x => t ^? (i -> Nat.natlit(x)))

   /**
    * turns a sequence variable (whose type is a sequence of types) into a variable sequence
    * also returns the substitution that maps the sequence variable to the variable sequence
    */
   private def seqVarToList(x: LocalName, tps: List[Term], df: Option[Term]): (List[VarDecl],Sub) = {
      val vds = (BigInt(0) until tps.length).toList.map {i =>
         val name = x / i.toString
         VarDecl(name, Some(tps(i.toInt)), None, None)
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
/** expands ellipses in the arguments of a composition */
object FlexaryComposition extends ExpandEllipsis(comp.path)

/**
 * s.i : A.i -> B.i for i=0,...,n
 * A_{i+1} = B.i for i=0,...,n-1
 * ------------------------------
 * comp(s) : A.0 -> B.n
 */
object FlexaryCompositionInfer extends InferenceRule(comp.path, OfType.path) {
   def apply(solver: Solver)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History) : Option[Term] = {
     val Sequences.comp(s) = tm
     val n = Length.infer(solver, s).getOrElse{return None}
     val (i,_) = Common.pickFresh(solver, LocalName("i"))
     val iT = OMV(i)
     solver.inferType(index(s, i), covered)(stack++i%n, history + "infering type of functions") flatMap {fT =>
        val fTS = Common.makePi(solver, fT)
        fTS match {
           case Pi(x,a,b) =>
             if (b.freeVars contains x) {
               solver.error("can only compose simple functions")
               return None
             }
             // m = pred(n), i.e., succ(m) = n
             val (m,_) = Common.pickFresh(solver, LocalName("pred"))
             solver.defineByConstraint(m, OMS(nat)){mT => 
               solver.check(Equality(stack, succ(mT), n, Some(OMS(nat))))(history + "obtaining predecessor of length")
             }
             // A.{i+1} = B.i 
             val mT = OMV(m)
             val j = Equality(stack ++ i%mT, index(a,succ(iT)), index(b, iT), None)
             solver.check(j)(history + "checking composability")
             // A.0 -> B.m
             val tp = Arrow(index(a, OMS(zero)), index(b, mT))
             Some(tp)
           case _ =>
             solver.error("argument of composition must be a function")
             None
        }
     }
   }
}
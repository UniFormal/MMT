package info.kwarc.mmt.sequences

import info.kwarc.mmt.api.{checking, _}
import checking._
import uom._
import objects._
import objects.Conversions._
import info.kwarc.mmt.lf._
import Sequences._
import Nat._
import NatRules.NatLit

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
   def apply(solver: Solver)(tm: Term)(implicit stack: Stack, history: History) : Option[Boolean] = {
     val Sequences.rep(t,n) = tm
     solver.check(Typing(stack, n, OMS(nat)))(history.branch) //TODO already covered by precondition or universe rules?
     t match {
       case OMS(Typed.ktype) => Some(true)
       case _ => Some(solver.error("not a universe: " + tm))
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
   def apply(solver: Solver)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History) : Option[Term] = tm match {
     case Sequences.ellipsis(NatLit(n), i, t) if n == BigInt(1) =>
       val aOpt = solver.inferType(t ^? (i / NatLit(0)))
       aOpt flatMap {a =>
         if (a == OMS(Typed.ktype)) {
           // sequences of types
           Some(rep(a,NatLit(1)))
         } else {
           // sequences of typed terms
           val ok = covered || {
               // inferred types must actually by types
               solver.check(Typing(stack, a, OMS(Typed.ktype)))(history.branch)
           }
           if (ok)
             Some(ellipsis(NatLit(1),i,a))
           else
           // TODO error message?
             None
         }
       }

      case Sequences.ellipsis(n, _, t) =>
         val (i,sub) = pickFreshIndexVar(solver, tm)
         val stackI = stack ++ i%OMS(nat)
         val iup = upBoundName(i)
         val stackILU = stackI ++ iup%lessType(OMV(i), n)
         val aOpt = solver.inferType(t ^? sub)(stackILU , history)
         aOpt flatMap {a =>
            if (a == OMS(Typed.ktype)) {
               // sequences of types
               Some(rep(a,n))
            } else {
               // sequences of typed terms
               val ok = covered || {
                  // generated variables i/low and i/up may not appear in inferred type
                  !a.freeVars.contains(iup) &&
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
           Some(rep(tsI.head, NatLit(tsI.length))) // optimization if all types are the same
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
  def apply(solver: Solver)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History): Option[Term] = tm match {
    case Sequences.index(s, at) =>
      if (!covered)
        solver.check(Typing(stack, at, OMS(Nat.nat)))(history.branch)
      val sTOpt = solver.inferType(s)(stack, history + "inferring type of sequence")
      sTOpt match {
        case Some(sT) =>
          if (!covered) {
            Length.infer(solver, s) match {
              case None =>
                history += "cannot infer length"
                return None
              case Some(sL) =>
                Length.checkBelow(solver)(at, sL)
            }
          }
          if (sT == OMS(Typed.kind))
          // special case for (type^n).i (which should never actually occur but is conceivable)
            Some(OMS(Typed.kind))
          else
            Some(index(sT, at))
        case _ => None // TODO decomposition? error message?
      }
    case _ =>
      None
  }
}

/**
 * ([a(i)] i=1^n).k  ----> a(k)
 * (a^n).i           ----> a
 * (a_1,...,a_n).k   ----> a_k    if k literal
 * a.0               ----> a      if |a|=1     // every plain term can be seen as a sequence of length 1
 */
object IndexCompute extends ComputationRule(index.path) {
  def apply(solver: CheckingCallback)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History): Simplifiability = {
    val Sequences.index(s,at) = tm
    val nO = Length.infer(solver, s)
    if (!covered) {
       val n = nO getOrElse {return Recurse}
       Length.checkBelow(solver)(at,n)
    }
    s match {
      case Sequences.ellipsis(n,i,t) =>
        val tS = t ^? (i -> at)
        Simplify(tS)
      case Sequences.rep(a,n) =>
        Simplify(a)
      case Sequences.flatseq(as@_*) =>
        at match {
          case NatLit(l) =>
            Simplify(as(l.toInt))
          case _ => RecurseOnly(List(2))
        }
      case s =>
        if ((nO contains OMS(one)) && at == NatLit(0)) {
          Simplify(s)
        } else
          RecurseOnly(List(1))
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
  def apply(solver: Solver)(tm: Term, tp: Term)(implicit stack: Stack, history: History): Option[Boolean] = {
    val equalLength = Length.checkEqual(solver, tm, tp).getOrElse {
      throw DelayJudgment("length not known")
    }
    if (!equalLength) return Some(false)
    val n = Length.infer(solver, tp).get
    val nS = solver.safeSimplifyUntil(n)(NatLit.unapply)._1
    nS match {
      case NatLit(nI) =>
        Some((BigInt(0) until nI) forall {iI =>
          val iL = NatLit(iI)
          solver.check(Typing(stack, index(tm, iL), index(tp, iL)))
        })
      case OMV(x) =>
        val i = pickFreshIndexVar(solver, tm)._1
        val iV = OMV(i)
        val iup = upBoundName(i)
        val newCon = stack ++ i%OMS(nat) ++ iup%lessType(iV, nS)
        Some(solver.check(Typing(newCon, index(tm, iV), index(tp, iV))))
      case _ => throw DelayJudgment("length not a literal")
    }
  }
}

object EllipsisTypeCheck extends SequenceTypeCheck(ellipsis.path)
object RepTypeCheck extends SequenceTypeCheck(rep.path)

class SequenceSubtypeCheck(op: GlobalName) extends VarianceRule(op) {
  override def applicable(tp1: Term, tp2: Term) = (tp1,tp2) match {
    case (ComplexTerm(a, _,_,_), _) if this.heads.contains(a) => true
    case (_, ComplexTerm(b, _, _, _)) if this.heads.contains(b) => true
    case _ => false
  }
  override def apply(solver: Solver)(tp1: Term, tp2: Term)(implicit stack: Stack, history: History): Option[Boolean] = {
    val equalLength = Length.checkEqual(solver, tp1, tp2)
    equalLength match {
      case Some(false) =>
        return Some(false)
      case None =>
        throw DelayJudgment("length not known")
      case _ =>
    }
    val n = Length.infer(solver, tp1).get
    val nS = solver.simplify(solver.safeSimplifyUntil(n)(NatLit.unapply)._1)
      // TODO solver.safeSimplify does not apply semantic operators => S(0) is not simplified to 1 and hence does not
      // match against NatLit
    nS match {
      case NatLit(nI) =>
        Some((BigInt(0) until nI) forall {iI =>
          val iL = NatLit(iI)
          solver.check(Subtyping(stack, index(tp1, iL), index(tp2, iL)))
        })
      case OMV(x) =>
        val i = pickFreshIndexVar(solver, tp1)._1
        val iV = OMV(i)
        val iup = upBoundName(i)
        val newCon = stack ++ i%OMS(nat) ++ iup%lessType(iV, nS)
        Some(solver.check(Subtyping(newCon,index(tp1,iV),index(tp2,iV))))
      case _ =>
        throw DelayJudgment("length not a literal")
    }
  }
}

object EllipsisSubtype extends SequenceSubtypeCheck(ellipsis.path)
object RepSubtype extends SequenceSubtypeCheck(rep.path)

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
class SequenceEqualityCheck(op: GlobalName) extends ExtensionalityRule(Nil, op) {
  val introForm = new {def unapply(tm: Term) = tm match {
    case Sequences.ellipsis(x, _, _) => Some(x)
    case Sequences.rep(x, _) => Some(x)
    case Sequences.flatseq(x) => Some(x)
    case _ => None 
  }}
  
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
      case NatLit(nI) =>
        val res = (BigInt(0) until nI) forall {iI =>
          val iL = NatLit(iI)
          solver.check(Equality(stack, index(tm1, iL), index(tm2, iL), Some(index(tp, iL))))
        }
        Some(res)
      case _ => throw DelayJudgment("length not a literal")
    }
  }
}

object EllipsisEqualityCheck extends SequenceEqualityCheck(ellipsis.path)
object RepEqualityCheck extends SequenceEqualityCheck(rep.path)

object EllipsisInjective extends CongruenceRule(ellipsis.path)

object FlatseqInjective extends CongruenceRule(flatseq.path)

/** a^n ---> [a]i=0^n for fresh i */
object ExpandRep extends ComputationRule(rep.path) {
   def apply(solver: CheckingCallback)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History) = {
     val Sequences.rep(t,n) = tm
     if (t == OMS(Typed.ktype))
       Simplifiability.NoRecurse
     else {
       val e = ellipsis(n, OMV.anonymous, t)
       Simplify(e)
     }
   }
}

/** inverse of ExpandRep, useful for complification, should not be used during simplification */
object ContractRep extends SimplificationRule(rep.path) {
  def apply(c: Context, t: Term): Simplifiability = {
    t match {
      case Sequences.ellipsis(n, x, t) if ! t.freeVars.contains(x) => Simplify(rep(t,n))
      case _ => Recurse
    }
  }
  override def complificationOnly = true
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
   def apply(solver: CheckingCallback)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History) = {
      implicit val s = solver
      tm match {
         // turn sequence arguments into argument sequences
         case OMA(f, args) =>
            val argsE = ExpandEllipsis.applyList(args)
            if (argsE != args)
              Simplify(OMA(f, argsE))
            else
              Simplifiability.NoRecurse
         // turn sequence variables into variable sequences
         case OMBINDC(binder, con, args) =>
            val (conE,subs) = ExpandEllipsis.applyCont(con)
            val argsS = args map {a => a ^? subs}
            val argsE = ExpandEllipsis.applyList(argsS)
            if (conE != con || argsE != args) {
              Simplify(OMBINDC(binder, conE, argsE))
            } else
              Simplifiability.NoRecurse
         case _ => Simplifiability.NoRecurse
      }
   }
}

object ExpandEllipsis {
   /** flattens an argument sequence */
   def applyList(tms: List[Term])(implicit solver: CheckingCallback, stack: Stack, history: History): List[Term] = {
     tms.flatMap {
        case Sequences.rep(t, NatLit(n)) =>
           (BigInt(0) until n).toList.map(_ => t)
        case Sequences.ellipsis(NatLit(n), i, t) =>
            ellipsisToList(n, i, t)
        case Sequences.flatseq(ts@_*) => ts
        case a => List(a)
      }
   }

   /** flattens a context and returns the substitution */
   def applyCont(con: Context)(implicit solver: CheckingCallback, stack: Stack, history: History): (Context,Substitution) = {
      var subs = Substitution.empty
      val newCon = con.mapVarDecls {case (conPrefix, vd) =>
        val vdS = vd ^? subs
        vdS match {
          case VarDecl(name, _, Some(t), dfs, _) =>
            t match {
               case Sequences.rep(tp, NatLit(n)) =>
                  val types = (BigInt(0) until n).toList.map(_ => tp)
                  val (vds,sub) = seqVarToList(name, types, dfs)
                  subs = subs ++ sub
                  vds
               case Sequences.ellipsis(NatLit(n),i,a) =>
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
      (BigInt(0) until n).toList.map(x => t ^? (i -> NatLit(x)))

   /**
    * turns a sequence variable (whose type is a sequence of types) into a variable sequence
    * also returns the substitution that maps the sequence variable to the variable sequence
    */
   private def seqVarToList(x: LocalName, tps: List[Term], df: Option[Term]): (List[VarDecl],Sub) = {
      val vds = (BigInt(0) until tps.length).toList.map {i =>
         val name = x / i.toString
         VarDecl(name, tps(i.toInt))
      }
      val vars = flatseq(vds.map(vd => OMV(vd.name)):_*)
      (vds, x -> vars)
   }
}

/** expands ellipses in the arguments of a Pi */
object FlexaryPi extends ExpandEllipsis(Pi.path) with PiOrArrowRule
/** expands ellipses in the arguments of a lambda */
object FlexaryLambda extends ExpandEllipsis(Lambda.path)
/** expands ellipses in the arguments of an apply */
object FlexaryApply extends ExpandEllipsis(Apply.path)
/** expands ellipses in the arguments of a composition */
object FlexaryComposition extends ExpandEllipsis(comp.path)

/**
 * Restriction of StandardArgumentChecker that requires that expected and provided length agree
 */
object LengthAwareArgumentChecker extends ArgumentChecker {
   def apply(solver: CheckingCallback)(tm: Term, tp: Term, covered: Boolean)(implicit stack: Stack, history: History): Boolean = {
      def sameLength = {
        history += "argument and expected type have the same length"
        StandardArgumentChecker(solver)(tm, tp, covered)
      }
      val tmLO = Length.infer(solver, tm)
      val tpLO = Length.infer(solver, tp)
      // try equating the lengths
      (tmLO, tpLO) match {
        case (Some(tmL),Some(tpL)) =>
          val r = solver.tryToCheckWithoutDelay(Equality(stack,tmL,tpL,Some(OMS(Nat.nat))))
          r match {
            case Some(true) =>
              return sameLength
            case _ =>
          }
        case _ =>
      }
      solver.tryToCheckWithoutDelay(Typing(stack, tm, tp)) match {
        case Some(true) =>
          sameLength
        case _ => false
      }
   }
}

object LengthAwareApplyTerm extends GenericApplyTerm(LengthAwareArgumentChecker) {
   /** LF's rule must be shadowed because it would allow not-length-matching inference */
   override def shadowedRules = List(ApplyTerm)
}

object LengthAwareBeta extends GenericBeta(LengthAwareArgumentChecker) {
   /** LF's beta-rule must be shadowed because it would allow not-length-matching reduction */
   override def shadowedRules = List(Beta)
}


/** matches an operator that expects sequence arguments against an argument sequence
 *
 *  this rule is called before ApplyTerm, solves unknown arity arguments, then fails
 *  once the arities are solved, ApplyTerm can do the rest
 */
object SolveArity extends InferenceRule(Apply.path, OfType.path) {
   /** make sure this is called before the usual rule */
   override val priority = 5
   def apply(solver: Solver)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History): Option[Term] = {
      history += solver.presentObj(tm)
      val (op, args) = tm match {
        case ApplySpine(OMS(op), args) => (op,args)
        case _ => throw Backtrack("operator not symbol")
      }
      val opTp = solver.getType(op) getOrElse {throw Backtrack("operator has no type")}
      val FunType(expTps,ret) = opTp
      if (expTps.length == args.length)
        throw Backtrack("number of (possibly sequence) arguments matches expected number; no arity solving needed")
      // there are more argument sequences than expected: some arguments must be grouped into sequences
      val expNats = expTps.takeWhile {case (n,tp) => n.isDefined && tp == OMS(nat)}
      if (expNats.length != 1)
        throw Backtrack("can only solve for a single natural number") // TODO multiple nats; nats that do not occur at beginning
      val n = expNats.head._1.get
      if (solver.Unknown.unapply(args.head).isEmpty)
        throw Backtrack("arity already known")
      //val expLs = expTps map {case (_,tp) => Length.infer(solver, tp).getOrElse{return None}}
      // Length.infer(a) == None if a is bound variable with omitted type: heuristically assume length 1
      val argLs = args map {a => Length.infer(solver, a).getOrElse{OMS(one)}}
      if (argLs exists (_ != OMS(one))) {
         // TODO case where some arguments are given as sequences
         /*if (args.length == expTp.length) {
           val tpLs = expTps map {case ()
           val lengthsMatch = (argLs zip expTpLs)
         }*/
         history += "can only handle handle pure argument sequences"
         throw Backtrack("can't solve arity, assuming argument grouping matches expected grouping")
      }
      val nS = NatLit(args.tail.length)
      solver.check(Equality(stack, args.head, nS, Some(OMS(nat))))(history + "solving by number of arguments")
      throw Backtrack("solved arity as " + args.tail.length + ", defering to other rules")
   }
}

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

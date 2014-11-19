package info.kwarc.mmt.sequences

import info.kwarc.mmt.api._
import checking._
import objects._
import objects.Conversions._

import info.kwarc.mmt.lf._

import LFS._

/* idea: use spines, sequences may not be split across spines
 * if length is
 *  literal: use rules (add flexary ApplyTerm, Beta rules)
 *  unknown: solve using number of arguments in spine
 *  term: unclear
 */

/** |- type ^ n UNIVERSE */
object UniverseNType extends UniverseRule(ntype.path) {
   def apply(solver: Solver)(tm: Term)(implicit stack: Stack, history: History) : Boolean = tm match {
      case LFS.ntype(n) =>
         solver.check(Typing(stack, n, OMS(LFS.nat))) //TODO already covered by precondition or universe rules?
   }
}

/** |- type ^ n : kind */
object NTypeTerm extends InferenceRule(ntype.path, OfType.path) {
   def apply(solver: Solver)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History) : Option[Term] = tm match {
      case LFS.ntype(n) =>
         solver.check(Typing(stack, n, OMS(nat)))
         Some(OMS(Typed.kind))
      case _ => None
   }
}

/** |- nat INHABITABLE */
object NatInhabitable extends InhabitableRule(nat) {
   def apply(solver: Solver)(tm: Term)(implicit stack: Stack, history: History) : Boolean = tm match {
      case OMS(LFS.nat) => true
   }
}

/** 
 *  i, i/low, i/up |- t_i : a_i : type --->  |- [t_i] i=m ^ n : [a_i] i=m ^ n
 *  i, i/low, i/up |- t_i : type       --->  |- [t_i] i=m ^ n : type ^ (n-m)
 */
object EllipsisInfer extends InferenceRule(ellipsis.path, OfType.path) {
   private val low = "low"
   private val up  = "up"
   def apply(solver: Solver)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History) : Option[Term] = tm match {
      case LFS.ellipsis(m, n, i, t) =>
         val stackI = stack ++ i%OMS(nat)
         val stackILU = stackI ++ i/low%leq(m, OMV(i)) ++ i/up%leq(OMV(i), m)
         val aOpt = solver.inferType(t)(stackILU , history)
         aOpt flatMap {a =>
            if (a == OMS(Typed.ktype))
               Some(ntype(n))
            else {
               val ok = if (covered) true else {
                  val vars = !a.freeVars.exists(x => x == i/low || x == i/up)
                  vars && solver.check(Typing(stackI, a, OMS(Typed.ktype)))
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


class NatSolver(stack: Stack) {
   private var lows: List[(Term,LocalName)] = Nil
   private var ups : List[(LocalName,Term)] = Nil
   private def add(l: Term, u: Term) {(l,u) match {
      case (succ(x), succ(y)) => add(x,y)
      case (OMV(x),OMV(y)) =>
         lows ::= (l, y)
         ups  ::= (x, u)
      case (l, OMV(y)) =>
         lows ::= (l, y)
      case (OMV(x),u) =>
         ups  ::= (x, u)
      case _ =>
   }}
   private def init {
      stack.context.foreach {
         case VarDecl(_, Some(LFS.leq(l,u)), _ ,_) => add(l,u)
         case _ =>
      }
   }
   init

   //TODO check for cycles
   def apply(x: Term, y: Term): Option[Boolean] = (x,y) match {
      case (x,y) if x == y => Some(true)
      case (succ(m), succ(n)) => apply(m,n)
      case (LFS.zero,_) => Some(true)
      case (succ(_), LFS.zero) => Some(false)
      case (OMV(vx),OMV(vy)) => if (useUpper(vx,y) || useLower(x, vy)) Some(true) else None
      case (OMV(vx),_) =>  if (useUpper(vx,y)) Some(true) else None
      case (_, OMV(vy)) => if (useLower(x, vy)) Some(true) else None
      case _ => None
   }
   private def useUpper(vx: LocalName, y: Term) = ups .exists {case (v,up)  => v == vx && apply(up,y) == Some(true)}
   private def useLower(x: Term, vy: LocalName) = lows.exists {case (low,v) => v == vy && apply(x,low) == Some(true)}
}

object IndexInfer extends InferenceRule(index.path, OfType.path) {
   private def checkWithin(solver: Solver)(low: Term, t: Term, up: Term)(implicit stack: Stack, history: History): Boolean = {
      val ns = new NatSolver(stack)
      ns.apply(low, t) == Some(true) && ns.apply(t, up) == Some(true)
   }
   def apply(solver: Solver)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History) : Option[Term] = tm match {
      case LFS.index(s, at) =>
         val sTOpt = solver.inferType(s)
         sTOpt flatMap {
            case LFS.ntype(n) =>
               if (covered || checkWithin(solver)(OMS(one), at, n))
                  Some(OMS(Typed.ktype))
               else
                  None
            case LFS.ellipsis(m,n,i,a) =>
               if (covered || checkWithin(solver)(OMS(one), at, n))
                  Some(a ^? (i -> at))
               else
                  None
            case _ => None // TODO error message?
         }
      case _ => None
   }
}

/**
 * the beta-style rule [E(i)]_{i=1}^n  --->  E(1),...,E(n)
 * 
 * The rule is applied only in argument sequences and contexts.
 * In contexts, it is only applied if the sequence variable occurs only with literal indices,
 * in which case each x.1 is replaced with x/"1" etc.
 * If the sequence variable occurs in any other way (e.g., as x or x.n), it is not expanded. 
 */ 
// TODO binder cases are not called if head is used to filter rules
object ExpandEllipsis extends ComputationRule(Apply.path) {

   /** turns an ellipsis whose bounds normalizes to a literal into a list of Terms */
   private def ellipsisToList(m: BigInt, n: BigInt, i: LocalName, t: Term): List[Term] =
      (m.toInt to n.toInt).toList.map(x => t ^? (i/LFS.natlit(x)))

   /** turns a sequence variable whose type normalizes to a list of types into a list of VarDecls */
   private def seqVarToList(x: LocalName, from: BigInt, tps: List[Term]): List[VarDecl] = {
      (0 until tps.length).toList.map {i =>
         val name = x / (from+i).toString
         VarDecl(name, Some(tps(i)), None, None)
      }
   }
   
   private type VarSet = scala.collection.mutable.Set[LocalName]
   /** collects all variables that cannot be expanded */
   private object NonExpandable extends Traverser[VarSet] {
      def traverse(t: Term)(implicit context : Context, state: VarSet) = t match {
         case LFS.index(OMV(x), i) =>
            i match {
               // seq vars with a literal index can be expanded
               case LFS.natlit(_) =>
               case _ => state.add(x)
            }
            t
        case OMA(f, args) =>
           Traverser(this, f)
           args.foreach {
              // seq vars in an argument list can be expanded
              case OMV(x) =>
              case a => Traverser(this, a)
           }
           t
        //TODO case OMBINDC(binder, con, args) =>
           // seq vars that type an expandable seq var can be expanded
        case OMV(x) =>
           state.add(x)
           t
        case t => Traverser(this, t)
      }
   }
   /** replaces sequence variables with variable sequences
    *  @param expansions the list of replaced sequence variables paired with the replacing variable sequence
    */
   private class SeqVarReplacer(expansions: List[(VarDecl, List[VarDecl])]) extends StatelessTraverser {
      private def replace(x: LocalName)(implicit con: Context) =
         !con.isDeclared(x) && expansions.exists(_._1.name == x)
      def traverse(t: Term)(implicit con : Context, state: Unit) = t match {
         // replace x.1 with x/"1"
         case LFS.index(OMV(x), LFS.natlit(i)) if replace(x) =>
            OMV(x/i.toString)
         case OMA(f, args) =>
            val argsS = args.flatMap {
               // replace x in argument list with x/"1", ..., x/"n"
               case OMV(x) if replace(x) =>
                  expansions.find(_._1.name == x).get._2.map(vd => OMV(vd.name))
               case a => List(Traverser(this, a))
            }
            OMA(Traverser(this, f), argsS)
         // TODO: replace y:x with y/"1":x/"1", ...
         case t => Traverser(this, t)
      }
   }
   
   def apply(solver: CheckingCallback)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History) : Option[Term] = {
      tm match {
         case ApplySpine(f, args) =>
            val argsS = args.flatMap {
               case LFS.ntype(LFS.natlit(n)) =>
                  (1 to n.toInt).toList.map(_ => OMS(Typed.ktype))
               case LFS.ellipsis(m, n, i, t) =>
                  val mS = solver.simplify(m)
                  val nS = solver.simplify(n)
                  (mS,nS) match {
                     case (LFS.natlit(mL),LFS.natlit(nL)) =>
                        ellipsisToList(mL,nL,i, t)
                   }
               case a => List(a)
            }
            if (argsS != args)
               Some(ApplySpine(f, argsS:_*))
            else
               None
         case OMBINDC(binder, con, args) =>
            // check which seq vars can be expanded 
            val nonexp = new scala.collection.mutable.HashSet[LocalName]
            args.foreach {a => NonExpandable(a, nonexp)}
            def expandable(n: LocalName) = ! nonexp.contains(n)
            // expansions stores the seq vars that will be expanded
            var expansions: List[(VarDecl, List[VarDecl])] = Nil
            // conExp is con with all expandable seq vars expanded
            val conExp: Context = con.flatMap {
               case vd @ VarDecl(name, Some(t), None, _) if expandable(name) =>
                  t match {
                     case LFS.ntype(LFS.natlit(n)) =>
                        val types = (1 to n.toInt).toList.map(_ => OMS(Typed.ktype))
                        val vds = seqVarToList(name, 1, types)
                        expansions = (vd, vds) :: expansions
                        vds
                     case LFS.ellipsis(LFS.natlit(m),LFS.natlit(n),i,a) =>
                        val types = ellipsisToList(m,n,i,a)
                        val vds = seqVarToList(name, m, types)
                        expansions = (vd, vds) :: expansions
                        vds
                     case _ => List(vd)
                  }
               case vd => List(vd)
            }
            if (expansions.isEmpty)
               return None
            // argsExp is args with all occurrences of expanded seq vars expanded 
            val svr = new SeqVarReplacer(expansions)
            val argsExp = args.map(a => svr(a, Context()))
            Some(OMBINDC(binder, conExp, argsExp))
         case _ => None
      }
   }
}
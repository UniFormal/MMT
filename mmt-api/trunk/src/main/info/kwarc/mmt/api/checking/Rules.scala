package info.kwarc.mmt.api.checking

import info.kwarc.mmt.api._
import libraries._
import objects._
import objects.Conversions._
import scala.collection.mutable.{HashMap,HashSet}
/* obsolete
trait GenericRuleMap {
   def delete(which: Rule => Boolean)
}

abstract class GenericRuleMapOne[I, R <: Rule] extends HashMap[I,R] with GenericRuleMap {
   def delete(which: Rule => Boolean) {
      iterator foreach {case (k,r) => if (which(r)) this -= k}
   }
}

abstract class GenericRuleSetMap[I, R <: Rule] extends utils.HashMapToSet[I,R] with GenericRuleMap {
   def delete(which: Rule => Boolean) {
      values foreach {
         s => s.iterator foreach {
            r => if (which(r)) s -= r
         }
      }
   }
}

/** rules indexed and uniquely determined by a single Path */
class RuleMap[R <: Rule] extends GenericRuleMapOne[ContentPath,R] {
}
/** rules indexed and uniquely determined by a pair of Path's */
class RuleMap2[R <: Rule] extends GenericRuleMapOne[(ContentPath,ContentPath),R]

/** rules indexed by a single Path, which multiple rules may share */
class RuleSetMap[R <: Rule] extends GenericRuleSetMap[ContentPath,R]
/** rules indexed by a pair of Paths, which multiple rules may share */
class RuleSetMap2[R <: Rule] extends GenericRuleSetMap[(ContentPath,ContentPath), R]

/** A RuleStore maintains sets of foundation-dependent rules that are used by a Solver.
 * 
 *  It maintains a separate set of TypingRule's for every subtype of Rule; the Rule's are indexed by their heads.
 *  An instance of RuleStore is created by the ExtensionManager.
 */
class RuleStore {
   val typingRules = new RuleMap[TypingRule]
   val inferenceRules = new RuleMap[InferenceRule]
   val computationRules = new RuleMap[ComputationRule]
   val inhabitableRules = new RuleMap[InhabitableRule]
   val universeRules = new RuleMap[UniverseRule]
   val typeBasedEqualityRules = new RuleMap[TypeBasedEqualityRule]
   val termBasedEqualityRules = new RuleSetMap2[TermBasedEqualityRule]
   val solutionRules = new RuleMap[SolutionRule]
   val typeSolutionRules = new RuleMap[TypeSolutionRule]
   val forwardSolutionRules = new RuleMap[ForwardSolutionRule]
   
   val introProvingRules = new RuleSetMap[IntroProvingRule]
   val elimProvingRules = new RuleSetMap[ElimProvingRule]

   /** the DepthRule's that this UOM will use, hashed by (outer,inner) pairs */
   val depthRules = new RuleSetMap2[uom.DepthRule]
   /** the BreadthRule's that this UOM will use, hashed by (outer,inner) pairs */
   val breadthRules = new RuleSetMap[uom.BreadthRule]
   /** the AbbrevRule's that this UOM will use, hashed by the abbreviating operator */
   val abbrevRules = new RuleSetMap[uom.AbbrevRule]
   
   private val all = List(typingRules, inferenceRules, computationRules, universeRules, inhabitableRules,
                     typeBasedEqualityRules , termBasedEqualityRules, solutionRules, typeSolutionRules, forwardSolutionRules, 
                     introProvingRules, elimProvingRules, depthRules, breadthRules, abbrevRules)
   
   /** add some Rule to this RuleStore */
   def add(rs: Rule*) {
      rs foreach {
         case r: TypingRule => typingRules(r.head) = r
         case r: InferenceRule => inferenceRules(r.head) = r
         case r: ComputationRule => computationRules(r.head) = r
         case r: InhabitableRule => inhabitableRules(r.head) = r
         case r: UniverseRule => universeRules(r.head) = r
         case r: TypeBasedEqualityRule => typeBasedEqualityRules(r.head) = r
         case r: TermBasedEqualityRule => termBasedEqualityRules((r.left,r.right)) += r
         case r: SolutionRule => solutionRules(r.head) = r
         case r: TypeSolutionRule => typeSolutionRules(r.head) = r
         case r: ForwardSolutionRule => forwardSolutionRules(r.head) = r
         case r: IntroProvingRule => introProvingRules(r.head) += r
         case r: ElimProvingRule => elimProvingRules(r.head) += r
         case r: uom.DepthRule => depthRules((r.outer,r.inner)) += r
         case r: uom.BreadthRule => breadthRules(r.head) += r
         case r: uom.AbbrevRule => abbrevRules(r.head) += r
      }
   }
   def add(rs: RuleSet) {
      rs.getAll.foreach(add(_))
   }
   def delete(which: Rule => Boolean) {
      all foreach {_.delete(which)}
   }
   
   def stringDescription = {
      var res = ""
      def mkM[A<:Rule](label: String, map: HashMap[ContentPath,A]) {
         if (! map.isEmpty) {
            res += "  " + label + "\n"
            map.values.foreach {r => res += "    " + r.toString + "\n\n"}
         }
      }
      def mkS[A, B<:Rule](label: String, map: utils.HashMapToSet[A,B]) {
         if (! map.isEmpty) {
            res += "  " + label + "\n"
            map.foreach {e => e._2.foreach {r => res += "    " + r.toString + "\n\n"}}
         }
      }
      mkM("typing rules", typingRules) 
      mkM("inference rules", inferenceRules)
      mkM("computation rules", computationRules)
      mkM("inhabitable rules", inhabitableRules)
      mkM("universe rules", universeRules)
      mkM("equality rules", typeBasedEqualityRules)
      mkS("term-based equality rules", termBasedEqualityRules)
      mkM("solution rules", solutionRules)
      mkM("type solution rules", typeSolutionRules)
      mkM("forwardSolution rules", forwardSolutionRules)
      mkS("intro proving rules", introProvingRules)
      mkS("elim proving rules", elimProvingRules)
      mkS("depth rules", depthRules)
      mkS("breadth rules", breadthRules)
      mkS("abbrev rules", abbrevRules)
      res
   }
}*/

/** the type of all Rules
 * 
 * All Rules have an apply method that is passed a Solver for callbacks.
 * The Solver does not implement any back-tracking. Therefore, rules may only use callbacks if their effects are required.
 */
trait Rule {
   /** an MMT URI that is used to indicate when the Rule is applicable */
   val head: GlobalName
   override def toString = {
      var name = getClass.getName
      if (name.endsWith("$"))
         name = name.substring(0,name.length-1)
      "rule " + name + " for " + head
   }
}

class Continue[A](a: => A) {
   def apply() = a
}

object Continue {
   def apply[A](a : => A) = new Continue(a)
}

/** A RuleSet groups some Rule's. Its construction and use corresponds to algebraic theories. */
class RuleSet {
   private val rules = new HashSet[Rule]

   def declares(rs: Rule*) {rs foreach {rules += _}}
   def imports(rss: RuleSet*) {rss foreach {rules ++= _.rules}}
   
   def getAll = rules
   def get[R<:Rule](cls: Class[R]): HashSet[R] = rules flatMap {r =>
      if (cls.isInstance(r))
         List(r.asInstanceOf[R])
      else
         Nil
   }
   def getByHead[R<:Rule](cls: Class[R], head: ContentPath): HashSet[R] = get(cls) filter {r => r.head == head}
   def getFirst[R<:Rule](cls: Class[R], head: ContentPath): Option[R] = getByHead(cls, head).headOption
   
   override def toString = rules.toList.map(_.toString).mkString(", ")
}

/**
 * passed to [[Rule]]s to permit callbacks to the Solver
 */
trait CheckingCallback {
   def check(j: Judgement)(implicit history: History): Boolean
}

object TypingRule {
   /**
    * may be thrown by a TypingRule to indicate that type of tm should be inferred and equated to tp 
    */
   object SwitchToInference extends java.lang.Throwable
}

/** An TypingRule checks a term against a type.
 *  It may recursively call other checks.
 *  @param head the head of the type to which this rule is applicable 
 */
abstract class TypingRule(val head: GlobalName) extends Rule {
   /**
    *  @param solver provides callbacks to the currently solved system of judgments
    *  @param tm the term
    *  @param the type
    *  @param context its context
    *  @return true iff the typing judgment holds
    *  
    *  may throw SwitchToInference
    */
   def apply(solver: Solver)(tm: Term, tp: Term)(implicit stack: Stack, history: History) : Boolean
}

/**
 * A SubtypingRule handles [[Subtyping]] judgements
 */
abstract class SubtypingRule extends Rule {
   def applicable(tp1: Term, tp2: Term): Boolean
   /**
    * pre all arguments covered
    * @return Some(b) if the judgment was proved/disproved, None if the result is inconclusive
    */
   def apply(solver: Solver)(tp1: Term, tp2: Term)(implicit stack: Stack, history: History) : Option[Boolean]
}

/** An InferenceRule infers the type of an expression
 *  It may recursively infer the types of components.
 *  @param head the head of the term whose type this rule infers 
 */
abstract class InferenceRule(val head: GlobalName, val typOp : GlobalName) extends Rule {
   /** 
    *  @param solver provides callbacks to the currently solved system of judgments
    *  @param tm the term
    *  @param covered if true, well-formedness may be assumed
    *  @param stack its context
    *  @param history the history so far
    *  @return the inferred type if inference was possible
    */
   def apply(solver: Solver)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History): Option[Term]
}

/** A ComputationRule simplifies an expression operating at the toplevel of the term.
 *  But it may recursively simplify the components if that makes the rule applicable.
 *  The rule must preserve equality and well-typedness of the simplified term. If necessary, additional checks must be performed.
 *  @param head the head of the term this rule can simplify 
 */
abstract class ComputationRule(val head: GlobalName) extends Rule {
   /** 
    *  @param solver provides callbacks to the currently solved system of judgments
    *  @param tm the term to simplify
    *  @param context its context
    *  @return the simplified term if simplification was possible
    */
   def apply(solver: Solver)(tm: Term)(implicit stack: Stack, history: History): Option[Term]
}

/** A UnaryTermRule checks a [[UnaryTermJudgement]]
 *  @param head the head of the term 
 */
abstract class UnaryTermRule(val head: GlobalName) extends Rule {
   /** 
    *  @param solver provides callbacks to the currently solved system of judgments
    *  @param term the Term
    *  @param stack its context
    *  @return true iff the judgment holds
    */
   def apply(solver: Solver)(term: Term)(implicit stack: Stack, history: History): Boolean
}
/** checks an [[Inhabitable]] judgement */
abstract class InhabitableRule(head: GlobalName) extends UnaryTermRule(head)
/** checks a [[Universe]] judgement */
abstract class UniverseRule(head: GlobalName) extends UnaryTermRule(head)

trait ApplicableUnder extends Rule {
   def under: List[GlobalName]
   private lazy val ops = (under:::List(head)).map(p => OMS(p))
   def applicable(tp: Term) = tp match {
      case OMA(f,a) => (f::a).startsWith(ops)
      case _ => false
   }
}

/** A TypeBasedEqualityRule checks the equality of two terms based on the head of their type
 *  @param head the head of the type of the two terms 
 */
abstract class TypeBasedEqualityRule(val under: List[GlobalName], val head: GlobalName) extends Rule with ApplicableUnder {
   /** 
    *  @param solver provides callbacks to the currently solved system of judgments
    *  @param tm1 the first term
    *  @param tm2 the second term
    *  @param tp their type
    *  @param context their context
    *  @return true iff the judgment holds
    */
   def apply(solver: Solver)(tm1: Term, tm2: Term, tp: Term)(implicit stack: Stack, history: History): Boolean
}

/**
 * A TermBasedEqualityRule checks the equality of two terms without considering types
 */
abstract class TermBasedEqualityRule extends Rule {
   /** 
    *  @return true if the rule is applicable to tm1 == tm2 
    */
   def applicable(tm1: Term, tm2: Term): Boolean
   /** 
    *  @param solver provides callbacks to the currently solved system of judgments
    *  @param tm1 the first term
    *  @param tm2 the second term
    *  @param tp their type
    *  @param stack their context
    *  @return true iff the judgment holds
    */
   def apply(check: CheckingCallback)(tm1: Term, tm2: Term, tp: Option[Term])(implicit stack: Stack, history: History): Option[Continue[Boolean]]
}


/** A TermBasedEqualityRule checks the equality of two terms with certain heads
 *  @param left the head of the first term
 *  @param right the head of the second term 
 */
abstract class TermHeadBasedEqualityRule(val under: List[GlobalName], val left: GlobalName, val right: GlobalName) extends TermBasedEqualityRule {
   val head = left
   private val opsLeft  = (under:::List(left)).map(p => OMS(p))
   private val opsRight = (under:::List(right)).map(p => OMS(p))
   def applicable(tm1: Term, tm2: Term) = (tm1,tm2) match {
      case (OMA(f1,a1),OMA(f2,a2)) => (f1::a1).startsWith(opsLeft) && (f2::a2).startsWith(opsRight)
      case (ComplexTerm(c1,_,_,_), ComplexTerm(c2,_,_,_)) => under.isEmpty && c1 == left && c2 == right 
      case _ => false
   }
   def apply(check: CheckingCallback)(tm1: Term, tm2: Term, tp: Option[Term])(implicit stack: Stack, history: History): Option[Continue[Boolean]]
}

/** Congruence as a TermBasedEqualityRule for two terms with the same head
 *  @param head the head of the terms
 *  This rule can be added whenever a constructor is known to be injective,
 *  which is typically the case for type formation and term introduction.
 */
class CongruenceRule(head: GlobalName) extends TermHeadBasedEqualityRule(Nil, head, head) {
   def apply(checker: CheckingCallback)(tm1: Term, tm2: Term, tp: Option[Term])(implicit stack: Stack, history: History) = {
      (tm1,tm2) match {
         case (ComplexTerm(this.head, args1, cont1, scps1), ComplexTerm(this.head, args2, cont2, scps2)) =>
            if (args1.length == args2.length && cont1.length == cont2.length && scps1.length == scps2.length) {
               val cont = Continue {
                  val args = (args1 zip args2) forall {case (Sub(l1,a1),Sub(l2,a2)) =>
                     l1 == l2 && checker.check(Equality(stack,a1,a2,None))}
                  val argsCont = args && checker.check(EqualityContext(stack, cont1, cont2))
                  val alpha = (cont2 alpha cont1).get // defined because cont1.length == cont2.length
                  val argsContScps = argsCont && (scps1 zip scps2).forall {case (s1,s2) =>
                     checker.check(Equality(stack ++ cont1, s1, s2 ^ alpha, None))
                  }
                  argsContScps
               }
               Some(cont)
            } else
               None
         case _ => None
      }
   }
}

/** A ForwardSolutionRule solves for an unknown by inspecting its declarations (as opposed to its use)
 * It can solve a variable directly (e.g., if it has unit type) or apply a variable transformation (e.g., X --> (X1,X2) if X has product type).
 * @param head the head of the type of the unknown to which this rule applies
 * @param priority rules with high priority are applied to a freshly activated constraint is activated;
 *   others when no activatable constraint exists 
 */
abstract class ForwardSolutionRule(val head: GlobalName, val priority: ForwardSolutionRule.Priority) extends Rule {
   /** 
    *  @param solver provides callbacks to the currently solved system of judgments
    *  @param decl the declaration of an unknown
    *  @return true iff it solved a variable
    */
   def apply(solver: Solver)(decl: VarDecl)(implicit stack: Stack, history: History): Boolean
}

/** auxiliary object for the class ForwardSolutionRule */
object ForwardSolutionRule {
   /** the two-valued type of priorities */
   type Priority = Boolean
   /** high priority */
   val high = true
   /** low priority */
   val log = false
}

/**
 * A SolutionRule tries to solve for an unknown that occurs in an equality judgement.
 * 
 * It may be partial by, e.g., by inverting the toplevel operation of a Term without completely isolating an unknown occurring in it.
 * 
 * f(t1) = t2   --->   t1 = g(t2), where t1 contains a target variable that we try to isolate
 * 
 * @param head the operator that the rule tries to invert
 * 
 * Because solution rules must be tried often and may fail, they do not have access to the Solver state
 * and instead transform one judgement into another.
 * This also allows reusing them in other situations, in particular when matching already-type-checked terms.
 */
abstract class SolutionRule(val head: GlobalName) extends Rule {
   /**
    * @return Some(i) if the rule is applicable to t1 in the judgment t1=t2,
    *   in that case, i is the position of the argument of t1 (starting from 0) that the rule will try to isolate
    */
   def applicable(t: Term) : Option[Int]
   /**
    *  @param j the equality in which to isolate a variable on the left side 
    *  @return the transformed equality and a log message if a step towards isolation was possible
    */
   def apply(j: Equality): Option[(Equality,String)]
}

/** A TypeSolutionRule tries to solve for an unknown that occurs in a non-solved position.
 * It may also be partial, e.g., by inverting the toplevel operation of a Term without completely isolating an unknown occurring in it.
 */
abstract class TypeSolutionRule(val head: GlobalName) extends Rule {
   /** 
    *  @param solver provides callbacks to the currently solved system of judgments
    *  @param tm the term that contains the unknown to be solved
    *  @param tp its type 
    *  @param stack the context
    *  @return false if this rule is not applicable;
    *    if this rule is applicable, it may return true only if the Typing Judgement is guaranteed
    *    (by calling an appropriate callback method such as delay or checkTyping)
    */
   def apply(solver: Solver)(tm: Term, tp: Term)(implicit stack: Stack, history: History): Boolean
}
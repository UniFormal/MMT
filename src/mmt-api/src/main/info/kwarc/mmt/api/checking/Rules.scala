package info.kwarc.mmt.api.checking

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.frontend.ChangeListener
import info.kwarc.mmt.api.modules.{DeclaredTheory, Module}
import info.kwarc.mmt.api.symbols.{Constant, RuleConstant}
import libraries._
import objects._
import objects.Conversions._

/** continuation used by some rules */
class Continue[A](a: => A) {
   def apply() = a
}

object Continue {
   def apply[A](a : => A) = new Continue(a)
}


/**
 * passed to [[Rule]]s to permit callbacks to the Solver
 */
trait CheckingCallback {
   /** a fixed context prefix that is not part of the contexts passed to the other methods */ 
   def outerContext : Context

   
   //def getType(p: GlobalName): Option[Term]
   //def getDef(p: GlobalName): Option[Term]

   /** checking */
   def check(j: Judgement)(implicit history: History): Boolean
   /** possibly unsafe simplification */
   def simplify(t : Obj)(implicit stack: Stack, history: History): t.ThisType
   /** type inference, fails by default */
   def inferType(t : Term, covered: Boolean = false)(implicit stack: Stack, history: History): Option[Term] = None
   /** runs code and succeeds by default */
   def dryRun[A](allowDelay: Boolean, commitOnSucces: A => Boolean)(code: => A): DryRunResult = Success(code)
   /**
    * tries to check some judgments without delaying constraints
    * 
    * if this returns None, the check is inconclusive at this point, and no state changes were applied
    * if this returns Some(true), all judgments have been derived and all state changes are applied 
    * if this returns Some(false), no state changes are applied and the caller still has to generate an error message, possibly by calling check(j)
    */
   def tryToCheckWithoutDelay(js:Judgement*): Option[Boolean] = {
     val dr = dryRun(false, (x:Boolean) => x) {
       js forall {j => check(j)(NoHistory)}
     }
     dr match {
      case Success(s:Boolean) => Some(s)
      case Success(_) => throw ImplementationError("illegal success value")
      case WouldFail => Some(false)
      case _:MightFail => None
     }
   }

   /** flag an error */
   def error(message: => String)(implicit history: History): Boolean = false

  def lookup(p: Path): Option[StructuralElement]
  def getTheory(tm : Term)(implicit stack : Stack, history : History) : Option[AnonymousTheory]
  def materialize(cont : Context, tm : Term, expandDefs : Boolean, parent : Option[MPath]) : Module
}

/**
  * rules can throw this exception after being called if they are not applicable
  * because actual back-tracking is not implemented yet, they may only do so
  * if they have not recursed into any state-changing judgments yet
  */
trait MaytriggerBacktrack {
   case class Backtrack(msg: String) extends Throwable {
     override def getMessage = "rule not applicable: " + msg
   }
}

/** super class of all rules primarily used by the [[Solver]] */
trait CheckingRule extends SyntaxDrivenRule {
  /** additional heads that can trigger the rule, e.g., arrow for a rule with head Pi */
  def alternativeHeads: List[GlobalName] = Nil
  def heads = head::alternativeHeads
  
  /** may be thrown to indicate that the judgment that the rules was called on should be delayed */
  case class DelayJudgment(msg: String) extends Throwable
}

object TypingRule {
   /**
    * may be thrown by a TypingRule to indicate that type of tm should be inferred and equated to tp 
    */
  case object SwitchToInference extends Throwable
}

/** An TypingRule checks a term against a type.
 *  It may recursively call other checks.
 *  @param head the head of the type to which this rule is applicable 
 */
abstract class TypingRule(val head: GlobalName) extends CheckingRule {
   /**
    *  @param solver provides callbacks to the currently solved system of judgments
    *  @param tm the term
    *  @param tp the type
    *  @param stack its context
    *  @return true iff the typing judgment holds
    *  
    *  may throw SwitchToInference
    */
   def apply(solver: Solver)(tm: Term, tp: Term)(implicit stack: Stack, history: History) : Boolean
}

/**
 * A SubtypingRule handles [[Subtyping]] judgements
 */
abstract class SubtypingRule extends CheckingRule with MaytriggerBacktrack {
   def applicable(tp1: Term, tp2: Term): Boolean
   /**
    * pre all arguments covered
    * @return Some(b) if the judgment was proved/disproved, None if the result is inconclusive
    */
   def apply(solver: Solver)(tp1: Term, tp2: Term)(implicit stack: Stack, history: History) : Option[Boolean]
}

sealed abstract class Variance
case object Covariant extends Variance
case object Contravariant extends Variance
case object Invariant extends Variance
case object Ignorevariant extends Variance

/** |- op(args1) <: op(args2)  according to variances */
class VarianceRule(val head: GlobalName, variance: List[Variance]) extends SubtypingRule {
  def applicable(tp1: Term, tp2: Term) = (tp1,tp2) match {
    case (OMA(OMS(this.head), _), OMA(OMS(this.head), _)) => true
    case _ => false
  }
  def apply(solver: Solver)(tp1: Term, tp2: Term)(implicit stack: Stack, history: History): Option[Boolean] = {
    val OMA(_, args1) = tp1
    val OMA(_, args2) = tp2
    if (args1.length != args2.length)
      return Some(false)
    if (args1.length != variance.length) {
      return Some(false) // TODO what to do here?
    }
    history += "checking subtyping by applying variance rule"
    val b = ((args1 zip args2) zip variance) forall {case ((a1,a2),v) =>
        v match {
          case Covariant => solver.check(Subtyping(stack, a1, a2))
          case Contravariant => solver.check(Subtyping(stack, a2, a1))
          case Invariant => solver.check(Equality(stack, a1, a2, None))
          case Ignorevariant => true
        }
    }
    Some(b)
  }
}

/** A SupertypeRule represnts a type as a subtype of a bigger one
  *  @param head the head of the term whose type this rule infers
  */
abstract class SupertypeRule(val head: GlobalName) extends CheckingRule {
   /**
     *  @param solver provides callbacks to the currently solved system of judgments
     *  @param tp the type
     *  @param covered if true, well-formedness may be assumed
     *  @param stack its context
     *  @param history the history so far
     *  @return the supertype and the condition defining the original type as a subtype
     */
   def apply(solver: Solver)(tp: Term, covered: Boolean)(implicit stack: Stack, history: History): (Option[(Boolean,Term)],Option[Term])
}


/** An InferenceRule infers the type of an expression
 *  It may recursively infer the types of components.
 *  @param head the head of the term whose type this rule infers 
 */
abstract class InferenceRule(val head: GlobalName, val typOp : GlobalName) extends CheckingRule with MaytriggerBacktrack {
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

@deprecated("must be reimplemented cleanly")
abstract class TheoryExpRule(head : GlobalName, oftype : GlobalName) extends InferenceRule(head,oftype) {
  def apply(solver: Solver)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History): Option[Term] = {
    val checks = apply(tm, covered)(solver,stack,history)
    if (checks) Some(OMS(ModExp.theorytype)) else None
  }

  protected def apply(tm : Term, covered: Boolean)(implicit solver : Solver, stack : Stack, history : History) : Boolean

  def applicable(tm : Term) : Boolean = tm match {
    case OMS(`head`) => true
    case OMA(OMS(`head`), args) => true
    case _ => false
  }

  def elaborate(prev : Context, df : Term)(implicit elab : (Context,Term) => Context): Context
}

abstract class FormationRule(h: GlobalName, t: GlobalName) extends InferenceRule(h,t)
abstract class IntroductionRule(h: GlobalName, t: GlobalName) extends InferenceRule(h,t)
abstract class EliminationRule(h: GlobalName, t: GlobalName) extends InferenceRule(h,t)

/** A ComputationRule simplifies an expression operating at the toplevel of the term.
 *  But it may recursively simplify the components if that makes the rule applicable.
 *  The rule must preserve equality and well-typedness of the simplified term. If necessary, additional checks must be performed.
 *  @param head the head of the term this rule can simplify 
 */
abstract class ComputationRule(val head: GlobalName) extends CheckingRule {
   /** 
    *  @param check provides callbacks to the currently solved system of judgments
    *  @param tm the term to simplify
    *  @param covered if true, term can be assumed to be well-formed
    *  @param stack its context
    *  @return the simplified term if simplification was possible
    */
   def apply(check: CheckingCallback)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History): Option[Term]
}

/** A UnaryTermRule checks a [[UnaryTermJudgement]]
 *  @param head the head of the term 
 */
abstract class UnaryTermRule(val head: GlobalName) extends CheckingRule {
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

trait ApplicableUnder extends CheckingRule {
   def under: List[GlobalName]
   private lazy val ops = (under:::List(head)).map(p => OMS(p))
   def applicable(tm: Term) = tm match {
      case OMA(f,a) => (f::a).startsWith(ops)
      case OMS(p) => under == Nil && heads.contains(p)
      case OMBINDC(OMS(p), _, _) => p == head && under.isEmpty
      case _ => false
   }
}

/** A TypeBasedEqualityRule checks the equality of two terms based on the head of their type
 *  @param head the head of the type of the two terms 
 */
abstract class TypeBasedEqualityRule(val under: List[GlobalName], val head: GlobalName) extends CheckingRule with ApplicableUnder {
   /** 
    *  @param solver provides callbacks to the currently solved system of judgments
    *  @param tm1 the first term
    *  @param tm2 the second term
    *  @param tp their type
    *  @param stack their context
    *  @return true iff the judgment holds; None if the solver should proceed with term-based equality checking
    */
   def apply(solver: Solver)(tm1: Term, tm2: Term, tp: Term)(implicit stack: Stack, history: History): Option[Boolean]
}

/** always succeeds, e.g., as needed to implement proof irrelevance */
@deprecated class TermIrrelevanceRule(under: List[GlobalName], head: GlobalName) extends TypeBasedEqualityRule(under, head) {
  final def apply(solver: Solver)(tm1: Term, tm2: Term, tp: Term)(implicit stack: Stack, history: History): Option[Boolean] = {
    history += "all terms of this type are equal"
    Some(true)
  }
}

/**
 * A TermBasedEqualityRule checks the equality of two terms without considering types
 */
abstract class TermBasedEqualityRule extends CheckingRule {
   /** 
    *  @return true if the rule is applicable to tm1 == tm2
    *  
    *  This method should fail quickly if possible; the apply method may still fail even it this returned true. 
    */
   def applicable(tm1: Term, tm2: Term): Boolean
   /**
    *  @param check provides callbacks to the currently solved system of judgments
    *  @param tm1 the first term
    *  @param tm2 the second term
    *  @param tp their type if known
    *  @param stack their context
    *  @param history the history so far
    *  @return Some(cont) continuation function to apply the rule
    *          None if this rule is not applicable
    */
   def apply(check: CheckingCallback)(tm1: Term, tm2: Term, tp: Option[Term])(implicit stack: Stack, history: History): Option[Continue[Boolean]]
}


/** A TermBasedEqualityRule checks the equality of two terms with certain heads
 *  @param left the head of the first term
 *  @param right the head of the second term 
 */
// TODO this currently ignores alternativeHeads, which is probably fine
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
            val cont = Continue {
               if (args1.length == args2.length && cont1.length == cont2.length && scps1.length == scps2.length) {
                  val args = (args1 zip args2) forall {case (Sub(l1,a1),Sub(l2,a2)) =>
                     l1 == l2 && checker.check(Equality(stack,a1,a2,None))(history + "checking equality of arguments")
                  }
                  
                  val argsCont = args && checker.check(EqualityContext(stack, cont1, cont2, true))(history + "checking equality of contexts")
                  val alpha = (cont2 alpha cont1).get // defined because cont1.length == cont2.length

                  val argsContScps = argsCont && (scps1 zip scps2).forall {case (s1,s2) =>
                     checker.check(Equality(stack ++ cont1, s1, s2 ^ alpha, None))(history + "checking equality of arguments")
                  }
                  
                  argsContScps
               } else {
                  checker.error("terms do not have the same number or arguments")
               }
            }
            Some(cont)
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
abstract class ForwardSolutionRule(val head: GlobalName, p: ForwardSolutionRule.Priority) extends CheckingRule {
   /** 
    *  @param solver provides callbacks to the currently solved system of judgments
    *  @param decl the declaration of an unknown
    *  @return true iff it solved a variable
    */
   def apply(solver: Solver)(decl: VarDecl)(implicit stack: Stack, history: History): Boolean
   
   override def priority = p
}

/** auxiliary object for the class ForwardSolutionRule */
object ForwardSolutionRule {
   /** the two-valued type of priorities */
   type Priority = Int
   /** high priority */
   val high = +1
   /** low priority */
   val low = -1
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
abstract class SolutionRule(val head: GlobalName) extends CheckingRule {
   /**
    * @return Some(i) if the rule is applicable to t1 in the judgment t1=t2,
    *   in that case, i is the position of the argument of t1 (starting from 0) that the rule will try to isolate
    *   this is about spotting unknowns, not predicting whether the isolation will succeed
    */
   def applicable(t: Term) : Option[Int]
   /**
    *  @param j the equality in which to isolate a variable on the left side 
    *  @return the transformed equality and a log message if a step towards isolation was possible
    */
   def apply(j: Equality): Option[(Equality,String)]
}

/**
 * A TypeSolutionRule tries to solve for the type of an unknown.
 */
abstract class TypeSolutionRule(val head: GlobalName) extends CheckingRule {
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

/**
 * A TypeBasedSolutionRule solves an unknown based on its type, by constructing a term of that type.
 * This is legal if all terms of that type are equal. 
 */
abstract class TypeBasedSolutionRule(under: List[GlobalName], head: GlobalName) extends TypeBasedEqualityRule(under,head) {

  def solve(solver : Solver)(tp : Term)(implicit stack: Stack, history: History): Option[Term]

  /** if used as an equality rule, this makes all terms of this type equal */
  final def apply(solver: Solver)(tm1: Term, tm2: Term, tp: Term)(implicit stack: Stack, history: History): Option[Boolean] = if (applicable(tp)) {
    history += "all terms of this type are equal"
    Some(true)
  } else None
}

class AbbreviationRuleGenerator extends ChangeListener {
  override val logPrefix = "abbrev-rule-gen"
  protected val abbreviationTag = "abbreviation"
  private def rulePath(r: GeneratedAbbreviationRule) = r.constant.path / abbreviationTag
  private def present(t: Term) = controller.presenter.asString(t)

  private def getGeneratedRule(p: Path): Option[GeneratedAbbreviationRule] = {
    p match {
      case p: GlobalName =>
        controller.globalLookup.getO(p / abbreviationTag) match {
          case Some(r: RuleConstant) => r.df.map(df => df.asInstanceOf[GeneratedAbbreviationRule])
          case _ => None
        }
      case _ => None
    }
  }

  override def onAdd(e: StructuralElement) {onCheck(e)}
  override def onDelete(e: StructuralElement) {
    getGeneratedRule(e.path).foreach {r => controller.delete(rulePath(r))}
  }
  override def onCheck(e: StructuralElement): Unit = e match {
    case c : Constant if (c.rl contains abbreviationTag) && c.dfC.analyzed.isDefined =>
      val rule = new GeneratedAbbreviationRule(c)
      val ruleConst = new RuleConstant(c.home, c.name / abbreviationTag, OMS(c.path), Some(rule))
      ruleConst.setOrigin(GeneratedBy(this))
      log(c.name + " ~~> " + present(c.df.get))
      controller add ruleConst
    case _ =>
  }

}

class GeneratedAbbreviationRule(val constant : Constant) extends ComputationRule(constant.path) {
  override def priority: Int = super.priority + 3
  def apply(check: CheckingCallback)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History): Option[Term] = tm match {
    case OMS(p) if p == constant.path => constant.df
    case _ => None
  }
}
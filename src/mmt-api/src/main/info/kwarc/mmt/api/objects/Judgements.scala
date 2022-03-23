package info.kwarc.mmt.api.objects
import info.kwarc.mmt.api._
import objects.Conversions._
import scala.collection.mutable.{HashSet}

/** the type of object level judgments as used for typing and equality of terms */
abstract class Judgement extends utils.HashEquality[Judgement] with checking.HistoryEntry {
  /** @return the set of names of the meta-variables occurring in this judgment
   *    Constraints must come with a Context binding all object variables occurring freely in any expressions;
   *    therefore, the remaining free variables are meta-variables
   */
  def freeVars : HashSet[LocalName]
  val stack: Stack
  def context = stack.context

  /** true if that is guaranteed if this holds */
  def implies(that: Judgement): Boolean = this == that
  //TODO also true that.context.subsumes(this.context)

  /** a toString method that may call a continuation on its objects */
  def present(implicit cont: Obj => String) = "Judgment " + presentAntecedent + "  |--  " + presentSucceedent
  def presentSucceedent(implicit cont: Obj => String) = toString
  def presentAntecedent(implicit cont: Obj => String) = {
     cont(stack.context)
  }
  /*
  def map(fC: Stack => Stack, fT: (Term, Boolean) => Term): Judgement
  private[objects] def check(implicit solver: Solver, history: History): Boolean
  */
}

/** A WFJudgment defines well-formed objects */
trait WFJudgement extends Judgement {
   val wfo: Obj
}

/** represents an equality judgement, optionally at a type
 * context |- t1 = t2 : tp  if t = Some(tp)
 * or
 * context |- t1 = t2       if t = None
 */
case class Equality(stack: Stack, tm1: Term, tm2: Term, tpOpt: Option[Term]) extends Judgement {
   lazy val freeVars = {
     val ret = new HashSet[LocalName]
     val fvs = stack.context.freeVars_ ::: tm1.freeVars_ ::: tm2.freeVars_ ::: (tpOpt.map(_.freeVars_).getOrElse(Nil))
     fvs foreach {n => if (! stack.context.isDeclared(n)) ret += n}
     ret
   }
   override def presentSucceedent(implicit cont: Obj => String): String =
      cont(tm1) + "  ==  " + cont(tm2) + tpOpt.map(tp => "  ::  " + cont(tp)).getOrElse("")
   /** swaps left and right side of equality */
   def swap = Equality(stack, tm2, tm1, tpOpt)
}


/**
 *  Common code for some binary judgements
 */
abstract class BinaryObjJudgement(stack: Stack, left: Obj, right: Obj, delim: String) extends Judgement {
   lazy val freeVars = {
    val ret = new HashSet[LocalName]
    val fvs = stack.context.freeVars_ ::: left.freeVars_ ::: right.freeVars_
    fvs foreach {n => if (! stack.context.isDeclared(n)) ret += n} //only vars from obj should be kicked out here
    ret
  }
  override def presentSucceedent(implicit cont: Obj => String): String =
      cont(left) + "  " + delim + "  " + cont(right)
}

/**
 * represents a subtyping judgement
 * context |- tp1 <: tp2
 */
case class Subtyping(stack: Stack, tp1: Term, tp2: Term) extends BinaryObjJudgement(stack, tp1, tp2, "<:")

/** represents a typing judgement
 * context |- tm : tp
 * tpSymb - optionally specified typing symbol
 */
case class Typing(stack: Stack, tm: Term, tp: Term, tpSymb : Option[GlobalName] = None)
           extends BinaryObjJudgement(stack, tm, tp, ":") with WFJudgement {
  val wfo = tm
}

/**
 *  Common code for some judgements
 */
abstract class UnaryObjJudgement(stack: Stack, obj: Obj, label: String) extends Judgement {
   lazy val freeVars = {
    val ret = new HashSet[LocalName]
    val fvs = stack.context.freeVars_ ::: obj.freeVars_
    fvs foreach {n => if (! stack.context.isDeclared(n)) ret += n} //only vars from obj should be kicked out here (multiple places in this file)
    ret
  }
  override def presentSucceedent(implicit cont: Obj => String): String =
      cont(obj) + " " + label
}

/**
 *  A term univ is a universe if all its inhabitants are [[Inhabitable]].
 */
case class Universe(stack: Stack, wfo: Term) extends UnaryObjJudgement(stack, wfo, "UNIVERSE") with WFJudgement {
   def univ = wfo
}
/**
 *  A term wfo is inhabitable if it can occur on the right side of a Typing judgement.
 *  Such terms can be used as the types of constants and variables.
 */
case class Inhabitable(stack: Stack, wfo: Term) extends UnaryObjJudgement(stack, wfo, "INHABITABLE") with WFJudgement
/**
 *  A term tp is inhabited if it occurs on the right side of a Typing judgement.
 *  Via Curry-Howard, such terms can be thought of as provable propositions.
 *  Therefore, this judgement is usually undecidable.
 */
case class Inhabited(stack: Stack, tp: Term) extends UnaryObjJudgement(stack, tp, "INHABITED")

/**
 * An abbreviation for the meta-level typing judgement for valid theories
 */
object IsTheory {
   def apply(stack: Stack, thy: Term) = Typing(stack, thy, TheoryType(Nil))
}
/**
 * An abbreviation for the meta-level typing judgement for valid morphisms
 */
object IsMorphism {
   def apply(stack: Stack, morphism: Term, from: Term, to: Term) = Typing(stack, morphism, MorphType(from, to))
}
/**
 * An abbreviation for a [[IsMorphism]] into the current theory
 */
object IsRealization {
   def apply(stack: Stack, morphism: Term, from: Term) = IsMorphism(stack, morphism, from, ComplexTheory(Nil))
}

/** well-formedness of contexts */
case class IsContext(stack: Stack, wfo: Context) extends UnaryObjJudgement(stack, wfo, "Context") with WFJudgement

/** represents an equality judgement between contexts
 * context |- context1 = context2
 *
 * @param uptoAlphy true: alpha-renaming allowed, false: reordering allowed
 */
case class EqualityContext(stack: Stack, context1: Context, context2: Context, uptoAlpha: Boolean) extends Judgement {
   lazy val freeVars = {
     val ret = new HashSet[LocalName]
     val fvs = stack.context.freeVars_ ::: context2.freeVars_ :::context1.freeVars_
     fvs foreach {n => if (! stack.context.isDeclared(n)) ret += n}
     ret
   }
   override def presentSucceedent(implicit cont: Obj => String): String =
      cont(context1) + " = " + cont(context2)
}

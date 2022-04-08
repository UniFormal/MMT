package info.kwarc.mmt.api.objects
import info.kwarc.mmt.api._
import objects.Conversions._

import scala.collection.mutable.HashSet

/** the type of object level judgments as used for typing and equality of terms */
abstract class Judgement extends utils.HashEquality[Judgement] with checking.HistoryEntry {
  /** @return the set of names of the meta-variables occurring in this judgment
   *    Constraints must come with a Context binding all object variables occurring freely in any expressions;
   *    therefore, the remaining free variables are meta-variables
   */
  def freeVars : HashSet[LocalName]
  val stack: Stack
  def context = stack.context

  /** a sound but not necessary complete criterion for whether that judgment is guaranteed if this one holds */
  def implies(that: Judgement): Boolean = {
    (this impliesSuccedent that) && (that.context subsumes this.context)
  }

  def impliesSuccedent(that: Judgement): Boolean = (this,that) match {
    case (j:UnaryObjJudgement, k: UnaryObjJudgement) => j.label == k.label && j.obj == k.obj
    case (j:BinaryObjJudgement, k: BinaryObjJudgement) => j.delim == k.delim && j.left == k.left && j.right == k.right
    case (e:Equality,f:Equality) =>
      val tpImplies = e.tpOpt == f.tpOpt || f.tpOpt.isEmpty
      tpImplies && ((e.tm1 == f.tm1 && e.tm2 == f.tm2) || (e.tm1 == f.tm2 && e.tm2 == f.tm1))
    case (e: EqualityContext, f: EqualityContext) =>
      // uptoAlpha handling can be improved
      e.uptoAlpha == f.uptoAlpha && ((e.context1 == f.context1 && e.context2 == f.context2) || (e.context1 == f.context2 && e.context2 == f.context1))
    case _ => false
  }
  //TODO also true that.context.subsumes(this.context)

  /** a toString method that may call a continuation on its objects */
  def present(implicit cont: Obj => String) = "Judgment " + presentAntecedent + "  |--  " + presentSuccedent
  def presentSuccedent(implicit cont: Obj => String) = toString
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

/**
  *  Common code for some binary judgements
  *  @param left the first object
  *  @param right the second object
  *  @param delim the property between the objects (part of both concrete and abstract syntax)
  */
abstract class BinaryObjJudgement(stack: Stack, val left: Obj, val right: Obj, val delim: String) extends Judgement {
   lazy val freeVars = {
    val ret = new HashSet[LocalName]
    val fvs = stack.context.freeVars_ ::: left.freeVars_ ::: right.freeVars_
    fvs foreach {n => if (! stack.context.isDeclared(n)) ret += n} //only vars from obj should be kicked out here
    ret
  }
  override def presentSuccedent(implicit cont: Obj => String): String =
      cont(left) + "  " + delim + "  " + cont(right)
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
  override def presentSuccedent(implicit cont: Obj => String): String =
    cont(tm1) + "  ==  " + cont(tm2) + tpOpt.fold("")(tp => "  ::  " + cont(tp))
  /** swaps left and right side of equality */
  def swap = Equality(stack, tm2, tm1, tpOpt)
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
           extends BinaryObjJudgement(stack, tm, tp, "::") with WFJudgement {
  val wfo = tm
}

/**
  *  Common code for some judgements
  *  @param obj the object that is judged
  *  @param label the syntactic category of the object (part of both concrete and abstract syntax)
  */
abstract class UnaryObjJudgement(stack: Stack, val obj: Obj, val label: String) extends Judgement {
   lazy val freeVars = {
    val ret = new HashSet[LocalName]
    val fvs = stack.context.freeVars_ ::: obj.freeVars_
    fvs foreach {n => if (! stack.context.isDeclared(n)) ret += n} //only vars from obj should be kicked out here (multiple places in this file)
    ret
  }
  override def presentSuccedent(implicit cont: Obj => String): String =
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
   override def presentSuccedent(implicit cont: Obj => String): String =
      cont(context1) + " = " + cont(context2)
}

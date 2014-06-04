package info.kwarc.mmt.api.objects
import info.kwarc.mmt.api._
import objects.Conversions._
import scala.collection.mutable.{HashSet}

/** the type of object level judgments as used for typing and equality of terms */
abstract class Judgement extends utils.HashEquality[Judgement] with HistoryEntry {
  /** @return the set of names of the meta-variables occurring in this judgment
   *    Constraints must come with a Context binding all object variables occurring freely in any expressions;
   *    therefore, the remaining free variables are meta-variables
   */ 
  def freeVars : HashSet[LocalName]
  val stack: Stack
  
   /** a toString method that may call a continuation on its objects
    */
  def present(implicit cont: Obj => String) = presentAntecedent + " |- " + presentSucceedent
  def presentSucceedent(implicit cont: Obj => String) = toString
  def presentAntecedent(implicit cont: Obj => String) = {
     stack.theory.toString + "; " + cont(stack.context)
  }
  /*
  def map(fC: Stack => Stack, fT: (Term, Boolean) => Term): Judgement
  private[objects] def check(implicit solver: Solver, history: History): Boolean
  */
}

/** A WFJudgment defines well-formed objects */
trait WFJudgement extends Judgement {
   val wfo: Term
}

/** represents an equality judgement, optionally at a type
 * context |- t1 = t2 : tp  if t = Some(tp)
 * or
 * context |- t1 = t2       if t = None
 */
case class Equality(stack: Stack, t1: Term, t2: Term, t: Option[Term]) extends Judgement {
   lazy val freeVars = {
     val ret = new HashSet[LocalName]
     val fvs = stack.context.freeVars_ ::: t1.freeVars_ ::: t2.freeVars_ ::: (t.map(_.freeVars_).getOrElse(Nil))
     fvs foreach {n => if (! stack.context.isDeclared(n)) ret += n}
     ret
   }
   override def presentSucceedent(implicit cont: Obj => String): String =
      cont(t1) + " = " + cont(t2) + (if (t.isDefined) " : " + cont(t.get) else "")
}

/** represents an equality judgement between contexts
 * context |- ctx = ctx
 */
case class EqualityContext(stack: Stack, context1: Context, context2: Context) extends Judgement {
   lazy val freeVars = {
     val ret = new HashSet[LocalName]
     val fvs = stack.context.freeVars_ ::: context2.freeVars_ :::context1.freeVars_
     fvs foreach {n => if (! stack.context.isDeclared(n)) ret += n}
     ret
   }
   override def presentSucceedent(implicit cont: Obj => String): String =
      cont(context1) + " = " + cont(context2)
}


/** represents a typing judgement
 * context |- tm : tp
 * tpSymb - optionally specified typing symbol
 */
case class Typing(stack: Stack, tm: Term, tp: Term, tpSymb : Option[GlobalName] = None) extends WFJudgement {
  lazy val freeVars = {
    val ret = new HashSet[LocalName]
    val fvs = stack.context.freeVars_ ::: tm.freeVars_ ::: tp.freeVars_
    fvs foreach {n => if (! stack.context.isDeclared(n)) ret += n}
    ret
  }
  val wfo = tm
  override def presentSucceedent(implicit cont: Obj => String): String =
      cont(tm) + " : " + cont(tp)
}

/**
 *  Common code for some judgements
 */
abstract class UnaryObjJudegment(stack: Stack, obj: Obj, label: String) extends Judgement {
   lazy val freeVars = {
    val ret = new HashSet[LocalName]
    val fvs = stack.context.freeVars_ ::: obj.freeVars_
    fvs foreach {n => if (! stack.context.isDeclared(n)) ret += n}
    ret
  }
  override def presentSucceedent(implicit cont: Obj => String): String =
      cont(obj) + " " + label
}

/**
 *  A term univ is a universe if all its inhabitants are [[Inhabitable]].
 */
case class Universe(stack: Stack, wfo: Term) extends UnaryObjJudegment(stack, wfo, "UNIVERSE") with WFJudgement {
   def univ = wfo
}
/**
 *  A term wfo is inhabitable if it can occur on the right side of a Typing judgement.
 *  Such terms can be used as the types of constants and variables.
 */
case class Inhabitable(stack: Stack, wfo: Term) extends UnaryObjJudegment(stack, wfo, "INHABITABLE") with WFJudgement
/**
 *  A term tp is inhabitable if it occurs on the right side of a Typing judgement.
 *  Via Curry-Howard, such terms can be thought of as provable propositions.
 *  Therefore, this judgement is usually undecidable. 
 */
case class Inhabited(stack: Stack, tp: Term) extends UnaryObjJudegment(stack, tp, "INHABITED")

/**
 * An abbreviation for the meta-level typing judgement for valid theories
 */
object IsTheory {
   def apply(stack: Stack, thy: Term) = Typing(stack, thy, TheoryType())
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

case class IsContext(stack: Stack, context: Context) extends UnaryObjJudegment(stack, context, "Context")
case class IsSubstitution(stack: Stack, substitution: Substitution, from: Context, to: Context) extends Judgement {
  lazy val freeVars = {
    val ret = new HashSet[LocalName]
    val fvs = stack.context.freeVars_ ::: substitution.freeVars_ ::: from.freeVars_ ::: to.freeVars_
    fvs foreach {n => if (! stack.context.isDeclared(n)) ret += n}
    ret
  }
}

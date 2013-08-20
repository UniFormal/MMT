package info.kwarc.mmt.api.objects
import info.kwarc.mmt.api._
import objects.Conversions._
import scala.collection.mutable.{HashSet}

/** the type of object level judgments as used for typing and equality of terms */
abstract class Judgement extends HistoryEntry {
  lazy val hash = this.hashCode
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
}

/** A WFJudgment defines well-formed objects */
abstract class WFJudgement extends Judgement {
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
 *  The universe judgement determines whether a term may be used as a type, i.e., may occur on the right side of a Typing judgement. 
 *  
 *  There are 2 closely related variants of this judgement, which we merge into one:
 *  In the typing judgement, t:A:U, the universe judgement can be called on A (being in a universe) or U (being a universe).
 *  @param isIn if true: univ=A; if false, univ=U
 */
case class Universe(stack: Stack, univ: Term, isIn: Boolean) extends WFJudgement {
   lazy val freeVars = {
    val ret = new HashSet[LocalName]
    val fvs = stack.context.freeVars_ ::: univ.freeVars_
    fvs foreach {n => if (! stack.context.isDeclared(n)) ret += n}
    ret
  }
  val wfo = univ
  override def presentSucceedent(implicit cont: Obj => String): String =
      cont(univ) + " UNIVERSE"
}

case class Inhabitation(stack: Stack, tp: Term) extends Judgement {
   lazy val freeVars = {
    val ret = new HashSet[LocalName]
    val fvs = stack.context.freeVars_ ::: tp.freeVars_
    fvs foreach {n => if (! stack.context.isDeclared(n)) ret += n}
    ret
  }   
}

case class IsTheory(stack: Stack, theory: Term) extends Judgement {
  lazy val freeVars = {
    val ret = new HashSet[LocalName]
    val fvs = stack.context.freeVars_ ::: theory.freeVars_
    fvs foreach {n => if (! stack.context.isDeclared(n)) ret += n}
    ret
  }
}

case class IsMorphism(stack: Stack, morphism: Term, from: Term) extends Judgement {
  lazy val freeVars = {
    val ret = new HashSet[LocalName]
    val fvs = stack.context.freeVars_ ::: morphism.freeVars_ ::: from.freeVars_
    fvs foreach {n => if (! stack.context.isDeclared(n)) ret += n}
    ret
  }
}

case class IsContext(stack: Stack, context: Context) extends Judgement {
  lazy val freeVars = {
    val ret = new HashSet[LocalName]
    val fvs = stack.context.freeVars_ ::: context.freeVars_
    ret
  }
}

case class IsSubstitution(stack: Stack, substitution: Substitution, from: Context, to: Context) extends Judgement {
  lazy val freeVars = {
    val ret = new HashSet[LocalName]
    val fvs = stack.context.freeVars_ ::: substitution.freeVars_ ::: from.freeVars_ ::: to.freeVars_
    fvs foreach {n => if (! stack.context.isDeclared(n)) ret += n}
    ret
  }
}

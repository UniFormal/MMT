package info.kwarc.mmt.api.objects
import info.kwarc.mmt.api._
import objects.Conversions._
import scala.collection.mutable.{HashSet}

/** the type of object level judgments as used for typing and equality of terms */
abstract class Judgement {
  /** @return the set of names of the meta-variables occurring in this judgment
   *    Constraints must come with a Context binding all object variables occurring freely in any expressions;
   *    therefore, the remaining free variables are meta-variables
   */ 
  def freeVars : HashSet[LocalName]
  val stack: Stack
   /** a toString method that may call a continuation on its objects
    */
   def present(cont: Obj => String) = toString 
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
   override def present(cont: Obj => String): String =
      stack.toString + " |- " + cont(t1) + " = " + cont(t2) + (if (t.isDefined) " : " + cont(t.get) else "")
}

/** represents a typing judgement
 * context |- tm : tp
 */
case class Typing(stack: Stack, tm: Term, tp: Term) extends WFJudgement {
  lazy val freeVars = {
    val ret = new HashSet[LocalName]
    val fvs = stack.context.freeVars_ ::: tm.freeVars_ ::: tp.freeVars_
    fvs foreach {n => if (! stack.context.isDeclared(n)) ret += n}
    ret
  }
  val wfo = tm
  override def present(cont: Obj => String): String =
      stack.toString + " |- " + cont(tm) + " : " + cont(tp)
}

case class Universe(stack: Stack, univ: Term) extends WFJudgement {
   lazy val freeVars = {
    val ret = new HashSet[LocalName]
    val fvs = stack.context.freeVars_ ::: univ.freeVars_
    fvs foreach {n => if (! stack.context.isDeclared(n)) ret += n}
    ret
  }
  val wfo = univ
  override def present(cont: Obj => String): String =
      stack.toString + " |- " + cont(univ) + " UNIVERSE"
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

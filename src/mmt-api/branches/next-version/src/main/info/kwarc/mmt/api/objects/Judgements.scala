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
}

/** represents a typing judgement
 * context |- tm : tp
 */
case class Typing(stack: Stack, tm: Term, tp: Term) extends Judgement {
  lazy val freeVars = {
    val ret = new HashSet[LocalName]
    val fvs = stack.context.freeVars_ ::: tm.freeVars_ ::: tp.freeVars_
    fvs foreach {n => if (! stack.context.isDeclared(n)) ret += n}
    ret
  }
}

//TODO case class Inhabitation(name: LocalName, tp: Term) extends Judgement

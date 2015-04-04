package info.kwarc.mmt.api.checking

import info.kwarc.mmt.api._
import objects._

/** A wrapper around a Judgement to maintain meta-information while a constraint is delayed */
abstract class DelayedConstraint(val incomplete: Boolean) {
  protected val freeVars: scala.collection.Set[LocalName]
  val history: History
  private var activatable = false
  /** This must be called whenever a variable that may occur free in this constraint has been solved */
  def solved(names: List[LocalName]) {
     if (! activatable && (names exists {name => freeVars contains name})) activatable = true
  }
  /** @return true if since delaying, a variable has been solved that occurs free in this Constraint */
  def isActivatable = activatable
}

/** A wrapper around a Judgement to maintain meta-information while a constraint is delayed */
class DelayedJudgement(val constraint: Judgement, val history: History, incomplete: Boolean) extends DelayedConstraint(incomplete) {
  protected val freeVars = constraint.freeVars
  override def toString = constraint.toString
}

/** A wrapper around a continuation function to be delayed until a certain type inference succeeds */
class DelayedInference(val stack: Stack, val history: History, val tm: Term, val cont: Term => Boolean) extends DelayedConstraint(false) {
   protected val freeVars = scala.collection.immutable.ListSet(tm.freeVars:_*)
   override def toString = "delayed inference of "  + tm.toString
}

/** wrapper for classes that can occur in the [[History]] */
trait HistoryEntry {
   /** for user-facing rendering */
   def present(implicit cont: Obj => String): String
}

/** a HistoryEntry that consists of a string, meant as a log or error message */
case class Comment(text: () => String) extends HistoryEntry {
   override def toString = text()
   def present(implicit cont: Obj => String) = text()
}

/**
 * The History is a branch in the tree of decisions, messages, and judgements that occurred during type-checking
 * 
 * The most import History's are those ending in an error message.
 * See [[Solver.getErrors]]
 * 
 * @param the nodes of the branch, from leaf to root
 */
class History(private var steps: List[HistoryEntry]) {
   /** creates and returns a new branch with a child appended to the leaf */
   def +(e: HistoryEntry) : History = new History(e::steps)
   /** shortcut for adding a Comment leaf */
   def +(s: => String) : History = this + new Comment(() => s)
   /** appends a child to the leaf */
   def +=(e: HistoryEntry) {steps ::= e}
   /** appends a child to the leaf */
   def +=(s: => String) {this += Comment(() => s)}
   /** creates a copy of the history that can be passed when branching */
   def branch = new History(steps)
   /** get the steps */
   def getSteps = steps
   /**
    * A History produced by the ObjectChecker starts with the ValidationUnit, but the error is only encountered along the way.
    * 
    * @return an educated guess which suffix of the history is most useful 
    */
   def narrowDownError : History = {
      // idea: we start at the comment immediately before the last WFJudgement before the first other Judgement
      var i = steps.length - 1
      var lastWFJ = i
      var continue = true
      while (continue && i >= 0) {
         steps(i) match {
            case j: WFJudgement => lastWFJ = i
            case j: Judgement => continue = false
            case _ =>
         }
         i -= 1
      }
      if (lastWFJ+1 < steps.length && steps(lastWFJ+1).isInstanceOf[Comment]) lastWFJ += 1
      new History(steps.take(lastWFJ+1))
   }
}

/** a history that ignores all messages */
object NoHistory extends History(Nil) {
   override def +=(e: HistoryEntry) {}
   override def +(e: HistoryEntry) = this
   override def branch = this
   override def narrowDownError = this
}
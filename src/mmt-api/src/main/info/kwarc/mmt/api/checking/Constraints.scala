package info.kwarc.mmt.api.checking

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.presentation.Presenter
import objects._

class BranchInfo(val history: History, val backtrack: Branchpoint)

/** A wrapper around a Judgement to maintain meta-information while a constraint is delayed
 *  @param incomplete pursuing this constraint is an incomplete reasoning step
 */
abstract class DelayedConstraint {
  def notTriedYet: Boolean
  val freeVars: scala.collection.Set[LocalName]
  val branchInfo: BranchInfo
  def history = branchInfo.history
  def branch = branchInfo.backtrack
  /** true if a solved variable occurs free in this Constraint */
  def isActivatable(solved: List[LocalName]) = notTriedYet || (solved exists {name => freeVars contains name})
  /** unknown-solving equalities that are sufficient but not necessary to discharge this */
  def suffices : Option[List[Equality]]
}

/** A wrapper around a Judgement to maintain meta-information while a constraint is delayed */
class DelayedJudgement(val constraint: Judgement, val branchInfo: BranchInfo, val suffices: Option[List[Equality]] = None, val notTriedYet: Boolean = false) extends DelayedConstraint {
  val freeVars = constraint.freeVars
  override def toString = constraint.toString
}

/** A wrapper around a continuation function to be delayed until a certain type inference succeeds */
class DelayedInference(val stack: Stack, val branchInfo: BranchInfo, val tm: Term, val cont: Term => Boolean) extends DelayedConstraint {
   def notTriedYet = false
   def suffices = None
   val freeVars = {
     val vs = stack.context.freeVars ++ tm.freeVars
     scala.collection.immutable.ListSet(vs :_*)
   }
   override def toString = "delayed inference of "  + tm.toString
}

/** wrapper for classes that can occur in the [[History]] */
trait HistoryEntry {
   /** for user-facing rendering */
   def present(implicit cont: Obj => String): String
   def indentation(level: Int) = if (level == 0) "" else utils.repeatString("-", level-1) + " "
   /** this history entry but without any wrappers like for indentation */
   def removeWrappers = this
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
 * @param e nodes of the branch, from leaf to root
 */
case class IndentedHistoryEntry(e : HistoryEntry, level: Int) extends HistoryEntry {
   def present(implicit cont: Obj => String): String = indentation(level) + e.present
   override def removeWrappers = e.removeWrappers
}
class History(var steps: List[HistoryEntry]) {
   /** creates and returns a new branch with a child appended to the leaf */
   def +(e: HistoryEntry) : History = {
     new History(IndentedHistoryEntry(e,inc)::steps)
   }
   /** shortcut for adding a Comment leaf */
   def +(s: => String) : History = this + new Comment(() => s)
   /** appends a child to the leaf */
   def +=(e: HistoryEntry) {steps ::= IndentedHistoryEntry(e,inc)}
   /** appends another history to the leaf */
   def +=(h: History) {steps :::= h.steps.map(e => IndentedHistoryEntry(e, inc))}
   /** appends a child to the leaf */
   def +=(s: => String) {this += Comment(() => s)}
   /** pre: that is extension of this, post: this == that */
   def mergeIn(that: History) {
     steps :::= that.steps.take(that.steps.length-this.steps.length)
   }
   /** creates a copy of the history that can be passed when branching */
   def branch = new History(steps)
   /** get the steps */
   def getSteps = steps

   private var inc = 0
   def indented[A](body: => A) = {
     inc += 1
     try {
       body
     } finally {
       inc -=1
     }
   }

  def present(p : Presenter) = steps.map(_.present(o => p.asString(o))).reverse.mkString("\n")

   override def toString = steps.map(_.toString).mkString("\n")

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

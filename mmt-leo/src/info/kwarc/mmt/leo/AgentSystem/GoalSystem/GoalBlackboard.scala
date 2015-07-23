package info.kwarc.mmt.leo.AgentSystem.GoalSystem

import info.kwarc.mmt.api.RuleSet
import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.objects.{Matcher, Context, Obj}
import info.kwarc.mmt.leo.AgentSystem.AndOrSystem.{AndOrBlackboard, AndOrSection}
import info.kwarc.mmt.leo.AgentSystem.{Section, Blackboard}
/*

/**
 * Created by Mark on 7/21/2015.
 *
 * This represents the class of the LF blackboard which handles proofs in the LF prover
 */
class GoalBlackboard(val controller: Controller, val rules:RuleSet,goal: Goal) extends Blackboard {
  implicit val presentObj: Obj => String = o => controller.presenter.asString(o)
  val report = controller.report
  override def logPrefix = "Goal Blackboard"

  val proofSection = new GoalSection(goal)
  sections ::= proofSection
  log("Added Goal of type: " + goal.getClass + goal)
  log(proofSection.toString)

  val shapeDepth = 2
  val factSection = new FactSection(this, shapeDepth)
  sections ::= proofSection
  def facts = factSection.data
  def factsChanges = factSection.changes

  val invertibleBackward = rules.get(classOf[BackwardInvertible]).toList
  val invertibleForward  = rules.get(classOf[ForwardInvertible]).toList
  val searchBackward     = rules.get(classOf[BackwardSearch]).toList.sortBy(_.priority).reverse
  val searchForward      = rules.get(classOf[ForwardSearch]).toList


  /** convenience function to create a matcher in the current situation */
  def makeMatcher(context: Context, queryVars: Context) = new Matcher(controller, rules, context, queryVars)

}

object Indenter {
  def indent(depth: Int) = (0 to depth).map(_ => "  ").mkString("")
}
*/

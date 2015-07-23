package info.kwarc.mmt.leo.AgentSystem.AndOrSystem.MMTSystem

import info.kwarc.mmt.api.RuleSet
import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.objects.Obj
import info.kwarc.mmt.leo.AgentSystem.AndOrSystem.{AndOrBlackboard, AndOrSection}
/*

/**
 * Created by Mark on 7/21/2015.
 *
 * This represents the class of the LF blackboard which handles proofs in the LF prover
 */
class LFBlackboard(val controller: Controller, val rules:RuleSet,goal: LFProofTree) extends AndOrBlackboard[LFProofTree](goal) {
  override val proofTreeSection = new AndOrSection(this) {override type ObjectType = LFProofTree
    override var data: PTType = goal }
  log("Added Goal of type: " + goal.getClass + goal)
  log(proofTreeSection.toString)

  implicit val presentObj: Obj => String = o => controller.presenter.asString(o)

  val shapeDepth = 2
  val factSection = new FactSection(this, shapeDepth)
  def facts = factSection.data
  def factsChanges = factSection.changes
}
*/

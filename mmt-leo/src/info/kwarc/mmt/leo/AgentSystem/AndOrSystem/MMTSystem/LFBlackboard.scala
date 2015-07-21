package info.kwarc.mmt.leo.AgentSystem.AndOrSystem.MMTSystem

import info.kwarc.mmt.api.RuleSet
import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.objects.Obj
import info.kwarc.mmt.leo.AgentSystem.AndOrSystem.{AndOrBlackboard, AndOrSection}

/**
 * Created by Mark on 7/21/2015.
 *
 * This represents the class of the LF blackboard which handles proofs in the LF prover
 */
class LFBlackboard(val controller: Controller, val rules:RuleSet,g: LFProofTree) extends AndOrBlackboard[LFProofTree](g) {
  override val proofSection = new AndOrSection {override type ObjectType = LFProofTree
    override var data: PTType = g }
  log("Added Goal of type: " + g.getClass + g)
  log(proofSection.toString)

  implicit val presentObj: Obj => String = o => controller.presenter.asString(o)


}

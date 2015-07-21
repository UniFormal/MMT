package info.kwarc.mmt.leo.AgentSystem.AndOrSystem.DataTreeSystem.PartitionImpl

import info.kwarc.mmt.leo.AgentSystem.AndOrSystem.AndOrBlackboard
import info.kwarc.mmt.leo.AgentSystem.AndOrSystem.DataTreeSystem.{DataTree, DataTreeSection}

/**
 * Created by Mark on 7/21/2015.
 */
class IntBlackboard(g:DataTree[Int]) extends AndOrBlackboard[DataTree[Int]](g) {
  override val proofSection = new DataTreeSection[Int](g)
  log("Added Goal of type: " + g.getClass + g)
  log(proofSection.toString)
}

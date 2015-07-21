package info.kwarc.mmt.leo.provers

import info.kwarc.mmt.leo.AgentSystem.AndOrSystem.DataTreeSystem.PartitionImpl.{PartitionAgent, PartitionPresenter, IntBlackboard}
import info.kwarc.mmt.leo.AgentSystem.AndOrSystem.DataTreeSystem.DataTree
import info.kwarc.mmt.leo.AgentSystem._

class PartitionProver(target: Int , usableNumbers: List[Int], cycles: Int = 5) {

  val goal = new DataTree(target,conjVar = true,None)
  val blackboard = new IntBlackboard(goal)
  val ra = new PartitionAgent(usableNumbers)
  val pa = new SingletonProofAgent(ra)
  val ma = new AuctionAgent

  blackboard.registerAgent(ra)
  blackboard.registerAgent(pa)
  blackboard.registerAgent(ma)

  def run():String = {
    blackboard.run(cycles)
    PartitionPresenter.present(goal)
  }

}
package info.kwarc.mmt.leo.provers

import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.leo.AgentSystem.AndOrSystem.DataTreeSystem.PartitionImpl.{PartitionAgent, PartitionPresenter, IntBlackboard}
import info.kwarc.mmt.leo.AgentSystem.AndOrSystem.DataTreeSystem.DataTree
import info.kwarc.mmt.leo.AgentSystem._

class PartitionProver(target: Int , usableNumbers: List[Int], cycles: Int = 5)(implicit controller: Controller) {

  val goal = new DataTree(target,conjVar = true,None)
  val blackboard = new IntBlackboard(goal)
  val pa = new PartitionAgent(usableNumbers)
  val aa = new AuctionAgent()
  val ea = new ExecutionAgent()
  pa.register(blackboard)
  aa.register(blackboard.asInstanceOf[aa.BBType])
  ea.register(blackboard.asInstanceOf[ea.BBType])

  def run():String = {
    blackboard.run(cycles)
    (new PartitionPresenter).present(goal)
  }

}
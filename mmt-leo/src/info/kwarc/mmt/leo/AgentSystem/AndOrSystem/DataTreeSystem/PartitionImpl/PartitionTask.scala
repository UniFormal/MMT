package info.kwarc.mmt.leo.AgentSystem.AndOrSystem.DataTreeSystem.PartitionImpl

import info.kwarc.mmt.leo.AgentSystem.AndOrSystem.DataTreeSystem.{DataTree, DataTreeSection}
import info.kwarc.mmt.leo.AgentSystem.AndOrSystem.PTApplicability
import info.kwarc.mmt.leo.AgentSystem.{RuleTask, Section}

/**
 * Created by Mark on 7/21/2015.
 */
class PartitionTask(nodeVar: DataTree[Int], agent: PartitionAgent) extends RuleTask(agent,"PartitionTask") with PTApplicability {

  val node = nodeVar

  val IntTree = new Def[DataTreeSection[Int]]
  override def readList(s: Section): List[s.ObjectType] = {s match {
    case IntTree(t) => List(node.asInstanceOf[s.ObjectType])
    case _ => Nil.asInstanceOf[List[s.ObjectType]]}
  }

  override def writeList(s: Section):List[s.ObjectType] = {s match {
    case IntTree(t) => List(node.asInstanceOf[s.ObjectType])
    case _ => Nil.asInstanceOf[List[s.ObjectType]]}
  }

  var isExpansion = true
  val allNumbers = agent.numbers

  def usableNumbers = allNumbers.filter(_ <= node.data)

  def addBranches(): Unit = {
    usableNumbers.foreach(int=>{
        val add = new DataTree(node.data - int,conjVar=false,None)
        val min: Int = usableNumbers.min
          node.addChild(add)
          if (add.data - int == 0) {
            add.setSat(true)
            add.percolate()
            return
          }
        else if  (add.data < min) {add.setSat(false); add.percolate()}
      }
    )
  }

}

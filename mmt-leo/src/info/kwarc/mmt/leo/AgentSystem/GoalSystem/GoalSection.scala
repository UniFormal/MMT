package info.kwarc.mmt.leo.AgentSystem.GoalSystem

import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.leo.AgentSystem.{Change, Section}


/**
 * This trait capsules the formulas responsible for the formula manipulation of the
 * blackboard.
 *
 */
class GoalSection(blackboard:GoalBlackboard, goal:Goal)(implicit controller: Controller) extends Section(blackboard) {
  override def logPrefix ="AndOrSection"

  /** this type of section only stores data which is a subtype of the AndOr tree type*/
  type ObjectType=Goal
  type PTType=ObjectType //Meaningful alias for object type

  var data:PTType = goal
  var changes: List[Change[_]] = List(new Change(this,goal,List("ADD")))


  def passiveOp(newData:Alternative,flag:String) ={
    handleChange(new Change(this,newData,List(flag)))
  }
  def passiveOp(newData:Goal,flag:String) ={
    handleChange(new Change(this,newData,List(flag)))
  }
  def passiveAdd(newData:Alternative) = passiveOp(newData,"ADD")
  def passiveAdd(newData:Goal) = passiveOp(newData,"ADD")
  def passiveDel(newData:Alternative) = passiveOp(newData,"DEL")
  def passiveDel(newData:Goal) = passiveOp(newData,"DEL")
  def passiveChange(newData:Alternative) = passiveOp(newData,"CHANGE")
  def passiveChange(newData:Goal) = passiveOp(newData,"CHANGE")


}


class FactSection(blackboard:GoalBlackboard,shapeDepth: Int)(implicit controller: Controller) extends Section(blackboard) {
  type ObjectType = Facts
  var data = new Facts(blackboard:GoalBlackboard,shapeDepth: Int)
  var changes: List[Change[_]] = Nil
}


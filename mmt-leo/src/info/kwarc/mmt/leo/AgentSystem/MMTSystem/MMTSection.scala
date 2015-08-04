package info.kwarc.mmt.leo.AgentSystem.MMTSystem

import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.objects.Term
import info.kwarc.mmt.leo.AgentSystem.{Change, Section}


/**
 * This trait capsules the formulas responsible for the formula manipulation of the
 * blackboard.
 *
 */
class GoalSection(blackboard:MMTBlackboard, goal:Goal)(implicit c: Controller,oLP:String) extends Section(blackboard) {
  override def logPrefix =oLP+"#AndOrSection"

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


class FactSection(blackboard:MMTBlackboard,shapeDepth: Int)(implicit c: Controller,oLP:String) extends Section(blackboard) {
  override def logPrefix =oLP+"#FactSection"
  type ObjectType = Facts
  var data = new Facts(blackboard:MMTBlackboard,shapeDepth: Int)
  var changes: List[Change[_]] = Nil

  def passiveOp(flag:String) = handleChange(new Change(this, true, List(flag)))//TODO add specific fact pointers
  def passiveAdd() = passiveOp("ADD")
}

class TermSection(blackboard:MMTBlackboard)(implicit c: Controller,oLP:String) extends Section(blackboard) {
  override def logPrefix =oLP+"#TermSection"
  type ObjectType = Terms
  var data = new Terms(blackboard:MMTBlackboard)
  var changes: List[Change[_]] = Nil

  def passiveOp(flag:String) = handleChange(new Change(this, true, List(flag)))//TODO add specific Term pointers
  def passiveAdd() = passiveOp("ADD")
}

class TransitivitySection(blackboard:MMTBlackboard)(implicit c: Controller,oLP:String) extends Section(blackboard) {
  override def logPrefix =oLP+"#TransitivitySection"
  type ObjectType = List[TransitivityDB]
  var data:ObjectType= Nil
  var changes: List[Change[_]] = Nil


  def passiveOp(flag:String) = handleChange(new Change(this, true, List(flag)))//TODO add specific Term pointers
  def passiveAdd() = passiveOp("ADD")
}

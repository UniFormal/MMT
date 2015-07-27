package info.kwarc.mmt.leo.AgentSystem

import info.kwarc.mmt.api.frontend.{Controller, Logger}

/** the abstract type of a section of the Blackboard,
  * intended to hold and monitor changes in the stored data
  */
abstract class Section(blackboard: Blackboard)(implicit controller: Controller) extends Logger with Speaker{
  var subscribers:List[Listener] = blackboard.agents

  val report = controller.report
  override def logPrefix = "Section"

  /** the type of data that the section holds*/
  type ObjectType
  var data : ObjectType

  /**the list of changes which mark operations on the stored data */
  var changes : List[Change[_]]

  protected def handleChange(newChange:Change[_])={
    changes ::= newChange
    sendToSubscribers(newChange,onlyInterested=true)
  }

  /** function that updates the stored data and adds a change to the changes list*/
  def update(newData:ObjectType, flags:List[String] = List("CHANGE")) = {
    val newChange = new Change(this,(data,newData),flags)
    handleChange(newChange)
    data = newData
  }





  override def addSubscriber(l:Listener) = {
    subscribers::=l
    sendAllInfo(l)
  }

  /** Sends entire change history to new agents who want to be caught up
    * should be overwritten in later extensions for efficiency*/
  def sendAllInfo(l:Listener)={changes.foreach(sendMessage(_,l))}

  override def toString: String = {data.getClass.toString + " Section"}
}


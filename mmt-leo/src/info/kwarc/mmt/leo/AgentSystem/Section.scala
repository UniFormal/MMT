package info.kwarc.mmt.leo.AgentSystem

/** the abstract type of a section of the Blackboard,
  * intended to hold and monitor changes in the stored data
  */
trait Section extends Debugger {
  val logPrefix = "Section"

  /** the type of data that the section holds*/
  type ObjectType
  var data : ObjectType

  /**the list of changes which mark operations on the stored data */
  var changes : List[Change[_]]

  override def toString: String = {data.getClass.toString + " Section"}

  /** function that updates the stored data and adds a change to the changes list*/
  def update(newData:ObjectType, flags:List[String] = List("CHANGE")) = {
    changes = new Change((data,newData),flags) :: changes
    data = newData
  }
}

/**
 * This section handles the lists of various tasks
 */
class TaskSection extends Section {

  type TaskType<:Task
  type ObjectType = List[TaskType]

  var data : ObjectType = Nil
  var changes: List[Change[_]] = Nil

  def add(task: TaskType) = {
    data = task::data
    changes = List(new Change(task,List("ADD")))
  }
}

/** these classes are the specific Sections representing the various tasks*/
class RuleTaskSection extends TaskSection { type TaskType = RuleTask}
class ProofTaskSection extends TaskSection { type TaskType = ProofTask}
class MetaTaskSection extends TaskSection { type TaskType = MetaTask}

package info.kwarc.mmt.leo.AgentSystem

/** Trait which encapsulates an event occurring on the blackboard*/
trait Event{
  var flags: List[String] = Nil
  def hasFlag(f:String): Boolean = flags.contains(f)
  def hasFlag(l:List[String]): Boolean = l.exists(flags.contains(_))
  var readBy: List[Agent] = Nil
  def wasReadBy(a: Agent): Boolean = readBy.contains(a)
}


/** Trait which encapsulates a change in data*/
class Change[T](dataVar:T, flagsVar: List[String]) extends Event{
  val data = dataVar
  flags =flagsVar
}

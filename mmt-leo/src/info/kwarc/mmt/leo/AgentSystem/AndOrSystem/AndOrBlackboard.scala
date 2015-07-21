package info.kwarc.mmt.leo.AgentSystem.AndOrSystem

import info.kwarc.mmt.leo.AgentSystem.Blackboard

/**
 * This class represents a blackboard specialized for proofTrees of any type
 *
 * @param g goal for the blackboard to solve
 * @tparam T type of proof tree that the goal represents
 */
class AndOrBlackboard[T>:Null <:AndOr[T]](g:T) extends Blackboard {

  /** this is the proof section which houses the proof treee
    * beginning with the goal node
    */
  val proofSection = new AndOrSection{type ObjectType = T; var data=g}
  //TODO figure out why cannot log from here
  def proofTree = proofSection.data
  sections =proofSection::sections
}

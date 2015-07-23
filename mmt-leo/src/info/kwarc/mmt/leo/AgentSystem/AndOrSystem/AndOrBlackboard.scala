package info.kwarc.mmt.leo.AgentSystem.AndOrSystem

import info.kwarc.mmt.leo.AgentSystem.{Section, Blackboard}

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
  val proofTreeSection = new AndOrSection(this,g)
  //TODO figure out why cannot log from here
  def proofTree = proofTreeSection.data

  def finished = proofTree.sat.isDefined

  addSection(proofTreeSection)
  log("Added Goal of type: " + g.getClass + g)
  log(this.toString)

}

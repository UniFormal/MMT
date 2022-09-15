package info.kwarc.mmt.api.utils

import scala.concurrent._
import ExecutionContext.Implicits.global

import java.lang.Thread

/**
 * a flag that can be used in multi-threaded applications to signal that processing should be aborted
 * see [[Killable]]
 */

class KillButton {
  @volatile private var killed: Boolean = false

  def press: Unit = {
    killed = true
  }

  def isPressed = killed
}

/** killable objects have a list of kill button any of which can be pressed to abort processing
 *
 *  objects working with killable objects must regularly check the state of the kill button and abort gracefully if pressed
 *
 *  multiple killable objects can share the same kill button;
 *  when creating a new killable object, it is often necessary to pass along an existing kill button
 */
trait Killable {
  private var killButtons: List[KillButton] = List(new KillButton)

  /** signals aborting of processing */
  def kill: Unit = {
    killButtons.foreach(_.press)
  }

  /** processing should be aborted gracefully if true */
  def isKilled = killButtons.exists(_.isPressed)

  /**
   * gives a killable object the same kill button as one that is already around
   *
   * For example, when processing a task, generates a subtask,
   * this should be called on the subtask to ensure killing the overall task also kills the subtask.
   */
  def diesWith(implicit that: Killable): this.type = {
    this.killButtons :::= that.killButtons
    this
  }

  /** presses the kill button after the specified number of milli seconds */
  def setTimeout[A](millisec: Int)(f : () => Unit): this.type = {
    Future {
      Thread.sleep(millisec)
      if (!isKilled) {
        kill
        f()
      }
    }
    this
  }

}
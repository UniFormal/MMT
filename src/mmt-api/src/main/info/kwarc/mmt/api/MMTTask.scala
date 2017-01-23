package info.kwarc.mmt.api

import utils._

/** killing a task signals that all processing of the task should be stopped */
trait MMTTask extends Killable

object MMTTask {
  /** creates a new throwaway task
   *  
   *  it's preferable to write a new subclass of MMTTask, but sometimes a dummy task is more convenient
   */
  def generic = new MMTTask {}
}
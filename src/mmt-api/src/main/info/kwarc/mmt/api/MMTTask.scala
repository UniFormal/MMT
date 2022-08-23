package info.kwarc.mmt.api

import utils._

/** killing a task signals that all processing of the task should be stopped */
trait MMTTask extends Killable {
  /** the unconsumed progress messages */
  private var updates: List[MMTTaskProgress] = Nil
  /** adds a report and forwards it to all listeners */ 
  def reportProgress(a: MMTTaskProgress): Unit = {
    updates ::= a
    listeners.foreach {l => l(a)}
  }
  /** get all reports in reverse chronological order */
  def getReports: List[MMTTaskProgress] = updates

  /** the listeners to which updates are sent */ 
  private var listeners : List[MMTTaskProgressListener] = Nil
  def addListener(l: MMTTaskProgressListener): Unit = {listeners ::= l}
  def removeListener(l: MMTTaskProgressListener): Unit = {listeners = listeners.filterNot(_ == l)}
}

object MMTTask {
  /** creates a new throwaway task
   *
   *  it's preferable to write a new subclass of MMTTask, but sometimes a dummy task is more convenient
   */
  def generic: MMTTask = new MMTTask {}
}

/** see [[MMTTask]] */
abstract class MMTTaskProgressListener {
  def apply(p: MMTTaskProgress): Unit
}

/** parent of all messages indicating progress when carrying out an [[MMTTask]] */
trait MMTTaskProgress

trait MMTInterpretationProgress extends MMTTaskProgress {
  def element: StructuralElement
  def sourceLine: Option[Int] = {
    parser.SourceRef.get(element) map {sref =>
      sref.region.start.line
    }
  }
   
}
/** sent by structure parsers after parsing */
case class Parsed(element: StructuralElement) extends MMTInterpretationProgress
/** sent by structure checker after checking */
case class Checked(element: StructuralElement) extends MMTInterpretationProgress
/** sent by structure simplifier after simplifying */
case class Elaborated(element: StructuralElement) extends MMTInterpretationProgress

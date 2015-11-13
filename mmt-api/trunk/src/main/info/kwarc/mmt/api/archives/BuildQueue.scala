package info.kwarc.mmt.api.archives

import java.util.concurrent.ConcurrentLinkedQueue

import info.kwarc.mmt.api._
import frontend._
import utils._

/** */
class QueuedTask(val target: TraversingBuildTarget, val task: BuildTask, val estDeps: Iterable[Dependency]) {
  var realDeps: List[Dependency] = Nil
}

/** */
sealed abstract class BuildResult

object BuildResult {
  def empty: BuildResult = BuildSuccess(Nil)
}

/** */
case class BuildSuccess(providedContent: List[MPath]) extends BuildResult

/** */
case class BuildFailure(providedContent: List[MPath], missingDeps: List[Dependency]) extends BuildResult


/** any dependency */
abstract class Dependency

/** dependency that can be directly checked or built */
sealed abstract class SimpleDependency extends Dependency

/** dependency on relative file path (without inDim) in archive processed by target */
case class BuildDependency(key: String, archive: Archive, filePath: FilePath) extends SimpleDependency

/** a dependency for directories on built children */
case class DirBuildDependency(dir: BuildTask, children: List[BuildTask], target: String) extends Dependency

/** a dependency on a theory that still needs to be resolved
  *
  * via a catalog to a simple BuildDependency
  */
case class LogicalDependency(mpath: MPath) extends Dependency

/** a dependency on a plain source file given externally
  *
  * only a newer date triggers rebuilding
  */
case class ForeignDependency(file: File) extends SimpleDependency


/** */
abstract class BuildManager extends Extension {
  def addTask(q: QueuedTask)
}

/**
  * builds tasks immediately (no queueing, no parallel processing)
  */
class TrivialBuildManager extends BuildManager {
  def addTask(qt: QueuedTask) {
    qt.target.runBuildTask(qt.task)
  }
}

class BuildQueue extends BuildManager {
  private var queue: ConcurrentLinkedQueue[QueuedTask] = new ConcurrentLinkedQueue()
  private var continue: Boolean = true
  private var stopOnEmpty: Boolean = false

  val sleepTime: Int = 2000

  /** add entry at tail of fifo queue */
  def addTask(qt: QueuedTask) {
    queue.add(qt)
  }

  override def start(args: List[String]) {
    buildThread.start
  }

  override def destroy {
    synchronized {
      continue = false
    }
  }

  def destroyWhenQueueEmpty {
    synchronized {
      stopOnEmpty = true
    }

  }

  val buildThread = new Thread {
    /** get entry at head of fifo queue */
    def getNextTask: Option[QueuedTask] = Option(queue.poll())

    override def run {
      while (continue) {
        getNextTask match {
          case Some(qt) =>
            val res = qt.target.runBuildTask(qt.task)
            res match {
              case BuildSuccess(provided) =>
              case BuildFailure(provided, missing) =>
            }
          case None =>
            if (stopOnEmpty)
              continue = false
            else
              Thread.sleep(sleepTime)
        }
      }
    }
  }

}

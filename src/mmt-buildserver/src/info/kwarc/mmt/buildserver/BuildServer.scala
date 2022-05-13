package info.kwarc.mmt.buildserver

import better.files
import info.kwarc.mmt.api.archives.{Archive, Build, BuildManager, QueuedTask, RedirectableDimension}
import info.kwarc.mmt.api.frontend.Extension
import info.kwarc.mmt.api.utils
import info.kwarc.mmt.api.utils.{File, Git}
import info.kwarc.mmt.api.web.{ServerExtension, ServerRequest, ServerResponse}
import io.methvin.better.files.RecursiveFileMonitor

import java.util.concurrent.ConcurrentLinkedDeque

abstract class BuildServerConfig(val format:String,val inDim: RedirectableDimension) extends Extension {
  def computeDeps(a:Archive,f : File) : Option[List[File]] = None
  def clean(f : File) : Unit = {}
  def applies(ftb : FileToBuild) : Boolean
}

sealed abstract class FileChange
case object Changed extends FileChange
case object Deleted extends FileChange
case object Created extends FileChange

case class FileToBuild(archive : Archive, file : File, dim : RedirectableDimension, change: FileChange)

class BuildServer extends ServerExtension("buildserver") with BuildManager {
  def configs = controller.extman.get(classOf[BuildServerConfig])
  protected lazy val git: Git = utils.OS.git
  /** the queue */
  private val queued: ConcurrentLinkedDeque[QueuedTask] = new ConcurrentLinkedDeque[QueuedTask]
  /** tasks that were queued but are blocked due to missing dependencies; they are to be re-queued when the queue is empty */
  private var blocked: List[QueuedTask] = Nil
  override def apply(request: ServerRequest): ServerResponse = ???

  override def addTasks(w: Build, qts: Iterable[QueuedTask]): Unit = ???

  override def doneBuilding: Boolean = ???

  def updateArchives(as : String*) = {
    //import info.kwarc.mmt.api.archives.source
    val dimensions = configs.map(_.inDim).distinct
    val archives = as.map{s =>
      controller.backend.getArchives.find(_.id == s) match {
        case Some(a) => a
        case None => ???
      }
    }

    var changed_files : List[FileToBuild] = Nil
    archives.foreach {a => dimensions.foreach { dim =>
      val watcher = new RecursiveFileMonitor(better.files.File((a / dim).toJava.toPath)) {
        override def onCreate(file: files.File, count: Int): Unit = changed_files ::= FileToBuild(a,File(file.toJava),dim,Created)
        override def onModify(file: files.File, count: Int): Unit = changed_files ::= FileToBuild(a,File(file.toJava),dim,Changed)
        override def onDelete(file: files.File, count: Int): Unit = changed_files ::= FileToBuild(a,File(file.toJava),dim,Deleted)
      }
      import scala.concurrent.ExecutionContext.Implicits.global
      watcher.start()
      git(a.root,"pull")
      watcher.stop()
    }}
    changed_files
  }
}
package info.kwarc.mmt.api.archives

import java.util.concurrent.ConcurrentLinkedDeque

import info.kwarc.mmt.api._
import frontend._
import utils._
import web._

import scala.annotation.tailrec
import scala.jdk.CollectionConverters._
import scala.collection.immutable.Queue
import scala.collection.mutable

/** input and current state of tasks in the build queue */
class QueuedTask(val target: TraversingBuildTarget, estRes: BuildResult, val task: BuildTask) {
  override def equals(obj: Any): Boolean = obj match {
    case qt: QueuedTask => qt.target == target && qt.task.archive == task.archive && qt.task.inFile == task.inFile
    case _ => false
  }
  override def hashCode(): Int = (target,task.archive,task.inFile).hashCode()

  /** task should be queued at end */
  // TODO make this part of constructor to avoid having a var?
  var lowPriority : Boolean = true

  var originalTarget = target
  def copy(newtarget : TraversingBuildTarget,result : BuildResult = estRes) = {
    val ret = new QueuedTask(newtarget,result,task)
    ret.originalTarget = target
    ret.missingDeps = missingDeps
    ret.willProvide = willProvide
    ret.allowblocking = allowblocking
    ret
  }

  var server_done = false

  var allowblocking : Boolean = true

  /** task should be queued at beginning */
  def highPriority : Boolean = !lowPriority

  /** task was not requested directly but added as dependency of some other task */
  // TODO make this part of constructor to avoid having a var?
  var dependencyClosure: Boolean = false

  /** task was eventually run despite being blocked due to missing dependencies */
  // TODO make this part of constructor to avoid having a var?
  var forceRun: List[Dependency] = Nil

  /** dependencies that are needed for an up-to-date check */
  var neededDeps: List[Dependency] = estRes.used

  /** dependencies that will be used but are not available */
  var missingDeps: List[Dependency] = estRes.used
  /** resources that will be provided once successfully built */
  var willProvide: List[ResourceDependency] = estRes.provided
  /** update policy */
  var updatePolicy : Build = BuildAll

  private def getErrorStrings(ec:ErrorHandler): List[(String,String)] = ec match {
    case ec:ErrorContainer => ec.getErrors.map(e => (e.level.toString + " " + e.getClass.getName.split('.').last + ": " + e.shortMsg ,e.extraMessage))
    case meh:MultipleErrorHandler => meh.handlers.foreach {h =>
        val r = getErrorStrings(h)
        if (r.nonEmpty) return r
      }
      Nil
    case _ => Nil
  }

  def errorStrings = getErrorStrings(task.errorCont)

  def toJString: String = {
    toJStringSimple +
      missingDeps.map(_.toJString).mkString(" [", ", ", "]")
  }
  def toJStringSimple: String = {
    val str = task.inPath.toString
    "[" + task.archive.id + "] " + str + " (" + target.key + ")"
  }

  def toJson: JSONString = JSONString(toJString)

  def merge(qt: QueuedTask): Unit = {
    updatePolicy = updatePolicy.merge(qt.updatePolicy)
    lowPriority = lowPriority && qt.lowPriority
    dependencyClosure = dependencyClosure && qt.dependencyClosure
    // not sure if missingDeps and willProvide should be merged
  }
}

/** output and result state of tasks in the build queue */
sealed abstract class BuildResult {
  /** resources that were used during building */
  def used: List[Dependency]

  /** resources that have been built successfully */
  def provided: List[ResourceDependency]

  /** used for the web interface of the [[BuildQueue]] */
  def toJson: JSON

  /** used for the web interface of the [[BuildQueue]] */
  def toJsonPart: List[(String, JSON)] =
    List(("needed", JSONArray()),
      ("used", JSONArray(Nil/*used.map(_.toJson)*/: _*)),
      ("provided", JSONArray(Nil/*provided.map(_.toJson)*/: _*)))
}

object BuildResult {
  def empty: BuildSuccess = BuildSuccess(Nil, Nil)
  /** convenience method to create the result of successfully importing a (typically) externally checked document */
  def fromImportedDocument(doc: documents.Document) : BuildSuccess = {
    val provs = doc.getDeclarations collect {
      case r: documents.MRef => LogicalDependency(r.target)
    }
    BuildSuccess(Nil, provs)
  }

  case class MissingDependency(paths : List[MPath]) extends Throwable
}

/** default build result */
// TODO what is this? do we need it?
case class BuildEmpty(str: String) extends BuildResult {
  def used: List[Dependency] = Nil
  def provided: List[ResourceDependency] = Nil
  def toJson: JSON = JSONObject(("result", JSONString(str)) :: toJsonPart: _*)
}

/** successful build */
case class BuildSuccess(used: List[Dependency], provided: List[ResourceDependency]) extends BuildResult {
  def toJson: JSON = JSONObject(("result", JSONString("success")) :: toJsonPart: _*)
  def +(that: BuildSuccess): BuildSuccess = BuildSuccess(this.used ::: that.used, this.provided ::: that.provided)
}

/** recoverable failure: build should be retried after building a missing dependency */
case class MissingDependency(needed: List[Dependency], provided: List[ResourceDependency], used: List[Dependency]) extends BuildResult {
  def toJson: JSON = JSONObject(("result", JSONString("failed")) ::
    ("needed", JSONArray(needed.map(_.toJson): _*)) :: toJsonPart.tail: _*)
}

/** unrecoverable failure */
// TODO this should carry an Error or at least a message
case class BuildFailure(used: List[Dependency], provided: List[ResourceDependency]) extends BuildResult {
  def toJson: JSON = JSONObject(("result", JSONString("failure")) :: toJsonPart: _*)
}

/** handles build tasks generated by a [[TraversingBuildTarget]] */
trait BuildManager extends Extension {
  def addTasks(w: Build, qts: Iterable[QueuedTask]): Unit
  // for asynchronous build managers, this should return whether it is currently building
  def doneBuilding: Boolean
}

/** builds tasks immediately (no queueing, no dependency management, no parallel processing) */
class TrivialBuildManager extends BuildManager {
  def addTasks(w: Build, qts: Iterable[QueuedTask]): Unit =
    qts.foreach { qt =>
      qt.target.runBuildTaskIfNeeded(Set(), qt.task, w)
    }
  def doneBuilding = true
}

/** uses a queue of build tasks for multi-threaded execution, includes dependency management */
class BuildQueue extends ServerExtension("queue") with BuildManager {
  override def logPrefix: String = "queue"
  // ******************* state

  /** the queue */
  private val queued: ConcurrentLinkedDeque[QueuedTask] = new ConcurrentLinkedDeque[QueuedTask]
  /** tasks that were queued but are blocked due to missing dependencies; they are to be re-queued when the queue is empty */
  private var blocked: List[QueuedTask] = Nil
  /** true while the BuildQueue is processing the blocked tasks (i.e., after all queued tasks are built), whose missing dependencies could not be resolved */
  private var processingBlockedTasks = false

  /** all queued tasks, indexed by their identifying data */
  private val alreadyQueued: mutable.HashMap[BuildDependency, QueuedTask] = new mutable.HashMap[BuildDependency, QueuedTask]
  /** all built (successfully or permanently-failing) tasks since the last time the queue was empty, indexed by their identifying data */
  private val alreadyBuilt: mutable.HashMap[BuildDependency, BuildResult] = new mutable.HashMap[BuildDependency, BuildResult]

  /** history of recent builds, used only for statistics/reporting */
  private var recentlyBuilt: List[(BuildDependency, BuildResult)] = Nil

  // TODO unclear how this is used
  private var currentQueueTask: Option[QueuedTask] = None

  // TODO unclear how this is used
  private var cycleCheck: Set[BuildDependency] = Set.empty

  /** the catalog mapping from (logical) resource dependency to build dependency */
  // TODO specfiy how this is used
  private val catalog: mutable.HashMap[ResourceDependency, BuildDependency] = new mutable.HashMap[ResourceDependency, BuildDependency]

  /** pause processing (triggered via web interface) */
  private var pauseQueue: Boolean = false
  /** true if the BuildQueue should continue building */
  private var continue: Boolean = true
  /** time to pause when nothing to do */
  private val pauseTime: Int = 2000
  /** true if the BuildQueue should destroy itself once it is empty (used to finish building when the main thread already terminated) */
  private var stopOnEmpty: Boolean = false

  // clears the queue
  override def clear: Unit = {
    queued.clear()
    alreadyBuilt.clear()
    alreadyQueued.clear()
    recentlyBuilt = Nil
    blocked = Nil
    currentQueueTask = None
    cycleCheck = Set.empty
  }

  // ******************* main management interface

  /** adds the ServerExtension :queue and starts a separate thread for building */
  override def start(args: List[String]): Unit = {
    //controller.extman.addExtension(serve)
    buildThread.start()
  }

  /** removes the ServerExtension and signals the build thread to terminate after the current task */
  override def destroy: Unit = {
    //controller.extman.removeExtension(serve)
    synchronized {
      continue = false
    }
  }

  /** signals the build thread to terminate once the queue is empty, then waits for that */
  override def waitUntilRemainingTasksFinished: Unit = {
    synchronized {
      stopOnEmpty = true
    }
    buildThread.join()
  }

  // ******************* task management (queuing and dequeuing)

  def addTasks(w: Build, qts: Iterable[QueuedTask]): Unit = {
    synchronized {
      qts.foreach(addTask(w, _))
    }
  }

  /** adds a single task */
  private def addTask(w: Build, qt: QueuedTask): Unit = {
    // TODO estimateResult should be called here and nowhere else
    qt.updatePolicy = w
    log("added:" + qt.toJString)
    val qtDep = qt.task.asDependency
    qt.willProvide.foreach {rd =>
      if (catalog.contains(rd))
        log(rd.toJString + " in " + catalog(rd).toJString)
      else
        catalog(rd) = qtDep
    }
    if (alreadyBuilt isDefinedAt qtDep) {
      if (qt.dependencyClosure) {
        // dependency of previous job: skip
        return
      } else {
        // new job: build anew
        alreadyBuilt -= qtDep
      }
    }
    if (alreadyQueued isDefinedAt qtDep) {
      if (qt.lowPriority) {
        // low priority: no need to add, skip
        return
      } else {
        // high priority: queue again but adjust updatePolicy
        val qt2 = alreadyQueued(qtDep)
        qt.merge(qt2)
        queued.remove(qt2)
        alreadyQueued -= qtDep
      }
    }
    // add to front/end of queue depending on priority
    if (qt.lowPriority) {
      queued.addLast(qt)
    } else {
      queued.addFirst(qt)
    }
    alreadyQueued(qtDep) = qt
  }

 // ******************* the actual building

  /** the thread for building */
  private lazy val buildThread = new Thread {
    override def run(): Unit = {
      while (continue) {
        if (pauseQueue) Thread.sleep(pauseTime) else {
          getNextTask() match {
            case Some(qt) =>
              log("Doing " + qt.task.inFile)
              // TODO run this in a Future and track dependencies
              qt.task.errorCont.reset
              val res1 = qt.target.runBuildTaskIfNeeded(qt.neededDeps.toSet, qt.task, qt.updatePolicy)
              doBuildResult(res1,qt)
            case None =>
              // no next task in queue
              if (blocked.nonEmpty) {
                //TODO check if this is needed
                // process blocked tasks
                log("flush blocked tasks by ignoring their missing dependencies")
                processingBlockedTasks = true
                val qt = blocked.head
                qt.forceRun = qt.missingDeps
                qt.missingDeps = Nil
                blocked = blocked.tail
                queued.add(qt)
              } else if (stopOnEmpty) {
                // signal termination
                continue = false
              } else {
                // pause and continue later
                if (currentQueueTask.isEmpty) {
                  //TODO check if this is needed
                  cycleCheck = Set.empty
                  alreadyBuilt.clear()
                  processingBlockedTasks = false
                }
                Thread.sleep(pauseTime)
              }
          }
        }
      }
    }
  }

  private def doBuildResult(res1 : BuildResult,qt : QueuedTask): Unit = {
    val res = res1 match {
      // let's assume for now that the estimation is better than the actual result
      case BuildSuccess(u, Nil) =>
        log("Success: " + qt.willProvide.mkString(", "))
        BuildSuccess(u, qt.willProvide)
      case bf@BuildFailure(use,pr) =>
        val newmissings = (queued.toArray(Nil.asInstanceOf[List[QueuedTask]].toArray).toList :::
          blocked).flatMap(_.willProvide).filter(use.contains)
        if (newmissings.nonEmpty) MissingDependency(newmissings,pr,use) else bf
      case MissingDependency(md,pr,use) =>
        val newmissings = (queued.toArray(Nil.asInstanceOf[List[QueuedTask]].toArray).toList :::
          blocked).flatMap(_.willProvide).filter(use.contains) ::: (md collect {
          case ld @ LogicalDependency(mp) if controller.getO(mp).isEmpty => ld
          case pd : PhysicalDependency => pd
        })
        MissingDependency(newmissings.distinct,pr,use)
      case _ => res1
    }
    val qtDep = qt.task.asDependency
    if (!qt.dependencyClosure) cycleCheck -= qtDep
      currentQueueTask = None
      /* add two dummy results into the finished queue to show what happened to a blocked task */
    /*
      if (qt.forceRun.nonEmpty) {
        recentlyBuilt ::= (qtDep, BuildEmpty("was blocked"))
        recentlyBuilt ::= (qtDep, MissingDependency(qt.forceRun, qt.willProvide,qt.missingDeps))
      }
      recentlyBuilt ::= (qtDep, res)
      */ // incomprehensible to me
    res match {
      case _: BuildSuccess | _: BuildFailure | _: BuildEmpty =>
        // remember finished build
        if (!alreadyBuilt.isDefinedAt(qtDep)) {
          alreadyBuilt(qtDep) = res
        }
        recentlyBuilt ::= (qtDep,res)
        res.provided.foreach(catalog(_) = qtDep)
        log(res.getClass.getCanonicalName + ": " + qt.task.inFile)
      // TODO write file errors/.../file.deps
      // XML file containing used, provided, had errors
      case MissingDependency(missing, provided, used) =>
        // register missing dependencies and requeue
        /*
        qt.missingDeps = missing collect {
          case ld @ LogicalDependency(mp) if controller.getO(mp).isEmpty => ld
          case pd : PhysicalDependency => pd
        }
        */
        qt.missingDeps = missing
        log("Missing Deps: " + qt.missingDeps.mkString(", "))
        if (!processingBlockedTasks) {
          // qt.missingDeps = missing
          recentlyBuilt ::= (qtDep, BuildEmpty("was blocked"))
          blocked = blocked ::: List(qt)
        } else {
          recentlyBuilt ::= (qtDep, res)
        }
    }
    unblockTasks(res,qt)
    if (recentlyBuilt.length > 200) {
      recentlyBuilt = recentlyBuilt.take(200)
    }
  }

  /** recursively queues all dependencies of the next task; then returns the head of the queue */
  //TODO why was this method changed to do something very different from what its documentation says?
  private def getTopTask: Option[List[Dependency]] = synchronized {
    currentQueueTask = Option(queued.poll)
    currentQueueTask.foreach(qt => alreadyQueued -= qt.task.asDependency)
    currentQueueTask map {qt =>
      qt.missingDeps.filter {
        case PhysicalDependency(file) => !file.exists
        case _ => true
      }
    }
  }

  private def getNextTask() : Option[QueuedTask] = {
    val currentMissingDeps = getTopTask.getOrElse {return None}
    var failedDeps: List[ResourceDependency] = Nil // the dependencies that we could not resolve
    // resolve all dependencies into build dependencies
    val buildDeps = currentMissingDeps.flatMap {
      case bd: BuildDependency => List(bd)
      case rd: ResourceDependency =>
        //TODO use findResource(rd)
        rd match {
          case ld: LogicalDependency =>
            catalog.get(ld) match {
              case Some(bd) => List(bd)
              case None =>
                failedDeps ::= ld
                Nil
            }
          case pd: PhysicalDependency =>
            failedDeps ::= pd
            Nil
        }
    }
    // queue dependencies if any
    if (currentMissingDeps.nonEmpty) {
      val qt = currentQueueTask.get // is non-empty if deps are missing
      currentQueueTask = None
      qt.missingDeps = failedDeps
      if (failedDeps.nonEmpty) {
        log("blocked: " + qt.toJString + ", missing: " + failedDeps.mkString(", "))
        blocked = blocked ::: List(qt)
      } else {
        // queue this task again along with its dependencies, then try again
        queued.addFirst(qt)
        cycleCheck += qt.task.asDependency
        // old line commented out when deleting old build code
        // buildDeps.foreach(t => addBuildDependency(qt.updatePolicy.forDependencies, t))
      }
      getNextTask()
    } else currentQueueTask
  }

  /** unblock previously blocked tasks whose dependencies have now been provided */
  private def unblockTasks(res: BuildResult,qt : QueuedTask): Unit = {
    res match {
      case MissingDependency(_,_,_) if blocked.contains(qt) =>
      case _ =>
        blocked.foreach { bt =>
          bt.missingDeps = bt.missingDeps diff res.provided
        }
    }
    val (unblocked, stillBlocked) = blocked.partition(_.missingDeps.isEmpty)
    blocked = stillBlocked
    unblocked.foreach(u => log("Unblocked: " + u.task.inFile))
    if (unblocked.nonEmpty) recentlyBuilt ::= ((qt.task.asDependency,BuildEmpty("unblocked: " + unblocked.map(_.task.inFile).mkString(", "))))
    unblocked.reverseIterator.foreach(queued.addFirst)
  }

  // ******************* dependency handling

  /* not used yet - logical dependencies resolved in getNextTask via catalog, file dependencies not resolved yet */
  private def findResource(r: ResourceDependency): Option[BuildDependency] = r match {
    case PhysicalDependency(f) =>
      val (root, out) = controller.backend.resolveAnyPhysical(f).getOrElse(return None)
      controller.addArchive(root)
      val a = controller.backend.getArchive(root).getOrElse(return None)
      out match {
        case FilePath("export" :: key :: _) =>
          // a resource generated by an [[Exporter]]
          val exp = controller.extman.get(classOf[Exporter], key).getOrElse(return None)
          val in = exp.producesFrom(out).getOrElse(return None)
          val bd = FileBuildDependency(key, a, in)
          Some(bd)
        case fp if fp.startsWith(a.resolveDimension(source)) =>
          val imp = controller.extman.get(classOf[Importer], ???).getOrElse(return None) //TODO what importer to use?
          val in = imp.producesFrom(out).getOrElse(return None)
          val bd = FileBuildDependency(imp.key, a, in)
          Some(bd)
        case _ =>
          catalog.get(r)
      }
    case LogicalDependency(mp) =>
      catalog.get(r)
  }

  /** adds tasks for all dependencies of a task (given as a [[BuildDependency]])
    *
    * @param up the update level for the dependency
    * @param bd build dependency to be added
    */
  private def addBuildDependency(w: Build, bd: BuildDependency) : Unit = if (!cycleCheck.contains(bd)) {
    val tar = bd.getTarget(controller)
    val inFile = bd.archive / tar.inDim / bd.inPath
    val bt = bd match {
      case _: FileBuildDependency => tar.makeBuildTask(bd.archive, bd.inPath)
      case dbd: DirBuildDependency => tar.makeBuildTask(dbd.archive, dbd.inPath, dbd.children)
    }
    val estRes = tar.estimateResult(bt)
    val qt = new QueuedTask(tar, estRes, bt)
    qt.lowPriority = false
    qt.dependencyClosure = true
    addTask(w, qt)
  }

  /** true if no jobs left in queue */
  def doneBuilding : Boolean = queued.isEmpty && blocked.isEmpty && currentQueueTask.isEmpty
 
  // ******************* web interface

  /** a ServerExtension for interacting with the queue through the browser */
      private def getQueueInfo: JSON = synchronized {
        val qSize = queued.size
        val iter = queued.iterator.asScala
        val num = 48
        val firsts = (if (qSize > num + 12) iter.take(num) else iter).toList.map(_.toJson)
        val hasMore = iter.hasNext
        val rest = if (hasMore) firsts :+ JSONString("and " + (qSize - num) + " more ...") else firsts
        val q = currentQueueTask.toList.map(q => JSONString("running: " + q.toJString)) ++ rest
        val bs = blocked.map(_.toJson)
        val fs = recentlyBuilt.map {
          case (d, r) =>
            JSONObject("dependency" -> d.toJson, "result" -> r.toJson)
        }
        JSONObject("count" -> JSONInt(qSize),
          "queue" -> JSONArray(q: _*),
          "blocked" -> JSONArray(bs: _*),
          "finished" -> JSONArray(fs: _*))
    }

    def apply(request: ServerRequest): ServerResponse = request.pathForExtension match {
      case List("clear") =>
        clear
        ServerResponse.JsonResponse(JSONNull)
      case List("pause") =>
        // toggles pausing
        pauseQueue = !pauseQueue
        ServerResponse.JsonResponse(JSONBoolean(pauseQueue))
      case List("targets") =>
        // list all targets
        val targets = "mmt-omdoc" :: controller.getConfig.getEntries(classOf[ExtensionConf]).collect {
          case ExtensionConf(key, cls, args) if classOf[BuildTarget].isAssignableFrom(Class.forName(cls)) => key
        }
        ServerResponse.JsonResponse(JSONArray(targets.map(JSONString): _*))
      case List("archives") =>
        val as = controller.backend.getArchives.map(a => a.archString)
        ServerResponse.JsonResponse(JSONArray(as.map(JSONString): _*))
      case _ =>
        ServerResponse.JsonResponse(getQueueInfo)
    }
}

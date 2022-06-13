package info.kwarc.mmt.buildserver

//import better.files
import info.kwarc.mmt.api
import info.kwarc.mmt.api.archives.{Archive, Build, BuildDependency, BuildFailure, BuildManager, BuildQueue, BuildResult, BuildSuccess, BuildTarget, Dependency, Dim, DocumentDependency, LogicalDependency, MissingDependency, PhysicalDependency, QueuedTask, RedirectableDimension, ResourceDependency, TraversingBuildTarget, narration, source}
import info.kwarc.mmt.api.frontend.actions.{ActionCompanion, ActionState, ResponsiveAction}
import info.kwarc.mmt.api.frontend.{Extension, ExtensionConf, NotFound}
import info.kwarc.mmt.api.utils
import info.kwarc.mmt.api.utils.JSONObject.toList
import info.kwarc.mmt.api.utils.{File, Git, JSON, JSONArray, JSONBoolean, JSONInt, JSONNull, JSONObject, JSONString, MMTSystem, MyList}
import info.kwarc.mmt.api.web.{ServerExtension, ServerRequest, ServerResponse}
import info.kwarc.mmt.stex.FullsTeX
//import io.methvin.better.files.RecursiveFileMonitor

import java.util.concurrent.ConcurrentLinkedDeque
import scala.collection.mutable

abstract class BuildServerConfig extends Extension {
  def applies(a : Archive,f:File) : Option[TraversingBuildTarget]
}

object sTeXBuildConfig extends BuildServerConfig {
  override def applies(a: Archive, f: File): Option[TraversingBuildTarget] = if (f.getExtension.contains("tex"))
    controller.extman.get(classOf[FullsTeX]).headOption else None
}
object MMTBuildConfig extends BuildServerConfig {
  override def applies(a: Archive, f: File): Option[TraversingBuildTarget] = if (f.getExtension.contains("mmt"))
    controller.extman.get(classOf[TraversingBuildTarget],"mmt-omdoc") else None

}


class BuildServer extends ServerExtension("buildserver") with BuildManager {
  override def logPrefix: String = "buildserver"
  def configs = controller.extman.get(classOf[BuildServerConfig])
  //protected lazy val git: Git = utils.OS.git
  object State {
    /** the queue */
    private[BuildServer] var toqueue: List[QueuedTask] = Nil
    private[BuildServer] val queue: mutable.Queue[QueuedTask] = mutable.Queue.empty
    /** tasks that were queued but are blocked due to missing dependencies; they are to be re-queued when the queue is empty */
    private[BuildServer] var blocked: List[QueuedTask] = Nil
    private[BuildServer] val running = mutable.HashMap.empty[Int,Option[QueuedTask]]
    private[BuildServer] var finished: List[(QueuedTask,BuildResult)] = Nil
    private[BuildServer] var failed: List[(QueuedTask,BuildResult)] = Nil
    def addSafe(qt : QueuedTask) = this.synchronized { toqueue = toqueue ::: List(qt) }
    private[BuildServer] var threadnumber : Int = 0
  }

  override def start(args: List[String]): Unit = {
    log("Starting")
    controller.extman.get(classOf[BuildQueue]).foreach(bq => {
      controller.extman.removeExtension(bq)
      bq.destroy
    })
    State.synchronized {
      State.threadnumber = args.headOption match {
        case Some(s) if s.forall(_.isDigit) => s.toInt
        case _ => 1
      }
      log("Using " + State.threadnumber + " threads")
    }
    FileDeps.init
    queuemanagementthread.start()
    State.synchronized {
      buildThreads = (0 until State.threadnumber).map { i =>
        val th = BuildThread(i)
        State.running(i) = None
        th.start()
        th
      }.toList
    }
    controller.extman.addExtension(GitUpdateActionCompanion)
    controller.extman.addExtension(sTeXBuildConfig)
    controller.extman.addExtension(MMTBuildConfig)
  }

  override def destroy: Unit = GitUpdateActionCompanion.destroy

  override def apply(request: ServerRequest): ServerResponse = request.pathForExtension match {
    case List("clear") =>
      //clear()
      State.synchronized {
        State.finished = Nil
        State.failed = Nil
      }
      ServerResponse.JsonResponse(JSONNull)
    case List("pause") =>
      // toggles pausing
      //pauseQueue = !pauseQueue
      ServerResponse.JsonResponse(JSONBoolean(true))
    case List("targets") =>
      // list all targets
      val targets = "mmt-omdoc" :: controller.getConfig.getEntries(classOf[ExtensionConf]).collect {
        case ExtensionConf(key, cls, args) if classOf[BuildTarget].isAssignableFrom(Class.forName(cls)) => key
      }
      ServerResponse.JsonResponse(JSONArray(targets.map(JSONString): _*))
    case List("archives") =>
      val as = controller.backend.getArchives.map(a => a.archString)
      ServerResponse.JsonResponse(JSONArray(as.map(JSONString): _*))
    case List("queue") =>
      ServerResponse.JsonResponse(getQueueInfo)
    case _ =>
      ServerResponse.apply(
        MMTSystem.getResourceAsString("/mmt-web/buildserver.html"),"text/html"
      )
  }
  private def getQueueInfo: JSON = State.synchronized {
    val qSize = State.queue.size
    val iter = State.queue.iterator
    val num = 48
    val firsts = (if (qSize > num + 12) iter.take(num) else iter).toList.map(_.toJson)
    val hasMore = iter.hasNext
    val rest = if (hasMore) firsts :+ JSONString("and " + (qSize - num) + " more ...") else firsts
    val q = State.running.values.collect{case Some(qt) => qt}.map(q => JSONString("running: " + q.toJString)).toList ++ rest
    val bs = State.blocked.map(_.toJson)
    val fs = (State.finished ::: State.failed).map {
      case (d,r) =>
        JSONObject("dependency" -> d.toJson, "result" -> r.toJson)
    }
    JSONObject("count" -> JSONInt(qSize),
      "queue" -> JSONArray(q: _*),
      "blocked" -> JSONArray(bs: _*),
      "finished" -> JSONArray(fs: _*))

  }

  override def addTasks(w: Build, qts: Iterable[QueuedTask]): Unit = qts.foreach {
    case q if q.task.isDir =>
      q.forceRun
    case q => State.addSafe(q)
  }

  val buildserver = this

  case class GitUpdateAction(ids:List[String]) extends ResponsiveAction {
    def toParseString = s"gitupdate ${MyList(ids).mkString("[", ",", "]")}"
    override def apply(): Unit = {
      //while(!ready) Thread.sleep(2000)
      val commits = State.synchronized {
        State.failed = Nil
        State.finished = Nil
        ids.flatMap { s =>
          controller.backend.getArchives.filter(_.id.startsWith(s)).map { a =>
            log("pulling " + a.id)
            val git = GitRepo(a)
            (git,git.pullDivs)
          }
        }
      }
      val configs = controller.extman.get(classOf[BuildServerConfig])
      commits.foreach {
        case (a,Some(commit)) =>
          commit.diffs.foreach {
            case Delete(p) =>
              log("Delete [" + a.archive.id + "] " + p)
              FileDeps.delete(a.archive,(a.archive / source) / p)
              None
            case c@(Change(_)|Add(_)) =>
              log("Update [" + a.archive.id + "] " + c.path)
              configs.collectFirst{
                case cf if cf.applies(a.archive,(a.archive / source) / c.path).isDefined =>
                  val bt = cf.applies(a.archive,(a.archive / source) / c.path).get
                  log("Using " + bt.key)
                  controller.handleLine("build " + a.archive.id + " " + bt.key + " " + c.path)
              }
          }
        case _ =>
          None
      }
    }
  }

  def ready : Boolean = State.synchronized {
    State.running.isEmpty && State.blocked.isEmpty && State.queue.isEmpty && State.toqueue.isEmpty
  }

  object GitUpdateActionCompanion extends ActionCompanion("git pull an archive, build changed/new files and notify aterwards","gitupdate") {
    import info.kwarc.mmt.api.frontend.actions.Action._

    override val keywords: List[String] = List("gitupdate")
    override val addKeywords: Boolean = false
    def parserActual(implicit state: ActionState) = strs(keyRegEx) ^^ GitUpdateAction
  }

  private val queuemanagementthread = new Thread {
    override def run(): Unit = while(true) {
      Thread.sleep(2000)
      if (State.synchronized(State.toqueue.nonEmpty)) {
        var go = false
        while (!go) {
          val currlength = State.synchronized(State.toqueue.length)
          Thread.sleep(5000)
          State.synchronized{
            if (State.toqueue.length == currlength) go = true
          }
        }
        order
      }
    }
  }

  override def doneBuilding: Boolean = {
    true
  }

  object FileDeps {
    val targets = mutable.HashMap.empty[Dependency,(TraversingBuildTarget,Archive,File)]
    class FileDeps(val a : Archive,val jsonfile : File) {
      val future = mutable.HashMap.empty[TraversingBuildTarget,mutable.HashSet[(TraversingBuildTarget,Archive,File)]]
      val sourcefile = a / source / (a / RedirectableDimension("buildresults")).relativize(jsonfile).toString.dropRight(5)
      val yields: mutable.HashMap[TraversingBuildTarget, List[Dependency]] = mutable.HashMap.empty
      val dependson: mutable.HashMap[TraversingBuildTarget, List[Dependency]] = mutable.HashMap.empty
      val timestamps: mutable.HashMap[TraversingBuildTarget,Long] = mutable.HashMap.empty

      def update(tg: TraversingBuildTarget,nyields:List[Dependency],ndeps : List[Dependency]) = {
        timestamps(tg) = System.currentTimeMillis / 1000
        yields.get(tg).foreach(_.foreach{d => targets.get(d) match {
          case Some((`tg`,`a`,`sourcefile`)) => targets.remove(d)
          case _ =>
        }})
        dependson.get(tg).foreach(_.foreach{d => targets.get(d) match {
          case Some((ntg,na,src)) =>
            getDep(na,src).future.get(ntg).foreach(_.remove((tg,a,sourcefile)))
          case _ =>
        }})
        yields(tg) = nyields
        dependson(tg) = ndeps
        nyields.foreach{y => targets(y) = (tg,a,sourcefile)}
        ndeps.foreach {d =>
          targets.get(d).foreach{case (ntg,na,src) => getDep(na,src).future.getOrElseUpdate(ntg,mutable.HashSet.empty).add((tg,a,sourcefile))}
        }

        File.write(jsonfile,{
          val ret = (yields.keySet ++ dependson.keySet).toList.map {k =>
            val yls = yields.getOrElse(k,Nil).map(_.toJson).mkString(",")
            val dls = dependson.getOrElse(k,Nil).map(_.toJson).mkString(",")
            "\"" + k.key + "\":{" +
              "\"timestamp\":" + timestamps.getOrElse(tg,0) +
              ",\"yields\":[" + yls + "]" +
              ",\"dependencies\":[" + dls + "]" +
              "}"
          }
          "{" + ret.mkString(",") + "}"
        })
      }
      if (jsonfile.exists()) {
        val obj = JSON.parse(File.read(jsonfile)).asInstanceOf[JSONObject]
        obj.map.foreach {
          case (JSONString(bts),o:JSONObject) =>
            controller.extman.getOrAddExtension(classOf[TraversingBuildTarget],bts,Nil) match {
              case Some(bt) =>
                timestamps(bt) = o.getAs(classOf[BigInt],"timestamp").toLong
                val ylds = o.getAsList(classOf[JSONArray],"yields").map(Dependency.parse)
                ylds.foreach{y => targets(y) = (bt,a,sourcefile)}
                yields(bt) = ylds
                dependson(bt) = o.getAsList(classOf[JSONArray],"dependencies").map(Dependency.parse)
              case _ =>
                println("???")
            }
          case _ =>
            println("???")
        }
      } else {
        jsonfile.up.mkdirs()
        jsonfile.createNewFile()
        File.write(jsonfile,"{}")
      }
    }
    def delete(a : Archive, f : File) = this.synchronized {
      deps.get(a,f) match {
        case Some(fd) =>
          clean(fd.yields.values.flatten.toList)
          fd.jsonfile.delete()
          deps.remove((a,f))
        case _ =>
      }
    }
    private val deps = mutable.HashMap.empty[(Archive,File),FileDeps]
    def getDeps(a : Archive, f: File,tg : TraversingBuildTarget) = this.synchronized(getDepsI(a,f,tg))
    private def getDep(a : Archive,f : File) =  deps.getOrElseUpdate((a,f),{
      val depfile = a / RedirectableDimension("buildresults") / ((a / source).relativize(f).toString + ".json")
      new FileDeps(a,depfile)
    })
    private def getDepsI(a : Archive, f: File,tg : TraversingBuildTarget) = {
      val dep = getDep(a,f)
      dep.dependson.get(tg)
    }
    def getProvides(a : Archive, f:File,tg : TraversingBuildTarget) = this.synchronized(getProvidesI(a,f,tg))
    private def getProvidesI(a : Archive, f:File,tg : TraversingBuildTarget) = {
      val dep = getDep(a,f)
      dep.yields.get(tg)
    }
    private class MutableList[A,B] {
      var ls : List[A] = Nil
      var nexts : List[B] = Nil
    }
    def getFutureCone(a : Archive, f:File,tg : TraversingBuildTarget) = this.synchronized{
      implicit val ml : MutableList[(TraversingBuildTarget,Archive, File),Dependency] = new MutableList
      ml.ls = getDep(a,f).future.getOrElse(tg,Nil).toList
      ml.nexts = getDep(a,f).yields.getOrElse(tg,Nil)
      getFutureConeI(ml)
      ml.ls.distinct
    }
    private def getFutureConeI(implicit ml : MutableList[(TraversingBuildTarget,Archive, File),Dependency]) : Unit = ml.nexts.headOption match {
      case Some(d) =>
        ml.nexts = ml.nexts.tail
        targets.get(d).toList.foreach { case (t, a, f) =>
          if (!ml.ls.contains((t,a,f))) {
            val fut = getDep(a,f).future.getOrElse(t,Nil).toList
            ml.ls = ml.ls ::: ((t, a, f) :: fut)
            ml.nexts = ml.nexts ::: fut.flatMap{case (t,a,f) => getDep(a,f).yields.getOrElse(t,Nil)}
          }
        case _ =>
        }
        getFutureConeI
      case _ =>
    }
    def update(a : Archive, f : File, tg: TraversingBuildTarget,deps : List[Dependency],yields : List[Dependency]) = this.synchronized {
      val dep = getDep(a,f)
      dep.update(tg,yields,deps)
    }
    def init = this.synchronized {
      controller.backend.getArchives.foreach { a =>
        val br = a / RedirectableDimension("buildresults")
        if (br.exists()) {
          br.descendants.foreach { f =>
            if (f.getExtension.contains("json")) {
              val fd = new FileDeps(a, f)
              deps((a, f)) = fd
            }
          }
        }
      }
      deps.values.foreach{fd => fd.dependson.toList.foreach { case (tg,ds) =>
        ds.foreach { d =>
          targets.get(d).foreach{
            case (ntg,na,src) =>
              getDep(na,src).future.getOrElseUpdate(ntg,mutable.HashSet.empty).add((tg,fd.a,fd.sourcefile))
          }
        }
      }}
    }

  }

  private def order = State.synchronized {
    log("Sorting " + State.toqueue.length + " tasks")

    State.toqueue = State.queue.dequeueAll(_ => true).toList ::: State.toqueue

    /*State.toqueue.foreach{ qt =>
      FileDeps.getFutureCone(qt.task.archive,qt.task.inFile,qt.target).foreach {
        case (bt, a, f) if State.toqueue.forall(q => q.target != bt || q.task.archive != a || q.task.inFile != f) =>
          val task = bt.makeBuildTask(a, (a / source).relativize(f).toFilePath)
          State.toqueue ::= new QueuedTask(bt, BuildResult.empty, task)
        case _ =>
      }
    }*/

    var donedeps : List[Dependency] = Nil
    var doneqts : List[QueuedTask] = Nil
    def doQ(q : QueuedTask) : Unit = if (!doneqts.contains(q)) {
      doneqts ::= q
      val provides = FileDeps.getProvides(q.task.archive,q.task.inFile,q.target)
      provides.foreach(_.foreach(donedeps ::= _))
      provides.foreach(l => q.willProvide = l.collect{case dp:ResourceDependency => dp})
      val deps = FileDeps.getDeps(q.task.archive,q.task.inFile,q.target)
      q.neededDeps = deps.getOrElse(Nil)
      provides.foreach(clean)
      deps.foreach{_.foreach {
        case d if !donedeps.contains(d) =>
          FileDeps.targets.get(d).foreach(t => State.toqueue.filterNot(doneqts.contains).find{qt =>
            qt.target == t._1 && qt.task.archive == t._2 && qt.task.inFile == t._3
          }.foreach(doQ))
        case _ =>
      }}
      State.queue.addOne(q)
    }
    State.toqueue.foreach(doQ)
    State.toqueue = Nil
  }

  def clean(toclean : List[Dependency]) = toclean.foreach {
    case LogicalDependency(mp) =>
      try {
        controller.backend.findOwningArchive(mp).foreach { a =>
          a.MMTPathToContentPath(mp).delete()
        }
        controller.library.delete(mp)
      } catch {
        case NotFound(_,_) =>
      }
    case PhysicalDependency(f) => f.delete()
    case DocumentDependency(doc) =>
      try {
        controller.backend.resolveLogical(doc.uri).foreach { case (a, ls) =>
          ((a / narration) / ls.mkString("/")).delete()
        }
        controller.library.delete(doc)
      } catch {
        case NotFound(_,_) =>
      }
    case _ =>
      println("Here!")
  }
  private case class BuildThread(num:Int) extends Thread {
    private def getNext(currents: List[QueuedTask]) : Option[QueuedTask] = {
      var prevs : List[QueuedTask] = Nil
      State.queue.foreach { q =>
        if ((q.missingDeps ::: q.neededDeps).distinct.forall(d => (currents ::: prevs).forall(t => !t.willProvide.contains(d)))) {
          return Some(q)
        } else prevs ::= q
      }
      None
    }
    override def run(): Unit = {
      while (true) {
        State.synchronized {
          val currentrunning = State.running.values.collect {case Some(qt) => qt}.toList
          if (State.queue.nonEmpty) {
            val r = getNext(currentrunning)
            //val r = State.queue.dequeue()
            r.foreach{qt =>
              State.queue.remove(State.queue.indexOf(qt))
              State.running(num) = Some(qt)
            }
            r
          } else if (State.blocked.nonEmpty) {
            //State.blocked.tail.foreach{q => q.allowblocking=false; State.queue.addOne(q)}
            val r = State.blocked.findLast(q => (q.missingDeps ::: q.neededDeps).distinct.forall(d => currentrunning.forall(t => !t.willProvide.contains(d))))
            r.foreach{qt =>
              State.running(num) = Some(qt)
              State.blocked = State.blocked.filterNot(_ == qt)
              qt.allowblocking = false
            }
            r
          } else None
        } match {
          case Some(qt) =>
            log("Doing " + qt.task.inFile)
            qt.task.errorCont.reset
            val res = try {
              qt.target.runBuildTaskIfNeeded(qt.neededDeps.toSet,qt.task,qt.updatePolicy)
            } catch {
              case t : info.kwarc.mmt.api.Error =>
                qt.task.errorCont(t)
                BuildFailure(Nil,Nil)
              case t : Error =>
                val err = new api.Error(t.getMessage) {}
                err.setCausedBy(t)
                qt.task.errorCont(err)
                BuildFailure(Nil,Nil)
            }
            doBuildResult(res,qt,num)
          case _ => Thread.sleep(2000)
        }
      }
    }
  }
  private var buildThreads : List[BuildThread] = Nil


  private def doBuildResult(res : BuildResult,qt : QueuedTask,num:Int): Unit = State.synchronized {
    res match {
      case BuildSuccess(used, provided) =>
        FileDeps.update(qt.task.archive, qt.task.inFile, qt.originalTarget, used, provided)
        qt.neededDeps = used
        val needed = used.filter(g => State.blocked.exists(_.willProvide.contains(g)) || State.queue.exists(_.willProvide.contains(g)))
        if (needed.isEmpty || !qt.allowblocking) {
          log("Success")
          State.finished ::= (qt,res)
        } else {
          log("(blocked) Success, but depends on incomplete targets " + needed.mkString(", "))
          qt.missingDeps = needed
          qt.willProvide = provided
          qt.allowblocking = false
          State.blocked ::= qt.target.onBlock(qt,res)
        }
        State.blocked.foreach { q =>
          q.missingDeps = q.missingDeps.filterNot(provided.contains)
          if (q.missingDeps.isEmpty) {
            log("Unblocking " + q.target.key + ": " + q.task.inFile)
            State.blocked = State.blocked.filterNot(_ == q)
            State.queue.prepend(q)
          }
        }
      case MissingDependency(needed, provided, used) =>
        qt.neededDeps = (used ::: needed).distinct
        if (qt.allowblocking) {
          log("(blocked) Missing dependencies: " + needed.mkString(", "))
          qt.missingDeps = needed
          qt.willProvide = provided
          FileDeps.update(qt.task.archive, qt.task.inFile, qt.originalTarget, used, provided)
          State.blocked ::= qt.target.onBlock(qt, res)
        } else {
          log("Missing dependencies and not allowed to block: " + needed.mkString(", "))
        }
      case BuildFailure(used, provided) =>
        log("Failed")
        qt.neededDeps = used
        val oldprovides = FileDeps.getProvides(qt.task.archive, qt.task.inFile, qt.originalTarget).getOrElse(Nil)
        val olddeps = FileDeps.getDeps(qt.task.archive, qt.task.inFile, qt.originalTarget).getOrElse(Nil)

        FileDeps.update(qt.task.archive, qt.task.inFile, qt.originalTarget, (used ::: olddeps).distinct, (provided ::: oldprovides).distinct)
        val needed = (used ::: olddeps).filter(g => State.blocked.exists(_.willProvide.contains(g)) || State.queue.exists(_.willProvide.contains(g))).distinct
        if (needed.nonEmpty && qt.allowblocking) {
          qt.willProvide = (provided ::: oldprovides).distinct.collect {case r : ResourceDependency => r}
          qt.missingDeps = needed
          log("blocked")
          State.blocked ::= qt.target.onBlock(qt,res)
        } else {
          State.failed ::= (qt,res)
        }
      case _ =>
        println("Here!")
    }
    State.running(num) = None
  }



  /*def updateArchives(as : String*) = {
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
  }*/
}
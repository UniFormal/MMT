package info.kwarc.mmt.api.archives

import java.nio.file.{Files, StandardCopyOption}

import info.kwarc.mmt.api._
import Level.Level
import frontend._
import utils._

sealed abstract class BuildTargetModifier {
  def toString(dim: String): String
}

case object Clean extends BuildTargetModifier {
  def toString(dim: String) = "-" + dim
}

case class BuildDepsFirst(up: UpdateOnError) extends BuildTargetModifier {
  def toString(dim: String) = dim + "&"
}

abstract class Update extends BuildTargetModifier

case class UpdateOnError(errorLevel: Level, dryRun: Boolean = false) extends Update {
  def key: String =
    if (errorLevel <= Level.Force) ""
    else if (errorLevel < Level.Ignore) "!" else "*"

  def toString(dim: String) = dim + key
}

/** forces building independent of status */
case object Build extends Update {
  def toString(dim: String) = dim
}

object BuildTargetModifier extends AnaArgs {
  def optDescrs: OptionDescrs = List(
    OptionDescr("clean", "", NoArg, "clean up"),
    OptionDescr("depsFirst", "", OptIntArg, "treat dependencies first"),
    OptionDescr("depsFirst?", "", OptIntArg, "dry-run dependencies first"),
    OptionDescr("onError", "", OptIntArg, "rebuild on error or change"),
    OptionDescr("onError?", "", OptIntArg, "dry-run on error or change"),
    OptionDescr("onChange", "", NoArg, "rebuild on change"),
    OptionDescr("dry-run", "n", NoArg, "only show what needs to be build"),
    OptionDescr("force", "", NoArg, "force building")
  )

  def makeUpdateModifier(flag: OptionValue, dry: Boolean): UpdateOnError = UpdateOnError(flag match {
    case IntVal(i) => i - 1
    case _ => Level.Error
  }, dryRun = dry)

  def splitArgs(args: List[String], log: String => Unit): Option[(BuildTargetModifier, List[String])] = {
    val (m, r) = anaArgs(optDescrs, args)
    val dr = m.isDefinedAt("dry-run")
    val clean = m.get("clean").toList
    val force = m.get("force").toList
    val onChange = m.get("onChange").toList
    val onError = m.get("onError").toList
    val onErrorDry = m.get("onError?").toList
    val depsFirst = m.get("depsFirst").toList
    val depsFirstDry = m.get("depsFirst?").toList
    val os = clean ++ force ++ onChange ++ onError ++ onErrorDry ++ depsFirst ++ depsFirstDry
    var fail = false
    var mod: BuildTargetModifier = UpdateOnError(Level.Ignore, dryRun = dr)
    if (os.length > 1) {
      log("only one allowed of: clean, force, onChange, onError, depsFirst")
      fail = true
    }
    if (dr && clean.nonEmpty) {
      log("dry-run not possible for clean")
      fail = true
    }
    clean.foreach { _ =>
      mod = Clean
    }
    force.foreach { _ =>
      mod = if (dr) UpdateOnError(Level.Force, dryRun = dr) else Build
    }
    onChange.foreach { _ =>
      mod = UpdateOnError(Level.Ignore, dryRun = dr)
    }
    onError.foreach { o =>
      mod = makeUpdateModifier(o, dr)
    }
    onErrorDry.foreach { o =>
      mod = makeUpdateModifier(o, dry = true)
    }
    depsFirst.foreach { o =>
      mod = BuildDepsFirst(makeUpdateModifier(o, dr))
    }
    depsFirstDry.foreach { o =>
      mod = BuildDepsFirst(makeUpdateModifier(o, dry = true))
    }
    if (fail) {
      usageMessage(optDescrs).foreach(println)
      None
    } else {
      Some((mod, r))
    }
  }
}

/** A BuildTarget provides build/update/clean methods that generate one or more dimensions in an [[Archive]]
  * from an input dimension.
  */
abstract class BuildTarget extends FormatBasedExtension {
  /** a string identifying this build target, used for parsing commands, logging, error messages */
  def key: String

  def isApplicable(format: String): Boolean = format == key

  /** defaults to the key */
  override def logPrefix: String = key

  private def testOps: OptionDescrs = List(
    OptionDescr("quiet", "q", NoArg, "do not show result information"),
    OptionDescr("verbose", "v", NoArg, "show log information"),
    OptionDescr("test", "", NoArg, "compare build results with test dimension"),
    OptionDescr("test-add", "", NoArg, "add new output files to test dimension"),
    OptionDescr("test-update", "", NoArg, "update changed ouput files in test dimension")
  )

  /** options to be overriden by subclasses */
  def buildOpts: OptionDescrs = Nil

  override def start(args: List[String]) {
    val (m, rest) = anaArgs(testOps ++ buildOpts, args)
    optionsMap = m
    remainingStartArguments = rest
    compareWithTest = m.isDefinedAt("test")
    addTest = m.isDefinedAt("test-add")
    updateTest = m.isDefinedAt("test-update")
    verbose = m.isDefinedAt("verbose")
    quiet = m.isDefinedAt("quiet")
    val (otherOpts, _) = splitOptions(rest)
    if (otherOpts.nonEmpty) {
      logError("unknown option: " + otherOpts.mkString(" "))
      usageMessage(ShellArguments.toplevelArgs ++ BuildTargetModifier.optDescrs ++ testOps ++ buildOpts).foreach(println)
    }
  }

  /** the map computed from buildOpts */
  var optionsMap: OptionMap = Map.empty

  /** arguments to be consumed by subclasses */
  var remainingStartArguments: List[String] = Nil

  /** should build results be compared with results in test dimension */
  var compareWithTest: Boolean = false
  var addTest: Boolean = false
  var updateTest: Boolean = false

  var verbose: Boolean = false
  var quiet: Boolean = false

  /** build this target in a given archive */
  def build(a: Archive, in: FilePath) //TODO this should simply call update(a, Build, in)}

  /** update this target in a given archive */
  def update(a: Archive, up: Update, in: FilePath)

  /** build estimated dependencies first */
  def buildDepsFirst(a: Archive, up: Update, in: FilePath = EmptyPath) {}

  /** clean this target in a given archive */
  def clean(a: Archive, in: FilePath)

  /** the main function to run the build target
    *
    * @param modifier chooses build, clean, or update
    * @param arch     the archive to build on
    * @param in       the folder inside the archive's inDim folder to which building in restricted (i.e., Nil for whole archive)
    */
  def apply(modifier: BuildTargetModifier, arch: Archive, in: FilePath) {
    modifier match {
      case Build => build(arch, in)
      case BuildDepsFirst(up) => buildDepsFirst(arch, up, in)
      case up: Update => update(arch, up, in)
      case Clean => clean(arch, in)
    }
  }

  /** auxiliary method for deleting a file */
  protected def delete(f: File) {
    if (f.exists) {
      log("deleting " + f)
      f.delete
    }
  }
}

/** auxiliary type to represent the parameters and result of building a file/directory
  *
  * @param inFile    the input file
  * @param inPath    the path of the input file inside the archive, relative to the input dimension
  * @param children  the build tasks of the children if this task refers to a directory
  * @param outFile   the intended output file
  * @param outPath   the output file inside the archive, relative to the archive root.
  * @param errorCont BuildTargets should report errors here
  */
class BuildTask(val key: String, val archive: Archive, val inFile: File, val children: Option[List[BuildTask]], val inPath: FilePath,
                val outFile: File, val outPath: FilePath, val errorCont: OpenCloseHandler) {
  /** build targets should set this to true if they skipped the file so that it is not passed on to the parent directory */
  var skipped = false
  /** the narration-base of the containing archive */
  val base = archive.narrationBase

  /** the MPath corresponding to the inFile if inFile is a file in a content-structured dimension */
  def contentMPath: MPath = Archive.ContentPathToMMTPath(inPath)

  /** the DPath corresponding to the inFile if inFile is a folder in a content-structured dimension */
  def contentDPath: DPath = Archive.ContentPathToDPath(inPath)

  /** the DPath corresponding to the inFile if inFile is in a narration-structured dimension */
  def narrationDPath: DPath = DPath(base / inPath.segments)

  def isDir = children.isDefined

  /** the name of the folder if inFile is a folder */
  def dirName: String = outFile.toFilePath.dirPath.name

  def asDependency = children match {
    case Some(ch) => DirBuildDependency(key, archive, inPath, ch)
    case None => BuildDependency(key, archive, inPath)
  }
}

/** This abstract class provides common functionality for [[BuildTarget]]s that traverse all files in the input dimension.
  *
  * It implements BuildTarget in terms of the abstract method buildFile called to build a file in the archive.
  * It is also possible to override the method buildDir to post process directory content.
  */
abstract class TraversingBuildTarget extends BuildTarget {
  /** the input dimension/archive folder */
  def inDim: ArchiveDimension

  /** the output archive folder */
  def outDim: ArchiveDimension

  /** if true, multiple files/folders are built in parallel */
  def parallel: Boolean = false

  /** the file extension used for generated files, defaults to outDim, override as needed */
  def outExt: String = outDim match {
    case Dim(path@_*) => path.last
    case d => d.toString
  }

  /** the name that is used for the special file representing the containing folder, empty by default */
  protected val folderName = ""

  protected def getOutFile(a: Archive, inPath: FilePath) = (a / outDim / inPath).setExtension(outExt)

  protected def getTestOutFile(a: Archive, inPath: FilePath) =
    (a / Dim("test", outDim.toString) / inPath).setExtension(outExt)

  protected def getFolderOutFile(a: Archive, inPath: FilePath) = a / outDim / inPath / (folderName + "." + outExt)

  protected def getOutPath(a: Archive, outFile: File) = outFile.toFilePath

  protected def getErrorFile(a: Archive, inPath: FilePath): File = (a / errors / key / inPath).addExtension("err")

  protected def getErrorFile(d: BuildDependency): File = (d.archive / errors / d.key / d.inPath).addExtension("err")

  protected def getFolderErrorFile(a: Archive, inPath: FilePath) = a / errors / key / inPath / (folderName + ".err")

  /** some logging for lmh */
  protected def logResult(s: String) {
    log(s, Some("result"))
  }

  /** there is no inExt, instead we test to check which files should be used;
    * this is often a test for the file extension
    *
    * This must be such that all auxiliary files are skipped.
    * see defaultFileExtension if you need an inExt (for meta targets)
    */
  def includeFile(name: String): Boolean

  /**
    * if this target produces additional files (e.g., the aux files of LaTeX),
    * this method should map them to the respective main file
    *
    * @param outPath the output path (relative to archive)
    * @return the input path (relative to inDim)
    */
  def producesFrom(outPath: FilePath): Option[FilePath] = None

  /** true by default; override to skip auxiliary directories */
  def includeDir(name: String): Boolean = true

  /** the main abstract method that implementations must provide: builds one file
    *
    * @param bf information about input/output file etc
    */
  def buildFile(bf: BuildTask): BuildResult

  /** similar to buildFile but called on every directory (after all its children have been processed)
    *
    * This does nothing by default and can be overridden if needed.
    *
    * @param bd            information about input/output file etc
    * @param builtChildren tasks for building the children
    */
  def buildDir(bd: BuildTask, builtChildren: List[BuildTask]): BuildResult = BuildSuccess(Nil, Nil)

  /** abstract method to compute the estimated direct dependencies */
  def getDeps(bf: BuildTask): Set[Dependency] =
    Set.empty

  /** entry point for recursive building */
  def build(a: Archive, in: FilePath = EmptyPath) {
    build(a, in, None)
  }

  def build(a: Archive, in: FilePath, errorCont: Option[ErrorHandler]) {
    val qts = makeBuildTasks(a, in, errorCont)
    // TODO delete output (and error) file?
    controller.buildManager.addTasks(qts)
  }

  /** like build, but returns all build tasks without adding them to the build manager */
  def makeBuildTasks(a: Archive, in: FilePath, errorCont: Option[ErrorHandler]): List[QueuedTask] = {
    var tasks: List[QueuedTask] = Nil
    buildAux(in, a, errorCont) { qt =>
      tasks ::= qt
    }
    tasks.reverse
  }

  /** recursive creation of [[BuildTask]]s */
  private def buildAux(in: FilePath, a: Archive, eCOpt: Option[ErrorHandler])(cont: QueuedTask => Unit) {
    //build every file
    a.traverse[BuildTask](inDim, in, TraverseMode(includeFile, includeDir, parallel))({
      case Current(inFile, inPath) =>
        val bf = makeBuildTask(a, inPath, inFile, None, eCOpt)
        val qt = new QueuedTask(this, bf)
        qt.missingDeps = getDeps(bf)
        cont(qt)
        bf
    }, {
      case (Current(inDir, inPath), builtChildren) =>
        val bd = makeBuildTask(a, inPath, inDir, Some(builtChildren), None)
        val qt = new QueuedTask(this, bd)
        cont(qt)
        bd
    })
  }

  private def makeHandler(a: Archive, inPath: FilePath, isDir: Boolean = false) = {
    val errFileName = if (isDir) getFolderErrorFile(a, inPath)
    else getErrorFile(a, inPath)
    new ErrorWriter(errFileName, Some(report))
  }

  /** create a [[BuildTask]] from some if its components
    *
    * @param eCOpt optional additional [[ErrorHandler]], errors are always written to errors dimension
    */
  protected def makeBuildTask(a: Archive, inPath: FilePath, inFile: File,
                              children: Option[List[BuildTask]], eCOpt: Option[ErrorHandler]): BuildTask = {
    val errorWriter = makeHandler(a, inPath, children.isDefined)
    val errorCont = eCOpt match {
      case None => errorWriter
      case Some(eC) => new MultipleErrorHandler(List(eC, errorWriter))
    }
    val outFile = if (children.isDefined) getFolderOutFile(a, inPath) else getOutFile(a, inPath)
    val outPath = getOutPath(a, outFile)
    new BuildTask(key, a, inFile, children, inPath, outFile, outPath, errorCont)
  }

  def compareOutputAndTest(bt: BuildTask) {
    val testFile = getTestOutFile(bt.archive, bt.inPath)
    val diffFile = testFile.addExtension("diff")
    val outFile = bt.outFile
    if (!outFile.exists) {
      logError("missing output file: " + outFile)
    } else if (testFile.exists) {
      val diffLog = ShellCommand.run("diff", "-u", outFile.toString, testFile.toString)
      if (diffLog.isDefined) {
        File.write(diffFile, diffLog.get)
        logResult("wrote: " + diffFile)
      } else {
        logResult("no differences for: " + outFile)
      }
      if (diffLog.isDefined && updateTest) {
        Files.copy(outFile.toPath, testFile.toPath, StandardCopyOption.REPLACE_EXISTING)
        logResult("updated " + testFile)
      }
    } else {
      if (addTest) {
        testFile.up.mkdirs
        Files.copy(outFile.toPath, testFile.toPath)
        logResult("added " + testFile)
      }
    }
  }

  def buildFileAndCompare(bt: BuildTask): BuildResult = {
    val res = buildFile(bt)
    if (compareWithTest) {
      compareOutputAndTest(bt)
    }
    res
  }

  /** like buildFile but with error handling, logging, etc.  */
  def runBuildTask(bt: BuildTask): BuildResult = {
    if (!bt.isDir) {
      val prefix = "[" + inDim + " -> " + outDim + "] "
      report("archive", prefix + bt.inFile + " -> " + bt.outFile)
      bt.outFile.up.mkdirs
    }
    var res: BuildResult = BuildResult.empty
    bt.errorCont.open
    try {
      res = bt.children match {
        case None => buildFileAndCompare(bt)
        case Some(children) =>
          buildDir(bt, children)
      }
    } catch {
      case e: Error => bt.errorCont(e)
      case e: Exception =>
        val le = LocalError("unknown build error: " + e.getMessage).setCausedBy(e)
        bt.errorCont(le)
        res = BuildFailure(Nil, Nil)
    } finally {
      bt.errorCont.close
    }
    if (!bt.isDir) controller.notifyListeners.onFileBuilt(bt.archive, this, bt.inPath)
    res
  }

  /** additional method that implementations may provide: cleans one file
    *
    * deletes the output and error file by default, may be overridden to, e.g., delete auxiliary files
    *
    * @param a    the containing archive
    * @param curr the inDim whose output is to be deleted
    */
  def cleanFile(a: Archive, curr: Current) {
    val inPath = curr.path
    val outFile = getOutFile(a, inPath)
    delete(outFile)
    delete(getErrorFile(a, inPath))
    controller.notifyListeners.onFileBuilt(a, this, inPath)
  }

  /** additional method that implementations may provide: cleans one directory
    *
    * does nothing by default
    *
    * @param a    the containing archive
    * @param curr the outDim directory to be deleted
    */
  def cleanDir(a: Archive, curr: Current) {
    delete(getFolderErrorFile(a, curr.path))
  }

  /** recursively delete output files in parallel (!) */
  def clean(a: Archive, in: FilePath = EmptyPath) {
    a.traverse[Unit](inDim, in, TraverseMode(includeFile, includeDir, parallel = true))(
      { c => cleanFile(a, c) }, { case (c, _) => cleanDir(a, c) })
  }

  private def modified(inFile: File, errorFile: File): Boolean = {
    val mod = Modification(inFile, errorFile)
    mod == Modified || mod == Added
  }

  /** @return status of input file, obtained by comparing to error file */
  private def hadErrors(errorFile: File, errorLevel: Level): Boolean =
    if (errorLevel > Level.Fatal)
      false // nothing is more severe than a fatal error
    else
      errorFile.exists && ErrorReader.getBuildErrors(errorFile, errorLevel, None).nonEmpty

  /** recursively reruns build if the input file has changed
    *
    * the decision is made based on the time stamps and the system's last-modified date
    */
  def update(a: Archive, up: Update, in: FilePath) {
    a.traverse[(Boolean, BuildTask)](inDim, in, TraverseMode(includeFile, includeDir, parallel))({
      case c@Current(inFile, inPath) =>
        val errorFile = getErrorFile(a, inPath)
        val outFile = getOutFile(a, inPath)
        val testFile = getTestOutFile(a, inPath)
        val outPath = getOutPath(a, outFile)
        lazy val bf = makeBuildTask(a, inPath, inFile, None, None)
        val rebuildNeeded = up match {
          case Build => true
          case up: UpdateOnError =>
            lazy val errs = hadErrors(errorFile, up.errorLevel)
            val rn = up.errorLevel <= Level.Force || modified(inFile, errorFile) || errs ||
              getDeps(bf).exists {
                case bd: BuildDependency =>
                  val errFile = getErrorFile(bd)
                  modified(errFile, errorFile)
                case ForeignDependency(fFile) => modified(fFile, errorFile)
                case _ => false
              }
            if (!rn) {
              logResult("up-to-date " + outPath)
            }
            if (rn && up.dryRun) {
              logResult("out-dated " + outPath)
              false
            } else {
              rn
            }
        }
        if (rebuildNeeded) {
          runBuildTask(bf)
        }
        if (compareWithTest) {
          compareOutputAndTest(bf)
        }
        (rebuildNeeded, bf)
    }, { case (c@Current(inDir, inPath), childChanged) =>
      val changes = childChanged.exists(_._1)
      val children = childChanged.map(_._2)
      val bd = makeBuildTask(a, inPath, inDir, Some(children), None)
      if (changes) {
        runBuildTask(bd)
      }
      (changes, bd)
    })
  }

  //TODO sort this out
  /*
   here is indeed some duplicate work.
   getFilesRec is similar to makeBuildTasks, both traverse the folders recursively
   - getFilesRec returns files to be built as dependencies (but no directories)
   - makeBuildTasks returns QueuedTasks (also for directories)
   QueuedTasks (bad name?) are created from BuildTasks still to be queued by addTasks in build
   (the construction via traverse, a continuation and reverse is overkill compared to getFilesRec)

   we still have 4 separate actions: build, update, depsFirst and clean (where clean is undisputed)
   - tasks are currently only collected and queued for "build"!
   - "build" should be a special case of "update", however
   "update" needs to perform the up-to-date test to exclude some task that need not to be rebuild
   but the up-to-date test should be made by the queue manager.

   "depsFirst" is a wrapper around the update action, where dependent task are updated earlier
    via the estimated dependencies getDeps. (This only works if the estimated dependencies are
    at least the actual dependencies and are non-cyclic.)
  */

  protected def getFilesRec(a: Archive, in: FilePath): Set[Dependency] = {
    val inFile = a / inDim / in
    if (inFile.isDirectory)
      inFile.list.flatMap(n => getFilesRec(a, FilePath(in.segments ::: List(n)))).toSet
    else if (inFile.isFile && includeFile(inFile.getName) && includeDir(inFile.up.getName))
      Set(BuildDependency(key, a, in))
    else Set.empty
  }

  /** makes a build task for a single file */
  def makeBuildTask(a: Archive, inPath: FilePath): BuildTask = {
    makeBuildTask(a, inPath, a / inDim / inPath, None, None)
  }

  def getAnyDeps(dep: BuildDependency): Set[Dependency] = {
    if (dep.key == key)
      getDeps(makeBuildTask(dep.archive, dep.inPath))
    else controller.extman.getOrAddExtension(classOf[BuildTarget], dep.key, Nil) match {
      case bt: TraversingBuildTarget => bt.getDeps(bt.makeBuildTask(dep.archive, dep.inPath))
      //TODO resolve non-simple dependencies
      case _ => Set.empty
    }
  }

  def getTopsortedDeps(args: Set[Dependency]): List[Dependency] = {
    var visited: Set[Dependency] = Set.empty
    var unknown = args
    var deps: Map[Dependency, Set[Dependency]] = Map.empty
    while (unknown.nonEmpty) {
      val p = unknown.head
      val ds: Set[Dependency] = p match {
        case bd: BuildDependency => getAnyDeps(bd)
        case _ => Set.empty
      }
      deps += ((p, ds))
      visited += p
      unknown -= p
      unknown ++= ds.diff(visited)
    }
    Relational.flatTopsort(controller, deps)
  }

  override def buildDepsFirst(a: Archive, up: Update, in: FilePath = EmptyPath) {
    val ts = getTopsortedDeps(getFilesRec(a, in))
    ts.foreach {
      case bd: BuildDependency => if (bd.key == key) update(bd.archive, up, bd.inPath)
      else controller.extman.getOrAddExtension(classOf[BuildTarget], bd.key, Nil) match {
        case bt: TraversingBuildTarget => bt.update(bd.archive, up, bd.inPath)
        case _ => log("build target not found: " + bd.key)
      }
      case _ =>
    }
  }
}

package info.kwarc.mmt.api.archives

import info.kwarc.mmt.api.Level.Level
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.frontend._
import info.kwarc.mmt.api.utils._

sealed abstract class BuildTargetModifier {
  def toString(dim: String): String
}

case object Clean extends BuildTargetModifier {
  def toString(dim: String): String = "-" + dim
}

case class Update(errorLevel: Level) extends BuildTargetModifier {
  def key: String = if (errorLevel <= Level.Force) ""
  else
  if (errorLevel < Level.Ignore) "!" else "*"

  def toString(dim: String): String = dim + key
}

case object Build extends BuildTargetModifier {
  def toString(dim: String): String = dim
}

case class BuildDepsFirst(errorLevel: Level) extends BuildTargetModifier {
  def toString(dim: String): String = dim + "&"
}

/** A BuildTarget provides build/update/clean methods that generate one or more dimensions in an [[Archive]]
  * from an input dimension.
  */
abstract class BuildTarget extends FormatBasedExtension with Dependencies {
  /** a string identifying this build target, used for parsing commands, logging, error messages
    */
  def key: String

  /** default file extension to be used by meta targets
    *
    * needs to be overwritten by build targets for sources
    * */
  def defaultFileExtension: String = "omdoc"

  def isApplicable(format: String): Boolean = format == key

  /** defaults to the key */
  override def logPrefix: String = key

  /** build this target in a given archive */
  def build(a: Archive, in: FilePath): Unit

  /** update this target in a given archive */
  def update(a: Archive, up: Update, in: FilePath): Unit

  /** clean this target in a given archive */
  def clean(a: Archive, in: FilePath): Unit

  /** build this target in a given archive but build dependencies first */
  def buildDepsFirst(arch: Archive, errorLevel: Level, in: FilePath): Unit = {}

  /** the main function to run the build target
    *
    * @param modifier chooses build, clean, or update
    * @param arch the archive to build on
    * @param in the folder inside the archive's inDim folder to which building in restricted (i.e., Nil for whole archive)
    */
  def apply(modifier: BuildTargetModifier, arch: Archive, in: FilePath): Unit = {
    modifier match {
      case up: Update => update(arch, up, in)
      case Clean => clean(arch, in)
      case Build => build(arch, in)
      case BuildDepsFirst(lvl) => buildDepsFirst(arch, lvl, in)
    }
  }

  /** auxiliary method for deleting a file */
  protected def delete(f: File): Unit = {
    if (f.exists) {
      log("deleting " + f)
      f.delete
    }
  }
}

/**
 * auxiliary type to represent the parameters and result of building a file/directory
 *
 * @param inFile the input file
 * @param inPath the path of the input file inside the archive, relative to the input dimension
 * @param outFile the intended output file
 * @param outPath the output file inside the archive, relative to the archive root.
 * @param errorCont BuildTargets should report errors here
 */
class BuildTask(val archive: Archive, val inFile: File, val isDir: Boolean, val inPath: FilePath,
                val outFile: File, val outPath: FilePath, val errorCont: ErrorHandler) {
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

  /** the name of the folder if inFile is a folder */
  def dirName: String = outFile.filepath.dirPath.baseName

}

/**
 * This trait provides common functionality for BuildTargets that traverse all files in the input dimension.
 *
 * It implements BuildTarget in terms of a single abstract method called to build a path in the archive.
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

  protected def getFolderOutFile(a: Archive, inPath: FilePath) = a / outDim / inPath / (folderName + "." + outExt)

  protected def getOutPath(a: Archive, outFile: File) = outFile.filepath

  protected def getErrorFile(a: Archive, inPath: FilePath): File = (a / errors / key / inPath).addExtension("err")

  protected def getErrorFile(d: Dependency): File = (d.archive / errors / d.target / d.filePath).addExtension("err")

  protected def getFolderErrorFile(a: Archive, inPath: FilePath) = a / errors / key / inPath / (folderName + ".err")

  /** some logging for lmh */
  protected def logResult(s: String) = log(s, Some("result"))

  /**
   * there is no inExt, instead we test to check which files should be used;
   * this is often a test for the file extension
   *
   * This must be such that all auxiliary files are skipped.
   */
  def includeFile(name: String): Boolean

  /**
   * true by default; override to skip auxiliary directories
   * TODO actually check for this
   */
  def includeDir(name: String): Boolean = true

  /** the main abstract method that implementations must provide: builds one file
    * @param bf information about input/output file etc
    */
  def buildFile(bf: BuildTask): Unit

  /** similar to buildOne but called on every directory (after all its children have been processed)
    * @param bd information about input/output file etc
    * @param builtChildren results from building the children
    *                      This does nothing by default and can be overridden if needed.
    */
  def buildDir(bd: BuildTask, builtChildren: List[BuildTask]): Unit = {}

  /** entry point for recursive building */
  def build(a: Archive, in: FilePath = EmptyPath): Unit = build(a, in, None)

  def build(a: Archive, in: FilePath, errorCont: Option[ErrorHandler]): Unit =
    buildAux(in)(a, errorCont)

  private def makeHandler(a: Archive, inPath: FilePath, isDir: Boolean = false) = {
    val errFileName = if (isDir) getFolderErrorFile(a, inPath)
    else getErrorFile(a, inPath)
    new ErrorWriter(errFileName, Some(report))
  }

  /** recursive building */
  private def buildAux(in: FilePath = EmptyPath)(implicit a: Archive, eCOpt: Option[ErrorHandler]): Unit = {
    //build every file
    val prefix = "[" + inDim + " -> " + outDim + "] "
    a.traverse[BuildTask](inDim, in, TraverseMode(includeFile, includeDir, parallel))({
      case Current(inFile, inPath) =>
        if (!inFile.isFile)
          throw LocalError("file does not exist: " + inPath)
        val outFile = getOutFile(a, inPath)
        val outPath = getOutPath(a, outFile)
        report("archive", prefix + inFile + " -> " + outFile)
        val errorWriter = makeHandler(a, inPath)
        val errorCont = eCOpt match {
          case None => errorWriter
          case Some(eC) => new MultipleErrorHandler(List(eC, errorWriter))
        }
        val bf = new BuildTask(a, inFile, false, inPath, outFile, outPath, errorCont)
        outFile.up.mkdirs
        try {
          buildFile(bf)
        } catch {
          case e: Error => errorCont(e)
          case e: Exception =>
            val le = LocalError("unknown build error: " + e.getMessage).setCausedBy(e)
            errorCont(le)
        } finally {
          errorWriter.close()
        }
        controller.notifyListeners.onFileBuilt(a, this, inPath)
        // a.timestamps(this).set(inPath) not needed anymore
        bf
    }, {
      case (Current(inDir, inPath), builtChildren) =>
        val outFile = getFolderOutFile(a, inPath)
        val errorCont = makeHandler(a, inPath, isDir = true)
        val outPath = getOutPath(a, outFile)
        val bd = new BuildTask(a, inDir, true, inPath, outFile, outPath, errorCont)
        buildDir(bd, builtChildren)
        errorCont.close()
        bd
    })
  }

  /** additional method that implementations may provide: cleans one file
    * @param a the containing archive
    * @param curr the inDim whose output is to be deleted
    *             deletes the output and error file by default, may be overridden to, e.g., delete auxiliary files
    */
  def cleanFile(a: Archive, curr: Current): Unit = {
    val inPath = curr.path
    val outFile = getOutFile(a, inPath)
    delete(outFile)
    delete(getErrorFile(a, inPath))
    logResult("deleted " + getOutPath(a, outFile))
    controller.notifyListeners.onFileBuilt(a, this, inPath)
  }

  /** additional method that implementations may provide: cleans one directory
    * @param a the containing archive
    * @param curr the outDim directory to be deleted
    *             does nothing by default
    */
  def cleanDir(a: Archive, curr: Current): Unit = {
    delete(getFolderErrorFile(a, curr.path))
  }

  /** recursively delete output files in parallel (!) */
  def clean(a: Archive, in: FilePath = EmptyPath): Unit = {
    a.traverse[Unit](inDim, in, TraverseMode(includeFile, includeDir, parallel = true))(
    { c => cleanFile(a, c) }, { case (c, _) => cleanDir(a, c) })
  }

  private def modified(inFile: File, errorFile: File): Boolean = {
    val mod = Modification(inFile, errorFile)
    mod == Modified || mod == Added
  }

  /** @return status of input file, obtained by comparing to error file */
  private def hadErrors(errorFile: File, errorLevel: Level): Boolean =
    if (errorLevel > Level.Fatal) false // nothing is more severe than a fatal error
    else
      errorFile.exists && ErrorReader.getBuildErrors(errorFile, errorLevel, None).nonEmpty

  /** recursively reruns build if the input file has changed
    *
    * the decision is made based on the time stamps and the system's last-modified date
    */
  def update(a: Archive, up: Update, in: FilePath = EmptyPath): Unit = {
    a.traverse[Boolean](inDim, in, TraverseMode(includeFile, includeDir, parallel))({
      case c@Current(inFile, inPath) =>
        val errorFile = getErrorFile(a, inPath)
        val inFile = a / inDim / inPath
        val errs = hadErrors(errorFile, up.errorLevel)
        val res = up.errorLevel <= Level.Force || modified(inFile, errorFile) || errs ||
          getSingleDeps(controller, a, inPath).exists{ p =>
           val errFile = getErrorFile(p)
           modified(errFile, errorFile)
           }
        if (res) buildAux(inPath)(a, None)
        else logResult("up-to-date " + getOutPath(a, getOutFile(a, inPath)))
        res
    }, { case (c@Current(inDir, inPath), childChanged) =>
      if (childChanged.contains(true)) {
        val outFile = getFolderOutFile(a, inPath)
        val outPath = getOutPath(a, outFile)
        val errorCont = makeHandler(a, inPath, isDir = true)
        val bd = new BuildTask(a, inDir, true, inPath, outFile, outPath, errorCont)
        errorCont.close()
        buildDir(bd, Nil) // TODO pass proper builtChildren
      }
      false
    })
  }

  protected def getFilesRec(a: Archive, in: FilePath): Set[Dependency] = {
    val inFile = a / inDim / in
    if (inFile.isDirectory && includeDir(inFile.getName))
      inFile.list.flatMap(n => getFilesRec(a, FilePath(in.segments ::: List(n)))).toSet
    else if (inFile.isFile && includeFile(inFile.getName))
      Set(Dependency(a, in, key))
    else Set.empty
  }

  override def buildDepsFirst(a: Archive, errorLevel: Level, in: FilePath = EmptyPath): Unit = {
    // includeFile will be checked by build again (as was already checked during dependency analysis)
    val ts = getDeps(controller, key, getFilesRec(a, in))
    ts.foreach(d => if (d.target == key) update(d.archive, Update(errorLevel), d.filePath)
    else controller.getBuildTarget(d.target) match {
      case Some(bt) => bt.update(d.archive, Update(errorLevel), d.filePath)
      case None => log("build target not found: " + d.target)
    })
  }
}

/**
 * a build target that chains multiple other targets
 */
class MetaBuildTarget extends BuildTarget {
  private var _key = ""
  private var _inExt = ""
  private var targets: List[BuildTarget] = Nil
  var startArgs: List[String] = Nil

  def key: String = _key

  override def defaultFileExtension: String = _inExt

  /**
   * first argument: the key of this build target
   * remaining arguments: the build targets to chain
   */
  override def start(args: List[String]): Unit = {
    startArgs = args
    _key = args.headOption.getOrElse(
      throw LocalError("at least one argument required")
    )
    targets = args.tail.map(k =>
      controller.extman.get(classOf[BuildTarget]).find(_.getClass.getName == k).getOrElse {
        throw LocalError("unknown target: " + k)
      })
    _inExt = targets.map(_.defaultFileExtension).headOption.getOrElse(
      throw LocalError("at least one target required")
    )
  }

  /** @return the path to pass to the target t, override as needed */
  def path(a: Archive, t: BuildTarget, inPath: FilePath): FilePath = {
    t match {
      case t: TraversingBuildTarget if t.inDim != content =>
        val file = a / t.inDim / inPath
        val in = if (file.isDirectory || t.includeFile(inPath.baseName)) inPath
        else
          inPath.toFile.setExtension(t.defaultFileExtension).filepath
        log("trying " + t.key + " in " + a.id + " with " + (t.inDim :: in.segments).mkString("/"))
        in
      case _ =>
        log("ignoring " + t.key + " in " + a.id +
          (if (inPath.segments.isEmpty) "" else " for " + inPath))
        EmptyPath
    }
  }

  def build(a: Archive, in: FilePath): Unit = {
    targets.foreach { t => t.build(a, path(a, t, in)) }
  }

  override def buildDepsFirst(a: Archive, errorLevel: Level, in: FilePath): Unit = {
    targets.foreach { t => t.buildDepsFirst(a, errorLevel, path(a, t, in)) }
  }

  def update(a: Archive, up: Update, in: FilePath): Unit = {
    targets.foreach { t => t.update(a, up, path(a, t, in)) }
  }

  def clean(a: Archive, in: FilePath): Unit = {
    targets.reverse.foreach { t => t.clean(a, path(a, t, in)) }
  }
}

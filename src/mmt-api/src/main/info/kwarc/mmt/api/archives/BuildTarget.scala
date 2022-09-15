package info.kwarc.mmt.api.archives

import java.nio.file.{Files, StandardCopyOption}

import info.kwarc.mmt.api._
import Level.Level
import frontend._
import utils._

/** A BuildTarget provides build/update/clean methods that generate one or more dimensions in an [[Archive]]
  * from an input dimension.
  */
abstract class BuildTarget extends FormatBasedExtension {
  /** a string identifying this build target, used for parsing commands, logging, error messages */
  def key : String

  override def toString : String = super.toString + " with key " + key

  def isApplicable(format: String) : Boolean = format == key

  /** defaults to the key */
  override def logPrefix : String = key

  /** build or update this target in a given archive */
  def build(a: Archive, which: Build, in: FilePath, errorCont: Option[ErrorHandler]) : Unit

  /** clean this target in a given archive */
  def clean(a: Archive, in: FilePath): Unit

  /** the main function to run the build target
    *
    * en empty in filepath addresses the whole archive
    *
    * @param modifier chooses build, clean, or update
    * @param arch     the archive to build on
    * @param in       the folder inside the archive's inDim folder to which building is restricted
    * @param errorCont continuation for reporting errors that this target recovered from (fatal errors should be thrown instead)
    */
  def apply(modifier: BuildTargetModifier, arch: Archive, in: FilePath, errorCont: Option[ErrorHandler]): Unit = {
    modifier match {
      case w:Build => build(arch, w, in, errorCont)
      case Clean => clean(arch, in)
    }
  }

  /** auxiliary method for deleting a file */
  protected def delete(f: File): Unit = {
    if (f.exists) {
      log("deleting " + f)
      f.deleteDir
    }
  }
}

/** when calling a [[BuildTarget]] we can use modifiers for, e.g., cleaning */
sealed abstract class BuildTargetModifier {
  def toString(dim: String): String
}

/** don't run, just delete all output files */
case object Clean extends BuildTargetModifier {
  def toString(dim: String) : String = "-" + dim
}

/** default modifier: build the target
  */
sealed abstract class Build extends BuildTargetModifier {
  def merge(that: Build): Build = (this,that) match {
    case (BuildSome(a,b),BuildSome(c,d)) => BuildSome(a||c, b||d)
    case _ => BuildAll
  }
}

/** build all files */
case object BuildAll extends Build {
  def toString(dim: String) : String = dim
}
/** build certain files:
  * * changed files: always
  * * files that depended on files that have changed: if the corresponding flag is set
  * * files that had errors: if the corresponding flag is true
  */
case class BuildSome(dependsOnChange: Boolean, hadErrors: Boolean) extends Build {
  /** letter C or E occurs if correspondng flag is set */
  def key: String = (if (dependsOnChange) "C" else "") + (if (hadErrors) "E" else "")
  def toString(dim: String) : String = dim + "*" + key
}
object BuildChanged {
  def apply() = BuildSome(false,false)
}

/**
  * parsing method for build target modifiers
  */
object BuildTargetModifier {
  /**
    * parses m.toString(d) into (d,m)
    */
  def parse(dm: String): (String,BuildTargetModifier) = {
    if (dm.startsWith("-"))
      (dm.tail, Clean)
    else {
      val i = dm.indexOf("*")
      if (i == -1)
        (dm,BuildAll)
      else {
        val d = dm.take(i)
        val mods = d.drop(i+1)
        val w = BuildSome(mods.contains('C'), mods.contains('E'))
        (d, w)
      }
    }
  }
}

/** auxiliary type to represent the parameters and result of building a file/directory
  *
  * this is no case class due to a state-dependent error continuation
  *
  * @param inFile    the input file
  * @param inPath    the path of the input file inside the archive, relative to the input dimension
  * @param children  the build tasks of the children if this task refers to a directory
  * @param outFile   the intended output file
  * @param errorCont BuildTargets should report errors here (instead of directly writing to errFile)
  */
class BuildTask(val key: String, val archive: Archive, val inFile: File, val children: Option[List[BuildTask]],
                val inPath: FilePath, val outFile: File, val errorCont: OpenCloseHandler) extends MMTTask {
  /** build targets should set this to true if they skipped the file so that it is not passed on to the parent directory */
  var skipped = false
  /** the narration-base of the containing archive */
  val base : URI = archive.narrationBase

  /** the MPath corresponding to the inFile if inFile is a file in a content-structured dimension */
  def contentMPath: MPath = Archive.ContentPathToMMTPath(inPath)

  /** the DPath corresponding to the inFile if inFile is a folder in a content-structured dimension */
  def contentDPath: DPath = Archive.ContentPathToDPath(inPath)

  /** (possibly shorter) output file name to be shown in user messages */
  def outPath: FilePath = outFile.toFilePath

  /** the DPath corresponding to the inFile if inFile is in a narration-structured dimension */
  def narrationDPath: DPath = DPath(base / inPath.segments)

  def isDir      : Boolean = children.isDefined
  def isEmptyDir : Boolean = children.isDefined && children.get.isEmpty

  /** the name of the folder if inFile is a folder */
  def dirName: String = outFile.toFilePath.dirPath.name

  def asDependency: BuildDependency = children match {
    case Some(ch) => DirBuildDependency(key, archive, inPath, ch)
    case None => FileBuildDependency(key, archive, inPath)
  }
}

/** This abstract class provides common functionality for [[BuildTarget]]s that traverse all files in the input dimension.
  *
  * It implements BuildTarget in terms of the abstract method buildFile called to build a file in the archive.
  * It is also possible to override the method buildDir to post process directory content.
  */
abstract class TraversingBuildTarget extends BuildTarget {

  // ***************** abstract or overridable methods for configuring basic properties such as file extensions

  def onBlock(bt: QueuedTask,br : BuildResult): QueuedTask = bt

  /** the input dimension/archive folder */
  def inDim: ArchiveDimension

  /** the output archive folder */
  def outDim: ArchiveDimension

  /** the name that is used for the special file representing the containing folder (without extension), empty by default */
  protected val folderName = ""

  /** the file extension used for generated files, defaults to outDim, override as needed */
  def outExt: String = outDim match {
    case Dim(path@_*) => path.last
    case d => d.toString
  }

  /** there is no inExt, instead we test to check which files should be used;
    * this is often a test for the file extension
    *
    * This must be such that all auxiliary files are skipped.
    * see defaultFileExtension if you need an inExt (for meta targets)
    *
    * @param name the name of the file (no path, with extension)
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

  /** true by default; override to skip auxiliary directories
    *
    * @param name the name of the directory (no path)
    */
  def includeDir(name: String): Boolean = true

  /** if true, multiple files/folders are built in parallel */
  def parallel: Boolean = false


  // ***************** the essential abstract or overridable methods for building

  /** estimate the [[BuildResult]] without building, e.g., to predict dependencies */
  def estimateResult(bf: BuildTask): BuildSuccess = BuildResult.empty

  /** the main abstract method for building one file
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
  def buildDir(bd: BuildTask,builtChildren: List[BuildTask]): BuildResult = BuildEmpty("nothing to be done")

  /// ***************** auxiliary methods for computing paths to output/error files etc.

  protected def getOutFile(a: Archive,inPath: FilePath): File = (a / outDim / inPath).setExtension(outExt)

  protected def getFolderOutFile(a: Archive,inPath: FilePath): File = getOutFile(a,inPath / folderName)

  protected def getErrorFile(a: Archive,inPath: FilePath): File = (a / errors / key / inPath).setExtension("err")

  // This should be protected but also gets called from ErrorManager
  def getFolderErrorFile(a: Archive,inPath: FilePath): File = a / errors / key / inPath / (folderName + ".err")

  protected def getOutPath(a: Archive,outFile: File): FilePath = outFile.toFilePath

  /** auxiliary method for logging results */
  protected def logResult(s: String): Unit = {
    log(s,Some("result"))
  }

  // ***************** building (i.e., create build tasks and add them to build manager

  /** entry point for recursive building */
  def build(a: Archive,w: Build,in: FilePath,errorCont: Option[ErrorHandler]): Unit = {
    val qts = makeBuildTasks(a,in,errorCont)
    controller.buildManager.addTasks(w,qts)
  }

  /** like build, but returns all build tasks without adding them to the build manager */
  private def makeBuildTasks(a: Archive,in: FilePath,errorCont: Option[ErrorHandler]): List[QueuedTask] = {
    // TODO it would be much cleaner if QueuedTask were not used in the BuildTarget class
    var tasks: List[QueuedTask] = Nil
    makeBuildTasksAux(in,a,errorCont) {qt =>
      tasks ::= qt
    }
    tasks.reverse
  }

  /** recursive creation of [[BuildTask]]s */
  private def makeBuildTasksAux(in: FilePath,a: Archive,eCOpt: Option[ErrorHandler])(cont: QueuedTask => Unit): Unit = {
    //build every file
    a.traverse[BuildTask](inDim,in,TraverseMode(includeFile,includeDir,parallel))({
      case Current(inFile,inPath) =>
        val bf = makeBuildTask(a,inPath,inFile,None,eCOpt)
        val estRes = estimateResult(bf)
        val qt = new QueuedTask(this,estRes,bf)
        cont(qt)
        bf
    }, {
      case (Current(inDir,inPath),builtChildren) =>
        val realChildren = builtChildren.filter(!_.isEmptyDir)
        val bd = makeBuildTask(a,inPath,inDir,Some(realChildren),None)
        val estRes = estimateResult(bd)
        val qt = new QueuedTask(this,estRes,bd)
        cont(qt)
        bd
    })
  }

  /** create a single [[BuildTask]]
    *
    * @param eCOpt optional additional [[ErrorHandler]], errors are always written to errors dimension
    */
  private def makeBuildTask(a: Archive,inPath: FilePath,inFile: File,
                            children: Option[List[BuildTask]],eCOpt: Option[ErrorHandler]): BuildTask = {
    val ew = makeHandler(a,inPath,children.isDefined)
    val errorCont = MultipleErrorHandler(ew :: eCOpt.toList, report)
    val outFile = if (children.isDefined) getFolderOutFile(a,inPath) else getOutFile(a,inPath)
    new BuildTask(key,a,inFile,children,inPath,outFile,errorCont)
  }

  /** makes a build task for a single file (ignoring built children) or directory */
  // TODO: public because it is called by BuildQueue on dependencies; clean that up
  def makeBuildTask(a: Archive,inPath: FilePath,children: List[BuildTask] = Nil): BuildTask = {
    val inFile = a / inDim / inPath
    val isDir = inFile.isDirectory
    makeBuildTask(a,inPath,inFile,if (isDir) Some(children) else None,None)
  }

  /** auxiliary function to create an error handler */
  private def makeHandler(a: Archive,inPath: FilePath, isDir: Boolean = false) = {
    val errFileName = if (isDir) getFolderErrorFile(a,inPath)
    else getErrorFile(a,inPath)
    new ErrorWriter(errFileName)
  }

  // ******************* Actual building (i.e., when the build manager calls a build task)

  // TODO the methods in this section should be revised together with a revision of the BuildQueue

  /** the entry point for build managers: runs a build task unless (depending on the modifier) nothing has changed */
  def runBuildTaskIfNeeded(deps: Set[Dependency],bt: BuildTask,w: Build): BuildResult = {
    var res: BuildResult = BuildEmpty("up-to-date")
    val outPath = bt.outPath
    val rn = rebuildNeeded(deps,bt,w)
    if (!rn) {
      logResult("up-to-date " + outPath)
    } else {
      res = runBuildTask(bt)
    }
    res
  }

  /** auxiliary method of runBuildTaskIfNeeded: implements the semantics of Update to determine whether a task has to be built */
  // TODO specify the semantics of Update
  private def rebuildNeeded(deps: Set[Dependency],bt: BuildTask,w: Build): Boolean = {
    val errorFile: File = bt.asDependency.getErrorFile(controller)
    lazy val modded: Boolean = modified(bt.inFile,errorFile)
    lazy val errors: Boolean = hadErrors(errorFile)

    lazy val isDir: Boolean = bt.isDir && bt.children.getOrElse(Nil).exists {bf =>
      modified(bf.asDependency.getErrorFile(controller),errorFile)
    }

    w match {
      case BuildAll => true
      case BuildSome(ifDep,ifErr) =>
        modded || (ifErr && errors)
    }
  }

  /** wraps around buildFile and buildDir (which do the actual building) to add error handling, logging, etc. */
  // TODO should be private, exposed only because it is overridden by LaTeXDirTarget
  protected def runBuildTask(bt: BuildTask): BuildResult = {
    if (!bt.isDir) {
      val prefix = "[" + inDim + " -> " + outDim + "] "
      report("archive",prefix + bt.inFile + " -> " + bt.outFile)
      bt.outFile.up.mkdirs
    }
    var res: BuildResult = if (bt.isEmptyDir) BuildEmpty("empty-directory") else BuildResult.empty
    if (!bt.isEmptyDir) bt.errorCont.open
    try {
      res = bt.children match {
        case None =>
          // remove the old document from memory (if any) and build the file
          controller.delete(bt.narrationDPath)
          buildFile(bt)
        case Some(children@_ :: _) =>
          // build a directory
          buildDir(bt,children)
        case _ => res
      }
    } catch {
      case e: Error =>
        bt.errorCont(e)
        res = BuildFailure(Nil,Nil)
      case e: Exception =>
        val le = LocalError("unknown build error: " + e.getMessage).setCausedBy(e)
        bt.errorCont(le)
        res = BuildFailure(Nil,Nil)
    } finally {
      if (!bt.isEmptyDir) bt.errorCont.close
    }
    controller.notifyListeners.onFileBuilt(bt.archive,this,bt.inPath,res)
    res
  }

  // ********************* functions for delete, update, change management etc.

  /** additional method that implementations may provide: cleans one file
    *
    * deletes the output and error file by default, may be overridden to, e.g., delete auxiliary files
    *
    * @param a    the containing archive
    * @param curr the inDim whose output is to be deleted
    */
  def cleanFile(a: Archive,curr: Current): Unit = {
    val inPath = curr.path
    val outFile = getOutFile(a,inPath)
    delete(outFile)
    delete(getErrorFile(a,inPath))
    //controller.notifyListeners.onFileBuilt(a, this, inPath)
  }

  /** additional method that implementations may provide: cleans one directory
    *
    * does nothing by default
    *
    * @param a    the containing archive
    * @param curr the outDim directory to be deleted
    */
  def cleanDir(a: Archive,curr: Current): Unit = {
    val inPath = curr.path
    val errFile = getFolderErrorFile(a,inPath)
    delete(errFile)
    val errDir = errFile.up
    if (errDir.isDirectory) errDir.deleteDir
    //controller.notifyListeners.onFileBuilt(a, this, inPath)
  }

  /** recursively delete output files in parallel (!) */
  def clean(a: Archive,in: FilePath = EmptyPath): Unit = {
    a.traverse[Unit](inDim,in,TraverseMode(includeFile,includeDir,parallel = true),sendLog = true,forClean = true)(
      {c => cleanFile(a,c)}, {case (c,_) => cleanDir(a,c)})
  }

  /** checks if a file has been modified since the last built (using the date of the error file as the build time) */
  private def modified(inFile: File,errorFile: File): Boolean = {
    val mod = Modification(inFile,errorFile)
    mod == Modified || mod == Added
  }

  /** @return status of input file, obtained by comparing to error file */
  private def hadErrors(errorFile: File): Boolean =
    errorFile.exists && ErrorReader.getBuildErrors(errorFile,Level.Error,None).nonEmpty
}

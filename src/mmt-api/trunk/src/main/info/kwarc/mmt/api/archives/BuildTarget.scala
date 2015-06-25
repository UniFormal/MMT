package info.kwarc.mmt.api.archives

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.frontend._
import info.kwarc.mmt.api.utils.File

sealed abstract class BuildTargetModifier {
  def toString(dim: String): String
}

case object Clean extends BuildTargetModifier {
  def toString(dim: String) = "-" + dim
}

case class Update(ifChanged: Boolean, ifHadErrors: Boolean) extends BuildTargetModifier {
  def key = (if (ifChanged) "*" else "") + (if (ifHadErrors) "!" else "")

  def toString(dim: String) = dim + key
}

case object Build extends BuildTargetModifier {
  def toString(dim: String) = dim
}

/** A BuildTarget provides build/update/clean methods that generate one or more dimensions in an [[Archive]]
  * from an input dimension.
  */
abstract class BuildTarget extends FormatBasedExtension {
  /** a string identifying this build target, used for parsing commands, logging, error messages
    */
  def key: String

  def isApplicable(format: String) = format == key

  /** defaults to the key */
  override def logPrefix = key

  /** number of required arguments, defaults to 0, override as needed */
  def requiredArguments(m: BuildTargetModifier): Int = 0

  /** build this target in a given archive */
  def build(a: Archive, args: List[String], in: List[String])

  /** update this target in a given archive */
  def update(a: Archive, args: List[String], up: Update, in: List[String])

  /** clean this target in a given archive */
  def clean(a: Archive, args: List[String], in: List[String])

  /** the main function to run the build target
    *
    * @param modifier chooses build, clean, or update
    * @param arch the archive to build on
    * @param in the folder inside the archive's inDim folder to which building in restricted (i.e., Nil for whole archive)
    * @param args arguments for the discretion of the BuildTarget; number must be equal to requiredArguments(modifier)
    */
  def apply(modifier: BuildTargetModifier, arch: Archive, in: List[String], args: List[String]) {
    val reqArgs = requiredArguments(modifier)
    if (reqArgs != args.length)
      throw ParseError("wrong number of arguments, required: " + reqArgs)
    modifier match {
      case up: Update => update(arch, args, up, in)
      case Clean => clean(arch, args, in)
      case Build => build(arch, args, in)
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

/**
 * auxiliary type to represent the parameters and result of building a file/directory
 *
 * @param inFile the input file
 * @param inPath the path of the input file inside the archive, relative to the input dimension
 * @param outFile the intended output file
 * @param errorCont BuildTargets should report errors here
 */
class BuildTask(val archive: Archive, val inFile: File, val isDir: Boolean, val inPath: List[String],
                val outFile: File, val errorCont: ErrorHandler) {
  /** build targets should set this to true if they skipped the file so that it is not passed on to the parent directory */
  var skipped = false
  /** the narration-base of the containing archive */
  val base = archive.narrationBase

  /** the MPath corresponding to the inFile if inFile is a file in a content-structured dimension */
  def contentMPath = Archive.ContentPathToMMTPath(inPath)

  /** the DPath corresponding to the inFile if inFile is a folder in a content-structured dimension */
  def contentDPath = Archive.ContentPathToDPath(inPath)

  /** the DPath corresponding to the inFile if inFile is in a narration-structured dimension */
  def narrationDPath = DPath(base / inPath)

  /** the name of the folder if inFile is a folder */
  def dirName: String = outFile.segments.init.last
}

/**
 * a path in an [[Archive]], relative to an [[ArchiveDimension]], used when traversing
 */
case class ArchivePath(segments: List[String]) {
  def toFile = File(toString)

  def getExtension = toFile.getExtension

  def setExtension(e: String) = ArchivePath(toFile.setExtension(e).segments)

  def removeExtension = ArchivePath(toFile.removeExtension.segments)

  override def toString = segments.mkString("/")
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

  protected def getOutFile(a: Archive, inPath: List[String]) = (a / outDim / inPath).setExtension(outExt)

  protected def getFolderOutFile(a: Archive, inPath: List[String]) = a / outDim / inPath / (folderName + "." + outExt)

  protected def getErrorFile(a: Archive, inPath: List[String]) = (a / errors / key / inPath).addExtension("err")

  protected def getFolderErrorFile(a: Archive, inPath: List[String]) = a / errors / key / inPath / (folderName + ".err")

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
  def buildFile(bf: BuildTask)

  /** similar to buildOne but called on every directory (after all its children have been processed)
    * @param bd information about input/output file etc
    * @param builtChildren results from building the children
    *                      This does nothing by default and can be overridden if needed.
    */
  def buildDir(bd: BuildTask, builtChildren: List[BuildTask]) {}

  /** entry point for recursive building */
  def build(a: Archive, args: List[String], in: List[String] = Nil) = build(a, args, in, None)

  def build(a: Archive, args: List[String], in: List[String], errorCont: Option[ErrorHandler]) {
    buildAux(in)(a, errorCont)
  }

  private def makeHandler(a: Archive, inPath: List[String], isDir: Boolean = false) = {
    val errFileName = if (isDir) getFolderErrorFile(a, inPath)
    else getErrorFile(a, inPath)
    new ErrorWriter(errFileName, Some(report))
  }

  /** recursive building */
  private def buildAux(in: List[String] = Nil)(implicit a: Archive, eCOpt: Option[ErrorHandler]) {
    //build every file
    val prefix = "[" + inDim + " -> " + outDim + "] "
    a.traversing[BuildTask](inDim, in, TraverseMode(includeFile, includeDir, parallel))({
      case Current(inFile, inPath) =>
        if (!inFile.isFile)
          throw LocalError("file does not exist: " + inPath)
        val outFile = getOutFile(a, inPath)
        report("archive", prefix + inFile + " -> " + outFile)
        var errorWriter = makeHandler(a, inPath)
        val errorCont = eCOpt match {
          case None => errorWriter
          case Some(eC) => new MultipleErrorHandler(List(eC, errorWriter))
        }
        val bf = new BuildTask(a, inFile, false, inPath, outFile, errorCont)
        outFile.up.mkdirs
        try {
          buildFile(bf)
        } catch {
          case e: Error => errorCont(e)
          case e: Exception =>
            val le = LocalError("unknown build error: " + e.getMessage).setCausedBy(e)
            errorCont(le)
        } finally {
          errorWriter.close
        }
        controller.notifyListeners.onFileBuilt(a, this, inPath)
        // a.timestamps(this).set(inPath) not needed anymore
        bf
    }, {
      case (Current(inDir, inPath), builtChildren) =>
        val outFile = getFolderOutFile(a, inPath)
        val errorCont = makeHandler(a, inPath, isDir = true)
        val bd = new BuildTask(a, inDir, true, inPath, outFile, errorCont)
        buildDir(bd, builtChildren)
        errorCont.close
        bd
    })
  }

  /** additional method that implementations may provide: cleans one file
    * @param a the containing archive
    * @param curr the inDim whose output is to be deleted
    *             deletes the output and error file by default, may be overridden to, e.g., delete auxiliary files
    */
  def cleanFile(a: Archive, curr: Current) {
    val outFile = getOutFile(a, curr.path)
    delete(outFile)
    delete(getErrorFile(a, curr.path))
    controller.notifyListeners.onFileBuilt(a, this, curr.path)
  }

  /** additional method that implementations may provide: cleans one directory
    * @param a the containing archive
    * @param curr the outDim directory to be deleted
    *             does nothing by default
    */
  def cleanDir(a: Archive, curr: Current) {}

  /** recursively delete output files in parallel (!) */
  def clean(a: Archive, args: List[String], in: List[String] = Nil) {
    a.traversing[Unit](outDim, in, TraverseMode(Archive.extensionIs(outExt), includeDir, parallel = true))(
    { c => cleanFile(a, c) }, { case (c, _) => cleanDir(a, c) })
  }

  /** @return status of input file, obtained by comparing to error file */
  private def modified(a: Archive, path: List[String]): (Modification, Boolean) = {
    val errorFile = getErrorFile(a, path)
    val inFile = a / inDim / path
    val mod = Modification(inFile, errorFile)
    val hadErrors = errorFile.exists && errorFile.length > 25 // TODO evil hack but more efficient than reading the error file
    (mod, hadErrors)
  }

  /** recursively reruns build if the input file has changed
    *
    * the decision is made based on the time stamps and the system's last-modified date
    */
  def update(a: Archive, args: List[String], up: Update, in: List[String] = Nil) {
    a.traversing[Boolean](inDim, in, TraverseMode(includeFile, includeDir, parallel))({
      case c@Current(inFile, inPath) =>
        modified(a, inPath) match {
          case (Deleted, _) =>
            cleanFile(a, c)
            true
          case (Added, _) =>
            buildAux(inPath)(a, None)
            true
          case (Modified, hadErrors) =>
            if (up.ifChanged || (hadErrors && up.ifHadErrors)) {
              cleanFile(a, c)
              buildAux(inPath)(a, None)
            }
            false
          case (Unmodified, hadErrors) =>
            if (hadErrors && up.ifHadErrors) {
              cleanFile(a, c)
              buildAux(inPath)(a, None)
            }
            false
        }
    }, { case (c@Current(inDir, inPath), childChanged) =>
      if (childChanged.contains(true)) {
        val outFile = getFolderOutFile(a, inPath)
        val errorCont = makeHandler(a, inPath, isDir = true)
        val bd = new BuildTask(a, inDir, true, inPath, outFile, errorCont)
        errorCont.close
        buildDir(bd, Nil) // TODO pass proper builtChildren
        false
      } else
        false
    })
  }
}

/**
 * a build target that chains multiple other targets
 */
class MetaBuildTarget extends BuildTarget {
  private var _key = ""
  private var targets: List[BuildTarget] = Nil

  def key = _key

  /**
   * first argument: the key of this build target
   * remaining arguments: the build targets to chain
   */
  override def start(args: List[String]) {
    _key = args.headOption.getOrElse {
      throw LocalError("at least one argument required")
    }
    targets = args.tail.map { k => controller.extman.get(classOf[BuildTarget], k).getOrElse {
      throw LocalError("unknown target: " + k)
    }
    }
  }

  /** @return the arguments to pass to the target with key k, override as needed */
  def arguments(k: String): List[String] = Nil

  /** @return the path to pass to the target t, override as needed */
  def path(t: BuildTarget, in: List[String]) = t match {
    case t: TraversingBuildTarget if t.inDim != content => in
    case _ => Nil
  }

  def build(a: Archive, args: List[String], in: List[String]) {
    targets.foreach { t => t.build(a, arguments(t.key), path(t, in)) }
  }

  def update(a: Archive, args: List[String], up: Update, in: List[String]) {
    targets.foreach { t => t.update(a, arguments(t.key), up, path(t, in)) }
  }

  def clean(a: Archive, args: List[String], in: List[String]) {
    targets.foreach { t => t.clean(a, arguments(t.key), path(t, in)) }
  }
}

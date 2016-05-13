package info.kwarc.mmt.stex

/**
  * Created by maeder on 11.11.15.
  */

import java.util.regex.PatternSyntaxException

import info.kwarc.mmt.api.Level._
import info.kwarc.mmt.api.archives._
import info.kwarc.mmt.api.utils.AnaArgs._
import info.kwarc.mmt.api.utils.{EmptyPath, File, FilePath, _}
import info.kwarc.mmt.api.{ExtensionError, Level}
import info.kwarc.mmt.stex.STeXUtils._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._
import scala.concurrent.duration._
import scala.sys.process.{ProcessBuilder, ProcessLogger}

/** common code for sms, latexml und pdf generation */
abstract class LaTeXBuildTarget extends TraversingBuildTarget with STeXAnalysis with BuildTargetArguments {
  val localpathsFile = "localpaths.tex"
  val inDim = source
  var pipeOutput: Boolean = false
  val pipeOutputOption: String = "pipe-worker-output"
  /** timout in seconds */
  private val timeoutDefault: Int = 300
  protected var timeoutVal: Int = timeoutDefault
  protected val timeoutOption: String = "timeout"
  protected var nameOfExecutable: String = ""

  protected case class LatexError(s: String, l: String) extends ExtensionError(key, s) {
    override val extraMessage = l
  }

  protected def commonOpts: OptionDescrs = List(
    OptionDescr(pipeOutputOption, "p", NoArg, "echo output of executables to console"),
    OptionDescr(timeoutOption, "", IntArg, "timeout in seconds for executables"),
    OptionDescr(key, "", StringArg, "name of executable for " + key),
    OptionDescr("execute", "", StringArg, "name of main executable")
  )

  override def buildOpts: OptionDescrs = commonOpts

  override def start(args: List[String]) {
    anaStartArgs(args)
    pipeOutput = optionsMap.get(pipeOutputOption).isDefined
    optionsMap.get(timeoutOption).foreach { case v => timeoutVal = v.getIntVal }
    optionsMap.get(key).foreach { case v => nameOfExecutable = v.getStringVal }
    optionsMap.get("execute").foreach { case v =>
      if (nameOfExecutable.isEmpty) nameOfExecutable = v.getStringVal
      else logError("executable already set by: --" + key + "=" + nameOfExecutable)
    }
  }

  protected def procLogger(output: StringBuffer, pipeOutput: Boolean): ProcessLogger = {
    def handleLine(line: String) {
      if (pipeOutput) println(line)
      output.append(line + "\n")
    }
    ProcessLogger(handleLine, handleLine)
  }

  def includeFile(n: String): Boolean =
    n.endsWith(".tex") && !n.endsWith(localpathsFile) && !n.startsWith("all.")

  protected def createLocalPaths(bt: BuildTask) {
    createLocalPaths(bt.archive, bt.inFile.up)
  }

  protected def createLocalPaths(a: Archive, dir: File) {
    val fileName = dir / localpathsFile
    val groupRepo = archString(a) + "}"
    val text: List[String] = List(
      "% this file defines root path local repository",
      "\\defpath{MathHub}{" + a.root.up.up.getPath + "}",
      "\\mhcurrentrepos{" + groupRepo,
      "\\libinput{WApersons}",
      "% we also set the base URI for the LaTeXML transformation",
      "\\baseURI[\\MathHub{}]{https://mathhub.info/" + groupRepo
    )
    if (!fileName.exists()) {
      File.WriteLineWise(fileName, text)
      log("created file " + fileName)
    }
  }

  protected def skip(bt: BuildTask): Boolean = {
    val optExcludes: Option[String] = bt.archive.properties.get("no-" + outExt)
    val optIncludes: Option[String] = bt.archive.properties.get("build-" + outExt)
    val excludes: List[String] = optExcludes.map(_.split(" ").toList).getOrElse(Nil)
    val includes: List[String] = optIncludes.map(_.split(" ").toList).getOrElse(Nil)
    def patternMatch(pat: String): Boolean =
      try {
        bt.inPath.toString.matches(pat)
      } catch {
        case e: PatternSyntaxException =>
          logResult(e.getMessage)
          logResult("correct build/no-" + outExt + " property in " +
            bt.archive.root.getName + "/META-INF/MANIFEST.MF")
          true // skip everything until corrected
      }
    val exclude = excludes.exists(patternMatch)
    val include = includes.exists(patternMatch)
    val noDoc = exclude || includes.nonEmpty && !include
    if (noDoc) logResult("skipping " + bt.outPath)
    noDoc
  }

  /** to be implemented */
  def reallyBuildFile(bt: BuildTask): BuildResult

  def buildFile(bt: BuildTask): BuildResult = if (!skip(bt)) reallyBuildFile(bt)
  else BuildEmpty("file excluded by MANIFEST")

  protected def readingSource(a: Archive, in: File): List[Dependency] = {
    val source = readSourceRebust(in)
    val STeXStructure(_, res) = mkSTeXStructure(a, in, source.getLines)
    log(in + ": " + res.mkString(", "))
    val outPath = getOutPath(a, in)
    val safe = res.filter {
      case FileBuildDependency(_, ar, fp) =>
        val f: File = ar / inDim / fp
        if (f == in) {
          log(outPath + " imports itself")
          false
        }
        else if (f.exists()) true
        else {
          log(outPath + " missing: " + f)
          // log messages oddly appear in different order for latexml and pdflatex
          false
        }
      case _ => true
    }
    safe
  }

  override def estimateResult(bt: BuildTask) = {
    val in = bt.inFile
    val ds = if (in.exists && in.isFile) {
      readingSource(bt.archive, in)
    } else if (in.isDirectory) Nil
    else {
      logResult("unknown file: " + in)
      logResult(" for: " + key)
      Nil
    }
    BuildSuccess(ds.distinct, Nil) //TODO add estimate of provided resources
  }

  /** run process with logger synchronously within the given timeout
    *
    * @return exit code
    */
  protected def timeout(pb: ProcessBuilder, log: ProcessLogger): Int = {
    val proc = pb.run(log)
    val fut = Future(blocking(proc.exitValue()))
    try {
      Await.result(fut, timeoutVal.seconds)
    } catch {
      case e: TimeoutException =>
        proc.destroy()
        throw e
    }
  }

  protected def getDirFiles(a: Archive, dir: File, includeFile: String => Boolean): List[String] =
    if (dir.isDirectory && includeDir(dir.getName) && a.includeDir(dir.getName))
      dir.list.filter(f => includeFile(f) && (dir / f).isFile).toList.sorted
    else Nil

  protected def getDirFilesByExt(a: Archive, dir: File, exts: List[String]): List[File] =
    getDirFiles(a, dir, f => exts.exists(e => f.endsWith("." + e))).map(f => dir / f)

  protected def deleteWithLog(f: File) {
    f.delete()
    logResult("deleted " + f)
  }

  override def cleanDir(a: Archive, curr: Current) {
    super.cleanDir(a, curr)
    val outFile = getFolderOutFile(a, curr.path)
    val outDir = outFile.up
    outFile.getExtension.foreach(ext => getDirFilesByExt(a, outDir, List(ext)).foreach(deleteWithLog))
  }
}

abstract class LaTeXDirTarget extends LaTeXBuildTarget {
  val outDim: ArchiveDimension = source
  override val outExt = "tex"

  override def getFolderOutFile(a: Archive, inPath: FilePath) = a / outDim / inPath

  // we do nothing for single files
  def reallyBuildFile(bt: BuildTask): BuildResult = BuildEmpty("nothing to do for files")

  protected def allFile(lang: Option[String]): String =
    "all" + lang.map("." + _).getOrElse("") + ".tex"

  protected def getAllFiles(bt: BuildTask): List[String] =
    if (bt.isDir) {
      val files = getDirFiles(bt.archive, bt.inFile, includeFile)
      val langs: Set[String] = files.flatMap(f => getLang(File(f))).toSet
      val allLangFiles = langs.toList.sorted.map(l => allFile(Some(l)))
      if (langFiles(None, files).isEmpty) allLangFiles
      else
        allFile(None) :: allLangFiles
    } else Nil

  override def runBuildTask(bt: BuildTask, level: Level): BuildResult = if (bt.isDir) {
    super.runBuildTask(bt, level)
  } else reallyBuildFile(bt)

  override def cleanFile(a: Archive, curr: Current) {
    // these error files are no longer generated, though
    delete(getErrorFile(a, curr.path))
  }

  // files to be cleaned
  def dirFileFilter(f: String): Boolean =
    f.startsWith("all.") && f.endsWith(".tex")

  override def buildDepsFirst(a: Archive, up: Update, in: FilePath = EmptyPath) {
    a.traverse[Unit](inDim, in, TraverseMode(includeFile, includeDir, parallel))({
      case _ =>
    }, {
      case (c@Current(inDir, inPath), _) =>
        buildDir(a, inPath, inDir, force = false)
    })
  }

  override def buildDir(bt: BuildTask, builtChildren: List[BuildTask], level: Level): BuildResult =
    buildDir(bt.archive, bt.inPath, bt.inFile, force = level <= Level.Force)

  def buildDir(a: Archive, in: FilePath, dir: File, force: Boolean): BuildResult
}

package info.kwarc.mmt.stex

/**
  * Created by maeder on 11.11.15.
  */

import java.util.regex.PatternSyntaxException

import STeXUtils._
import info.kwarc.mmt.api.ExtensionError
import info.kwarc.mmt.api.archives._
import info.kwarc.mmt.api.frontend._
import info.kwarc.mmt.api.utils.File

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._
import scala.concurrent.duration._
import scala.sys.process.{ProcessBuilder, ProcessLogger}

/** common code for sms, latexml und pdf generation */
abstract class LaTeXBuildTarget extends TraversingBuildTarget with STeXAnalysis {
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

  private val commonOpts: OptionDescrs = List(
    OptionDescr(pipeOutputOption, "", NoArg, "echo output of executables to console"),
    OptionDescr(timeoutOption, "", IntArg, "timeout in seconds for executables"),
    OptionDescr(key, "", StringArg, "name of executable for " + key),
    OptionDescr("execute", "", StringArg, "name of main executable")
  )

  override def start(args: List[String]) {
    super.start(args)
    val (m, rest) = anaArgs(commonOpts, remainingStartArguments)
    remainingStartArguments = rest
    pipeOutput = m.get(pipeOutputOption).isDefined
    m.get(timeoutOption).foreach { case v => timeoutVal = v.getIntVal }
    m.get(key).foreach { case v => nameOfExecutable = v.getStringVal }
    m.get("execute").foreach { case v =>
      if (nameOfExecutable.isEmpty) nameOfExecutable = v.getStringVal
      else logError("executable already set by: --" + key + "=" + nameOfExecutable) }
  }

  protected def execArgs(args: List[String]): (List[String], List[String]) = {
    val (matched, rest) = partArg(key, args)
    val (opts, nonOpts) = rest.filter(pipeOutputOption != _).partition(_.startsWith("--"))
    val (timeOpts, restOpts) = partArg(timeoutOption, opts)
    timeOpts match {
      case Nil =>
      case hd :: tl => try timeoutVal = hd.toInt catch {
        case _: Exception =>
          logResult("illegal timeout value: " + hd)
      }
        tl.foreach(t => logResult("ignored time value: " + t))
    }
    (restOpts, matched ++ nonOpts)
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

  override def includeDir(n: String): Boolean = !n.endsWith("tikz")

  protected def createLocalPaths(bt: BuildTask) {
    createLocalPaths(bt.archive, bt.inFile.up)
  }

  protected def createLocalPaths(a: Archive, dir: File) {
    val fileName = dir / localpathsFile
    val groupRepo = archString(a) + "}"
    val text: List[String] = List(
      "% this file defines root path local repository",
      "\\defpath{MathHub}{" + a.baseDir.getPath + "}",
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
    if (noDoc) logResult("skipping " + getOutPath(bt.archive, bt.inFile))
    noDoc
  }

  /** to be implemented */
  def reallyBuildFile(bt: BuildTask): BuildResult

  def buildFile(bt: BuildTask): BuildResult = if (!skip(bt)) reallyBuildFile(bt)
  else BuildResult.empty


  protected def readingSource(key: String, a: Archive, in: File): Seq[Dependency] = {
    var res: List[Dependency] = Nil
    val source = readSourceRebust(in)
    source.getLines().foreach { line =>
      val l = stripComment(line).trim
      val err = "unexpected line: " + l + "\nin file: " + in
      val verbIndex = l.indexOf("\\verb")
      if (verbIndex <= -1 && importRegs.findFirstIn(l).isDefined) {
        matchPathAndRep(key, a, l) match {
          case None => log(err)
          case Some(p) => res ::= p
        }
      }
    }
    log(in + ": " + res.mkString(", "))
    val safe = res.filter {
      case BuildDependency(_, ar, fp) =>
        val f: File = ar / inDim / fp
        if (f == in) {
          log(LocalError(getOutPath(a, in) + " imports itself"))
          false
        }
        else if (f.exists()) true
        else {
          log(LocalError(getOutPath(a, in) + " missing: " + f))
          false
        }
      case _ => true
    }
    safe
  }

  override def getDeps(bt: BuildTask): Set[Dependency] = {
    val in = bt.inFile
    if (in.exists()) {
      if (key == "sms") Set.empty
      else
        readingSource(if (List("tikzsvg", "allpdf").contains(key)) "pdflatex" else key, bt.archive, in).toSet
    } else {
      logResult("unknown file: " + in)
      Set.empty
    }
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
    val errDir = getFolderErrorFile(a, curr.path).up
    val outFile = getFolderOutFile(a, curr.path)
    val outDir = outFile.up
    if (errDir.isDirectory) errDir.deleteDir
    outFile.getExtension.foreach(ext => getDirFilesByExt(a, outDir, List(ext)).foreach(deleteWithLog))
  }
}

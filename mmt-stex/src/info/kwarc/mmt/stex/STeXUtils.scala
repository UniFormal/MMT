package info.kwarc.mmt.stex

import java.util.regex.PatternSyntaxException

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.archives._
import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.utils._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._
import scala.concurrent.duration._
import scala.io.{BufferedSource, Codec}
import scala.sys.process._
import scala.util.matching.Regex

trait STeXUtils {
  protected val c = java.io.File.pathSeparator

  protected def partArg(arg: String, args: List[String]): (List[String], List[String]) = {
    val (matched, rest) = args.partition(_.startsWith("--" + arg + "="))
    (matched.map(_.substring(arg.length + 3)), rest)
  }

  protected def mathHubDir(bt: BuildTask): File = bt.archive.baseDir.up

  protected def extBase(bt: BuildTask): File = mathHubDir(bt) / "ext"

  protected def stexStyDir(bt: BuildTask): File = extBase(bt) / "sTeX" / "sty"

  protected def styPath(bt: BuildTask): File = mathHubDir(bt) / "sty"

  protected def sysEnv(v: String): String = sys.env.getOrElse(v, "")

  protected def env(bt: BuildTask): List[(String, String)] = {
    val sty = "STEXSTYDIR"
    val tex = "TEXINPUTS"
    val styEnv = sysEnv(sty)
    List(
      sty -> (if (styEnv.isEmpty) stexStyDir(bt).toString else styEnv),
      tex -> (".//" + c + styPath(bt) + c + stexStyDir(bt) + "//"
        + c + sysEnv(tex)))
  }

  protected def archString(a: Archive): String = a.groupDir.getName + "/" + a.root.getName

  protected def getLang(f: File): Option[String] = f.stripExtension.getExtension

  protected def getAmbleFile(preOrPost: String, bt: BuildTask): File = {
    getAmbleFile(preOrPost, bt.archive, getLang(bt.inFile))
  }

  protected def getAmbleFile(preOrPost: String, a: Archive, lang: Option[String]): File = {
    val repoDir = a.root
    val filePrefix = repoDir / "lib" / preOrPost
    val defaultFile = filePrefix.setExtension("tex")
    if (lang.isDefined) {
      val langFile = filePrefix.setExtension(lang.get + ".tex")
      if (langFile.exists)
        langFile
      else defaultFile
    }
    else defaultFile
  }

  protected def readSourceRebust(f: File): BufferedSource =
    scala.io.Source.fromFile(f)(Codec.ISO8859)

  protected def langFiles(lang: Option[String], files: List[String]): List[String] =
    files.filter(f => getLang(File(f)) == lang)

  def mkRegGroup(l: List[String]): String = l.mkString("(", "|", ")")

  private val importKeys: List[String] = List(
    "guse", "gimport", "usemhmodule", "importmhmodule"
  )
  protected val importRegs: Regex = ("^\\\\" + mkRegGroup(importKeys)).r
  private val groups: Regex = "\\\\\\w*\\*?(\\[(.*?)\\])?\\{(.*?)\\}.*".r

  protected def matchPathAndRep(aStr: String, line: String): Option[(String, FilePath)] =
    line match {
      case groups(_, a, b) =>
        val fp = File(b).setExtension("tex").filepath
        val optRepo = Option(a).map(_.split(",").toList.sorted.map(_.split("=").toList))
        optRepo match {
          case Some(List(List(id))) => Some((id, fp))
          case Some(List("path", p) :: tl) =>
            val path = File(p).setExtension("tex").filepath
            tl match {
              case List(List("repos", id)) => Some((id, path))
              case Nil => Some((aStr, path))
              case _ => None
            }
          case None => Some((aStr, fp))
          case _ => None
        }
      case _ => None
    }

  protected def noAmble(f: File): Boolean = {
    val source = readSourceRebust(f)
    var res = false
    for (l <- source.getLines(); if !res)
      if (l.trim.startsWith("\\documentclass")) res = true
    res
  }
}

/** common code for sms, latexml und pdf generation */
abstract class LaTeXBuildTarget extends TraversingBuildTarget with STeXUtils {
  val localpathsFile = "localpaths.tex"
  val inDim = source
  protected var pipeOutput: Boolean = false
  protected val pipeOutputOption: String = "--pipe-worker-output"
  /** timout in seconds */
  private val timeoutDefault: Int = 300
  protected var timeoutVal: Int = timeoutDefault
  protected val timeoutOption: String = "timeout"

  protected case class LatexError(s: String, l: String) extends ExtensionError(key, s) {
    override val extraMessage = l
  }

  protected def logSuccess(f: FilePath) = logResult("success " + f)

  protected def logFailure(f: FilePath) = logResult("failure " + f)

  override def defaultFileExtension: String = "tex"

  override def start(args: List[String]): Unit = {
    if (args.contains(pipeOutputOption)) pipeOutput = true
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

  protected def procLogger(output: StringBuffer): ProcessLogger = {
    def handleLine(line: String): Unit = {
      if (pipeOutput) println(line)
      output.append(line + "\n")
    }
    ProcessLogger(handleLine, handleLine)
  }

  def includeFile(n: String): Boolean =
    n.endsWith(".tex") && !n.endsWith(localpathsFile) && !n.startsWith("all.")

  override def includeDir(n: String): Boolean = !n.endsWith("tikz")

  protected def createLocalPaths(bt: BuildTask): Unit = {
    createLocalPaths(bt.archive, bt.inFile.up)
  }

  protected def createLocalPaths(a: Archive, dir: File): Unit = {
    val fileName = dir / localpathsFile
    val groupRepo = archString(a) + "}"
    val text: List[String] = List(
      "% this file defines root path local repository",
      "\\defpath{MathHub}{" + a.baseDir.getPath + "}",
      "\\mhcurrentrepos{" + groupRepo,
      "\\input{" + a.root.getPath + "/lib/WApersons}",
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
    val excludes: List[String] = optExcludes.map(_.split(" ").toList).getOrElse(Nil)
    def patternMatch(pat: String): Boolean =
      try {
        bt.inFile.getName.matches(pat)
      } catch {
        case e: PatternSyntaxException =>
          logResult(e.getMessage)
          logResult("correct no-" + outExt + " property in " +
            bt.archive.root.getName + "/META-INF/MANIFEST.MF")
          true // skip everything until corrected
      }
    val exclude = excludes.exists(patternMatch)
    if (exclude) logResult("skipping " + getOutPath(bt.archive, bt.inFile))
    exclude
  }

  /** to be implemented */
  def reallyBuildFile(bt: BuildTask)

  def buildFile(bt: BuildTask): Unit = if (!skip(bt)) reallyBuildFile(bt)

  protected def readingSource(a: Archive, in: File, init: List[(String, FilePath)] = Nil):
  Seq[(String, FilePath)] = {
    val aStr: String = archString(a)
    var res = init
    val source = readSourceRebust(in)
    source.getLines().foreach { line =>
      val idx = line.indexOf('%')
      val l = (if (idx > -1) line.substring(0, idx) else line).trim
      val err = "unexpected line: " + l + "\nin file: " + in
      val verbIndex = l.indexOf("\\verb")
      if (verbIndex <= -1 && importRegs.findFirstIn(l).isDefined) {
        matchPathAndRep(aStr, l) match {
          case None => log(err)
          case Some(p) => res ::= p
        }
      }
    }
    log(in + ": " + res.mkString(", "))
    val safe = res.filter { case ((ar, fp)) =>
      val ap = File(ar) / inDim.toString / fp.toString
      val f: File = a.baseDir / ap.toString
      if (f == in) {
        log(LocalError(getOutPath(a, in) + " imports itself"))
        false
      } else if (f.exists()) true
      else {
        log(LocalError(getOutPath(a, in) + " missing: " + ap))
        false
      }
    }
    safe
  }

  override def getSingleDeps(controller: Controller, a: Archive, fp: FilePath): Set[(Archive, FilePath)] = {
    val in = a / inDim / fp
    if (in.exists()) {
      val optLang = getLang(in)
      val aStr = archString(a)
      val name = in.getName
      val init = if (name.startsWith("all"))
        langFiles(optLang, getDirFiles(a, in.up)).filter(_ != name).map(f => (aStr, fp.dirPath / f))
      else optLang match {
        case None => Nil
        case Some(lang) =>
          List((aStr, fp.toFile.stripExtension.stripExtension.setExtension("tex").filepath))
      }
      val fs = readingSource(a, in, init)
      var res: Set[(Archive, FilePath)] = Set.empty
      fs foreach { case (ar, p) =>
        controller.getOrAddArchive(a.baseDir / ar) match {
          case None =>
          case Some(arch) =>
            res += ((arch, p))
        }
      }
      res
    } else {
      logResult("unknown file: " + in)
      Set.empty
    }
  }

  /** run process with logger synchronously within the given timeout
    *
    * @return exit code
    **/
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

  protected def getDirFiles(a: Archive, dir: File): List[String] =
    if (dir.isDirectory && includeDir(dir.getName) && a.includeDir(dir.getName))
      dir.list.filter(f => includeFile(f) && (dir / f).isFile).toList.sorted
    else Nil
}

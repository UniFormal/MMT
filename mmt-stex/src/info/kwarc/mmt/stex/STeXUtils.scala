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

  private def getLangAmbleFile(defaultFile: File, lang: Option[String]): File =
    if (lang.isDefined) {
      val langFile = defaultFile.stripExtension.setExtension(lang.get + ".tex")
      if (langFile.exists)
        langFile
      else defaultFile
    }
    else defaultFile

  protected def groupMetaInf(a: Archive): File = a.groupDir / "meta-inf"

  protected def getAmbleFile(preOrPost: String, a: Archive, lang: Option[String]): File = {
    def ambleFile(root: File): File = (root / "lib" / preOrPost).setExtension("tex")
    val repoFile = getLangAmbleFile(ambleFile(a.root), lang)
    if (repoFile.exists())
      repoFile
    else getLangAmbleFile(ambleFile(groupMetaInf(a)), lang)
  }

  protected def readSourceRebust(f: File): BufferedSource =
    scala.io.Source.fromFile(f)(Codec.ISO8859)

  protected def stripComment(line: String): String = {
    val idx = line.indexOf('%')
    idx match {
      case -1 => line
      case 0 => ""
      case _ =>
        val l = line.substring(0, idx)
        if (l.charAt(idx - 1) == '\\')
          l + "%" + stripComment(line.substring(idx + 1))
        else l
    }
  }

  protected def langFiles(lang: Option[String], files: List[String]): List[String] =
    files.filter(f => getLang(File(f)) == lang)

  def mkRegGroup(l: List[String]): String = l.mkString("(", "|", ")")

  private val importKeys: List[String] = List(
    "guse", "gimport", "usemhmodule", "importmhmodule", "begin\\{modnl\\}",
    "mhinputref", "mhtikzinput", "cmhtikzinput", "tikzinput", "ctikzinput"
  )
  protected val importRegs: Regex = ("\\\\" + mkRegGroup(importKeys)).r
  protected val groups: Regex = "\\\\\\w*\\*?(\\[(.*?)\\])?\\{(.*?)\\}.*".r
  protected val beginModnl: Regex = "\\\\begin\\{modnl\\}\\[.*?\\]?\\{(.*?)\\}.*".r
  protected val mhinputRef: Regex = "\\\\mhinputref(\\[(.*?)\\])?\\{(.*?)\\}.*".r
  protected val tikzinput: Regex = ".*\\\\c?m?h?tikzinput(\\[(.*?)\\])?\\{(.*?)\\}.*".r

  protected def entryToPath(p: String) = File(p).setExtension("tex").filepath

  protected def noAmble(f: File): Boolean = {
    val source = readSourceRebust(f)
    var res = false
    for (l <- source.getLines(); if !res)
      if (l.trim.startsWith("\\documentclass")) res = true
    res
  }

  protected def getProfile(a: Archive): Option[String] = {
    val key = "profile"
    var opt = a.properties.get(key)
    if (opt.isEmpty) {
      val gm: File = groupMetaInf(a) / "MANIFEST.MF"
      if (gm.exists && gm.isFile)
        opt = File.readProperties(gm).get(key)
    }
    opt
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

  protected def procLogger(output: StringBuffer, pipeOutput: Boolean): ProcessLogger = {
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
  def reallyBuildFile(bt: BuildTask)

  def buildFile(bt: BuildTask): Unit = if (!skip(bt)) reallyBuildFile(bt)

  def mkDep(a: Archive, ar: String, fp: FilePath, key: String): Option[Dependency] =
    controller.getOrAddArchive(a.baseDir / ar) match {
      case None => None
      case Some(arch) => Some(Dependency(arch, fp, key))
    }

  protected def matchPathAndRep(a: Archive, line: String): Option[Dependency] =
    line match {
      case beginModnl(b) => Some(Dependency(a, entryToPath(b), key))
      case mhinputRef(_, r, b) =>
        val fp = entryToPath(b)
        Option(r) match {
          case Some(id) => mkDep(a, id, fp, "sms")
          case None => Some(Dependency(a, fp, "sms"))
        }
      case tikzinput(_, r, b) =>
        val fp = entryToPath(b)
        val optRepo = Option(r).map(_.split(",").toList.map(_.split("=").toList).
          filter {
            case List("mhrepos", _) => true
            case _ => false
          })
        optRepo match {
          case Some(List(List(_, id))) => mkDep(a, id, fp, "tikzsvg")
          case _ => Some(Dependency(a, fp, "tikzsvg"))
        }
      case groups(_, r, b) =>
        val fp = entryToPath(b)
        val optRepo = Option(r).map(_.split(",").toList.sorted.map(_.split("=").toList))
        optRepo match {
          case Some(List(List(id))) => mkDep(a, id, fp, key)
          case Some(List("path", p) :: tl) =>
            val path = entryToPath(p)
            tl match {
              case List(List("repos", id)) => mkDep(a, id, path, key)
              case Nil => Some(Dependency(a, path, key))
              case _ => None
            }
          case None => Some(Dependency(a, fp, key))
          case _ => None
        }
      case _ => None
    }

  protected def readingSource(a: Archive, in: File): Seq[Dependency] = {
    var res: List[Dependency] = Nil
    val source = readSourceRebust(in)
    source.getLines().foreach { line =>
      val l = stripComment(line).trim
      val err = "unexpected line: " + l + "\nin file: " + in
      val verbIndex = l.indexOf("\\verb")
      if (verbIndex <= -1 && importRegs.findFirstIn(l).isDefined) {
        matchPathAndRep(a, l) match {
          case None => log(err)
          case Some(p) => res ::= p
        }
      }
    }
    log(in + ": " + res.mkString(", "))
    val safe = res.filter { case Dependency(ar, fp, _) =>
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
    }
    safe
  }

  override def getSingleDeps(controller: Controller, a: Archive, fp: FilePath): Set[Dependency] = {
    val in = a / inDim / fp
    if (in.exists()) {
      readingSource(a, in).filter {
        case Dependency(_, _, tgt) => key != "sms" || tgt != "tikzsvg"
      }.toSet
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

  protected def deleteWithLog(f: File): Unit = {
    f.delete()
    logResult("deleted " + f)
  }

  override def cleanDir(a: Archive, curr: Current): Unit = {
    super.cleanDir(a, curr)
    val errDir = getFolderErrorFile(a, curr.path).up
    val outFile = getFolderOutFile(a, curr.path)
    val outDir = outFile.up
    if (errDir.isDirectory) errDir.deleteDir()
    outFile.getExtension.foreach(ext => getDirFilesByExt(a, outDir, List(ext)).foreach(deleteWithLog))
  }
}

package info.kwarc.mmt.stex

import java.nio.charset.{Charset, MalformedInputException}
import java.nio.file.Files
import java.util.regex.PatternSyntaxException

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.archives._
import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.parser.{SourcePosition, SourceRef, SourceRegion}
import info.kwarc.mmt.api.utils._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._
import scala.concurrent.duration._
import scala.io.BufferedSource
import scala.sys.process._
import scala.util.matching.Regex

/** common code for sms, latexml und pdf generation */
abstract class LaTeXBuildTarget extends TraversingBuildTarget {
  val localpathsFile = "localpaths.tex"
  val inDim = source
  protected var pipeOutput: Boolean = false
  protected val pipeOutputOption: String = "--pipe-worker-output"
  /** timout in seconds */
  protected var timeoutVal: Int = 300
  protected val timeoutOption: String = "timeout"
  protected val c = java.io.File.pathSeparator

  protected case class LatexError(s: String, l: String) extends ExtensionError(key, s) {
    override val extraMessage = l
  }

  protected def logSuccess(f: FilePath) = logResult("success " + f)

  protected def logFailure(f: FilePath) = logResult("failure " + f)

  protected def sysEnv(v: String): String = sys.env.getOrElse(v, "")

  override def defaultFileExtension: String = "tex"

  override def start(args: List[String]): Unit = {
    if (args.contains(pipeOutputOption)) pipeOutput = true
  }

  protected def partArg(arg: String, args: List[String]): (List[String], List[String]) = {
    val (matched, rest) = args.partition(_.startsWith("--" + arg + "="))
    (matched.map(_.substring(arg.length + 3)), rest)
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

  protected def mathHubDir(bt: BuildTask): File = bt.archive.baseDir.up

  protected def extBase(bt: BuildTask): File = mathHubDir(bt) / "ext"

  protected def stexStyDir(bt: BuildTask): File = extBase(bt) / "sTeX" / "sty"

  protected def styPath(bt: BuildTask): File = mathHubDir(bt) / "sty"

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

  def mkRegGroup(l: List[String]): String = l.mkString("(", "|", ")")

  private val importKeys: List[String] = List(
    "guse", "gimport", "usemhmodule", "importmhmodule"
  )
  private val importRegs: Regex = ("^\\\\" + mkRegGroup(importKeys)).r
  private val groups: Regex = "\\\\\\w*\\*?(\\[(.*?)\\])?\\{(.*?)\\}.*".r

  private def matchPathAndRep(aStr: String, line: String): Option[(String, FilePath)] =
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

  protected def readSourceRebust(f: File): BufferedSource = scala.io.Source.fromFile(f, "ISO-8859-1")

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

  protected def noAmble(f: File): Boolean = {
    val source = readSourceRebust(f)
    var res = false
    for (l <- source.getLines(); if !res)
      if (l.trim.startsWith("\\documentclass")) res = true
    res
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

  protected def langFiles(lang: Option[String], files: List[String]): List[String] =
    files.filter(f => getLang(File(f)) == lang)
}

class AllTeX extends LaTeXBuildTarget {
  val key: String = "alltex"
  val outDim: ArchiveDimension = source
  override val outExt = "tex"

  // we do nothing for single files
  def reallyBuildFile(bt: BuildTask): Unit = {}

  override def cleanFile(a: Archive, curr: Current): Unit = {}

  override def cleanDir(a: Archive, curr: Current): Unit = {
    val dir = curr.file
    dir.list.filter(f => f.startsWith("all.") && f.endsWith(".tex")).sorted.
      map(f => dir / f) foreach { f =>
      f.delete()
      logResult("deleted " + getOutPath(a, f))
    }
    super.cleanDir(a, curr)
  }

  override def update(a: Archive, up: Update, in: FilePath = EmptyPath): Unit =
    buildDir(a, in, a / inDim / in, force = false)

  override def buildDepsFirst(a: Archive, in: FilePath = EmptyPath): Unit =
    buildDir(a, in, a / inDim / in, force = false)

  override def buildDir(bt: BuildTask, builtChildren: List[BuildTask]): Unit =
    buildDir(bt.archive, bt.inPath, bt.inFile, force = true)

  private def buildDir(a: Archive, in: FilePath, dir: File, force: Boolean): Unit = {
    val dirFiles = getDirFiles(a, dir)
    if (dirFiles.nonEmpty) {
      createLocalPaths(a, dir)
      val ts = getDeps(controller, getFilesRec(a, in)).map(p => p._1 / inDim / p._2)
      val files = ts.filter(dirFiles.map(f => dir / f).contains(_)).map(_.getName)
      assert(files.length == dirFiles.length)
      val langs = files.flatMap(f => getLang(File(f))).toSet
      val nonLangFiles = langFiles(None, files)
      if (nonLangFiles.nonEmpty) createAllFile(a, None, dir, nonLangFiles, force)
      langs.toList.sorted.foreach(l => createAllFile(a, Some(l), dir, files, force))
    }
  }

  private def ambleText(preOrPost: String, a: Archive, lang: Option[String]): List[String] =
    readSourceRebust(getAmbleFile(preOrPost, a, lang)).getLines().toList

  private def createAllFile(a: Archive, lang: Option[String], dir: File,
                            files: List[String], force: Boolean): Unit = {
    val all = dir / ("all" + lang.map("." + _).getOrElse("") + ".tex")
    val ls = langFiles(lang, files)
    if (!force && all.exists() && ls.forall(f => (dir / f).lastModified() < all.lastModified()))
      logResult("up-to-date " + getOutPath(a, all)) // does not detect deleted files!
    else {
      val w = File.Writer(all)
      ambleText("pre", a, lang).foreach(w.println)
      w.println("")
      ls.foreach { f =>
        w.println("\\begin{center} \\LARGE File: \\url{" + f + "} \\end{center}")
        w.println("\\input{" + File(f).stripExtension + "} \\newpage")
        w.println("")
      }
      ambleText("post", a, lang).foreach(w.println)
      w.close()
      logSuccess(getOutPath(a, all))
    }
  }
}

/** sms generation */
class SmsGenerator extends LaTeXBuildTarget {
  val key = "sms"
  val outDim: ArchiveDimension = source
  override val outExt = "sms"
  private val smsKeys: List[String] = List(
    "gadopt", "symdef", "abbrdef", "symvariant", "keydef", "listkeydef",
    "importmodule", "gimport", "adoptmodule", "importmhmodule", "adoptmhmodule"
  )
  private val smsTopKeys: List[String] = List(
    "module", "importmodulevia", "importmhmodulevia"
  )
  private val smsRegs: Regex = {
    val alt: String = smsTopKeys.mkString("\\{(", "|", ")\\}")
    ("^\\\\(" + mkRegGroup(smsKeys) + "|begin" + alt + "|end" + alt + ")").r
  }
  private val encodings = List("ISO-8859-1", Charset.defaultCharset.toString, "UTF-8",
    "UTF-16").distinct

  private def createSms(bt: BuildTask, encs: List[String]): Unit = {
    val readMsg = "reading " + bt.inPath
    encs match {
      case hd :: tl =>
        try {
          log(readMsg + " using encoding " + hd)
          creatingSms(bt.inFile, bt.outFile, hd)
          logSuccess(bt.outPath)
        }
        catch {
          case _: MalformedInputException =>
            log(readMsg + bt.inPath + " failed")
            createSms(bt, tl)
        }
      case Nil =>
        bt.errorCont(LocalError("no suitable encoding found for " + bt.inPath))
        logFailure(bt.outPath)
    }
  }

  private def creatingSms(inFile: File, outFile: File, enc: String): Unit = {
    val source = scala.io.Source.fromFile(inFile, enc)
    val w = File.Writer(outFile)
    source.getLines().foreach { line =>
      val idx = line.indexOf('%')
      val l = (if (idx > -1) line.substring(0, idx) else line).trim
      val verbIndex = l.indexOf("\\verb")
      if (verbIndex <= -1 && smsRegs.findFirstIn(l).isDefined)
        w.println(l + "%")
    }
    w.close()
  }

  def reallyBuildFile(bt: BuildTask): Unit = {
    try createSms(bt, encodings)
    catch {
      case e: Exception =>
        bt.errorCont(LocalError("sms exception: " + e))
        logFailure(bt.outPath)
    }
  }
}

/** importer wrapper for stex */
class LaTeXML extends LaTeXBuildTarget {
  val key = "latexml"
  override val outExt = "omdoc"
  val outDim = RedirectableDimension("latexml")
  // the latexml client
  private var latexmlc = "latexmlc"
  private var expire = "600"
  private var port = "3334"
  private var profile = "stex-smglom-module"
  private var perl5lib = "perl5lib"
  private var preloads: Seq[String] = Nil
  private var paths: Seq[String] = Nil

  private def getArg(arg: String, args: List[String]): Option[String] =
    partArg(arg, args)._1.headOption.map(Some(_)).
      getOrElse(controller.getEnvVar("LATEXML" + arg.toUpperCase))

  override def start(args: List[String]): Unit = {
    super.start(args)
    val (opts, nonOptArgs) = execArgs(args)
    latexmlc = getFromFirstArgOrEnvvar(nonOptArgs, "LATEXMLC", latexmlc)
    expire = getArg("expire", opts).getOrElse(expire)
    port = getArg("port", opts).getOrElse(port)
    profile = getArg("profile", opts).getOrElse(profile)
    val (preloadOpts, rest1) = partArg("preload", opts)
    preloads = preloads ++
      controller.getEnvVar("LATEXMLPRELOADS").getOrElse("").split(" ").filter(_.nonEmpty)
    val (pathOpts, rest2) = partArg("path", opts)
    paths = pathOpts ++
      controller.getEnvVar("LATEXMLPATHS").getOrElse("").split(" ").filter(_.nonEmpty)
    val restOpts = rest2.diff(List("--expire=" + expire, "--port=" + port, "--profile=" + profile))
    if (restOpts.nonEmpty) log("unknown options: " + restOpts.mkString(" "))
  }

  private def str2Level(lev: String): Level.Level = lev match {
    case "Info" => Level.Info
    case "Error" => Level.Error
    case "Fatal" => Level.Fatal
    case _ => Level.Warning
  }

  private def line2Region(sLine: String, inFile: File): SourceRegion = {
    val regEx = """#textrange\(from=(\d+);(\d+),to=(\d+);(\d+)\)""".r
    val range = sLine.stripPrefix(inFile.toString)
    range match {
      case regEx(l1, c1, l2, c2) =>
        SourceRegion(SourcePosition(-1, l1.toInt, c1.toInt),
          SourcePosition(-1, l2.toInt, c2.toInt))
      case _ => SourceRegion.none
    }
  }

  private def line2Level(line: String): (Option[Level.Level], String) = {
    val msgLine = """(Info|Warning|Error|Fatal):(.*)""".r
    line match {
      case msgLine(lev, rest) => (Some(str2Level(lev)), rest)
      case _ => (None, line)
    }
  }

  private object LtxLog {
    var optLevel: Option[Level.Level] = None
    var msg: List[String] = Nil
    var newMsg = true
    var region = SourceRegion.none

    def reportError(bt: BuildTask): Unit = {
      optLevel match {
        case Some(lev) =>
          val ref = SourceRef(FileURI(bt.inFile), region)
          bt.errorCont(CompilerError(key, ref, msg.reverse, lev))
          optLevel = None
        case None =>
      }
      msg = Nil
      newMsg = true
      region = SourceRegion.none
    }
  }

  private def readLogFile(bt: BuildTask, logFile: File) {
    val source = scala.io.Source.fromFile(logFile)
    source.getLines().foreach { line =>
      val (newLevel, restLine) = line2Level(line)
      if (newLevel.isDefined) {
        LtxLog.reportError(bt)
        LtxLog.optLevel = newLevel
        LtxLog.msg = List(restLine)
        LtxLog.newMsg = false
      }
      else if (line.startsWith("\t")) {
        val sLine = line.substring(1)
        val newRegion = line2Region(sLine, bt.inFile)
        if (newRegion == SourceRegion.none)
          LtxLog.msg = sLine :: LtxLog.msg
        else LtxLog.region = newRegion
      }
      else LtxLog.reportError(bt)
    }
    LtxLog.reportError(bt)
  }

  private def extEnv(bt: BuildTask): List[(String, String)] = {
    val perl5 = extBase(bt) / perl5lib
    val p5 = "PERL5LIB"
    val perl5path = perl5 / "lib" / "perl5"
    val latexmlBlib = extBase(bt) / "LaTeXML" / "blib" / "lib"
    val path = if (c == ":") "PATH" else "Path"
    (path -> (perl5 / "bin" + c + sysEnv(path))) ::
      (p5 -> (perl5path + c + latexmlBlib + c + sysEnv(p5))) :: env(bt)
  }

  private def setLatexmlc(bt: BuildTask): Unit =
    if (!File(latexmlc).isAbsolute) {
      val latexmlcpath = extBase(bt) / perl5lib / "bin" / latexmlc
      if (latexmlcpath.exists) {
        latexmlc = latexmlcpath.toString
      }
    }

  /** Compile a .tex file to OMDoc */
  def reallyBuildFile(bt: BuildTask): Unit = {
    val lmhOut = bt.outFile
    val logFile = bt.outFile.setExtension("ltxlog")
    lmhOut.delete()
    logFile.delete()
    createLocalPaths(bt)
    setLatexmlc(bt)
    val output = new StringBuffer()
    val argSeq = Seq(latexmlc, bt.inFile.toString,
      "--profile=" + profile, "--path=" + styPath(bt),
      "--destination=" + lmhOut, "--log=" + logFile) ++
      (if (noAmble(bt.inFile)) Nil
      else Seq("--preamble=" + getAmbleFile("pre", bt),
        "--postamble=" + getAmbleFile("post", bt))) ++
      Seq("--expire=" + expire, "--port=" + port) ++ preloads.map("--preload=" + _) ++
      paths.map("--path=" + _)
    log(argSeq.mkString(" ").replace(" --", "\n --"))
    try {
      val pb = Process(argSeq, bt.archive / inDim, extEnv(bt): _*)
      val exitCode = timeout(pb, procLogger(output))
      if (exitCode != 0 || lmhOut.length == 0) {
        bt.errorCont(LatexError("no omdoc created", output.toString))
        lmhOut.delete()
        logFailure(bt.outPath)
      }
      if (lmhOut.exists()) logSuccess(bt.outPath)
      if (logFile.exists())
        readLogFile(bt, logFile)
    } catch {
      case e: Exception =>
        lmhOut.delete()
        bt.errorCont(LatexError(e.toString, output.toString))
        logFailure(bt.outPath)
    }
  }

  override def cleanFile(arch: Archive, curr: Current): Unit = {
    getOutFile(arch, curr.path).setExtension("ltxlog").delete()
    super.cleanFile(arch, curr)
  }
}

/** pdf generation */
class PdfLatex extends LaTeXBuildTarget {
  val key = "pdflatex"
  override val outExt = "pdf"
  val outDim = Dim("export", "pdflatex", inDim.toString)
  private var pdflatexPath: String = "xelatex"

  override def start(args: List[String]): Unit = {
    super.start(args)
    val (opts, nonOptArgs) = execArgs(args)
    val newPath = getFromFirstArgOrEnvvar(nonOptArgs, "PDFLATEX", pdflatexPath)
    if (newPath != pdflatexPath) {
      pdflatexPath = newPath
      log("using executable \"" + pdflatexPath + "\"")
    }
    if (opts.nonEmpty) logResult("unknown options: " + opts.mkString(" "))
  }

  def reallyBuildFile(bt: BuildTask): Unit = {
    val pdfFile = bt.inFile.setExtension("pdf")
    pdfFile.delete()
    bt.outFile.delete()
    createLocalPaths(bt)
    val styDir = stexStyDir(bt)
    val output = new StringBuffer()
    try {
      val in = bt.inFile
      val pbCat = if (noAmble(in)) Process.cat(in.toJava)
      else Process.cat(Seq(getAmbleFile("pre", bt), in,
        getAmbleFile("post", bt)).map(_.toJava))
      val pb = pbCat #| Process(Seq(pdflatexPath, "-jobname",
        in.stripExtension.getName, "-interaction", "scrollmode"),
        in.up, env(bt): _*)
      timeout(pb, procLogger(output))
      val pdflogFile = in.setExtension("pdflog")
      if (!pipeOutput) File.write(pdflogFile, output.toString)
      if (pdfFile.length == 0) {
        bt.errorCont(LatexError("no pdf created", output.toString))
        pdfFile.delete()
        logFailure(bt.outPath)
      }
      if (pdfFile.exists && pdfFile != bt.outFile)
        Files.copy(pdfFile.toPath, bt.outFile.toPath)
      if (bt.outFile.exists)
        logSuccess(bt.outPath)
    } catch {
      case e: Exception =>
        bt.outFile.delete()
        bt.errorCont(LatexError(e.toString, output.toString))
        logFailure(bt.outPath)
    }
  }

  override def cleanFile(arch: Archive, curr: Current): Unit = {
    val f = arch / inDim / curr.path
    List("aux", "idx", "log", "out", "pdf", "pdflog", "thm", "nav", "snm", "toc").
      foreach(f.setExtension(_).delete())
    super.cleanFile(arch, curr)
  }
}

class AllPdf extends PdfLatex {
  override val key = "allpdf"

  override def includeFile(n: String): Boolean =
    n.endsWith(".tex") && n.startsWith("all.")
}

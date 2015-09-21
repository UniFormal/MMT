package info.kwarc.mmt.stex

import java.nio.charset.{Charset, MalformedInputException}
import java.nio.file.Files
import java.util.regex.PatternSyntaxException

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.archives._
import info.kwarc.mmt.api.parser.{SourcePosition, SourceRef, SourceRegion}
import info.kwarc.mmt.api.utils._

import scala.sys.process.{Process, ProcessLogger}
import scala.util.matching.Regex

/** common code for sms, latexml und pdf generation */
abstract class LaTeXBuildTarget extends TraversingBuildTarget {
  val localpathsFile = "localpaths.tex"
  val inDim = source
  protected var pipeOutput: Boolean = false
  protected val pipeOutputOption: String = "--pipe-worker-output"
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

  protected def execArgs(args: List[String]): List[String] =
    args.map(a => if (a.startsWith("--" + key + "=")) a.substring(key.length + 3) else a)

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

  protected def createLocalPaths(bt: BuildTask): Unit = {
    val dir = bt.inFile.up
    val fileName = dir / localpathsFile
    val a = bt.archive
    val repoDir = a.root
    val groupRepo = a.groupDir.getName + "/" + repoDir.getName + "}"
    val text: List[String] = List(
      "% this file defines root path local repository",
      "\\defpath{MathHub}{" + a.baseDir.getPath + "}",
      "\\mhcurrentrepos{" + groupRepo,
      "\\input{" + repoDir.getPath + "/lib/WApersons}",
      "% we also set the base URI for the LaTeXML transformation",
      "\\baseURI[\\MathHub{}]{https://mathhub.info/" + groupRepo
    )
    if (!fileName.exists()) {
      File.WriteLineWise(fileName, text)
      log("created file " + fileName)
    }
  }

  protected def getAmbleFile(preOrPost: String, bt: BuildTask): File = {
    val repoDir = bt.archive.root
    val lang: Option[String] = bt.inFile.stripExtension.getExtension
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
          logResult("correct no-" + outExt + " property in META-INF/MANIFEST.MF")
          true // skip everything until corrected
      }
    val exclude = excludes.exists(patternMatch)
    if (exclude) logResult("skipping " + getOutPath(bt.archive, bt.inFile))
    exclude
  }

  def reallyBuildFile(bt: BuildTask)

  def buildFile(bt: BuildTask): Unit = if (!skip(bt)) reallyBuildFile(bt)

  def mkRegGroup(l: List[String]): String = l.mkString("(", "|", ")")
}

/** sms generation */
class SmsGenerator extends LaTeXBuildTarget {
  val key = "sms"
  val outDim: ArchiveDimension = source
  override val outExt = "sms"
  private val smsKeys: List[String] = List(
    "guse", "gadopt", "symdef", "abbrdef", "symvariant", "keydef", "listkeydef",
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
      case e: Throwable =>
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
  private var expire = "10"
  private var port: Option[String] = None
  private var profile = "stex-smglom-module"
  private var perl5lib = "perl5lib"
  private var preloads: Seq[String] = Nil
  private var paths: Seq[String] = Nil

  private def filterArg(arg: String, args: List[String]): List[String] =
    args.filter(_.startsWith("--" + arg + "=")).map(_.substring(arg.length + 3))

  private def getArg(arg: String, args: List[String]): Option[String] =
    filterArg(arg, args).headOption.map(Some(_)).
      getOrElse(controller.getEnvVar("LATEXML" + arg.toUpperCase))

  override def start(args: List[String]): Unit = {
    super.start(args)
    val (opts, nonOptArgs) = execArgs(args).partition(_.startsWith("--"))
    latexmlc = getFromFirstArgOrEnvvar(nonOptArgs, "LATEXMLC", latexmlc)
    expire = getArg("expire", opts).getOrElse(expire)
    port = getArg("port", opts)
    profile = getArg("profile", opts).getOrElse(profile)
    preloads = filterArg("preload", opts) ++
      controller.getEnvVar("LATEXMLPRELOADS").getOrElse("").split(" ").filter(_.nonEmpty)
    paths = filterArg("path", opts) ++
      controller.getEnvVar("LATEXMLPATHS").getOrElse("").split(" ").filter(_.nonEmpty)
    val restOpts = opts.diff(List(pipeOutputOption, "--expire=" + expire, "--port=" + port, "--profile=" + profile)).
      filter(a => !(a.startsWith("--path=") || a.startsWith("--preload=")))
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
    bt.outFile.delete()
    createLocalPaths(bt)
    setLatexmlc(bt)
    val output = new StringBuffer()
    val argSeq = Seq(latexmlc, bt.inFile.toString,
      "--quiet", "--profile=" + profile, "--path=" + styPath(bt),
      "--destination=" + lmhOut, "--log=" + logFile,
      "--preamble=" + getAmbleFile("pre", bt),
      "--postamble=" + getAmbleFile("post", bt),
      "--expire=" + expire) ++ port.map("--port=" + _) ++ preloads.map("--preload=" + _) ++
      paths.map("--path=" + _)
    log(argSeq.mkString(" ").replace(" --", "\n --"))
    val pb = Process(argSeq, bt.archive / inDim, extEnv(bt): _*)
    val exitCode = pb.!(procLogger(output))
    if (exitCode != 0 || lmhOut.length == 0) {
      bt.errorCont(LatexError("no omdoc created", output.toString))
      lmhOut.delete()
      logFailure(bt.outPath)
    }
    if (bt.outFile.exists()) logSuccess(bt.outPath)
    if (logFile.exists())
      readLogFile(bt, logFile)
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
  val outDim = Dim("export", key, inDim.toString)
  private var pdflatexPath: String = "xelatex"

  override def start(args: List[String]): Unit = {
    super.start(args)
    val (opts, nonOptArgs) = execArgs(args).diff(List(pipeOutputOption)).partition(_.startsWith("--"))
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
      val pbCat = Process.cat(Seq(getAmbleFile("pre", bt), bt.inFile,
        getAmbleFile("post", bt)).map(_.toJava))
      val pb = pbCat #| Process(Seq(pdflatexPath, "-jobname",
        bt.inFile.stripExtension.getName, "-interaction", "scrollmode"),
        bt.inFile.up, env(bt): _*)
      pb.!(procLogger(output))
      val pdflogFile = bt.inFile.setExtension("pdflog")
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
      case e: Throwable =>
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

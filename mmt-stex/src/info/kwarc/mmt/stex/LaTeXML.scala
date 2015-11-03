package info.kwarc.mmt.stex

import java.net.{BindException, ServerSocket}
import java.nio.charset.{Charset, MalformedInputException}
import java.nio.file.Files

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.archives._
import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.parser.{SourcePosition, SourceRef, SourceRegion}
import info.kwarc.mmt.api.utils._

import scala.sys.process._
import scala.util.matching.Regex

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
      map(f => dir / f).foreach(deleteWithLog)
    super.cleanDir(a, curr)
  }

  override def update(a: Archive, up: Update, in: FilePath = EmptyPath): Unit =
    a.traverse[Unit](inDim, in, TraverseMode(includeFile, includeDir, parallel))({
      case _ =>
    }, { case (c@Current(inDir, inPath), _) =>
         buildDir(a, inPath, inDir, force = false)
    })

  override def buildDepsFirst(a: Archive, up: Update, in: FilePath = EmptyPath): Unit =
    update(a, up, in)

  override def buildDir(bt: BuildTask, builtChildren: List[BuildTask]): Unit =
    buildDir(bt.archive, bt.inPath, bt.inFile, force = true)

  private def buildDir(a: Archive, in: FilePath, dir: File, force: Boolean): Unit = {
    val dirFiles = getDirFiles(a, dir, includeFile)
    if (dirFiles.nonEmpty) {
      createLocalPaths(a, dir)
      val ts = getDeps(controller, key, getFilesRec(a, in)).map(d => d.archive / inDim / d.filePath)
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
          creatingSms(bt.archive, bt.inFile, bt.outFile, hd)
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

  private val importMhModule: Regex = "\\\\importmhmodule\\[(.*?)\\](.*?)".r
  private val gimport: Regex = "\\\\gimport\\*?(\\[(.*?)\\])?\\{(.*?)\\}.*".r

  private def mkImport(b: File, r: String, p: String, a: String) =
    "\\importmodule[load=" + b + "/" + r + "/source/" + p + "]" + a

  private def creatingSms(a: Archive, inFile: File, outFile: File, enc: String): Unit = {
    val source = scala.io.Source.fromFile(inFile, enc)
    val w = File.Writer(outFile)
    source.getLines().foreach { line =>
      val l = stripComment(line).trim
      var n = l
      val verbIndex = l.indexOf("\\verb")
      if (verbIndex <= -1 && smsRegs.findFirstIn(l).isDefined) {
        l match {
          case importMhModule(r, b) =>
            val m = r.split(",").toList.sorted.map(_.split("=").toList)
            m match {
              case List("path", p) :: tl =>
                tl match {
                  case Nil =>
                    n = mkImport(a.baseDir, archString(a), p, b)
                  case List(List("repos", id)) =>
                    n = mkImport(a.baseDir, id, p, b)
                  case _ =>
                }
              case _ =>
            }
          case gimport(_, r, p) =>
            val b = "{" + p + "}"
            Option(r) match {
              case Some(id) =>
                n = mkImport(a.baseDir, id, p, b)
              case None =>
                n = mkImport(a.baseDir, archString(a), p, b)
            }
          case _ =>
        }
        w.println(n + "%")
      }
    }
    w.close()
  }

  def reallyBuildFile(bt: BuildTask): Unit = {
    createLocalPaths(bt)
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
  private var latexmls = "latexmls"
  private var expire = "600"
  private val delaySecs: Int = 1000
  private val portDefault: Int = 3334
  private var port: Int = portDefault
  private val portModulo: Int = 1000
  private var portSet: Boolean = false
  private var profile = "stex-smglom-module"
  private var profileSet: Boolean = false
  private var perl5lib = "perl5lib"
  private var preloads: Seq[String] = Nil
  private var paths: Seq[String] = Nil

  private def getArg(arg: String, args: List[String]): Option[String] =
    partArg(arg, args)._1.headOption.map(Some(_)).
      getOrElse(controller.getEnvVar("LATEXML" + arg.toUpperCase))

  override def start(args: List[String]): Unit = {
    super.start(args)
    val (rOpts, nonOptArgs) = execArgs(args)
    latexmlc = getFromFirstArgOrEnvvar(nonOptArgs, "LATEXMLC", latexmlc)
    val (sOpts, opts) = partArg(latexmls, rOpts)
    latexmls = sOpts.headOption.map(Some(_)).getOrElse(controller.getEnvVar("LATEXMLS")).
      getOrElse(latexmls)
    expire = getArg("expire", opts).getOrElse(expire)
    val newPort = getArg("port", opts)
    portSet = newPort.isDefined
    port = try newPort.getOrElse(port.toString).toInt catch {
      case _: Exception => port
    }
    val newProfile = getArg("profile", opts)
    profileSet = newProfile.isDefined
    profile = newProfile.getOrElse(profile)
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
    var phase = 1

    def phaseToString(p: Int): String = "latexml-" + (p match {
      case 1 => "compiler"
      case 2 => "parsing"
      case 3 => "post"
      case 4 => "xslt"
    })

    def reportError(bt: BuildTask): Unit = {
      optLevel match {
        case Some(lev) =>
          val ref = SourceRef(FileURI(bt.inFile), region)
          val messageList = msg.reverse
          bt.errorCont(SourceError(key, ref, phaseToString(phase) + ": " + messageList.head
            , messageList.tail, lev))
          optLevel = None
        case None =>
      }
      msg = Nil
      newMsg = true
      region = SourceRegion.none
    }
  }

  private def readLogFile(bt: BuildTask, logFile: File) {
    LtxLog.phase = 1
    val source = readSourceRebust(logFile)
    source.getLines().foreach { line =>
      if (line.startsWith("(Math Parsing..."))
        LtxLog.phase = 2
      else if (line.startsWith("(post-processing..."))
        LtxLog.phase = 3
      else if (line.startsWith("(XSLT[using "))
        LtxLog.phase = 4
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

  private def setLatexmlBin(bin: String, bt: BuildTask): String =
    if (!File(bin).isAbsolute) {
      val latexmlcpath = extBase(bt) / perl5lib / "bin" / bin
      if (latexmlcpath.exists)
        latexmlcpath.toString
      else bin
    }
    else bin

  private def setLatexmlBins(bt: BuildTask): Unit = {
    latexmlc = setLatexmlBin(latexmlc, bt)
    latexmls = setLatexmlBin(latexmls, bt)
  }

  private def procIO(output: StringBuffer): ProcessIO =
    new ProcessIO(_.close(), _.close(), { i =>
      val in = scala.io.Source.fromInputStream(i)
      val str = in.mkString
      in.close()
      output.append(str)
      if (pipeOutput) System.err.print(str)
    }, true)

  /** Compile a .tex file to OMDoc */
  def reallyBuildFile(bt: BuildTask): Unit = {
    val lmhOut = bt.outFile
    val logFile = bt.outFile.setExtension("ltxlog")
    lmhOut.delete()
    logFile.delete()
    createLocalPaths(bt)
    setLatexmlBins(bt)
    val realPort: Int = if (portSet) port
    else port + Math.abs(bt.archive.id.hashCode % portModulo)
    val realProfile = if (profileSet) profile
    else getProfile(bt.archive).getOrElse(profile)
    val output = new StringBuffer()
    val argSeq = Seq(latexmlc, bt.inFile.toString,
      "--profile=" + realProfile, "--path=" + styPath(bt),
      "--destination=" + lmhOut, "--log=" + logFile) ++
      (if (noAmble(bt.inFile)) Seq("--whatsin=document")
      else Seq("--preamble=" + getAmbleFile("pre", bt),
        "--postamble=" + getAmbleFile("post", bt))) ++
      Seq("--expire=" + expire, "--port=" + realPort) ++ preloads.map("--preload=" + _) ++
      paths.map("--path=" + _)
    log(argSeq.mkString(" ").replace(" --", "\n --"))
    val lEnv = extEnv(bt)
    try {
      val pbs = Process(Seq(latexmls, "--expire=" + expire, "--port=" + realPort,
        "--autoflush=100"), bt.archive / inDim, lEnv: _*)
      val startServer = try {
        val s = new ServerSocket(realPort)
        s.close()
        true
      }
      catch {
        case ex: BindException =>
          false
      }
      if (startServer) {
        pbs.run(procIO(output))
        Thread.sleep(delaySecs)
      }
      val pb = Process(argSeq, bt.archive / inDim, lEnv: _*)
      val exitCode = timeout(pb, procLogger(output, pipeOutput = false))
      if (exitCode != 0 || lmhOut.length == 0) {
        bt.errorCont(LatexError("no omdoc created", output.toString))
        lmhOut.delete()
        logFailure(bt.outPath)
      }
      if (lmhOut.exists()) logSuccess(bt.outPath)
    } catch {
      case e: Exception =>
        lmhOut.delete()
        bt.errorCont(LatexError(e.toString, output.toString))
        logFailure(bt.outPath)
    }
    if (logFile.exists()) {
      readLogFile(bt, logFile)
      if (pipeOutput) File.ReadLineWise(logFile)(println)
    }
    if (pipeOutput) print(output.toString)
  }

  override def cleanFile(arch: Archive, curr: Current): Unit = {
    getOutFile(arch, curr.path).setExtension("ltxlog").delete()
    super.cleanFile(arch, curr)
  }

  override def cleanDir(a: Archive, curr: Current): Unit = {
    super.cleanDir(a, curr)
    val outDir = getFolderOutFile(a, curr.path).up
    if (outDir.isDirectory) outDir.deleteDir()
  }
}

/** pdf generation */
class PdfLatex extends LaTeXBuildTarget {
  val key = "pdflatex"
  override val outExt = "pdf"
  val outDim: ArchiveDimension = Dim("export", "pdflatex", inDim.toString)
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

  protected def runPdflatex(bt: BuildTask, output: StringBuffer): Int = {
    val in = bt.inFile
    val pbCat = if (noAmble(in)) Process.cat(in.toJava)
    else Process.cat(Seq(getAmbleFile("pre", bt), in,
      getAmbleFile("post", bt)).map(_.toJava))
    val pb = pbCat #| Process(Seq(pdflatexPath, "-jobname",
      in.stripExtension.getName, "-interaction", "scrollmode"),
      in.up, env(bt): _*)
    val exit = timeout(pb, procLogger(output, pipeOutput = pipeOutput))
    val pdflogFile = in.setExtension("pdflog")
    if (!pipeOutput) File.write(pdflogFile, output.toString)
    exit
  }

  def reallyBuildFile(bt: BuildTask): Unit = {
    val pdfFile = bt.inFile.setExtension("pdf")
    pdfFile.delete()
    bt.outFile.delete()
    createLocalPaths(bt)
    val output = new StringBuffer()
    try {
      val exit = runPdflatex(bt, output)
      if (exit != 0) {
        bt.errorCont(LatexError("exit code " + exit, output.toString))
        bt.outFile.delete()
        logFailure(bt.outPath)
      } else {
        if (pdfFile.length > 0 && pdfFile != bt.outFile)
          Files.copy(pdfFile.toPath, bt.outFile.toPath)
        logSuccess(if (bt.outFile.exists) bt.outPath else EmptyPath)
      }
    } catch {
      case e: Exception =>
        bt.outFile.delete()
        bt.errorCont(LatexError(e.toString, output.toString))
        logFailure(bt.outPath)
    }
  }

  protected def toBeCleanedExts: List[String] =
    List("aux", "idx", "log", "out", "pdf", "pdflog", "thm", "nav", "snm", "toc")

  override def cleanFile(arch: Archive, curr: Current): Unit = {
    val f = arch / inDim / curr.path
    toBeCleanedExts.foreach(f.setExtension(_).delete())
    super.cleanFile(arch, curr)
  }

  override def cleanDir(a: Archive, curr: Current): Unit = {
    super.cleanDir(a, curr)
    val outDir = getFolderOutFile(a, curr.path).up
    if (outDir.isDirectory) outDir.deleteDir()
    val srcDir = a / inDim / curr.path
    getDirFilesByExt(a, srcDir, toBeCleanedExts).foreach(deleteWithLog)
  }
}

class AllPdf extends PdfLatex {
  override val key = "allpdf"

  override def includeFile(n: String): Boolean =
    n.endsWith(".tex") && n.startsWith("all.")

  override def getSingleDeps(controller: Controller, a: Archive, fp: FilePath): Set[Dependency] = {
    val in = a / inDim / fp
    val optLang = getLang(in)
    val aStr = archString(a)
    val name = in.getName
    langFiles(optLang, getDirFiles(a, in.up, super.includeFile)).
      filter(_ != name).map(f => Dependency(a, fp.dirPath / f, "pdflatex")).toSet
  }
}

class TikzSvg extends PdfLatex {
  override val key = "tikzsvg"
  override val outExt = "svg"
  override val outDim = source

  override def includeDir(n: String): Boolean = n.endsWith("tikz")

  override def reallyBuildFile(bt: BuildTask): Unit = {
    val pdfFile = bt.inFile.setExtension("pdf")
    val svgFile = bt.inFile.setExtension("svg")
    bt.outFile.delete()
    createLocalPaths(bt)
    val output = new StringBuffer()
    try {
      val exit = runPdflatex(bt, output)
      if (exit != 0) {
        bt.errorCont(LatexError("pdflatex exit code " + exit, output.toString))
        logFailure(bt.outPath)
      } else {
        if (pdfFile.length > 0) {
          val pb = Process(Seq("convert", pdfFile.toString, svgFile.toString))
          val exitConvert = timeout(pb, procLogger(output, pipeOutput = pipeOutput))
          if (exitConvert == 0 && svgFile.length() > 0)
            logSuccess(bt.outPath)
          else {
            bt.errorCont(LatexError(if (exitConvert != 0) "exit code " + exitConvert
            else "no svg created", output.toString))
            logFailure(bt.outPath)
          }
        } else {
          bt.errorCont(LatexError("no pdf created", output.toString))
          logFailure(bt.outPath)
        }
      }
    }
    catch {
      case e: Exception =>
        bt.outFile.delete()
        bt.errorCont(LatexError(e.toString, output.toString))
        logFailure(bt.outPath)
    }
  }

  override def cleanDir(a: Archive, curr: Current): Unit = {
    super.cleanDir(a, curr)
    val srcDir = a / inDim / curr.path
    getDirFilesByExt(a, srcDir, toBeCleanedExts).foreach(deleteWithLog)
  }
}

package info.kwarc.mmt.stex

import java.io.{PrintStream, InputStream}
import java.net.{BindException, ServerSocket}
import java.nio.file.Files

import STeXUtils._
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.archives._
import info.kwarc.mmt.api.frontend._
import info.kwarc.mmt.api.utils.AnaArgs._
import info.kwarc.mmt.api.parser.{SourcePosition, SourceRef, SourceRegion}
import info.kwarc.mmt.api.utils._

import scala.sys.process._

class AllTeX extends LaTeXDirTarget {
  val key: String = "alltex"

  def dirFileFilter(f: String): Boolean =
    f.startsWith("all.") && f.endsWith(".tex")

  def buildDir(a: Archive, in: FilePath, dir: File, force: Boolean): BuildResult = {
    val dirFiles = getDirFiles(a, dir, includeFile)
    if (dirFiles.nonEmpty) {
      createLocalPaths(a, dir)
      val deps = getDepsMap(getFilesRec(a, in))
      val ds = Relational.flatTopsort(controller, deps)
      val ts = ds.collect { case bd: FileBuildDependency if bd.key == key => bd }.map(d => d.archive / inDim / d.inPath)
      val files = ts.filter(dirFiles.map(f => dir / f).contains(_)).map(_.getName)
      assert(files.length == dirFiles.length)
      val langs = files.flatMap(f => getLang(File(f))).toSet
      val nonLangFiles = langFiles(None, files)
      if (nonLangFiles.nonEmpty) createAllFile(a, None, dir, nonLangFiles, force)
      langs.toList.sorted.foreach(l => createAllFile(a, Some(l), dir, files, force))
    }
    BuildResult.empty
  }

  private def ambleText(preOrPost: String, a: Archive, lang: Option[String]): List[String] =
    readSourceRebust(getAmbleFile(preOrPost, a, lang)).getLines().toList

  private def createAllFile(a: Archive, lang: Option[String], dir: File,
                            files: List[String], force: Boolean) {
    val all = dir / ("all" + lang.map("." + _).getOrElse("") + ".tex")
    val ls = langFiles(lang, files)
    val outPath = getOutPath(a, all)
    if (!force && all.exists() && ls.forall(f => (dir / f).lastModified() < all.lastModified()))
      logResult("up-to-date " + outPath) // does not detect deleted files!
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
      logSuccess(outPath)
    }
  }
}

import STeXUtils._

/** sms generation */
class SmsGenerator extends LaTeXBuildTarget {
  val key = "sms"
  val outDim: ArchiveDimension = source
  override val outExt = "sms"

  override def includeDir(n: String): Boolean = !n.endsWith("tikz")

  def reallyBuildFile(bt: BuildTask): BuildResult = {
    createLocalPaths(bt)
    try createSmsForEncodings(bt, encodings)
    catch {
      case e: Exception =>
        bt.errorCont(LocalError("sms exception: " + e))
        logFailure(bt.outPath)
    }
    BuildResult.empty
  }
}

/** importer wrapper for stex */
class LaTeXML extends LaTeXBuildTarget {
  val key = "latexml"
  override val outExt = "omdoc"

  override def includeDir(n: String): Boolean = !n.endsWith("tikz")

  val outDim = RedirectableDimension("latexml")
  // the latexml client
  private var latexmlc = "latexmlc"
  private var latexmls = "latexmls"
  private var expire: Int = 600
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
  private var reboot: Boolean = false
  private var nopost: Boolean = false

  private val latexmlOpts: OptionDescrs = List(
    OptionDescr("latexmlc", "", StringArg, "executable path for (client) latexmlc"),
    OptionDescr("latexmls", "", StringArg, "executable path for (server) latexmls"),
    OptionDescr("expire", "", IntArg, "expire argument for (server) latexmls"),
    OptionDescr("port", "", IntArg, "port for (server) latexmls"),
    OptionDescr("profile", "", StringArg, "latexml profile"),
    OptionDescr("preload", "", StringListArg, "preload arguments"),
    OptionDescr("path", "", StringListArg, "path arguments"),
    OptionDescr("reboot", "", NoArg, "only try to terminate (server) latexmls"),
    OptionDescr("nopost", "", NoArg, "omit post processing, create xml")
  )

  override def buildOpts: OptionDescrs = commonOpts ++ latexmlOpts

  private def getArg(arg: String, m: OptionMap): Option[String] =
    m.get(arg).map {
      case IntVal(v) => Some(v.toString)
      case StringVal(s) => Some(s)
      case _ => None
    }.getOrElse(controller.getEnvVar("LATEXML" + arg.toUpperCase))

  override def start(args: List[String]) {
    super.start(args)
    val (_, nonOpts) = splitOptions(remainingStartArguments)
    optionsMap.get("latexmlc").foreach { s =>
      if (nameOfExecutable.nonEmpty) {
        logError("executable overwritten by --latexmlc")
      }
      nameOfExecutable = s.getStringVal
    }
    val nonOptArgs = if (nameOfExecutable.isEmpty) nonOpts else nameOfExecutable :: nonOpts
    latexmlc = getFromFirstArgOrEnvvar(nonOptArgs, "LATEXMLC", latexmlc)
    latexmls = optionsMap.get("latexmls").map(v => Some(v.getStringVal)).getOrElse(controller.getEnvVar("LATEXMLS")).
      getOrElse(latexmls)
    expire = getArg("expire", optionsMap).getOrElse(expire.toString).toInt
    val newPort = getArg("port", optionsMap)
    portSet = newPort.isDefined
    port = newPort.getOrElse(port.toString).toInt
    val newProfile = getArg("profile", optionsMap)
    profileSet = newProfile.isDefined
    profile = newProfile.getOrElse(profile)
    preloads = getStringList(optionsMap, "preload") ++
      controller.getEnvVar("LATEXMLPRELOADS").getOrElse("").split(" ").filter(_.nonEmpty)
    paths = getStringList(optionsMap, "path") ++
      controller.getEnvVar("LATEXMLPATHS").getOrElse("").split(" ").filter(_.nonEmpty)
    reboot = optionsMap.get("reboot").isDefined
    if (reboot) expire = 1
    nopost = optionsMap.get("nopost").isDefined
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

    def reportError(bt: BuildTask) {
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

  private def setLatexmlBins(bt: BuildTask) {
    latexmlc = setLatexmlBin(latexmlc, bt)
    latexmls = setLatexmlBin(latexmls, bt)
  }

  def isServerRunning(realPort: Int): Boolean =
    try {
      val s = new ServerSocket(realPort)
      s.close()
      false
    }
    catch {
      case ex: BindException =>
        true
    }

  /** Compile a .tex file to OMDoc */
  def reallyBuildFile(bt: BuildTask): BuildResult = {
    val realPort: Int = if (portSet) port
    else port + Math.abs(bt.archive.id.hashCode % portModulo)
    setLatexmlBins(bt)
    val lEnv = extEnv(bt)
    val output = new StringBuffer()
    if (reboot) {
      val pbc = Process(Seq(latexmlc, "--expire=" + expire, "--port=" + realPort,
        "literal:restarting"), bt.archive / inDim, lEnv: _*)
      if (isServerRunning(realPort)) {
        logResult("trying to kill latexml server: " + latexmls + " --port=" + realPort)
        pbc.!(ProcessLogger(_ => (), _ => ()))
        Thread.sleep(delaySecs)
      }
    } else {
      val lmhOut = bt.outFile
      val logFile = bt.outFile.setExtension("ltxlog")
      lmhOut.delete()
      logFile.delete()
      createLocalPaths(bt)
      val realProfile = if (profileSet) profile
      else getProfile(bt.archive).getOrElse(profile)
      val argSeq = Seq(latexmlc, bt.inFile.toString,
        "--profile=" + realProfile, "--path=" + styPath(bt),
        "--destination=" + lmhOut, "--log=" + logFile) ++
        (if (noAmble(bt.inFile)) Seq("--whatsin=document")
        else Seq("--preamble=" + getAmbleFile("pre", bt),
          "--postamble=" + getAmbleFile("post", bt))) ++
        Seq("--expire=" + expire, "--port=" + realPort) ++
        (if (nopost) Seq("--nopost") else Nil) ++
        preloads.map("--preload=" + _) ++
        paths.map("--path=" + _)
      log(argSeq.mkString(" ").replace(" --", "\n --"))
      try {
        val pbs = Process(Seq(latexmls, "--expire=" + expire, "--port=" + realPort,
          "--autoflush=100"), bt.archive / inDim, lEnv: _*)
        if (!isServerRunning(realPort) && expire > -1) {
          pbs.run(BasicIO.standard(false).daemonized)
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
    BuildResult.empty
  }

  override def cleanFile(arch: Archive, curr: Current) {
    getOutFile(arch, curr.path).setExtension("ltxlog").delete()
    super.cleanFile(arch, curr)
  }

  override def cleanDir(a: Archive, curr: Current) {
    super.cleanDir(a, curr)
    val outDir = getFolderOutFile(a, curr.path).up
    if (outDir.isDirectory) outDir.deleteDir
  }
}

/** pdf generation */
class PdfLatex extends LaTeXBuildTarget {
  val key = "pdflatex"
  override val outExt = "pdf"
  val outDim: ArchiveDimension = Dim("export", "pdflatex", inDim.toString)
  private var pdflatexPath: String = "xelatex"

  override def start(args: List[String]) {
    super.start(args)
    val (_, nonOpts) = splitOptions(remainingStartArguments)
    val nonOptArgs = if (nameOfExecutable.nonEmpty) nameOfExecutable :: nonOpts
    else nonOpts
    val newPath = getFromFirstArgOrEnvvar(nonOptArgs, "PDFLATEX", pdflatexPath)
    if (newPath != pdflatexPath) {
      pdflatexPath = newPath
      log("using executable \"" + pdflatexPath + "\"")
    }
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

  def reallyBuildFile(bt: BuildTask): BuildResult = {
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
    BuildResult.empty
  }

  protected def toBeCleanedExts: List[String] =
    List("aux", "idx", "log", "out", "pdf", "pdflog", "thm", "nav", "snm", "toc")

  override def cleanFile(arch: Archive, curr: Current) {
    val f = arch / inDim / curr.path
    toBeCleanedExts.foreach(f.setExtension(_).delete())
    super.cleanFile(arch, curr)
  }

  override def cleanDir(a: Archive, curr: Current) {
    super.cleanDir(a, curr)
    val outDir = getFolderOutFile(a, curr.path).up
    if (outDir.isDirectory) outDir.deleteDir
    val srcDir = a / inDim / curr.path
    getDirFilesByExt(a, srcDir, toBeCleanedExts).foreach(deleteWithLog)
  }
}

class AllPdf extends PdfLatex {
  override val key = "allpdf"

  override def includeFile(n: String): Boolean =
    n.endsWith(".tex") && n.startsWith("all.")

  override def estimateResult(bt: BuildTask) = {
    val in = bt.inFile
    val a = bt.archive
    val optLang = getLang(in)
    val aStr = archString(a)
    val name = in.getName
    val ls = langFiles(optLang, getDirFiles(a, in.up, super.includeFile)).
      filter(_ != name).map(f => FileBuildDependency("pdflatex", a, bt.inPath.dirPath / f))
    BuildSuccess(ls, Nil)
  }
}

class TikzSvg extends PdfLatex {
  override val key = "tikzsvg"
  override val outExt = "svg"
  override val outDim = source

  override def includeDir(n: String): Boolean = n.endsWith("tikz")

  override def reallyBuildFile(bt: BuildTask): BuildResult = {
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
    BuildResult.empty
  }

  override def cleanDir(a: Archive, curr: Current) {
    super.cleanDir(a, curr)
    val srcDir = a / inDim / curr.path
    getDirFilesByExt(a, srcDir, toBeCleanedExts).foreach(deleteWithLog)
  }
}

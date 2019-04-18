package info.kwarc.mmt.stex

import java.net.{BindException, ServerSocket}
import java.nio.file.Files

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.archives._
import info.kwarc.mmt.api.parser.{SourcePosition, SourceRef, SourceRegion}
import info.kwarc.mmt.api.utils.AnaArgs._
import info.kwarc.mmt.api.utils._
import info.kwarc.mmt.stex.STeXUtils._

import scala.sys.process._

class AllPdf extends LaTeXDirTarget {
  val key: String = "allpdf"

  def buildDir(a: Archive, in: FilePath, dir: File, force: Boolean): BuildResult = {
    BuildResult.empty
  }

  override def estimateResult(bt: BuildTask): BuildSuccess = {
    if (bt.isDir) {
      val a = bt.archive
      val ls = getAllFiles(bt).map(f => FileBuildDependency("pdflatex", a, bt.inPath / f))
      BuildSuccess(ls :+ DirBuildDependency("alltex", a, bt.inPath, Nil), Nil)
    } else BuildResult.empty
  }

  override def cleanDir(a: Archive, curr: Current) {
    val dir = curr.file
    if (dir.exists && dir.isDirectory) {
      dir.list.filter(dirFileFilter).sorted.
        foreach { f =>
          val d = FileBuildDependency("pdflatex", a, curr.path / f)
          d.getTarget(controller).clean(a, d.inPath)
        }
    }
    super.cleanDir(a, curr)
  }
}

class AllTeX extends LaTeXDirTarget {
  val key: String = "alltex"

  override def estimateResult(bt: BuildTask): BuildSuccess = {
    if (bt.isDir) {
      BuildSuccess(Nil, getAllFiles(bt).map(f => PhysicalDependency(bt.inFile / f)))
    } else {
      val used = super.estimateResult(bt).used.collect {
        case d@FileBuildDependency(k, _, _) if List("tex-deps", "sms").contains(k) => d
      }
      BuildSuccess(used, Nil)
    }
  }

  def buildDir(a: Archive, in: FilePath, dir: File, force: Boolean): BuildResult = {
    val dirFiles = getDirFiles(a, dir, includeFile)
    var success = false
    if (dirFiles.nonEmpty) {
      createLocalPaths(a, dir)
      val deps = getDepsMap(getFilesRec(a, in))
      val ds = Relational.flatTopsort(controller, deps)
      val ts = ds.collect {
        case bd: FileBuildDependency if List(key, "tex-deps").contains(bd.key) => bd
      }.map(d => d.archive / inDim / d.inPath)
      val files = ts.distinct.filter(dirFiles.map(f => dir / f).contains(_)).map(_.getName)
      assert(files.length == dirFiles.length)
      val langs = files.flatMap(f => getLang(File(f))).toSet
      val nonLangFiles = langFiles(None, files)
      if (nonLangFiles.nonEmpty) success = createAllFile(a, None, dir, nonLangFiles, force)
      langs.toList.sorted.foreach { l =>
        val res = createAllFile(a, Some(l), dir, files, force)
        success ||= res
      }
    }
    if (success) BuildResult.empty
    else BuildEmpty("up-to-date")
  }

  private def ambleText(preOrPost: String, a: Archive, lang: Option[String]): List[String] =
    readSourceRebust(getAmbleFile(preOrPost, a, lang)).getLines().toList

  /** return success */
  private def createAllFile(a: Archive, lang: Option[String], dir: File,
                            files: List[String], force: Boolean): Boolean = {
    val all = dir / ("all" + lang.map("." + _).getOrElse("") + ".tex")
    val ls = langFiles(lang, files)
    val w = new StringBuilder
    def writeln(s: String): Unit = w.append(s + "\n")
    ambleText("pre", a, lang).foreach(writeln)
    writeln("")
    ls.foreach { f =>
      writeln("\\begin{center} \\LARGE File: \\url{" + f + "} \\end{center}")
      writeln("\\input{" + File(f).stripExtension + "} \\newpage")
      writeln("")
    }
    ambleText("post", a, lang).foreach(writeln)
    val newContent = w.result
    val outPath = getOutPath(a, all)
    if (force || !all.exists() || File.read(all) != newContent) {
      File.write(all, newContent)
      logSuccess(outPath)
      true
    } else {
      logResult("up-to-date " + outPath)
      false
    }
  }

  override def cleanDir(a: Archive, curr: Current) {
    val dir = curr.file
    if (dir.exists && dir.isDirectory) {
      dir.list.filter(dirFileFilter).sorted.
        map(f => dir / f).foreach(deleteWithLog)
    }
    super.cleanDir(a, curr)
  }
}

import info.kwarc.mmt.stex.STeXUtils._

/** sms generation */
class SmsGenerator extends LaTeXBuildTarget {
  val key = "sms"
  val outDim: ArchiveDimension = source
  override val outExt = "sms"

  override def includeDir(n: String): Boolean = !n.endsWith("tikz")

  def reallyBuildFile(bt: BuildTask): BuildResult = {
    createLocalPaths(bt)
    try {
      createSms(bt.archive, bt.inFile, bt.outFile)
      logSuccess(bt.outPath)
    }
    catch {
      case e: Exception =>
        bt.errorCont(LocalError("sms exception: " + e))
        logFailure(bt.outPath)
    }
    BuildResult.empty
  }
}

class DepsGenerator extends LaTeXBuildTarget {
  val key = "tex-deps"
  val outDim: ArchiveDimension = RedirectableDimension("tex-deps")
  override val outExt = "deps"

  def reallyBuildFile(bt: BuildTask): BuildResult = {
    logSuccess(bt.outPath)
    BuildEmpty("only create error file as timestamp")
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
    var region : SourceRegion = SourceRegion.none
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

  private val provideMarker = "provides theory: "
  private val missingFileMarker = "missing_file:"
  private val endOfMissingFile = " Can't find"

  private def readLogFile(bt: BuildTask, logFile: File): (List[String], List[String], Boolean) = {
    var providedTheories: List[String] = Nil
    var missingFiles: List[String] = Nil
    var errors = false
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
        val level = newLevel.get
        if (level >= Level.Error) errors = true
        if (level == Level.Info && restLine.startsWith(provideMarker)) {
          providedTheories ::= restLine.substring(provideMarker.length)
        }
        if (level == Level.Error && restLine.startsWith(missingFileMarker)) {
          val endPos = restLine.indexOf(endOfMissingFile)
          missingFiles ::= restLine.substring(missingFileMarker.length, endPos)
        }
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
    (missingFiles.reverse, providedTheories.reverse, errors)
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
      BuildResult.empty
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
      var failure = false
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
          failure = true
          bt.errorCont(LatexError(if (exitCode == 0) "no omdoc created" else "exit code " + exitCode, output.toString))
        }
      } catch {
        case e: Exception =>
          failure = true
          bt.errorCont(LatexError(e.toString, output.toString))
      }
      var providedTheories: List[ResourceDependency] = Nil
      var missingFiles: List[Dependency] = Nil
      if (logFile.exists()) {
        val (mFs, pTs, hasErrs) = readLogFile(bt, logFile)
        failure = failure || hasErrs
        missingFiles = mFs.map(s => PhysicalDependency(File(s)))
        providedTheories = pTs.map(s => LogicalDependency(Path.parseM("https://mathhub.info/" + s, NamespaceMap.empty)))
        if (pipeOutput) File.ReadLineWise(logFile)(println)
      }
      if (pipeOutput) print(output.toString)
      if (failure) {
        logFailure(bt.outPath)
        if (missingFiles.isEmpty) BuildFailure(Nil, providedTheories)
        else MissingDependency(missingFiles, providedTheories,missingFiles)
      } else {
        logSuccess(bt.outPath)
        BuildSuccess(Nil, providedTheories)
      }
    }
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

  override def includeFile(n: String): Boolean =
    n.endsWith(".tex") && !n.endsWith(localpathsFile)

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

  override def estimateResult(bt: BuildTask): BuildSuccess = {
    val bs@BuildSuccess(used, provided) = super.estimateResult(bt)
    if (bt.inPath.name.startsWith("all.")) {
      BuildSuccess(used :+ DirBuildDependency("alltex", bt.archive, bt.inPath.dirPath, Nil), provided)
    } else bs
  }

  protected def runPdflatex(bt: BuildTask, output: StringBuffer): Int = {
    val in = bt.inFile

    // find files that have to be read
    val texFiles = if(noAmble(in)){ List(in)
    } else {
      List(getAmbleFile("pre", bt), in, getAmbleFile("post", bt))
    }

    // create a temporary file to concatinate the inputs
    val tmpFile = bt.inFile.addExtension("tmp")
    File.write(tmpFile, texFiles.map(File.read) :_*)

    // create a process to compile the tex file
    val pb = Process(
      Seq(pdflatexPath, "-jobname", in.stripExtension.getName, "-interaction", "scrollmode", tmpFile.toString),
      in.up, env(bt): _*
    )

    // run the process and delete the temporary file if it was successfull
    val exit = timeout(pb, procLogger(output, pipeOutput = pipeOutput))
    if (exit == 0) tmpFile.delete

    // and write a logFile
    val pdflogFile = in.setExtension("pdflog")
    if (!pipeOutput) File.write(pdflogFile, output.toString)

    // and return
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

class TikzSvg extends PdfLatex
{
  override val key    : String = "tikzsvg"
  override val outExt : String = "svg"
  override val outDim : ArchiveDimension = Dim("content", "images")

  override def includeDir(n: String): Boolean = n.endsWith("tikz")

  override def reallyBuildFile(bt: BuildTask): BuildResult =
  {
    // SVG file is generated content and goes elsewhere.
    val svgFile : File = bt.outFile

    // ToDo: This pdf is ~technically~ also generated content,
    //       suggesting it should be elsewhere. But a bunch
    //       of things assume it's a sibling from the inFile so
    //       its complicated. Link? Copy? Ignore?
    val pdfFile : File = bt.inFile.setExtension("pdf")


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

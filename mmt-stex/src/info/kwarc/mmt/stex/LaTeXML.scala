package info.kwarc.mmt.stex

import java.nio.file.Files

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.archives._
import info.kwarc.mmt.api.documents.Document
import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.parser.{SourcePosition, SourceRef, SourceRegion}
import info.kwarc.mmt.api.utils._

import scala.sys.process.{Process, ProcessLogger}
import scala.util.matching.Regex

class SmsGenerator extends TraversingBuildTarget {
  val localpathsFile = "localpaths.tex"
  val key = "sms"
  val inDim = source
  val outDim: ArchiveDimension = source
  val c = java.io.File.pathSeparator
  override val outExt = "sms"

  case class LatexError(s: String, l: String) extends ExtensionError(key, s) {
    override val extraMessage = l
  }

  def sysEnv(v: String): String = sys.env.getOrElse(v, "")

  def includeFile(n: String): Boolean =
    n.endsWith(".tex") && !n.endsWith(localpathsFile) && !n.startsWith("all.")

  override def includeDir(n: String): Boolean = !n.endsWith("tikz")

  val SmsKeys: List[String] = List(
    "guse", "gadopt", "symdef", "abbrdef", "symvariant", "keydef", "listkeydef",
    "importmodule", "gimport", "adoptmodule", "importmhmodule", "adoptmhmodule"
  )
  val SmsTopKeys: List[String] = List(
    "module", "importmodulevia", "importmhmodulevia"
  )
  val SmsRegs: Regex = (SmsKeys.map("\\\\" + _) ++
    SmsTopKeys.map("\\\\begin\\{" + _ + "\\}") ++
    SmsTopKeys.map("\\\\end\\{" + _ + "\\}")
    ).mkString("|").r

  def createSms(inFile: File, outFile: File): Unit = {
    val source = scala.io.Source.fromFile(inFile)
    val w = File.Writer(outFile)
    source.getLines().foreach { line =>
      val idx = line.indexOf(`%`)
      val l = (if (idx > -1) line.substring(0, idx) else line).trim
      val verbIndex = l.indexOf("\\verb")
      if (verbIndex <= -1 && SmsRegs.findFirstIn(l).isDefined)
        w.println(l + "%")
    }
    w.close()
  }

  def buildFile(bt: BuildTask): Unit = {
    try createSms(bt.inFile, bt.outFile)
    catch {
      case e: Throwable =>
        bt.errorCont(LocalError("sms exception: " + e))
    }
  }

  def mathHubDir(bt: BuildTask): File = bt.archive.baseDir.up

  def extBase(bt: BuildTask): File = mathHubDir(bt) / "ext"

  def stexStyDir(bt: BuildTask): File = extBase(bt) / "sTeX" / "sty"

  def styPath(bt: BuildTask): File = mathHubDir(bt) / "sty"

  def env(bt: BuildTask): List[(String, String)] = {
    val sty = "STEXSTYDIR"
    val tex = "TEXINPUTS"
    val styEnv = sysEnv(sty)
    val styRest = if (styEnv.isEmpty) "" else c + styEnv
    List(
      sty -> (stexStyDir(bt).toString() + styRest),
      tex -> (".//" + c + styPath(bt) + c + stexStyDir(bt) + "//"
        + c + sysEnv(tex)))
  }

  def createLocalPaths(bt: BuildTask): Unit = {
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

  def getAmbleFile(preOrPost: String, bt: BuildTask): File = {
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
}

/** importer wrapper for stex
  */
class LaTeXML extends SmsGenerator {
  override val key = "latexml"
  override val outExt = "omdoc"
  override val outDim = RedirectableDimension("latexml")
  // the latexml client
  private var latexmlc = "latexmlc"
  private var expire = "10"
  private var profile = "stex-smglom-module"
  private var perl5lib = "perl5lib"
  private var preloads: Seq[String] = Nil
  private var paths: Seq[String] = Nil

  override def start(args: List[String]): Unit = {
    latexmlc = getFromFirstArgOrEnvvar(args, "LATEXMLC", latexmlc)
    expire = controller.getEnvVar("LATEXMLEXPIRE").getOrElse(expire)
    profile = controller.getEnvVar("LATEXMLPROFILE").getOrElse(profile)
    preloads = controller.getEnvVar("LATEXMLPRELOADS").getOrElse("").split(" ").filter(_.nonEmpty)
    paths = controller.getEnvVar("LATEXMLPATHS").getOrElse("").split(" ").filter(_.nonEmpty)
  }

  def str2Level(lev: String): Level.Level = lev match {
    case "Info" => Level.Info
    case "Error" => Level.Error
    case "Fatal" => Level.Fatal
    case _ => Level.Warning
  }

  def line2Region(sLine: String, inFile: File): SourceRegion = {
    val regEx = """#textrange\(from=(\d+);(\d+),to=(\d+);(\d+)\)""".r
    val range = sLine.stripPrefix(inFile.toString)
    range match {
      case regEx(l1, c1, l2, c2) =>
        SourceRegion(SourcePosition(-1, l1.toInt, c1.toInt),
          SourcePosition(-1, l2.toInt, c2.toInt))
      case _ => SourceRegion.none
    }
  }

  def line2Level(line: String): (Option[Level.Level], String) = {
    val msgLine = """(Info|Warning|Error|Fatal):(.*)""".r
    line match {
      case msgLine(lev, rest) => (Some(str2Level(lev)), rest)
      case _ => (None, line)
    }
  }

  object LtxLog {
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

  def extEnv(bt: BuildTask): List[(String, String)] = {
    val perl5 = extBase(bt) / perl5lib
    val p5 = "PERL5LIB"
    val perl5path = perl5 / "lib" / "perl5"
    val latexmlBlib = extBase(bt) / "LaTeXML" / "blib" / "lib"
    val path = if (c == ":") "PATH" else "Path"
    (path -> (perl5 / "bin" + c + sysEnv(path))) ::
      (p5 -> (perl5path + c + latexmlBlib + c + sysEnv(p5))) :: env(bt)
  }

  def setLatexmlc(bt: BuildTask): Unit =
    if (!File(latexmlc).isAbsolute) {
      val latexmlcpath = extBase(bt) / perl5lib / "bin" / latexmlc
      if (latexmlcpath.exists) {
        latexmlc = latexmlcpath.toString
        log("executing " + latexmlc)
      }
    }

  /**
   * Compile a .tex file to OMDoc
   */
  override def buildFile(bt: BuildTask): Unit = {
    val lmhOut = bt.inFile.setExtension("omdoc")
    val logFile = bt.inFile.setExtension("ltxlog")
    val logOutFile = bt.outFile.setExtension("ltxlog")
    lmhOut.delete()
    logFile.delete()
    logOutFile.delete()
    bt.outFile.delete()
    createLocalPaths(bt)
    setLatexmlc(bt)
    val output = new StringBuffer()
    val pb = Process(Seq(latexmlc,
      "--quiet", "--profile", profile, "--path=" + styPath(bt),
      bt.inFile.toString, "--destination=" + lmhOut, "--log=" + logFile,
      "--preamble=" + getAmbleFile("pre", bt),
      "--postamble=" + getAmbleFile("post", bt),
      "--expire=" + expire) ++ preloads.map("--preload=" + _) ++
      paths.map("--path=" + _), bt.archive / inDim, extEnv(bt): _*)
    val exitCode = pb.!(ProcessLogger(line => output.append(line + "\n"),
      line => output.append(line + "\n")))
    if (exitCode != 0 || lmhOut.length == 0) {
      bt.errorCont(LatexError("no omdoc created", output.toString))
      lmhOut.delete()
    }
    if (lmhOut.exists() && lmhOut != bt.outFile)
      Files.move(lmhOut.toPath, bt.outFile.toPath)
    if (logFile.exists()) {
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
      if (logFile != logOutFile) Files.move(logFile.toPath, logOutFile.toPath)
    }
  }
}

class PdfLatex extends SmsGenerator {
  override val key = "pdflatex"
  override val outExt = "pdf"
  override val outDim = Dim("export", key, inDim.toString)
  private var pdflatexPath: String = "xelatex"

  override def start(args: List[String]): Unit = {
    pdflatexPath = getFromFirstArgOrEnvvar(args, "PDFLATEX", pdflatexPath)
  }

  override def buildFile(bt: BuildTask): Unit = {
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
      pb.!(ProcessLogger(line => output.append(line + "\n"),
        line => output.append(line + "\n")))
      if (pdfFile.length == 0) {
        bt.errorCont(LatexError("no pdf created", output.toString))
        pdfFile.delete()
      }
      if (pdfFile.exists && pdfFile != bt.outFile)
        Files.move(pdfFile.toPath, bt.outFile.toPath)
    } catch {
      case e: Throwable =>
        bt.outFile.delete()
        bt.errorCont(LatexError(e.toString, output.toString))
    }
    List("aux", "idx", "log", "out", "thm", "pdf").
      foreach(bt.inFile.setExtension(_).delete())
  }
}

class LaTeXMLAndSTeX extends Importer {
  private val latexmlBuilder = new LaTeXML
  private val stexImporter = new STeXImporter {
    override val inDim = RedirectableDimension("latexml")
  }
  val key = latexmlBuilder.key + "-" + stexImporter.key

  /** the (unused) file extensions to which this may be applicable */
  override def inExts: List[String] = List("tex")

  override def includeFile(n: String): Boolean = latexmlBuilder.includeFile(n)

  override def includeDir(n: String): Boolean = latexmlBuilder.includeDir(n)

  override def init(controller: Controller): Unit = {
    latexmlBuilder.init(controller)
    stexImporter.init(controller)
    super.init(controller)
  }

  override def start(args: List[String]): Unit = {
    latexmlBuilder.start(args)
    stexImporter.start(args)
    super.start(args)
  }

  /** the main abstract method to be implemented by importers
    *
    * @param bt information about the input document and error reporting
    * @param index a continuation function to be called on every generated document
    */
  def importDocument(bt: BuildTask, index: (Document) => Unit): Unit = {
    // no code is needed here since apply is overridden below
  }

  override def apply(modifier: BuildTargetModifier, arch: Archive, in: FilePath): Unit = {
    modifier match {
      case up: Update =>
        latexmlBuilder.update(arch, up, in)
        stexImporter.update(arch, up, in)
      case Clean =>
        latexmlBuilder.clean(arch, in)
        stexImporter.clean(arch, in)
      case Build =>
        latexmlBuilder.build(arch, in)
        //running twice, first to load all theories, then to successfully parse objects
        stexImporter.build(arch, in)
        stexImporter.build(arch, in)
    }
  }
}

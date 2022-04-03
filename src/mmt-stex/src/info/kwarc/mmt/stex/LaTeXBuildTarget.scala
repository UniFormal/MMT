package info.kwarc.mmt.stex

import info.kwarc.mmt.api.Level.Level
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.archives._
import info.kwarc.mmt.api.documents.{DRef, Document, FolderLevel, MRef}
import info.kwarc.mmt.api.parser.{ParsingStream, ParsingUnit, SourcePosition, SourceRef, SourceRegion}
import info.kwarc.mmt.api.utils.AnaArgs.OptionDescrs
import info.kwarc.mmt.api.utils.{EmptyPath, File, FilePath, IntArg, NoArg, OptionDescr, StringArg, URI}
import info.kwarc.mmt.stex.xhtml.{HTMLParser, SemanticState}
import info.kwarc.rustex.Params

import scala.xml.parsing.XhtmlParser

object TeXError {
  def apply(uri:String,msg:String,stacktrace:List[(String,String)],reg:SourceRegion) =
    info.kwarc.mmt.api.SourceError(msg,
      SourceRef(URI(uri),reg),msg,stacktrace.map{case (a,b) => a + " - " + b}
    )
}


object RusTeX {
  import info.kwarc.rustex.Bridge
  private val github_rustex_prefix = "https://github.com/slatex/RusTeX/releases/download/latest/"

  def initializeBridge(f : => File): Unit = this.synchronized {
    if (!Bridge.initialized()) {
      val path = f
      val file = path / Bridge.library_filename()
      if (!file.exists()) {
        File.download(URI(github_rustex_prefix + Bridge.library_filename()),file)
      }
      Bridge.initialize(path.toString)
    }
  }
  def parse(f : File,p:Params,memories:List[String] = Nil) = {
    if (Bridge.initialized()) Bridge.parse(f.toString,p,memories) else ""
  }

}

trait XHTMLParser extends TraversingBuildTarget {

  var stexserver : STeXServer = null

  override def start(args: List[String]): Unit = try {
    super.start(args)
    //LaTeXML.initializeIfNecessary(controller)
    controller.extman.get(classOf[STeXServer]) match {
      case Nil =>
        stexserver = new STeXServer
        controller.extman.addExtension(stexserver)
      case a :: _ =>
        stexserver = a
    }
  } catch {
    case t : Throwable =>
      throw t
  }

  def buildFileActually(inFile : File,outFile : File ,state : HTMLParser.ParsingState,errorCont : ErrorHandler) = {
    RusTeX.initializeBridge{
      sys.env.get("MATHHUB") match {
        case Some(v) =>
          File(v) / ".rustex"
        case _ => ???
      }
    } // c_stex_module_
    log("building " + inFile)
    val self = this
    val params = new Params {
      private var files: List[String] = Nil
      override def log(s: String): Unit = self.log(s,Some("rustex-log"))
      override def write_16(s: String): Unit = self.log(s,Some("rustex-16"))
      override def write_17(s: String): Unit = self.log(s,Some("rustex-17"))
      override def write_18(s: String): Unit = self.log(s,Some("rustex-18"))
      override def write_neg_1(s: String): Unit = self.log(s,Some("rustex-neg1"))
      override def write_other(s: String): Unit = self.log(s,Some("rustex-other"))
      override def message(s: String): Unit = self.log(s,Some("rustex-msg"))
      def file_open(s: String): Unit = {
        files ::= s
        self.log(s,Some("rustex-file"))
        if (s.contains("MathHub/smglom") && s.contains(".en.tex")) {
          println(s.trim)
        }
      }
      def file_close(): Unit = if (files.isEmpty) {
        print("")
      } else {
        self.log(files.head,Some("rustex-file-close"))
        files = files.tail
      }
      def error(msg : String, stacktrace : List[(String, String)],files:List[(String,Int,Int)]) : Unit = {
        val region = SourceRegion(SourcePosition(0,files.head._2,files.head._3),SourcePosition(0,files.head._2,files.head._3))
        errorCont(TeXError(inFile.toURI.toString,msg,stacktrace,region))
      }
    }
    val html = RusTeX.parse(inFile,params,List("c_stex_module_"))
    File.write(outFile.setExtension("shtml"),html)
    val doc = try {
      HTMLParser(outFile.setExtension("shtml"))(state)
    } catch {
      case e =>
        println(e)
        ???
    }
    doc.get("head")()().head.children.foreach(_.delete)
    outFile.setExtension("shtml").delete()
    File.write(outFile, doc.toString)
    doc
  }

}

class LaTeXToHTML extends XHTMLParser {
  val key = "stex-xhtml"
  val outDim = Dim("xhtml")
  val inDim = info.kwarc.mmt.api.archives.source
  def includeFile(name: String): Boolean = name.endsWith(".tex") && !name.startsWith("all.")

  override def buildFile(bf: BuildTask): BuildResult = {
    val extensions = stexserver.extensions
    val rules = extensions.flatMap(_.rules)
    //val state = new XHTMLParsingState(xhtmlrules,bf.errorCont)
    val state = new HTMLParser.ParsingState(controller,rules)
    val doc = buildFileActually(bf.inFile,bf.outFile,state,bf.errorCont)
    log("Finished: " + bf.inFile)
    BuildResult.empty
  }
}

class HTMLToOMDoc extends Importer with XHTMLParser {
  val key = "xhtml-omdoc"
  val inExts = List("xhtml")
  override val inDim = Dim("xhtml")

  override def importDocument(bt: BuildTask, index: Document => Unit): BuildResult = {
    log("postprocessing " + bt.inFile)
    val dpath = Path.parseD(bt.narrationDPath.toString.split('.').init.mkString(".") + ".omdoc",NamespaceMap.empty)
    val extensions = stexserver.extensions
    val rules = extensions.flatMap(_.rules)
    val state = new SemanticState(controller, rules, bt.errorCont, dpath)
    HTMLParser(bt.inFile)(state)
    index(state.doc)
    log("Finished: " + bt.inFile)
    val results = state.doc.getDeclarations.collect {
      case mr:MRef =>
        LogicalDependency(mr.target)
    }
    state.missings match {
      case Nil => BuildSuccess(Nil,results)
      case o => MissingDependency(o.map(LogicalDependency),results,Nil)
    }
  }
}

class STeXToOMDoc extends Importer with XHTMLParser {
  val key = "stex-omdoc"
  override val inDim = info.kwarc.mmt.api.archives.source
  val inExts = List("tex")
  override def includeFile(name: String): Boolean = name.endsWith(".tex") && !name.startsWith("all.")
  override def importDocument(bt: BuildTask, index: Document => Unit): BuildResult = {
    val extensions = stexserver.extensions
    val rules = extensions.flatMap(_.rules)
    val dpath = Path.parseD(bt.narrationDPath.toString.split('.').init.mkString(".") + ".omdoc",NamespaceMap.empty)
    val outFile : File = (bt.archive / Dim("xhtml") / bt.inPath).setExtension("xhtml")
    val state = new SemanticState(controller,rules,bt.errorCont,dpath)
    outFile.up.mkdirs()
    buildFileActually(bt.inFile, outFile, state, bt.errorCont)
    log("postprocessing " + bt.inFile)
    index(state.doc)
    log("Finished: " + bt.inFile)
    val results = state.doc.getDeclarations.collect {
      case mr:MRef =>
        LogicalDependency(mr.target)
    }
    state.missings match {
      case Nil => BuildSuccess(Nil,results)
      case o => MissingDependency(o.map(LogicalDependency),results,Nil)
    }
  }

}

/*
import STeXUtils._
import java.util.regex.PatternSyntaxException
import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.sys.process.{ProcessBuilder,ProcessLogger}

/** common code for sms, latexml und pdf generation */
abstract class LaTeXBuildTarget extends TraversingBuildTarget with STeXAnalysis
{
  val localpathsFile   : String = "localpaths.tex"
  val inDim  : ArchiveDimension = source
  var pipeOutput      : Boolean = false
  val pipeOutputOption : String = "pipe-worker-output"

  /** timout in seconds */
  private val timeoutDefault        : Int = 600
  protected var timeoutVal          : Int = timeoutDefault
  protected val timeoutOption    : String = "timeout"
  protected var nameOfExecutable : String = ""

  protected case class LatexError(s: String, l: String) extends ExtensionError(key, s) {
    override val extraMessage : String = l
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
    pipeOutput = optionsMap.contains(pipeOutputOption)
    optionsMap.get(timeoutOption).foreach(v => timeoutVal = v.getIntVal)
    optionsMap.get(key).foreach(v => nameOfExecutable = v.getStringVal)
    optionsMap.get("execute").foreach { v =>
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

  @deprecated("sTeX no longer relies on localpaths.tex")
  protected def createLocalPaths(bt: BuildTask) {
    createLocalPaths(bt.archive, bt.inFile.up)
  }

  @deprecated("sTeX no longer relies on localpaths.tex")
  protected def createLocalPaths(a: Archive, dir: File) {
    val fileName = dir / localpathsFile
    val groupRepo = archString(a) + "}"
    val text: List[String] = List(
      "% this file defines root path local repository",
      "\\defpath{MathHub}{" + a.root.up.up.getPath + "}",
      "\\mhcurrentrepos{" + groupRepo,
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

  def buildFile(bt: BuildTask): BuildResult = {
    if (!skip(bt)) { reallyBuildFile(bt) } else { BuildEmpty("file excluded by MANIFEST") }
  }

  protected def readingSource(a: Archive, in: File, amble: Option[File] = None): List[Dependency] = {
    val res = getDeps(a, in, Set(in), amble)
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

  override def estimateResult(bt: BuildTask): BuildSuccess = {
    val in = bt.inFile
    val a = bt.archive
    val ds = if (in.exists && in.isFile) {
      readingSource(a, in) ++
        (if (noAmble(in) || key == "sms") Nil
        else {
          val pre = getAmbleFile("pre", bt)
          val post = getAmbleFile("post", bt)
          List(pre, post).map(PhysicalDependency) ++ readingSource(a, in, Some(pre)) ++ readingSource(a, in, Some(post))
        })
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
      Await.result(fut,Duration(timeoutVal,duration.SECONDS))
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

  override def getFolderOutFile(a: Archive, inPath: FilePath) : File = a / outDim / inPath

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

  /** do not create error files in all cases */
  override def runBuildTask(bt: BuildTask, level: Level): BuildResult = if (bt.isDir) {
    var res: BuildResult = BuildResult.empty
    var cont = bt.errorCont
    try {
      res = buildDir(bt, Nil, level)
      res match {
        case BuildSuccess(_, _) =>
          cont.open
          cont.close
        case _ => // assume no failures in buildDir
      }
    } catch {
      case e: Exception =>
        cont.open
        val le = LocalError("unknown build error: " + e.getMessage).setCausedBy(e)
        cont(le)
        cont.close
        res = BuildFailure(Nil, Nil)
    }
    res
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
      _ =>
    }, {
      case (c@Current(inDir, inPath), _) =>
        buildDir(a, inPath, inDir, force = false)
    })
  }

  override def buildDir(bt: BuildTask, builtChildren: List[BuildTask], level: Level): BuildResult =
    buildDir(bt.archive, bt.inPath, bt.inFile, force = level <= Level.Force)

  def buildDir(a: Archive, in: FilePath, dir: File, force: Boolean): BuildResult
}

 */

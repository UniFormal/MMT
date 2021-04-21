package info.kwarc.mmt.stex

import info.kwarc.mmt.api.Level.Level
import info.kwarc.mmt.api.{ContainerElement, DPath, ErrorHandler, ExtensionError, GetError, LNStep, Level, LocalName, MMTTask, StructuralElement}
import info.kwarc.mmt.api.archives.{Archive, ArchiveDimension, BuildEmpty, BuildFailure, BuildResult, BuildSuccess, BuildTargetArguments, BuildTask, Current, Dependency, Dim, FileBuildDependency, Importer, PhysicalDependency, TraverseMode, TraversingBuildTarget, Update, `export`, source}
import info.kwarc.mmt.api.checking.{CheckingEnvironment, CheckingResult, ExtendedCheckingEnvironment, Interpreter, MMTStructureChecker, RelationHandler, Solver}
import info.kwarc.mmt.api.documents.{DRef, Document, FolderLevel, MRef}
import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.metadata.MetaDatum
import info.kwarc.mmt.api.modules.{Link, Module, Theory}
import info.kwarc.mmt.api.objects.{Context, OMS, StatelessTraverser, Term, Traverser}
import info.kwarc.mmt.api.parser.{ParsingStream, ParsingUnit, SourcePosition, SourceRef, SourceRegion}
import info.kwarc.mmt.api.utils.AnaArgs.OptionDescrs
import info.kwarc.mmt.api.utils.FilePath.filePathToList
import info.kwarc.mmt.api.utils.{EmptyPath, File, FilePath, IntArg, NoArg, OptionDescr, StringArg, URI}
import info.kwarc.mmt.stex.Extensions.STeXExtension
import info.kwarc.mmt.stex.xhtml.{ConstantAnnotation, DeclarationAnnotation, ModuleAnnotation, PreElement, PreParent, PreTheory, SourceRefAnnotation, StructureAnnotation, TheoryAnnotation, XHTML, XHTMLAnnotation, XHTMLNode, XHTMLParsingState}
import info.kwarc.mmt.api.symbols.Constant

trait XHTMLParser extends TraversingBuildTarget {

  var stexserver : STeXServer = null

  override def start(args: List[String]): Unit = try {
    super.start(args)
    LaTeXML.initializeIfNecessary(controller)
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

  def buildFileActually(bf: BuildTask, state : XHTMLParsingState) = {
    log("building " + bf.inFile)
    LaTeXML.latexmlc(bf.inFile,bf.outFile,Some(s => log(s,Some(bf.inFile.toString))),Some(s => log(s,Some(bf.inFile.toString)))).foreach {
      case (i,ls) if i > Level.Warning =>
        bf.errorCont(new STeXError("LaTeXML: " + ls.head,Some(ls.tail.mkString("\n")),Some(i)))
      case _ =>
    }
    if (!bf.outFile.exists()) throw new STeXError("LaTeXML failed: No .xhtml generated",None,Some(Level.Error))
    log("postprocessing " + bf.inFile)
    val doc = XHTML.parse(bf.outFile,Some(state))
    doc.get("div")()("ltx_page_logo").foreach(_.delete)
    doc.get("div")()("ltx_page_footer").foreach(f => if (f.isEmpty) f.delete)
    //val (mmtdoc,missing) = PreElement.extract(doc)(controller)
    File.write(bf.outFile, doc.toString)
    bf.outFile.up.children.foreach {
      case f if f.getExtension.contains("css") => f.delete()
      case _ =>
    }
    log("Finished: " + bf.inFile)
    //(mmtdoc,missing)
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
    val xhtmlrules = extensions.flatMap(_.xhtmlRules)
    val state = new XHTMLParsingState(xhtmlrules,bf.errorCont)
    buildFileActually(bf,state)
    BuildResult.empty
  }
}


class PreDocument(val path : DPath) extends PreParent {
  var level : Int = 0
  // TODO Sections
  val document = new Document(path)
  override def getElement(implicit state : SemanticParsingState) : List[Document] = List(document)
}

class SemanticParsingState(rules : List[PartialFunction[(XHTMLNode,XHTMLParsingState), Unit]],eh : ErrorHandler, val dpath : DPath, controller : Controller)
  extends XHTMLParsingState(rules,eh) {
  val maindoc = new PreDocument(dpath)
  private var _transforms: List[PartialFunction[Term, Term]] = Nil
  private var _setransforms: List[PartialFunction[(StructuralElement,SemanticParsingState), StructuralElement]] = Nil

  def addTransform(pf: PartialFunction[Term, Term]) = _transforms ::= pf
  def addTransformSE(pf: PartialFunction[(StructuralElement,SemanticParsingState), StructuralElement]) = _setransforms ::= pf

  private val traverser = new StatelessTraverser {
    override def traverse(t: Term)(implicit con: Context, state: State): Term = {
      val ret = _transforms.foldLeft(t)((it, f) => f.unapply(it).getOrElse(it))
      Traverser(this, ret)
    }
  }

  def applyTerm(tm: Term): Term = traverser(tm, ())

  lazy val checker = controller.extman.get(classOf[MMTStructureChecker]).head

  lazy val ce = new CheckingEnvironment(controller.simplifier, eh, RelationHandler.ignore,new MMTTask {})

  def checkDefault[A <: StructuralElement](se : A) : A = {
    se match {
      case c : Constant =>
        controller add c
        (c.tp,c.df) match {
          case (None,None) => return se
          case (None,Some(OMS(_))) => return se
          case _ =>
        }
        checker.apply(c)(ce)
        se
      case _ =>
        controller add se
        checker.apply(se)(ce)
        se
    }
  }

  def applySE[A <: StructuralElement](se: A, pe : PreElement) = {
    pe.metadata.toList.foreach(p => se.metadata.add(MetaDatum(p._1, p._2)))
    pe match {
      case a : XHTMLAnnotation =>
        a.node.getAnnotations.collectFirst{
          case sr : SourceRefAnnotation =>
            SourceRef.update(se,SourceRefAnnotation.toSourceRef(controller,sr))
        }
      case _ =>
    }
    _setransforms.collectFirst { case r if r.isDefinedAt(se,this) => r(se,this) }.getOrElse(checkDefault(se)).asInstanceOf[A]
  }

  def build = {
    val doc = new Document(dpath)
    controller add doc
    maindoc.children.foreach {
      case t : TheoryAnnotation =>
        t.subelements.foreach{t =>
          buildTheory(doc,t)
        }
      case _ =>
    }
    doc
  }
  private def buildTheory(parent : Document,th : PreTheory) = th.getElement(this).foreach { tI =>
    val t = applySE(tI, th)
    controller.getO(th.path.parent) match {
      case Some(d: Document) =>
      case _ => controller.add(new Document(th.path.parent))
    }
    controller add MRef(parent.path, t.path)
    th.children.foreach {
      case d: DeclarationAnnotation =>
        d.subelements.foreach(buildDeclaration(t, _))
      case _ =>
    }
    val ce = new CheckingEnvironment(controller.simplifier, eh, RelationHandler.ignore,new MMTTask {})
    checker.applyElementEnd(t)(ce)
  }
  private def buildDeclaration(parent : Module, da : DeclarationAnnotation) = da.getElement(this).foreach{dI =>
    val d = applySE(dI,da)
    d match {
      case l : Link if l.isImplicit =>
        controller.library.endAdd(l)
      case _ =>
    }
  }

  private var elems: List[PreElement] = List(maindoc)

  def getParent = elems.head

  override def open(node: XHTMLNode): Unit = {
    super.open(node)
    node.getAnnotations.collect {
      case e: PreElement =>
        e.open(this)
        elems ::= e
    }
  }

  override def close(node: XHTMLNode): Unit = {
    super.close(node)
    node.getAnnotations.reverse.collect {
      case e : PreElement =>
        elems.headOption match {
          case Some(`e`) =>
            elems = elems.tail
            e.close(this)
          case _ =>
            print("")
            ???
        }
    }
  }

}

class HTMLToOMDoc extends Importer with XHTMLParser {
  val key = "xhtml-omdoc"
  val inExts = List("xhtml")
  override val inDim = Dim("xhtml")

  override def importDocument(bt: BuildTask, index: Document => Unit): BuildResult = {
    log("postprocessing " + bt.inFile)
    val dpath = bt.narrationDPath // / LocalName(filePathToList(bt.inPath.setExtension("omdoc")):_*)
    val extensions = stexserver.extensions
    val xhtmlrules = extensions.flatMap(_.xhtmlRules)
    val trules = extensions.flatMap(_.checkingRules)
    implicit val state = new SemanticParsingState(xhtmlrules,bt.errorCont,dpath,controller)
    trules.foreach(state.addTransformSE)
    XHTML.parse(bt.inFile,Some(state))
    //index(state.maindoc.getElement)
    val doc = state.build
    index(doc)
    log("Finished: " + bt.inFile)
    BuildResult.empty
  }
}

/*
class LaTeXToHTML extends Importer {
  def format = "stex"
  override val inExts = List("tex")
  override val outDim = Dim("xhtml")
  override val outExt = "xhtml"
  private var stexserver : STeXServer = null

  def apply(pu: ParsingUnit)(implicit errorCont: ErrorHandler): CheckingResult = {
    ???
  }
  def apply(ps: ParsingStream)(implicit errorCont: ErrorHandler): StructuralElement = {
    ???
  }

  override def includeFile(name: String): Boolean = name.endsWith(".tex") && !name.startsWith("all.")

  override def start(args: List[String]): Unit = try {
    super.start(args)
    LaTeXML.initializeIfNecessary(controller)
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

  override def importDocument(bt: BuildTask, index: Document => Unit): BuildResult = {
    val (doc,missing) = buildFileActually(bt)
    missing.foreach(m => bt.errorCont(GetError("no backend applicable to " + m)))
    //index(doc)

    BuildResult.empty
  }
/*
  override def buildFile(bf: BuildTask): BuildResult = {
    val (html,extensions) = buildFileActually(bf)
    val doc = PreElement.extract(html,extensions)(controller)
    var dl = doc
    while (dl.path.^.uri != bf.base) {
      val nd = controller.getO(dl.path.^) match {
        case Some(d : Document) =>
          d
        case _ =>
          val ndi = new Document(dl.path.^,FolderLevel)
          controller add ndi
          ndi
      }
      controller add DRef(nd.path,dl.path)
      dl = nd
    }
    controller.getO(dl.path.^) match {
      case Some(d : Document) =>
        controller add DRef(d.path,dl.path)
      case _ =>
        val ndi = new Document(dl.path.^,FolderLevel)
        controller add ndi
        controller add DRef(ndi.path,dl.path)
    }
    BuildResult.empty
  }

 */

  def buildFileActually(bf: BuildTask) = {
    val extensions = stexserver.extensions
    val xhtmlrules = extensions.flatMap(_.xhtmlRules)
    val state = new XHTMLParsingState(xhtmlrules)
    log("building " + bf.inFile)
    LaTeXML.latexmlc(bf.inFile,bf.outFile,Some(s => log(s,Some(bf.inFile.toString))),Some(s => log(s,Some(bf.inFile.toString)))).foreach {
      case (i,ls) if i > Level.Warning =>
        bf.errorCont(new STeXError("LaTeXML: " + ls.head,Some(ls.tail.mkString("\n")),Some(i)))
      case _ =>
    }
    if (!bf.outFile.exists()) throw new STeXError("LaTeXML failed: No .xhtml generated",None,Some(Level.Error))
    log("postprocessing " + bf.inFile)
    val doc = XHTML.parse(bf.outFile,Some(state))
    doc.get("div")(("", "class", "ltx_page_logo")).foreach(_.delete)
    doc.get("div")(("", "class", "ltx_page_footer")).foreach(f => if (f.isEmpty) f.delete)
    //val (mmtdoc,missing) = PreElement.extract(doc)(controller)
    File.write(bf.outFile, doc.toString)
    log("Finished: " + bf.inFile)
    //(mmtdoc,missing)
    (doc,Nil)
  }
}

 */

import STeXUtils._
import java.util.regex.PatternSyntaxException
import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.sys.process.{ProcessBuilder,ProcessLogger}

/** common code for sms, latexml und pdf generation */
abstract class LaTeXBuildTarget extends TraversingBuildTarget with STeXAnalysis with BuildTargetArguments
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

package info.kwarc.mmt.stex

import info.kwarc.mmt.api.Level.Level
import info.kwarc.mmt.api.{MultipleErrorHandler, _}
import info.kwarc.mmt.api.archives._
import info.kwarc.mmt.api.documents.{DRef, Document, FolderLevel, MRef}
import info.kwarc.mmt.api.modules.AbstractTheory
import info.kwarc.mmt.api.objects.OMPMOD
import info.kwarc.mmt.api.parser.{ParsingStream, ParsingUnit, SourcePosition, SourceRef, SourceRegion}
import info.kwarc.mmt.api.utils.AnaArgs.OptionDescrs
import info.kwarc.mmt.api.utils.{EmptyPath, File, FilePath, IntArg, NoArg, OptionDescr, StringArg, URI}
import info.kwarc.mmt.stex.lsp.STeXLSPErrorHandler
import info.kwarc.mmt.stex.xhtml.{HTMLParser, SearchOnlyState, SemanticState, SimpleHTMLRule}
import info.kwarc.rustex.Params

import scala.annotation.tailrec
import scala.collection.mutable
import scala.sys.process.Process
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
      /*val file = path / Bridge.library_filename()
      if (!file.exists()) {
        File.download(URI(github_rustex_prefix + Bridge.library_filename()),file)
      }*/
      Bridge.initialize(path.toString)
    }
  }
  def parse(f : File,p:Params,memories:List[String] = Nil) = {
    if (Bridge.initialized()) this.synchronized { Bridge.parse(f.toString,p,memories) } else ""
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
        case _ => sys.env.get("HOME") match {
          case Some(f) if (File(f) / ".stex" / "mathhub.path").exists() =>
            val tf = File(f) / ".stex" / "mathhub.path"
            val nf = File(File.read(tf).trim)
            if (nf.exists()) nf else ???
          case _ => ???
        }
      }
    } // c_stex_module_
    var errored = false
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
        errored = true
        val region = SourceRegion(SourcePosition(0,files.head._2,files.head._3),SourcePosition(0,files.head._2,files.head._3))
        errorCont(TeXError(inFile.toURI.toString,msg,stacktrace,region))
      }
    }
    val html = RusTeX.parse(inFile,params,List("c_stex_module_"))
    File.write(outFile.setExtension("shtml"),html)
    val doc = try { controller.library.synchronized {
      HTMLParser(outFile.setExtension("shtml"))(state)
    }} catch {
      case e =>
        e.printStackTrace()
        throw e
    }
    doc.get("head")()().head.children.foreach(_.delete)
    outFile.setExtension("shtml").delete()
    File.write(outFile, doc.toString)
    (errored,doc)
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
    val (errored,doc) = buildFileActually(bf.inFile,bf.outFile,state,bf.errorCont)
    log("Finished: " + bf.inFile)
    if (errored) BuildFailure(Nil,List(PhysicalDependency(bf.outFile)))
    else BuildSuccess(Nil,List(PhysicalDependency(bf.outFile)))
  }
}

class HTMLToOMDoc extends Importer with XHTMLParser {
  val key = "xhtml-omdoc"
  val inExts = List("xhtml")
  //override val inDim = Dim("xhtml")
  override val inDim = info.kwarc.mmt.api.archives.source

  override def importDocument(bt: BuildTask, index: Document => Unit): BuildResult = {
    log("postprocessing " + bt.inFile)
    val dpath = Path.parseD(bt.narrationDPath.toString.split('.').init.mkString(".") + ".omdoc",NamespaceMap.empty)
    val inFile = bt.archive / RedirectableDimension("xhtml") / bt.inPath.setExtension("xhtml")
    if (!inFile.exists) return {
      bt.errorCont(SourceError(bt.inFile.toString,SourceRef.anonymous(""),"xhtml file " + inFile.toString + " does not exist"))
      BuildFailure(Nil,Nil)
    }
    val extensions = stexserver.extensions
    val rules = extensions.flatMap(_.rules)
    val state = new SemanticState(controller, rules, bt.errorCont, dpath)
    controller.library.synchronized{HTMLParser(inFile)(state)}
    index(state.doc)
    log("Finished: " + inFile)
    val results = DocumentDependency(state.doc.path) :: state.doc.getDeclarations.collect {
      case mr:MRef =>
        LogicalDependency(mr.target)
    }
    val used = state.doc.getDeclarations.flatMap {
      case m : MRef => controller.getO(m.target).toList.flatMap{
        case t : AbstractTheory => t.getAllIncludes.map(m => LogicalDependency(m.from)) ::: t.getNamedStructures.map(s => LogicalDependency(s.from match {case OMPMOD(p,_) => p}))
        case _ => Nil
      }
      case d: DRef if d.getOrigin == GeneratedDRef => List(DocumentDependency(d.target))
      case _ => Nil
    }.filterNot(results.contains)
    state.missings match {
      case Nil => BuildSuccess(used,results)
      case o => MissingDependency(o.map(LogicalDependency),results,used)
    }
  }
}

class HTMLToLucene extends XHTMLParser {
  val key = "xhtml-lucene"
  override val outDim: ArchiveDimension = Dim("export", "lucene")
  val inDim = info.kwarc.mmt.api.archives.source
  def includeFile(name: String): Boolean = name.endsWith(".tex") && !name.startsWith("all.")

  /** the main abstract method for building one file
    *
    * @param bf information about input/output file etc
    */
  override def buildFile(bt: BuildTask): BuildResult = {
    val dpath = Path.parseD(bt.narrationDPath.toString.split('.').init.mkString(".") + ".omdoc",NamespaceMap.empty)
    val inFile = bt.archive / RedirectableDimension("xhtml") / bt.inPath.setExtension("xhtml")
    if (!inFile.exists) return {
      bt.errorCont(SourceError(bt.inFile.toString,SourceRef.anonymous(""),"xhtml file " + inFile.toString + " does not exist"))
      BuildFailure(Nil,Nil)
    }

    val extensions = stexserver.extensions
    val rules = extensions.flatMap(_.rules)
    val state = new SearchOnlyState(controller,rules,bt.errorCont,dpath)
    controller.library.synchronized{HTMLParser(inFile)(state)}
    val doc = state.Search.makeDocument(bt.outFile.stripExtension,bt.inFile,bt.archive)

    doc.save
    BuildResult.empty
  }
}

class STeXToOMDoc extends Importer with XHTMLParser {
  override def onBlock(qt : QueuedTask,br:BuildResult) = {
    val bt = controller.extman.getOrAddExtension(classOf[HTMLToOMDoc],"xhtml-omdoc").getOrElse(this)
    qt.copy(bt,br)
  }
  val key = "stex-omdoc"
  override val inDim = info.kwarc.mmt.api.archives.source
  val inExts = List("tex")
  override def includeFile(name: String): Boolean = name.endsWith(".tex") && !name.startsWith("all.")
  override def importDocument(bt: BuildTask, index: Document => Unit): BuildResult = {
    val extensions = stexserver.extensions
    val rules = extensions.flatMap(_.rules)
    val dpath = Path.parseD(bt.narrationDPath.toString.split('.').init.mkString(".") + ".omdoc",NamespaceMap.empty)
    val outFile : File = (bt.archive / RedirectableDimension("xhtml") / bt.inPath).setExtension("xhtml")
    val state = new SemanticState(controller,rules,bt.errorCont,dpath)
    outFile.up.mkdirs()
    val (errored,_) = buildFileActually(bt.inFile, outFile, state, bt.errorCont)
    log("postprocessing " + bt.inFile)
    index(state.doc)
    log("Finished: " + bt.inFile)
    val results = PhysicalDependency(outFile) ::  DocumentDependency(state.doc.path) :: state.doc.getDeclarations.collect {
      case mr:MRef =>
        LogicalDependency(mr.target)
    }
    val used = state.doc.getDeclarations.flatMap {
      case m : MRef => controller.getO(m.target).toList.flatMap{
        case t : AbstractTheory => t.getAllIncludes.map(m => LogicalDependency(m.from)) ::: t.getNamedStructures.map(s => LogicalDependency(s.from match {case OMPMOD(p,_) => p}))
        case _ => Nil
      }
      case d: DRef if d.getOrigin == GeneratedDRef => List(DocumentDependency(d.target))
      case _ =>  Nil
    }.filterNot(results.contains)
    if (errored) BuildFailure(used,results) else state.missings match {
      case Nil => BuildSuccess(used,results)
      case o => MissingDependency(o.map(LogicalDependency),results,used)
    }
  }
}

object PdfLatex {
  def clear(pdffile : File): Unit = {
    val supportfiles = List(
      pdffile.setExtension("aux"),
      pdffile.setExtension("log"),
      pdffile.setExtension("bbl"),
      pdffile.setExtension("toc"),
      pdffile.setExtension("upa"),
      pdffile.setExtension("upb"),
      pdffile.setExtension("blg"),
      pdffile.setExtension("out"),
      pdffile.setExtension("idx"),
      pdffile.setExtension("mw"),
      pdffile.setExtension("nav"),
      pdffile.setExtension("snm"),
      pdffile.setExtension("vrb"),
      pdffile.setExtension("bcf"),
      pdffile.setExtension("blg"),
      pdffile.setExtension("fdb_latexmk"),
      pdffile.setExtension("fls"),
      pdffile.setExtension("sref"),
      pdffile.setExtension("sms"),
      File(pdffile.stripExtension.toString + ".run.xml"),
      File(pdffile.stripExtension.toString + ".synctex.gz"),
      File(pdffile.stripExtension.toString + "-blx.bib")
    )
    supportfiles.foreach(f => if (f.exists()) f.delete())
  }
  def pdflatex(file : File) : (Option[File],List[String]) = {
    val pb = Process(Seq("pdflatex","-interaction","nonstopmode","-halt-on-error",file.stripExtension.getName),file.up)
    val output = pb.lazyLines_!.toList
    if (output.exists(_.contains("!  ==> Fatal error"))) {
      val err = output.drop(output.indexWhere(_.startsWith("!")))
      clear(file)
      return (None,err)
    }
    val pdffile = file.setExtension("pdf")
    if (pdffile.exists()) (Some(pdffile),Nil) else {
      clear(file)
      (None,Nil)
    }
  }
  case class PdflatexError(errs : List[String]) extends Throwable
  def buildSingle(bf: BuildTask) : File = {
    pdflatex(bf.inFile) match {
      case (Some(pdffile),Nil) =>
        File.copy(pdffile,bf.outFile,true)
        pdffile
      case (_,ls) =>
        throw PdflatexError(ls)
    }
  }
}

class PdfLatex extends TraversingBuildTarget {
  val key: String = "pdflatex"
  override val outExt: String = "pdf"
  override val outDim: ArchiveDimension = Dim("export", "pdf")
  val inDim = Dim("source")
  override def includeFile(name: String): Boolean = name.endsWith(".tex")

  override def buildFile(bf: BuildTask): BuildResult = {
    log("Building pdflatex " + bf.inPath)
    PdfLatex.pdflatex(bf.inFile) match {
      case (Some(pdffile),Nil) =>
        File.copy(pdffile,bf.outFile,true)
        pdffile.delete()
        PdfLatex.clear(pdffile)
        BuildSuccess(Nil,List(PhysicalDependency(bf.outFile)))
      case (None,Nil) =>
        bf.errorCont(SourceError(bf.inFile.toString,SourceRef.anonymous(""),"(No error message)"))
        BuildFailure(Nil,Nil)
      case (None,ls) =>
        bf.errorCont(SourceError(bf.inFile.toString,SourceRef.anonymous(""),ls.head,ls.tail))
        BuildFailure(Nil,Nil)
    }
  }
}

class PdfBibLatex extends TraversingBuildTarget {
  val key: String = "pdfbiblatex"
  override val outExt: String = "pdf"
  override val outDim: ArchiveDimension = Dim("export", "pdf")
  val inDim = Dim("source")
  override def includeFile(name: String): Boolean = name.endsWith(".tex")

  override def buildFile(bf: BuildTask): BuildResult = {
    log("Building pdflatex " +  bf.inPath + " (first run)")
    import PdfLatex._
    try {
      val pdffile = buildSingle(bf)
      if (pdffile.setExtension(".bcf").exists()) {
        log("    -    biber " +  bf.inPath)
        Process(Seq("biber",pdffile.stripExtension.getName),pdffile.up).lazyLines_!
      } else {
        log("    -    bibtex " + bf.inPath)
        Process(Seq("bibtex",pdffile.stripExtension.getName),pdffile.up).lazyLines_!
      }
      log("    -    pdflatex " + bf.inPath + " (second run)")
      buildSingle(bf)
      log("    -    pdflatex " + bf.inPath + " (final run)")
      buildSingle(bf)
      File.copy(pdffile,bf.outFile,true)
      PdfLatex.clear(pdffile)
      pdffile.delete()
      BuildSuccess(Nil,List(PhysicalDependency(bf.outFile)))
    } catch {
      case PdflatexError(Nil) =>
        bf.errorCont(SourceError(bf.inFile.toString, SourceRef.anonymous(""), "(No error message)"))
        BuildFailure(Nil,Nil)
      case PdflatexError(ls) =>
        bf.errorCont(SourceError(bf.inFile.toString, SourceRef.anonymous(""), ls.head, ls.tail))
        BuildFailure(Nil,Nil)
    }
  }
}


class FullsTeX extends Importer with XHTMLParser {
  override def onBlock(qt : QueuedTask,br:BuildResult) = {
    val bt = controller.extman.getOrAddExtension(classOf[HTMLToOMDoc],"xhtml-omdoc").getOrElse(this)
    qt.copy(bt,br)
  }
  val key = "fullstex"
  override val inDim = info.kwarc.mmt.api.archives.source
  val inExts = List("tex")
  override def importDocument(bt: BuildTask, index: Document => Unit): BuildResult = {
    val ilog = (str : String) => {
      log(str)
      bt.errorCont match {
        case s:STeXLSPErrorHandler =>
          s.cont(0,str)
        case eh:MultipleErrorHandler => eh.handlers.collectFirst {
          case s : STeXLSPErrorHandler => s
        }.foreach {s =>
          s.cont(0,str)
        }
        case _ =>
      }
    }
    import PdfLatex._
    val extensions = stexserver.extensions
    val rules = extensions.flatMap(_.rules)
    val dpath = Path.parseD(bt.narrationDPath.toString.split('.').init.mkString(".") + ".omdoc",NamespaceMap.empty)
    val outFile : File = (bt.archive / Dim("xhtml") / bt.inPath).setExtension("xhtml")
    val state = new SemanticState(controller,rules,bt.errorCont,dpath)
    outFile.up.mkdirs()
    try {
      ilog("Building pdflatex " +  bt.inPath + " (first run)")
      val pdffile = buildSingle(bt)
      if (pdffile.setExtension(".bcf").exists()) {
        ilog("    -       biber " +  bt.inPath)
        Process(Seq("biber",pdffile.stripExtension.getName),pdffile.up).lazyLines_!
      } else {
        ilog("    -      bibtex " + bt.inPath)
        Process(Seq("bibtex",pdffile.stripExtension.getName),pdffile.up).lazyLines_!
      }
      ilog("    -    pdflatex " + bt.inPath + " (second run)")
      buildSingle(bt)
      ilog("    -    pdflatex " + bt.inPath + " (final run)")
      buildSingle(bt)
      ilog("    -       omdoc " + bt.inPath)
      val (errored,_) = buildFileActually(bt.inFile, outFile, state, bt.errorCont)
      val npdffile = (bt.archive / RedirectableDimension("export") / "pdf") / bt.inPath.setExtension("pdf").toString
      File.copy(pdffile,npdffile,true)
      pdffile.delete()
      PdfLatex.clear(pdffile)
      index(state.doc)
      ilog("    -      lucene " + bt.inPath)
      state.Search.makeDocument(
        (bt.archive / Dim("export","lucene") / bt.inPath).stripExtension,
        bt.inFile,bt.archive
      ).save
      ilog("Finished: " + bt.inFile)
      val results = PhysicalDependency(npdffile) :: PhysicalDependency(outFile) :: DocumentDependency(state.doc.path) :: state.doc.getDeclarations.collect {
        case mr: MRef =>
          LogicalDependency(mr.target)
      } ::: (bt.archive / Dim("export","lucene") / bt.inPath).stripExtension.descendants.map(PhysicalDependency)
      val used = state.doc.getDeclarations.flatMap {
        case m : MRef => controller.getO(m.target).toList.flatMap{
          case t : AbstractTheory => t.getAllIncludes.map(m => LogicalDependency(m.from)) ::: t.getNamedStructures.map(s => LogicalDependency(s.from match {case OMPMOD(p,_) => p}))
          case _ => Nil
        }
        case d: DRef if d.getOrigin == GeneratedDRef => List(DocumentDependency(d.target))
        case _ =>  Nil
      }.filterNot(results.contains)
      if (errored) BuildFailure(used,results) else state.missings match {
        case Nil => BuildSuccess(used, results)
        case o => MissingDependency(o.map(LogicalDependency), results, used)
      }
    } catch {
      case PdflatexError(Nil) =>
        bt.errorCont(SourceError(bt.inFile.toString, SourceRef.anonymous(""), "(No error message)"))
        BuildFailure(Nil,Nil)
      case PdflatexError(ls) =>
        bt.errorCont(SourceError(bt.inFile.toString, SourceRef.anonymous(""), ls.head, ls.tail))
        BuildFailure(Nil,Nil)
    }
  }
}
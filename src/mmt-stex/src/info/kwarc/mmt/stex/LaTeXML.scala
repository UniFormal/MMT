/*

package info.kwarc.mmt.stex

import info.kwarc.mmt.api.Level
import info.kwarc.mmt.api.archives.{Archive, ArchiveDimension, BuildEmpty, BuildFailure, BuildResult, BuildSuccess, BuildTask, Current, Dependency, Dim, DirBuildDependency, FileBuildDependency, PhysicalDependency, RedirectableDimension, Relational, content}
import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.utils.AnaArgs.splitOptions
import info.kwarc.mmt.api.utils.{EmptyPath, File, FilePath, MMTSystem, OS, Windows}

import java.nio.file.Files
import scala.sys.process.{Process, ProcessLogger}

object LaTeXML {
  private var latexmlc_cmd = "latexmlc"
  private var latexmls = "latexmls"
  private var expire: Int = 0 // 600
  private val delaySecs: Int = 1000
  private val portDefault: Int = 3334
  private var port: Int = 0 //portDefault
  // private val portModulo: Int = 1000
  // private var portSet: Boolean = false
  // private var profile = "stex-smglom-module"
  // private var profileSet: Boolean = false
  // private val perl5lib = "perl5lib"
  private var preloads: Seq[String] = Seq("LaTeX.pool")
  private var paths: Seq[String] = Nil
  private var includestyles = true
  private var initialized = false
  private var mathhub : File = File("")
  // private var reboot: Boolean = false
  // private var nopost: Boolean = false

  def latexmlc(in: File, out: File, log_out : Option[String => Unit] = None,log_err : Option[String => Unit] = None) = {
    var args = List("--dest=" + out.toString, in.toString,"--noparse")
    if (expire != 0) args ::= "--expire=" + expire.toString
    if (port != 0) args ::= "--port=" + port.toString
    preloads.foreach(args ::= "--preload=" + _)
    if (includestyles) args ::= "--includestyles"
    val path = mathhub / ".ltxml"
    if ((path / "sTeX.xsl").exists()) {
      args ::= "--path=" + path.toString
      args ::= "--stylesheet=" + (path / "sTeX.xsl").toString
    }
    val log = (log_out,log_err) match {
      case (None,None) => None
      case (o,e) => Some(ProcessLogger(o.getOrElse(_ => ()),e.getOrElse(_ => ())))
    }
    val (_,_,errStrs) = runCmd(latexmlc_cmd, args.reverse: _*)(Some(in.up),log)
    errStrs.foldRight(Nil : List[(Int,List[String])]){(s, ls) =>
      if (s.startsWith("Warning:")) {
        (Level.Warning,s.drop(8) :: Nil) :: ls
      } else if (s.startsWith("Error:")) {
        (Level.Error,s.drop(6) :: Nil) :: ls
      } else if (s.startsWith("Info:")) {
        (Level.Info,s.drop(5) :: Nil) :: ls
      } else if (s.startsWith("Fatal:")) {
        (Level.Fatal,s.drop(5) :: Nil) :: ls
      }  else if (s.startsWith("\t")) ls match {
        case (i,err) :: rest =>
          (i, s.drop(1) :: err) :: rest
        case _ =>
          ls
      } else ls
    }.map{case (i,ls) => (i,ls.reverse)}.reverse
  }

  def runCmd(cmd: String, args: String*)(in: Option[File] = None, log: Option[ProcessLogger] = None) = {
    val process = Process(cmd + args.mkString(" ", " ", ""), in.map(_.toJava))
    var out: List[String] = Nil
    var err: List[String] = Nil
    val logger = ProcessLogger(s => {out ::= s; log.foreach(_.out(s))}, {s => err ::= s; log.foreach(_.err(s))})
    val exitcode = process.!(logger)
    (exitcode, out, err)
  }

  def which(cmd: String) = {
    val cmdres = OS.detect match {
      case Windows => runCmd("for", "%i in (" + cmd + ".exe) do @echo.")()
      case _ => runCmd("which", cmd)()
    }
    cmdres match {
      case (0, s, _) if s.nonEmpty =>
        Some(s.head)
      case _ =>
        None
    }
  }

  def initialize(controller: Controller) {
    initialized = true

    controller.getEnvVar("LATEXMLC") match {
      case Some(s) =>
        latexmlc_cmd = s
      case None =>
        which("latexmlc").foreach(latexmlc_cmd = _)
    }
    controller.getEnvVar("MATHHUB") match {
      case Some(s) =>
        mathhub = File(s)
      case None =>
    }
  }
  def initializeIfNecessary(controller: Controller) = if (!initialized) initialize(controller)
}

class AllPdf extends LaTeXDirTarget {
  val key: String = "allpdf"

  def buildDir(a: Archive, in: FilePath, dir: File, force: Boolean): BuildResult = {
    BuildResult.empty
  }

  override def estimateResult(bt: BuildTask): BuildSuccess = {
    if (bt.isDir) {
      val a = bt.archive
      val ls = getAllFiles(bt).map(f => FileBuildDependency("pdflatex", a, bt.inPath / f))
      val at = DirBuildDependency("alltex", a, bt.inPath, Nil)
      BuildSuccess(ls :+ at, Nil)
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

// TODO clean up all of this

import STeXUtils._

class AllTeX extends LaTeXDirTarget {
  val key: String = "alltex"

  override def estimateResult(bt: BuildTask): BuildSuccess = {
    if (bt.isDir) {
      BuildSuccess(Nil, getAllFiles(bt).map(f => PhysicalDependency(bt.inFile / f)))
    } else {
      val used = super.estimateResult(bt).used.collect {
        case d@FileBuildDependency(k, _, _) if List("tex-deps").contains(k) => d
      }
      BuildSuccess(used, Nil)
    }
  }

  /* To topologically sort our alltex includes, we don't need to worry about dependencies that don't have any
  *  alltex files in their dependency closure, which, conveniently, is also where all the "cycles" happen. */
  def forgetIrrelevantDeps(in : Map[Dependency, Set[Dependency]]) : Map[Dependency, Set[Dependency]] = {
    def goodDependency(dep : Dependency) : Boolean = dep match {
      case FileBuildDependency(k, _, _) => k != "sms"
      case _ => true
    }

    var clean : Map[Dependency, Set[Dependency]] = in.filter(kv => goodDependency(kv._1))
    for ((k,v) <- clean) { clean = clean + (k -> v.filter(goodDependency)) }
    clean
  }

  /* For reasons likely to be irrelevant in the future (because SMS files are being deprecated alltogether),
   * we have this depsMap with dependencies on the sms-files, not the alltex-files, so we're rewiring that manually. */
  def rewireDepsMap(in : Map[Dependency, Set[Dependency]]) : Map[Dependency, Set[Dependency]] = {
    // rewire one FBD fomr oldkey to newkey.
    def changeOne(oldkey : String, newkey : String, dep : Dependency) : Dependency = dep match {
      case fbd@FileBuildDependency(key,z,d) => if (key == oldkey) { FileBuildDependency(newkey,z,d) } else { fbd }
      case other@_ => other
    }

    // All alltex dependencies there are, but changed to sms sp we can check easily below.
    val alltexs : Set[Dependency] = in.keySet.union(in.values.flatten.toSet).filter({
      case FileBuildDependency("alltex", _, _) => true
      case _ => false
    }).map(changeOne(oldkey = "alltex", newkey = "sms", _))

    var clean : Map[Dependency, Set[Dependency]] = Map.empty
    for ((k,v) <- in) {
      val kc = if (alltexs.contains(k)) { changeOne(oldkey = "sms", newkey = "alltex", k) } else k
      val vc = v map (d => if (alltexs.contains(d)) { changeOne(oldkey = "sms", newkey = "alltex", d) } else d)
      clean += (kc -> vc)
    }
    clean
  }

  override def getAnyDeps(dep: FileBuildDependency) : Set[Dependency] = {
    // We only need better coverage in the alltex target (for now)
    if (dep.key == key) {
      // ToDo: Not taking the inDim from the dep seems extremely fishy. Review.
      val inFile : File            = dep.archive / inDim / dep.inPath
      val res    : Set[Dependency] = readingSource(dep.archive, inFile, None).toSet
      res
    } else {
      super.getAnyDeps(dep)
    }
  }

  def buildDir(a: Archive, in: FilePath, dir: File, force: Boolean): BuildResult = {
    val dirFiles = getDirFiles(a, dir, includeFile)
    var success = false
    if (dirFiles.nonEmpty) {

      // One FileBuildDependency with key "alltex" for all files present.
      val the_dependencies : Set[Dependency] = getFilesRec(a, in)
      val deps = forgetIrrelevantDeps(rewireDepsMap(getDepsMap(the_dependencies)))

      val dso : Option[List[Dependency]] = Relational.flatTopsort(controller, deps)
      if (dso.isEmpty) {
        logError("Cyclical dependencies, topological sort is impossible.")
        return BuildFailure(Nil,Nil)
      }
      val ds : List[Dependency] = dso.get
      val ts = ds.collect {
        case bd: FileBuildDependency if List(key, "tex-deps", "sms").contains(bd.key) => bd
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

    /* If in an archive (always), we don't want to include (non)lang files excluded in .gitignore.
     * see: https://github.com/UniFormal/MMT/issues/542 */
    def gitignored(fn : String) : Boolean = Process("git check-ignore " + fn, dir.toJava).! == 0
    val ls : List[String] = langFiles(lang, files).filterNot(gitignored)

    val w = new StringBuilder
    def writeln(s: String) : Unit = w.append(s + "\n")

    ambleText(preOrPost = "pre", a, lang).foreach(writeln)
    writeln("")

    ls.foreach { f =>
      writeln("\\begin{center} \\LARGE File: \\url{" + f + "} \\end{center}")
      writeln("\\input{" + File(f).stripExtension + "} \\newpage")
      writeln("")
    }

    ambleText(preOrPost = "post", a, lang).foreach(writeln)
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

class DepsGenerator extends LaTeXBuildTarget {
  val key = "tex-deps"
  val outDim: ArchiveDimension = RedirectableDimension("tex-deps")
  override val outExt = "deps"

  def reallyBuildFile(bt: BuildTask): BuildResult = {
    logSuccess(bt.outPath)
    BuildEmpty("only create error file as timestamp")
  }
}

/** pdf generation */
class PdfLatex extends LaTeXBuildTarget {
  val key = "pdflatex"
  override val outExt = "pdf"
  val outDim: ArchiveDimension = Dim("export", "pdflatex", inDim.toString)
  private var pdflatexPath: String = "pdflatex"

  override def includeFile(n: String): Boolean =
    n.endsWith(".tex") && !n.endsWith(localpathsFile)

  override def start(args: List[String]) {
    super.start(args)
    val (_, nonOpts) = splitOptions(remainingStartArguments)
    val nonOptArgs = if (nameOfExecutable.nonEmpty) nameOfExecutable :: nonOpts
    else nonOpts
    val newPath = getFromFirstArgOrEnvvar(nonOptArgs, name = "xelatex", pdflatexPath)
    if (newPath != pdflatexPath) {
      pdflatexPath = newPath
      log("using executable \"" + pdflatexPath + "\"")
    }
  }

  override def estimateResult(bt: BuildTask): BuildSuccess = {
    val BuildSuccess(used, provided) = super.estimateResult(bt)
    if (bt.inPath.name.startsWith("all.")) {
      BuildSuccess(used :+ DirBuildDependency("alltex", bt.archive, bt.inPath.dirPath, Nil), provided)
    } else BuildSuccess(used, provided)
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
  override val outDim : ArchiveDimension = content

  override def includeDir(n: String): Boolean = n.endsWith("tikz")

  override def reallyBuildFile(bt: BuildTask): BuildResult =
  {
    // ToDo: This pdf is ~technically~ also generated content,
    //       suggesting it should be elsewhere. But a bunch
    //       of things assume it's a sibling from the inFile so
    //       its complicated. Link? Copy? Ignore?
    val pdfFile : File = bt.inFile.setExtension("pdf")
    val svgFile : File = bt.outFile

    bt.outFile.delete()
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
    } catch {
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

*/

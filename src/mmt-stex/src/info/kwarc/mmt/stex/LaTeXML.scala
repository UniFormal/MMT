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
  // private var reboot: Boolean = false
  // private var nopost: Boolean = false

  def latexmlc(in: File, out: File, log_out : Option[String => Unit] = None,log_err : Option[String => Unit] = None) = {
    var args = List("--dest=" + out.toString, in.toString,"--pmml")
    if (expire != 0) args ::= "--expire=" + expire.toString
    if (port != 0) args ::= "--port=" + port.toString
    preloads.foreach(args ::= "--preload=" + _)
    if (includestyles) args ::= "--includestyles"
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
      } else if (s.startsWith("\t")) ls match {
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
  private var pdflatexPath: String = "xelatex"

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

/*    val (_, nonOpts) = splitOptions(remainingStartArguments)
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
    reboot = optionsMap.contains("reboot")
    if (reboot) expire = 1
    nopost = optionsMap.contains("nopost") */

/*
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

// Deprecated as of October 2020, see:
// https://github.com/UniFormal/MMT/issues/540
@deprecated("sTeX no longer relies on localpaths.tex")
class LocalPaths extends LaTeXDirTarget {
  val key : String = "localpaths"

  override def estimateResult(bt: BuildTask) : BuildSuccess = {
    if (bt.isDir) {
      val lp : ResourceDependency = PhysicalDependency(File(bt.dirName + "/localpaths.tex"))
      BuildSuccess(Nil,List(lp))
    } else { BuildResult.empty }
  }

  override def buildDir(a: Archive, in: FilePath, dir: File, force: Boolean) : BuildResult = {
    val target  : File    = dir / "localpaths.tex"
    var success : Boolean = false

    /* Only create file if it has a sibling tex file */
    val siblings : Boolean = dir.children.exists(s => s.getExtension.contains("tex") && s.name != "localpaths.tex")

    /* forcing recreation of file means deleting the old one. */
    if (force && target.exists()) { target.delete() }

    if (force || (!target.exists() && siblings)) {
      createLocalPaths(a, dir)
      success = true
    }

    if (success) { BuildSuccess(Nil, List(PhysicalDependency(target))) }
    else if (!siblings) {
      log("No sibling .tex-file, localpaths.tex not created")
      BuildResult.empty
    }
    else BuildEmpty("up-to-date")
  }
}

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

import info.kwarc.mmt.stex.STeXUtils._

/** sms generation */
class SmsGenerator extends LaTeXBuildTarget {
  val key = "sms"
  val outDim: ArchiveDimension = source
  override val outExt = "sms"

  override def includeDir(n: String): Boolean = !n.endsWith("tikz")

  def reallyBuildFile(bt: BuildTask): BuildResult = {
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

  val outDim: ArchiveDimension = RedirectableDimension("latexml")

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
  private val perl5lib = "perl5lib"
  private var preloads: Seq[String] = Nil
  private var paths: Seq[String] = Nil
  private var reboot: Boolean = false
  private var nopost: Boolean = false

  private val latexmlOpts: OptionDescrs = List(
    OptionDescr("latexmlc", "", StringArg, "executable path for (client) latexmlc"),
    OptionDescr("latexmls", "", StringArg, "executable path for (server) latexmls"),
    // OptionDescr("expire", "", IntArg, "expire argument for (server) latexmls"),
    // OptionDescr("port", "", IntArg, "port for (server) latexmls"),
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
    reboot = optionsMap.contains("reboot")
    if (reboot) expire = 1
    nopost = optionsMap.contains("nopost")
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
    var region: SourceRegion = SourceRegion.none
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
      case _: BindException =>
        true
    }

  /** Compile a .tex file to OMDoc */
  def reallyBuildFile(bt: BuildTask): BuildResult = {
    //val realPort: Int = if (portSet) port
    //else port + Math.abs(bt.archive.id.hashCode % portModulo)
    setLatexmlBins(bt)
    val lEnv = extEnv(bt)
    val output = new StringBuffer()
    /*
    if (reboot) {
      val pbc = Process(Seq(latexmlc, // "--expire=" + expire, "--port=" + realPort,
        "literal:restarting"), bt.archive / inDim, lEnv: _*)
      if (isServerRunning(realPort)) {
        logResult("trying to kill latexml server: " + latexmls + " --port=" + realPort)
        pbc.!(ProcessLogger(_ => (), _ => ()))
        Thread.sleep(delaySecs)
      }
      BuildResult.empty
    } else { */
    val lmhOut = bt.outFile
    val logFile = bt.outFile.setExtension("ltxlog")
    lmhOut.delete()
    logFile.delete()
    val realProfile = if (profileSet) profile
    else getProfile(bt.archive).getOrElse(profile)
    val argSeq = Seq(latexmlc, bt.inFile.toString,
      "--profile=" + realProfile, "--path=" + styPath(bt),
      "--destination=" + lmhOut, "--log=" + logFile) ++
      (if (noAmble(bt.inFile)) Seq("--whatsin=document")
      else Seq("--preamble=" + getAmbleFile("pre", bt),
        "--postamble=" + getAmbleFile("post", bt))) ++
      // Seq("--expire=" + expire, "--port=" + realPort) ++
      (if (nopost) Seq("--nopost") else Nil) ++
      preloads.map("--preload=" + _) ++
      paths.map("--path=" + _)
    log(argSeq.mkString(" ").replace(" --", "\n --"))
    var failure = false
    try {
      /*
        val pbs = Process(Seq(latexmls, // "--expire=" + expire, "--port=" + realPort,
          "--autoflush=100"), bt.archive / inDim, lEnv: _*)
        if (!isServerRunning(realPort) && expire > -1) {
          pbs.run(BasicIO.standard(false).daemonized)
          Thread.sleep(delaySecs)
        }
         */
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
      else MissingDependency(missingFiles, providedTheories, missingFiles)
    } else {
      logSuccess(bt.outPath)
      BuildSuccess(Nil, providedTheories)
    }
    // }
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
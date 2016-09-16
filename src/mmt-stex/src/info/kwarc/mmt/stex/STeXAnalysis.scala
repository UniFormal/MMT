package info.kwarc.mmt.stex

import info.kwarc.mmt.api.archives._
import info.kwarc.mmt.api.utils.{File, FilePath}
import info.kwarc.mmt.stex.STeXUtils._

case class STeXStructure(smslines: List[String], deps: List[Dependency]) {
  def join(s2: STeXStructure): STeXStructure =
    if (s2.smslines.isEmpty && s2.deps.isEmpty) this
    else STeXStructure(s2.smslines ++ smslines, s2.deps ++ deps)
}

/**
  * Created by maeder on 11.11.15.
  */
trait STeXAnalysis {
  self: TraversingBuildTarget =>

  protected def logSuccess(f: FilePath) {
    logResult("success " + f)
  }

  protected def logFailure(f: FilePath) {
    logResult("failure " + f)
  }

  def mkDep(a: Archive, ar: String, fp: FilePath): List[Dependency] = {
    val root = a.root.up.up / ar
    controller.addArchive(root)
    controller.backend.getArchive(root) match {
      case None =>
        log("missing archive " + ar + " for " + fp)
        Nil
      case Some(arch) => List(mkFileDep(arch, fp))
    }
  }

  def mkFileDep(archive: Archive, filePath: FilePath): Dependency =
    FileBuildDependency("tex-deps", archive, filePath)

  def mhRepos(a: Archive, r: String, b: String): List[Dependency] = {
    val fp = entryToPath(b)
    val optRepo = Option(r).map(getArgMap)
    optRepo match {
      case Some(m) => mkDep(a, m.getOrElse("mhrepos", archString(a)), fp)
      case None => List(mkFileDep(a, fp))
    }
  }

  protected def toKeyDep(d: Dependency, key: String): Dependency = d match {
    case FileBuildDependency(_, ar, fp) => FileBuildDependency(key, ar, fp)
    case fd => fd
  }

  protected def matchPathAndRep(a: Archive, inFile: File, line: String, parents: Set[File]): List[Dependency] =
    line match {
      case beginModnl(_, _, b) => List(mkFileDep(a, entryToPath(b)))
      case input(_, _, _, b) =>
        val p = if (line.startsWith("\\lib"))
          STeXUtils.getAmbleFile(b + ".tex", a, None)
        else (inFile.up / b).setExtension("tex")
        val d = PhysicalDependency(p)
        d :: (if (!parents.contains(p)) getDeps(a, p, parents + p) else Nil)
      case includeGraphics(_, _, b) =>
        val gExts = List("png", "jpg", "eps", "pdf")
        val p = inFile.up / b
        if (p.getExtension.exists(gExts.contains)) List(PhysicalDependency(p))
        else {
          val os = gExts.find { s =>
            p.setExtension(s).exists
          }
          if (os.isEmpty) log(inFile + " misses graphics file: " + b)
          os.toList.map(s => PhysicalDependency(p.setExtension(s)))
        }
      case importOrUseModule(r) =>
        getArgMap(r).get("load").map(f => PhysicalDependency(File(f).setExtension(".sms"))).toList
      case mhinputRef(_, r, b) =>
        val fp = entryToPath(b)
        Option(r) match {
          case Some(id) => mkDep(a, id, fp)
          case None => List(mkFileDep(a, fp))
        }
      case tikzinput(_, r, b) => mhRepos(a, r, b).map(toKeyDep(_, "tikzsvg"))
      case guse(r, b) => mkDep(a, r, entryToPath(b))
      case useMhProblem(_, r, b) => createMhImport(a, r, b).flatMap(_.deps).map(toKeyDep(_, "tikzsvg"))
      case includeMhProblem(_, r, b) => mhRepos(a, r, b)
      case _ => Nil
    }

  private def mkImport(a: Archive, r: String, p: String, s: String, ext: String): String =
    "\\importmodule[load=" + a.root.up.up + "/" + r + "/source/" + p + ",ext=" + ext + "]" + s + "%"

  private def mkMhImport(a: Archive, r: String, p: String, s: String): STeXStructure =
    STeXStructure(List(mkImport(a, r, p, s, "sms")), mkDep(a, r, entryToPath(p)).map(toKeyDep(_, "sms")))

  private def mkGImport(a: Archive, r: String, p: String): STeXStructure =
    STeXStructure(List(mkImport(a, r, p, "{" + p + "}", "tex"), "\\mhcurrentrepos{" + r + "}%"),
      mkDep(a, r, entryToPath(p)))

  // line list is reversed!

  private def splitArgs(r: String): List[List[String]] = r.split(",").toList.map(_.split("=").toList)

  private def mkArgMap(ll: List[List[String]]): Map[String, String] = {
    var m: Map[String, String] = Map.empty
    ll.foreach {
      case List(k, v) => m += ((k.trim, v.trim))
      case _ =>
    }
    m
  }

  private def getArgMap(r: String): Map[String, String] = mkArgMap(splitArgs(r))

  private def createMhImport(a: Archive, r: String, b: String): List[STeXStructure] = {
    val m = getArgMap(r)
    m.get("path").toList.map(p => mkMhImport(a, m.getOrElse("repos", archString(a)), p, b))
  }

  private def createGImport(a: Archive, r: String, p: String): STeXStructure = {
    Option(r) match {
      case Some(id) =>
        mkGImport(a, id, p)
      case None =>
        mkGImport(a, archString(a), p)
    }
  }

  private def createImport(r: String, p: String): STeXStructure =
    STeXStructure(List("\\importmodule[" + r + "]{" + p + "}%"), Nil)

  /** create sms file */
  def createSms(a: Archive, inFile: File, outFile: File) {
    val smsLines = mkSTeXStructure(a, inFile, readSourceRebust(inFile).getLines, Set.empty).smslines
    if (smsLines.nonEmpty) File.write(outFile, smsLines.reverse.mkString("", "\n", "\n"))
    else log("no sms content")
  }

  /** get dependencies */
  def getDeps(a: Archive, in: File, parents: Set[File], amble: Option[File] = None): List[Dependency] = {
    val f = amble.getOrElse(in)
    if (f.exists)
      mkSTeXStructure(a, in, readSourceRebust(f).getLines, parents).deps
    else Nil
  }

  /** in file is used for relative \input paths */
  def mkSTeXStructure(a: Archive, in: File, lines: Iterator[String], parents: Set[File]): STeXStructure = {
    var struct = STeXStructure(Nil, Nil)
    def join(s: STeXStructure) = {
      struct = struct.join(s)
    }

    lines.foreach { line =>
      val l = stripComment(line).trim
      val verbIndex = l.indexOf("\\verb")
      if (verbIndex <= -1) {
        val sl = matchSmsEntry(a, l)
        sl.foreach(join)
        if (key != "sms") {
          val od = matchPathAndRep(a, in, l, parents)
          od.foreach(d => join(STeXStructure(Nil, List(d))))
        }
      }
    }
    struct
  }

  def matchSmsEntry(a: Archive, line: String): List[STeXStructure] = {
    line match {
      case importMhModule(r, b) =>
        createMhImport(a, r, b)
      case gimport(_, r, p) =>
        List(createGImport(a, r, p))
      case smsGStruct(_, r, _, p) =>
        List(createGImport(a, r, p))
      case smsMhStruct(r, _, p) =>
        createMhImport(a, r, p)
      case smsSStruct(r, _, p) =>
        List(createImport(r, p))
      case smsViewsig(r, _, f, t) =>
        val m = getArgMap(r)
        val fr = m.getOrElse("fromrepos", archString(a))
        val tr = m.getOrElse("torepos", archString(a))
        List(mkGImport(a, fr, f), mkGImport(a, tr, t))
      case smsViewnl(_, r, p) =>
        List(createGImport(a, archString(a), p))
      case smsMhView(r, _, f, t) =>
        val m = getArgMap(r)
        var ofp = m.get("frompath")
        var otp = m.get("topath")
        val fr = m.getOrElse("fromrepos", archString(a))
        val tr = m.getOrElse("torepos", archString(a))
        (ofp, otp) match {
          case (Some(fp), Some(tp)) =>
            List(mkMhImport(a, fr, fp, f), mkMhImport(a, tr, tp, t))
          case _ => Nil
        }
      case smsView(r, f, t) =>
        val m = getArgMap(r)
        val ofr = m.get("from")
        val otr = m.get("to")
        (ofr, otr) match {
          case (Some(fr), Some(tr)) =>
            List(createImport(fr, f), createImport(tr, t))
          case _ => Nil
        }
      case _ if smsRegs.findFirstIn(line).isDefined => List(STeXStructure(List(line + "%"), Nil))
      case _ => Nil
    }
  }
}

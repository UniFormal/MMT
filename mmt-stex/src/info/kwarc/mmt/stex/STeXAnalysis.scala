package info.kwarc.mmt.stex

import java.nio.charset.{Charset, MalformedInputException}

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

  def mkDep(a: Archive, ar: String, fp: FilePath, key: String): Option[Dependency] = {
    val root = a.root.up.up / ar
    controller.addArchive(root)
    controller.backend.getArchive(root) match {
      case None =>
        log("missing archive " + ar + " for " + fp)
        None
      case Some(arch) => Some(mkFileDep(key, arch, fp))
    }
  }

  def mkFileDep(key: String, archive: Archive, filePath: FilePath): Dependency =
    if (List("latexml", "pdflatex", "tikzsvg", "tex-deps").contains(key)) {
      FileBuildDependency("tex-deps", archive, filePath)
    } else {
      FileBuildDependency(key, archive, filePath)
    }

  def mhRepos(a: Archive, r: String, b: String, key: String): Option[Dependency] = {
    val fp = entryToPath(b)
    val optRepo = Option(r).map(_.split(",").toList.map(_.split("=").toList).
      filter {
        case List("mhrepos", _) => true
        case _ => false
      })
    optRepo match {
      case Some(List(List(_, id))) => mkDep(a, id, fp, key)
      case _ => Some(mkFileDep(key, a, fp))
    }
  }

  protected def matchPathAndRep(key: String, a: Archive, line: String): Option[Dependency] =
    line match {
      case beginModnl(_, _, b) => Some(mkFileDep(key, a, entryToPath(b)))
      case mhinputRef(_, r, b) =>
        val fp = entryToPath(b)
        Option(r) match {
          case Some(id) => mkDep(a, id, fp, key)
          case None => Some(mkFileDep(key, a, fp))
        }
      case tikzinput(_, r, b) => mhRepos(a, r, b, "tikzsvg")
      case includeMhProblem(_, r, b) => mhRepos(a, r, b, key)
      case groups(_, _, r, b) =>
        val depKey = if (line.startsWith("\\usemhmodule")) "sms" else key
        val fp = entryToPath(b)
        val optRepo = Option(r).map(splitArgs)
        optRepo match {
          case Some(List(List(id))) => mkDep(a, id, fp, depKey)
          case Some(ll) =>
            val m = mkArgMap(ll)
            val op = m.get("path")
            op.flatMap { pa =>
              val p = entryToPath(pa)
              m.get("repos") match {
                case Some(repos) => mkDep(a, repos, p, depKey)
                case None => Some(mkFileDep(depKey, a, p))
              }
            }
          case None => Some(mkFileDep(depKey, a, fp))
          case _ => None
        }
      case _ => None
    }

  private def mkImport(a: Archive, r: String, p: String, s: String, ext: String): String =
    "\\importmodule[load=" + a.root.up.up + "/" + r + "/source/" + p + ",ext=" + ext + "]" + s + "%"

  private def mkMhImport(a: Archive, r: String, p: String, s: String): STeXStructure =
    STeXStructure(List(mkImport(a, r, p, s, "sms")), mkDep(a, r, entryToPath(p), "sms").toList)

  private def mkGImport(key: String, a: Archive, r: String, p: String): STeXStructure =
    STeXStructure(List(mkImport(a, r, p, "{" + p + "}", "tex"), "\\mhcurrentrepos{" + r + "}%"),
      mkDep(a, r, entryToPath(p), key).toList)

  // line list is reversed!

  private def splitArgs(r: String): List[List[String]] = r.split(",").toList.map(_.split("=").toList)

  private def mkArgMap(ll: List[List[String]]): Map[String, String] = {
    var m: Map[String, String] = Map.empty
    ll.foreach {
      case List(k, v) => m += ((k, v))
      case _ =>
    }
    m
  }

  private def getArgMap(r: String): Map[String, String] = mkArgMap(splitArgs(r))

  private def createMhImport(a: Archive, r: String, b: String): List[STeXStructure] = {
    val m = getArgMap(r)
    m.get("path").toList.map(p => mkMhImport(a, m.getOrElse("repos", archString(a)), p, b))
  }

  private def createGImport(key: String, a: Archive, r: String, p: String): STeXStructure = {
    Option(r) match {
      case Some(id) =>
        mkGImport(key, a, id, p)
      case None =>
        mkGImport(key, a, archString(a), p)
    }
  }

  private def createImport(r: String, p: String): STeXStructure =
    STeXStructure(List("\\importmodule[" + r + "]{" + p + "}%"), Nil)

  /** create sms file */
  def createSms(a: Archive, inFile: File, outFile: File) {
    val source = readSourceRebust(inFile)
    val w = new StringBuilder
    val STeXStructure(smsLines, _) = mkSTeXStructure("sms", a, source.getLines)
    if (smsLines.nonEmpty) File.write(outFile, smsLines.reverse.mkString("", "\n", "\n"))
    else log("no sms content")
  }

  def mkSTeXStructure(key: String, a: Archive, lines: Iterator[String]): STeXStructure = {
    var struct = STeXStructure(Nil, Nil)
    def join(s: STeXStructure) = {
      struct = struct.join(s)
    }
    lines.foreach { line =>
      val l = stripComment(line).trim
      val verbIndex = l.indexOf("\\verb")
      if (verbIndex <= -1) {
        val sl = matchSmsEntry(key, a, l)
        sl.foreach(join)
        if (key != "sms") {
          val od = matchPathAndRep(key, a, l)
          od.foreach(d => join(STeXStructure(Nil, List(d))))
        }
      }
    }
    struct
  }

  def matchSmsEntry(key: String, a: Archive, line: String): List[STeXStructure] = {
    line match {
      case importMhModule(r, b) =>
        createMhImport(a, r, b)
      case gimport(_, r, p) =>
        List(createGImport(key, a, r, p))
      case smsGStruct(_, r, _, p) =>
        List(createGImport(key, a, r, p))
      case smsMhStruct(r, _, p) =>
        createMhImport(a, r, p)
      case smsSStruct(r, _, p) =>
        List(createImport(r, p))
      case smsViewsig(r, _, f, t) =>
        val m = getArgMap(r)
        val fr = m.getOrElse("fromrepos", archString(a))
        val tr = m.getOrElse("torepos", archString(a))
        List(mkGImport(key, a, fr, f), mkGImport(key, a, tr, t))
      case smsViewnl(_, r, p) =>
        List(createGImport(key, a, archString(a), p))
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

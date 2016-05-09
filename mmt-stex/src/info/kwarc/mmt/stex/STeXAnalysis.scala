package info.kwarc.mmt.stex

import java.nio.charset.{Charset, MalformedInputException}

import info.kwarc.mmt.api.archives._
import info.kwarc.mmt.api.utils.{File, FilePath}
import info.kwarc.mmt.stex.STeXUtils._

case class STeXStructure(smslines: List[String], deps: List[Dependency])

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
        if (key != "tex-deps") logError("missing archive " + ar + " for " + fp)
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
      case groups(_, r, b) =>
        val depKey = if (line.startsWith("\\importmhmodule") || line.startsWith("\\usemhmodule")) "sms" else key
        val fp = entryToPath(b)
        val optRepo = Option(r).map(_.split(",").toList.sorted.map(_.split("=").toList))
        optRepo match {
          case Some(List(List(id))) => mkDep(a, id, fp, depKey)
          case Some(List("path", p) :: tl) =>
            val path = entryToPath(p)
            tl match {
              case List(List("repos", id)) => mkDep(a, id, path, depKey)
              case Nil => Some(mkFileDep(depKey, a, path))
              case _ => None
            }
          case None => Some(mkFileDep(depKey, a, fp))
          case _ => None
        }
      case _ => None
    }

  val encodings = List("ISO-8859-1", Charset.defaultCharset.toString, "UTF-8",
    "UTF-16").distinct

  /** pick encoding for sms creation */
  def createSmsForEncodings(bt: BuildTask, encs: List[String]) {
    val readMsg = "reading " + bt.inPath
    encs match {
      case hd :: tl =>
        try {
          log(readMsg + " using encoding " + hd)
          createSms(bt.archive, bt.inFile, bt.outFile, hd)
          logSuccess(bt.outPath)
        }
        catch {
          case _: MalformedInputException =>
            log(readMsg + bt.inPath + " failed")
            createSmsForEncodings(bt, tl)
        }
      case Nil =>
        bt.errorCont(LocalError("no suitable encoding found for " + bt.inPath))
        logFailure(bt.outPath)
    }
  }

  private def mkImport(a: Archive, r: String, p: String, s: String, ext: String) =
    "\\importmodule[load=" + a.root.up.up + "/" + r + "/source/" + p + ",ext=" + ext + "]" + s

  private def mkMhImport(a: Archive, r: String, p: String, s: String) =
    mkImport(a, r, p, s, "sms")

  private def mkGImport(a: Archive, r: String, p: String) =
    "\\mhcurrentrepos{" + r + "}%\n" + mkImport(a, r, p, "{" + p + "}", "tex")

  private def getArgMap(r: String): Map[String, String] = {
    val ll = r.split(",").toList.map(_.split("=").toList)
    var m: Map[String, String] = Map.empty
    ll.foreach {
      case List(k, v) => m += ((k, v))
      case _ =>
    }
    m
  }

  private def createMhImport(a: Archive, r: String, b: String): Option[String] = {
    val m = getArgMap(r)
    m.get("path").map(p => mkMhImport(a, m.getOrElse("repos", archString(a)), p, b))
  }

  private def createGImport(a: Archive, r: String, p: String): String = {
    Option(r) match {
      case Some(id) =>
        mkGImport(a, id, p)
      case None =>
        mkGImport(a, archString(a), p)
    }
  }

  private def createImport(r: String, p: String): String =
    "\\importmodule[" + r + "]{" + p + "}"

  /** create sms file */
  private def createSms(a: Archive, inFile: File, outFile: File, enc: String) {
    val source = scala.io.Source.fromFile(inFile, enc)
    val w = new StringBuilder
    source.getLines().foreach { line =>
      val l = stripComment(line).trim
      var n = l
      val verbIndex = l.indexOf("\\verb")
      if (verbIndex <= -1 && smsRegs.findFirstIn(l).isDefined) {
        l match {
          case importMhModule(r, b) =>
            createMhImport(a, r, b).foreach(n = _)
          case gimport(_, r, p) =>
            n = createGImport(a, r, p)
          case smsGStruct(_, r, _, p) =>
            n = createGImport(a, r, p)
          case smsMhStruct(r, _, p) =>
            createMhImport(a, r, p).foreach(n = _)
          case smsSStruct(r, _, p) =>
            n = createImport(r, p)
          case smsViewsig(r, _, f, t) =>
            val m = getArgMap(r)
            val fr = m.getOrElse("fromrepos", archString(a))
            val tr = m.getOrElse("torepos", archString(a))
            n = mkGImport(a, fr, f) + "\n" + mkGImport(a, tr, t)
          case smsViewnl(_, r, p, _, _, _) =>
            n = createGImport(a, r, p)
          case smsMhView(r, _, f, t) =>
            val m = getArgMap(r)
            var ofp = m.get("frompath")
            var otp = m.get("topath")
            val fr = m.getOrElse("fromrepos", archString(a))
            val tr = m.getOrElse("torepos", archString(a))
            (ofp, otp) match {
              case (Some(fp), Some(tp)) =>
                n = mkMhImport(a, fr, fp, f) + "\n" + mkMhImport(a, tr, tp, t)
              case _ => // this leaves the original begin line
            }
          case smsView(r, f, t) =>
            val m = getArgMap(r)
            val ofr = m.get("from")
            val otr = m.get("to")
            (ofr, otr) match {
              case (Some(fr), Some(tr)) =>
                n = createImport(fr, f) + "\n" + createImport(tr, t)
              case _ => // this leaves the original begin line
            }
          case _ =>
        }
        w.append(n + "%\n")
      }
    }
    val res = w.result
    if (res.nonEmpty) File.write(outFile, res)
    else log("no sms content")
  }
}

package info.kwarc.mmt.stex

import java.nio.charset.{MalformedInputException, Charset}

import info.kwarc.mmt.api.archives._
import info.kwarc.mmt.api.utils.{File, FilePath}
import info.kwarc.mmt.stex.STeXUtils._


case class STeXStructure(smslines: List[String], deps: List[Dependency])

/**
  * Created by maeder on 11.11.15.
  */
trait STeXAnalysis {self: TraversingBuildTarget =>
  def mkDep(a: Archive, ar: String, fp: FilePath, key: String): Option[Dependency] =
    controller.getOrAddArchive(a.baseDir / ar) match {
      case None => None
      case Some(arch) => Some(BuildDependency(arch, fp, key))
    }

  protected def matchPathAndRep(key: String, a: Archive, line: String): Option[Dependency] =
    line match {
      case beginModnl(b) => Some(BuildDependency(a, entryToPath(b), key))
      case mhinputRef(_, r, b) =>
        val fp = entryToPath(b)
        Option(r) match {
          case Some(id) => mkDep(a, id, fp, "sms")
          case None => Some(BuildDependency(a, fp, "sms"))
        }
      case tikzinput(_, r, b) =>
        val fp = entryToPath(b)
        val optRepo = Option(r).map(_.split(",").toList.map(_.split("=").toList).
          filter {
            case List("mhrepos", _) => true
            case _ => false
          })
        optRepo match {
          case Some(List(List(_, id))) => mkDep(a, id, fp, "tikzsvg")
          case _ => Some(BuildDependency(a, fp, "tikzsvg"))
        }
      case groups(_, r, b) =>
        val depKey = if (line.startsWith("\\usemhmodule")) "sms" else key
        val fp = entryToPath(b)
        val optRepo = Option(r).map(_.split(",").toList.sorted.map(_.split("=").toList))
        optRepo match {
          case Some(List(List(id))) => mkDep(a, id, fp, depKey)
          case Some(List("path", p) :: tl) =>
            val path = entryToPath(p)
            tl match {
              case List(List("repos", id)) => mkDep(a, id, path, depKey)
              case Nil => Some(BuildDependency(a, path, depKey))
              case _ => None
            }
          case None => Some(BuildDependency(a, fp, depKey))
          case _ => None
        }
      case _ => None
    }

  private val encodings = List("ISO-8859-1", Charset.defaultCharset.toString, "UTF-8",
    "UTF-16").distinct

  /** pick encoding for sms creation */
  private def createSmsForEncodings(bt: BuildTask, encs: List[String]): Unit = {
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



  private def mkImport(b: File, r: String, p: String, a: String, ext: String) =
    "\\importmodule[load=" + b + "/" + r + "/source/" + p + ",ext=" + ext + "]" + a

  private def mkMhImport(b: File, r: String, p: String, a: String) =
    mkImport(b, r, p, a, "sms")

  private def mkGImport(b: File, r: String, p: String) =
    "\\mhcurrentrepos{" + r + "}%\n" + mkImport(b, r, p, "{" + p + "}", "tex")

  /** create sms file */
  private def createSms(a: Archive, inFile: File, outFile: File, enc: String): Unit = {
    val source = scala.io.Source.fromFile(inFile, enc)
    val w = File.Writer(outFile)
    source.getLines().foreach { line =>
      val l = stripComment(line).trim
      var n = l
      val verbIndex = l.indexOf("\\verb")
      if (verbIndex <= -1 && smsRegs.findFirstIn(l).isDefined) {
        l match {
          case importMhModule(r, b) =>
            val m = r.split(",").toList.sorted.map(_.split("=").toList)
            m match {
              case List("path", p) :: tl =>
                tl match {
                  case Nil =>
                    n = mkMhImport(a.baseDir, archString(a), p, b)
                  case List(List("repos", id)) =>
                    n = mkMhImport(a.baseDir, id, p, b)
                  case _ =>
                }
              case _ =>
            }
          case gimport(_, r, p) =>
            Option(r) match {
              case Some(id) =>
                n = mkGImport(a.baseDir, id, p)
              case None =>
                n = mkGImport(a.baseDir, archString(a), p)
            }
          case _ =>
        }
        w.println(n + "%")
      }
    }
    w.close()
  }


}



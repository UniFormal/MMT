package info.kwarc.mmt.api.checking

import info.kwarc.mmt.api._
import archives._
import documents._
import frontend.Controller
import ontology.{Declares, RelationExp}
import parser._
import utils._

/** parses and returns a checked result */
abstract class Interpreter extends Importer {
  /** the input format (e.g., the file extension) to which this interpreter is applicable */
  def format: String

  def key = format + "-omdoc"

  override def isApplicable(s: String) = s == format || super.isApplicable(s)

  def inExts = List(format)

  /** parses a [[ParsingStream]] and returns a checked result */
  def apply(ps: ParsingStream)(implicit errorCont: ErrorHandler): Document

  /** creates a [[ParsingStream]] for the input file and interprets it */
  def importDocument(bf: BuildTask, index: Document => Unit): BuildResult = {
    val dPath = getDPath(bf.archive, bf.inPath) // bf.narrationDPath except for extension
    val ps = new ParsingStream(bf.base / bf.inPath.segments, dPath, bf.archive.namespaceMap, format, File.Reader(bf.inFile))
    val doc = apply(ps)(bf.errorCont)
    index(doc)
    BuildResult.empty
  }

  /** bf.narrationDPath except for extension */
  def getDPath(a: Archive, fp: FilePath): DPath = {
    val inPathOMDoc = fp.toFile.setExtension("omdoc").toFilePath
    DPath(a.narrationBase / inPathOMDoc.segments)
  }

  /** reconstruct source file form DPATH */
  def dPathToFile(p: Path): List[(Archive, FilePath)] =
    p match {
      case DPath(uri) =>
        Relational.getArchives(controller).
          flatMap { a =>
            if (a.narrationBase <= uri) {
              val b = a.narrationBase.toString
              val c = uri.toString.stripPrefix(b)
              if (c.startsWith("/http..")) None // filter out content documents
              else Some((a, File(a.rootString + "/" + inDim + c).setExtension(format)))
            } else None
          }.map { afp =>
          if (afp._2.length() == 0) log("missing file: " + afp._2 + " for path " + p)
          (afp._1, FilePath((afp._1.root / inDim.toString).relativize(afp._2).segments.tail))
        }
      case _ => Nil
    }

  /** directly resolved logical dependencies */
  override def getDeps(bt: BuildTask): Set[Dependency] = {
    val a = bt.archive
    val fp = bt.inPath
    val rs = controller.depstore
    val d = getDPath(a, fp)
    log("estimate dependencies for: " + d.toString)
    val usedTheories = rs.querySet(d, +Declares * RelationExp.Deps)
    val reducedTheories = usedTheories.map {
      case MPath(p, LocalName(hd :: _ :: _)) => MPath(p, LocalName(List(hd)))
      case t => t
    }.toList.sortBy(_.toString)
    var result: Set[(Archive, FilePath)] = Set.empty
    reducedTheories.foreach { theo =>
      val provider = rs.querySet(theo, -Declares).toList.sortBy(_.toString)
      if (provider.isEmpty)
        log("  nothing provides: " + theo + " for " + d)
      else {
        val ds = provider.flatMap(dPathToFile)
        result ++= ds
        ds match {
          case Nil => log("  no document found for: " + theo)
          case hd :: Nil =>
            if (ds == dPathToFile(d))
              log("  theory provided in same document: " + theo)
            else
              log("  " + hd + " for " + theo)
          case _ =>
            log("  several documents found for: " + theo)
            log("    " + ds.mkString(" "))
        }
      }
    }
    result -= ((a, fp))
    log(if (result.isEmpty) "no dependencies" else "dependencies are: " + result.mkString(" "))
    result.map(p => BuildDependency(key, p._1, p._2))
  }
}

/** a combination of a Parser and a Checker
  *
  * @param parser  the parser
  * @param checker the checker
  */
class TwoStepInterpreter(val parser: Parser, val checker: Checker) extends Interpreter {
  def format = parser.format

  /** parses a [[ParsingStream]] and checks the result */
  def apply(ps: ParsingStream)(implicit errorCont: ErrorHandler): Document = {
    try {
      val doc = parser(ps)(errorCont)
      checker(doc)(new CheckingEnvironment(errorCont, RelationHandler.ignore))
      doc
    } finally {
      ps.stream.close
    }
  }
}

/** an interpreter created from a trusted parser */
class OneStepInterpreter(p: Parser) extends TwoStepInterpreter(p, NullChecker.structure)

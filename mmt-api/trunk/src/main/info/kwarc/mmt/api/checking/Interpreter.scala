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
  def apply(ps: ParsingStream)(implicit errorCont: ErrorHandler): StructuralElement

  /** converts the interface of [[Importer]] to the one of [[Parser]] */
  protected def buildTaskToParsingStream(bf: BuildTask): (DPath, ParsingStream) = {
    val inPathOMDoc = bf.inPath.toFile.setExtension("omdoc").toFilePath
    val dPath = DPath(bf.archive.narrationBase / inPathOMDoc.segments) // bf.narrationDPath except for extension
    val nsMap = controller.getNamespaceMap ++ bf.archive.namespaceMap
    val ps = new ParsingStream(bf.base / bf.inPath.segments, IsRootDoc(dPath), nsMap, format, File.Reader(bf.inFile))
    (dPath,ps)
  }

  /** creates a [[ParsingStream]] for the input file and interprets it */
  def importDocument(bf: BuildTask, index: Document => Unit): BuildResult = {
    val (dPath,ps) = buildTaskToParsingStream(bf)
    apply(ps)(bf.errorCont)
    val doc = try {
       controller.globalLookup.getAs(classOf[Document], dPath)
    } catch {
       case e: Error => throw LocalError("no document produced")
    }
    index(doc)
    val provided = doc.getModulesResolved(controller.globalLookup).map {m =>
       m.path
    } map LogicalDependency
    val used = Nil //TODO how to get exact dependencies at this point?
    //TODO how to return MissingDependency?
    if (bf.errorCont.hasNewErrors)
       BuildFailure(used, provided)
    else
       BuildSuccess(used, provided)
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
  def apply(ps: ParsingStream)(implicit errorCont: ErrorHandler): StructuralElement = {
    try {
      val se = parser(ps)(errorCont)
      checker(se)(new CheckingEnvironment(errorCont, RelationHandler.ignore))
      se
    } finally {
      ps.stream.close
    }
  }
}

/** an interpreter created from a trusted parser */
class OneStepInterpreter(parser: Parser) extends Interpreter {
    def format = parser.format
    def apply(ps: ParsingStream)(implicit errorCont: ErrorHandler) = parser(ps)
}

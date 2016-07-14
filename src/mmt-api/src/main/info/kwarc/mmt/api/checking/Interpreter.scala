package info.kwarc.mmt.api.checking

import info.kwarc.mmt.api._
import archives._
import documents._
import frontend.Controller
import ontology.{Declares, RelationExp}
import parser._
import utils._
import objects._

/** parses and returns a checked result */
abstract class Interpreter extends Importer {
  /** the input format (e.g., the file extension) to which this interpreter is applicable */
  def format: String

  def key = format + "-omdoc"

  override def isApplicable(s: String) = s == format || super.isApplicable(s)

  def inExts = List(format)

  /** structure interpretation */
  def apply(ps: ParsingStream)(implicit errorCont: ErrorHandler): StructuralElement
  /** object interpretation */
  def apply(pu: ParsingUnit)(implicit errorCont: ErrorHandler): Term

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
    val ce = new CheckingEnvironment(errorCont, RelationHandler.ignore)
    try {
      val cont = new StructureParserContinuations(errorCont) {
        override def onElement(se: StructuralElement) {
          //checker.checknewElement(se)(ce)
        }
        override def onElementEnd(se: ContainerElement[_]) {
          //checker.checkElementEnd(se)(ce)
        }
      }
      val se = parser(ps)(cont)
      checker(se)(ce)
      se
    } finally {
      ps.stream.close
    }
  }

  def apply(pu: ParsingUnit)(implicit errorCont: ErrorHandler): Term = {
    val pr = parser(pu)
    val cu = CheckingUnit.byInference(None, pu.context, pr)
    val rules = RuleSet.collectRules(controller, pu.context)
    val cr = checker(cu, rules)(new CheckingEnvironment(errorCont, RelationHandler.ignore))
    cr.term
  }
}

/** an interpreter created from a trusted parser */
class OneStepInterpreter(val parser: Parser) extends Interpreter {
    def format = parser.format
    def apply(ps: ParsingStream)(implicit errorCont: ErrorHandler) = {
      val cont = new StructureParserContinuations(errorCont)
      parser(ps)(cont)
    }
    def apply(pu: ParsingUnit)(implicit errorCont: ErrorHandler) = parser(pu).toTerm
}

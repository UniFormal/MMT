package info.kwarc.mmt.api.checking

import info.kwarc.mmt.api._
import archives._
import documents._
import frontend._
import modules._
import ontology.{Declares, RelationExp}
import parser._
import utils._
import objects._

import scala.util.Try

/** parses and returns a checked result */
abstract class Interpreter extends Importer {
  /** the input format (e.g., the file extension) to which this interpreter is applicable */
  def format: String

  val key = format + "-omdoc"

  override def isApplicable(s: String) = s == format || super.isApplicable(s)

  def inExts = List(format)

  /** structure interpretation */
  def apply(ps: ParsingStream)(implicit errorCont: ErrorHandler): StructuralElement
  /** object interpretation */
  def apply(pu: ParsingUnit)(implicit errorCont: ErrorHandler): CheckingResult

  /** convenience method for parsing and checking a term in context */
  def fromObjectString(context: Context, term: String, errorCont: ErrorHandler = ErrorThrower): CheckingResult = {
    val iic = new InterpretationInstructionContext(controller.getNamespaceMap)
    val pu = ParsingUnit(SourceRef.anonymous(term), context, term, iic)
    val cr = apply(pu)(errorCont)
    cr
  }

  def simplifier = controller.simplifier
  
  /** converts the interface of [[Importer]] to the one of [[Parser]] */
  protected def buildTaskToParsingStream(bf: BuildTask): (DPath, ParsingStream) = {
    val inPathOMDoc = bf.inPath.toFile.setExtension("omdoc").toFilePath
    val dPath = DPath(bf.archive.narrationBase / inPathOMDoc.segments) // bf.narrationDPath except for extension
    val nsMap = controller.getNamespaceMap ++ bf.archive.namespaceMap
    val ps = new ParsingStream(bf.base / bf.inPath.segments, IsRootDoc(dPath), nsMap, format, File.Reader(bf.inFile)).diesWith(bf)
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
    // TODO this is an ugly hack and should be replaced by a precise method. Requires some planning though; in the meantime it's better than nothing
    // TODO handle definiens
    val used = doc.getModulesResolved(controller.globalLookup).flatMap {
      case th : AbstractTheory => th.getAllIncludes.map(_.from) ::: th.getNamedStructures.map(_.from.toMPath)
      case v : View => v.domainAsContext.getIncludes ::: v.codomainAsContext.getIncludes ::: v.getAllIncludes.map(_.from)
    }.distinct.map(LogicalDependency)
    val missing = used.collect {
        case ld if Try(controller.getO(ld.mpath)).toOption.flatten.isEmpty => ld
      }
    if (missing.nonEmpty) {
      MissingDependency(missing,provided,used)
    } else if (bf.errorCont.hasNewErrors)
      BuildFailure(used, provided)
    else
      BuildSuccess(used, provided)
  }
}

/** a combination of a Parser and a Checker
  *
  * @param parser  the first step: parsing
  * @param checker the first part of the second step: checking
  * @param simplifier the second part of the second step: elaboration/simplification
  */
class TwoStepInterpreter(val parser: Parser, val checker: Checker, override val simplifier: uom.Simplifier) extends Interpreter {
  def format = parser.format

  /** parses a [[ParsingStream]] and checks the result */
  def apply(ps: ParsingStream)(implicit errorCont: ErrorHandler): StructuralElement = {
    val ce = new CheckingEnvironment(simplifier, errorCont, RelationHandler.ignore, ps)
    try {
      val cont = new StructureParserContinuations(errorCont) {
        override def onElement(se: StructuralElement) {
          checker.applyElementBegin(se)(ce)
        }
        override def onElementEnd(se: ContainerElement[_]) {
          checker.applyElementEnd(se)(ce)
        }
      }
      val se = parser(ps)(cont)
      //checker(se)(ce) // only needed for double-checking or if the parser ignores the StructureParserContinuations
      se
    } finally {
      ps.stream.close
    }
  }

  def apply(pu: ParsingUnit)(implicit errorCont: ErrorHandler) = {
    val pr = parser(pu)
    val cu = CheckingUnit.byInference(None, pu.context, pr).diesWith(pu)
    val rules = RuleSet.collectRules(controller, pu.context)
    val cr = checker(cu, rules)(new CheckingEnvironment(simplifier, errorCont, RelationHandler.ignore, cu))
    cr
  }
}

/** an interpreter created from a trusted parser */
class OneStepInterpreter(val parser: Parser) extends Interpreter {
    def format = parser.format
    def apply(ps: ParsingStream)(implicit errorCont: ErrorHandler) = {
      val cont = new StructureParserContinuations(errorCont)
      parser(ps)(cont)
    }
    def apply(pu: ParsingUnit)(implicit errorCont: ErrorHandler) = {
      val pr = parser(pu)
      val unk = pr.unknown
      val term = pr.copy(unknown = Context.empty).toTerm
      CheckingResult(unk.isEmpty, Some(unk), term)
    }
}

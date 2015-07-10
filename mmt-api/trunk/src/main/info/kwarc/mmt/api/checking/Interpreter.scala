package info.kwarc.mmt.api.checking

import info.kwarc.mmt.api._
import parser._
import archives._
import documents._
import utils._

/**
 * parses and returns a checked result
 */
abstract class Interpreter extends Importer {
   /** the input format (e.g., the file extension) to which this interpreter is applicable */
   def format: String
   def key = format + "-omdoc"
   override def isApplicable(s: String) = s == format || super.isApplicable(s)
   def inExts = List(format)

   /** parses a [[ParsingStream]] and returns a checked result */
   def apply(ps: ParsingStream)(implicit errorCont: ErrorHandler): Document

   /** creates a [[ParsingStream]] for the input file and interprets it */
   def importDocument(bf: BuildTask, index: Document => Unit) {
      val inPathOMDoc = bf.inPath.toFile.setExtension("omdoc").filepath
      val dpath = DPath(bf.base / inPathOMDoc.segments) // bf.narrationDPath except for extension
      val ps = new ParsingStream(bf.base / bf.inPath.segments, dpath, bf.archive.namespaceMap, format, File.Reader(bf.inFile))
      val doc = apply(ps)(bf.errorCont)
      index(doc)
   }
}
/**
 * a combination of a Parser and a Checker
 *
 * @param parser the parser
 * @param checker the checker
 *
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

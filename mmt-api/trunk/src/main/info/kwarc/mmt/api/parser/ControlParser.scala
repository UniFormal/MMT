package info.kwarc.mmt.api.parser
import info.kwarc.mmt.api._

/** A ParserExtension that sends commands to the controller */
class ControlParser extends ParserExtension {
   def isApplicable(se: StructuralElement, keyword: String) = keyword == "MMTControl"
   def apply(sp: StructureParser, s: ParserState, se: StructuralElement, keyword: String) {
      val (a, _) = s.reader.readAll
      controller.handleLine(a)
   }
}
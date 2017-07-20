package info.kwarc.mmt.api.parser
import info.kwarc.mmt.api._
import frontend._
import documents._
import info.kwarc.mmt.api.objects.Context
import modules._
import symbols._

/**
 * arguments passed to a [[ParserExtension]]
 * 
 * @param sp the StructureParser that is calling this extension
 * @param s the current parsing state
 * @param se the current structural element (Document, DeclaredModule, or Constant)
 * @param keyword the keyword that was read
 */ 
case class ParserExtensionArguments(parser: KeywordBasedParser, state: ParserState, se: StructuralElement, keyword: String, context:Context = Context.empty)

/**
 * classes implementing InDocParser may be registered with a [[StructureParser]]
 * to extend MMT's concrete syntax with new keywords
 */
abstract class ParserExtension extends Extension {
   /**
    * @param se the current structural element
    * @param keyword the keyword encountered within se
    * @return true iff this parser can parse at this position
    */
   def isApplicable(se: StructuralElement, keyword: String): Boolean
   /**
    * Called to parse a declaration if the respective keyword has been read.
    * the keyword but nothing else has been read already when this is called
    */
   def apply(pea: ParserExtensionArguments): Option[StructuralElement]
}
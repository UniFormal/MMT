package info.kwarc.mmt.api.parser
import info.kwarc.mmt.api._

object CommentIgnorer extends ParserExtension {
   private val keywords = List("//")
   def isApplicable(se: StructuralElement, kw: String) = keywords contains kw 
   def apply(sp: StructureParser, s: ParserState, se: StructuralElement, k: String) {}
}
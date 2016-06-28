package info.kwarc.mmt.api.parser
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.objects.Context

object CommentIgnorer extends ParserExtension {
   private val keywords = List("//")
   def isApplicable(se: StructuralElement, kw: String) = keywords contains kw 
   def apply(sp: KeywordBasedParser, s: ParserState, se: StructuralElement, k: String, con:Context = Context.empty) {}
}
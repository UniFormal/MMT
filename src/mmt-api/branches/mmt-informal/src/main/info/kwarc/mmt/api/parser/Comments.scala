package info.kwarc.mmt.api.parser
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.documents._

object CommentIgnorer extends ParserExtension {
   private val keywords = List("//")
   def isApplicable(se: StructuralElement, kw: String) = keywords contains kw 
   def apply(sp: StructureParser, s: ParserState, se: StructuralElement, k: String) {}
}


object CommentHandler extends ParserExtension {
  private val keywords = List("//")
  def isApplicable(se : StructuralElement, kw: String) = keywords contains kw
  
  def apply(sp: StructureParser, s: ParserState, se: StructuralElement, k: String) {
    val (commentS, _) = s.reader.readAll
    val narr = new PlainNarration(s.container, new NarrativeText(commentS))
    controller.add(narr)    
  }
}
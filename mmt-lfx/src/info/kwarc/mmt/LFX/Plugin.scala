package info.kwarc.mmt.LFX

import info.kwarc.mmt.api.objects.Context
import Subtyping._
import info.kwarc.mmt.api.symbols.FinalConstant
import info.kwarc.mmt.api.{DPath, SourceError, StructuralElement, frontend}
import info.kwarc.mmt.api.parser.{KeywordBasedParser, ParserExtension, ParserState, SourceRef}
import info.kwarc.mmt.api.utils.URI

object LFX {
  val ns = DPath(URI.http colon "gl.mathhub.info") / "MMT" / "LFX"
}

class SubTypeParserExt extends ParserExtension {

  def isApplicable(se: StructuralElement, keyword: String): Boolean = se match {
    case c:FinalConstant => c.tp.isEmpty && keyword == "<:"
    case _ => false
  }

  def apply(sp: KeywordBasedParser, s: ParserState, se: StructuralElement, keyword: String,con:Context) = se match {
    case c:FinalConstant if keyword == "<:" =>
      val (obj, reg, tm) = sp.readParsedObject(con)(s)
      c.tpC.read = obj
      c.tpC.parsed = subtypeOf(tm)
    case _ => s.errorCont(SourceError("SubTypeParserExt", SourceRef(s.ps.source, s.startPosition.toRegion),
      "not applicable to StructuralElement "+se.getClass.toString))
  }
}

class Plugin extends frontend.Plugin {
  val theory = Subtyping.SubTyped.path
  val dependencies = List("info.kwarc.mmt.lf.Plugin")
  override def start(args: List[String]) {
    val em = controller.extman
    // content enhancers
    em.addExtension(new SubTypeParserExt)
  }
}

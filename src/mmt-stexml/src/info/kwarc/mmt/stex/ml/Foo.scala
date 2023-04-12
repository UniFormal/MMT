package info.kwarc.mmt.stex.ml

import info.kwarc.mmt.api.utils.File
import info.kwarc.mmt.stex.lsp.languagemodel.HuggingFaceTokenModel
import info.kwarc.mmt.stex.parsing.stex.{DefinameRule, DefiniendumRule, ImportModuleRuleLike, SymDeclRule, SymDefRule, SymdeclApp, SymnameRule, SymnamesRule, SymrefApp, SymrefRule, TextSymDeclRule, UseStructureRule, VarDefRule, VarSeqRule}
import info.kwarc.mmt.stex.parsing.{MacroApplication, PlainMacro, PlainText, TeXTokenLike}

sealed trait TargetClass
case object NonSemantic extends TargetClass
case object Semantic extends TargetClass
case class PreToken(str:String,tk: TeXTokenLike,cls:TargetClass)

object Foo {
  def foo(top:List[TeXTokenLike],macrostrip:Boolean) = {
    var ret : List[PreToken] = Nil
    def dostuff(tk:TeXTokenLike) : Unit = tk match {
      case txt: PlainText if !txt.str.isBlank =>
        ret ::= PreToken(txt.str, txt, NonSemantic)
      case ma: SymrefApp => ma.rule match {
        case Some(SymrefRule(_,_) | SymnameRule(_,_,_) | SymnamesRule(_,_,_) | DefinameRule(_,_,_) | DefiniendumRule(_)) => ret ::= PreToken(ma.asPlainString,ma,Semantic)
        //case _ if !macrostrip => ma.iterateChildren(dostuff)
        case _ =>
      }
      case ma: MacroApplication => ma.rule match {
        case Some(_:SymDeclRule | _:TextSymDeclRule | _:SymDefRule | _:VarDefRule | _:VarSeqRule |_:ImportModuleRuleLike|_:UseStructureRule) =>
        case _ => ma.iterateChildren(dostuff)
      }
      case pm:PlainMacro if !macrostrip => ret ::= PreToken(pm.toString,pm,NonSemantic)
      case o => o.iterateChildren(dostuff)
    }
    top.foreach(dostuff)
    ret.reverse
  }
  private var model : HuggingFaceTokenModel = null
  def initialize(zipfile: File) = {
    model = new HuggingFaceTokenModel
    model.initialize(zipfile.toString)
  }
  def infer(ls : List[PreToken],cutoff:Double) = {
    import scala.jdk.CollectionConverters._
    ls.zip(model.predict(ls.map(_.str).toArray).groupedPredictions.asScala.toList.map(_.asScala.toList)).collect {
      case (a,b) if a.cls == NonSemantic && b.exists(_.positiveCertainty > cutoff) =>
        b.filter(_.positiveCertainty > cutoff).map(e => (a.tk.startoffset + e.charSpan.getStart, a.tk.startoffset + e.charSpan.getEnd))
    }.flatten
  }
}

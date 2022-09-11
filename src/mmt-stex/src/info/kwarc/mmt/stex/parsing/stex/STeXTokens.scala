package info.kwarc.mmt.stex.parsing.stex

import info.kwarc.mmt.api.{GlobalName, Level, LocalName, MPath, Path, SourceError}
import info.kwarc.mmt.api.parser.{SourcePosition, SourceRef, SourceRegion}
import info.kwarc.mmt.api.utils.URI
import info.kwarc.mmt.stex.lsp.sTeXDocument
import info.kwarc.mmt.stex.parsing.{Environment, EnvironmentRule, MacroApplication, PlainMacro, TeXRule, TeXTokenLike}
import org.eclipse.lsp4j.SymbolKind

import scala.collection.mutable

trait HasAnnotations extends TeXTokenLike {
  def doAnnotations(in:sTeXDocument) : Unit
}

abstract class TeXModuleLike(pm:PlainMacro,val mp:MPath,ch:List[TeXTokenLike],rl:EnvironmentRule) extends MacroApplication(pm,ch,rl) {
  val sig : String
}
class StructureModuleBegin(pm:PlainMacro, mp:MPath, val dom:DictionaryModule, ch:List[TeXTokenLike], val rl:StructureLikeRule)
  extends TeXModuleLike(pm,mp,ch,rl) {
  val sig = ""
  val fields = mutable.HashMap.empty[SemanticMacro,(Option[LocalName],String,Boolean)]
  lazy val domfields = dom.getRules("").collect {
    case sm : SemanticMacro => sm
  }
}
case class StructureModule(bg:StructureModuleBegin,en:TeXTokenLike,ch:List[TeXTokenLike]) extends Environment(bg,en,ch,Some(bg.rl)) with HasAnnotations {
  override def doAnnotations(in: sTeXDocument): Unit = {
    val a = in.Annotations.add(this, startoffset, endoffset - startoffset, SymbolKind.Module, bg.mp.toString, true)
    val mac = in.Annotations.add(bg, bg.startoffset, bg.endoffset - bg.startoffset, SymbolKind.Constructor, "module")
    val mace = in.Annotations.add(end, end.startoffset, end.endoffset - end.startoffset, SymbolKind.Constructor, "module")
    mac.setHover(bg.mp.toString)
    //mac.addInlay(mod.bg.mp.toString,kind = Some(InlayHintKind.Type),positionOffset = mod.bg.endoffset,padleft = true)
    mac.addCodeLens("Structure " + bg.mp.name.last.toString + ": " + bg.dom.path.toString, "", Nil, bg.startoffset, bg.endoffset)
    //mace.setHover(bg.mp.toString)
    bg.children.take(2).foreach { c =>
      val a = in.Annotations.add(this, c.startoffset, c.endoffset - c.startoffset, SymbolKind.Module, bg.mp.toString)
      a.setSemanticHighlightingClass(0)
    }
    mace.setSemanticHighlightingClass(0)
    //mace.addInlay(mod.bg.mp.toString,kind = Some(InlayHintKind.Type),positionOffset = mod.end.endoffset,padleft = true)
    //mace.addCodeLens(bg.mp.toString, "", Nil, end.startoffset, end.endoffset)
  }
}
case class TeXModuleMacro(
                           pm:PlainMacro,
                           mpi:MPath,
                           ch:List[TeXTokenLike],
                           rl : EnvironmentRule,
                           meta:Option[MPath],
                           sig:String = "",
                           lang:String="",
                           title:String="",deprecation:String = "") extends TeXModuleLike(pm,mpi,ch,rl)

case class TeXModule(bg:TeXModuleMacro,en:TeXTokenLike,ch:List[TeXTokenLike],rl:EnvironmentRule) extends Environment(bg,en,ch,Some(rl)) with HasAnnotations {
  override def doAnnotations(in: sTeXDocument): Unit = {
    val a = in.Annotations.add(this, startoffset, endoffset - startoffset, SymbolKind.Module, bg.mp.toString, true)
    val mac = in.Annotations.add(bg, bg.startoffset, bg.endoffset - bg.startoffset, SymbolKind.Constructor, "module")
    val mace = in.Annotations.add(end, end.startoffset, end.endoffset - end.startoffset, SymbolKind.Constructor, "module")
    mac.setHover(bg.mp.toString)
    //mac.addInlay(mod.bg.mp.toString,kind = Some(InlayHintKind.Type),positionOffset = mod.bg.endoffset,padleft = true)
    mac.addCodeLens(bg.mp.toString,"",Nil,bg.startoffset,bg.endoffset)
    mace.setHover(bg.mp.toString)
    bg.children.take(2).foreach {c =>
      val a = in.Annotations.add(this, c.startoffset, c.endoffset - c.startoffset, SymbolKind.Module, bg.mp.toString)
      a.setSemanticHighlightingClass(0)
    }
    mace.setSemanticHighlightingClass(0)
    //mace.addInlay(mod.bg.mp.toString,kind = Some(InlayHintKind.Type),positionOffset = mod.end.endoffset,padleft = true)
    //mace.addCodeLens(bg.mp.toString,"",Nil,end.startoffset,end.endoffset)
  }
}


case class ImportModuleApp(
                            pm:PlainMacro,
                            mp:MPath,
                            ch:List[TeXTokenLike],
                            rl:ImportModuleRuleLike,
                            archivestring:String,
                            path:String,
                            isusemodule:Boolean,
                            parent:Option[DictionaryModule]
                          ) extends MacroApplication(pm,ch,rl) with HasAnnotations {
  override def doAnnotations(in: sTeXDocument): Unit = {
    val a = in.Annotations.add(this,startoffset,endoffset - startoffset,SymbolKind.Module,symbolname = "import")
    //a.addInlay(im.mp.toString,kind = Some(InlayHintKind.Type),positionOffset = im.endoffset,padleft = true)
    val pma = in.Annotations.add(pm,pm.startoffset,pm.endoffset - pm.startoffset,SymbolKind.Module,symbolname = "import")
    pma.setSemanticHighlightingClass(0)
    a.addCodeLens(mp.toString,"",Nil,startoffset,endoffset)
    rl.dict.all_modules.get(mp).foreach {mod =>
      mod.file.foreach(f => a.setDeclaration(f.toURI.toString,mod.macr.startoffset,mod.macr.endoffset))
      mod.macr match {
        case tmm:TeXModuleMacro =>
          tmm.deprecation match {
            case "" =>
            case dep =>
              pma.setSemanticHighlightingClass(0,List(0))
              val start = in._doctext.toLC(startoffset)
              val end = in._doctext.toLC(endoffset)
              in.client.documentErrors(rl.dict.controller,in,in.uri,SourceError(in.uri,SourceRef(URI(in.uri),
                SourceRegion(
                  SourcePosition(startoffset,start._1,start._2),
                  SourcePosition(endoffset,end._1,end._2)
                )),mp.toString + " is deprecated",List("Use " + dep + " instead"),Level.Warning)
              )
          }
        case _ =>
      }
    }
  }
}

trait SymDeclLike extends MacroApplication {
  val syminfo:SymdeclInfo
}
trait NotationLike extends MacroApplication with TeXRule {
  val notinfo:NotationInfo
}
trait SetNotationLike extends MacroApplication with NotationLike {
  val notinfo:NotationInfo
}
trait SymRefLike extends MacroApplication with HasAnnotations {
  val syminfo:SymdeclInfo
  val alternatives : (TeXTokenLike,List[(GlobalName,String)])
  def doInit = if (alternatives._2.nonEmpty) {
    addError("Ambiguous symbol reference",lvl = Level.Warning)
  }
  override def doAnnotations(in: sTeXDocument): Unit = {
    val a = in.Annotations.add(this,startoffset,endoffset - startoffset,SymbolKind.Constant,syminfo.path.toString)
    a.setDeclaration(syminfo.file,syminfo.start,syminfo.end)
    val pla = in.Annotations.add(this.plain,this.plain.startoffset,this.plain.endoffset - this.plain.startoffset,SymbolKind.Constant,syminfo.path.toString)
    pla.setSemanticHighlightingClass(2)
    val start = in._doctext.toLC(alternatives._1.startoffset)
    val end = in._doctext.toLC(alternatives._1.endoffset)
    alternatives._2.foreach {alt =>
      a.addCodeActionEdit(alt._1.toString,"quickfix",List((in.uri,List((start._1,start._2+1,end._1,end._2-1,alt._2)))))
      /*a.addCodeLens("Disambiguate: " + alt,"stexide.insertCode",List(
        alt.asInstanceOf[AnyRef],start._1.asInstanceOf[AnyRef],(start._2+1).asInstanceOf[AnyRef],
        end._1.asInstanceOf[AnyRef],(end._2-1).asInstanceOf[AnyRef]),startoffset,endoffset)*/
    }
    a.setHover(syminfo.path.toString)
  }
}

case class SemanticMacroApp(pl:PlainMacro,rl:SemanticMacro,ch:List[TeXTokenLike],not:Option[NotationInfo]) extends MacroApplication(pl,ch,rl) with HasAnnotations {
  override def doAnnotations(in: sTeXDocument): Unit = {
    val a = in.Annotations.add(this,startoffset,endoffset - startoffset,SymbolKind.Constant,rl.syminfo.path.toString)
    val pma = if (children.isEmpty) a else in.Annotations.add(pl,pl.startoffset,pl.endoffset - pl.startoffset,SymbolKind.Constant)
    pma.setDeclaration(rl.syminfo.file,rl.syminfo.start,rl.syminfo.end)
    pma.setSemanticHighlightingClass(2)
    pma.setHover(rl.syminfo.path.toString + (not match {
      case Some(n) => "#" + n.id
      case _ => ""
    }))
  }
}
case class SemanticVariableApp(pl:PlainMacro,rl:VariableRule,ch:List[TeXTokenLike],not:Option[NotationInfo]) extends MacroApplication(pl,ch,rl) with HasAnnotations {
  override def doAnnotations(in: sTeXDocument): Unit = {
    val a = in.Annotations.add(this,startoffset,endoffset - startoffset,SymbolKind.Constant,rl.syminfo.path.toString)
    val pma = if (children.isEmpty) a else in.Annotations.add(pl,pl.startoffset,pl.endoffset - pl.startoffset,SymbolKind.Constant)
    pma.setDeclaration(rl.syminfo.file,rl.syminfo.start,rl.syminfo.end)
    pma.setSemanticHighlightingClass(3)
    pma.setHover("Variable " + rl.syminfo.path.name.toString)
  }
}
case class StructureFieldApp(pl:PlainMacro,rl:InstanceFieldRule,ch:List[TeXTokenLike],orig:Option[SymdeclInfo],not:Option[NotationInfo]) extends MacroApplication(pl,ch,rl) with HasAnnotations {
  override def doAnnotations(in: sTeXDocument): Unit = {
    val a = in.Annotations.add(this,startoffset,endoffset - startoffset,SymbolKind.Constant,rl.syminfo.path.toString)
    val pma = if (children.isEmpty) a else in.Annotations.add(pl,pl.startoffset,pl.endoffset - pl.startoffset,SymbolKind.Constant)
    pma.setDeclaration(rl.syminfo.file,rl.syminfo.start,rl.syminfo.end)
    pma.setSemanticHighlightingClass(3)
    pma.setHover("Variable " + rl.syminfo.path.name.toString)
  }
}
case class VarStructureFieldApp(pl:PlainMacro,rl:VarInstanceFieldRule,ch:List[TeXTokenLike],orig:Option[SymdeclInfo],not:Option[NotationInfo]) extends MacroApplication(pl,ch,rl) with HasAnnotations {
  override def doAnnotations(in: sTeXDocument): Unit = {
    val pma = orig match {
      case None =>
        val a = in.Annotations.add(this,pl.startoffset,pl.endoffset - pl.startoffset,SymbolKind.Constant,rl.syminfo.path.toString)
        a.setDeclaration(rl.syminfo.file, rl.syminfo.start, rl.syminfo.end)
        a.setHover("Variable " + rl.syminfo.path.name.toString)
        a
      case Some(sd) =>
        val a = in.Annotations.add(this,pl.startoffset,children.head.endoffset - pl.startoffset,SymbolKind.Constant)
        a.setDeclaration(sd.file,sd.start,sd.end)
        a.setHover("Variable " + rl.syminfo.path.name.toString + "\nField " + sd.path)
        a
    }
    pma.setDeclaration(rl.syminfo.file,rl.syminfo.start,rl.syminfo.end)
    pma.setSemanticHighlightingClass(3)
  }
}

case class SymdeclApp(pl:PlainMacro,ch:List[TeXTokenLike],rl:SymDeclRuleLike,
                      syminfo:SymdeclInfo) extends MacroApplication(pl,ch,rl)
  with SymDeclLike with SemanticMacro with HasAnnotations {
  override def doAnnotations(in: sTeXDocument): Unit = {
    val a = in.Annotations.add(this,startoffset,endoffset - startoffset,SymbolKind.Function,symbolname = syminfo.path.toString)
    val pma = in.Annotations.add(pl,pl.startoffset,pl.endoffset - pl.startoffset,SymbolKind.Constant)
    pma.setSemanticHighlightingClass(1)
    //a.addInlay(im.mp.toString,kind = Some(InlayHintKind.Type),positionOffset = im.endoffset,padleft = true)
    a.addCodeLens(syminfo.path.toString,"",Nil,startoffset,endoffset)
    if (syminfo.local) {
      pma.setSemanticHighlightingClass(1,List(0))
      val start = in._doctext.toLC(startoffset)
      val end = in._doctext.toLC(endoffset)
      in.client.documentErrors(rl.dict.controller,in,in.uri,SourceError(in.uri,SourceRef(URI(in.uri),
        SourceRegion(
          SourcePosition(startoffset,start._1,start._2),
          SourcePosition(endoffset,end._1,end._2)
        )),"key \"local\" is deprecated",List("Please use a variable instead"),Level.Warning)
      )
    }
  }
}

case class SymdefApp(pl:PlainMacro,ch:List[TeXTokenLike],rl:STeXRules.SymDefRule,
                     syminfo:SymdeclInfo,notinfo:NotationInfo) extends MacroApplication(pl,ch,rl)
  with SymDeclLike with NotationLike with SetNotationLike with SemanticMacro with HasAnnotations {
  override def doAnnotations(in: sTeXDocument): Unit = {
    val a = in.Annotations.add(this,startoffset,endoffset - startoffset,SymbolKind.Function,symbolname = syminfo.path.toString)
    val pma = in.Annotations.add(pl,pl.startoffset,pl.endoffset - pl.startoffset,SymbolKind.Constant)
    pma.setSemanticHighlightingClass(1)
    //a.addInlay(im.mp.toString,kind = Some(InlayHintKind.Type),positionOffset = im.endoffset,padleft = true)
    a.addCodeLens(syminfo.path.toString + "#" + notinfo.id,"",Nil,startoffset,endoffset)
    if (syminfo.local) {
      pma.setSemanticHighlightingClass(1,List(0))
      val start = in._doctext.toLC(startoffset)
      val end = in._doctext.toLC(endoffset)
      in.client.documentErrors(rl.dict.controller,in,in.uri,SourceError(in.uri,SourceRef(URI(in.uri),
        SourceRegion(
          SourcePosition(startoffset,start._1,start._2),
          SourcePosition(endoffset,end._1,end._2)
        )),"key \"local\" is deprecated",List("Please use a variable instead"),Level.Warning)
      )
    }
  }
}
case class VardefApp(pl:PlainMacro,ch:List[TeXTokenLike],rl:STeXRules.VarDefRule,macroname:String,_name:String,args:String,assoctype:String,file:String,end:Int,defd:Boolean) extends MacroApplication(pl,ch,rl)
  with VariableRule with HasAnnotations {
  override val syminfo: SymdeclInfo = SymdeclInfo(macroname,Path.parseS("var://foo?foo?" + _name),args,assoctype,false,file,plain.startoffset,end,defd)
  override def doAnnotations(in: sTeXDocument): Unit = {
    //val a = in.Annotations.add(this,startoffset,endoffset - startoffset,SymbolKind.Function,symbolname = syminfo.path.toString)
    val pma = in.Annotations.add(pl,pl.startoffset,pl.endoffset - pl.startoffset,SymbolKind.Constant)
    pma.setSemanticHighlightingClass(1)
  }
}

case class InstanceApp(pl:PlainMacro,ch:List[TeXTokenLike],rl:STeXRules.InstanceRule,macroname:String,path:GlobalName,module:DictionaryModule,file:String,end:Int)
  extends MacroApplication(pl,ch,rl) with InstanceFieldRule with HasAnnotations {
  val syminfo:SymdeclInfo = SymdeclInfo(macroname,path,"","",false,file,pl.startoffset,end,false)

  override def doAnnotations(in: sTeXDocument): Unit = {
    val a = in.Annotations.add(this, startoffset, endoffset - startoffset, SymbolKind.Function, symbolname = syminfo.path.toString)
    val pma = in.Annotations.add(pl, pl.startoffset, pl.endoffset - pl.startoffset, SymbolKind.Constant)
    pma.setSemanticHighlightingClass(1)
    a.addCodeLens(syminfo.path.toString, "", Nil, startoffset, endoffset)
  }
}
case class VarInstanceApp(pl:PlainMacro,ch:List[TeXTokenLike],rl:STeXRules.VarInstanceRule,macroname:String,_name:String,module:DictionaryModule,file:String,end:Int)
  extends MacroApplication(pl,ch,rl) with VarInstanceFieldRule with HasAnnotations {
  override val syminfo: SymdeclInfo = SymdeclInfo(macroname,Path.parseS("var://foo?foo?" + _name),"","",false,file,plain.startoffset,end,false)

  override def doAnnotations(in: sTeXDocument): Unit = {
    //val a = in.Annotations.add(this, startoffset, endoffset - startoffset, SymbolKind.Function, symbolname = syminfo.path.toString)
    val pma = in.Annotations.add(pl, pl.startoffset, pl.endoffset - pl.startoffset, SymbolKind.Constant)
    pma.setSemanticHighlightingClass(1)
  }
}

case class NotationApp(pl:PlainMacro,ch:List[TeXTokenLike],rl:STeXRules.NotationRule,
                       notinfo:NotationInfo) extends MacroApplication(pl,ch,rl)
  with NotationLike with HasAnnotations {
  val name = "Notation " + notinfo.id + " for " + notinfo.syminfo.path.toString

  override def doAnnotations(in: sTeXDocument): Unit = {
    val a = in.Annotations.add(this,startoffset,endoffset - startoffset,SymbolKind.Function,symbolname = notinfo.syminfo.path.toString + "#" + notinfo.id)
    val pma = in.Annotations.add(pl,pl.startoffset,pl.endoffset - pl.startoffset,SymbolKind.Constant)
    pma.setSemanticHighlightingClass(1)
    //a.addInlay(im.mp.toString,kind = Some(InlayHintKind.Type),positionOffset = im.endoffset,padleft = true)
    a.addCodeLens(notinfo.syminfo.path.toString + "#" + notinfo.id,"",Nil,startoffset,endoffset)
  }
}

case class BeginStatement(pl:PlainMacro,ch:List[TeXTokenLike],rl:StatementRule,name:Option[String]) extends MacroApplication(pl,ch,rl)
abstract class Statement(bg: BeginStatement,en:TeXTokenLike,ch:List[TeXTokenLike],rl:StatementRule) extends Environment(bg,en,ch,Some(rl)) with HasAnnotations {
  override def doAnnotations(in: sTeXDocument): Unit = {}
}
abstract class InlineStatement(pl:PlainMacro,ch:List[TeXTokenLike],rl:InlineStatementRule) extends MacroApplication(pl,ch,rl)

case class MathStructure(bg:MathStructureMacro,en:TeXTokenLike,ch:List[TeXTokenLike],rl:STeXRules.MathStructureRule) extends Environment(bg,en,ch,Some(rl)) with HasAnnotations {
  override def doAnnotations(in: sTeXDocument): Unit = {
    val a = in.Annotations.add(this, startoffset, endoffset - startoffset, SymbolKind.Module, bg.mp.toString, true)
    val mac = in.Annotations.add(bg, bg.startoffset, bg.endoffset - bg.startoffset, SymbolKind.Constructor, "module")
    val mace = in.Annotations.add(end, end.startoffset, end.endoffset - end.startoffset, SymbolKind.Constructor, "module")
    mac.setHover(bg.mp.toString)
    //mac.addInlay(mod.bg.mp.toString,kind = Some(InlayHintKind.Type),positionOffset = mod.bg.endoffset,padleft = true)
    mac.addCodeLens(bg.mp.toString,"",Nil,bg.startoffset,bg.endoffset)
    mace.setHover(bg.mp.toString)
    bg.children.take(2).foreach {c =>
      val a = in.Annotations.add(this, c.startoffset, c.endoffset - c.startoffset, SymbolKind.Module, bg.mp.toString)
      a.setSemanticHighlightingClass(0)
    }
    mace.setSemanticHighlightingClass(0)
    //mace.addInlay(mod.bg.mp.toString,kind = Some(InlayHintKind.Type),positionOffset = mod.end.endoffset,padleft = true)
    mace.addCodeLens(bg.mp.toString,"",Nil,end.startoffset,end.endoffset)
  }
}
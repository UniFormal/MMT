package info.kwarc.mmt.stex.parsing.stex

import info.kwarc.mmt.api.utils.URI
import info.kwarc.mmt.api.{ComplexStep, DPath, GlobalName, Level, LocalName, MPath, NamespaceMap, Path}
import info.kwarc.mmt.lsp.Annotation
import info.kwarc.mmt.stex.lsp.{SemanticHighlighting, sTeXDocument}
import info.kwarc.mmt.stex.parsing.{Begin, End, Environment, EnvironmentRule, Group, Grouped, LaTeXParseError, LaTeXParser, MacroApplication, MacroRule, OptionalArgument, ParseState, PlainMacro, PlainText, RuleContainer, Single, SkipCommand, TeXRule, TeXRules, TeXTokenLike}
import org.eclipse.lsp4j.{InlayHintKind, SymbolKind}

import scala.collection.mutable
import scala.util.Try

object STeXRules {
  def allRules(dict: Dictionary) : List[TeXRule] = List(
    ModuleRule(dict),ProblemRule(dict),UseModuleRule(dict),UseStructureRule(dict),
    SymuseRule(dict),
    SymrefRule("symref",dict),SymrefRule("sr",dict),
    SymnameRule("symname",dict,false),SymnameRule("Symname",dict,true),
    SymnameRule("sn", dict, false), SymnameRule("Sn", dict, true),
    SymnamesRule("sns", dict, false), SymnamesRule("Sns", dict, true),
    SkipCommand("stexstyleassertion","ovv"),
    SkipCommand("stexstyledefinition", "ovv"),
    SkipCommand("stexstyleparagraph", "ovv"),
    SkipCommand("stexstyleexample", "ovv"),
    SkipCommand("stexstyleproblem", "ovv"),
    VarDefRule(dict),VarSeqRule(dict),
    SDefinitionRule(dict),SAssertionRule(dict),SParagraphRule(dict),
    InlineDefRule(dict),InlineAssRule(dict:Dictionary),
    InputrefRule(dict)
  )
  def moduleRules(dict:Dictionary) : List[TeXRule] = List(
    SymDeclRule(dict),SymDefRule(dict),NotationRule(dict),TextSymDeclRule(dict),
    ImportModuleRule(dict),MathStructureRule(dict),ExtStructureRule(dict),ExtStructureStarRule(dict),
    CopyModRule(dict),CopyModuleRule(dict),
    InterpretModRule(dict), InterpretModuleRule(dict),
    RealizeRule(dict),RealizationRule(dict)
  )
}

trait STeXRule extends TeXRule {
  val dict:Dictionary
  def addExportRule(rl : TeXRule) = dict.getModuleOpt.foreach{ mod =>
      mod.exportrules ::= rl
      mod.rules(rl.name) = rl
  }
}
trait NonFormalRule extends TeXRule
trait HasAnnotations extends TeXTokenLike {
  def doAnnotations(in:sTeXDocument) : Unit = {}
}
trait STeXMacro extends MacroApplication with HasAnnotations {
  override def asPlain: String = ""
}
abstract class STeXEnvironment(begin:Begin.BeginMacro) extends Environment(begin) with STeXMacro {
  override def asPlain: String = children.map(_.asPlain).mkString
}
trait STeXModuleLike extends STeXEnvironment {
  val sig : String
  val mp:MPath
}
class STeXModule(begin:Begin.BeginMacro,val mp:MPath,val meta:Option[MPath],lang:String,val sig:String) extends STeXEnvironment(begin) with STeXModuleLike {
  override def doAnnotations(in: sTeXDocument): Unit = {
    in.Annotations.add(this,startoffset,endoffset - startoffset,SymbolKind.Module,mp.toString,true)
    val bg = in.Annotations.add(begin,begin.startoffset,begin.endoffset - begin.startoffset)
    bg.setHover(mp.toString)
    bg.addCodeLens(mp.toString, "", Nil, begin.startoffset, begin.endoffset)
    bg.setSemanticHighlightingClass(SemanticHighlighting.module)
    this.end.foreach {end =>
      val e = in.Annotations.add(end, end.startoffset, end.endoffset - end.startoffset)
      e.setHover(mp.toString)
      e.setSemanticHighlightingClass(SemanticHighlighting.module)
    }
  }
}
case class ModuleRule(dict : Dictionary) extends STeXRule with EnvironmentRule {
  val envname = "smodule"

  override def applyStart(implicit parser: ParseState[Begin.BeginMacro]): STeXModule = {
    import parser._
    val opts = readOptAgument
    val name = readArgument.asname
    val meta = opts.flatMap(_.consumeStr("meta")).map{
      case "" => None
      case o => Some(Path.parseM(o))
    }.getOrElse(Some(dict.defaultMeta))
    val lang = opts.flatMap(_.consumeStr("lang")).getOrElse(dict.getLanguage)
    val sig = opts.flatMap(_.consumeStr("sig")).getOrElse("")
    opts.foreach(_.drop("title","deprecate","creators","contributors","id","srccite"))
    val mp = opts.flatMap(_.consumeStr("ns")) match {
      case Some(s) =>
        Try(Path.parseD(s,NamespaceMap.empty)).toOption match {
          case Some(dp) => dp ? LocalName.parse(name)
          case _ => dict.getMPath(name)
        }
      case _ => dict.getMPath(name)
    }
    val macr = new STeXModule(parser.trigger,mp,meta,lang,sig)
    //if (deprecation != "") macr.addError("Deprecated: Use " + deprecation + " instead",lvl=Level.Warning)
    dict.openModule(macr)
  }

  override def applyEnd(env: Environment)(implicit parser: ParseState[Begin.BeginMacro]): Environment = super.applyEnd(env) match {
    case tmm: STeXModule =>
      dict.closeModule
      tmm
    case _ => env
  }
}

case class ProblemRule(dict : Dictionary) extends STeXRule with EnvironmentRule {
  val envname = "sproblem"

  override def applyStart(implicit parser: ParseState[Begin.BeginMacro]): STeXModule = {
    import parser._
    val opts = readOptAgument
    val name = opts.flatMap(_.consumeStr("name")).getOrElse {
      dict.getCurrentNS.map(_.last).getOrElse("")
    }
    val meta = Some(dict.defaultMeta)
    val lang = opts.flatMap(_.consumeStr("lang")).getOrElse(dict.getLanguage)
    opts.foreach(_.drop("title","deprecate","creators","contributors","id","srccite"))
    val mp = dict.getMPath(name)
    val macr = new STeXModule(parser.trigger,mp,meta,lang,"")
    //if (deprecation != "") macr.addError("Deprecated: Use " + deprecation + " instead",lvl=Level.Warning)
    dict.openModule(macr)
  }

  override def applyEnd(env: Environment)(implicit parser: ParseState[Begin.BeginMacro]): Environment = super.applyEnd(env) match {
    case tmm: STeXModule =>
      dict.closeModule
      tmm
    case _ => env
  }
}

case class InheritModuleRule(dm : DictionaryModule,export:Boolean) extends STeXMacro with TeXRule {
  val name = "inherits " + export.toString + " " + dm.path.toString

  override def doAnnotations(in: sTeXDocument): Unit = {
    val a = in.Annotations.add(this,startoffset,endoffset - startoffset)
    a.addCodeLens(dm.path.toString,"",Nil,startoffset,endoffset)
    dm.file.foreach {f =>
      a.addDefinition(f.toURI.toString,dm.macr.startoffset,dm.macr.endoffset)
    }
    this.plain.foreach {plain =>
      in.Annotations.add(plain,plain.startoffset,plain.endoffset - plain.startoffset)
        .setSemanticHighlightingClass(SemanticHighlighting.module)
    }
  }
}
trait ImportModuleRuleLike extends STeXRule with MacroRule {
  val export: Boolean
  override def apply(implicit parser: ParseState[PlainMacro]): InheritModuleRule = {
    import parser._
    val archive = readOptAgument.map(_.asname).getOrElse("")
    val path = readArgument.asname
    val mp = dict.resolveMPath(archive,path)
    dict.addimport(mp,archive,path,export)
  }
}

case class InputrefRule(dict:Dictionary) extends STeXRule with MacroRule with NonFormalRule {
  val name = "inputref"

  override def apply(implicit parser: ParseState[PlainMacro]): MacroApplication = {
    parser.readChar('*')
    val archive = parser.readOptAgument.map(_.asname).getOrElse("")
    val path = parser.readArgument.asname
    val file = dict.resolveFilePath(archive,path)
    val ret = new MacroApplication with STeXMacro {
      if (!file.exists()) addError("File not found: " + file.toString)

      override def doAnnotations(in: sTeXDocument): Unit = {
        val a = in.Annotations.add(this,this.startoffset,endoffset - startoffset)
        if (file.exists()) {
          a.addDefinition(file.toString,0,0)
          a.addCodeLens(file.toString,"",Nil,startoffset,endoffset)
        }
        a.setSemanticHighlightingClass(SemanticHighlighting.file)
      }
    }
    ret
  }
}

case class UseStructureRule(dict:Dictionary) extends STeXRule with MacroRule with NonFormalRule {
  val name = "usestructure"

  override def apply(implicit parser: ParseState[PlainMacro]): InheritModuleRule = {
    import parser._
    val path = readArgument.asname
    val mod = dict.structure(path)
    val ret = new InheritModuleRule(mod, false) {
      override def doAnnotations(in: sTeXDocument): Unit = {
        val path = mod.macr.asInstanceOf[MathStructure].syminfo.path.toString
        val a = in.Annotations.add(this, startoffset, endoffset - startoffset)
        a.addCodeLens(path, "", Nil, startoffset, endoffset)
        dm.file.foreach { f =>
          a.addDefinition(f.toURI.toString, dm.macr.startoffset, dm.macr.endoffset)
        }
        this.plain.foreach { plain =>
          in.Annotations.add(plain, plain.startoffset, plain.endoffset - plain.startoffset)
            .setSemanticHighlightingClass(SemanticHighlighting.module)
        }
      }
    }
    val head = parser.latex.groups.head
    head.rules(ret.name) = ret
    ret.dm.getRules(dict.getLanguage).foreach { rl =>
      head.rules(rl.name) = rl
    }
    ret
  }
}
case class ImportModuleRule(dict : Dictionary) extends ImportModuleRuleLike {
  val export = true
  val name = "importmodule"
}
case class UseModuleRule(dict : Dictionary) extends ImportModuleRuleLike with NonFormalRule {
  val export = false
  val name = "usemodule"
}

class SymdeclInfo(val macroname:String,val path:GlobalName,val args:String,val file:String,val start:Int,val end:Int,var defined:Boolean,val ret:Option[List[TeXTokenLike]]) {
  def arity = args.length
  var isDocumented = false

  def copy(macroname : String = this.macroname,path:GlobalName = this.path, args:String = this.args,file:String=this.file,start:Int = this.start,end:Int=this.end,defined:Boolean=this.defined,ret:Option[List[TeXTokenLike]] = this.ret) = {
    new SymdeclInfo(macroname,path, args, file, start, end, defined, ret)
  }
}
case class NotationInfo(syminfo:SymdeclInfo,id:String,notation:List[TeXTokenLike],opnotation:Option[List[TeXTokenLike]])
trait SymRefRuleLike extends STeXRule {
  def getSymbolsAndThen(f : (List[SymdeclInfo],List[TeXTokenLike]) => MacroApplication)(implicit parser: ParseState[PlainMacro]) : MacroApplication = {
    val (symname,tks) = {
      val arg = parser.readArgumentNoRules
      (arg.asname,arg match {
        case Grouped(group) => group.children
        case Single(tk) => List(tk)
      })
    }
    val syms = dict.resolveName(symname)
    if (syms.isEmpty) {
      val ret = new MacroApplication
      ret.addError("No symbol " + symname + " found")
      return ret
    }
    f(syms,tks)
  }
}
class SemanticMacroApp(syminfo: SymdeclInfo,notinfo:Option[NotationInfo] = None) extends STeXMacro {
  override def doAnnotations(in: sTeXDocument): Unit = {
    //in.Annotations.add(this,startoffset,endoffset - startoffset,SymbolKind.Constant,syminfo.path.toString)
    plain.foreach { plain =>
      val pa = in.Annotations.add(plain,plain.startoffset,plain.endoffset - plain.startoffset)
      pa.addDefinition(syminfo.file,syminfo.start,syminfo.end)
      pa.setSemanticHighlightingClass(SemanticHighlighting.symbol)
      pa.setHover(syminfo.path.toString + (notinfo match {
        case Some(nt) => "#" + nt.id
        case _ => ""
      }))
    }
  }
}
trait SemanticMacro extends STeXMacro with SymRefRuleLike with MacroRule {
  val syminfo: SymdeclInfo
  def doText(implicit parser: ParseState[PlainMacro]): MacroApplication = {
    new SemanticMacroApp(syminfo)
  }
  def doMath(implicit parser: ParseState[PlainMacro]): MacroApplication = {
    new SemanticMacroApp(syminfo)
  }

  override def apply(implicit parser: ParseState[PlainMacro]): MacroApplication = {
    if (dict.formalOnly) {
      new MacroApplication
    } else {
      if (parser.latex.inmath) doMath else doText
    }
  }
}
trait SymRefLike extends STeXMacro {
  val alternatives : List[SymdeclInfo]
  val reftokens : List[TeXTokenLike]
  def annotateWithAlternatives(a : Annotation,in:sTeXDocument): Unit = if (alternatives.length > 1) {
    addError("Ambiguous symbol reference", lvl = Level.Warning)
    class mutvar(val path: GlobalName) {
      var string = path.module.name.toString + "?" + path.name
    }
    val retstrings = alternatives.distinct.map(s => new mutvar(s.path))
    var done = false
    while (!done) {
      val todos = retstrings.filter(mv => retstrings.exists(o => o.path != mv.path && o.string == mv.string))
      if (todos.isEmpty) done = true
      todos.foreach { td =>
        var idone = false
        var i = 1
        while (!idone) {
          val np = td.path.doc.uri.path.takeRight(i)
          if ((np.mkString("/") + "?" + td.path.module.name + "?" + td.path.name).length <= td.string.length) i += 1 else idone = true
        }
        td.string = td.path.doc.uri.path.takeRight(i).mkString("/") + "?" + td.path.module.name + "?" + td.path.name
      }
    }
    val start = in._doctext.toLC(reftokens.head.startoffset)
    val end = in._doctext.toLC(reftokens.last.endoffset)
    retstrings.foreach{alt =>
      a.addCodeActionEdit(alt.path.toString,"quickfix",List(
        (in.uri,List((start._1,start._2,end._1,end._2,alt.string)))
      ))
    }
    val strs = retstrings.map(r => (r.path, r.string))

  }
}
trait NotationMacro extends SymRefLike {
  val notinfo : NotationInfo
}

trait SymDeclRuleLike extends STeXRule {
  def parseNameAndOpts(makemacro:Boolean)(implicit parser: ParseState[PlainMacro]) = {
    import parser._
    val maybename = readArgument.asname
    val opts = readOptAgument
    (processOpts(maybename,makemacro,opts),opts)
  }
  def processOpts[A <: TeXTokenLike](maybename:String,makemacro:Boolean,opts:Option[OptionalArgument])(implicit parser: ParseState[A]) = {
    val name = opts.flatMap(_.consumeStr("name")).getOrElse(maybename)
    val gn = dict.getGlobalName(name)
    opts.foreach { o =>
      o.drop("assoc", "gfc", "reorder", "role", "argtypes","style")
    }
    if (dict.formalOnly) {
      opts.foreach(_.drop("type"))
    } else {
      opts.flatMap(_.consume("type")).foreach(tks => parser.latex.inMath{ parser.reparse(tks) })
    }
    val defi = opts.flatMap(_.consume("def"))
    val defined = defi.isDefined
    if (!dict.formalOnly && defined) {
      defi.foreach(parser.reparse)
    }
    // TODO maybe check ret?
    val ret = opts.flatMap(_.consume("return"))
    var argstr = opts.flatMap(_.consumeStr("args")).getOrElse("")
    if (argstr.nonEmpty && argstr.forall(_.isDigit)) {
      argstr = (1 to argstr.toInt).map(_ => 'i').mkString
    } else argstr.foreach{ c =>
      if (c != 'i' && c != 'a' && c != 'b' && c != 'B') throw LaTeXParseError("Invalid character in args: " + c)
    }
    new SymdeclInfo(if (makemacro) maybename else gn.toString, gn, argstr, dict.getFile, parser.trigger.startoffset, parser.children.head.endoffset, defined, ret)
  }
}

trait NotationRuleLike extends STeXRule with MacroRule {
  def parseOptsNot(syminfo:SymdeclInfo,opts:Option[OptionalArgument])(implicit parser: ParseState[PlainMacro]) = {
    opts.foreach(_.drop("prec"))
    val op = opts.flatMap(_.consume("op"))
    val id = opts.flatMap{o =>
      o.asKeyVals.find(_._2.isEmpty).map{p =>
        val key = p._1 match {
          case List(g: Group) => g.children.mkString.trim
          case o => o.mkString.trim
        }
        o.asKeyVals.remove(p._1)
        key
      }
    }.getOrElse("")
    val not = parser.readArgumentNoRules.asList
    NotationInfo(syminfo,id,not,op)
  }
}
class SymdeclApp(val syminfo:SymdeclInfo,val dict:Dictionary)(val name : String = syminfo.macroname) extends MacroApplication with SemanticMacro {
  def cloned = new SymdeclApp(syminfo,dict)(syminfo.path.toString)

  override def doAnnotations(in: sTeXDocument): Unit = {
    val a = in.Annotations.add(this,startoffset,endoffset - startoffset,SymbolKind.Function,symbolname = syminfo.path.toString)
    a.addCodeLens(syminfo.path.toString,"",Nil,startoffset,endoffset)
    plain.foreach{plain =>
      val pa = in.Annotations.add(plain,plain.startoffset,plain.endoffset - plain.startoffset)
      pa.setSemanticHighlightingClass(SemanticHighlighting.declaration)
    }
    if (!syminfo.isDocumented) {
      this.addError("Symbol is not documented",Some("Consider adding an {sdefinition} or {sparagraph}[style=symdoc] for this symbol"),Level.Info)
    }
  }
}
case class TextSymdeclApp(_syminfo:SymdeclInfo,_dict:Dictionary)(name : String = _syminfo.macroname) extends SymdeclApp(_syminfo,_dict)(name)
case class NotationApp(notinfo:NotationInfo,dict:Dictionary,alternatives:List[SymdeclInfo],reftokens:List[TeXTokenLike]) extends MacroApplication with NotationMacro with STeXRule {
  val name = "notation/" + notinfo.syminfo.path + "#" + notinfo.id
  override def doAnnotations(in: sTeXDocument): Unit = {
    val a = in.Annotations.add(this, startoffset, endoffset - startoffset)
    plain.foreach { plain =>
      val pa = in.Annotations.add(plain, plain.startoffset, plain.endoffset - plain.startoffset)
      pa.setSemanticHighlightingClass(SemanticHighlighting.declaration)
    }
    a.setHover(notinfo.syminfo.path.toString + "#" + notinfo.id)
    annotateWithAlternatives(a,in)
  }
}
case class SymDeclRule(dict:Dictionary) extends SymDeclRuleLike with MacroRule {
  val name = "symdecl"
  def apply(implicit parser: ParseState[PlainMacro]): SymdeclApp = {
    val makemacro = !parser.readChar('*')
    val (si,o) = parseNameAndOpts(makemacro)
    val ret = new SymdeclApp(si,dict)()
    o.foreach(_.asKeyVals.foreach{p =>
      ret.addError("Unknown argument: " + p._1.mkString.trim)
    })
    addExportRule(ret)
    if (makemacro) addExportRule(ret.cloned)
    ret
  }
}

case class TextSymDeclRule(dict:Dictionary) extends SymDeclRuleLike with MacroRule {
  val name = "textsymdecl"

  override def apply(implicit parser: ParseState[PlainMacro]): TextSymdeclApp = {
    val (si, o) = parseNameAndOpts(true)
    //val si = new SymdeclInfo(_si.macroname,_si.path.module ? (_si.path.name.toString + "-sym"),_si.args,_si.file,_si.start,_si.end,_si.defined,_si.ret)
    val ret = TextSymdeclApp(si,dict)()
    o.foreach(_.asKeyVals.foreach { p =>
      ret.addError("Unknown argument: " + p._1.mkString.trim)
    })
    addExportRule(ret)
    addExportRule(ret.cloned)
    ret
  }
}
case class SymDefRule(dict:Dictionary) extends SymDeclRuleLike with NotationRuleLike with MacroRule {
  val name = "symdef"

  override def apply(implicit parser: ParseState[PlainMacro]): MacroApplication = {
    val (si, o) = parseNameAndOpts(true)
    val not = parseOptsNot(si,o)
    val ret = new SymdeclApp(si, dict)() with NotationMacro {
      override val notinfo: NotationInfo = not
      override val alternatives: List[SymdeclInfo] = Nil
      override val reftokens: List[TeXTokenLike] = Nil
    }
    o.foreach(_.asKeyVals.foreach { p =>
      ret.addError("Unknown argument: " + p._1.mkString.trim)
    })
    addExportRule(ret)
    addExportRule(ret.cloned)
    ret
  }
}
class VarApp(syminfo: SymdeclInfo,notinfo:Option[NotationInfo]) extends STeXMacro {
  override def doAnnotations(in: sTeXDocument): Unit = {
    //in.Annotations.add(this,startoffset,endoffset - startoffset,SymbolKind.Constant,syminfo.path.toString)
    plain.foreach { plain =>
      val pa = in.Annotations.add(plain,plain.startoffset,plain.endoffset - plain.startoffset)
      pa.addDefinition(syminfo.file,syminfo.start,syminfo.end)
      pa.setSemanticHighlightingClass(SemanticHighlighting.variable)
      pa.setHover("Variable " + syminfo.path.name.toString + (notinfo match {
        case Some(nt) => "#" + nt.id
        case _ => ""
      }))
    }
  }
}

case class VardefApp(notinfo:NotationInfo, dict:Dictionary) extends SemanticMacro with NotationMacro {
  val syminfo = notinfo.syminfo
  val reftokens = Nil
  val alternatives = Nil
  val name = notinfo.syminfo.macroname

  override def doText(implicit parser: ParseState[PlainMacro]): MacroApplication = {
    new VarApp(syminfo,Some(notinfo))
  }
  override def doMath(implicit parser: ParseState[PlainMacro]): MacroApplication = {
    new VarApp(syminfo, Some(notinfo))
  }
  override def doAnnotations(in: sTeXDocument): Unit = {
    val a = in.Annotations.add(this,startoffset,endoffset - startoffset,SymbolKind.Variable,syminfo.path.toString)
    a.setHover("Variable " + syminfo.path.name.toString)
    plain.foreach {plain =>
      val pa = in.Annotations.add(plain,plain.startoffset,plain.endoffset - plain.startoffset)
      pa.addDefinition(syminfo.file,syminfo.start,syminfo.end)
      pa.setSemanticHighlightingClass(SemanticHighlighting.declaration)
    }
  }
}
case class VarDefRule(dict:Dictionary) extends SymDeclRuleLike with NotationRuleLike with NonFormalRule with MacroRule {
  val name = "vardef"

  override def apply(implicit parser: ParseState[PlainMacro]): MacroApplication = {
    import parser._
    val maybename = readArgument.asname
    val opts = readOptAgument
    val name = opts.flatMap(_.consumeStr("name")).getOrElse(maybename)
    val path = DPath(URI.scheme("var")) ? "" ? name
    opts.foreach { o =>
      o.drop("assoc", "reorder", "role", "argtypes")
    }
    opts.flatMap(_.consume("type")).foreach(tks => parser.latex.inMath{ parser.reparse(tks) })
    val defi = opts.flatMap(_.consume("def"))
    val defined = defi.isDefined
    if (!dict.formalOnly && defined) {
      defi.foreach(parser.reparse)
    }
    // TODO maybe check type / def?
    val returns = opts.flatMap(_.consume("return"))
    var argstr = opts.flatMap(_.consumeStr("args")).getOrElse("")
    if (argstr.nonEmpty && argstr.forall(_.isDigit)) {
      argstr = (1 to argstr.toInt).map(_ => 'i').mkString
    } else argstr.foreach { c =>
      if (c != 'i' && c != 'a' && c != 'b' && c != 'B') throw LaTeXParseError("Invalid character in args: " + c)
    }
    val si = new SymdeclInfo(maybename,path,argstr,dict.getFile,parser.trigger.startoffset,parser.children.head.endoffset,defined,returns)
    val not = parseOptsNot(si,opts)
    val ret = VardefApp(not,dict)
    opts.foreach(_.asKeyVals.foreach { p =>
      ret.addError("Unknown argument: " + p._1.mkString.trim)
    })
    parser.latex.addRule(ret)
    ret
  }
}
class SeqApp(syminfo: SymdeclInfo,notinfo:Option[NotationInfo]) extends STeXMacro {
  override def doAnnotations(in: sTeXDocument): Unit = {
    //in.Annotations.add(this,startoffset,endoffset - startoffset,SymbolKind.Constant,syminfo.path.toString)
    plain.foreach { plain =>
      val pa = in.Annotations.add(plain,plain.startoffset,plain.endoffset - plain.startoffset,SymbolKind.Variable,syminfo.path.toString)
      pa.addDefinition(syminfo.file,syminfo.start,syminfo.end)
      pa.setSemanticHighlightingClass(SemanticHighlighting.variable)
      pa.setHover("Variable Sequence " + syminfo.path.name.toString + (notinfo match {
        case Some(nt) => "#" + nt.id
        case _ => ""
      }))
    }
  }
}
case class VarseqApp(notinfo:NotationInfo,range:List[TeXTokenLike], dict:Dictionary) extends SemanticMacro with NotationMacro {
  val syminfo = notinfo.syminfo
  val reftokens = Nil
  val alternatives = Nil
  val name = notinfo.syminfo.macroname

  override def doText(implicit parser: ParseState[PlainMacro]): MacroApplication = {
    new SeqApp(syminfo,Some(notinfo))
  }
  override def doMath(implicit parser: ParseState[PlainMacro]): MacroApplication = {
    new SeqApp(syminfo, Some(notinfo))
  }
  override def doAnnotations(in: sTeXDocument): Unit = {
    val a = in.Annotations.add(this,startoffset,endoffset - startoffset)
    a.setHover("Variable Sequence " + syminfo.path.name.toString)
    plain.foreach {plain =>
      val pa = in.Annotations.add(plain,plain.startoffset,plain.endoffset - plain.startoffset)
      pa.addDefinition(syminfo.file,syminfo.start,syminfo.end)
      pa.setSemanticHighlightingClass(SemanticHighlighting.declaration)
    }
  }
}
case class VarSeqRule(dict:Dictionary) extends SymDeclRuleLike with NotationRuleLike with NonFormalRule with MacroRule {
  val name = "varseq"

  override def apply(implicit parser: ParseState[PlainMacro]): MacroApplication = {
    import parser._
    val maybename = readArgument.asname
    val opts = readOptAgument
    val name = opts.flatMap(_.consumeStr("name")).getOrElse(maybename)
    val path = DPath(URI.scheme("varseq")) ? "" ? name
    opts.foreach { o =>
      o.drop("assoc", "reorder", "role", "argtypes")
    }
    opts.flatMap(_.consume("type")).foreach(tks => parser.latex.inMath{ parser.reparse(tks) })
    val defi = opts.flatMap(_.consume("def"))
    val defined = defi.isDefined
    if (!dict.formalOnly && defined) {
      defi.foreach(parser.reparse)
    }
    // TODO maybe check type / def?
    val returns = opts.flatMap(_.consume("return"))
    var argstr = opts.flatMap(_.consumeStr("args")).getOrElse("i")
    if (argstr.nonEmpty && argstr.forall(_.isDigit)) {
      argstr = (1 to argstr.toInt).map(_ => 'i').mkString
    } else argstr.foreach { c =>
      if (c != 'i' && c != 'a' && c != 'b' && c != 'B') throw LaTeXParseError("Invalid character in args: " + c)
    }
    val si = new SymdeclInfo(maybename,path,argstr,dict.getFile,parser.trigger.startoffset,parser.children.head.endoffset,defined,returns)
    val range = parser.readArgument
    val not = parseOptsNot(si,opts)
    val ret = VarseqApp(not,range.asList,dict)
    opts.foreach(_.asKeyVals.foreach { p =>
      ret.addError("Unknown argument: " + p._1.mkString.trim)
    })
    parser.latex.addRule(ret)
    ret
  }
}

case class NotationRule(dict:Dictionary) extends NotationRuleLike with SymRefRuleLike {
  val name = "notation"

  override def apply(implicit parser: ParseState[PlainMacro]): MacroApplication = {
    val setdefault = parser.readChar('*')
    getSymbolsAndThen{ (syms,reftks) =>
      val o = parser.readOptAgument
      val not = parseOptsNot(syms.head, o)
      val ret = NotationApp(not, dict, syms,reftks)
      o.foreach(_.asKeyVals.foreach { p =>
        ret.addError("Unknown argument: " + p._1.mkString.trim)
      })
      addExportRule(ret)
      if (setdefault) addExportRule(NotationApp(not.copy(id = ""), dict, syms,reftks))
      ret
    }
  }
}

case class MetaTheoryRule(mp : MPath,dict:Dictionary) extends STeXRule {
  val name = "meta theory rule"
}

class SymrefApp(dict:Dictionary,val alternatives:List[SymdeclInfo],val asPlainString: String,trivial:Boolean,val reftokens:List[TeXTokenLike]) extends SymRefLike {
  override def doAnnotations(in: sTeXDocument): Unit = {
    val a = in.Annotations.add(this, startoffset, endoffset - startoffset)
    a.setHover(alternatives.head.path.toString + {
      if (trivial) "" else "\n\nYields: " + asPlainString
    })
    a.addDefinition(alternatives.head.file,alternatives.head.start,alternatives.head.end)
    a.setSemanticHighlightingClass(SemanticHighlighting.symbol)
    /*this.plain.foreach {plain =>
      val pla = in.Annotations.add(plain,plain.startoffset,plain.endoffset - plain.startoffset,SymbolKind.Constant,alternatives.head.path.toString)
    }*/
    annotateWithAlternatives(a, in)
  }
}
case class SymrefRule(name:String,dict:Dictionary) extends SymRefRuleLike with MacroRule with NonFormalRule {
  override def apply(implicit parser: ParseState[PlainMacro]): MacroApplication = {
    parser.readOptAgument
    getSymbolsAndThen{(syms,reftks) =>
      val txt = parser.readArgument
      new SymrefApp(dict,syms,{ txt.asname },true,reftks)
    }
  }
}
case class SymuseRule(dict:Dictionary) extends SymRefRuleLike with MacroRule with NonFormalRule {
  val name = "symuse"
  override def apply(implicit parser: ParseState[PlainMacro]): MacroApplication = {
    parser.readOptAgument
    getSymbolsAndThen{(syms,reftks) =>
      new SymrefApp(dict,syms,{ "" },true,reftks)
    }
  }
}

case class DefiniendumRule(dict:Dictionary) extends SymRefRuleLike with MacroRule with NonFormalRule {
  val name = "definiendum"
  override def apply(implicit parser: ParseState[PlainMacro]): MacroApplication = {
    parser.readOptAgument
    getSymbolsAndThen{(syms,reftks) =>
      val txt = parser.readArgument
      syms.headOption.foreach(_.isDocumented = true)
      new SymrefApp(dict,syms,{ txt.asname },true,reftks)
    }
  }
}

case class DefinameRule(dict:Dictionary,name:String,cap:Boolean) extends SymRefRuleLike with MacroRule with NonFormalRule {
  override def apply(implicit parser: ParseState[PlainMacro]): MacroApplication = {
    val o = parser.readOptAgument
    val pre = o.flatMap(_.consumeStr("pre")).getOrElse("")
    val post = o.flatMap(_.consumeStr("post")).getOrElse("")
    getSymbolsAndThen { (syms,reftks) =>
      syms.headOption.foreach(_.isDocumented = true)
      new SymrefApp(dict, syms, {
        val str = pre + syms.head.path.name.last.toString + post
        if (cap) str.head.toUpper + str.tail else str
      },false,reftks)
    }
  }
}
case class SymnameRule(name: String, dict:Dictionary,cap:Boolean) extends SymRefRuleLike with MacroRule with NonFormalRule {
  override def apply(implicit parser: ParseState[PlainMacro]): MacroApplication = {
    val o = parser.readOptAgument
    val pre = o.flatMap(_.consumeStr("pre")).getOrElse("")
    val post = o.flatMap(_.consumeStr("post")).getOrElse("")
    getSymbolsAndThen { (syms,reftks) =>
      new SymrefApp(dict, syms, {
        val str = pre + syms.head.path.name.last.toString + post
        if (cap) str.head.toUpper + str.tail else str
      },false,reftks)
    }
  }
}

case class SymnamesRule(name: String, dict:Dictionary,cap:Boolean) extends SymRefRuleLike with MacroRule with NonFormalRule {
  override def apply(implicit parser: ParseState[PlainMacro]): MacroApplication = {
    getSymbolsAndThen { (syms,reftks) =>
      new SymrefApp(dict, syms, {
        val str = syms.head.path.name.last.toString + "s"
        if (cap) str.head.toUpper + str.tail else str
      },false,reftks)
    }
  }
}
trait StatementRule extends SymDeclRuleLike with SymRefRuleLike {
  lazy val exrules: List[TeXRule] = Nil
  lazy val defrules: List[TeXRule] = List(DefinameRule(dict,"definame",false),DefinameRule(dict,"Definame",true),DefiniendumRule(dict))
  lazy val assrules: List[TeXRule] = Nil
  protected val dodefs: Option[Boolean]
  protected val doexs: Boolean
  protected val doass: Boolean
  def parseStatement[A <: TeXTokenLike](implicit parser: ParseState[A]) = {
    val opts = parser.readOptAgument
    val (macroname,name) = opts.flatMap(_.getStr("macro")) match {
      case Some(s) =>
        (Some(s),opts.flatMap(_.getStr("name")).orElse(Some(s)))
      case _ => (None,opts.flatMap(_.getStr("name")))
    }
    val sym = name match {
      case Some(_) if dict.getModuleOpt.isEmpty =>
        parser.trigger.addError("\"name\" argument only allowed in modules")
        None
      case Some(_) =>
        val sym = processOpts(macroname.getOrElse(""),macroname.isDefined,opts)
        sym.isDocumented = true
        val ret = new SymdeclApp(sym, dict)()
        addExportRule(ret)
        if (macroname.isDefined) addExportRule(ret.cloned)
        Some(ret)
      case _ =>
        None
    }
    (opts,opts.flatMap(_.consumeStr("for")).map{s =>
      s.split(',').toList.map(_.trim).map(s => (s,dict.resolveName(s)(parser)))
    }.getOrElse(Nil),sym)
  }
}
abstract class InlineStatementRule extends StatementRule with MacroRule {
  override def apply(implicit parser: ParseState[PlainMacro]): MacroApplication = {
    val (opts, fors,sym) = parseStatement
    parser.latex.inGroup{
      def dofors = {
        defrules.foreach(parser.latex.addRule(_))
        fors.foreach(_._2.headOption.foreach(_.isDocumented = true))
      }

      dodefs match {
        case Some(true) =>
          dofors
        case None if opts.toList.flatMap(_.consumeStr("type").toList.flatMap(_.split(',').toList.map(_.trim))).contains("symdoc") => dofors
        case _ =>
      }
      if (doexs) exrules.foreach(parser.latex.addRule(_))
      if (doass) assrules.foreach(parser.latex.addRule(_))
      val ret = sym.getOrElse(new MacroApplication)
      fors.foreach {
        case (s, Nil) =>
          ret.addError("No symbol " + s + " found")
        case _ =>
      }
      ret
    }
  }
}
case class StatementEnv(begin:Begin.BeginMacro,symopt:SymdeclApp,val dict:Dictionary) extends STeXEnvironment(begin) {

  override def doAnnotations(in: sTeXDocument): Unit = {
    val a = in.Annotations.add(this, startoffset, endoffset - startoffset, SymbolKind.Function, symbolname = symopt.syminfo.path.toString)
    a.addCodeLens(symopt.syminfo.path.toString, "", Nil, startoffset, endoffset)
    val pa = in.Annotations.add(begin, begin.startoffset, begin.endoffset - begin.startoffset)
    pa.setSemanticHighlightingClass(SemanticHighlighting.declaration)
    end.foreach{end =>
      val pa = in.Annotations.add(end, end.startoffset, end.endoffset - end.startoffset)
      pa.setSemanticHighlightingClass(SemanticHighlighting.declaration)
    }
  }
}
abstract class StatementEnvRule extends StatementRule with EnvironmentRule {
  override def applyStart(implicit parser: ParseState[Begin.BeginMacro]): Environment = {
    val (opts, fors,sym) = parseStatement

    def dofors = {
      defrules.foreach(parser.latex.addRule(_))
      fors.foreach(_._2.headOption.foreach(_.isDocumented = true))
    }
    dodefs match {
      case Some(true) =>
        dofors
      case None if opts.toList.flatMap(o => (o.consumeStr("type").toList ::: o.consumeStr("style").toList).flatMap(_.split(',').toList.map(_.trim))).contains("symdoc") => dofors
      case _ =>
    }
    if (doexs) exrules.foreach(parser.latex.addRule(_))
    if (doass) assrules.foreach(parser.latex.addRule(_))
    val ret = sym.map(s => StatementEnv(parser.trigger,s,dict)).getOrElse(new Environment(parser.trigger))
    fors.foreach {
      case (s, Nil) =>
        ret.addError("No symbol " + s + " found")
      case _ =>
    }
    ret
  }
}

case class SDefinitionRule(dict:Dictionary) extends StatementEnvRule {
  override val envname: String = "sdefinition"
  override protected val dodefs: Option[Boolean] = Some(true)
  override protected val doexs: Boolean = false
  override protected val doass: Boolean = false
}
case class InlineDefRule(dict:Dictionary) extends InlineStatementRule {
  val name = "inlinedef"
  override protected val dodefs: Option[Boolean] = Some(true)
  override protected val doexs: Boolean = false
  override protected val doass: Boolean = false
}
case class SParagraphRule(dict: Dictionary) extends StatementEnvRule {
  override val envname: String = "sparagraph"
  override protected val dodefs: Option[Boolean] = None
  override protected val doexs: Boolean = false
  override protected val doass: Boolean = false
}
case class SAssertionRule(dict:Dictionary) extends StatementEnvRule {
  override val envname: String = "sassertion"
  override protected val dodefs: Option[Boolean] = Some(false)
  override protected val doexs: Boolean = false
  override protected val doass: Boolean = true
}
case class InlineAssRule(dict:Dictionary) extends InlineStatementRule {
  override val name: String = "inlineass"
  override protected val dodefs: Option[Boolean] = Some(false)
  override protected val doexs: Boolean = false
  override protected val doass: Boolean = true
}

case class MathStructure(begin:Begin.BeginMacro,override val mp:MPath,syminfo:SymdeclInfo,dict:Dictionary) extends STeXEnvironment(begin) with SemanticMacro with STeXModuleLike {
  val name = syminfo.macroname
  val sig = ""
  def cloned = new MathStructure(begin,mp,syminfo,dict) {
    override val name = syminfo.path.toString
  }

  override def doAnnotations(in: sTeXDocument): Unit = {
    in.Annotations.add(this, startoffset, endoffset - startoffset, SymbolKind.Module, mp.toString, true)
    val bg = in.Annotations.add(begin, begin.startoffset, begin.endoffset - begin.startoffset)
    bg.setHover(syminfo.path.toString)
    bg.addCodeLens(syminfo.path.toString, "", Nil, begin.startoffset, begin.endoffset)
    bg.setSemanticHighlightingClass(SemanticHighlighting.declaration)
    this.end.foreach { end =>
      val e = in.Annotations.add(end, end.startoffset, end.endoffset - end.startoffset)
      e.setHover(syminfo.path.toString)
      e.setSemanticHighlightingClass(SemanticHighlighting.declaration)
    }
  }
}
trait MathStructureDeclRule extends SymDeclRuleLike with SymRefRuleLike with EnvironmentRule {
  def parseHeader(implicit parser: ParseState[Begin.BeginMacro]) = {
    import parser._
    val macroname = readArgument.asname
    val opts = readOptAgument
    val name = opts.flatMap{o =>
      o.consume("comp")
      o.asKeyVals.find(_._2.isEmpty).map { p =>
        val key = p._1 match {
          case List(g: Group) => g.children.mkString.trim
          case o => o.mkString.trim
        }
        o.asKeyVals.remove(p._1)
        key
      }
    }.getOrElse(macroname) //readOptAgument.map(_.asname).getOrElse(macroname)
    //val lang = dict.getLanguage
    val sym =new SymdeclInfo(macroname, dict.getGlobalName(name),"",dict.getFile,parser.trigger.startoffset,parser.trigger.endoffset,true,None)
    val mp = dict.getMPath(name + "-module")
    val ret = MathStructure(parser.trigger,mp,sym,dict)
    addExportRule(ret)
    addExportRule(ret.cloned)
    dict.openModule(ret)
    ret
  }

  override def applyEnd(env: Environment)(implicit parser: ParseState[Begin.BeginMacro]): Environment = super.applyEnd(env) match {
    case tmm: MathStructure =>
      dict.closeModule
      tmm
    case _ => env
  }
}
case class MathStructureRule(dict:Dictionary) extends MathStructureDeclRule {
  val envname = "mathstructure"

  override def applyStart(implicit parser: ParseState[Begin.BeginMacro]): Environment = {
    parseHeader
  }
}

case class ExtStructureRule(dict:Dictionary) extends MathStructureDeclRule {
  val envname = "extstructure"

  override def applyStart(implicit parser: ParseState[Begin.BeginMacro]): Environment = {
    val env = parseHeader
    val imports = parser.readArgument.asname.split(',').map(_.trim)
    imports.foreach {s =>
      val mod = dict.moduleOrStructure(s)
      val rl = InheritModuleRule(mod,true)
      dict.getModuleOpt.foreach { m =>
        m.imports ::= (mod, true)
        m.rules(rl.name) = rl
        mod.getRules(dict.getLanguage).foreach { rl =>
          m.rules(rl.name) = rl
        }
      }
    }
    env
  }
}

case class ExtStructureStarRule(dict:Dictionary) extends MathStructureDeclRule {
  val envname = "extstructure*"

  override def applyStart(implicit parser: ParseState[Begin.BeginMacro]): Environment = {
    var mod : Option[DictionaryModule] = None
    val env = {
      val sym = new SymdeclInfo("EXTSTRUCT", dict.getGlobalName(name), "", dict.getFile, parser.trigger.startoffset, parser.trigger.endoffset, true, None)
      val mp = dict.getMPath("EXTSTRUCT")
      val ret = new MathStructure(parser.trigger, mp, sym, dict) {
        override def doAnnotations(in: sTeXDocument): Unit = {
          in.Annotations.add(this, startoffset, endoffset - startoffset, SymbolKind.Module, mp.toString, true)
          val bg = in.Annotations.add(begin, begin.startoffset, begin.endoffset - begin.startoffset)
          bg.setHover(syminfo.path.toString)
          mod.map(_.macr).foreach{case MathStructure(_,_,syminfo,_) =>
            bg.addCodeLens("Conservative Extension of " + syminfo.path, "", Nil, begin.startoffset, begin.endoffset)
            case _ =>
          }
          bg.setSemanticHighlightingClass(SemanticHighlighting.declaration)
          this.end.foreach { end =>
            val e = in.Annotations.add(end, end.startoffset, end.endoffset - end.startoffset)
            e.setHover(syminfo.path.toString)
            e.setSemanticHighlightingClass(SemanticHighlighting.declaration)
          }
        }
      }
      addExportRule(ret)
      addExportRule(ret.cloned)
      dict.openModule(ret)
      ret
    }
    val imports = parser.readArgument.asname
    mod = Some(dict.moduleOrStructure(imports))
    val rl = InheritModuleRule(mod.get, true)
    dict.getModuleOpt.foreach { m =>
      m.imports ::= (mod.get, true)
      m.rules(rl.name) = rl
      mod.get.getRules(dict.getLanguage).foreach { rl =>
        m.rules(rl.name) = rl
      }
    }
    env
  }
}


trait MMTStructure extends STeXRule {
  val domain : DictionaryModule
  lazy val rules = domain.getRules(dict.getLanguage)
  val structname : String
  val parent = dict.getModule
  val (isimplicit,path) = structname match {
    case "" => (true,parent.path ?LocalName(ComplexStep(domain.path)))
    // TODO check implicits
    case o => (false,parent.path ?LocalName.parse(o))
  }
  val name = path.toString

  val elaboration = mutable.HashMap.empty[GlobalName,(Option[String],Option[LocalName],Option[List[TeXTokenLike]])]
  def rename[A <: TeXTokenLike](orig:String, alias:Option[String], macroname:String)(implicit parser: ParseState[A]) = {
    getInDomain(orig).headOption.map { si =>
      elaboration.get(si.path) match {
        case None =>
          elaboration(si.path) = (Some(macroname),alias.map(LocalName.parse),None)
          si
        case Some((s,n,d)) if s.isEmpty && n.isEmpty =>
          elaboration(si.path) = (Some(macroname),alias.map(LocalName.parse),d)
          si
        case Some(_) =>
          // TODO error
          si
      }
    }
  }
  def assign[A <: TeXTokenLike](orig:String,defi:List[TeXTokenLike])(implicit parser: ParseState[A]) = {
    getInDomain(orig).headOption.map { si =>
      elaboration.get(si.path) match {
        case None =>
          elaboration(si.path) = (None, None, Some(defi))
          si
        case Some((s, n, d)) if d.isEmpty =>
          elaboration(si.path) = (s,n,Some(defi))
          si
        case Some(_) =>
          // TODO error
          si
      }
    }
  }

  def getInDomain[A <: TeXTokenLike](orig:String)(implicit parser: ParseState[A]) = {
    val groups = parser.latex.groups
    try {
      parser.latex.groups = List(new RuleContainer)
      rules.foreach(parser.latex.addRule(_))
      dict.resolveName(orig)
    } finally {
      parser.latex.groups = groups
    }
  }

  def elaborate(istotal : Boolean) = {
    val syms = rules.collect{case sm:SemanticMacro => sm.syminfo}.distinct
    val rets = syms.foreach { sm =>
      elaboration.get(sm.path) match {
        case Some((m,n,d)) =>
          val newpath = n match {
            case Some(name) => parent.path ? name
            case None if isimplicit => sm.path
            case None => parent.path ? (path.name / sm.path.name)
          }
          val si = sm.copy(macroname = m.getOrElse(newpath.toString),path = newpath,defined = sm.defined || d.isDefined)
          val ret = new SymdeclApp(si,dict)()
          addExportRule(ret)
          if (si.macroname != newpath.toString) addExportRule(ret.cloned)
          ret
        case None /* TODO if istotal? */ =>
          val newpath = if (isimplicit) sm.path else parent.path ? (path.name / sm.path.name)
          val si = sm.copy(macroname = newpath.toString, path = newpath)
          val ret = new SymdeclApp(si, dict)()
          addExportRule(ret)
          ret
      }
    }
  }
}

trait MMTStructureRule extends SymDeclRuleLike with SymRefRuleLike {
  def parseDomain[A <: TeXTokenLike](implicit parser: ParseState[A]) = {
    val maybearchive = parser.readOptAgument.map(_.asname)
    val domainstr = parser.readArgument.asname
    maybearchive match {
      case Some(a) => dict.requireModule(dict.resolveMPath(a,domainstr),a,domainstr)
      case _ => dict.moduleOrStructure(domainstr)
    }
  }
  def parseContent[A <: TeXTokenLike](struct: MMTStructure)(implicit parser: ParseState[A]): Unit = {
    class Triple {
      var name = ""
      var defi: List[TeXTokenLike] = Nil
      var alias = ""
      var in = 0
      def process = {
        name = name.trim
        alias = alias.trim
        if (alias.startsWith("[")) {
          val als = alias.drop(1).split(']')
          // TODO check als.length == 2
          struct.rename(name,Some(als.head.trim),als(1).trim)
        } else if (alias.nonEmpty) struct.rename(name,None,alias)
        if (defi.nonEmpty) struct.assign(name,defi.reverse)
      }
      def processMaybe = if (name.nonEmpty) process
    }
    var current = new Triple
    parser.readArgument.asList.foreach {
      case PlainText(",",_,_) =>
        current.process
        current = new Triple
      case PlainText("@", _, _) =>
        // TODO check duplicate, name non-empty
        current.in = 2
      case PlainText("=", _, _) =>
        // TODO check duplicate, name non-empty
        current.in = 1
      case p if current.in == 0 =>
        current.name += p.toString
      case p if current.in == 2 =>
        current.alias += p.toString
      case p if current.in == 1 =>
        current.defi ::= p
    }
    current.processMaybe
  }

  def rules: List[TeXRule] = List(
    RenameDeclRule(dict),AssignDeclRule(dict)
  )
}

case class RenameDeclRule(dict : Dictionary) extends STeXRule with MacroRule {
  val name = "renamedecl"
  override def apply(implicit parser: ParseState[PlainMacro]): MacroApplication = {
    parser.latex.collectFirstRule { case e:MMTStructureEnv => e}.flatMap { struct =>
      val orig = parser.readArgument.asname
      val alias = parser.readOptAgument.map(_.asname)
      val macroname = parser.readArgument.asname
      struct.rename(orig,alias, macroname).map { sym =>
        new STeXMacro {
          override def doAnnotations(in: sTeXDocument): Unit = {
            val a = in.Annotations.add(this, startoffset, endoffset - startoffset)
            plain.foreach { plain =>
              val pa = in.Annotations.add(plain, plain.startoffset, plain.endoffset - plain.startoffset)
              pa.setSemanticHighlightingClass(SemanticHighlighting.declaration)
            }

          }
        }
      }
    }.getOrElse(new MacroApplication)
  }
}

case class AssignDeclRule(dict : Dictionary) extends STeXRule with MacroRule {
  val name = "assign"
  override def apply(implicit parser: ParseState[PlainMacro]): MacroApplication = {
    parser.latex.collectFirstRule { case e:MMTStructureEnv => e}.flatMap { struct =>
      val orig = parser.readArgument.asname
      val defi = parser.readArgument.asList
      struct.assign(orig,defi).map { sym =>
        new STeXMacro {
          override def doAnnotations(in: sTeXDocument): Unit = {
            val a = in.Annotations.add(this, startoffset, endoffset - startoffset)
            plain.foreach { plain =>
              val pa = in.Annotations.add(plain, plain.startoffset, plain.endoffset - plain.startoffset)
              pa.setSemanticHighlightingClass(SemanticHighlighting.declaration)
            }

          }
        }
      }
    }.getOrElse(new MacroApplication)
  }
}

trait InlineStructure extends MacroApplication with STeXMacro with MMTStructure {
  override def doAnnotations(in: sTeXDocument): Unit = {
    val a = in.Annotations.add(this, startoffset, endoffset - startoffset, SymbolKind.Constant, path.toString)
    a.setHover("Structure " + path.name.toString + ": " + domain.path.toString)
    plain.foreach { plain =>
      val pa = in.Annotations.add(plain, plain.startoffset, plain.endoffset - plain.startoffset)
      pa.addCodeLens(path.toString, "", Nil, plain.startoffset, plain.endoffset)
      pa.addDefinition(domain.file.get.toString, domain.macr.startoffset, domain.macr.endoffset)
      pa.setSemanticHighlightingClass(SemanticHighlighting.declaration)
    }
  }
}
trait MMTStructureEnv extends STeXEnvironment with MMTStructure {
  val begin:Begin.BeginMacro
  var end:Option[End.EndMacro]
  override def doAnnotations(in: sTeXDocument): Unit = {
    val a = in.Annotations.add(this, startoffset, endoffset - startoffset, SymbolKind.Constant, path.toString)
    a.setHover("Structure " + path.name.toString + ": " + domain.path.toString)
    val bg = in.Annotations.add(begin, begin.startoffset, begin.endoffset - begin.startoffset)
    bg.addCodeLens(path.toString, "", Nil, begin.startoffset, begin.endoffset)
    bg.addDefinition(domain.file.get.toString, domain.macr.startoffset, domain.macr.endoffset)
    bg.setSemanticHighlightingClass(SemanticHighlighting.declaration)
    end.foreach { plain =>
      val pa = in.Annotations.add(plain, plain.startoffset, plain.endoffset - plain.startoffset)
      pa.setSemanticHighlightingClass(SemanticHighlighting.declaration)
    }
  }
}

case class CopyMod(structname : String,domain:DictionaryModule,dict:Dictionary) extends InlineStructure
case class CopyModule(structname : String,domain:DictionaryModule,dict:Dictionary,begin:Begin.BeginMacro) extends STeXEnvironment(begin) with MMTStructureEnv
case class CopyModRule(dict : Dictionary) extends MMTStructureRule with MacroRule {
  val name = "copymod"

  override def apply(implicit parser: ParseState[PlainMacro]): MacroApplication = {
    val structstr = parser.readArgument.asname
    val dom = parseDomain
    val struct = CopyMod(structstr,dom,dict)
    parseContent(struct)
    struct.elaborate(false)
    struct
  }
}

case class CopyModuleRule(dict:Dictionary) extends MMTStructureRule with EnvironmentRule {
  val envname = "copymodule"
  override def applyStart(implicit parser: ParseState[Begin.BeginMacro]): Environment = {
    val structstr = parser.readArgument.asname
    val dom = parseDomain
    val struct = CopyModule(structstr,dom,dict,parser.trigger)
    rules.foreach(rl => parser.latex.addRule(rl))
    parser.latex.addRule(struct)
    struct
  }

  override def applyEnd(env: Environment)(implicit parser: ParseState[Begin.BeginMacro]): Environment = super.applyEnd(env) match {
    case cm: CopyModule =>
      cm.elaborate(false)
      cm
    case e => e
  }
}

case class InterpretModRule(dict : Dictionary) extends MMTStructureRule with MacroRule {
  val name = "interpretmod"

  override def apply(implicit parser: ParseState[PlainMacro]): MacroApplication = {
    val structstr = parser.readArgument.asname
    val dom = parseDomain
    val struct = CopyMod(structstr,dom,dict)
    parseContent(struct)
    struct.elaborate(true)
    struct
  }
}

case class InterpretModuleRule(dict:Dictionary) extends MMTStructureRule with EnvironmentRule {
  val envname = "interpretmodule"
  override def applyStart(implicit parser: ParseState[Begin.BeginMacro]): Environment = {
    val structstr = parser.readArgument.asname
    val dom = parseDomain
    val struct = CopyModule(structstr,dom,dict,parser.trigger)
    rules.foreach(rl => parser.latex.addRule(rl))
    parser.latex.addRule(struct)
    struct
  }

  override def applyEnd(env: Environment)(implicit parser: ParseState[Begin.BeginMacro]): Environment = super.applyEnd(env) match {
    case cm: CopyModule =>
      cm.elaborate(true)
      cm
    case e => e
  }
}
case class RealizeRule(dict : Dictionary) extends MMTStructureRule with MacroRule {
  val name = "realize"

  override def apply(implicit parser: ParseState[PlainMacro]): MacroApplication = {
    val dom = parseDomain
    val struct = CopyMod("",dom,dict)
    parseContent(struct)
    struct.elaborate(true)
    struct
  }
}

case class RealizationRule(dict:Dictionary) extends MMTStructureRule with EnvironmentRule {
  val envname = "realization"
  override def applyStart(implicit parser: ParseState[Begin.BeginMacro]): Environment = {
    val dom = parseDomain
    val struct = CopyModule("",dom,dict,parser.trigger)
    rules.foreach(rl => parser.latex.addRule(rl))
    parser.latex.addRule(struct)
    struct
  }

  override def applyEnd(env: Environment)(implicit parser: ParseState[Begin.BeginMacro]): Environment = super.applyEnd(env) match {
    case cm: CopyModule =>
      cm.elaborate(true)
      cm
    case e => e
  }
}
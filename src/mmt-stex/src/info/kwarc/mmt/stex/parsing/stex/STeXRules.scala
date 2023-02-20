package info.kwarc.mmt.stex.parsing.stex

import info.kwarc.mmt.api.utils.URI
import info.kwarc.mmt.api.{DPath, GlobalName, Level, MPath, Path}
import info.kwarc.mmt.lsp.Annotation
import info.kwarc.mmt.stex.lsp.{SemanticHighlighting, sTeXDocument}
import info.kwarc.mmt.stex.parsing.{Begin, Environment, EnvironmentRule, Group, Grouped, LaTeXParseError, MacroApplication, MacroRule, OptionalArgument, ParseState, PlainMacro, Single, SkipCommand, TeXRule, TeXRules, TeXTokenLike}
import org.eclipse.lsp4j.{InlayHintKind, SymbolKind}

object STeXRules {
  def allRules(dict: Dictionary) : List[TeXRule] = List(
    ModuleRule(dict),ProblemRule(dict),UseModuleRule(dict),UseStructureRule(dict),
    SymrefRule("symref",dict),SymrefRule("sr",dict),
    SymnameRule("symname",dict,false),SymnameRule("Symname",dict,true),
    SymnameRule("sn", dict, false), SymnameRule("Sn", dict, true),
    SymnamesRule("sns", dict, false), SymnamesRule("Sns", dict, true),
    SkipCommand("stexstyleassertion","onn"),
    SkipCommand("stexstyledefinition", "onn"),
    SkipCommand("stexstyleparagraph", "onn"),
    SkipCommand("stexstyleexample", "onn"),
    SkipCommand("stexstyleproblem", "onn"),
    VarDefRule(dict),
    SDefinitionRule(dict),SAssertionRule(dict),SParagraphRule(dict),
    InlineDefRule(dict),InlineAssRule(dict:Dictionary)
  )
  def moduleRules(dict:Dictionary) : List[TeXRule] = List(
    SymDeclRule(dict),SymDefRule(dict),NotationRule(dict),TextSymDeclRule(dict),
    ImportModuleRule(dict),MathStructureRule(dict),ExtStructureRule(dict)
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
    val bg = in.Annotations.add(begin,begin.startoffset,begin.endoffset - begin.startoffset,SymbolKind.Constructor,"module")
    bg.setHover(mp.toString)
    bg.addCodeLens(mp.toString, "", Nil, begin.startoffset, begin.endoffset)
    bg.setSemanticHighlightingClass(SemanticHighlighting.module)
    this.end.foreach {end =>
      val e = in.Annotations.add(end, end.startoffset, end.endoffset - end.startoffset, SymbolKind.Constructor, "module")
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
    val mp = dict.getMPath(name)
    val macr = new STeXModule(parser.trigger,mp,meta,lang,sig)
    //if (deprecation != "") macr.addError("Deprecated: Use " + deprecation + " instead",lvl=Level.Warning)
    dict.openModule(macr)
  }

  override def applyEnd(env: Environment)(implicit parser: ParseState[Begin.BeginMacro]): Environment = env match {
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

  override def applyEnd(env: Environment)(implicit parser: ParseState[Begin.BeginMacro]): Environment = env match {
    case tmm: STeXModule =>
      dict.closeModule
      tmm
    case _ => env
  }
}

case class InheritModuleRule(dm : DictionaryModule,export:Boolean) extends STeXMacro with TeXRule {
  val name = "inherits " + export.toString + " " + dm.path.toString

  override def doAnnotations(in: sTeXDocument): Unit = {
    val a = in.Annotations.add(this,startoffset,endoffset - startoffset,SymbolKind.Module,symbolname = "import")
    a.addCodeLens(dm.path.toString,"",Nil,startoffset,endoffset)
    dm.file.foreach {f =>
      a.setDeclaration(f.toURI.toString,dm.macr.startoffset,dm.macr.endoffset)
    }
    this.plain.foreach {plain =>
      in.Annotations.add(plain,plain.startoffset,plain.endoffset - plain.startoffset,SymbolKind.Module,symbolname = "import")
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

case class UseStructureRule(dict:Dictionary) extends STeXRule with MacroRule with NonFormalRule {
  val name = "usestructure"

  override def apply(implicit parser: ParseState[PlainMacro]): InheritModuleRule = {
    import parser._
    val path = readArgument.asname
    val mod = dict.structure(path)
    val ret = new InheritModuleRule(mod, false) {
      override def doAnnotations(in: sTeXDocument): Unit = {
        val path = mod.macr.asInstanceOf[MathStructure].syminfo.path.toString
        val a = in.Annotations.add(this, startoffset, endoffset - startoffset, SymbolKind.Module, symbolname = "usestructure")
        a.addCodeLens(path, "", Nil, startoffset, endoffset)
        dm.file.foreach { f =>
          a.setDeclaration(f.toURI.toString, dm.macr.startoffset, dm.macr.endoffset)
        }
        this.plain.foreach { plain =>
          in.Annotations.add(plain, plain.startoffset, plain.endoffset - plain.startoffset, SymbolKind.Module, symbolname = "usestructure")
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
      val pa = in.Annotations.add(plain,plain.startoffset,plain.endoffset - plain.startoffset,SymbolKind.Constant,syminfo.path.toString)
      pa.setDeclaration(syminfo.file,syminfo.start,syminfo.end)
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
      o.drop("assoc", "gfc", "reorder", "role", "argtypes")
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
class SymdeclApp(val syminfo:SymdeclInfo,val dict:Dictionary) extends MacroApplication with SemanticMacro {
  val name = syminfo.macroname
  def cloned = new SymdeclApp(syminfo,dict) {
    override val name = syminfo.path.toString
  }

  override def doAnnotations(in: sTeXDocument): Unit = {
    val a = in.Annotations.add(this,startoffset,endoffset - startoffset,SymbolKind.Function,symbolname = syminfo.path.toString)
    a.addCodeLens(syminfo.path.toString,"",Nil,startoffset,endoffset)
    plain.foreach{plain =>
      val pa = in.Annotations.add(plain,plain.startoffset,plain.endoffset - plain.startoffset,SymbolKind.Constant)
      pa.setSemanticHighlightingClass(SemanticHighlighting.declaration)
    }
    if (!syminfo.isDocumented) {
      this.addError("Symbol is not documented",Some("Consider adding an {sdefinition} or {sparagraph}[type=symdoc] for this symbol"),Level.Info)
    }
  }
}
case class TextSymdeclApp(_syminfo:SymdeclInfo,_dict:Dictionary) extends SymdeclApp(_syminfo,_dict)
case class NotationApp(notinfo:NotationInfo,dict:Dictionary,alternatives:List[SymdeclInfo],reftokens:List[TeXTokenLike]) extends MacroApplication with NotationMacro with STeXRule {
  val name = "notation/" + notinfo.syminfo.path + "#" + notinfo.id
  override def doAnnotations(in: sTeXDocument): Unit = {
    val a = in.Annotations.add(this, startoffset, endoffset - startoffset, SymbolKind.Function, symbolname = notinfo.syminfo.path.toString)
    plain.foreach { plain =>
      val pa = in.Annotations.add(plain, plain.startoffset, plain.endoffset - plain.startoffset, SymbolKind.Constant)
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
    val ret = new SymdeclApp(si,dict)
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
    val (_si, o) = parseNameAndOpts(true)
    val si = new SymdeclInfo(_si.macroname,_si.path.module ? (_si.path.name.toString + "-sym"),_si.args,_si.file,_si.start,_si.end,_si.defined,_si.ret)
    val ret = TextSymdeclApp(si,dict)
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
    val ret = new SymdeclApp(si, dict) with NotationMacro {
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
      val pa = in.Annotations.add(plain,plain.startoffset,plain.endoffset - plain.startoffset,SymbolKind.Constant,syminfo.path.toString)
      pa.setDeclaration(syminfo.file,syminfo.start,syminfo.end)
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
    val a = in.Annotations.add(this,startoffset,endoffset - startoffset,SymbolKind.Constant,syminfo.path.toString)
    a.setHover("Variable " + syminfo.path.name.toString)
    plain.foreach {plain =>
      val pa = in.Annotations.add(plain,plain.startoffset,plain.endoffset - plain.startoffset,SymbolKind.Constant)
      pa.setDeclaration(syminfo.file,syminfo.start,syminfo.end)
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

class SymrefApp(dict:Dictionary,val alternatives:List[SymdeclInfo],asPlainString: => String,trivial:Boolean,val reftokens:List[TeXTokenLike]) extends SymRefLike {
  override def doAnnotations(in: sTeXDocument): Unit = {
    val a = in.Annotations.add(this, startoffset, endoffset - startoffset, SymbolKind.Constant, alternatives.head.path.toString)
    a.setHover(alternatives.head.path.toString + {
      if (trivial) "" else "\n\nYields: " + asPlainString
    })
    this.plain.foreach {plain =>
      val pla = in.Annotations.add(plain,plain.startoffset,plain.endoffset - plain.startoffset,SymbolKind.Constant,alternatives.head.path.toString)
      pla.setSemanticHighlightingClass(SemanticHighlighting.symbol)
    }
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
    opts.flatMap(_.getStr("name")) match {
      case Some(_) if dict.getModuleOpt.isEmpty =>
        parser.trigger.addError("\"name\" argument only allowed in modules")
      case Some(_) =>
        val (sym,makemacro) = opts.get.consumeStr("macroname") match {
          case Some(macroname) =>
            (processOpts(macroname,true,opts),true)
          case _ =>
            (processOpts("",false,opts),false)
        }
        sym.isDocumented = true
        val ret = new SymdeclApp(sym, dict)
        addExportRule(ret)
        if (makemacro) addExportRule(ret.cloned)
      case _ =>
    }
    (opts,opts.flatMap(_.consumeStr("for")).map{s =>
      s.split(',').toList.map(_.trim).map(s => (s,dict.resolveName(s)(parser)))
    }.getOrElse(Nil))
  }
}
abstract class InlineStatementRule extends StatementRule with MacroRule {
  override def apply(implicit parser: ParseState[PlainMacro]): MacroApplication = {
    val (opts, fors) = parseStatement
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
      val ret = new MacroApplication
      fors.foreach {
        case (s, Nil) =>
          ret.addError("No symbol " + s + " found")
        case _ =>
      }
      ret
    }
  }
}
abstract class StatementEnvRule extends StatementRule with EnvironmentRule {
  override def applyStart(implicit parser: ParseState[Begin.BeginMacro]): Environment = {
    val (opts, fors) = parseStatement

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
    val ret = new Environment(parser.trigger)
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
    val bg = in.Annotations.add(begin, begin.startoffset, begin.endoffset - begin.startoffset, SymbolKind.Constructor, "module")
    bg.setHover(syminfo.path.toString)
    bg.addCodeLens(syminfo.path.toString, "", Nil, begin.startoffset, begin.endoffset)
    bg.setSemanticHighlightingClass(SemanticHighlighting.declaration)
    this.end.foreach { end =>
      val e = in.Annotations.add(end, end.startoffset, end.endoffset - end.startoffset, SymbolKind.Constructor, "module")
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

  override def applyEnd(env: Environment)(implicit parser: ParseState[Begin.BeginMacro]): Environment = env match {
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

/*
import info.kwarc.mmt.api.{GlobalName, Level, LocalName, MPath, Path}
import info.kwarc.mmt.lsp.SyncedDocUnparsed
import info.kwarc.mmt.stex.parsing.stex.STeXRules.{AssignRule, DonotCopyRule, RenameDeclRule}
import info.kwarc.mmt.stex.parsing.{Environment, EnvironmentRule, Group, LaTeXParseError, LaTeXParserState, MacroApplication, MacroRule, PlainMacro, PlainText, TeXRule, TeXTokenLike}


trait ImportModuleRuleLike extends MacroRule {
  val dict: Dictionary
  val isusemodule: Boolean

  override def parse(plain: PlainMacro)(implicit in: SyncedDocUnparsed, state: LaTeXParserState): TeXTokenLike = safely[TeXTokenLike](plain) {
    var children: List[TeXTokenLike] = Nil
    val (optargs, ch) = readOptArg
    children = ch
    val (n, ch2) = readArg
    val a = optargs match {
      case List(List(pt: PlainText)) => pt.str
      case _ => ""
    }
    children = children ::: ch2
    val path = n match {
      case gr: Group =>
        gr.content match {
          case List(t: PlainText) => t.str
          case _ => {
            plain.addError("Malformed Argument")
            return plain
          }
        }
      case _ =>
        plain.addError("Missing Argument")
        return plain
    }
    val mp = dict.resolveMPath(a, path)
    val ret = ImportModuleApp(plain, mp, children, this, a, path, isusemodule, dict.getModuleOpt)
    safely(ret) {
      dict.addimport(ret)
    }
  }
}



trait NotationRuleLike extends MacroRule {
  val dict: Dictionary

  def optsAndNotation(syminfo: SymdeclInfo, ls: List[List[TeXTokenLike]])(implicit in: SyncedDocUnparsed, state: LaTeXParserState) = {
    val (id, prec, opnot, error, ret) = parseNotOpts(syminfo, ls)
    val (not, nch) = readArg
    var children = nch
    var notation = List(not)
    /*syminfo.args.filter(c => c == 'a' || c == 'B').foreach { a =>
      val (ret, nch) = readArg
      notation ::= ret
      children = children ::: nch
    }*/
    (NotationInfo(syminfo, prec, id, notation.reverse, opnot), error, ret, children)
  }

  def parseNotOpts(syminfo: SymdeclInfo, ls: List[List[TeXTokenLike]]) = {
    var ret = ls
    var id = ""
    var error = ""
    var prec: List[Int] = Nil
    var opnot: Option[List[TeXTokenLike]] = None
    ls.foreach { l =>
      val s = l.mkString.flatMap(c => if (c.isWhitespace) "" else c.toString)
      s match {
        case s if s.startsWith("prec=") =>
          ret = ret.filterNot(_ == l)
        //val rest = s.drop(5)
        // TODO
        case s if !s.contains('=') && id == "" =>
          ret = ret.filterNot(_ == l)
          if (s.startsWith("{") && s.endsWith("}")) id = s.init.tail else id = s
        case s if s.startsWith("op=") =>
          ret = ret.filterNot(_ == l)
          opnot = Some(l)
        case _ =>
          print("")
      }
    }
    (id, prec, opnot, error, ret)
  }
}


trait SymRefRuleLike extends MacroRule {
  val dict: Dictionary
  def doAlternatives(ls : List[GlobalName]) = if (ls.length > 1) {
    class mutvar(val path : GlobalName) { var string = path.module.name.toString + "?" + path.name}
    val retstrings = ls.distinct.map(new mutvar(_))
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
    retstrings.map(r => (r.path,r.string))
  } else Nil
  def getNotations(sym : SymdeclInfo)(implicit state: LaTeXParserState) = {
    val allnots = state.rules.flatMap(_.rules).collect {
      case nl:NotationLike if nl.notinfo.syminfo == sym => nl
    }
    allnots.filter(_.isInstanceOf[SetNotationLike]) ::: allnots.filterNot(_.isInstanceOf[SetNotationLike])
  }
  def parseSym(implicit in: SyncedDocUnparsed, state: LaTeXParserState) = {
    val (nametk,children) = readArg
    val namestr = nametk match {
      case g : Group =>
        g.content match {
          case List(pt: PlainText) => pt.str
          case _ => throw LaTeXParseError("Could not determine name for \\symdef",lvl = Level.Warning)
        }
      case _ => throw LaTeXParseError("Name for \\symdef expected")
    }
    val rn = dict.resolveName(namestr)
    rn match {
      case (None,_) =>
        throw LaTeXParseError("Could not find symbol named " + namestr, lvl = Level.Warning)
      case (Some(r),ls) =>
        (r,nametk,doAlternatives(r.path :: ls),children)
    }
  }

}

trait SemanticMacro extends MacroRule with SymRefRuleLike {
  val syminfo:SymdeclInfo
  lazy val name = syminfo.macroname

  def parseInner(plain:PlainMacro,requireNotation:Boolean)
                          (cons : (PlainMacro,List[TeXTokenLike],Option[NotationInfo]) => TeXTokenLike)(implicit in: SyncedDocUnparsed, state: LaTeXParserState) : TeXTokenLike = safely[TeXTokenLike](plain) {
    var children: List[TeXTokenLike] = Nil
    val (custom, op) = if (state.inmath) {
      val o = readChar('!') match {
        case (b, ch) =>
          children = ch.reverse
          b
      }
      val c = readChar('*') match {
        case (b, ch) =>
          children = children ::: ch.reverse
          b
      }
      (c, o)
    } else {
      (true, readChar('!') match {
        case (b, ch) =>
          children = ch.reverse
          b
      })
    }
    if (custom) {
      val (a, ch) = readSafeArg("\\" + plain.name)
      children = children ::: ch
      cons(plain, children, None)
    } else {
      val notations = getNotations(syminfo)
      val notation = readOptArg match {
        case (Nil, ch) =>
          children = children ::: ch
          notations.headOption match {
            case None =>
              val ret = cons(plain, children, None)
              if (requireNotation) ret.addError("No notation found for " + syminfo.path.toString, lvl=Level.Warning)
              None
            case Some(not) =>
              Some(not.notinfo)
          }
        case (ls, ch) =>
          children = children ::: ch
          val notid = ls.map(_.mkString).mkString
          notations.find(_.notinfo.id == notid) match {
            case None =>
              val ret = cons(plain, children, None)
              if (requireNotation) ret.addError("No notation found for " + syminfo.path.toString, lvl=Level.Warning)
              None
              //return ret
            case Some(not) => Some(not.notinfo)
          }
      }
      if (op) {
        val ret = cons(plain, children, notation)
        /*notation.notinfo.opnotation match {
          case None =>
            if (requireNotation) ret.addError("Notation " + notation.notinfo.id + " for " + notation.notinfo.syminfo.path.toString + " has no operator notation!")
          case _ =>
        }*/
        ret
      } else safely {
        cons(plain, children, notation)
      } {
        syminfo.args.foreach { _ =>
          val (_, nch) = readSafeArg("\\" + plain.name)
          children = children ::: nch
        }
        cons(plain, children, notation)
      }
    }
  }

  override def parse(plain: PlainMacro)(implicit in: SyncedDocUnparsed, state: LaTeXParserState): TeXTokenLike =
    parseInner(plain,true)((pl,ch,nt) => SemanticMacroApp(pl,this,ch,nt))
}

trait VariableRule extends SemanticMacro {
  override def parse(plain: PlainMacro)(implicit in: SyncedDocUnparsed, state: LaTeXParserState): TeXTokenLike =
    parseInner(plain,false)((pl,ch,nt) => SemanticVariableApp(pl,this,ch,nt))
}
trait InstanceRuleLike extends SemanticMacro {
  val module:DictionaryModule

  def parseField(plain: PlainMacro)
                          (cons: (PlainMacro, List[TeXTokenLike], Option[SymdeclInfo], Option[NotationInfo]) => TeXTokenLike)(implicit in: SyncedDocUnparsed, state: LaTeXParserState): TeXTokenLike = safely[TeXTokenLike](plain) {
    var children: List[TeXTokenLike] = Nil
    val (custom, op) = if (state.inmath) {
      val o = readChar('!') match {
        case (b, ch) =>
          children = ch.reverse
          b
      }
      val c = if (o) readChar('*') match {
        case (b, ch) =>
          children = children ::: ch.reverse
          b
      } else false
      (c, o)
    } else {
      (true, readChar('!') match {
        case (b, ch) =>
          children = ch.reverse
          b
      })
    }
    if (!custom && op) return cons(plain,children,None,None)
    if (op) {
      val (a, ch) = readSafeArg("\\" + plain.name)
      children = children ::: ch
      return cons(plain, children, None,None)
    }
    val (sym,ch) = readArg
    children = children ::: ch
    val mac = sym match {
      case g: Group =>
        g.content match {
          case List(pt: PlainText) =>
            module.getRules("").collectFirst {
              case mr : SemanticMacro if mr.syminfo.path.name.toString == pt.str => mr
            }.getOrElse {
              throw LaTeXParseError("No field " + pt.str + " found in module " + module.path)
            }
          case _ => throw LaTeXParseError("Could not determine field name for instance \\" + this.name, lvl = Level.Warning)
        }
      case _ => throw LaTeXParseError("Field name for instance \\" + this.name + " expected")
    }
    mac.parseInner(plain,false){(pl,ch,nt) =>
      cons(pl,children ::: ch,Some(mac.syminfo),nt)
    }
  }
}
trait InstanceFieldRule extends InstanceRuleLike {
  override def parse(plain: PlainMacro)(implicit in: SyncedDocUnparsed, state: LaTeXParserState): TeXTokenLike =
    parseField(plain)((pl, ch, sd,nt) => StructureFieldApp(pl, this, ch, sd,nt))
}
trait VarInstanceFieldRule extends InstanceRuleLike {
  override def parse(plain: PlainMacro)(implicit in: SyncedDocUnparsed, state: LaTeXParserState): TeXTokenLike =
    parseField(plain)((pl, ch, sd,nt) => VarStructureFieldApp(pl, this, ch, sd,nt))
}

class InlineStatementRule(val name:String,val dict:Dictionary) extends MacroRule {
  override def parse(plain: PlainMacro)(implicit in: SyncedDocUnparsed, state: LaTeXParserState): TeXTokenLike = {
    val (optargs, ch) = readOptArg
    var name: Option[String] = None
    optargs.foreach { l =>
      val s = l.mkString.flatMap(c => if (c.isWhitespace) "" else c.toString)
      s match {
        case s if s.trim.startsWith("name=") =>
          name = Some(s.drop(5))
        case s if s.trim.startsWith("type=") =>
        case s if s.trim.startsWith("id=") =>
        case s if s.trim.startsWith("for=") =>
        case s if s.trim.startsWith("judgment=") =>
        case _ =>
          throw LaTeXParseError("Unknown key " + s.trim)
      }
    }
    val (_,nch) = readArg
    name match {
      case Some(name) =>
        val gn = dict.getGlobalName(name)
        val self = this
        val ret = new InlineStatement(plain,ch:::nch,this) with SemanticMacro {
          val dict = self.dict
          override val syminfo: SymdeclInfo =
            SymdeclInfo(gn.toString, gn, "", "", false, dict.getFile, plain.startoffset, children.last.endoffset,false)
        }
        val mod = dict.getModule
        mod.exportrules ::= ret
        mod.rules ::= ret
        ret
      case None => new InlineStatement(plain,ch:::nch,this) {}
    }
  }
}
class StatementRule(_name:String,val dict:Dictionary) extends EnvironmentRule(_name) {
  override def finalize(env: Environment)(implicit state: LaTeXParserState): Environment = {
    env.begin match {
      case bs : BeginStatement =>
        bs.name match {
          case None =>
            new Statement(bs, env.end, env.children, this) {}
          case Some(name) =>
            val gn = dict.getGlobalName(name)
            val self = this
            val ret = new Statement(bs,env.end,env.children,this) with SemanticMacro {
              val dict = self.dict
              override val syminfo: SymdeclInfo =
                SymdeclInfo(gn.toString,gn,"","",false,dict.getFile,env.children.head.startoffset,children.last.endoffset,false)
            }
            val mod = dict.getModule
            mod.exportrules ::= ret
            mod.rules ::= ret
            ret
        }
      case _ => env
    }
  }

  protected def doFor(s:String)(implicit in: SyncedDocUnparsed, state: LaTeXParserState) = {
    dict.resolveName(s) match {
      case (Some(sd), _) =>
        sd.isDocumented = true
      case _ =>
    }
  }

  override def parse(begin: MacroApplication)(implicit in: SyncedDocUnparsed, state: LaTeXParserState): MacroApplication = {
    val (optargs, ch) = readOptArg
    var name : Option[String] = None
    optargs.foreach { l =>
      val s = l.mkString.flatMap(c => if (c.isWhitespace) "" else c.toString)
      s match {
        case s if s.trim.startsWith("name=") =>
          name = Some(s.drop(5))
        case s if s.trim.startsWith("type=") =>
        case s if s.trim.startsWith("style=") =>
        case s if s.trim.startsWith("id=") =>
        case s if s.trim.startsWith("title=") =>
        case s if s.trim.startsWith("for=") =>
          val r = s.drop(4).trim
          (
            if (r.headOption.contains('{') && r.lastOption.contains('}')) r.tail.init else r
          ).split(',').foreach(st => doFor(st.trim))
        case s if s.trim.startsWith("judgment=") =>
        case _ =>
          throw LaTeXParseError("Unknow key " + s.trim)
      }
    }
    BeginStatement(begin.plain,begin.children ::: ch,this,name)
  }
}
trait MathStructureInfo extends SymdeclInfo {
  val module_path : MPath
}
case class MathStructureMacro(
                               pm:PlainMacro,
                               mpi:MPath,
                               macroname:String,
                               symbolpath:GlobalName,
                               ch:List[TeXTokenLike],
                               rl : STeXRules.MathStructureRule,
                               file:String,dict:Dictionary
                             ) extends TeXModuleLike(pm,mpi,ch,rl) with TeXRule with SemanticMacro {
  val sig = ""
  //override lazy val name = "mathstructure " + mpi.toString
  val syminfo = new SymdeclInfo(macroname,symbolpath,"","",false,this.file,this.startoffset,this.endoffset,true) with MathStructureInfo {
    val module_path = mpi
  }
}

trait StructureLikeRule extends EnvironmentRule with InStructureRule {
  val dict: Dictionary

  def finalizeStructure(mod:StructureModuleBegin,env:StructureModule)(implicit state: LaTeXParserState) = {}
  override def finalize(env: Environment)(implicit state: LaTeXParserState): Environment = {
    env.begin match {
      case sm: StructureModuleBegin =>
        dict.getModule.macr match {
          case bg: StructureModuleBegin =>
            var allnots = bg.dom.getRules("").collect {
              case nl: NotationLike => nl
            }
            allnots = allnots.filter(_.isInstanceOf[SetNotationLike]) ::: allnots.filterNot(_.isInstanceOf[SetNotationLike])
            bg.domfields.foreach { sm =>
              if (!bg.fields.contains(sm)) {
                val name = getName / sm.syminfo.path.name
                bg.fields(sm) = (Some(name), name.toString, false)
              } else if (bg.fields(sm)._1.isEmpty) {
                val trpl = bg.fields(sm)
                val name = getName / sm.syminfo.path.name
                bg.fields(sm) = (Some(name), trpl._2, trpl._3)
              }
            }
            dict.closeModule
            val self = this
            bg.fields.foreach { case (sm, (Some(newname), macroname, ass)) =>
              val macr = new SemanticMacro {
                val dict = self.dict
                val syminfo = SymdeclInfo(macroname, dict.getGlobalName(newname.toPath), sm.syminfo.args, sm.syminfo.assoctype, false, dict.getFile, bg.startoffset, env.endoffset,ass)
              }
              dict.getModule.exportrules ::= macr
              dict.getModule.rules ::= macr
              allnots.filter(_.notinfo.syminfo == sm.syminfo).foreach { nt =>
                val nr = new MacroApplication(nt.plain, nt.children, this) with NotationLike {
                  override val notinfo: NotationInfo = NotationInfo(
                    macr.syminfo, nt.notinfo.prec, nt.notinfo.id, nt.notinfo.notation, nt.notinfo.opnotation
                  )

                  override def name: String = nt.name
                }
                dict.getModule.exportrules ::= nr
                dict.getModule.rules ::= nr
              }
            }
            val ret = StructureModule(sm, env.end, env.children)
            finalizeStructure(bg,ret)
            ret
          case _ =>
            dict.closeModule
            StructureModule(sm, env.end, env.children)
        }
      case _ => env
    }
  }
  def setupBody[A <:StructureModuleBegin](archive: List[List[TeXTokenLike]], domaintk: TeXTokenLike, nametk: Option[TeXTokenLike])
                                         (make: (MPath,DictionaryModule) => A)(implicit state: LaTeXParserState) = {
    val domstr = domaintk match {
      case gr: Group =>
        gr.content match {
          case List(t: PlainText) => t.str
          case _ =>
            throw LaTeXParseError("CopyModule missing domain")
        }
      case _ =>
        throw LaTeXParseError("CopyModule missing domain")
    }
    val a = archive match {
      case List(List(pt: PlainText)) => pt.str
      case _ => ""
    }
    val module = dict.resolveName(domstr) match {
      case (Some(ds : MathStructureInfo),_) =>
        dict.requireModule(ds.module_path,a,domstr)
      case _ =>
        dict.requireModule(dict.resolveMPath(a, domstr), a, domstr)
    }

    val name = nametk match {
      case Some(gr: Group) =>
        gr.content match {
          case List(t: PlainText) => LocalName(t.str)
          case _ =>
            throw LaTeXParseError("CopyModule missing name")
        }
      case Some(_) =>
        throw LaTeXParseError("CopyModule missing name")
      case None =>
        LocalName(module.path)
    }

    val mp = dict.getModule.path.parent ? (dict.getModule.path.name/ name)
    val ret = dict.openModule(make(mp,module))
    state.addRule(RenameDeclRule(dict))
    state.addRule(AssignRule(dict))
    state.addRule(DonotCopyRule(dict))
    ret
  }
}

trait InStructureRule extends TeXRule {
  val dict:Dictionary
  def getName = {
    dict.getModule.macr match {
      case bg: StructureModuleBegin =>
        bg.mp.name.last
      case _ =>
        throw LaTeXParseError("Not in CopyModule")
    }
  }
  def addToField(sm: SemanticMacro,name:Option[LocalName] = None,macroname:String = "",isassigned: Boolean = false) = {
    dict.getModule.macr match {
      case bg: StructureModuleBegin =>
        bg.fields.get(sm) match {
          case None =>
            bg.fields(sm) = (name,macroname,isassigned)
          case Some((a,b,c)) =>
            bg.fields(sm) = (
              (a,name) match {
                case (None,Some(n)) => Some(n)
                case (Some(n),None) => Some(n)
                case _ => name
              },
              (b,macroname) match {
                case (s,"") => s
                case _ => macroname
              },
              c || isassigned
            )
        }
      case _ =>
        throw LaTeXParseError("Not in CopyModule")
    }
  }
  def getTarget(tk : TeXTokenLike) = {
    tk match {
      case g: Group =>
        g.content match {
          case List(pt: PlainText) =>
            val tg = pt.str
            dict.getModule.macr match {
              case bg: StructureModuleBegin =>
                bg.domfields.collectFirst {
                  case sm if sm.syminfo.path.name.toString == tg => sm
                }.getOrElse {
                  throw LaTeXParseError("No field " + tg + " found in copymodule")
                }
              case _ =>
                throw LaTeXParseError("Not in CopyModule")
            }
          case _ => throw LaTeXParseError("Could not identify field for \\" + name, lvl = Level.Warning)
        }
      case _ => throw LaTeXParseError("Could not identify field for \\" + name)
    }
  }
}

trait ProofLike extends TeXRule {
  def parseI(plain: PlainMacro,children:List[TeXTokenLike])(implicit in: SyncedDocUnparsed, state: LaTeXParserState): MacroApplication = {
    val math = state.inmath
    state.inmath = true
    val (_, ch) = try {
      readOptArg
    } finally {
      state.inmath = math
    }
    val (_, ch2) = readArg
    new MacroApplication(plain, children ::: ch ::: ch2, this)
  }

}
object Proofs {

  lazy val rules = List(ProofEnv("subproof"), ProofMacro("assumption"), ProofMacro("spfstep"),
    ProofMacro("conclude"), new TeXRule with MacroRule {
      val name: String = "eqstep"

      override def parse(plain: PlainMacro)(implicit in: SyncedDocUnparsed, state: LaTeXParserState): TeXTokenLike = {
        val math = state.inmath
        state.inmath = true
        val (_, ch) = try {
          readArg
        } finally {
          state.inmath = math
        }
        new MacroApplication(plain, ch, this)
      }
    })
}
case class ProofEnv(_name:String) extends EnvironmentRule(_name) with ProofLike {
  override def finalize(env: Environment)(implicit state: LaTeXParserState): Environment = env
  def parse(begin: MacroApplication)(implicit in: SyncedDocUnparsed, state: LaTeXParserState): MacroApplication = {
    val ret = parseI(begin.plain,begin.children)
    Proofs.rules.foreach(state.addRule(_))
    ret
  }
}
case class ProofMacro(name : String) extends MacroRule with ProofLike {
  override def parse(plain: PlainMacro)(implicit in: SyncedDocUnparsed, state: LaTeXParserState): TeXTokenLike = {
    parseI(plain, Nil)
  }
}

case class MMTInterfaceRule(dict:Dictionary) extends EnvironmentRule("mmtinterface") {
  override def parse(begin: MacroApplication)(implicit in: SyncedDocUnparsed, state: LaTeXParserState): MacroApplication = {
    var children: List[TeXTokenLike] = Nil
    val (opts, cho) = readOptArg
    children = cho
    val (nametk,ch) = readArg
    children = ch
    val (pathstr,ch2) = readArg
    children = children ::: ch2
    val mmtpath = pathstr match {
      case gr: Group =>
        gr.content match {
          case List(pt: PlainText) =>
            try {
              Path.parseM(pt.str)
            } catch {
              case t =>
                throw LaTeXParseError("Module path expected")
            }
          case _ =>
            throw LaTeXParseError("Missing MMT-URI")
        }
      case _ => throw LaTeXParseError("Missing MMT-URI")
    }
    val name = nametk match {
      case gr: Group =>
        gr.content match {
          case List(t: PlainText) => t.str
          case _ =>
            throw LaTeXParseError("Module name expected")
        }
      case _ =>
        throw LaTeXParseError("Module name expected")
    }
    val stexpath = dict.getMPath(name)
    val ret = MMTInterfaceBegin(begin.plain,mmtpath,stexpath,begin.children ::: children,this)
    dict.openModule(ret)
  }

  override def finalize(env: Environment)(implicit state: LaTeXParserState): Environment = {
    dict.closeModule
    env
  }
}

 */
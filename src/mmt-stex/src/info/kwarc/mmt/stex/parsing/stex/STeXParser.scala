package info.kwarc.mmt.stex.parsing.stex

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.archives.{Archive, source}
import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.parser.{SourcePosition, SourceRef, SourceRegion}
import info.kwarc.mmt.api.utils.{File, URI}
import info.kwarc.mmt.lsp.{SyncedDocUnparsed, SyncedDocument}
import info.kwarc.mmt.stex.STeX
import info.kwarc.mmt.stex.lsp.sTeXDocument
import info.kwarc.mmt.stex.parsing._
import org.eclipse.lsp4j.SymbolKind

import java.util.concurrent.ForkJoinPool
import scala.collection.mutable
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}

class MutList(var ls : List[DictionaryModule] = Nil)
class DictionaryModule(val macr:TeXModuleLike,val meta:Option[DictionaryModule]) extends RuleContainer {
  val path = macr.mp
  var archive : Option[Archive] = None
  var file : Option[File] = None
  var lang:String = ""
  var imports : List[(DictionaryModule,Boolean)] = Nil
  var exportrules : List[TeXRule] = Nil //List(ModuleRule(this,false))
  def getRules(lang:String,dones : MutList = new MutList()) : List[TeXRule] = if (dones.ls.contains(this)) Nil else {
    dones.ls ::= this
    val r = exportrules ::: imports.filter(p => p._2 && !dones.ls.contains(p._1)).flatMap(m => ModuleRule(m._1,true) :: m._1.getRules(lang,dones))
    langs.get(lang) match {
      case Some(m) => m.exportrules ::: r
      case _ => r
    }
  }
  val langs = mutable.HashMap.empty[String,DictionaryModule]

}
case class MetaTheoryRule(mp : MPath) extends TeXRule {
  val name = "meta theory rule"
}
class Dictionary(val controller:Controller,parser:STeXSuperficialParser) {

  def defaultMeta(implicit state:LaTeXParserState) = {
    state.getRules.collectFirst {
      case MetaTheoryRule(mp) => mp
    }.getOrElse{
      Path.parseM("http://mathhub.info/sTeX/meta?Metatheory")
    }
  }

  implicit val ec : ExecutionContext = ExecutionContext.fromExecutorService(new ForkJoinPool(1000))

  def getMPath(name:String):MPath = {
    current_modules.headOption match {
      case None =>
        current_namespace match {
          case Some(d) if d.last == name =>
            d.^ ? name
          case Some(d) =>
            d ? LocalName.parse(name)
          case _ =>
            throw LaTeXParseError("Error resolving Global Name")
        }
      case Some(m) =>
        m.path.parent ? (m.path.name / LocalName.parse(name))
    }
  }
  def getGlobalName(name:String):GlobalName = current_modules.head.path ? LocalName.parse(name)
  def getLanguage : String = current_language
  def getFile : String = current_file.get.toURI.toString


  def getNS(a:Archive) = a.ns match {
    case Some(p:DPath) => p
    case None => a.properties.get("source-base") match {
      case Some(s) => Path.parseD(s,NamespaceMap.empty)
      case _ =>
        DPath(a.root.toURI)
    }
  }

  def resolveMPath(archiveid:String,path:String) : MPath = {
    val archive = if (archiveid == "") current_archive else Some(controller.backend.getArchive(archiveid).getOrElse{
      throw LaTeXParseError("Archive missing: " + archiveid)
    })
    resolveMPath(archive,path)
  }

  def resolveMPath(archive:Option[Archive],path:String) : MPath = {
    val (rpath,name) = path.split('?').toList match {
      case List(n) => (Nil,n)
      case List(p,n) => (p.split('/').toList,n)
    }
    (archive,rpath) match {
      case (_,Nil) if current_namespace.exists(dp => all_modules.isDefinedAt(dp ? LocalName.parse(name))) =>
        current_namespace.get ? LocalName.parse(name)
      case (Some(a),Nil) =>
        getNS(a) ? LocalName.parse(name)
      case (Some(a),p) =>
        p.foldLeft(getNS(a))((d,s) => d / s) ? LocalName.parse(name)
      case (None,Nil) =>
        DPath(current_file.head.up.toURI) ? LocalName.parse(name)
      case (None,p) =>
        p.foldLeft(DPath(current_file.head.up.toURI))((d,s) => d / s) ? LocalName.parse(name)
    }
  }

  private def compute_ns : Unit = {
    current_file match {
      case Some(f) =>
        current_archive match {
          case Some(a) => getNS(a) match {
            case d:DPath =>
              var sub = (a / source).relativize(f).stripExtension
              sub.getExtension match {
                case Some(lang) =>
                  current_language = lang
                  sub = sub.stripExtension
                case _ =>
              }
              current_namespace = Some(sub.segments.foldLeft(d)((ns,s) => ns / s))
              return
            case _ =>
          }
          case _ =>
        }
        var sub = f.stripExtension
        sub.getExtension match {
          case Some(lang) =>
            current_language = lang
            sub = sub.stripExtension
          case _ =>
        }
        current_namespace = Some(DPath(sub.toURI))
      case _ =>
        current_namespace = None
    }
  }

  private var current_archive: Option[Archive] = None
  private var current_file: Option[File] = None
  private var current_modules : List[DictionaryModule] = Nil
  val all_modules = mutable.HashMap.empty[MPath,DictionaryModule]
  private var current_namespace : Option[DPath] = None
  private var current_language : String = "en"

  def getModule = current_modules.headOption.getOrElse({
    throw LaTeXParseError("Only allowed in Module")
  })
  def getModuleOpt = current_modules.headOption

  def inFile[A](file : File,a:Option[Archive] = None)(f: => A) : A = {
    //filetracker ::= file
    val prevarch = current_archive
    val prevfile = current_file
    val prevmod = current_modules
    val prevns = current_namespace
    val prevlang = current_language
    a.foreach(x => current_archive = Some(x))
    current_file = Some(file)
    current_modules = Nil
    compute_ns
    try {
      f
    } finally {
      current_namespace = prevns
      current_language = prevlang
      current_file = prevfile
      current_archive = prevarch
      current_modules = prevmod
    }
  }
  def openModule[A <:TeXModuleLike](macr:A)(implicit state: LaTeXParserState) = {
    val meta = macr match {
      case tmm: TeXModuleMacro =>
        tmm.meta match {
          case Some(m) =>
            all_modules.get(m)
          case _ => None
        }
      case _ => None
    }

    val mod = if (macr.sig != "") {
      val language = getLanguage
      all_modules.getOrElse(macr.mp,{
        val sigfile = current_file.get.stripExtension.stripExtension.setExtension(macr.sig + ".tex")
        Await.result(Future {
          try {
            parser.applyFormally(sigfile, current_archive)
          } catch {
            case e: Throwable =>
              e.printStackTrace()
              print("")
          }
        }, Duration.Inf)
        all_modules.get(macr.mp) match {
          case None =>
            macr.addError("No module " + macr.mp.toString + " found", Some("In file " + sigfile))
            new DictionaryModule(macr,meta)
          case Some(m) =>
            val n = new DictionaryModule(macr,meta)
            n.imports ::= (m,true)
            m.langs(language) = n
            n.rules = m.getRules(language)
            n
        }
      })
    } else new DictionaryModule(macr,meta)
    mod.exportrules = meta.toList.flatMap(_.getRules(getLanguage))
    mod.rules = mod.exportrules ::: mod.rules
    current_file.foreach(f => mod.file = Some(f))
    current_archive.foreach(a => mod.archive = Some(a))
    current_modules ::= mod
    state.rules ::= mod
    mod.rules = parser.moduleRules ::: mod.rules
    macr
  }
  def closeModule(implicit state: LaTeXParserState) = current_modules.headOption match {
    case Some(m) =>
      val mod = m
      state.rules = state.rules.filterNot(_ == mod)
      current_modules = current_modules.tail
      if (mod.macr.sig == "") all_modules(mod.path) = mod
    case _ =>
  }

  //private case class ParseThread(file : File,ft : Future[Unit])
  //private var filetracker : List[ParseThread] = Nil
  private var readfiles : List[File] = Nil

  def requireModule(mp: MPath, arch: String, mpstr: String): DictionaryModule = {
    all_modules.getOrElse(mp, {
      val archive = if (arch == "") current_archive else controller.backend.getArchive(arch)
      val (rpath, name) = mpstr.split('?').toList match {
        case List(n) => ("", n)
        case List(p, n) => (p, n)
      }
      val abs = (archive, rpath) match {
        case (Some(a), "") =>
          (a / info.kwarc.mmt.api.archives.source) / name
        case (Some(a), p) =>
          (a / info.kwarc.mmt.api.archives.source) / p
        case (None, "") =>
          current_file.get.up
        case (None, p) =>
          current_file.get.up / p
      }
      val lang = getLanguage
      val file = if (abs.setExtension("tex").exists()) abs.setExtension("tex")
      else if (abs.setExtension(lang + ".tex").exists()) abs.setExtension(lang + ".tex")
      else if (abs.setExtension("en.tex").exists()) abs.setExtension("en.tex")
      else {
        val nabs = abs / name
        if (nabs.setExtension("tex").exists()) nabs.setExtension("tex")
        else if (nabs.setExtension(lang + ".tex").exists()) nabs.setExtension(lang + ".tex")
        else if (nabs.setExtension("en.tex").exists()) nabs.setExtension("en.tex")
        else {
          throw LaTeXParseError("No candidate file for module " + mp + " found")
        }
      }
      /*
      filetracker.find(_.file == file) match {
        case Some(f) => Await.result(f.ft,Duration.Inf)
        case None =>
          val future = Future {
            try { */
      if (readfiles contains file) {
        throw LaTeXParseError("No module " + mp + " found")
      }
      else {
        def add() = {
          readfiles ::= file
        }
        try {
          parser.applyFormally(file, archive)
        } finally {
          add()
        }
      } /*
            } catch {
              case e: Throwable =>
                e.printStackTrace()
                print("")
            } finally {
              filetracker = filetracker.filterNot(_.file == file)
            }
            ()
          }
          filetracker ::= ParseThread(file, future)
          Await.result(future, Duration.Inf)
      } */

      all_modules.getOrElse(mp, {
        throw LaTeXParseError("No module " + mp + " found")
      })

    })
  }

  def addimport(ima: ImportModuleApp)(implicit state: LaTeXParserState): ImportModuleApp = {
    val mod = all_modules.get(ima.mp) match {
      case Some(mod) =>
        if (state.getRules.contains(ModuleRule(mod,true)) ||
          (state.getRules.contains(ModuleRule(mod,_)) && ima.isusemodule)
        ) {
          ima.addError("Redundant Import",None,Level.Info)
        }
        mod
      case _ =>
        requireModule(ima.mp,ima.archivestring,ima.path)
    }
    if (ima.isusemodule) {
      state.rules.headOption.foreach(r => r.rules = ModuleRule(mod,false) :: mod.getRules(getLanguage) ::: r.rules)
    } else current_modules.headOption match {
      case Some(m) =>
        m.imports ::= (mod, !ima.isusemodule)
        m.rules = ModuleRule(mod,true) :: mod.getRules(getLanguage) ::: m.rules
      case None =>
        throw LaTeXParseError("Only allowed in module")
    }
    ima
  }

}

class STeXSuperficialParser(controller:Controller) {
  def init() = controller.backend.getArchive("sTeX/meta-inf") match {
    case Some(a) =>
      applyFormally((a / source) / "Metatheory.en.tex",Some(a))
    case _ =>
  }
  val dict = new Dictionary(controller,this)
  var allsTeXRules = STeXRules.allRules(dict) ::: LaTeXRules.allrules
  var moduleRules : List[TeXRule] = STeXRules.moduleRules(dict)
  def apply(file: File,a:Option[Archive] = None) : List[TeXTokenLike] = apply(File.read(file),file,a)
  def apply(string : String,asFile : File,arch:Option[Archive]) : List[TeXTokenLike] = {
    val sd = new SyncedDocument
    sd.set(string)
    apply(sd,asFile,arch)
  }
  def apply(sd : SyncedDocument, asFile: File, arch: Option[Archive]): List[TeXTokenLike] = {
    dict.inFile(asFile, arch) {
      SuperficialLaTeXParser(sd, allsTeXRules)
    }
  }
  def applyFormally(file: File,a:Option[Archive] = None) : List[TeXTokenLike] = applyFormally(File.read(file),file,a)
  def applyFormally(string : String,asFile : File,arch:Option[Archive]) : List[TeXTokenLike] = {
    dict.inFile(asFile,arch) {
      SuperficialLaTeXParser(string,allsTeXRules.filterNot{_.isInstanceOf[NonFormalRule]})
    }
  }
}

case class SymdeclInfo(macroname:String,path:GlobalName,args:String,assoctype:String,local:Boolean,file:String,start:Int,end:Int,defined:Boolean) {
  def arity = args.length
}
case class NotationInfo(syminfo:SymdeclInfo,prec:List[Int],id:String,notation:List[TeXTokenLike],opnotation:Option[List[TeXTokenLike]])

object STeXRules {

  //val defaultMeta = Path.parseM("http://mathhub.info/sTeX/meta?Metatheory")

  def allRules(dict:Dictionary) = List(
    ModuleRule(dict),UseModuleRule(dict),NotationRule(dict),mmtrule,
    SymrefRule(dict),SymnameRule(dict),CapSymnameRule(dict),
    DefiniendumRule(dict),DefinameRule(dict),CapDefinameRule(dict),ProblemRule(dict),
    VarDefRule(dict),VarInstanceRule(dict),UseStructureRule(dict),
    patchdefinitionrule,patchassertionrule,stexinline,stexcode,nstexcode,texcode,ProofEnv("sproof"),
    MMTInterfaceRule(dict),SetMetaTheoryRule(dict)
  )
  def moduleRules(dict:Dictionary) = List(
    ImportModuleRule(dict),new SymDefRule(dict),SymDeclRule(dict),new MathStructureRule(dict),
    ExtStructureRule(dict),TextSymDeclRule(dict),MMTDef(dict),
    AssertionRule(dict),InlineAssertionRule(dict),InstanceRule(dict),
    CopymoduleRule(dict),InterpretmoduleRule(dict),RealizationRule(dict)
  )

  class PatchRule(name : String) extends SimpleMacroRule(name,2) {
    override def parse(plain: PlainMacro)(implicit in: SyncedDocUnparsed, state: LaTeXParserState): MacroApplication = {
      val inmath = state.inmath
      val currrules = state.rules
       try {
        state.rules = Nil
         readOptArg
        super.parse(plain)
      } finally {
        state.rules = currrules
      }
    }
  }
  val patchdefinitionrule = new PatchRule("stexpatchdefinition")
  val patchassertionrule = new PatchRule("stexpatchassertion")

  val stexinline = new InlineVerbRule("stexinline")
  val texcode = new MacroRule {
    override def name: String = "texcode"
    override def parse(plain: PlainMacro)(implicit in: SyncedDocUnparsed, state: LaTeXParserState): TeXTokenLike = {
      val currrules = state.rules
      val main = try {
        state.rules = Nil
        readArg
      } finally {
        state.rules = currrules
      }
      new SimpleMacroApplication(plain, main._2, false, Nil, this)
    }
  }

  val stexcode = new EnvironmentRule("stexcode") with VerbatimLikeRule {
    override def finalize(env: Environment)(implicit state: LaTeXParserState): Environment = env
    override def parse(begin: MacroApplication)(implicit in: SyncedDocUnparsed, state: LaTeXParserState): MacroApplication = {
      val (terminated, nch) = readVerb("\\end{stexcode}")
      val ret = new MacroApplication(begin.plain, begin.children ::: nch :: Nil, this)
      if (!terminated) {
        ret.addError("stexcode environment closed unexpectedly")
      }
      ret
    }
  }

  val nstexcode = new EnvironmentRule("nstexcode") with VerbatimLikeRule {
    override def finalize(env: Environment)(implicit state: LaTeXParserState): Environment = env
    override def parse(begin: MacroApplication)(implicit in: SyncedDocUnparsed, state: LaTeXParserState): MacroApplication = {
      val (terminated, nch) = readVerb("\\end{nstexcode}")
      val ret = new MacroApplication(begin.plain, begin.children ::: nch :: Nil, this)
      if (!terminated) {
        ret.addError("nstexcode environment closed unexpectedly")
      }
      ret
    }
  }

  val mmtrule = new SimpleMacroRule("MMTrule",2) {
    override def parse(plain: PlainMacro)(implicit in: SyncedDocUnparsed, state: LaTeXParserState): MacroApplication = {
      val inmath = state.inmath
      try {
        state.inmath = true
        super.parse(plain)
      } finally {
        state.inmath = inmath
      }
    }
  }

  case class SymDeclRule(dict:Dictionary) extends MacroRule with SymDeclRuleLike {
    val name = "symdecl"
    override def parse(plain: PlainMacro)(implicit in: SyncedDocUnparsed, state: LaTeXParserState): TeXTokenLike = safely[TeXTokenLike](plain){
      val (sdinfo,children,err,nopt) = parseNameAndOpts(dict.getFile)
      val ret = SymdeclApp(plain,children,this,sdinfo)
      if (err != "") ret.addError(err)
      if (nopt.nonEmpty) ret.addError("Unknown parameters for \\symdecl: " + nopt.mkString(", "))
      val mod = dict.getModule
      mod.exportrules ::= ret
      mod.rules ::= ret
      ret
    }
  }

  case class SymrefRule(dict:Dictionary) extends SymRefRuleLike with NonFormalRule {
    override val name: String = "symref"

    override def parse(plain: PlainMacro)(implicit in: SyncedDocUnparsed, state: LaTeXParserState): TeXTokenLike = safely[TeXTokenLike](plain){
      val (_,ch1) = readOptArg
      val (info,ntk,alt,ch) = parseSym
      val (_,ch2) = readArg
      val ret = new MacroApplication(plain,ch1 ::: ch ::: ch2,this) with SymRefLike {
        override val syminfo: SymdeclInfo = info
        override val alternatives = (ntk,alt)
        doInit
      }
      //if (err != "") ret.addError(err,lvl = Level.Warning)
      ret
    }
  }
  case class SymnameRule(dict:Dictionary) extends SymRefRuleLike with NonFormalRule {
    override val name: String = "symname"

    override def parse(plain: PlainMacro)(implicit in: SyncedDocUnparsed, state: LaTeXParserState): TeXTokenLike = safely[TeXTokenLike](plain){
      val (_,ch1) = readOptArg
      val (info,ntk,alt,ch) = parseSym
      //val (_,ch2) = readArg
      val ret = new MacroApplication(plain,ch1 ::: ch,this) with SymRefLike {
        override val syminfo: SymdeclInfo = info
        override val alternatives = (ntk, alt)
        doInit
      }
      //if (err != "") ret.addError(err,lvl = Level.Warning)
      ret
    }
  }
  case class CapSymnameRule(dict:Dictionary) extends SymRefRuleLike with NonFormalRule {
    override val name: String = "Symname"

    override def parse(plain: PlainMacro)(implicit in: SyncedDocUnparsed, state: LaTeXParserState): TeXTokenLike = safely[TeXTokenLike](plain){
      val (_,ch1) = readOptArg
      val (info,ntk,alt,ch) = parseSym
      //val (_,ch2) = readArg
      val ret = new MacroApplication(plain,ch1 ::: ch,this) with SymRefLike {
        override val syminfo: SymdeclInfo = info
        override val alternatives = (ntk, alt)
        doInit
      }
      //if (err != "") ret.addError(err,lvl = Level.Warning)
      ret
    }
  }
  case class DefiniendumRule(dict:Dictionary) extends SymRefRuleLike with NonFormalRule {
    override val name: String = "definiendum"

    override def parse(plain: PlainMacro)(implicit in: SyncedDocUnparsed, state: LaTeXParserState): TeXTokenLike = safely[TeXTokenLike](plain){
      val (_,ch1) = readOptArg
      val (info,ntk,alt,ch) = parseSym
      val (_,ch2) = readArg
      val ret = new MacroApplication(plain,ch1 ::: ch ::: ch2,this) with SymRefLike {
        override val syminfo: SymdeclInfo = info
        override val alternatives = (ntk, alt)
        doInit
      }
      //if (err != "") ret.addError(err,lvl = Level.Warning)
      ret
    }
  }
  case class DefinameRule(dict:Dictionary) extends SymRefRuleLike with NonFormalRule {
    override val name: String = "definame"

    override def parse(plain: PlainMacro)(implicit in: SyncedDocUnparsed, state: LaTeXParserState): TeXTokenLike = safely[TeXTokenLike](plain){
      val (_,ch1) = readOptArg
      val (info,ntk,alt,ch) = parseSym
      //val (_,ch2) = readArg
      val ret = new MacroApplication(plain,ch1 ::: ch,this) with SymRefLike {
        override val syminfo: SymdeclInfo = info
        override val alternatives = (ntk, alt)
        doInit
      }
      //if (err != "") ret.addError(err,lvl = Level.Warning)
      ret
    }
  }

  case class CapDefinameRule(dict:Dictionary) extends SymRefRuleLike with NonFormalRule {
    override val name: String = "Definame"

    override def parse(plain: PlainMacro)(implicit in: SyncedDocUnparsed, state: LaTeXParserState): TeXTokenLike = safely[TeXTokenLike](plain){
      val (_,ch1) = readOptArg
      val (info,ntk,alt,ch) = parseSym
      //val (_,ch2) = readArg
      val ret = new MacroApplication(plain,ch1 ::: ch,this) with SymRefLike {
        override val syminfo: SymdeclInfo = info
        override val alternatives = (ntk, alt)
        doInit
      }
      //if (err != "") ret.addError(err,lvl = Level.Warning)
      ret
    }
  }
  case class DonotCopyRule(dict:Dictionary) extends MacroRule with InStructureRule {
    val name = "donotcopy"

    override def parse(plain: PlainMacro)(implicit in: SyncedDocUnparsed, state: LaTeXParserState): TeXTokenLike = {
      val (domtk, ch) = readArg
      val path = domtk match {
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
      val mp = dict.resolveMPath(None, path)
      val mod = dict.all_modules.getOrElse(mp,{
        plain.addError("Module not found: " + mp)
        return plain
      })
      dict.getModule.macr match {
        case bg: StructureModuleBegin =>
          mod.getRules(dict.getLanguage).foreach {
            case sd: SemanticMacro =>
              val ret = bg.domfields.foreach {
                case sm if sm.syminfo.path == sd.syminfo.path =>
                  addToField(sm, isassigned = true)
                case _ =>
              }
            case _ =>
          }
        case _ =>
      }
      new MacroApplication(plain,ch,this)
    }
  }

  case class AssignRule(dict: Dictionary) extends MacroRule with InStructureRule with SymRefRuleLike {
    val name = "assign"

    override def parse(plain: PlainMacro)(implicit in: SyncedDocUnparsed, state: LaTeXParserState): TeXTokenLike = safely[TeXTokenLike](plain) {
      var children: List[TeXTokenLike] = Nil

      val (domtk, ch) = readArg
      children = ch
      val target = getTarget(domtk)

      val inmath = state.inmath
      val (_, nch2) = try {
        state.inmath = true
        readArg
      } finally {
        state.inmath = inmath
      }
      children = children ::: nch2

      addToField(target, isassigned = true)
      new MacroApplication(plain, children, this) with SymRefLike {
        override val syminfo: SymdeclInfo = target.syminfo
        override val alternatives: (TeXTokenLike, List[(GlobalName, String)]) = (plain, Nil)
      }
    }
  }
  case class RenameDeclRule(dict: Dictionary) extends MacroRule with InStructureRule with SymRefRuleLike {
    val name = "renamedecl"

    override def parse(plain: PlainMacro)(implicit in: SyncedDocUnparsed, state: LaTeXParserState): TeXTokenLike = safely[TeXTokenLike](plain) {
      var children: List[TeXTokenLike] = Nil
      val (maybename,ch1) = readOptArg
      children = ch1
      var name : Option[String] = None
      maybename.foreach { l =>
        val s = l.mkString.flatMap(c => if (c.isWhitespace) "" else c.toString)
        s match {
          case s if s.trim.startsWith("name=") =>
            name = Some(s.drop(5))
          case _ =>
            throw LaTeXParseError("Unknown key " + s.trim)
        }
      }

      val (domtk,ch2) = readSafeArg("\\renamedecl")
      children = children ::: ch2
      val target = getTarget(domtk)

      val (macronamestr,ch3) = readSafeArg("\\renamedecl")
      children = children ::: ch3
      val macroname = macronamestr match {
        case g:Group =>
          g.content match {
            case List(pt : PlainText) => pt.str
            case _ => throw LaTeXParseError("Macroname expected")
          }
        case _ => throw LaTeXParseError("Macroname expected")
      }
      val realname = name match {
        case Some(n) => LocalName.parse(n)
        case None =>
          getName / target.syminfo.path.name
      }
      addToField(target,Some(realname),macroname)
      new MacroApplication(plain,children,this) with SymRefLike {
        override val syminfo: SymdeclInfo = target.syminfo
        override val alternatives: (TeXTokenLike, List[(GlobalName, String)]) = (plain,Nil)
      }
    }
  }

  case class NotationRule(dict:Dictionary) extends MacroRule with NotationRuleLike with SymRefRuleLike {
    val name = "notation"

    override def parse(plain: PlainMacro)(implicit in: SyncedDocUnparsed, state: LaTeXParserState): TeXTokenLike = safely[TeXTokenLike](plain) {
      var children: List[TeXTokenLike] = Nil
      val setdefault = readChar('*') match {
        case (b, ls) =>
          children = ls.reverse
          b
      }
      val (info, _, _, ch) = parseSym
      children = children ::: ch
      val (opt, nch) = readOptArg
      children = children ::: nch
      val (notinfo, err2, nopt, nch2) = optsAndNotation(info, opt)
      children = children ::: nch2
      val ret = if (setdefault)
        new NotationApp(plain, children, this, notinfo) with SetNotationLike
      else getNotations(info) match {
        case Nil => new NotationApp(plain, children, this, notinfo) with SetNotationLike
        case _ => NotationApp(plain, children, this, notinfo)
      }
      if (nopt.nonEmpty) ret.addError("Unknown parameters for \\notations: " + nopt.mkString(", "))
      //if (err != "") ret.addError(err, lvl = Level.Warning)
      if (err2 != "") ret.addError(err2)
      dict.getModuleOpt match {
        case Some(mod) =>
          mod.exportrules ::= ret
          mod.rules ::= ret
        case None => state.addRule(ret)
      }
      ret
    }
  }
  class SymDefRule(val dict:Dictionary) extends MacroRule with SymDeclRuleLike with NotationRuleLike {
    val name = "symdef"
    override def parse(plain: PlainMacro)(implicit in: SyncedDocUnparsed, state: LaTeXParserState): TeXTokenLike = safely[TeXTokenLike](plain){
      var children : List[TeXTokenLike] = Nil
      val (sdinfo,ch,err,nopt) = parseNameAndOpts(dict.getFile)
      children = ch
      safely[TeXTokenLike]{
        val n = SymdeclApp(plain,children,this,sdinfo)
        if (err != "") n.addError(err)
        n
      } {
        val (ninfo,err2,nopt2,nch) = optsAndNotation(sdinfo,nopt)
        children = children ::: nch
        val ret = SymdefApp(plain, children, this, sdinfo, ninfo)
        if (err != "") ret.addError(err)
        if (err2 != "") ret.addError(err2)
        if (nopt2.nonEmpty) ret.addError("Unknown parameters for \\symdef: " + nopt.mkString(", "))
        val mod = dict.getModule
        mod.exportrules ::= ret
        mod.rules ::= ret
        ret
      }
    }
  }

  case class SetMetaTheoryRule(dict:Dictionary) extends MacroRule {
    val name = "setmetatheory"

    override def parse(plain: PlainMacro)(implicit in: SyncedDocUnparsed, state: LaTeXParserState): TeXTokenLike = {
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
      dict.all_modules.getOrElse(mp,{
        dict.requireModule(mp,a,path)
      })
      state.addRule(MetaTheoryRule(mp))
      new MacroApplication(plain,children,this) with HasAnnotations {
        override def doAnnotations(in: sTeXDocument): Unit = {
          val a = in.Annotations.add(this, startoffset, endoffset - startoffset, SymbolKind.Module, mp.toString, true)
          a.addCodeLens(mp.toString, "", Nil, startoffset, endoffset)
          a.setSemanticHighlightingClass(0)
        }
      }
    }
  }
  case class MMTDef(_dict:Dictionary) extends SymDefRule(_dict) {
    override val name = "mmtdef"
  }

  case class TextSymDeclRule(override val dict: Dictionary) extends SymDefRule(dict) {
    override val name = "textsymdecl"

    override def parse(plain: PlainMacro)(implicit in: SyncedDocUnparsed, state: LaTeXParserState): TeXTokenLike = safely[TeXTokenLike](plain) {
      var children: List[TeXTokenLike] = Nil
      val (sdinfo, ch, err, nopt) = parseNameAndOpts(dict.getFile)
      children = ch
      safely[TeXTokenLike] {
        val n = SymdeclApp(plain, children, this, sdinfo)
        if (err != "") n.addError(err)
        n
      } {
        val (ninfo, err2, nopt2, nch) = optsAndNotation(sdinfo, nopt)
        val nsdinfo = SymdeclInfo(sdinfo.macroname + "-sym",sdinfo.path,"","",false,sdinfo.file,sdinfo.start,sdinfo.end,false)
        children = children ::: nch
        val ret = SymdefApp(plain, children, this, nsdinfo, ninfo)
        val ret2 = new SymdefApp(plain,children,this,sdinfo,ninfo) {
          override def parseInner(plain: PlainMacro, requireNotation: Boolean)(cons: (PlainMacro, List[TeXTokenLike], Option[NotationInfo]) => TeXTokenLike)(implicit in: SyncedDocUnparsed, state: LaTeXParserState): TeXTokenLike = {
            val notations = getNotations(syminfo)
            val notation = notations.headOption match {
              case None =>
                val ret = cons(plain, children, None)
                if (requireNotation) ret.addError("No notation found for " + syminfo.path.toString)
                return ret
              case Some(not) =>
                not
            }
            cons(plain, children, Some(notation.notinfo))
          }
        }
        if (err != "") ret.addError(err)
        if (err2 != "") ret.addError(err2)
        if (nopt2.nonEmpty) ret.addError("Unknown parameters for \\symdef: " + nopt.mkString(", "))
        val mod = dict.getModule
        mod.exportrules ::= ret
        mod.exportrules ::= ret2
        mod.rules ::= ret
        mod.rules ::= ret2
        ret
      }
    }
  }
  case class VarDefRule(dict: Dictionary) extends MacroRule with SymDeclRuleLike with NotationRuleLike {
    val name = "vardef"

    override def parse(plain: PlainMacro)(implicit in: SyncedDocUnparsed, state: LaTeXParserState): TeXTokenLike = safely[TeXTokenLike](plain) {
      var children: List[TeXTokenLike] = Nil
      val (nametk, nch) = readArg
      children = nch
      val maybename = nametk match {
        case g: Group =>
          g.content match {
            case List(pt: PlainText) => pt.str
            case _ => throw LaTeXParseError("Could not determine name for \\symdef", lvl = Level.Warning)
          }
        case _ => throw LaTeXParseError("Name for \\symdef expected")
      }
      val inmath = state.inmath
      val (opt, nch2) = try {
        state.inmath = true
        readOptArg
      } finally {
        state.inmath = inmath
      }
      children = children ::: nch2
      var args = ""
      var error = ""
      var name = maybename
      var defd = false
      var assoctype = ""
      var ls = opt
      ls.foreach { l =>
        val s = l.mkString.flatMap(c => if (c.isWhitespace) "" else c.toString)
        s match {
          case s if s.startsWith("args=") =>
            ls = ls.filterNot(_ == l)
            val argstr = s.drop(5)
            if (argstr.forall(_.isDigit)) {
              val arity = argstr.toInt
              args = (0 until arity).map(_ => "i").mkString
            } else {
              argstr.filterNot(List('i', 'a', 'b', 'B').contains) match {
                case "" =>
                  args = argstr
                case na =>
                  error = "Characters not allowed in args: " + na.mkString(", ")
              }
            }
          case s if s.startsWith("assoc=") =>
            ls = ls.filterNot(_ == l)
            assoctype = s.drop(6)
          case s if s.startsWith("name=") =>
            ls = ls.filterNot(_ == l)
            val rest = s.drop(5)
            name = if (rest.startsWith("{") && rest.endsWith("}")) rest.init.tail else rest
          case s if s.startsWith("reorder=") =>
            ls = ls.filterNot(_ == l)
          // TODO?
          case s if s.startsWith("type=") =>
            ls = ls.filterNot(_ == l)
          // TODO?
          case s if s.startsWith("def=") =>
            ls = ls.filterNot(_ == l)
            defd = true
          // TODO?
          case s if s.startsWith("op=") || s.startsWith("prec=") || !s.contains('=') =>
          case _ =>
            print("")
        }
      }
      val (_,nch3) = readArg
      children = children ::: nch3
      val ret = VardefApp(plain,children,this,maybename,name,args,assoctype,dict.getFile,in.offset,defd)
      state.addRule(ret)
      ret
    }
  }


  case class InstanceRule(dict:Dictionary) extends MacroRule with SymDeclRuleLike with NotationRuleLike {
    val name = "instantiate"

    override def parse(plain: PlainMacro)(implicit in: SyncedDocUnparsed, state: LaTeXParserState): TeXTokenLike = safely[TeXTokenLike](plain) {
      var children: List[TeXTokenLike] = Nil
      val (nametk, nch) = readArg
      children = nch
      val maybename = nametk match {
        case g: Group =>
          g.content match {
            case List(pt: PlainText) => pt.str
            case _ => throw LaTeXParseError("Could not determine name for \\instantiate", lvl = Level.Warning)
          }
        case _ => throw LaTeXParseError("Name for \\instantiate expected")
      }
      val (opt, nch2) = readOptArg
      children = children ::: nch2
      var name = maybename
      var ls = opt
      ls.foreach { l =>
        val s = l.mkString.flatMap(c => if (c.isWhitespace) "" else c.toString)
        s match {
          case s if s.startsWith("name=") =>
            ls = ls.filterNot(_ == l)
            val rest = s.drop(5)
            name = if (rest.startsWith("{") && rest.endsWith("}")) rest.init.tail else rest
          case _ =>
            print("")
        }
      }
      val (structArg, nch3) = readArg
      children = children ::: nch3
      val struct = structArg match {
        case g: Group =>
          g.content match {
            case List(pt: PlainText) => pt.str
            case _ => throw LaTeXParseError("Could not determine structure name for \\instantiate", lvl = Level.Warning)
          }
        case _ => throw LaTeXParseError("mathstructure name for \\instantiate expected")
      }
      val module = state.macrorules.collectFirst {
        case m: MathStructureMacro if m.mpi.name.toString == struct || m.macroname == struct =>
          dict.all_modules.get(m.mpi)
      }.flatten match {
        case Some(mod) => mod
        case _ =>
          throw LaTeXParseError("No mathstructure" + struct + " found")
      }

      val (_, nch4) = readArg
      children = children ::: nch4

      val inmath = state.inmath
      state.inmath = true
      val (_, nch5) = try {readOptArg} finally {
        state.inmath = inmath
      }
      children = children ::: nch5

      val path =dict.getGlobalName(name)
      val ret = InstanceApp(plain, children, this, maybename, path, module, dict.getFile, in.offset)
      val top = dict.getModule
      top.rules ::= ret
      top.exportrules ::= ret
      ret
    }
  }

  case class VarInstanceRule(dict: Dictionary) extends MacroRule with SymDeclRuleLike with NotationRuleLike {
    val name = "varinstantiate"
    override def parse(plain: PlainMacro)(implicit in: SyncedDocUnparsed, state: LaTeXParserState): TeXTokenLike = safely[TeXTokenLike](plain) {
      var children: List[TeXTokenLike] = Nil
      val (nametk, nch) = readArg
      children = nch
      val maybename = nametk match {
        case g: Group =>
          g.content match {
            case List(pt: PlainText) => pt.str
            case _ => throw LaTeXParseError("Could not determine name for \\varinstantiate", lvl = Level.Warning)
          }
        case _ => throw LaTeXParseError("Name for \\varinstantiate expected")
      }
      val inmath = state.inmath
      val (opt, nch2) = try {
        state.inmath = true
        readOptArg
      } finally {
        state.inmath = inmath
      }
      children = children ::: nch2
      var name = maybename
      var ls = opt
      ls.foreach { l =>
        val s = l.mkString.flatMap(c => if (c.isWhitespace) "" else c.toString)
        s match {
          case s if s.startsWith("name=") =>
            ls = ls.filterNot(_ == l)
            val rest = s.drop(5)
            name = if (rest.startsWith("{") && rest.endsWith("}")) rest.init.tail else rest
          case _ =>
            print("")
        }
      }
      val (structArg, nch3) = readArg
      children = children ::: nch3
      val struct = structArg match {
        case g: Group =>
          g.content match {
            case List(pt: PlainText) => pt.str
            case _ => throw LaTeXParseError("Could not determine structure name for \\varinstantiate", lvl = Level.Warning)
          }
        case _ => throw LaTeXParseError("mathstructure name for \\varinstantiate expected")
      }
      val module = state.macrorules.collectFirst {
        case m:MathStructureMacro if m.mpi.name.toString == struct || m.macroname == struct =>
          dict.all_modules.get(m.mpi)
      }.flatten match {
        case Some(mod) => mod
        case _ =>
          throw LaTeXParseError("No mathstructure" + struct + " found")
      }

      val (notation, nch4) = readArg
      children = children ::: nch4

      val inmath2 = state.inmath
      state.inmath = true
      val (assignments,nch5) = try {readOptArg} finally {state.inmath = inmath2}
      children = children ::: nch5

      val ret = VarInstanceApp(plain, children, this, maybename, name, module, dict.getFile, in.offset)
      state.addRule(ret)
      ret
    }
  }

  case class ImportModuleRule(dict : Dictionary) extends MacroRule with ImportModuleRuleLike {
    val name = "importmodule"
    override val isusemodule: Boolean = false
  }
  case class UseModuleRule(dict : Dictionary) extends MacroRule with ImportModuleRuleLike with NonFormalRule {
    val name = "usemodule"
    override val isusemodule: Boolean = true
  }

  case class InlineAssertionRule(dict:Dictionary) extends InlineStatementRule("inlineass",dict)
  case class AssertionRule(dict:Dictionary) extends StatementRule("sassertion",dict)

  case class CopymoduleRule(dict:Dictionary) extends EnvironmentRule("copymodule") with StructureLikeRule {

    override def parse(begin: MacroApplication)(implicit in: SyncedDocUnparsed, state: LaTeXParserState): MacroApplication = {
      var children: List[TeXTokenLike] = Nil
      val (ns,ch0) = readOptArg
      children = ch0
      val (n, ch) = readArg
      children = children ::: ch
      val (n2, ch2) = readArg
      children = children ::: ch2
      setupBody(ns,n,Some(n2))((mp,dom) =>
        new StructureModuleBegin(begin.plain,mp,dom,ch,this)
      )
    }
  }

  case class InterpretmoduleRule(dict: Dictionary) extends EnvironmentRule("interpretmodule") with StructureLikeRule {
    override def finalizeStructure(mod:StructureModuleBegin,env:StructureModule)(implicit state: LaTeXParserState): Unit = {
      val missings = mod.fields.collect {
        case (sm,(_,_,false)) if !sm.syminfo.defined => sm
      }.toList
      missings match {
        case Nil =>
        case _ =>
          env.addError("Missing assignments: " + missings.map(_.syminfo.path.toString).mkString(", "))
      }
    }
    override def parse(begin: MacroApplication)(implicit in: SyncedDocUnparsed, state: LaTeXParserState): MacroApplication = {
      var children: List[TeXTokenLike] = Nil
      val (ns, ch0) = readOptArg
      children = ch0
      val (n, ch) = readArg
      children = children ::: ch
      val (n2, ch2) = readArg
      children = children ::: ch2
      setupBody(ns, n, Some(n2))((mp, dom) =>
        new StructureModuleBegin(begin.plain, mp, dom, ch, this)
      )
    }
  }

  case class RealizationRule(dict: Dictionary) extends EnvironmentRule("realization") with StructureLikeRule {
    override def finalizeStructure(mod: StructureModuleBegin, env: StructureModule)(implicit state: LaTeXParserState): Unit = {
      val missings = mod.fields.collect {
        case (sm, (_, _, false)) if !sm.syminfo.defined => sm
      }.toList
      missings match {
        case Nil =>
        case _ =>
          env.addError("Missing assignments: " + missings.map(_.syminfo.path.toString).mkString(", "))
      }
        dict.addimport(ImportModuleApp(mod.plain, mod.dom.path, Nil, ImportModuleRule(dict), "", "", false, None))
    }

    override def parse(begin: MacroApplication)(implicit in: SyncedDocUnparsed, state: LaTeXParserState): MacroApplication = {
      var children: List[TeXTokenLike] = Nil
      val (ns, ch0) = readOptArg
      children = ch0
      val (n, ch) = readArg
      children = children ::: ch
      setupBody(ns, n, None)((mp, dom) =>
        new StructureModuleBegin(begin.plain, mp, dom, ch, this)
      )
    }
  }
  class MathStructureRule(dict: Dictionary) extends EnvironmentRule("mathstructure") {
    override def finalize(env: Environment)(implicit state: LaTeXParserState): Environment = env.begin match {
      case ms : MathStructureMacro =>
        dict.closeModule
        MathStructure(ms,env.end,env.children,this)
      case _ => env
    }
    override def parse(begin: MacroApplication)(implicit in: SyncedDocUnparsed, state: LaTeXParserState): MacroApplication = {
      var children : List[TeXTokenLike] = Nil
      val (n,ch) = readArg
      children = ch
      val maybename = n match {
        case gr:Group =>
          gr.content match {
            case List(t:PlainText) => t.str
            case _ =>
              throw LaTeXParseError("Name expected")
          }
        case _ =>
          throw LaTeXParseError("Name expected")
      }
      val (optargs,ch2) = readOptArg
      children = children ::: ch2
      var name = maybename
      optargs.foreach{l =>
        val s = l.mkString.flatMap(c => if (c.isWhitespace) "" else c.toString)
        s match {
          case s if s.trim.startsWith("name=") =>
            name = s.drop(5).trim
          case _ =>
            throw LaTeXParseError("Unknown key:" + s.drop(5).trim)
        }
      }
      val mp = dict.getMPath(name + "-structure")
      val sympath = dict.getGlobalName(name)
      val macr = MathStructureMacro(begin.plain,mp,maybename,sympath,begin.children ::: children,this,dict.getFile)
      dict.getModuleOpt match {
        case Some(mod) =>
          mod.exportrules ::= macr
          mod.rules ::= macr
        case _ =>
      }
      //if (deprecation != "") macr.addError("Deprecated: Use " + deprecation + " instead",lvl=Level.Warning)
      val ret = safely(macr){dict.openModule(macr)}

      ret
    }
  }

  case class ExtStructureRule(dict:Dictionary) extends MathStructureRule(dict) {
    override val name: String = "extstructure"

    override def parse(begin: MacroApplication)(implicit in: SyncedDocUnparsed, state: LaTeXParserState): MacroApplication = {
      super.parse(begin) match {
        case m : MathStructureMacro =>
          val (args, children) = readArg
          val ret = MathStructureMacro(m.pm,m.mpi,m.macroname,m.symbolpath,m.ch ::: children,this,m.file)
          val exts = args match {
            case g: Group =>
              g.content.filterNot(_.isInstanceOf[Comment]) match {
                case List(pt:PlainText) =>
                  pt.str.split(',').map(_.trim)
                case _ =>
                  ret.addError("Expected mathstructures to extend")
                  return ret
              }
            case _ =>
              ret.addError("Expected mathstructures to extend")
              return ret
          }
          val modules = exts.flatMap { struct =>
            state.macrorules.collectFirst {
              case m: MathStructureMacro if m.mpi.name.toString == struct || m.macroname == struct =>
                dict.all_modules.get(m.mpi)
            }.flatten match {
              case Some(mod) => Some(mod)
              case _ =>
                ret.addError("No mathstructure" + struct + " found")
                None
            }
          }
          modules.foreach{mod =>
            dict.addimport(ImportModuleApp(begin.plain,mod.path,Nil,ImportModuleRule(dict),"","",false,None))
          }
          ret
        case o => o
      }

    }
  }

  case class ModuleRule(dict : Dictionary) extends EnvironmentRule("smodule") {
    override def parse(begin: MacroApplication)(implicit in: SyncedDocUnparsed, state: LaTeXParserState): MacroApplication = safely[MacroApplication](begin) {
      var children : List[TeXTokenLike] = Nil
      val (optargs,ch) = readOptArg
      children = ch
      val (n,ch2) = readArg
      children = children ::: ch2
      val name = n match {
        case gr:Group =>
          gr.content match {
            case List(t:PlainText) => t.str
            case _ =>
              throw LaTeXParseError("Group expected after \\begin{smodule}")
          }
        case _ =>
          throw LaTeXParseError("{name} expected after \\begin{smodule}")
      }
      var meta : Option[MPath] = Some(dict.defaultMeta)
      var lang = dict.getLanguage
      var sig = ""
      var title:String = ""
      var deprecation:String = ""
      optargs.foreach { l =>
        val s = l.mkString.flatMap(c => if (c.isWhitespace) "" else c.toString)
        s match {
          case s if s.trim.startsWith("meta=") =>
            val mod = s.trim.drop(5).trim
            if (mod == "NONE") meta = None
            else {
              try {
                meta = Some(Path.parseM(mod))
              } catch {
                case t : ParseError =>
                  throw LaTeXParseError("Not a valid module path: " + mod)
              }
            }
          case s if s.trim.startsWith("title=") =>
            title = s.trim.drop(6).trim
          case s if s.trim.startsWith("sig=") =>
            sig = s.trim.drop(4).trim
          case s if s.trim.startsWith("lang=") =>
            lang = s.trim.drop(5).trim
          case s if s.trim.startsWith("creators=") =>
          case s if s.trim.startsWith("contributors=") =>
          case s if s.trim.startsWith("deprecate=") =>
          case s if s.trim.startsWith("ns=") => // TODO
          case s if s.trim.startsWith("srccite=") =>
            deprecation = s.trim.drop(10).trim
          case s =>
            throw LaTeXParseError("Unknow key " + s.trim)
        }
      }
      val mp = dict.getMPath(name)
      val macr = stex.TeXModuleMacro(begin.plain,mp,begin.children ::: children,this,meta,sig,lang,title,deprecation)
      //if (deprecation != "") macr.addError("Deprecated: Use " + deprecation + " instead",lvl=Level.Warning)
      safely(macr){dict.openModule(macr)}
    }

    override def finalize(env: Environment)(implicit state:LaTeXParserState): Environment = env.begin match {
      case tmm: TeXModuleMacro =>
        dict.closeModule
        stex.TeXModule(tmm,env.end,env.children,this)
      case _ => env
    }
  }

  case class UseStructureRule(dict: Dictionary) extends EnvironmentRule("usestructure") {
    override def finalize(env: Environment)(implicit state: LaTeXParserState): Environment = env
    override def parse(begin: MacroApplication)(implicit in: SyncedDocUnparsed, state: LaTeXParserState): MacroApplication = {
      val (structArg, children) = readArg
      val struct = structArg match {
        case g: Group =>
          g.content match {
            case List(pt: PlainText) => pt.str
            case _ => throw LaTeXParseError("Could not determine structure name forusestructure", lvl = Level.Warning)
          }
        case _ => throw LaTeXParseError("mathstructure name for usestructure expected")
      }
      val module = state.macrorules.collectFirst {
        case m: MathStructureMacro if m.mpi.name.toString == struct || m.macroname == struct =>
          dict.all_modules.get(m.mpi)
      }.flatten match {
        case Some(mod) => mod
        case _ =>
          throw LaTeXParseError("No mathstructure" + struct + " found")
      }
      module.getRules("").foreach(state.addRule(_))
      new MacroApplication(begin.plain,begin.children ::: children,this)
    }
  }

  case class ProblemRule(dict : Dictionary) extends EnvironmentRule("sproblem") {
    override def parse(begin: MacroApplication)(implicit in: SyncedDocUnparsed, state: LaTeXParserState): MacroApplication = safely[MacroApplication](begin) {
      var children : List[TeXTokenLike] = Nil
      val (optargs,ch) = readOptArg
      children = ch
      var name = ""
      var meta : Option[MPath] = Some(dict.defaultMeta)
      var lang = dict.getLanguage
      var sig = ""
      var title:String = ""
      var deprecation:String = ""
      optargs.foreach { l =>
        val s = l.mkString.flatMap(c => if (c.isWhitespace) "" else c.toString)
        s match {
          case s if s.trim.startsWith("meta=") =>
            val mod = s.trim.drop(5).trim
            if (mod == "NONE") meta = None
            else {
              meta = Some(try{Path.parseM(mod)} catch {
                case t =>
                  throw LaTeXParseError("Module path expected: " + mod)
              })
            }
          case s if s.trim.startsWith("title=") =>
            title = s.trim.drop(6).trim
          case s if s.trim.startsWith("sig=") =>
            sig = s.trim.drop(4).trim
          case s if s.trim.startsWith("lang=") =>
            lang = s.trim.drop(5).trim
          case s if s.trim.startsWith("creators=") =>
          case s if s.trim.startsWith("pts=") =>
          case s if s.trim.startsWith("min=") =>
          case s if s.trim.startsWith("id=") =>
          case s if s.trim.startsWith("name=") =>
            name = s.drop(5)
          case s if s.trim.startsWith("contributors=") =>
          case s if s.trim.startsWith("deprecate=") =>
          case s if s.trim.startsWith("srccite=") =>
            deprecation = s.trim.drop(10).trim
          case s if s.startsWith("uses=") || s.startsWith("imports=") =>
            // TODO
          case s =>
            throw LaTeXParseError("Unknow key " + s.trim)
        }
      }
      if (name == "") {
        val segs = dict.getFile.split('/').last.split('.').init.toList
        if (STeX.all_languages.contains(segs.last)) name = segs.init.mkString(".")
        else name = segs.mkString(".")
      }
      val mp = dict.getMPath(name)
      val macr = stex.TeXModuleMacro(begin.plain,mp,begin.children ::: children,this,meta,sig,lang,title,deprecation)
      //if (deprecation != "") macr.addError("Deprecated: Use " + deprecation + " instead",lvl=Level.Warning)
      safely(macr){dict.openModule(macr)}
    }

    override def finalize(env: Environment)(implicit state:LaTeXParserState): Environment = env.begin match {
      case tmm: TeXModuleMacro =>
        dict.closeModule
        stex.TeXModule(tmm,env.end,env.children,this)
      case _ => env
    }
  }

}

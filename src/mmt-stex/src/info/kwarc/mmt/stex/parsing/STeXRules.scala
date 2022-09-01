package info.kwarc.mmt.stex.parsing

import info.kwarc.mmt.api.{DPath, GlobalName, LNStep, Level, LocalName, MPath, NamespaceMap, ParseError, Path, SimpleStep, SourceError}
import info.kwarc.mmt.api.archives.{Archive, source}
import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.parser.{SourcePosition, SourceRef, SourceRegion}
import info.kwarc.mmt.api.utils.{File, URI}
import info.kwarc.mmt.lsp.{LSPDocument, SyncedDocUnparsed, SyncedDocument}
import info.kwarc.mmt.stex.{STeX, STeXError}
import info.kwarc.mmt.stex.lsp.sTeXDocument
import org.eclipse.lsp4j.SymbolKind

import java.util.concurrent.ForkJoinPool
import scala.collection.mutable
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration.Duration

class MutList(var ls : List[DictionaryModule] = Nil)
class DictionaryModule(val macr:TeXModuleLike) extends RuleContainer {
  val path = macr.mp
  var archive : Option[Archive] = None
  var file : Option[File] = None
  var lang:String = ""
  var imports : List[(DictionaryModule,Boolean)] = Nil
  var exportrules : List[TeXRule] = Nil
  def getRules(lang:String,dones : MutList = new MutList()) : List[TeXRule] = if (dones.ls.contains(this)) Nil else {
    dones.ls ::= this
    val r = exportrules ::: imports.filter(p => p._2 && !dones.ls.contains(p._1)).flatMap(_._1.getRules(lang,dones))
    langs.get(lang) match {
      case Some(m) => m.exportrules ::: r
      case _ => r
    }
  }
  val langs = mutable.HashMap.empty[String,DictionaryModule]

}

class Dictionary(val controller:Controller,parser:STeXSuperficialParser) {

  private var filetracker:List[File] = Nil
  def previouslyread(f : File) = filetracker.contains(f)

  implicit val ec : ExecutionContext = ExecutionContext.fromExecutorService(new ForkJoinPool(1000))

  def getMPath(name:String):MPath = {
    current_modules.headOption match {
      case None =>
        current_namespace match {
          case Some(d) if d.last == name =>
            d.^ ? name
          case Some(d) =>
            d ? LocalName.parse(name)
          case _ => ???
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
      case (_,Nil) if current_namespace.exists(dp => all_modules.isDefinedAt(dp ? name)) =>
        current_namespace.get ? name
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
    print("")
    ???
  })
  def getModuleOpt = current_modules.headOption

  def inFile[A](file : File,a:Option[Archive] = None)(f: => A) : A = {
    filetracker ::= file
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
            new DictionaryModule(macr)
          case Some(m) =>
            val n = new DictionaryModule(macr)
            n.imports ::= (m,true)
            m.langs(language) = n
            n.rules = m.getRules(language)
            n
        }
      })
    } else new DictionaryModule(macr)
    current_file.foreach(f => mod.file = Some(f))
    current_archive.foreach(a => mod.archive = Some(a))
    current_modules ::= mod
    state.rules ::= mod
    mod.rules = parser.moduleRules ::: mod.rules
    macr match {
      case tmm: TeXModuleMacro =>
        tmm.meta match {
          case Some(m) =>
            all_modules.get(m) match {
              case Some(mt) =>
                mod.imports ::= (mt,false)
                mod.rules = mod.rules ::: mt.rules
              case _ =>
            }
          case _ =>
        }
      case _ =>
    }
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

  def addimport(ima: ImportModuleApp)(implicit state: LaTeXParserState): ImportModuleApp = {
    val mod = all_modules.get(ima.mp) match {
      case Some(mod) => mod
      case _ =>
        val archive = if (ima.archivestring == "") current_archive else controller.backend.getArchive(ima.archivestring)
        val (rpath, name) = ima.path.split('?').toList match {
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
            throw LaTeXParseError("No candidate file for module " + ima.mp + " found")
          }
        }
        //if (!filetracker.contains(file)) {
          Await.result(Future {
            try {
              parser.applyFormally(file, archive)
            } catch {
              case e: Throwable =>
                e.printStackTrace()
                print("")
            }
          }, Duration.Inf)
       // }

        all_modules.getOrElse(ima.mp, {
          ima.addError("No module " + ima.mp.toString + " found", Some("In file " + file))
          return ima
        })
    }
    if (ima.isusemodule) {
      state.rules.headOption.foreach(r => r.rules = mod.getRules(getLanguage) ::: r.rules)
    } else current_modules.headOption match {
      case Some(m) =>
        m.imports ::= (mod, !ima.isusemodule)
        m.rules = mod.getRules(getLanguage) ::: m.rules
      case None =>
        print("")
        ???
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

trait NonFormalRule extends TeXRule
trait HasAnnotations extends TeXTokenLike {
  def doAnnotations(in:sTeXDocument) : Unit
}
abstract class TeXModuleLike(pm:PlainMacro,val mp:MPath,ch:List[TeXTokenLike],rl:EnvironmentRule) extends MacroApplication(pm,ch,rl) {
  val sig : String
}
case class MathStructureMacro(
                            pm:PlainMacro,
                            mpi:MPath,
                            macroname:String,
                            symbolpath:GlobalName,
                            ch:List[TeXTokenLike],
                            rl : STeXRules.MathStructureRule,
                            file:String
                            ) extends TeXModuleLike(pm,mpi,ch,rl) with TeXRule with SemanticMacro {
  val sig = ""
  override lazy val name = "mathstructure " + mpi.toString
  val syminfo = SymdeclInfo(macroname,symbolpath,"","",false,this.file,this.startoffset,this.endoffset)
}
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
case class TeXModuleMacro(
                           pm:PlainMacro,
                           mpi:MPath,
                           ch:List[TeXTokenLike],
                           rl : EnvironmentRule,
                           meta:Option[MPath],
                           sig:String = "",
                           lang:String="",
                           title:String="",deprecation:String = "") extends TeXModuleLike(pm,mpi,ch,rl)//MacroApplication(pm,ch,rl)
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
    mace.addCodeLens(bg.mp.toString,"",Nil,end.startoffset,end.endoffset)
  }
  var dict_module: Option[DictionaryModule] = None
}

case class ImportModuleApp(
                            pm:PlainMacro,
                            mp:MPath,
                            ch:List[TeXTokenLike],
                            rl:STeXRules.ImportModuleRuleLike,
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
trait SymRefRuleLike extends MacroRule {
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
          if ((np.mkString("/") + td.path.module.name + "?" + td.path.name).length <= td.string.length) i += 1 else idone = true
        }
        td.string = td.path.doc.uri.path.takeRight(i).mkString("/") + "?" + td.path.name
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
    val rn = resolveName(namestr)
    rn match {
      case (None,_) =>
        throw LaTeXParseError("Could not find symbol named " + namestr, lvl = Level.Warning)
      case (Some(r),ls) =>
        (r,nametk,doAlternatives(r.path :: ls),children)
    }
  }
  def resolveName(namestr:String)(implicit state: LaTeXParserState) = {
    val rules = state.macrorules
    namestr.split('?') match {
      case Array(s) => rules.collectFirst {
        case m: SemanticMacro if m.name == s => m.syminfo
      } match {
        case Some(s) => (Some(s), Nil)
        case _ =>
          val candidates =rules.collect {
            case m: SemanticMacro if m.syminfo.path.name.toString == s => m.syminfo
          }.distinct
          candidates match {
            case List(a) => (Some(a), Nil)
            case Nil => (None, Nil)
            case h :: tail => (Some(h), tail.map(_.path))
          }
      }
      case Array(m,n) =>
        rules.collect {
          case sm: SemanticMacro if sm.syminfo.path.module.toString.endsWith(m) &&
            sm.syminfo.path.name.toString == n => sm.syminfo
        }.distinct match {
          case List(a) => (Some(a), Nil)
          case Nil => (None, Nil)
          case h :: tail => (Some(h), tail.map(_.path))
        }
      case _ =>
        throw LaTeXParseError("Too many '?' in symbol identifier",lvl = Level.Warning)
    }
  }
}
trait SemanticMacro extends MacroRule with SymRefRuleLike {
  val syminfo:SymdeclInfo
  lazy val name = syminfo.macroname

  override def parse(plain: PlainMacro)(implicit in: SyncedDocUnparsed, state: LaTeXParserState): TeXTokenLike = safely[TeXTokenLike](plain){
    var children : List[TeXTokenLike] = Nil
    val (custom,op) = if (state.inmath) {
      val o = readChar('!') match {
        case (b,ch) =>
          children = ch.reverse
          b
      }
      val c = readChar('*') match {
        case (b,ch) =>
          children = children ::: ch.reverse
          b
      }
      (c,o)
    } else {
      (true,readChar('!') match {
        case (b,ch) =>
          children = ch.reverse
          b
      })
    }
    if (custom) {
      val (a,ch) = readSafeArg("\\" + plain.name)
      children = children ::: ch
      SemanticMacroApp(plain,this,children,None)
    } else {
      val notations = getNotations(syminfo)
      val notation = readOptArg match {
        case (Nil,ch) =>
          children = children ::: ch
          notations.headOption match {
            case None =>
              val ret = SemanticMacroApp(plain,this,children,None)
              ret.addError("No notation found for " + syminfo.path.toString)
              return ret
            case Some(not) =>
              not
          }
        case (ls,ch) =>
          children = children ::: ch
          val notid = ls.map(_.mkString).mkString
          notations.find(_.notinfo.id == notid).getOrElse {
            val ret = SemanticMacroApp(plain,this,children,None)
            ret.addError("No notation found for " + syminfo.path.toString)
            return ret
          }
      }
      if (op) {
        val ret = SemanticMacroApp(plain,this,children,Some(notation.notinfo))
        notation.notinfo.opnotation match {
          case None =>
            ret.addError("Notation " + notation.notinfo.id + " for " + notation.notinfo.syminfo.path.toString + " has no operator notation!")
          case _ =>
        }
        ret
      } else safely{
        SemanticMacroApp(plain,this,children,Some(notation.notinfo))
      }{
        syminfo.args.foreach{_ =>
          val (_,nch) = readSafeArg("\\" + plain.name)
          children = children ::: nch
        }
        SemanticMacroApp(plain,this,children,Some(notation.notinfo))
      }
    }
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

case class SymdeclInfo(macroname:String,path:GlobalName,args:String,assoctype:String,local:Boolean,file:String,start:Int,end:Int) {
  def arity = args.length
}
case class NotationInfo(syminfo:SymdeclInfo,prec:List[Int],id:String,notation:List[TeXTokenLike],opnotation:Option[List[TeXTokenLike]])

case class SymdeclApp(pl:PlainMacro,ch:List[TeXTokenLike],rl:STeXRules.SymDeclRuleLike,
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

object STeXRules {

  val defaultMeta = Path.parseM("http://mathhub.info/sTeX/meta?Metatheory")

  def allRules(dict:Dictionary) = List(
    ModuleRule(dict),UseModuleRule(dict),NotationRule(dict),mmtrule,
    SymrefRule(dict),SymnameRule(dict),CapSymnameRule(dict),
    DefiniendumRule(dict),DefinameRule(dict),CapDefinameRule(dict),ProblemRule(dict),
    patchdefinitionrule,patchassertionrule,stexinline
  )
  def moduleRules(dict:Dictionary) = List(
    ImportModuleRule(dict),SymDefRule(dict),SymDeclRule(dict),MathStructureRule(dict)
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

  trait SymDeclRuleLike extends MacroRule {
    val dict:Dictionary
    def parseNameAndOpts(file:String)(implicit in: SyncedDocUnparsed, state: LaTeXParserState) = {
      var children : List[TeXTokenLike] = Nil
      val makemacro = readChar('*') match {
        case (b,ls) =>
          children = ls.reverse
          !b
      }
      val (nametk,nch) = readArg
      children = children ::: nch
      val maybename = nametk match {
        case g : Group =>
          g.content match {
            case List(pt: PlainText) => pt.str
            case _ => throw LaTeXParseError("Could not determine name for \\symdef",lvl = Level.Warning)
          }
        case _ => throw LaTeXParseError("Name for \\symdef expected")
      }
      val inmath = state.inmath
      val (opt,nch2) = try {
        state.inmath = true
        readOptArg
      } finally {
        state.inmath = inmath
      }
      children = children ::: nch2
      val (name,args,assoctype,local,err,nopt) = parseSymOpts(opt)
      val gn = dict.getGlobalName(if (name == "") maybename else name)
      (SymdeclInfo(if (makemacro) maybename else gn.toString,gn,args,assoctype,local,file,children.head.startoffset,children.last.endoffset),children,err,nopt)
    }

    def parseSymOpts(ls: List[List[TeXTokenLike]]) = {
      var args = ""
      var ret = ls
      var error = ""
      var name = ""
      var assoctype = ""
      var local = false
      ls.foreach{l =>
        val s = l.mkString.flatMap(c => if (c.isWhitespace) "" else c.toString)
        s match {
        case s if s.startsWith("args=") =>
          ret = ret.filterNot(_ == l)
          val argstr = s.drop(5)
          if (argstr.forall(_.isDigit)) {
            val arity = argstr.toInt
            args = (0 until arity).map(_ => "i").mkString
          } else {
            argstr.filterNot(List('i','a','b','B').contains) match {
              case "" =>
                args = argstr
              case na =>
                error = "Characters not allowed in args: " + na.mkString(", ")
            }
          }
        case s if s.startsWith("assoc=") =>
          ret = ret.filterNot(_ == l)
          assoctype = s.drop(6)
        case s if s.startsWith("name=") =>
          ret = ret.filterNot(_ == l)
          val rest = s.drop(5)
          name = if (rest.startsWith("{") && rest.endsWith("}")) rest.init.tail else rest
        case s if s.startsWith("gfc=") =>
          ret = ret.filterNot(_ == l)
        case s if s == "local=true" =>
          ret = ret.filterNot(_ == l)
          local = true
        case s if s.startsWith("reorder=") =>
          ret = ret.filterNot(_ == l)
        // TODO?
        case s if s.startsWith("type=") =>
          ret = ret.filterNot(_ == l)
          // TODO?
        case s if s.startsWith("def=") =>
          ret = ret.filterNot(_ == l)
        // TODO?
        case s if s.startsWith("op=") || s.startsWith("prec=") || !s.contains('=') =>
        case _ =>
          print("")
      }}
      (name,args,assoctype,local,error,ret)
    }
  }
  trait NotationRuleLike extends MacroRule {
    val dict:Dictionary
    def optsAndNotation(syminfo:SymdeclInfo,ls: List[List[TeXTokenLike]])(implicit in: SyncedDocUnparsed, state: LaTeXParserState) = {
      val (id,prec,opnot,error,ret) = parseNotOpts(syminfo,ls)
      val (not,nch) = readArg
      var children = nch
      var notation = List(not)
      syminfo.args.filter(c => c=='a' || c=='B').foreach {a =>
        val (ret,nch) = readArg
        notation ::= ret
        children = children ::: nch
      }
      (NotationInfo(syminfo,prec,id,notation.reverse,opnot),error,ret,children)
    }
    def parseNotOpts(syminfo:SymdeclInfo,ls: List[List[TeXTokenLike]]) = {
      var ret = ls
      var id = ""
      var error = ""
      var prec : List[Int] = Nil
      var opnot : Option[List[TeXTokenLike]] = None
      ls.foreach{l =>
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
      }}
      (id,prec,opnot,error,ret)
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
  case class SymDefRule(dict:Dictionary) extends MacroRule with SymDeclRuleLike with NotationRuleLike {
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

  trait ImportModuleRuleLike extends MacroRule {
    val dict:Dictionary
    val isusemodule : Boolean

    override def parse(plain: PlainMacro)(implicit in: SyncedDocUnparsed, state: LaTeXParserState): TeXTokenLike = safely[TeXTokenLike](plain){
      var children : List[TeXTokenLike] = Nil
      val (optargs,ch) = readOptArg
      children = ch
      val (n,ch2) = readArg
      val a = optargs match {
        case List(List(pt:PlainText)) => pt.str
        case _ => ""
      }
      children = children ::: ch2
      val path = n match {
        case gr:Group =>
          gr.content match {
            case List(t:PlainText) => t.str
            case _ => {
              plain.addError("Malformed Argument")
              return plain
            }
          }
        case _ =>
          plain.addError("Missing Argument")
          return plain
      }
      val mp = dict.resolveMPath(a,path)
      val ret = ImportModuleApp(plain,mp,children,this,a,path,isusemodule,dict.getModuleOpt)
      safely(ret){dict.addimport(ret)}
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

  case class MathStructureRule(dict: Dictionary) extends EnvironmentRule("mathstructure") {
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
            case _ => ???
          }
        case _ =>
          ???
      }
      val (optargs,ch2) = readOptArg
      children = children ::: ch2
      var name = maybename
      optargs.foreach{l =>
        val s = l.mkString.flatMap(c => if (c.isWhitespace) "" else c.toString)
        s match {
          case s if s.trim.startsWith("name=") =>
            name = s.drop(5)
          case _ =>
            ???
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
      var meta : Option[MPath] = Some(defaultMeta)
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
          case s if s.trim.startsWith("srccite=") =>
            deprecation = s.trim.drop(10).trim
          case s =>
            ???
        }
      }
      val mp = dict.getMPath(name)
      val macr = TeXModuleMacro(begin.plain,mp,begin.children ::: children,this,meta,sig,lang,title,deprecation)
      //if (deprecation != "") macr.addError("Deprecated: Use " + deprecation + " instead",lvl=Level.Warning)
      safely(macr){dict.openModule(macr)}
    }

    override def finalize(env: Environment)(implicit state:LaTeXParserState): Environment = env.begin match {
      case tmm: TeXModuleMacro =>
        dict.closeModule
        TeXModule(tmm,env.end,env.children,this)
      case _ => env
    }
  }


  case class ProblemRule(dict : Dictionary) extends EnvironmentRule("sproblem") {
    override def parse(begin: MacroApplication)(implicit in: SyncedDocUnparsed, state: LaTeXParserState): MacroApplication = safely[MacroApplication](begin) {
      var children : List[TeXTokenLike] = Nil
      val (optargs,ch) = readOptArg
      children = ch
      var name = ""
      var meta : Option[MPath] = Some(defaultMeta)
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
              ???
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
            ???
        }
      }
      if (name == "") {
        val segs = dict.getFile.split('/').last.split('.').init.toList
        if (STeX.all_languages.contains(segs.last)) name = segs.init.mkString(".")
        else name = segs.mkString(".")
      }
      val mp = dict.getMPath(name)
      val macr = TeXModuleMacro(begin.plain,mp,begin.children ::: children,this,meta,sig,lang,title,deprecation)
      //if (deprecation != "") macr.addError("Deprecated: Use " + deprecation + " instead",lvl=Level.Warning)
      safely(macr){dict.openModule(macr)}
    }

    override def finalize(env: Environment)(implicit state:LaTeXParserState): Environment = env.begin match {
      case tmm: TeXModuleMacro =>
        dict.closeModule
        TeXModule(tmm,env.end,env.children,this)
      case _ => env
    }
  }

}

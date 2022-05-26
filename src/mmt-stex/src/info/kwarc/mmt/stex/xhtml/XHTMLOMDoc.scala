package info.kwarc.mmt.stex.xhtml

import info.kwarc.mmt.api.documents.{DRef, Document, MRef}
import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.metadata.{HasMetaData, MetaDatum}
import info.kwarc.mmt.api.modules.{AbstractTheory, Theory}
import info.kwarc.mmt.api.notations.NotationContainer
import info.kwarc.mmt.api.{AddError, ComplexStep, ContentPath, DPath, GeneratedDRef, GlobalName, LocalName, MPath, NamespaceMap, Path, Rule, RuleSet, SimpleStep, StructuralElement}
import info.kwarc.mmt.api.objects.{Context, OMA, OMAorAny, OMBIND, OMBINDC, OMFOREIGN, OMID, OMIDENT, OML, OMMOD, OMS, OMV, Obj, Term, VarDecl}
import info.kwarc.mmt.api.parser.SourceRef
import info.kwarc.mmt.api.symbols.{Constant, Declaration, DerivedDeclaration, Include, NestedModule, PlainInclude, RuleConstant, Structure, TermContainer}
import info.kwarc.mmt.stex.Extensions.NotationExtractor
import info.kwarc.mmt.stex.rules.{AssocBinL, AssocBinR, AssocConj, AssocPre, AssocPwconj, Getfield, ModelsOf, RecMerge, RecType, StringLiterals}
import info.kwarc.mmt.stex.xhtml.HTMLParser.{HTMLNode, HTMLText, ParsingState}
import info.kwarc.mmt.stex.{OMDocHTML, SCtx, SOMB, STeX, STeXServer, STerm}

import scala.collection.mutable

class OMDocHTML(orig : HTMLParser.HTMLNode) extends CustomHTMLNode(orig) {
  override def copy : this.type = {
    val ret = this.getClass.getConstructor(classOf[HTMLParser.HTMLNode]).newInstance(orig.copy).asInstanceOf[this.type]
    children.foreach(c => ret.add(c.copy))
    ret
  }
  def resource = attributes.getOrElse((namespace,"resource"),"")
  def property = attributes.getOrElse((namespace,"property"),"")
  val sstate = state match {
    case s : SemanticState => Some(s)
    case _ => None
  }
  def getTerm : Option[Term] = {
    this match {
      case t : HTMLTerm => return Some(t.toTerm)
      case _ => children.foreach{case c : OMDocHTML =>
        c.getTerm match {
          case Some(t) => return Some(t)
          case _ =>
        }
      case _ =>
      }
    }
    None
  }

  def findTerm : Option[Term] = findTermI(this)
  private def findTermI(h : HTMLNode): Option[Term] = {
    h match {
      case o:HTMLTerm => Some(o.toTerm)
      case _ => h.children.foreach { c =>
        findTermI(c) match {
          case Some(t) => return Some(t)
          case _ =>
        }
      }
      None
    }
  }

  def forceTerm : Term = {
    getTerm match {
      case Some(t) => t
      case None =>  forceTerm2(this)
    }
  }
  protected def forceTerm2(n : HTMLNode): Term = n.children match {
    case Nil if n.isInstanceOf[HTMLText] =>
      try {STeX.informal(n.parent.get.node) } catch {
        case e =>
          ???
      }
    case List(_: HTMLText) | Nil =>
      try {STeX.informal(n.node) } catch {
        case e =>
          ???
      }
    case _ =>
      val args = n.children.flatMap{
        case o : HTMLTerm => Some(o.toTerm)
        case _ : NotationComponent => None
        case o => Some(forceTerm2(o))
      }
      STeX.informal.op(n.label, args)
  }
}

trait HTMLGroupLike extends OMDocHTML {
  var variables = Context.empty
  def getVariables : Context = collectAncestor {
    case gl:HTMLGroupLike => gl
  }.map(_.getVariables).getOrElse(Context.empty) ++ variables
}

trait HasRuleContext extends OMDocHTML with HTMLGroupLike {
  protected var _context = Context.empty
  def context : Context = sstate.map(_ => collectAncestor{ case th:HasRuleContext => th}.map(_.context).getOrElse(Context.empty) ++ _context).getOrElse(Context.empty)
  def addToContext(ctx : Context) = _context = _context ++ ctx
}

class HTMLDocument(val path : DPath,orig : HTMLParser.HTMLNode) extends OMDocHTML(orig) with HasLanguage with HasRuleContext {
  val doc = new Document(path)
  sourceref.foreach(s => SourceRef.update(doc,s))
  sstate.foreach { state =>
    state.controller.library.add(doc)
  }
  override def onAdd: Unit = sstate.foreach { state =>
    state.controller.library.endAdd(doc)
  }
}


case class HTMLInputref(orig : HTMLParser.HTMLNode) extends OMDocHTML(orig) {
  sstate.foreach { state =>
    collectAncestor {
      case t: HTMLDocument => t
    }.foreach{ doc =>
      val dref = DRef(doc.doc.path,Path.parseD(resource + ".omdoc",NamespaceMap.empty))
      dref.setOrigin(GeneratedDRef)
      state.controller.add(dref)
    }
  }
}

case class HTMLIncludeproblem(orig : HTMLParser.HTMLNode) extends OMDocHTML(orig) {
  sstate.foreach { state =>
    collectAncestor {
      case t: HTMLDocument => t
    }.foreach{ doc =>
      val dref = DRef(doc.doc.path,Path.parseD(resource + ".omdoc",NamespaceMap.empty))
      dref.setOrigin(GeneratedDRef)
      state.controller.add(dref)
    }
  }
}

trait HasLanguage extends OMDocHTML {
  var language : String = ""
}

trait HasType extends OMDocHTML {
  var tp : Option[Term] = None
}

trait HasDefiniens extends OMDocHTML {
  var df : Option[Term] = None
}

trait HasDefinientia extends OMDocHTML {
  var dfs: mutable.HashMap[GlobalName,Term] = mutable.HashMap.empty
}

trait HasArity extends OMDocHTML {
  var arity: String = ""
}

trait HasAssocType extends OMDocHTML {
  var assoctype: String = ""
}

trait HasReorderArgs extends OMDocHTML {
  var reorders: String = ""
}

trait HasMacroName extends OMDocHTML {
  var macroname: String = ""
}

trait HasSimpleAssignments extends OMDocHTML {
  var assignments: List[(GlobalName,Term)] = Nil
}

trait HasComplexAssignments extends OMDocHTML {
  var assignments: List[(GlobalName,Option[LocalName],Option[Term])] = Nil
  var moduleassignments : List[(MPath,ContentPath)] = Nil
}

trait HasDomain extends OMDocHTML {
  var domain : Option[MPath] = None
}

case class HTMLDonotcopy(orig : HTMLParser.HTMLNode) extends OMDocHTML(orig) with HasDomain {
  def path = Path.parseMS(resource,NamespaceMap.empty)
  override def onAdd: Unit = sstate.foreach {state =>
    collectAncestor {
      case ha: HasComplexAssignments => ha
    }.foreach {ha => domain.foreach {dom =>
      ha.moduleassignments ::= (dom,path)
    }}
  }
}

trait HTMLConstant extends OMDocHTML {
  def path = Path.parseS(resource,NamespaceMap.empty)
  var role = ""
}

case class HTMLDomainComponent(orig : HTMLParser.HTMLNode) extends OMDocHTML(orig) {
  collectAncestor {
    case hl: HasDomain =>
      hl
  }.foreach { ha =>
    ha.domain = Some(Path.parseM(resource))
  }
}

case class HTMLComplexAssignment(orig : HTMLParser.HTMLNode) extends OMDocHTML(orig) with HasDefiniens {
  var alias : String = ""
  def path = Path.parseS(resource)
  override def onAdd: Unit = {
    collectAncestor{
      case hl : HasComplexAssignments =>
        hl
    }.foreach { ha => sstate.foreach{ state =>
      (alias,df) match {
        case ("",None) =>
        case ("",Some(df)) => ha.assignments ::= (path,None,Some(state.applyTopLevelTerm(df)))
        case (s,Some(df)) => ha.assignments ::= (path,Some(LocalName(s)),Some(state.applyTopLevelTerm(df)))
        case (s,None) => ha.assignments ::= (path,Some(LocalName(s)),None)
      }
    }}
  }
}

case class HTMLAliasComponent(orig : HTMLParser.HTMLNode) extends OMDocHTML(orig) {
  collectAncestor {
    case ass : HTMLComplexAssignment => ass
  }.foreach(_.alias = resource)
}

case class HTMLSimpleAssignment(orig : HTMLParser.HTMLNode) extends OMDocHTML(orig) {
  collectAncestor{
    case hl : HasSimpleAssignments =>
      hl
  }.foreach{ha =>
    val (dm,tg) = {
      val ls = resource.split(',')
      assert(ls.length == 2)
      (Path.parseS(ls.head.split("#").head),{
        if (ls(1).startsWith("var://")) {
          val rest = ls(1).drop(6).split("#").head
          if (rest.contains('.')) {
            val (v,field) = {
              val ls = rest.split('.')
              (ls.head,ls(1))
            }
            Getfield(OMV(v),LocalName(field))
          } else OMV(rest)
        } else if (ls(1).startsWith("varseq://")) {
          val rest = resource.drop(9).split("#").head
          val ret = OMV(rest)
          ret.metadata.update(STeX.flatseq.sym,OMS(STeX.flatseq.sym))
          ret
        } else OMID(Path.parseS(ls(1).split("#").head))
      })
    }
    ha.assignments ::= (dm,tg)
  }
}

case class HTMLLanguageComponent(orig : HTMLParser.HTMLNode) extends OMDocHTML(orig) {
  collectAncestor{
    case hl : HasLanguage =>
      hl
  }.foreach(_.language = resource)
}

case class HTMLSignatureComponent(orig : HTMLParser.HTMLNode) extends OMDocHTML(orig) {
  collectAncestor {
    case th : HTMLTheory => th
  }.foreach(_.signature = resource)
}

case class HTMLArityComponent(orig : HTMLParser.HTMLNode) extends OMDocHTML(orig) {
  collectAncestor {
    case ha: HasArity => ha
  }.foreach(_.arity = resource)
}

case class HTMLMacroNameComponent(orig : HTMLParser.HTMLNode) extends OMDocHTML(orig) {
  collectAncestor {
    case ha: HasMacroName => ha
  }.foreach(_.macroname = resource)
}

case class HTMLMetatheoryComponent(orig : HTMLParser.HTMLNode) extends OMDocHTML(orig) {
  if (resource.nonEmpty) {
    collectAncestor {
      case ha: HTMLModuleLike => ha
    }.foreach(_.metatheory = Some(Path.parseM(resource)))
  }
}

case class HTMLBindTypeComponent(orig:HTMLParser.HTMLNode) extends OMDocHTML(orig) {
  collectAncestor {
    case g: HTMLGroupLike => g
  }.foreach{g =>
    val (tp,v) = {
      val ls = resource.split(',')
      (ls.head,ls(1))
    }
    g.getVariables.findLast(_.name.toString == v) match {
      case Some(vd) =>
        val nvd = VarDecl(vd.name,vd.feature,vd.tp,vd.df,vd.not)
        vd.metadata.getAll.foreach(nvd.metadata.update)
        nvd.metadata.update(STeX.meta_quantification,OMS(
          if (tp == "forall") STeX.meta_qforall else STeX.meta_qexists
        ))
        g.variables = g.variables ++ nvd
      case _ =>
        print("")
    }
  }
}

case class HTMLAssoctypeComponent(orig:HTMLParser.HTMLNode) extends OMDocHTML(orig) {
  collectAncestor {
    case ha: HasAssocType => ha
  }.foreach(_.assoctype = resource)
}

case class HTMLReorderComponent(orig:HTMLParser.HTMLNode) extends OMDocHTML(orig) {
  collectAncestor {
    case ha: HasReorderArgs => ha
  }.foreach(_.reorders = resource)
}

case class HTMLTypeComponent(orig:HTMLParser.HTMLNode) extends OMDocHTML(orig) {
  if (sstate.exists(_.in_term)) {
    ???
  }
  sstate.foreach(_.in_term = true)


  override def onAdd: Unit = {
    sstate.foreach { state =>
      state.in_term = false
      collectAncestor {
        case hd: HasType => hd
      }.foreach(_.tp = findTerm)
    }
  }
}

case class HTMLDefComponent(orig:HTMLParser.HTMLNode) extends OMDocHTML(orig) {
  assert(sstate.forall(!_.in_term))
  sstate.foreach(_.in_term = true)

  override def onAdd: Unit = {
    sstate.foreach { state =>
      state.in_term = false
      collectAncestor {
        case hd: HasDefiniens =>
          hd.df = findTerm
          true
        case hd : HasDefinientia =>
          resource.split(',').foreach {r =>
            if (r.nonEmpty) {
              val gn = Path.parseS(r)
              findTerm.foreach(hd.dfs(gn) = _)
              collectAncestor { case s: HTMLStatement => s}.foreach(_.fors ::= gn)
            }
          }
          true
      }
    }
  }
}

case class HTMLArgMarker(orig : HTMLParser.HTMLNode) extends OMDocHTML(orig) {}

case class HTMLNotationFragment(orig : HTMLParser.HTMLNode) extends OMDocHTML(orig) {
  override def onAdd: Unit = if (resource != "") collectAncestor {
    case s: NotationLike => s
  }.foreach(_.fragment = resource)
}
case class HTMLNotationPrec(orig : HTMLParser.HTMLNode) extends OMDocHTML(orig) {
  override def onAdd: Unit = if (resource != "") collectAncestor {
    case s: NotationLike => s
  }.foreach(_.precedence = resource)
}

case class HTMLNotationComponent(orig : HTMLParser.HTMLNode) extends OMDocHTML(orig) {
  assert(sstate.forall(!_.in_term))
  sstate.foreach(_.in_term = true)
  def getNotChild() : Option[HTMLTerm] = {
    iterate{
      case t : HTMLTerm => return Some(t)
      case  _ =>
    }
    None
  }
  override def onAdd: Unit = {
    sstate.foreach(_.in_term = false)
    collectAncestor {
      case c : NotationLike =>
        c.notation = getNotChild()
    }
  }
}

case class HTMLNotationOpComponent(orig : HTMLParser.HTMLNode) extends OMDocHTML(orig) {
  assert(sstate.forall(!_.in_term))
  sstate.foreach(_.in_term = true)
  def getNotChild() : Option[HTMLTerm] = {
    this.iterate{
      case t : HTMLTerm => return Some(t)
      case  _ =>
    }
    None
  }
  override def onAdd: Unit = {
    sstate.foreach(_.in_term = false)
    collectAncestor {
      case c : NotationLike =>
        c.opnotation = this.getNotChild()
    }
  }
}

trait HTMLModuleLike extends OMDocHTML with HasRuleContext with HasLanguage {
  var metatheory : Option[MPath] = None

  def path = Path.parseMS(resource,NamespaceMap.empty)
  def name = path.name
  def dpath = path.toMPath.parent

  var signature_theory : Option[AbstractTheory] = None
  var language_theory : Option[AbstractTheory] = None
  def sighome = signature_theory match {
    case Some(t : Theory) => Some(t.toTerm)
    case Some(d : DerivedDeclaration) => Some(OMMOD(d.modulePath))
    case _ => None
  }

  def open: Unit
}

case class HTMLStructuralFeature(orig:HTMLParser.HTMLNode,feature:String) extends OMDocHTML(orig) with HTMLModuleLike {
  override def open: Unit = sstate.foreach { state => collectAncestor {case th : HTMLModuleLike => th}.foreach {ml => ml.signature_theory.foreach {th =>
    val p = path
    val dd = new DerivedDeclaration(th.toTerm,p.name,feature,TermContainer.empty(),NotationContainer.empty())
    state.controller.add(dd)
    _context = _context ++ Context(dd.modulePath)
    signature_theory = Some(dd)
    ml.language_theory.foreach{lt =>
      state.controller.add(PlainInclude(dd.modulePath,lt.path.toMPath))
      language_theory = Some(lt)
    }
  }}}

  override def onAdd: Unit = {
    sstate.foreach { state => signature_theory.foreach{th =>
      state.controller.endAdd(th)
      state.check(th)
    }}

  }
}

case class HTMLStructureFeature(orig:HTMLParser.HTMLNode) extends OMDocHTML(orig) with HTMLModuleLike {
  private var parentmodule : Option[AbstractTheory] = None
  override def open: Unit = sstate.foreach { state => collectAncestor {case th : HTMLModuleLike => th}.foreach {ml => ml.signature_theory.foreach {t =>
    val th = Theory(path.module.parent,path.toMPath.name,None)
    parentmodule = Some(t)
    val nt = new NestedModule(t.toTerm,path.name,th)
    state.controller.add(nt)
    signature_theory = Some(th)
  }
    ml.language_theory.foreach {t =>
      val th = Theory(t.modulePath.parent,t.name / path.name,None)
      val nt = new NestedModule(t.toTerm,path.name,th)
      val incl = PlainInclude(path.module.parent ? path.toMPath.name,th.path)
      state.controller.add(nt)
      state.controller.add(incl)
      state.controller.endAdd(incl)
      language_theory = Some(th)
      this.language = ml.language
    }
  }}

  override def onAdd: Unit = {
    sstate.foreach { state =>
      signature_theory.foreach { th =>
        state.controller.endAdd(th)
        state.check(th)
        val rname = LocalName(path.name.toString.dropRight(10))
        val c = Constant(parentmodule.get.toTerm, rname, Nil, Some(OMS(ModelsOf.tp)), Some(ModelsOf(path.toMPath)), None)
        state.controller.add(c)
      }
      language_theory.foreach { th =>
        state.controller.endAdd(th)
        //state.check(th)
      }
    }

  }
}

case class HTMLTheory(orig : HTMLParser.HTMLNode) extends OMDocHTML(orig) with HTMLModuleLike {
  var signature : String = ""

  _context = _context ++ Context(path.toMPath)

  def open : Unit = sstate.foreach { state =>
    val pth = collectAncestor {case th : HTMLTheory => th}
    metatheory.foreach { p =>
      state.controller.getO(p) match {
        case None =>
          state.missings ::= p
          metatheory = None
        case _ =>
      }
    }
    pth match {
      case Some(t) =>
        t.signature_theory.foreach {t =>
          val th = Theory(dpath, path.name, metatheory) // TODO parameters
          val nt = new NestedModule(t.toTerm,path.name.tail,th)
          state.controller.add(nt)
          signature_theory = Some(th)
        }
        if (language != "") {
          t.language_theory.foreach { t =>
            val th = Theory(dpath / path.name, LocalName(language), metatheory) // TODO parameters
            val nt = new NestedModule(t.toTerm,  LocalName(language), th)
            state.controller.add(nt)
            signature_theory = Some(th)
          }
        }
        return ()
      case _ =>
    }
    if (signature == "") {
      val th = Theory(dpath, path.name, metatheory) // TODO parameters
      sourceref.foreach(s => SourceRef.update(th,s))
      signature_theory = Some(th)
      state.controller.add(th)
      if (language.isEmpty) language = "en"
      //if (language != "") {
        val lang = Theory(dpath / path.name,LocalName(language),metatheory)
        lang.metadata.update(STeX.meta_language,StringLiterals(language))
        sourceref.foreach(s => SourceRef.update(lang,s))
        language_theory = Some(lang)
        state.controller.add(lang)
        val incl = PlainInclude(path.toMPath,lang.path)
        state.controller.add(incl)
        state.controller.endAdd(incl)
        _context = _context ++ Context(lang.path)
      //}
    } else if (language == signature) {
      val sig = Theory(dpath, path.name, metatheory) // TODO parameters
      sourceref.foreach(s => SourceRef.update(sig,s))
      signature_theory = Some(sig)
      state.controller.add(sig)
      val lang = Theory(dpath / path.name,LocalName(language),metatheory)
      lang.metadata.update(STeX.meta_language,StringLiterals(language))
      sourceref.foreach(s => SourceRef.update(lang,s))
      language_theory = Some(lang)
      state.controller.add(lang)
      val incl = PlainInclude(sig.path,lang.path)
      state.controller.add(incl)
      state.controller.endAdd(incl)
      _context = _context ++ Context(lang.path)
    } else {
      val lang = Theory(dpath / path.name,LocalName(language),metatheory)
      lang.metadata.update(STeX.meta_language,StringLiterals(language))
      sourceref.foreach(s => SourceRef.update(lang,s))
      language_theory = Some(lang)
      state.controller.add(lang)
      val incl = PlainInclude(path.toMPath,lang.path)
      state.controller.add(incl)
      state.controller.endAdd(incl)
      _context = _context ++ Context(lang.path)
    }
    print("")
    (signature_theory.toList ::: language_theory.toList).foreach {t =>
      collectAncestor{ case th:HasRuleContext => th} match {
        case Some(st : HTMLDocument) => state.controller.add(MRef(st.path,t.path.toMPath))
        case _ =>
          state.controller.add(MRef(state.doc.path,t.path.toMPath))
      }
    }
  }

  override def onAdd = sstate.foreach { state =>
    signature_theory.foreach(state.controller.endAdd)
    language_theory.foreach(state.controller.endAdd)
  }
}

case class HTMLProblem(orig : HTMLParser.HTMLNode) extends OMDocHTML(orig) with HTMLModuleLike {
  var signature : String = ""

  _context = _context ++ Context(path.toMPath)

  def open : Unit = sstate.foreach { state =>
    collectAncestor {case th : HTMLTheory => th} match {
      case Some(t) =>
        t.signature_theory.foreach {t =>
          val th = Theory(dpath, path.name, metatheory) // TODO parameters
          val nt = new NestedModule(t.toTerm,path.name,th)
          state.controller.add(nt)
          signature_theory = Some(th)
        }
        if (language != "") {
          t.language_theory.foreach { t =>
            val th = Theory(dpath / path.name, LocalName(language), metatheory) // TODO parameters
            val nt = new NestedModule(t.toTerm,  LocalName(language), th)
            state.controller.add(nt)
            signature_theory = Some(th)
          }
        }
        return ()
      case _ =>
    }
    if (signature == "") {
      val th = Theory(dpath, path.name, metatheory) // TODO parameters
      sourceref.foreach(s => SourceRef.update(th,s))
      signature_theory = Some(th)
      state.controller.add(th)
      if (language.isEmpty) language = "en"
      //if (language != "") {
        val lang = Theory(dpath / path.name,LocalName(language),metatheory)
        lang.metadata.update(STeX.meta_language,StringLiterals(language))
        sourceref.foreach(s => SourceRef.update(lang,s))
        language_theory = Some(lang)
        state.controller.add(lang)
        val incl = PlainInclude(path.toMPath,lang.path)
        state.controller.add(incl)
        state.controller.endAdd(incl)
        _context = _context ++ Context(lang.path)
      //}
    } else if (language == signature) {
      val sig = Theory(dpath, path.name, metatheory) // TODO parameters
      sourceref.foreach(s => SourceRef.update(sig,s))
      signature_theory = Some(sig)
      state.controller.add(sig)
      val lang = Theory(dpath / path.name,LocalName(language),metatheory)
      lang.metadata.update(STeX.meta_language,StringLiterals(language))
      sourceref.foreach(s => SourceRef.update(lang,s))
      language_theory = Some(lang)
      state.controller.add(lang)
      val incl = PlainInclude(sig.path,lang.path)
      state.controller.add(incl)
      state.controller.endAdd(incl)
      _context = _context ++ Context(lang.path)
    } else {
      val lang = Theory(dpath / path.name,LocalName(language),metatheory)
      lang.metadata.update(STeX.meta_language,StringLiterals(language))
      sourceref.foreach(s => SourceRef.update(lang,s))
      language_theory = Some(lang)
      state.controller.add(lang)
      val incl = PlainInclude(path.toMPath,lang.path)
      state.controller.add(incl)
      state.controller.endAdd(incl)
      _context = _context ++ Context(lang.path)
    }
    print("")
    (signature_theory.toList ::: language_theory.toList).foreach {t =>
      collectAncestor{ case th:HasRuleContext => th} match {
        case Some(st : HTMLDocument) => state.controller.add(MRef(st.path,t.path.toMPath))
        case _ =>
          state.controller.add(MRef(state.doc.path,t.path.toMPath))
      }
    }
  }

  override def onAdd = sstate.foreach { state =>
    signature_theory.foreach(state.controller.endAdd)
    language_theory.foreach(state.controller.endAdd)
  }
}


case class HTMLTheoryHeader(orig : HTMLParser.HTMLNode) extends OMDocHTML(orig) {
  override def onAdd: Unit = collectAncestor {
    case th : HTMLModuleLike => th
    /*case th : HTMLDerived =>
      th.open*/
  }.foreach(_.open)
}

case class HTMLSymbol(orig : HTMLParser.HTMLNode) extends OMDocHTML(orig) with HTMLConstant with HasArity with HasMacroName with HasType with HasDefiniens with HasAssocType with HasReorderArgs {
  override def onAdd: Unit = sstate.foreach { state =>
    collectAncestor {
      case th: HTMLModuleLike => th
    }.foreach {th =>
        if (th.signature_theory.isDefined) {
          val c = Constant(th.sighome.get,path.name,Nil,tp.map(state.applyTopLevelTerm),df.map(state.applyTopLevelTerm),Some("stexsymbol"))
          OMDocHTML.setAssoctype(c,assoctype)
          OMDocHTML.setReorder(c,reorders)
          OMDocHTML.setMacroName(c,macroname)
          OMDocHTML.setArity(c,arity)
          sourceref.foreach(s => SourceRef.update(c,s))
          state.controller.add(c)
          (df,tp) match {
            case (Some(OMS(_)),None) =>
            case _ =>
              state.check(c)
          }
          /*th match {
            case _:HTMLTheory =>
              state.check(c)
            case _ =>
          }*/
          assoctype match {
            case "" =>
            case "binr" | "bin" =>
              val rc = state.rci(th.sighome.get.toMPath,OMA(OMID(AssocBinR.mpath),List(OMS(c.path))),true)
              sourceref.foreach(s => SourceRef.update(rc,s))
              state.controller.add(rc)
            case "binl" =>
              val rc = state.rci(th.sighome.get.toMPath,OMA(OMID(AssocBinL.mpath),List(OMS(c.path))),true)
              sourceref.foreach(s => SourceRef.update(rc,s))
              state.controller.add(rc)
            case "conj" =>
              val rc = state.rci(th.sighome.get.toMPath,OMA(OMID(AssocConj.mpath),List(OMS(c.path))),true)
              sourceref.foreach(s => SourceRef.update(rc,s))
              state.controller.add(rc)
            case "pre" =>
              val rc = state.rci(th.sighome.get.toMPath,OMA(OMID(AssocPre.mpath),List(OMS(c.path))),true)
              sourceref.foreach(s => SourceRef.update(rc,s))
              state.controller.add(rc)
            case "pwconj" =>
              val rc = state.rci(th.sighome.get.toMPath,OMA(OMID(AssocPwconj.mpath),List(OMS(c.path))),true)
              sourceref.foreach(s => SourceRef.update(rc,s))
              state.controller.add(rc)
            case _ =>
              ???
          }
        }
    }
  }
}

trait NotationLike extends OMDocHTML with HasArity {
  var fragment : String = ""
  var precedence : String = ""
  var notation : Option[HTMLNode] = None
  var opnotation : Option[HTMLNode] = None
}

case class HTMLNotation(orig : HTMLParser.HTMLNode) extends OMDocHTML(orig) with HTMLConstant with NotationLike {
  print("")
  override def onAdd: Unit = sstate.foreach { state =>
    collectAncestor {
      case t: HTMLModuleLike => t
    }.foreach { t =>
        notation.foreach { not =>
          val th = t.signature_theory match {
            case Some(th) => Some(th)
            case _ => t.language_theory
          }
          th.foreach { th =>
            val tp = STeX.notation.tp(path, arity)
            val df = STeX.notation(not.node,precedence,fragment,opnotation.map(_.node))
            val c = Constant(t.sighome.getOrElse(th.toTerm),state.newName("notation"),Nil,Some(tp),Some(df),Some("notation"))
            sourceref.foreach(s => SourceRef.update(c,s))
            state.controller.add(c)
          }
        }
    }
  }
}

trait HTMLVariable extends OMDocHTML with HasType {
  //var bindtype : String = ""
  override def onAdd: Unit = {
    sstate.foreach { state =>
      collectAncestor {
        case t: HTMLGroupLike => t
      }.foreach { g =>
        val vd = VarDecl(LocalName(resource),None,tp.map(state.applyTerm),None,None)
        /*if (bindtype != "") {
          vd.metadata.update(STeX.meta_quantification,OMS(
            if (bindtype == "forall") STeX.meta_qforall else STeX.meta_qexists
          ))
        } */
        g.variables = g.variables ++ vd
      }
    }
  }
}

case class HTMLVarDecl(orig : HTMLParser.HTMLNode) extends OMDocHTML(orig) with HTMLVariable with HasMacroName with NotationLike with HasDefiniens with HasAssocType with HasReorderArgs {
  override def onAdd: Unit = {
    sstate.foreach { state =>
      collectAncestor {
        case t: HTMLGroupLike => t
      }.foreach { g =>
        val vd = VarDecl(LocalName(resource),None,tp.map(state.applyTerm),None,None)
        OMDocHTML.setArity(vd,arity)
        OMDocHTML.setAssoctype(vd,assoctype)
        OMDocHTML.setReorder(vd,reorders)
        OMDocHTML.setMacroName(vd,macroname)
        notation.foreach { not =>
          vd.metadata.update(STeX.notation.tp.sym, STeX.notation(
            not.node,precedence,fragment,opnotation.map(_.node)
          ))
        }
        g.variables = g.variables ++ vd
      }
    }
  }
}
case class HTMLVarSeqDecl(orig : HTMLParser.HTMLNode) extends OMDocHTML(orig) with HTMLVariable with HasMacroName with NotationLike {
  var startidx : Option[Term] = None
  var endidx : Option[Term] = None
  override def onAdd: Unit = {
    sstate.foreach { state =>
      collectAncestor {
        case t: HTMLGroupLike => t
      }.foreach { g =>
        assert(startidx.isDefined && endidx.isDefined)
        tp = tp.map {
          case OMV(n) => state.getVariableContext.findLast(_.name == n).flatMap(_.tp) match {
            case Some(STeX.flatseq.tp(_)) => OMV(n)
            case _ => STeX.flatseq.tp(OMV(n))
          }
          case t => STeX.flatseq.tp(t)
        }
        val vd = VarDecl(LocalName(resource),None,tp.map(state.applyTerm),None,None)
        OMDocHTML.setArity(vd,arity)
        //OMDocHTML.setAssoctype(vd,assoctype)
        //OMDocHTML.setReorder(vd,reorders)
        OMDocHTML.setMacroName(vd,macroname)
        notation.foreach { not =>
          vd.metadata.update(STeX.notation.tp.sym, STeX.notation(
            not.node,precedence,fragment,opnotation.map(_.node)
          ))
        }
        g.variables = g.variables ++ vd
      }
    }
  }
}
case class HTMLVarSeqStart(orig : HTMLParser.HTMLNode) extends OMDocHTML(orig) with HTMLVariable {
  assert(sstate.forall(!_.in_term))
  sstate.foreach(_.in_term = true)
  override def onAdd = {
    sstate.foreach(_.in_term = false)
    collectAncestor {
      case c : HTMLVarSeqDecl => c
    }.foreach {p => p.startidx = findTerm.map {
      case STeX.informal.op("mrow",List(t)) => t
      case t => t
    } }
  }
}

case class HTMLVarSeqEnd(orig : HTMLParser.HTMLNode) extends OMDocHTML(orig) with HTMLVariable {
  assert(sstate.forall(!_.in_term))
  sstate.foreach(_.in_term = true)
  override def onAdd = {
    sstate.foreach(_.in_term = false)
    collectAncestor {
      case c : HTMLVarSeqDecl => c
    }.foreach {p => p.endidx = findTerm.map {
      case STeX.informal.op("mrow",List(t)) => t
      case t => t
    } }
  }
}

case class HTMLVarStructDecl(orig : HTMLParser.HTMLNode) extends OMDocHTML(orig) with HTMLVariable with HasSimpleAssignments with HasDomain {
  override def onAdd: Unit = {
    tp = if (assignments.isEmpty) domain.map(ModelsOf(_)) else {
      val omls = assignments.reverse.map(p => OML(p._1.name.filter(_.isInstanceOf[SimpleStep]),None,Some(p._2)))
      domain.map(d => RecMerge(ModelsOf(OMMOD(d)),RecType.make(omls:_*)))
    }
    super.onAdd
  }
}

case class HTMLImport(orig : HTMLParser.HTMLNode) extends OMDocHTML(orig) {
  def domain = Path.parseM(resource)
  sstate.foreach { state =>
    collectAncestor {
      case t: HTMLModuleLike => t
    }.foreach{t => t.signature_theory.foreach { th =>
      try {
        val incl = PlainInclude(domain, t.sighome.get.toMPath)
        sourceref.foreach(s => SourceRef.update(incl, s))
        state.controller.add(incl)
        state.controller.endAdd(incl)
        state.check(incl)
      } catch {
        case a : AddError =>
      }
    }}
  }
}

case class HTMLCopyModule(orig : HTMLParser.HTMLNode,istotal:Boolean) extends OMDocHTML(orig) with HasDomain with HasComplexAssignments {
  def path = Path.parseS(resource)
  override def onAdd: Unit = sstate.foreach { state =>
    collectAncestor{
      case t : HTMLModuleLike => t
    }.foreach { t => t.signature_theory.foreach{th =>
      val struct = Structure(t.sighome.get,path.name,OMMOD(domain.get),false,istotal)
      state.controller.add(struct)
      moduleassignments.foreach {
        case (mp,cp) =>
          val s = Include(t.sighome.get,mp,Nil,Some(cp match {
            case s : GlobalName => OMS(s)
            case m : MPath =>
              OMIDENT(OMMOD(m))
          }))
          state.controller.add(s)
          state.controller.endAdd(s)
      }
      assignments.reverse.foreach {
        case (p,alias,df) =>
          val npath = state.controller.getO(p) match {
            case Some(d : Declaration) => d.path
            case _ => p
          }
          val c = Constant(struct.toTerm,ComplexStep(npath.module) / npath.name,alias.toList,None,df,None)
          df match {
            case Some(OMS(p)) =>
              state.controller.getO(p) match {
                case Some(d) => d.metadata.getAll.foreach(c.metadata.update)
                case _ =>
                  print("")
              }
            case Some(o) =>
              println(o)
            case None =>
          }
          state.controller.getO(p) match {
            case Some(d) => d.metadata.getAll.foreach(c.metadata.update)
            case _ =>
              print("")
          }
          state.controller.add(c)
          state.check(c)
      }
      state.controller.endAdd(struct)
      state.check(struct)

    }}
  }
}

case class HTMLRealization(orig : HTMLParser.HTMLNode) extends OMDocHTML(orig) with HasDomain with HasComplexAssignments {
  def path = Path.parseS(resource)
  override def onAdd: Unit = sstate.foreach { state =>
    collectAncestor{
      case t : HTMLModuleLike => t
    }.foreach { t => t.signature_theory.foreach{th =>
      val struct = Structure(t.sighome.get,LocalName(domain.get),OMMOD(domain.get),true,true)
      state.controller.add(struct)
      moduleassignments.foreach {
        case (mp,cp) =>
          val s = Include(t.sighome.get,mp,Nil,Some(cp match {
            case s : GlobalName => OMS(s)
            case m : MPath =>
              OMIDENT(OMMOD(m))
          }))
          state.controller.add(s)
          state.controller.endAdd(s)
      }
      assignments.reverse.foreach {
        case (p,alias,df) =>
          val npath = state.controller.getO(p) match {
            case Some(d : Declaration) => d.path
            case _ => p
          }
          val c = Constant(struct.toTerm,ComplexStep(npath.module) / npath.name,alias.toList,None,df,None)
          state.controller.add(c)
          state.check(c)
      }
      state.controller.endAdd(struct)
      state.check(struct)

    }}
  }
}

case class HTMLUseModule(orig : HTMLParser.HTMLNode) extends OMDocHTML(orig) {
  def domain = Path.parseM(resource)
  sstate.foreach { state =>
    collectAncestor {
      case t: HTMLModuleLike => t
    }.foreach{t =>
        t.language_theory.foreach { th =>
          val incl = PlainInclude(domain, th.path.toMPath)
          sourceref.foreach(s => SourceRef.update(incl,s))
          state.controller.add(incl)
          state.controller.endAdd(incl)
        }
    }
  }
}

trait NotationComponent extends OMDocHTML {
  override def getTerm: Option[Term] = None
}

case class HTMLComp(orig : HTMLParser.HTMLNode) extends OMDocHTML(orig) with NotationComponent {
  this.classes ::= "symcomp"
}
case class HTMLVarComp(orig : HTMLParser.HTMLNode) extends OMDocHTML(orig) with NotationComponent {
  this.classes ::= "varcomp"
}

case class HTMLDefiniendum(orig : HTMLParser.HTMLNode) extends OMDocHTML(orig) with NotationComponent with HasHead {
  def path = Path.parseMS(resource,NamespaceMap.empty)
  this.classes ::= "definiendum"
  def toTerm = OMID(path)
  collectAncestor {
    case s : HTMLStatement if resource != "" => s
  }.foreach(_.fors ::= path)
}

case class HTMLArg(orig : HTMLParser.HTMLNode) extends OMDocHTML(orig) {
  override def forceTerm: Term = children match {
    case List(o:OMDocHTML) => o.forceTerm
    case _ => super.forceTerm2(this)
  }
}

final case class HTMLTopLevelTerm(orig : OMDocHTML) extends OMDocHTML(orig) with HTMLConstant {
  assert(sstate.forall(!_.in_term))
  sstate.foreach(_.in_term = true)

  override def onAdd: Unit = {
    children.foreach(c => orig.add(c.copy))
    orig.onAdd
    sstate.foreach{state =>
      state.in_term = false
      collectAncestor {
        case t: HTMLModuleLike => t
      }.foreach { th =>
        orig.getTerm match {
          case None | Some(OMV(_)) | Some(OMS(_)) =>
          case Some(tm) =>
            val t = state.applyTopLevelTerm(tm)
            if (th.language_theory.isDefined) {
              val c = Constant(th.language_theory.get.toTerm,state.newName("term"),Nil,None,Some(t),Some("term"))
              this.attributes((HTMLParser.ns_mmt,"term")) = c.path.toString
              sourceref.foreach(s => SourceRef.update(c,s))
              state.controller.add(c)
              th match {
                case _:HTMLTheory =>
                  state.check(c)
                case _ =>
              }
            }
        }
    }}
  }
}

trait HTMLStatement extends OMDocHTML with HTMLGroupLike with HasLanguage {
  var typestrings : List[String] = Nil
  var fors = resource.split(',').filter(_.nonEmpty).map(s => Path.parseMS(s.trim,NamespaceMap.empty)).toList
  var name : String = ""

  def addSymbolDoc(paths:List[ContentPath]): Unit = if (paths.nonEmpty) {
    sstate.foreach { state =>
      collectAncestor {
        case t: HTMLModuleLike => t
      }.foreach{ t =>
          if (language == "") language = t.language
          t.language_theory.foreach{th =>
            val c = Constant(OMID(th.path),state.newName("symboldoc"),Nil,None,Some(STeX.symboldoc(paths.distinct,language,this)),Some("symboldoc"))
            sourceref.foreach(s => SourceRef.update(c,s))
            state.controller.add(c)
          }
      }
    }
  }

  def bindvars(tm : Term) = {
    val bvs = this.getVariables.filter(_.metadata.get(STeX.meta_quantification).exists(_.value == OMS(STeX.meta_qforall)))
    val ntm = if (bvs.isEmpty) tm else STeX.binder(bvs,tm)
    sstate.get.applyTopLevelTerm(ntm)
  }

  override def onAdd: Unit = {
    if (name.nonEmpty) {
      print("")
    }
  }
}

case class HTMLTypeStringComponent(orig : HTMLParser.HTMLNode) extends OMDocHTML(orig) {
  collectAncestor {
    case s : HTMLStatement => s
  }.foreach(_.typestrings = resource.split(',').map(_.trim).toList)
}

case class HTMLFromComponent(orig : HTMLParser.HTMLNode) extends OMDocHTML(orig) {}
case class HTMLToComponent(orig : HTMLParser.HTMLNode) extends OMDocHTML(orig) {}

case class HTMLStatementNameComponent(orig : HTMLParser.HTMLNode) extends OMDocHTML(orig) {
  collectAncestor {
    case s : HTMLStatement => s
  }.foreach(_.name = resource)
}

case class HTMLConclusionComponent(orig : HTMLParser.HTMLNode) extends OMDocHTML(orig) {
  assert(sstate.forall(!_.in_term))
  sstate.foreach(_.in_term = true)
  override def onAdd = sstate.foreach{ state =>
    state.in_term = false
    collectAncestor {
      case s: HTMLSAssertion => s
    }.foreach(_.conc = getTerm)
  }
}

case class HTMLSDefinition(orig : HTMLParser.HTMLNode) extends OMDocHTML(orig) with HTMLStatement with HasDefinientia {
  override def onAdd = { sstate.foreach{ state =>
    super.onAdd
    addSymbolDoc(fors)
    if (this.dfs.nonEmpty) {
      dfs.foreach {
        case (path,tm) =>
          val defi = bindvars(tm)
          val orig = state.controller.getO(path)
          (orig,collectAncestor { case s : HTMLModuleLike => s}) match {
            case (Some(c : Constant),Some(t)) if t.signature_theory.isDefined && c.parent == t.sighome.get.toMPath && c.df.isEmpty =>
              val nc = Constant(c.home,c.name,c.alias,c.tp,Some(defi),c.rl)
              state.controller.library.update(nc)
              //state.controller.add(nc)
              state.check(nc)
            case _ =>
              println("TODO: Equality rule")
          }
          print("")
      }
    }
  }}
}
case class HTMLSParagraph(orig : HTMLParser.HTMLNode) extends OMDocHTML(orig) with HTMLStatement {
  override def onAdd = {
    super.onAdd
    if (typestrings.contains("symdoc")) addSymbolDoc(fors)
  }
}

case class HTMLDoctitle(orig : HTMLParser.HTMLNode) extends OMDocHTML(orig) {
  override def onAdd = {
    super.onAdd
    collectAncestor {
      case d : HTMLDocument => d
    }.foreach { d =>
      val nnode = this.plaincopy
      nnode.label = "span"
      nnode.classes = Nil
      nnode.attributes.clear()
      d.doc.metadata.update(STeX.meta_doctitle,OMFOREIGN(nnode.node))
    }
  }
}

case class HTMLSAssertion(orig : HTMLParser.HTMLNode) extends OMDocHTML(orig) with HTMLStatement {
  var conc : Option[Term] = None
  override def onAdd = {sstate.foreach{ state => if (name.nonEmpty) { collectAncestor {
    case m:HTMLModuleLike => m
  }.foreach { m => m.signature_theory.foreach {_ =>
    val tm = conc match {
      case Some(t) => bindvars(STeX.judgmentholds(t))
      case _ => state.applyTopLevelTerm(STeX.judgmentholds(forceTerm2(this)))
    }
    val c = Constant(m.sighome.get,LocalName(name),Nil,Some(tm),None,Some("stexsymbol"))
    addSymbolDoc(List(c.path))
    state.controller.add(c)
    state.check(c)
  }}}}}
}
case class HTMLSExample(orig : HTMLParser.HTMLNode) extends OMDocHTML(orig) with HTMLStatement {
  override def onAdd = {sstate.foreach{ state => if (name.nonEmpty) { collectAncestor {
    case m:HTMLModuleLike => m
  }.foreach { m => m.signature_theory.foreach {_ =>
    val c = Constant(m.sighome.get,LocalName(name),Nil,None,None,Some("stexsymbol"))
    state.controller.add(c)
    state.check(c)
  }}}}}
}
case class HTMLSProof(orig : HTMLParser.HTMLNode) extends OMDocHTML(orig) with HTMLStatement {}
case class HTMLSProofstep(orig : HTMLParser.HTMLNode) extends OMDocHTML(orig) with HTMLStatement {}
case class HTMLSProofsketch(orig : HTMLParser.HTMLNode) extends OMDocHTML(orig) with HTMLStatement {}
case class HTMLSubproof(orig : HTMLParser.HTMLNode) extends OMDocHTML(orig) with HTMLStatement {}
case class HTMLSpfcase(orig : HTMLParser.HTMLNode) extends OMDocHTML(orig) with HTMLStatement {}
case class HTMLSpfeq(orig : HTMLParser.HTMLNode) extends OMDocHTML(orig) with HTMLStatement {}

case class HTMLSolution(orig : HTMLParser.HTMLNode) extends OMDocHTML(orig) {
  this.classes ::= "solution"
}
case class HTMLFrame(orig : HTMLParser.HTMLNode) extends OMDocHTML(orig) {
  this.classes ::= "frame"
}
// ---------------------------------------------------------------------------------------------------------------------

trait HTMLTerm extends OMDocHTML {
  def toTerm : Term
  override def isEmpty = false
}

trait HasHead extends HTMLTerm {
  def head = {
    Path.parseMS(resource.split("#").head,NamespaceMap.empty)
  }
  def headTerm = {
    if (resource.startsWith("var://")) {
      val rest = resource.drop(6).split("#").head
      if (rest.contains('.')) {
        val (v,field) = {
          val ls = rest.split('.')
          (ls.head,ls(1))
        }
        Getfield(OMV(v),LocalName(field))
      } else OMV(rest)
    } else if (resource.startsWith("varseq://")) {
      val rest = resource.drop(9).split("#").head
      val ret = OMV(rest)
      ret.metadata.update(STeX.flatseq.sym,OMS(STeX.flatseq.sym))
      ret
    } else if (head.name.toString.startsWith("field:") && head.name.toString.contains(".")) {
      val npath = head.module ? LocalName(head.name.toString.drop(6).takeWhile(_ != '.'))
      Getfield(OMS(npath),LocalName(head.name.toString.split('.').last))
    } else OMID(head)
  }
  def fragment = resource.split("#").tail.mkString("#")
}

trait ComplexTerm extends HTMLTerm with HasHead {
  def sortArgs : List[(Char,Term)] = {
    if (fragment.startsWith("CUSTOM-")) {
      val tms = children.flatMap(getCustomArgs).sortBy(_._1)
      var ret : List[(Char,List[Term])] = Nil
      tms.foreach {
        case (_,b@('i'|'b'),t) => ret ::= (b,List(t))
        case (i,b@('a'|'B'),t) if ret.length < i => ret ::= (b,List(t))
        case (_,_,t) => ret = (ret.head._1,t :: ret.head._2) :: ret.tail
      }
      ret.map {
        case (b@('i'|'b'),List(t)) => (b,t)
        case (b,List(OMV(v))) => sstate.get.getVariableContext.find(_.name == v) match {
          case Some(vd) => vd.tp match {
            case Some(STeX.flatseq.tp(_)) =>
              (b, OMV(v))
            case _ =>
              (b, STeX.flatseq(OMV(v)))
          }
          case _ =>
            (b, STeX.flatseq(OMV(v)))
        }
        case (b,ls) => (b,STeX.flatseq(ls.reverse :_*))
      }.reverse
    } else {
      val args = children.flatMap(getargs).sortBy(_._1).distinctBy(_._1)
      args.map(t => (t._2, t._3))
    }
  }

  private def getCustomArgs(top: HTMLNode) : List[(Int,Char,Term)] = top match {
    case a : HTMLArg => List((a.resource.tail.toInt,a.resource.head,a.findTerm.getOrElse(forceTerm2(a))))
    case n => n.children.flatMap(getCustomArgs)
  }

  private def getargs(top : HTMLNode) : List[(Int,Char,Term)] = top match {
    case a : HTMLArg if a.resource.head == 'a' || a.resource.head == 'B' =>
      val ret = top.children.flatMap(getargs)
      if (ret.isEmpty) {
        val forced = a.forceTerm
        (forced,sstate) match {
          case (OMV(v),Some(state)) => state.getVariableContext.findLast(_.name == v) match {
            case Some(vd) => vd.tp match {
              case Some(STeX.flatseq.tp(_)) =>
                return List((a.resource.tail.toInt,a.resource.head,forced))
              case _ =>
            }
            case _ =>
          }
          case _ =>
        }
        List((a.resource.tail.toInt,a.resource.head,STeX.flatseq(a.forceTerm)))
      } else {
        val as = ret.filter(t => t._2 == a.resource.head && t._1.toString.startsWith(a.resource.tail))
        val os = ret.filterNot(as.contains)
        (a.resource.tail.toInt, a.resource.head, STeX.flatseq(as.map(_._3): _*)) :: os
      }
    case a : HTMLArg if a.resource.head == 'A' => List((a.resource.tail.tail.toInt,a.resource.tail.head,a.forceTerm))
    case a : HTMLArg => List((a.resource.tail.toInt,a.resource.head,a.forceTerm))
    case n => n.children.flatMap(getargs)
  }
}

case class HTMLOMBIND(orig : HTMLParser.HTMLNode) extends OMDocHTML(orig) with ComplexTerm {

  override def toTerm = {
    import info.kwarc.mmt.stex.SOMBArg._
    val args = sortArgs.map {
      case (b@('b'|'B'),tm) =>
        SCtx(sstate.get.makeBinder(tm,b == 'B'))
      case (_,tm) => STerm(tm)
    }
    val tm = SOMB(headTerm,args:_*) //OMBINDC(OMID(head),ctx,args)
    if (fragment != "") tm.metadata.update(STeX.meta_notation,StringLiterals(fragment))
    tm
  }
}

case class HTMLOMID(orig : HTMLParser.HTMLNode) extends OMDocHTML(orig) with HTMLTerm with HasHead {
  override def toTerm: Term = {
    val tm = headTerm
    if (fragment != "") tm.metadata.update(STeX.meta_notation,StringLiterals(fragment))
    tm
  }
}

case class HTMLOMV(orig : HTMLParser.HTMLNode) extends OMDocHTML(orig) with HTMLTerm with HasHead {
  override def toTerm: Term = {
    val tm = headTerm
    if (fragment != "") tm.metadata.update(STeX.meta_notation,StringLiterals(fragment))
    tm
  }
}

case class HTMLOMA(orig : HTMLParser.HTMLNode) extends OMDocHTML(orig) with ComplexTerm {
  override def toTerm: Term = {
    val tm = OMDocHTML.OMAorSOMA(headTerm,sortArgs.map(_._2))
    if (fragment != "") tm.metadata.update(STeX.meta_notation,StringLiterals(fragment))
    tm
  }
}

case class MathMLNode(orig : HTMLParser.HTMLNode) extends OMDocHTML(orig) with HTMLTerm {
  override def toTerm: Term = forceTerm2(this)
}


case class HTMLMMTRule(orig : HTMLParser.HTMLNode) extends OMDocHTML(orig) with ComplexTerm {
  def toTerm = ???
  lazy val path = Path.parseM(resource)
  assert(sstate.forall(!_.in_term))
  sstate.foreach(_.in_term = true)

  override def onAdd: Unit = sstate.foreach { state =>
    sstate.foreach(_.in_term = false)
    collectAncestor {
      case t: HTMLModuleLike => t
    }.foreach { t =>
        t.signature_theory.foreach { th =>
          val args = sortArgs match {
            case List((_,STeX.informal(_))) => Nil
            case ls => ls.map(_._2)
          }
          val rc = state.rci(t.sighome.get.toMPath,OMAorAny(OMID(path),args.map(state.applyTerm)),true)
          sourceref.foreach(s => SourceRef.update(rc,s))
          state.controller.add(rc)
        }
    }
  }

}

/*

trait OMDocParent extends OMDocHTML {
  protected var _semanticchildren : List[OMDocHTML] = Nil
  def semanticchildren = _semanticchildren.reverse
  def add(child : OMDocHTML) = _semanticchildren ::= child
  private var structnames : List[LocalName] = Nil
  def newName(s : String,i : Int =0) : LocalName = if (structnames.contains(LocalName(s))) {
    if (structnames.contains(LocalName(s + i.toString))) newName(s,i+1) else {
      structnames ::= LocalName(s + i.toString)
      LocalName(s + i.toString)
    }
  } else {
    structnames ::= LocalName(s)
    LocalName(s)
  }

}

trait HasTermArgs extends OMDocHTML {
  protected var _terms : List[Term] = Nil
  def addArg(t : Term) = _terms ::= t
  def getArgs = _terms.reverse
}

class HTMLDeclaration(orig : HTMLParser.HTMLNode) extends OMDocHTML(orig)


class HTMLStructure(orig : HTMLParser.HTMLNode) extends HTMLDeclaration(orig) with OMDocParent {
  private var _name : String = "" // TODO
  def name = if (_name.nonEmpty) LocalName(_name) else LocalName(domain)
  var _nonEmpty = false
  def empty = (!_nonEmpty && _semanticchildren.isEmpty)
  def path = semanticParent match {
    case Some(p : HTMLModule) => p.path ? name
    case _ =>
      print("")
      ???
  }
}
class HTMLUseModule(orig : HTMLParser.HTMLNode) extends HTMLStructure(orig) {
  sstate.foreach { state =>
    collectAncestor {
      case t: HTMLTheory =>
        t.language_theory.foreach { th =>
          val incl = PlainInclude(domain, th.path)
          sourceref.foreach(s => SourceRef.update(incl,s))
          state.controller.add(incl)
          state.controller.endAdd(incl)
        }
      case p : OMDocParent =>
        print("")
    }
  }
}

class HTMLDerived(orig : HTMLParser.HTMLNode) extends HTMLDeclaration(orig) with HTMLModule with HasType with HasDefiniens {
  val feature = property.split(':') match {
    case Array("stex","feature",feat) => "stex:" + feat
    case _ =>
      ???
  }
  val decpath = Path.parseS(resource + "-feature")
  override def path = decpath.toMPath

  var dd : Option[DerivedDeclaration] = None
  def open = sstate.foreach { state => collectAncestor {
    case t : HTMLTheory =>
      t.signature_theory.foreach {th =>
        dd = Some(new DerivedDeclaration(th.toTerm,decpath.name,feature,TermContainer.asAnalyzed(tp.map(state.applyTopLevelTerm)),NotationContainer.empty()))
        sourceref.foreach(s => SourceRef.update(dd.get,s))
        state.controller.add(dd.get)
      }
    case p : OMDocParent =>
      print("")
  }}

  override def onAdd: Unit = sstate.foreach { state => dd.foreach{dd =>
    state.controller.endAdd(dd)
    state.controller.simplifier(dd)
  } }

}


class OMDocComponent(orig : HTMLParser.HTMLNode) extends OMDocHTML(orig)
class HTMLTermComponent(orig : HTMLParser.HTMLNode) extends OMDocComponent(orig) {
  var term : Option[Term] = None
  override def onAdd: Unit = {
    print("")
  }
}
class HTMLType(orig : HTMLParser.HTMLNode) extends HTMLTermComponent(orig) {
  override def onAdd: Unit = term.foreach { tm =>
    collectAncestor {
      case c : HasType =>
        sourceref.foreach(s => SourceRef.update(tm,s))
        c.tp = Some(tm)
    }
  }
}
class HTMLDefiniens(orig : HTMLParser.HTMLNode) extends HTMLTermComponent(orig) {
  override def onAdd: Unit = term.foreach { tm =>
    collectAncestor {
      case c : HasDefiniens =>
        sourceref.foreach(s => SourceRef.update(tm,s))
        c.df = Some(tm)
    }
  }
}
class HTMLMacroname(orig : HTMLParser.HTMLNode) extends OMDocComponent(orig) {
  override def onAdd: Unit = if (resource != "") {
    collectAncestor {
      case c : HTMLConstant => c.macroname = resource
    }
  }
}
class MetatheoryComponent(orig : HTMLParser.HTMLNode) extends OMDocComponent(orig) {
  var metatheory : Option[MPath] = None
  override def onAdd: Unit = metatheory match {
    case Some(mt) =>
      collectAncestor {
        case c : HTMLTheory => c.metatheory = Some(mt)
      }
    case _ =>
  }
}


class MathMLTerm(orig : HTMLParser.HTMLNode) extends HTMLTerm(orig)

class MathMLLiteral(orig : HTMLParser.HTMLNode) extends MathMLTerm(orig) {
  override def toTerm: Term = children match {
    case (t : HTMLParser.HTMLText) :: Nil =>
      t.text.toDoubleOption match {
        case Some(db) if db.isValidInt && db>0 => STeX.PosLiterals(BigInt(db.toInt))
        case Some(db) if db.isValidInt && db>=0 => STeX.NatLiterals(BigInt(db.toInt))
        case Some(db) if db.isValidInt => STeX.IntLiterals(BigInt(db.toInt))
        case Some(db) => STeX.RealLiterals(db)
        case _ =>
          ???
      }
    case _ =>
      print("")
      ???
  }
  print("")
}

trait HasHeadSymbol extends HTMLTerm {
  lazy val (head,arity,fragment) = resource.split('#') match {
    case Array(p,a,f) => (Path.parseMS(p,NamespaceMap.empty),a,f)
    case Array(l) => (Path.parseMS(l,NamespaceMap.empty),"","")
    case Array(p,a) => (Path.parseMS(p,NamespaceMap.empty),a,"")
    case _ =>
      println("Argh! Here: " + resource.split('#').mkString("Array(",",",")"))
      ???
  }
}


 */
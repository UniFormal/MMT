package info.kwarc.mmt.stex.xhtml

import info.kwarc.mmt.api.documents.{Document, MRef}
import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.informal.Informal
import info.kwarc.mmt.api.metadata.MetaDatum
import info.kwarc.mmt.api.modules.Theory
import info.kwarc.mmt.api.notations.{NotationContainer, TextNotation}
import info.kwarc.mmt.api.{ContentPath, DPath, GlobalName, LocalName, MPath, NamespaceMap, Path, Rule, RuleSet, StructuralElement}
import info.kwarc.mmt.api.objects.{Context, OMA, OMAorAny, OMBIND, OMBINDC, OMID, OMMOD, OMS, OMV, Obj, Term, VarDecl}
import info.kwarc.mmt.api.parser.{SourcePosition, SourceRef, SourceRegion}
import info.kwarc.mmt.api.symbols.{Constant, Declaration, DerivedDeclaration, PlainInclude, Structure, TermContainer}
import info.kwarc.mmt.api.uom.Scala.Opaque
import info.kwarc.mmt.api.utils.{File, URI, XMLEscaping}
import info.kwarc.mmt.odk.{IntegerLiterals, LFX, NatLiterals, PosLiterals}
import info.kwarc.mmt.sequences.Sequences
import info.kwarc.mmt.stex.Extensions.NotationExtractor
import info.kwarc.mmt.stex.xhtml.HTMLParser.{HTMLNode, HTMLText, ParsingState}
import info.kwarc.mmt.stex.{STeX, STeXServer}

import scala.collection.mutable
import scala.util.Try
import scala.xml.{Elem, Node}

object OMDocHTML {

  def makeMacroName(macroname : String) = {
    (STeX.meta_macro,STeX.StringLiterals(macroname))
  }

  def getMacroName(se : StructuralElement) = se.metadata.getValues(STeX.meta_macro).collectFirst {
    case STeX.StringLiterals(mn) => mn
  }

  def getNotations(se : StructuralElement,controller : Controller) = {
    val server = controller.extman.get(classOf[STeXServer]).head
    controller.depstore.queryList(se.path,NotationExtractor.notation).map(controller.getO).map {
      case Some(c : Constant) =>
        val arity = c.tp match {
          case Some(STeX.notation.tp(_,a)) => a
          case _ =>
            print("")
            ???
        }
        val (fragment,prec,node) = c.df match {
          case Some(STeX.notation(n,p,f)) => (f,p,n)
          case _ =>
            print("")
            ???
        }
        (fragment,prec,arity,HTMLParser(node.toString())(new ParsingState(controller,server.extensions.flatMap(_.rules))))
    }
  }
}


class OMDocHTML(orig : HTMLParser.HTMLNode) extends CustomHTMLNode(orig) {
  protected def resource = attributes.getOrElse((namespace,"resource"),"")
  protected def property = attributes.getOrElse((namespace,"property"),"")
  val sstate = state match {
    case s : SemanticState => Some(s)
    case _ => None
  }
  def semanticParent = collectAncestor{
    case p : OMDocParent => p
  }
}

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

  protected var _context = Context.empty
  protected var _rules : List[PartialFunction[Term, Term]] = Nil
  def context : Context = sstate.map(_ => semanticParent.map(_.context).getOrElse(Context.empty) ++ _context).getOrElse(Context.empty)
  def rules : List[PartialFunction[Term, Term]] = sstate.map(s => _rules ::: semanticParent.map(_.rules).getOrElse(Nil)).getOrElse(Nil)
  def addToContext(ctx : Context) = _context = _context ++ ctx
  def addRule(r : PartialFunction[Term, Term]) = _rules ::= r
}

trait HasNotation extends OMDocHTML {
  var arity : String = ""
  var fragment : String = ""
  var precedence : String = ""
  var notation : Option[HTMLNode] = None
}

trait HasType extends OMDocHTML {
  var tp : Option[Term] = None
}

trait HasDefiniens extends OMDocHTML {
  var df : Option[Term] = None
}

trait HasTermArgs extends OMDocHTML {
  protected var _terms : List[Term] = Nil
  def addArg(t : Term) = _terms ::= t
  def getArgs = _terms.reverse
}

trait HasLanguage extends OMDocHTML {
  var language : String = ""
}

class HTMLDocument(val path : DPath,orig : HTMLParser.HTMLNode) extends OMDocHTML(orig) with OMDocParent {
  val doc = new Document(path)
  sourceref.foreach(s => SourceRef.update(doc,s))
  sstate.foreach { state =>
    state.controller.library.add(doc)
  }
  override def onAdd: Unit = sstate.foreach { state =>
    state.controller.library.endAdd(doc)
  }
}

trait HTMLModule extends OMDocParent {
  def path = Path.parseM(resource,NamespaceMap.empty)
  def name = path.name
  def dpath = path.parent
}

class HTMLTheory(orig : HTMLParser.HTMLNode) extends OMDocHTML(orig) with HTMLModule with HasLanguage {
  var signature : String = ""
  var metatheory : Option[MPath] = None
  _context = _context ++ Context(path)

  var signature_theory : Option[Theory] = None
  var language_theory : Option[Theory] = None

  def open = sstate.foreach { state =>
    if (signature == "") {
      val th = Theory(path.parent, path.name, metatheory) // TODO parameters
      sourceref.foreach(s => SourceRef.update(th,s))
      signature_theory = Some(th)
      state.controller.add(th)
      if (language != "") {
        val lang = Theory(path.parent / path.name,LocalName(language),metatheory)
        lang.metadata.update(STeX.meta_language,STeX.StringLiterals(language))
        sourceref.foreach(s => SourceRef.update(lang,s))
        language_theory = Some(lang)
        state.controller.add(lang)
        val incl = PlainInclude(path,lang.path)
        state.controller.add(incl)
        state.controller.endAdd(incl)
        _context = _context ++ Context(lang.path)
      }
    } else if (language == signature) {
      val sig = Theory(path.parent, path.name, metatheory) // TODO parameters
      sourceref.foreach(s => SourceRef.update(sig,s))
      signature_theory = Some(sig)
      state.controller.add(sig)
      val lang = Theory(path.parent / path.name,LocalName(language),metatheory)
      lang.metadata.update(STeX.meta_language,STeX.StringLiterals(language))
      sourceref.foreach(s => SourceRef.update(lang,s))
      language_theory = Some(lang)
      state.controller.add(lang)
      val incl = PlainInclude(sig.path,lang.path)
      state.controller.add(incl)
      state.controller.endAdd(incl)
      _context = _context ++ Context(lang.path)
    } else {
      val lang = Theory(path.parent / path.name,LocalName(language),metatheory)
      lang.metadata.update(STeX.meta_language,STeX.StringLiterals(language))
      sourceref.foreach(s => SourceRef.update(lang,s))
      language_theory = Some(lang)
      state.controller.add(lang)
      val incl = PlainInclude(path,lang.path)
      state.controller.add(incl)
      state.controller.endAdd(incl)
      _context = _context ++ Context(lang.path)
    }
    print("")
    (signature_theory.toList ::: language_theory.toList).foreach {t =>
      semanticParent match {
        case Some(st : HTMLDocument) => state.controller.add(MRef(st.path,t.path))
        case _ =>
          state.controller.add(MRef(state.doc.path,t.path))
      }
    }
  }

  override def onAdd = sstate.foreach { state =>
    signature_theory.foreach(state.controller.endAdd)
    language_theory.foreach(state.controller.endAdd)
  }
}

class HTMLTheoryHeader(orig : HTMLParser.HTMLNode) extends OMDocHTML(orig) {
  override protected def onAdd: Unit = collectAncestor {
      case th : HTMLTheory => th.open
      case th : HTMLDerived =>
        th.open
  }
}

class HTMLDeclaration(orig : HTMLParser.HTMLNode) extends OMDocHTML(orig)

class HTMLConstant(orig : HTMLParser.HTMLNode) extends HTMLDeclaration(orig) with HasType with HasDefiniens {
  def path = Path.parseS(resource,NamespaceMap.empty)
  var role = ""
  var macroname = ""
}

class HTMLSymbol(orig : HTMLParser.HTMLNode) extends HTMLConstant(orig) with HasNotation {
  override def onAdd: Unit = sstate.foreach { state =>
    collectAncestor {
      case th : HTMLTheory =>
        if (th.signature_theory.isDefined) {
          val c = Constant(th.signature_theory.get.toTerm,path.name,Nil,tp.map(state.applyTopLevelTerm),df.map(state.applyTopLevelTerm),if (role == "") None else Some(role))
          if (arity != "") c.metadata.update(STeX.meta_arity,STeX.StringLiterals(arity))
          sourceref.foreach(s => SourceRef.update(c,s))
          state.controller.add(c)
          state.check(c)
        }
      case p : HTMLDerived =>
        val c = Constant(OMMOD(p.path),path.name,Nil,tp.map(state.applyTopLevelTerm),df.map(state.applyTopLevelTerm),if (role == "") None else Some(role))
        if (arity != "") c.metadata.update(STeX.meta_arity,STeX.StringLiterals(arity))
        sourceref.foreach(s => SourceRef.update(c,s))
        state.controller.add(c)
        state.check(c)
      case p : OMDocParent =>
        print("")
    }
  }
}
class HTMLNotation(orig : HTMLParser.HTMLNode) extends HTMLConstant(orig) with HasNotation {
  override def onAdd: Unit = sstate.foreach { state =>
    collectAncestor {
      case t: HTMLTheory =>
        notation.foreach { not =>
          val th = t.signature_theory match {
            case Some(th) => Some(th)
            case _ => t.language_theory
          }
          th.foreach { th =>
            val tp = STeX.notation.tp(path, arity)
            val df = STeX.notation(not.node,precedence,fragment)
            val c = Constant(th.toTerm,state.newName("notation"),Nil,Some(tp),Some(df),Some("notation"))
            sourceref.foreach(s => SourceRef.update(c,s))
            state.controller.add(c)
          }
        }
      case p : OMDocParent =>
        print("")
    }
  }
}

class HTMLStructure(orig : HTMLParser.HTMLNode) extends HTMLDeclaration(orig) with OMDocParent {
  private var _name : String = "" // TODO
  def domain = Path.parseM(resource)
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
class HTMLImport(orig : HTMLParser.HTMLNode) extends HTMLStructure(orig) {
  sstate.foreach { state =>
    collectAncestor {
      case mtc: MetatheoryComponent =>
        mtc.metatheory = Some(domain)
      case t: HTMLTheory =>
        t.signature_theory.foreach { th =>
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

  /*
  val feature = property.split(':') match {
    case Array("stex","feature",feat) => "stex:" + feat
    case _ =>
      ???
  }
  val decpath = Path.parseS(resource + "-feature")
  override def path = decpath.toMPath
  override def getElement(implicit state: SemanticParsingState): List[DerivedDeclaration] = _parent match {
    case Some(_ : ModuleAnnotation) =>
      val dd = new DerivedDeclaration(OMMOD(decpath.module),decpath.name,feature,TermContainer.asAnalyzed(_types.headOption.map(state.applyTopLevelTerm)),NotationContainer.empty())
      List(dd)
    case _ =>
    ???
  }
   */
}

class HTMLMMTRule(orig : HTMLParser.HTMLNode) extends HTMLDeclaration(orig) {
  lazy val path = Path.parseM(resource)
  def sortArgs : List[Term] = children.flatMap(getargs).sortBy(_._1).map(_._2)

  private def getargs(top : HTMLNode) : List[(Int,Term)] = top match {
    case n if n.attributes.contains((HTMLParser.ns_stex,"arg")) =>
      List((n.attributes((HTMLParser.ns_stex,"arg")).toInt,HTMLTerm(n)))
    case n if n.attributes.contains((n.namespace,"property")) && n.attributes((n.namespace,"property")) == "stex:arg" =>
      if(n.children.length == 1 && !n.children.head.isInstanceOf[HTMLText])
        List((n.attributes((n.namespace,"resource")).toInt,HTMLTerm(n.children.head)))
      else List((n.attributes((n.namespace,"resource")).toInt,HTMLTerm(n)))
    case n : MathMLTerm =>
      n.children.flatMap(getargs)
    case n : HTMLTerm =>
      Nil
    case t : HTMLText =>
      Nil
    case _ =>
      Nil
  }
  override protected def onAdd: Unit = sstate.foreach { state =>
    collectAncestor {
      case t: HTMLTheory =>
        t.signature_theory.foreach { th =>
          val rc = state.rci(th.path,OMAorAny(OMID(path),sortArgs.map(state.applyTopLevelTerm)),true)
          sourceref.foreach(s => SourceRef.update(rc,s))
          state.controller.add(rc)
        }
      case p : OMDocParent =>
        print("")
    }
  }

  /*
  override def getElement(implicit state: SemanticParsingState): List[RuleConstant] = _parent match {
    case Some(pt : TheoryAnnotation) =>
      List(rci(pt.path,OMAorAny(OMID(Path.parseM(resource)),getArgs.map(state.applyTopLevelTerm)),true))
  }

   */
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
class HTMLArity(orig : HTMLParser.HTMLNode) extends OMDocComponent(orig) {
  override def onAdd: Unit = if (resource != "") parent match {
    case Some(s : HasNotation) =>
      s.arity = resource
    case _ =>
      print("")
  }
}
class HTMLNotationFragment(orig : HTMLParser.HTMLNode) extends OMDocComponent(orig) {
  override def onAdd: Unit = if (resource != "") parent match {
    case Some(s : HasNotation) =>
      s.fragment = resource
    case _ =>
      print("")
  }
}
class HTMLNotationPrec(orig : HTMLParser.HTMLNode) extends OMDocComponent(orig) {
  override def onAdd: Unit = if (resource != "") parent match {
    case Some(s : HasNotation) =>
      s.precedence = resource
    case _ =>
      print("")
  }
}
class HTMLNotationComponent(orig : HTMLParser.HTMLNode) extends OMDocComponent(orig) {
  var notation : Option[HTMLParser.HTMLNode] = None
  override def onAdd: Unit = notation.foreach{not => collectAncestor {
    case c : HasNotation =>
      c.notation = Some(not)
  }}
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
class LanguageComponent(orig : HTMLParser.HTMLNode) extends OMDocComponent(orig) {
  collectAncestor {
    case hl : HasLanguage => hl.language = resource
  }
}
class SignatureComponent(orig : HTMLParser.HTMLNode) extends OMDocComponent(orig) {
  collectAncestor {
    case th : HTMLTheory => th.signature = resource
  }
}

object HTMLTerm {
  def apply(xn : HTMLNode) : Term = xn match {
    case ml : MathMLTerm if xn.label == "math" && xn.children.length == 1 =>
      apply(xn.children.head)
    case ml : MathMLTerm =>
      ml.children match {
        case List(t : HTMLText) =>
          STeX.informal.applySimple(ml.node)
        case _ =>
          val args = ml.children.map(apply)
          STeX.informal.applyOp(ml.label,args)
      }
    case t : HTMLTerm =>
      t.toTerm
    case o if o.label == "span" && o.children.length == 1 =>
      apply(o.children.head)
    case _ =>
      print("")
      ???
  }
    /* {
    case List(a) if is(a) =>
      a.getAnnotations.collectFirst{case ta : TermAnnotation => ta.toTerm(state)}.get
    case List(t : XHTMLText) =>
      //xn.strip
      STeX.informal.applySimple(xn.node)
    case _ =>
      val args = xn.children.flatMap({
        case c if c.isEmpty => None
        case a if is(a) =>
          a.getAnnotations.collectFirst{case ta : TermAnnotation => ta.toTerm(state)}
        case t: XHTMLText =>
          t.strip
          Some(STeX.informal.applySimple(<mi>{t.node}</mi>))
        case o =>
          Some(getUnique(o,state))
      })
      STeX.informal.applyOp(xn.label, args)
  } */

  def getUnique(xn : HTMLNode) : Term = xn.children.filterNot(_.isEmpty) match {
    case _ =>
      print("")
      ???
  } /*{
    case List(t) if is(t) =>
      t.getAnnotations.collectFirst{case ta : TermAnnotation => ta.toTerm(state)}.get
    case List(t) => getUnique(t,state)
    case _ =>
      ???
  } */
}

class HTMLTerm(orig : HTMLParser.HTMLNode) extends OMDocHTML(orig) {
  def toTerm : Term = HTMLTerm(this) // XHTMLTerm(node,state)
  def isTop = collectAncestor{case _ : HTMLTerm | _ : HTMLTermComponent => false}.getOrElse(true)

  override def onAdd: Unit = sstate.foreach { _ =>
    collectAncestor { case o: OMDocHTML => o } match {
      case Some(nc : HTMLNotationComponent) => // this should be <ml:math>-node
        children match {
          case List(c: HTMLTerm) =>
            nc.notation = Some(c)
          case _ =>
            print("")
            ???
        }
      case Some(c : HasTermArgs) =>
        val tm = toTerm
        sourceref.foreach(s => SourceRef.update(tm,s))
        c.addArg(tm)
      case Some(c: HTMLTermComponent) =>
        c.term = Some(toTerm)
        c.term.foreach(tm => sourceref.foreach(s => SourceRef.update(tm,s)))
      case Some(t: HTMLTerm) =>
      case x =>
        print("")
    }
  }
  override def isEmpty = false
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

class HTMLOMID(orig : HTMLParser.HTMLNode) extends HTMLTerm(orig) with HasHeadSymbol {
  override def toTerm: Term = {
    val tm = OMID(head)
    if (fragment != "") tm.metadata.update(STeX.meta_notation,STeX.StringLiterals(fragment))
    tm
  }
}

trait ComplexTerm extends HTMLTerm with HasHeadSymbol {
  def sortArgs : List[Term] ={
    val args = children.flatMap(getargs)
    arity.zipWithIndex.map {
      case ('i'|'b',i) =>
        args.filter(_._1 == i+1) match {
          case List(t) =>
            t._2
          case _ =>
            print("")
            ???
        }
      case ('a',i) =>
        STeX.flatseq(args.filter(_._1 == i+1).map(_._2):_*)
    }.toList
  }

  private def getargs(top : HTMLNode) : List[(Int,Term)] = top match {
    case n if n.attributes.contains((HTMLParser.ns_stex,"arg")) =>
      List((n.attributes((HTMLParser.ns_stex,"arg")).toInt,HTMLTerm(n)))
    case n if n.attributes.contains((n.namespace,"property")) && n.attributes((n.namespace,"property")) == "stex:arg" =>
      if(n.children.length == 1 && !n.children.head.isInstanceOf[HTMLText])
        List((n.attributes((n.namespace,"resource")).toInt,HTMLTerm(n.children.head)))
      else List((n.attributes((n.namespace,"resource")).toInt,HTMLTerm(n)))
    case n : MathMLTerm =>
      n.children.flatMap(getargs)
    case n : HTMLTerm =>
      Nil
    case t : HTMLText =>
      Nil
    case _ =>
      Nil
  }
}

class HTMLOMA(orig : HTMLParser.HTMLNode) extends HTMLTerm(orig) with ComplexTerm {
  override def toTerm: Term = {
    val tm = OMA(OMID(head),sortArgs)
    if (fragment != "") tm.metadata.update(STeX.meta_notation,STeX.StringLiterals(fragment))
    tm
  }
}
class HTMLOMBIND(orig : HTMLParser.HTMLNode) extends HTMLTerm(orig) with ComplexTerm {

  override def toTerm = {
    // TODO
    var args : List[Term] = Nil
    var ctx = Context.empty
    sortArgs.zip(arity).foreach {
      case (tm,'b') =>
        ctx = ctx ++ sstate.get.makeBinder(tm)
      case (tm,_) => args ::= tm
    }
    val tm = OMBINDC(OMID(head),ctx,args) //OMBINDC(OMID(head),ctx,args)
    if (fragment != "") tm.metadata.update(STeX.meta_notation,STeX.StringLiterals(fragment))
    tm
  }
}
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
        (fragment,prec,arity,HTMLParser(node)(new ParsingState(controller,server.extensions.flatMap(_.rules))))
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
  private var _signaturechildren : List[OMDocHTML] = Nil
  def addSignature(child : OMDocHTML) = if (signature == "" || language == signature) _signaturechildren ::= child
  def addLanguage(child : OMDocHTML) = {
    add(child)
  }
  _context = _context ++ Context(path)

  var signature_theory : Option[Theory] = None
  var language_theory : Option[Theory] = None

  def open = sstate.foreach { state =>
    if (signature == "") {
      val th = Theory(path.parent, path.name, metatheory) // TODO parameters
      sourceref.foreach(s => SourceRef.update(th,s))
      signature_theory = Some(th)
      state.controller.add(th)
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
    case n : MathMLTerm =>
      n.children.flatMap(getargs)
    case n : HTMLTerm =>
      Nil
    case t : HTMLText =>
      Nil
    case _ =>
      print("")
      ???
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
          print("")
          ???
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
    case n : MathMLTerm =>
      n.children.flatMap(getargs)
    case n : HTMLTerm =>
      Nil
    case t : HTMLText =>
      Nil
    case _ =>
      print("")
      ???
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

// ---------------------------------------------------------------------------------------------------------------------

/*
object XHTMLOMDoc {
  def notation(parse : String = "", present : String = "", verbalize : String = "") = {
    val nc = new NotationContainer
    if (parse != "") nc.parsingDim.set(TextNotation.parse(parse,NamespaceMap.empty))
    if (present != "") nc.presentationDim.set(TextNotation.parse(present,NamespaceMap.empty))
    if (verbalize != "") nc.verbalizationDim.set(TextNotation.parse(verbalize,NamespaceMap.empty))
    nc
  }

  def toRuleC(property : String => Boolean, resource : Option[String => Boolean] = None)(f : (XHTMLNode,XHTMLParsingState) => Unit) : PartialFunction[(XHTMLNode,XHTMLParsingState), Unit] = {
    case (n,s) if property(n.prefix + ":" + n.label) && resource.forall(r => n.attributes.get(("","resource")).exists(r)) => f(n,s)
    case (n,s) if property(n.label) && resource.forall(r => n.attributes.get(("","resource")).exists(r)) => f(n,s)
    case (n,s) if n.attributes.get(("","property")).exists(property) && resource.forall(r => n.attributes.get(("","resource")).exists(r)) => f(n,s)
    case _ =>
  }

  def toRule(property : String, resource : Option[String => Boolean] = None)(f : (XHTMLNode,XHTMLParsingState) => Unit) : PartialFunction[(XHTMLNode,XHTMLParsingState), Unit] = {
    case (n,s) if n.prefix + ":" + n.label == property && resource.forall(r => n.attributes.get(("","resource")).exists(r)) => f(n,s)
    case (n,s) if n.label == property && resource.forall(r => n.attributes.get(("","resource")).exists(r)) => f(n,s)
    case (n,s) if n.attributes.get(("","property")).contains(property) && resource.forall(r => n.attributes.get(("","resource")).exists(r)) => f(n,s)
    case (n,s) if n.classes.exists(_.startsWith(property)) =>
      val cls = n.classes.find(_.startsWith(property)).get
      n.removeClass(cls)
      cls.split('_') match {
        case Array(p,r) =>
          n.attributes(("","property")) = p
          n.attributes(("","resource")) = r
          f(n,s)
        case Array(p) =>
          n.attributes(("","property")) = p
          f(n,s)
        case _ =>
          print("")
      }
    case _ =>
  }
}

class OMDocAnnotation(node : XHTMLNode) extends XHTMLAnnotation(node) {
  import node._
  val (property,resource) =  if (prefix == "stex")
    (label,attributes.getOrElse(("","resource"),""))
  else (attributes.getOrElse(("","property"),""),attributes.getOrElse(("","resource"),""))
}

trait PreElement {
  type A <: PreElement
  def getElement(implicit state : SemanticParsingState) : List[StructuralElement]
  def subelements : List[A] = List(this.asInstanceOf[A])
  //def path : Path
  protected var _parent : Option[PreParent] = None
  val metadata = mutable.Map.empty[GlobalName,Obj]
  metadata(STeX.meta_source) = STeX.StringLiterals("sTeX")
  protected def findUp(pe : PreElement => Boolean) : Option[PreElement] = if (pe(this)) Some(this) else _parent.flatMap(_.findUp(pe))
  def find(pe : PreElement => Boolean) : Option[PreElement] = findUp(pe)
  def open(state : SemanticParsingState) : Unit = state.getParent match {
    case p : PreParent =>
      p.add(this)
      _parent = Some(p)
    case _ =>
  }
  def close(state : SemanticParsingState) : Unit = {}
}





trait PreParent extends PreElement {
/*
  override def find(pe: PreElement => Boolean): Option[PreElement] = if (pe(this)) Some(this) else {
    _children.find(_.find(pe).isDefined) match {
      case Some(c) => Some(c)
      case _ =>
        _parent.flatMap(_.findUp(pe))
    }
  }
 */

  protected var _children : List[PreElement] = Nil
  def children = _children.reverse
  def add(child : PreElement) = _children ::= child
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

object SourceRefAnnotation {
  def toOffset(f : File, line : Int, col : Int) : SourcePosition = {
    var (o,l,c) = (0,1,0)
    File.read(f).foreach {
      case _ if l > line =>
        return SourcePosition(o,line,col)
      case _ if l == line && col == c =>
        return SourcePosition(o,line,col)
      case '\n' =>
        c = 0
        l += 1
        o += 1
      case _ =>
        o += 1
        c += 1
    }
    SourcePosition(-1,line,col)
  }
  def toSourceRef(controller : Controller,sra : SourceRefAnnotation) = {
    val file = controller.backend.resolvePhysical(File(sra.file)).map { case (archive, path) =>
      path.foldLeft(archive.narrationBase)((u, s) => u / s)
    }.getOrElse {
      URI(File(sra.file).toURI)
    }
    SourceRef(file, SourceRegion(toOffset(File(sra.file), sra.from._1, sra.from._2), toOffset(File(sra.file), sra.to._1, sra.to._2)))
  }
}

class SourceRefAnnotation(node: XHTMLNode) extends XHTMLAnnotation(node) {
  override val isEmpty: Boolean = true
  var file : String = ""
  var from : (Int,Int) = (0,0)
  var to : (Int,Int) = (0,0)
  def setvars = {
    val str = if (node.attributes.get(("","property")).contains("srcref")) {
      node.attributes(("","resource"))
    } else if (node.attributes.contains(("stex","srcref"))) {node.attributes(("stex","srcref"))}
    else ""
    str.split('#') match {
      case Array(f,r) =>
        file = f
        r.drop(1).dropRight(1).split(')') match {
          case Array(b,e) =>
            b.split(';') match {
              case Array(l,c) =>
                from = (l.toInt,c.toInt)
              case _ =>
                print("")
                ???
            }
            e.drop(1).split(';') match {
              case Array(l,c) =>
                to = (l.toInt,c.toInt)
              case _ =>
                print("")
                ???
            }
          case _ =>
            print("")
            ???
        }
      case _ =>
        print("")
        ???
    }
  }
  override def afterPopulating(s : XHTMLParsingState): Unit = if (node.attributes.get(("","property")).contains("srcref")) {
    node.delete
    node.parent.foreach {p =>
      p.addAnnotation(this)
      p.attributes(("stex","srcref")) = node.attributes.getOrElse(("","resource"),"")
      val n = new SourceRefAnnotation(p)
      n.setvars
    }
  } else setvars
}

object InScript {
  def apply(node : XHTMLNode) = node.collectFirstAncestor{case n if n.getAnnotations.exists(_.isInstanceOf[ToScript]) => n}.isDefined
}

trait Plain extends OMDocAnnotation {
  override def afterPopulating(s : XHTMLParsingState): Unit = {
    super.afterPopulating(s)
    node.children.foreach(_.delete)
    if (node.label == "span") node.add(XHTML.empty)
  }
}

trait ModuleAnnotation extends OMDocAnnotation with PreParent {
  def path = Path.parseM(resource,NamespaceMap.empty)
  def name = path.name
  def dpath = path.parent
}

class TheoryAnnotation(node : XHTMLNode) extends OMDocAnnotation(node) with ModuleAnnotation {

  override type A = PreTheory
  val pt = new PreTheory(node,path)
  def addMeta(mt : Option[MPath]) = {
    languagemodule.foreach(_.metatheory = mt)
    signaturemodule.foreach(_.metatheory = mt)
  }
  var languagemodule : Option[PreTheory] = Some(pt)
  var signaturemodule : Option[PreTheory] = Some(pt)

  override final def subelements: List[A] = List(signaturemodule.toList,languagemodule.toList).flatten.distinct

  override def add(child: PreElement): Unit = pt.add(child)

  // TODO parameters
  override def getElement(implicit state : SemanticParsingState): List[Theory] = {
    ???
  }//state.applySE(Theory(path.doc,path.name,metatheory))
}

class PreTheory(node : XHTMLNode, override val path : MPath) extends OMDocAnnotation(node) with ModuleAnnotation {
  private[xhtml] var metatheory : Option[MPath] = Some(STeX.meta)
  var language = ""

  override def open(state: SemanticParsingState): Unit = {}
  override def close(state: SemanticParsingState): Unit = {}

  override def getElement(implicit state : SemanticParsingState): List[Theory] = {
    if (language != "") metadata(STeX.meta_language) = OMS(STeX.language(language))
    List(Theory(path.doc,path.name,metatheory))
  } //state.applySE(Theory(path.doc,path.name,metatheory))
  override def init: Unit = {}
}

trait ToScript extends OMDocAnnotation {
  import node._
  private val srcref = attributes.get(("stex","srcref"))
  attributes.clear()
  label = "script"
  attributes(("","type")) = "application/xml"
  attributes(("","property")) = property
  attributes(("","resource")) = resource
  srcref.foreach{attributes(("stex","srcref")) = _}
  classes.foreach(removeClass)

  override def afterPopulating(s : XHTMLParsingState): Unit = {
    super.afterPopulating(s)
    if (children.isEmpty) add(XHTML.empty)
    get()()().flatMap(_.getAnnotations).foreach {
      case cmp : ComponentAnnotation => cmp.makeScript
      case _ =>
    }
  }
}

abstract class DeclarationAnnotation(node : XHTMLNode) extends OMDocAnnotation(node) with PreElement {
  override type A = DeclarationAnnotation
  /*
  def path = Path.parseS(resource,NamespaceMap.empty)
  def module = path.module
  def name = path.name
  def dpath = module.parent
   */
}

class DerivedAnnotation(node : XHTMLNode) extends DeclarationAnnotation(node) with ModuleAnnotation with HasType with HasDefiniens {
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
}

class ConstantAnnotation(node : XHTMLNode) extends DeclarationAnnotation(node) with HasNotation with HasType with HasDefiniens {
  def path = Path.parseS(resource,NamespaceMap.empty)

  private var _roles : List[String] = Nil
  private var _macronames : List[String] = Nil
  def addMacro(s : String) = _macronames ::= s
  def addRole(s : String) = _roles ::= s
  def getElement(implicit state : SemanticParsingState) : List[Declaration] = {
    val c = Constant(OMID(path.module),path.name,Nil,
      _types.headOption.map(state.applyTopLevelTerm),
      _definientia.headOption.map(state.applyTopLevelTerm),
      _roles.headOption,NotationContainer.empty())
    _macronames.headOption.foreach{ m =>
      val (k,v) = PreElement.makeMacroName(m)
      metadata(k) = v
    }
    metadata(STeX.meta_arity) = STeX.StringLiterals(arity)
    List(c)
  }
}

class StructureAnnotation(node : XHTMLNode) extends DeclarationAnnotation(node) with PreParent {
  var _name : String = ""
  lazy val domain = Path.parseM(resource)
  lazy val name = if (_name.nonEmpty) LocalName(_name) else LocalName(domain)
  var _nonEmpty = false
  def empty = (!_nonEmpty && _children.isEmpty)
  lazy val path = _parent match {
    case Some(p : ModuleAnnotation) => p.path ? name
    case _ =>
      print("")
      ???
  }

  // TODO structures, views, ...
  override def getElement(implicit state : SemanticParsingState) = List(
    if (empty) PlainInclude(domain,path.module) else {
      ???
    }
  )
}

class ComponentAnnotation(node : XHTMLNode) extends OMDocAnnotation(node) with PreElement {
  import node._
  def makeScript = {
    label = property
    attributes.clear
    attributes(("","resource")) = resource
  }

  override def getElement(implicit state: SemanticParsingState): List[StructuralElement] = ???
}

object TermComponentA {
  def clean(node : XHTMLNode) = node.get()()().collectFirst{
    case n if n.label == "math" =>
      n.children.foreach{case c if c.isEmpty => c.delete case _ =>}
      node.children.foreach(_.delete)
      n.children.headOption.foreach(node.add)
      n
  }.getOrElse {
    node.children.foreach{
      case n if !n.getAnnotations.exists(_.isInstanceOf[TermAnnotation]) => n.delete
      case _ =>
    }
    print("")
  }
}

class TermComponentA(node : XHTMLNode) extends ComponentAnnotation(node : XHTMLNode) {
  override def makeScript: Unit = {
    super.makeScript
    TermComponentA.clean(node)
  }
  def getTerm(state : SemanticParsingState) : Term = node.children.filterNot(_.isEmpty) match {
    case List(t) if XHTMLTerm.is(t) =>
      t.getAnnotations.collectFirst{case t : TermAnnotation => t.toTerm(state)}.get
    case List(t) =>
      XHTMLTerm.getUnique(t,state)
    case _ =>
      print("")
      ???
  }
}

/*

abstract class XHTMLTermComponent(initial_node : Option[Node] = None) extends XHTMLComponent(initial_node) {
  override def cleanup: Unit = {
    super.cleanup
    _children = semanticChildren.collectFirst {
      case t : XHTMLMath if t.children.length == 1 =>
        add(t.children.head)
        t.children.head
      case t : XHTMLTerm =>
        add(t)
        t
    }.toList
    getTerm
    print("")
  }
  def getTerm : Option[Term] = semanticChildren.collectFirst{
    case t : XHTMLTerm => t.toTerm
  }

  override def node = if (inscript) {
    Elem(null,property,XHTML.makeAttributes((("","resource"),resource)),scope,false,children.map(_.strip):_*)
  }else super.node
}

 */

class TypeComponentA(node : XHTMLNode) extends TermComponentA(node) {
  override def close(state: SemanticParsingState): Unit = {
    super.close(state)
    if (!node.children.forall(_.isEmpty)) {
      state.getParent match {
        case c: HasType =>
          c.addType(getTerm(state))
        case _ =>
          print("")
          ???
      }
    }
  }
}
class DefComponentA(node : XHTMLNode) extends TermComponentA(node) {
  override def close(state: SemanticParsingState): Unit = {
    super.close(state)
    if (!node.children.forall(_.isEmpty)) {
      state.getParent match {
        case c: HasDefiniens =>
          c.addDefiniens(getTerm(state))
        case _ =>
          print("")
          ???
      }
    }
  }
}
class ArityComponentA(node : XHTMLNode) extends ComponentAnnotation(node) with Plain {
  def arity = resource
  override def close(state: SemanticParsingState): Unit = {
    super.close(state)
    state.getParent match {
      case c : HasNotation =>
        c.arity = arity
      case _ =>
        print("")
        ???
    }
  }
}
class NotationFragmentComponentA(node : XHTMLNode) extends ComponentAnnotation(node) with Plain {
  def fragment = resource
  override def close(state: SemanticParsingState): Unit = {
    super.close(state)
    state.getParent match {
      case c : HasNotation =>
        c.fragment = fragment
      case _ =>
        print("")
        ???
    }
  }
}
class PrecedenceComponentA(node : XHTMLNode) extends ComponentAnnotation(node) with Plain {
  def prec = resource

  override def close(state: SemanticParsingState): Unit = {
    super.close(state)
    state.getParent match {
      case c : HasNotation =>
        c.precedence = prec
      case _ =>
        print("")
        ???
    }
  }
}

class NotationComponentA(node : XHTMLNode) extends TermComponentA(node) {
  override def afterPopulating(s : XHTMLParsingState): Unit = {
    TermComponentA.clean(node)
    super.afterPopulating(s)
    node.children match {
      case List(a) =>
        a.strip
        print("")
      case _ =>
        print("")
        ???
    }
  }

  override def close(state: SemanticParsingState): Unit = {
    super.close(state)
    state.getParent match {
      case c : HasNotation =>
        c.addNotation(c.fragment,c.precedence,node.children match {case List(a) => a.node case _ =>
          print("")
            ???
        })
      case _ =>
        print("")
        ???
    }
  }
}

class MacronameComponentA(node : XHTMLNode) extends ComponentAnnotation(node) with Plain {
  def macroname = resource

  override def close(state: SemanticParsingState): Unit = {
    super.close(state)
    state.getParent match {
      case c : ConstantAnnotation =>
        c.addMacro(macroname)
      case _ =>
        print("")
        ???
    }
  }
}

class ArgumentAnnotation(node : XHTMLNode) extends OMDocAnnotation(node) {
  import node._
  override val priority: Int = 5
  val num = if (attributes.get(("","property")).contains("stex:arg")) {
    attributes.remove(("","property"))
    val argstr = attributes.getOrElse(("","resource"),{
      print("")
      ???
    })
    attributes.remove(("","resource"))
    argstr.toInt
  } else if (classes.exists(_.startsWith("stex:arg"))) classes.collectFirst {
    case s if s.startsWith("stex:arg_") =>
      removeClass(s)
      val argstr = s.drop(9)
      argstr.toInt
  }.getOrElse {
    print("")
    ???
  } else if (attributes.contains(("stex","arg"))) attributes(("stex","arg")).toInt
  else {
    print("")
    ???
  }
  attributes(("stex","arg")) = num.toString

  override def afterPopulating(s : XHTMLParsingState): Unit = {
    super.afterPopulating(s)
    if (label == "span" && InScript(node)) {
      TermComponentA.clean(node)
      label = "math"
    }
  }

  def toTerm(state : SemanticParsingState) = node.getAnnotations.collectFirst{case t : TermAnnotation => t.toTerm(state)}.getOrElse(XHTMLTerm(node,state))
}


trait TermAnnotation extends XHTMLAnnotation with PreElement {
  def toTerm(state : SemanticParsingState) : Term = XHTMLTerm(node,state)
  def isTop = node.collectFirstAncestor{case a if XHTMLTerm.is(a) => false}.getOrElse(true)

  override def open(state: SemanticParsingState): Unit = {
    state.getParent match {
      case t : PreParent =>
        super.open(state)
      case t : HasTermArgs =>
        super.open(state)
      case _ =>
        print("")
    }
  }

  override def close(state : SemanticParsingState) = {
    state.getParent match {
      case tp : HasTermArgs =>
        val t = toTerm(state)
        tp.addArg(t)
      case p : PreParent =>
        print("") // TODO add to language module
      case _ =>
        print("")
    }
  }

  override def getElement(implicit state: SemanticParsingState): List[StructuralElement] = {
    print("")
    ???
  }
}


class MathMLAnnotation(override val node : XHTMLNode) extends XHTMLAnnotation(node) with TermAnnotation {
  override def toTerm(state : SemanticParsingState): Term = XHTMLTerm(node,state)

  //if (!node.attributes.contains("","xmlns")) node.attributes(("","xmlns")) = "http://www.w3.org/1998/Math/MathML"
}

case class MathMLLiteral(override val node : XHTMLNode) extends MathMLAnnotation(node) {
  override def toTerm(state : SemanticParsingState): Term = node.children.filterNot(_.isEmpty) match {
    case (t : XHTMLText) :: Nil =>
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
}

class OMIDAnnotation(node : XHTMLNode) extends OMDocAnnotation(node) with TermAnnotation with HasHeadSymbol {
  override val priority = 3

  override def toTerm(state : SemanticParsingState): Term = {
    val tm = OMID(head)
    if (fragment != "") tm.metadata.update(STeX.meta_notation,STeX.StringLiterals(fragment))
    tm
  }
}

trait ComplexTerm extends OMDocAnnotation with TermAnnotation with HasHeadSymbol {
  private def getargs(n : XHTMLNode,state : SemanticParsingState) : List[(Int,Term)] = n.getAnnotations.collectFirst {
    case a : ArgumentAnnotation => List((a.num,a.toTerm(state)))
  }.getOrElse(n.children.flatMap(getargs(_,state)))

  def sortArgs(state : SemanticParsingState) = {
    val args = if (node.getAnnotations.exists(_.isInstanceOf[ArgumentAnnotation]))
      node.children.flatMap(getargs(_,state))
    else getargs(node,state)
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
      case ('b',i) =>
        // TODO
        ???
    }.toList
  }
}

class OMAAnnotation(node : XHTMLNode) extends OMDocAnnotation(node) with ComplexTerm {
  override val priority = 3

  override def toTerm(state : SemanticParsingState): Term = {
    val tm = OMA(OMID(head),sortArgs(state))
    if (fragment != "") tm.metadata.update(STeX.meta_notation,STeX.StringLiterals(fragment))
    tm
  }
}
class OMBINDAnnotation(node : XHTMLNode) extends OMDocAnnotation(node) with ComplexTerm {
  override val priority = 3

  override def toTerm(state : SemanticParsingState) = {
    // TODO
    var args : List[Term] = Nil
    var ctx : List[Term] = Nil
    sortArgs(state).zip(arity).foreach {
      case (tm,'b') =>
        ctx ::= tm //state.makeBinder(tm)
      case (tm,_) => args ::= tm
    }
    val ctxtm = STeX.flatseq(ctx.reverse:_*)
    val tm = OMA(OMID(head),ctxtm :: args) //OMBINDC(OMID(head),ctx,args)
    state.markBinder(tm)
    if (fragment != "") tm.metadata.update(STeX.meta_notation,STeX.StringLiterals(fragment))
    tm
  }
}

object XHTMLTerm {
  def is(node : XHTMLNode) = node.getAnnotations.exists(_.isInstanceOf[TermAnnotation])

  def apply(xn : XHTMLNode,state : SemanticParsingState) : Term = xn.children.filterNot(_.isEmpty) match {
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
  }

  def getUnique(xn : XHTMLNode,state : SemanticParsingState) : Term = xn.children.filterNot(_.isEmpty) match {
    case List(t) if is(t) =>
      t.getAnnotations.collectFirst{case ta : TermAnnotation => ta.toTerm(state)}.get
    case List(t) => getUnique(t,state)
    case _ =>
      ???
  }

}

/*

class XHTMLMath(initial_node : Option[Node] = None) extends XHTMLTerm(initial_node) {
  private def cond(n : XHTMLNode) = n.getClass == classOf[XHTMLNode]
  override def cleanup: Unit = children match {
    case List(e) if cond(e) && e.label == "semantics" =>
      children.foreach(_.delete)
      e.children match {
        case List(a) =>
          a.delete
          add(a)
        case List(a,b) if cond(b) && b.label == "annotation-xml" =>
          a.delete
          add(a)
        case _ =>
          e.children.foreach{c => c.delete; add(c)}
      }
    case _ =>
  }
  // attributes(("","xmlns")) = "http://www.w3.org/1998/Math/MathML"

  override val ismath = true

  override def toTermI : Term = children match {
    case List(tm: XHTMLTerm) =>
      tm.toTerm
    case _ =>
      ???
  }

}

object XHTMLComplexTerm {
  def args(n : XHTMLOMDoc) = n.semanticChildren.collect {
    case STeXArg(n,t) =>
      (n,t)
  }.sortBy(_._1).map(_._2)
}

abstract class XHTMLComplexTerm(initial_node : Option[Node] = None) extends XHTMLTerm(initial_node) with HasHeadSymbol {
  sealed trait SubElem {val elem : XHTMLNode}
  case class TextElem(elem : XHTMLNode) extends SubElem
  case class SemanticElem(elem : XHTMLNode) extends SubElem

  lazy val argstr = children.collectFirst{
    case ac : XHTMLArityComponent =>
      ac.delete
      ac.arity
  }

  lazy val (ps,arity,frag) = {
    resource.split('#') match {
      case Array(p,a,f) => (p,a,f)
      case Array(l) => (l,"","")
      case Array(p,a) => (p,a,"")
      case _ =>
        println(resource.split('#').mkString("Array(",",",")"))
        ???
    }
  }
  lazy val head = Path.parseMS(ps,NamespaceMap.empty)

  def args = XHTMLComplexTerm.args(this)
/*
  def args = semanticChildren.collect({case a : XHTMLStexArg => a}).map{arg =>
    arg.semanticChildren.collect {
      case t : XHTMLTerm => t
    } match {
      case List(a) =>
        (arg.number,a.toTerm)
      case _ =>
        (arg.number,XMathML.toTermI(arg))
    }
  }.sortBy(_._1).map(_._2)

 */

  override def node: Node = {
    argstr
    super.node
  }
}
class XHTMLOMA(initial_node : Option[Node] = None) extends XHTMLComplexTerm(initial_node) {

  def toTermI = {
    val t = OMA(OMID(head),args)
    t.metadata.update(STeX.meta_notation,STeX.StringLiterals(frag))
    t
  }
}

class XHTMLOMBind(initial_node : Option[Node] = None) extends XHTMLComplexTerm(initial_node) {

  def toTermI = {
    val t = OMA(OMID(head),args)
    t.metadata.update(STeX.meta_notation,STeX.StringLiterals(frag))
    t
  }
}

class XHTMLOMV(initial_node : Option[Node] = None) extends XHTMLTerm(initial_node) {
  lazy val path = Path.parseS(resource)
  def name = path.name
  def toTermI = {
    val t = OMV(name)
    t.metadata.update(STeX.meta_vardecl,OMS(path))
    t
  }
}

class XHTMLOMID(initial_node : Option[Node] = None) extends XHTMLTerm(initial_node) with HasHeadSymbol {
  lazy val (ps,frag) = {
    resource.split('#') match {
      case Array(p,f) => (p,f)
      case Array(l) => (l,"")
      case _ =>
        ???
    }
  }
  lazy val head = Path.parseMS(ps,NamespaceMap.empty)
  def toTermI = {
    val t = OMID(head)
    t.metadata.update(STeX.meta_notation,STeX.StringLiterals(frag))
    t
  }
}


class XHTMLOMNum(initial_node : Option[Node] = None) extends XHTMLTerm(initial_node) {
  override def toTermI = children match {
    case (t : XHTMLText) :: Nil =>
      t.text.toDoubleOption match {
        // case Some(db) if db.isValidInt && db>0 => STeX.PosLiterals(BigInt(db.toInt))
        case Some(db) if db.isValidInt && db>=0 => STeX.NatLiterals(BigInt(db.toInt))
        case Some(db) if db.isValidInt => STeX.IntLiterals(BigInt(db.toInt))
        case Some(db) => STeX.RealLiterals(db)
        case _ =>
          ???
      }
    case _ =>
      ???
  }

  override def cleanup: Unit = {}
}

object XMathML {
  def toTermI(xn : XHTMLNode) : Term = xn.children match {
    case List(a : XHTMLTerm) =>
      a.toTerm
    case _ =>
      val args = xn.children.map({
        case m: XHTMLTerm => m.toTerm
        case t: XHTMLText if xn.children == List(t) =>
          return (STeX.informal.applySimple(xn.strip))
        case t: XHTMLText =>
          STeX.informal.applySimple(<mi>t.strip</mi>)
        case _ =>
          ???
      })
      STeX.informal.applyOp(xn.label, args)
  }
}

class XMathML(initial_node : Option[Node] = None) extends XHTMLTerm(initial_node) {
  override def isEmpty : Boolean = label != "mspace" && (_children.isEmpty || _children.forall(_.isEmpty))
  override def toTermI: Term = XMathML.toTermI(this)

  override def strip: Node = if (label == "mspace") {
    val width = attributes.get(("","width"))
    <mspace width={width.getOrElse("0pt")}/>
  } else super.strip
}

// TODO deprecate
class XHTMLTref(initial_node : Option[Node] = None) extends XHTMLOMDoc(initial_node) with HasHeadSymbol {
  var head = Path.parseMS(resource,NamespaceMap.empty)
}

 */
 */
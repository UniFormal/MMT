package info.kwarc.mmt.stex

import info.kwarc.mmt.api.informal.{MathMLNarration, Narration}
import info.kwarc.mmt.api.modules.Theory
import info.kwarc.mmt.api.notations.{Delim, Delimiter, NotationContainer}
import info.kwarc.mmt.api.objects.{ComplexTerm, Context, OMA, OMATTR, OMFOREIGN, OMID, OMLIT, OMLITTrait, OMS, Obj, Position, StatelessTraverser, Term, Traverser}
import info.kwarc.mmt.api.ontology.{AlignmentsServer, ConceptReference, LogicalReference, PhysicalReference}
import info.kwarc.mmt.api.parser.ParseResult
import info.kwarc.mmt.api.presentation.HTMLAttributes.toggleTarget
import info.kwarc.mmt.api.{AbstractObjectContainer, CPath, ComponentKey, ContentPath, DPath, DeclarationComponent, GlobalName, MPath, NotationComponentKey, StructuralElement, metadata, ontology, presentation}
import info.kwarc.mmt.api.presentation.{ContentMathMLPresenter, HTMLAttributes, HTMLPresenter, PresentationContext, RenderingHandler}
import info.kwarc.mmt.api.symbols.{Constant, Declaration, Include, Structure}
import info.kwarc.mmt.api.utils.{HTML, mmt, xml}
import info.kwarc.mmt.api.utils.xml.{closeTag, namespace, openTag}

import scala.util.Try
import scala.xml.NodeSeq
//import info.kwarc.mmt.stex.xhtml.HTMLParser.HTMLText
//import info.kwarc.mmt.stex.xhtml.{HTMLParser, OMDocHTML}

import scala.xml.Node

class MMTsTeXPresenter(stex: STeXPresenterTex, mathml:STeXPresenterML) extends HTMLPresenter(new InformalMathMLPresenter) {
  val key = "html"

  def doNotation(n: NodeSeq): NodeSeq = n


  private def getParent(d : Declaration) : Option[StructuralElement] = controller.getO(d.parent) match {
    case Some(t : Theory) if t.name.length > 1 =>
      controller.getO(t.parent ? t.name.head)
    case Some(s : Structure) => getParent(s)
    case o => o
  }
}

class InformalMathMLPresenter extends presentation.PresentationMathMLPresenter {
  def doTermApplication(f : Term, args : List[Term])(implicit pc: PresentationContext) : Int = {
    doDefault(f)
    doBracketedGroup {
      val comps : List[() => Unit] = args.zipWithIndex.map {case (t,i) =>
        () => {
          recurse(t)(pc.child(i))
          ()
        }
      }
      doListWithSeparator(comps, () => pc.out(", "))
    }
    1
  }

  override def doDefault(o: Obj)(implicit pc: PresentationContext): Int = o match {
    case ComplexTerm(op, subs, con, args) =>
      if (subs.isEmpty && con.isEmpty)
        doTermApplication(OMS(op), args)
      else super.doDefault(o)
    case OMA(f, args) => doTermApplication(f, args)
    case _ => super.doDefault(o)
  }

  override def apply(o: Obj, origin: Option[CPath])(implicit rh : RenderingHandler): Unit = {
    implicit val pc = preparePresentation(o, origin)
    doInfToplevel(o) {
      recurse(o)
    }
  }

  override def doAttributedTerm(t : Term, k : OMID, v : Term)(implicit pc : PresentationContext) = k.path match {
    case Narration.path | MathMLNarration.path =>
      doInformal(v,t)
      1
    case _ => doDefault(t)
  }

  def doInformal(t : Term, tm : Term)(implicit pc : PresentationContext) : Unit = t match {
    case OMFOREIGN(n) => doInformal(n, tm)(pc)
    case _ => doInfToplevel(t) {
      recurse(t)(pc)
    }
  }

  // TODO this should override doToplevel and apply should go
  def doInfToplevel(o: Obj)(body: => Unit)(implicit pc: PresentationContext) = o match {
    case OMATTR(t, k, v) if k.path != MathMLNarration.path => //normal html
      //val attrs = t.head.map(p => HTMLAttributes.symref -> p.toPath).toList
      pc.out(openTag("span",Nil))
      body
      pc.out(closeTag("span"))
    case _ => //wrapping as mathml
      val nsAtts = List("xmlns" -> namespace("mathml"))
      /*val mmtAtts = pc.owner match {
        case None => Nil
        case Some(cp) => List(HTMLAttributes.owner -> cp.parent.toPath, HTMLAttributes.component -> cp.component.toString, HTMLAttributes.position -> "")
      }
      val idAtt = ( "id" -> o.hashCode.toString)*/
      // <mstyle displaystyle="true">
      pc.out(openTag("math",  nsAtts))
      pc.out(openTag("semantics", Nil))
      pc.out(openTag("mrow", Nil))
      body
      pc.out(closeTag("mrow"))
      pc.out(openTag("annotation-xml", List("encoding" -> "MathML-Content")))
      pc.out(ContentMathMLPresenter(o).toString)
      pc.out(closeTag("annotation-xml"))
      pc.out(closeTag("semantics"))
      pc.out(closeTag("math"))
  }

  override def doDelimiter(p: GlobalName, d: Delimiter, implicits: List[Cont])(implicit pc : PresentationContext) = d.text match {
    case "&#40;" => super.doDelimiter(p, Delim("("), implicits)
    case "&#41;" => super.doDelimiter(p, Delim(")"), implicits)
    //unescaping things already escaped by latexml
    case "&gt;" => super.doDelimiter(p, Delim(">"), implicits)
    case "&lt;" => super.doDelimiter(p, Delim("<"), implicits)
    case _ => super.doDelimiter(p, d, implicits)
  }


  //Namespaces whose nodes and attributes we ignore
  private val ignoredNamespaces = List("http://omdoc.org/ns",  //omdoc
    "http://kwarc.info/ns/sTeX",  //stex
    "http://purl.org/dc/elements/1.1/", //dc
    "http://www.openmath.org/OpenMath" //openmath
  )
  //Namespaces for whose nodes and attributes we remove the prefix because they are in HTML5 (e.g. mathml & svg)
  private val coveredNamespaces = List("http://www.w3.org/1998/Math/MathML" //mathml
  )

  def cleanAttribs(attrs : scala.xml.MetaData, scope : scala.xml.NamespaceBinding) : scala.xml.MetaData = {
    var newAttr : scala.xml.MetaData = attrs
    def traverse(att : scala.xml.MetaData) : Unit = att match {
      case scala.xml.Null => scala.xml.Null //nothing to do
      case p : scala.xml.PrefixedAttribute =>
        val uri = scope.getURI(p.pre)
        if (ignoredNamespaces.contains(uri) || coveredNamespaces.contains(uri)) { //removing prefix attr
          newAttr = newAttr.remove(uri, scope, p.key)
        }
        if (coveredNamespaces.contains(uri)) { //adding an unprefixed attr back
          newAttr = new scala.xml.UnprefixedAttribute(p.key, p.value, newAttr)
        }
        traverse(att.next)
      case u : scala.xml.UnprefixedAttribute =>
        if (u.key == "about" || u.key == "id")
          newAttr = newAttr.remove(u.key)
        traverse(att.next)
    }
    traverse(attrs)
    newAttr
  }

  private def cleanScope(scope : scala.xml.NamespaceBinding) : scala.xml.NamespaceBinding = {
    if (scope == scala.xml.TopScope) {
      scope
    } else {
      if (ignoredNamespaces.contains(scope.uri) || coveredNamespaces.contains(scope.uri) || scope.prefix == null) {
        cleanScope(scope.parent)
      } else {
        scala.xml.NamespaceBinding(scope.prefix, scope.uri, cleanScope(scope.parent))
      }
    }
  }

  def doInformal(n : scala.xml.Node, tm : Term)(implicit pc : PresentationContext) : Unit = n match {
    case _ if (n.label == "immtref") =>
      val pos = Position.parse(xml.attr(n,"pos"))
      val inPos = Position(pos.indices.tail)
      val term = tm.subobject(inPos)._2
      doInfToplevel(term) {
        recurse(term)(pc)
      }
    case s : scala.xml.SpecialNode => pc.out(s.toString)
    case _ =>
      val prefixURI = n.scope.getURI(n.prefix)
      if (n.prefix == null || !ignoredNamespaces.contains(prefixURI)) {
        //removing prefix for html5-convered namespaces (e.g. mathml, svg)
        val prefix = if (coveredNamespaces.contains(prefixURI)) null else n.prefix
        val scope = cleanScope(n.scope)
        val attribs = cleanAttribs(n.attributes, n.scope)
        pc.rh.writeStartTag(prefix, n.label, attribs, scope)
        n.child.map(c => doInformal(c, tm)(pc))
        pc.rh.writeEndTag(prefix, n.label)
      } else {
        //TODO report warning
        //nothing to do
      }
  }

  override protected def preparePresentation(o: Obj, origin: Option[CPath])(implicit rh : RenderingHandler) = {
    origin.map(_.parent).foreach {
      case p: ContentPath => if (p.module != p) Try(controller.simplifier(p.module))
      case _ =>
    }
    val globalCont = (o,origin) match {
      case (tm : Term,None) =>
        var mpaths : List[MPath] = Nil
        new StatelessTraverser {
          override def traverse(t: Term)(implicit con: Context, state: State): Term = t match {
            case OMS(s) =>
              mpaths ::= s.module
              t
            case _ => Traverser(this,t)
          }
        }.apply(tm,())
        val em = mpaths.distinct.foldLeft(Context.empty)((c,p) => c ++ Context(p))
        controller.simplifier.elaborateContext(Context.empty,em)
      case (_,Some(cp)) => controller.getO(cp.parent) match {
        case None =>
          Context.empty
        case Some(se) =>
          val c1 = controller.getContext(se)
          val c2 = se.getComponentContext(cp.component)
          c1 ++ c2
      }
      case _ => Context.empty
    }
    PresentationContext(rh, origin, Nil, None, Position.Init, globalCont, Nil, None)
  }
}

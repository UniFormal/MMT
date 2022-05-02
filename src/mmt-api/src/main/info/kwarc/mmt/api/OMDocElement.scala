package info.kwarc.mmt.api

import modules._
import presentation._
import symbols._
import objects._
import documents._
import opaque._

import scala.xml.Node

/** A StructuralElement is any knowledge item on the document, module, or symbol level.
  *
  * The structural elements are subdivided according to their dimension: content, presentation, or narration.
  */
trait StructuralElement extends Content with NamedElement {
  /** the kind of declaration, e.g., "constant" */
  val feature: String
  /** the MMT URI of the element */
  def path: ComponentParent

  //def governingPath = path match {case c: ContentPath => Some(c) case _ => None}
  /** the containing knowledge item, a URL if none */
  def parent: ComponentParent

  /** the children of this element */
  def getDeclarations: List[StructuralElement]

  /** returns all term components of this elements */
  def getComponents: List[DeclarationComponent]

  /** returns a specific component if present */
  def getComponent(k: ComponentKey) = getComponents find (_.key == k) map (_.value)

  /** like getComponent but returns the additional context (in addition to the context of the element) of the component,
   *  empty by default, override as needed
   *  unspecified if the component does not exist
   */
  def getComponentContext(k: ComponentKey) = Context.empty

  /** If a StructuralElement has been generated (as opposed to being physically present in the document),
    * this gives its origin.
    *
    * The origin must be set by overriding the field when creating the ContentElement.
    */
  private var origin: Origin = Original
  def setOrigin(o: Origin) {
    origin = o
  }
  def getOrigin = origin
  def isGenerated = origin != Original

  /** header information of this elements
    * includes the MMT types (e.g., domain, codomain of links) but not the logical types of constants
    */
  def headerInfo: HeaderInfo = {
    val attributes = this match {
      case d: Document => List(d.root, d.contentAncestor)
      case r: NRef => List(r.target)
      case e: OpaqueElement => Nil
      case t: Theory => List(t.meta, t.parameters)
      case v: View => List(v.from, v.to, v.isImplicit)
      case m: DerivedModule => List(m.meta, m.tp)
      case n: NestedModule => n.module.headerInfo.attributes
      case c: Constant => List(c.alias, c.rl)
      case s: Structure => List(s.from, s.isImplicit, s.isTotal)
      case r: RuleConstant => Nil
      case d: DerivedDeclaration => List(d.tp)
    }
    HeaderInfo(feature, path, attributes)
  }

  /** logically equivalent: compares headerInfo, components, and declarations */
  def equivalentTo(that: StructuralElement): Boolean = {
    if (this.headerInfo != that.headerInfo)
      return false
    val thisComps = this.getComponents
    val thatComps = that.getComponents
    if (thisComps.length != thatComps.length)
      return false
    (thisComps zip thatComps).foreach {case (c,d) =>
      if (c.key != d.key)
        return false
      if (!c.value.equivalentTo(d.value))
        return false
    }
    val thisDecl = this.getDeclarations
    val thatDecl = that.getDeclarations
    if (thatDecl.length != that.getDeclarations.length)
      return false
    (thisDecl zip thatDecl) forall {case (d,e) =>
      d equivalentTo e
    }
  }

  /** two StructuralElement's are compatible
    * if they have the same type, same Path, and agree in all parts that are TermContainer's
    */
  def compatible(that: StructuralElement): Boolean = {
    if (this.path != that.path || this.getClass != that.getClass || this.getOrigin != that.getOrigin)
       return false
    if (this.headerInfo != that.headerInfo)
      return false
    ((this, that) match {
        case (a: Document, b: Document) =>
           true
        case (a: NRef, b: NRef) =>
           true
        case (a: OpaqueElement, b: OpaqueElement) =>
           false // subclasses of OpaqueElement may override this method to give better results
        case (a: Theory, b: Theory) =>
          a.df == b.df
        case (a: View, b: View) =>
          a.df == b.df
        case (a: NestedModule, b: NestedModule) =>
          a.module.compatible(b.module)
        case (a: Constant, b: Constant) =>
          true
        case (a: Structure, b: Structure) =>
          a.df == b.df
        case (a: DerivedDeclaration, b: DerivedDeclaration) =>
          a.df == b.df
        case _ => false
      })
  }
  /** merge all properties of 'that' into 'this' except for components and declarations */
  def merge(that: StructuralElement) {
     this.metadata = that.metadata
  }
}

/** all information that defines the nature of this element
  * a change to this information is so fundamental that it should be treated like a delete-add, not an update event
  */
case class HeaderInfo(feature: String, path: Path, attributes: List[Any])

/** A ContentElement is any knowledge item that is used to represent mathematical content.
  *
  * These are the core MMT items such as modules, and symbols.
  * This includes virtual knowledge items.
  */
trait ContentElement extends StructuralElement {
  /** the kind of declaration, e.g., "constant" */
  val feature: String
  
  /** local name relative to the parent element or namespace */
  def name: LocalName

  def path: ContentPath

  def toTerm: Term
  
  /** returns all children of this elements */
  def getDeclarations: List[ContentElement]

  /** recursively applies a function to all declarations in this element (in declaration order) */
  def foreachDeclaration(f: ContentElement => Unit) {
    f(this)
    getDeclarations foreach { d => d.foreachDeclaration(f) }
  }

  /** recursively applies a function to all components in this element (in declaration order) */
  def foreachComponent(f: (CPath, ComponentContainer) => Unit) {
    getComponents foreach { case DeclarationComponent(c, t) => f(path $ c, t) }
    getDeclarations foreach { d => d.foreachComponent(f) }
  }
}

/** A NarrativeElement is any OMDoc element that is used to represent narration and document structure.
  *
  * These include documents and cross-references.
  */
trait NarrativeElement extends StructuralElement {
  def path: DPath
  /** the containing document (if any) */
  def parentOpt: Option[DPath]
  def getComponents: List[DeclarationComponent] = Nil
}

/** The trait Content is mixed into any class that can be rendered.
  */
trait Content extends metadata.HasMetaData with ClientProperties {
  /** XML representation */
  def toNode: Node

  /** by default, this prints out toNode
    *
    * potentially large [[StructuralElement]]s should override it with a memory-efficient implementation
    */
  def toNode(rh: RenderingHandler) {
    rh(toNode)
  }
}

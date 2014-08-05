package info.kwarc.mmt.api
import presentation._
import symbols._
import modules._
import scala.xml.Node

/** 
 * A StructuralElement is any knowledge item on the document, module, or symbol level.
 * The structural elements are subdivided according to their dimension: content, presentation, or narration.
 */
trait StructuralElement extends Content with metadata.HasMetaData {
   /** the MMT URI of the element */
   def path : Path
   def governingPath = path match {case c: ContentPath => Some(c) case _ => None}
   /** the containing knowledge item, a URL if none */
   def parent : Path
   /** If a StructuralElement has been generated (as opposed to being physically present in the document),
    * this gives its origin.
    * The origin must be set by overriding the field when creating the ContentElement. 
    */
   private var origin : Origin = Unelaborated
   def setOrigin(o: Origin) {origin = o}
   def getOrigin = origin
   def isGenerated = origin != Unelaborated && origin != Elaborated
   def inElaborated = origin match {
      case Unelaborated => true
      case InstanceElaboration(_) => true
      case _ => false
   }
}

/**
 * A ContentElement is any knowledge item that is used to represent mathematical content.
 * These are the core MMT items such as modules, and symbols.
 * This includes virtual knowledge items.
 */
trait ContentElement extends StructuralElement {
   /**
    * the API may deactivate a ContentElement instead of deleting it to permit reusing it later
    * 
    * invariant: API client code may assume that this flag is never set   
    */
   private[api] var inactive: Boolean = false
   def path : ContentPath
   /** returns all children of this elements */
   def getDeclarations: List[ContentElement]
   /** returns all term components of this elements */
   def getComponents: List[(DeclarationComponent,ComponentContainer)]
   /** returns a specific component if present */
   def getComponent(c: DeclarationComponent) = getComponents find (_._1 == c) map (_._2)
   /** recursively applies a function to all declarations in this element (in declaration order) */
   def foreachDeclaration(f: ContentElement => Unit) {
      f(this)
      getDeclarations foreach {d => d.foreachDeclaration(f)}
   }
   /** recursively applies a function to all components in this element (in declaration order) */
   def foreachComponent(f: (CPath,ComponentContainer) => Unit) {
      getComponents foreach {case (c,t) => f(path $ c,t)}
      getDeclarations foreach {d => d.foreachComponent(f)}
   }
   /** two ContentElement's are compatible
    * if they have the same type, same Path, and agree in all parts that are TermContainer's
    */  
   def compatible(that: ContentElement): Boolean = {(this, that) match {
      case (a: DeclaredTheory, b: DeclaredTheory) =>
         a.path == b.path && a.meta == b.meta && a.parameters == b.parameters
      case (a: DefinedTheory, b: DefinedTheory) =>
         a.path == b.path && a.parameters == b.parameters
      case (a: View, b: View) =>
         a.getClass == b.getClass && a.path == b.path && a.from == b.from && a.to == b.to && (a.isImplicit == b.isImplicit)
      case (a: NestedModule, b: NestedModule) =>
         a.module.compatible(b.module)
      case (a: Constant, b: Constant) =>
         a.path == b.path && a.alias == b.alias && a.rl == b.rl
      case (a: Structure, b: Structure) =>
         a.getClass == b.getClass && a.path == b.path && (a.isImplicit == b.isImplicit)
      case _ => false
   }}
}

/**
 * A PresentationElement is any knowledge item element that is used to represent notations.
 * These includes styles and notations.
 */
trait PresentationElement extends StructuralElement

/**
 * A NarrativeElement is any OMDoc element that is used to represent narration and document structure.
 * These include documents and cross-references.
 */
trait NarrativeElement extends StructuralElement

/**
 * The trait Content is mixed into any class that can be rendered using notations.
 */
trait Content {
   /** XML representation */
   def toNode : Node
   /** the role, the non-terminal in the MMT grammar producing this item */  
   def role : Role
   /** the governingPath is used to define the owner of components in a CPath */
   def governingPath : Option[ContentPath]
   /**
    * the components are an abstract definition of the children of a content item that are used for presentation 
    *
    * Other algorithms than presentation should use the corresponding methods of ContentElement   
    */
   def components : List[Content]
   /** content items may provide short names for their components, Nil by default */
   def compNames : List[(DeclarationComponent,Int)] = Nil
   /** this ContentComponents object permits accessing components by name */
   def contComponents = ContentComponents(components, compNames, governingPath)
   /** the children of this content element, used for fragmentPaths */
   def children : List[Content]
}

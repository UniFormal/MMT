package info.kwarc.mmt.api
import presentation._
import scala.xml.Node

/** 
 * A StructuralElement is any knowledge item on the document, module, or symbol level.
 * The structural elements are subdivided according to their dimension: content, presentation, or narration.
 */
trait StructuralElement extends Content with metadata.HasMetaData {
   /** the MMT URI of the element */
   def path : Path
   /** the governingPath required by content is the path */
   def governingPath = Some(path)
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
   def getComponents: List[(objects.DeclarationComponent,symbols.TermContainer)]
   /** returns a specific component if present */
   def getComponent(c: objects.DeclarationComponent) = getComponents find (_._1 == c) map (_._2)
   /** recursively applies a function to all declarations in this element (in declaration order) */
   def foreachDeclaration(f: ContentElement => Unit) {
      f(this)
      getDeclarations foreach {d => d.foreachDeclaration(f)}
   }
   /** recursively applies a function to all components in this element (in declaration order) */
   def foreachComponent(f: (CPath,symbols.TermContainer) => Unit) {
      getComponents foreach {case (c,t) => f(path $ c,t)}
      getDeclarations foreach {d => d.foreachComponent(f)}
   }
   /** two ContentElement's are compatible
    * if they have the same type, same Path, and agree in all parts that are TermContainer's
    */  
   def compatible(that: ContentElement) = {(this, that) match {
      case (a: symbols.Constant, b: symbols.Constant) =>
         a.path == b.path && a.alias == b.alias && a.rl == b.rl && a.not == b.not
      case (a: modules.DeclaredTheory, b: modules.DeclaredTheory) =>
         a.path == b.path && a.meta == b.meta
      case (a: modules.DeclaredView, b: modules.DeclaredView) =>
         a.path == b.path && a.from == b.from && a.to == b.to
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

/** A RelationalElement is any element that is used in the relational representation of MMT content.
 * These include the unary and binary predicates occurring in an MMT ABox.
 * They do not correspond to XML elements in an OMDoc element and thus do not extend StructuralElement. 
 */
trait RelationalElement {
   /** the URL from which this item originated, currently not implemented */
   //val parent : Path = null //TODO origin of relational items
   /** the MMTURI of the "about" item in the RDF sense */
   val path : Path
   /** XML representation */
   def toNode : scala.xml.Node
   /** text representation */
   def toPath : String
}

/**
 * The trait Content is mixed into any class that can be rendered using notations.
 */
trait Content {
   /** XML representation */
   def toNode : Node
   /** the role, the non-terminal in the MMT grammar producing this item */  
   def role : Role
   def governingPath : Option[Path]
   /**
    * the components are an abstract definition of the children of a content item that are used for presentation 
    *
    * Other algorithms than presentation should use the corresponding methods of ContentElement   
    */
   def components : List[Content]
   /** content items may provide short names for their components, Nil by default */
   def compNames : List[(String,Int)] = Nil
   /** this ContentComponents object permits accessing components by name */
   def contComponents = ContentComponents(components, compNames, governingPath)
}

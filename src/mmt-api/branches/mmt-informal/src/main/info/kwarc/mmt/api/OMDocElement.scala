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
   /** the components are an abstract definition of the children of a content item */
   def components : List[Content]
   /** content items may provide short names for their components, Nil by default */
   def compNames : List[(String,Int)] = Nil
   /** this ContentComponents object permits accessing components by name */
   def contComponents = ContentComponents(components, compNames, governingPath)
}

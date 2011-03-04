package jomdoc
import jomdoc.presentation._
import scala.xml.Node

/**
 * The trait Content is mixed into any class that can be rendered using notations.
 */
trait Content {
   def presentation(lpar : LocalParams) : PresentationData
   def toNode : Node   
}

/**
 * The trait MetaData is mixed into any class that can carry metadata (not used yet)
 */
trait MetaData {
   val metadata = new scala.collection.mutable.HashMap[SPath,String]
}

/** 
 * A StructuralElement is any OMDoc element on the document, module, or symbol level.
 */
trait StructuralElement extends Content with MetaData {
   def role : Role
   def path : Path
   def parent : Path
   def components : List[Content]
   def presentation(lpar : LocalParams) = ByNotation(NotationKey(Some(path), role), components, lpar)
}

/**
 * A ContentElement is any OMDoc element that is used to represent mathematical content.
 */
trait ContentElement extends StructuralElement

/**
 * A PresentationElement is any OMDoc element that is used to represent notations.
 */
trait PresentationElement extends StructuralElement

/**
 * A DocumentElement is any OMDoc element that is used to represent document structure.
 */
trait DocumentElement extends StructuralElement
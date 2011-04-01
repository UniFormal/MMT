package info.kwarc.mmt.api.documents
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.presentation._

/**
 * A Document represents an MMT document.
 *
 * A document is stored as a list of references (XRef) to content items.
 * The content itself is stored in the controller.
 * 
 * @param path the URI of the document
 * @param inititems the initial list of references
 * @param controller a read-only controller in which references are resolved to obtain the full document
 */
class Document(val path : DPath) extends NarrativeElement {
   /** constructs a non-empty document */
   def this(path : DPath, inititems : List[XRef]) = {
	   this(path)
	   inititems foreach add
   }
   private var items : List[XRef] = Nil
   /** returns the list of children of the document */
   def getItems = items
   /** adds a child at the end of the documents */
   def add(r : XRef) {
      items = items ::: List(r)
   }
   val parent = path ^!
   val role = Role_Document
   def components = StringLiteral(path.toString) :: items
   override def toString = items.map(_.toString).mkString("","\n","")
   def toNode = <omdoc base={path.toString}>{items.map(_.toNode)}</omdoc>
}

/**
 * An XRef represents a reference from a document to an external document part or module.
 * An XRef is semantically equivalent to copy-pasting the referenced module.
 * All documents are represented as lists of XRefs no matter whether a module is given locally or remotely.
 * @param parent the containing document
 * @param target the referenced module
 * @param generated true iff the module was given directly in the document rather than referenced remotely
 */
abstract class XRef(val parent : DPath, val target : Path) extends NarrativeElement {
   val path = parent
   val role = Role_XRef
   def components = List(StringLiteral(target.toString))
   def toNode : scala.xml.Node
   override def toString = "ref " +  target.toPath
}

/** reference to a document section */
class DRef(p : DPath, target : DPath) extends XRef(p, target) {
   def toNode = <omdoc href={target.toString}/>
}
object DRef {
   def apply(p : DPath, target : DPath, generated: Boolean): DRef = {
      val r = new DRef(p, target)
      if (generated) r.setOrigin(DocumentSkeleton)
      r
   }
}
/** reference to a module */
class MRef(p : DPath, target : MPath) extends XRef(p, target) {
   def toNode = <mref target={target.toString}/>
}
object MRef {
   def apply(p : DPath, target : MPath, generated: Boolean): MRef = {
      val r = new MRef(p, target) 
      if (generated) r.setOrigin(DocumentSkeleton)
      r
   }
}

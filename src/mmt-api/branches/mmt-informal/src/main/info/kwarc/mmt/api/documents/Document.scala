package info.kwarc.mmt.api.documents
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.presentation._
import info.kwarc.mmt.api.libraries._
import info.kwarc.mmt.api.modules._

/**
 * A Document represents an MMT document.
 *
 * A document is stored as a list of references (XRef) to content items.
 * The content itself is stored in the controller.
 * 
 * @param path the URI of the document; for toplevel documents, this is a URL
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
   /** returns the list of local children (as opposed to remotely referenced ones) in document-order */
   def getLocalItems : List[XRef] = items filter (_.isGenerated)
   /** returns the list of modules declared in the document (not user-written references) */
   def getModulesResolved(lib: Lookup) : List[Module] = items flatMap {
       case r: MRef if r.isGenerated => List(lib.getModule(r.target))
       case _ => Nil
   }
   /** adds a child at the end of the documents */
   def add(r : XRef) {
      items = items ::: List(r)
   }
   val parent = path ^!
   val role = Role_Document
   def components = StringLiteral(path.toString) :: StringLiteral(path.last) :: items
   override def toString = items.map(_.toString).mkString("","\n","")
   def toNode = <omdoc base={path.toPath}>{items.map(_.toNode)}</omdoc>
   /** prints document with all generated module references expanded (document references are not expanded) */
   def toNodeResolved(lib: Lookup) =
      <omdoc base={path.toPath}>
        {items map {
           case r : MRef if r.isGenerated => lib.get(r.target).toNode
           case r => r.toNode
        }}
     </omdoc>
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
   def components = List(StringLiteral(target.toString), StringLiteral(target.last))
   def toNode : scala.xml.Node
   override def toString = "ref " +  target.toPath
}

/** reference to a document section */
class DRef(p : DPath, override val target : DPath) extends XRef(p, target) {
   val role = Role_DRef
   def toNode = <omdoc href={target.toPath}/>
}
object DRef {
   def apply(p : DPath, target : DPath, generated: Boolean = false): DRef = {
      val r = new DRef(p, target)
      if (generated) r.setOrigin(DocumentSkeleton)
      r
   }
}
/** reference to a module */
class MRef(p : DPath, override val target : MPath) extends XRef(p, target) {
   val role = Role_MRef
   def toNode = <mref target={target.toPath}/>
}
object MRef {
   def apply(p : DPath, target : MPath, generated: Boolean = false): MRef = {
      val r = new MRef(p, target) 
      if (generated) r.setOrigin(DocumentSkeleton)
      r
   }
}

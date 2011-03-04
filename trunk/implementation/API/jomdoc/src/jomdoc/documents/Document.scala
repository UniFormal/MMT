package jomdoc.documents
import jomdoc._
import jomdoc.frontend._
import jomdoc.libraries._
import jomdoc.presentation._
import jomdoc.modules._
import jomdoc.symbols._
import jomdoc.objects._
import jomdoc.utils._

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
class Document(val path : DPath) extends DocumentElement {
   /** constructs a non-empty document */
   def this(path : DPath, inititems : List[XRef]) = {
	   this(path)
	   inititems foreach add
   }
   private var items : List[XRef] = Nil
   def getItems = items
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
 * An XRef represents a reference from a document to a module declared in some other document.
 * 
 * An XRef is semantically equivalent to copy-pasting the referenced module.
 * All documents are represented as lists of XRefs no matter whether a module is given directly or by reference.
 * @param parent the containing document
 * @param target the referenced module
 * @param generated true iff the module was given directly and replaced by a generated XRef during parsing.
 */
abstract class XRef(val parent : DPath, val target : Path, val generated : Boolean) extends DocumentElement {
   val path = parent
   val role = Role_XRef
   def components = List(StringLiteral(target.toString))
   def toNode : scala.xml.Node
}

case class DRef(p : DPath, override val target : DPath, g : Boolean) extends XRef(p, target, g) {
   def toNode = <omdoc href={target.toString}/>
}
case class MRef(p : DPath, override val target : MPath, g : Boolean) extends XRef(p, target, g) {
   def toNode = <mref target={target.toString}/>
}

/**
 * A DocStore holds a set of documents indexed by their URIs.
 */
class DocStore(abox : ontology.ABoxStore, report : Report) {
   private val documents = new scala.collection.mutable.HashMap[DPath,Document]
   /** adds a document to the DocStore */
   def add(d : DocumentElement) {d match {
      case d : Document =>
         documents(d.path) = d
         abox += ontology.IsDocument(d.path)
      case r : XRef =>
         val d = try {documents(r.parent)} catch {case _ => throw AddError("document does not exist in " + r)}
         d.add(r)
         abox += ontology.Declares(d.path, r.target)
   }}
   /** retrieves a document from the DocStore */
   def get(p : DPath) = {
      try {documents(p)}
      catch {case _ => throw frontend.NotFound(p)}
   }
   /** deletes all documents */
   def clear {documents.clear}
}


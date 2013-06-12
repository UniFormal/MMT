package info.kwarc.mmt.api.documents
import info.kwarc.mmt.api._
import presentation._
import libraries._
import modules._
import utils.MyList._

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
   def this(path : DPath, inititems : List[DocumentItem]) = {
	   this(path)
	   inititems foreach add
   }
   private var items : List[DocumentItem] = Nil
   /** returns the list of children of the document (including narration) */
   def getNarratedItems = items
   /** returns the list of children of the document (excluding narration) */
   def getItems : List[XRef] = items mapPartial {
     case r: XRef => Some(r)
     case _ => None
   }
   /** returns the list of local children (as opposed to remotely referenced ones) in document-order */
   def getLocalItems : List[XRef] = getItems filter (_.isGenerated)
   /** returns the list of modules declared in the document (not user-written references) */
   def getModulesResolved(lib: Lookup) : List[Module] = getItems flatMap {
       case r: MRef if r.isGenerated => List(lib.getModule(r.target))
       case _ => Nil
   }
   /** adds a child at the end of the documents */
   def add(i : DocumentItem) {
      items = items ::: List(i)
   }
   val parent = path ^!
   val role = Role_Document
   def components = StringLiteral(path.toString) :: StringLiteral(path.last) :: items
   override def toString = "document " + path + items.map(_.toString).mkString("\n\t","\n\t","")
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

trait DocumentItem extends Content {
  
}

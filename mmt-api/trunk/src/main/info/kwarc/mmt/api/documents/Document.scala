package info.kwarc.mmt.api.documents

import info.kwarc.mmt.api._
import libraries._
import modules._

import scala.xml.Elem

/**
 * A Document represents an MMT document.
 *
 * A document is stored as a list of references (XRef) to content items.
 * The content itself is stored in the controller.
 *
 * @param path the URI of the document; for toplevel documents, this is a URL
 * @param root true if this does not have a containing document
 * @param nsMap the namespaces declared in this document
 */
class Document(val path: DPath, val root: Boolean = false, val contentAncestor: Option[Body] = None, inititems: List[NarrativeElement] = Nil, val nsMap: NamespaceMap = NamespaceMap.empty) extends NarrativeElement {
  /** the containing document if root == false; otherwise, the URI without path */ 
  def parentOpt = if (root) None else Some(parent)
  val parent: DPath = if (root) path ^^ else path ^
  val name = if (root) path.name else LocalName(path.name.last)

  private var items: List[NarrativeElement] = inititems

  /** returns the namespace map that was used while parsing the document */
  def getNamespaceMap: NamespaceMap = nsMap

  /** returns the list of children of the document (including narration) */
  def getDeclarations: List[NarrativeElement] = items

  /** returns the list of children of the document with nested documents replaced with NRefs */
  def getRefs: List[NRef] = items map {
    case d: Document => new DRef(path, LocalName.empty, d.path)
    case r: NRef => r
  }

  /** returns the list of modules declared in the document (not user-written references) */
  def getModulesResolved(lib: Lookup): List[Module] = items collect {
    case r: MRef => lib.getModule(r.target)
  }

  /**
   * @param controller Controller for looking up documents
   * @return list of modules declared/referenced anywhere in this Document (depth first)
   */
  def collectModules(controller: frontend.Controller): List[MPath] = items flatMap {
    case r: MRef => List(r.target)
    case d: DRef => controller.get(d.target).asInstanceOf[Document].collectModules(controller)
    case _: SRef => Nil
    case d: Document => d.collectModules(controller)
  }

  /** adds a child at the end of the documents */
  def add(i: NarrativeElement, afterOpt: Option[LocalName] = None) {
     var pos = afterOpt.map(a => items.indexWhere(_.name == a)).getOrElse(items.length)
     if (pos == -1) pos = items.length // maybe issue warning that afterOpt not found
     val (bef,aft) = items.splitAt(pos) // order(pos) == aft.head
     items = bef ::: i :: aft
  }
  /** retrieves a child */
  def getO(n: LocalName) = items.find(_.name == n)
  /** retrieves a child */
  def get(n: LocalName) = getO(n).get
  /** deletes a child */
  def delete(n: LocalName) {
     items = items.filterNot(_.name == n)
  }

  override def toString: String = "document " + path + items.map("\n  " + _.toString).mkString + "\n"

  def toNode: Elem = <omdoc base={path.toPath}>{getMetaDataNode}{items.map(_.toNode)}</omdoc>

  override def toNode(rh: presentation.RenderingHandler) {
    rh( s"""<omdoc base="${path.toPath}">""")
    rh(getMetaDataNode)
    items.foreach { i =>
      i.toNode(rh)
    }
    rh("</omdoc>")
  }

  /** prints document with all generated references expande */
  def toNodeResolved(lib: Lookup): Elem =
      <omdoc base={path.toPath}>
        {getMetaDataNode}
        {items map {
           case d: Document => d.toNodeResolved(lib)
           case r: NRef if r.isGenerated => lib.get(r.target).toNode
           case i => i.toNode
        }}
     </omdoc>
}

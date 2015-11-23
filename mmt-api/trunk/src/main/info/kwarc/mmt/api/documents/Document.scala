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
class Document(val path: DPath, val root: Boolean = false, inititems: List[NarrativeElement] = Nil, val nsMap: NamespaceMap = NamespaceMap.empty) extends NarrativeElement {
  /** the containing document if root == false; otherwise, unspecified */ 
  val parent: DPath = path ^!
  def parentOpt = if (root) None else Some(parent)

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
    case d: Document => d.collectModules(controller)
  }

  /** adds a child at the end of the documents */
  def add(i: NarrativeElement) {
    items = items ::: List(i)
  }

  override def toString: String = "document " + path + items.map("\n  " + _.toString).mkString

  def toNode: Elem = <omdoc base={path.toPath}>{items.map(_.toNode)}</omdoc>

  override def toNode(rh: presentation.RenderingHandler) {
    rh( s"""<omdoc base="${path.toPath}">""")
    items.foreach { i =>
      i.toNode(rh)
    }
    rh("</omdoc>")
  }

  /** prints document with all generated module references expanded (document references are not expanded) */
  def toNodeResolved(lib: Lookup): Elem =
      <omdoc base={path.toPath}>
        {items map {
        case r: MRef if r.isGenerated => lib.get(r.target).toNode
        case r => r.toNode
      }}
     </omdoc>
}

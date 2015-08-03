package info.kwarc.mmt.api.documents

import info.kwarc.mmt.api._
import libraries._
import modules._
import utils.MyList._

import scala.language.postfixOps
import scala.xml.Elem

/**
 * A Document represents an MMT document.
 *
 * A document is stored as a list of references (XRef) to content items.
 * The content itself is stored in the controller.
 *
 * @param path the URI of the document; for toplevel documents, this is a URL
 */
class Document(val path: DPath, inititems: List[DocumentItem] = Nil, val nsMap: NamespaceMap = NamespaceMap.empty) extends NarrativeElement {
  private var items: List[DocumentItem] = inititems

  /** returns the namespace map that was used while parsing the document */
  def getNamespaceMap: NamespaceMap = nsMap

  /** returns the list of children of the document (including narration) */
  def getDeclarations: List[NarrativeElement] = items.flatMap {
    case d: NarrativeElement => List(d)
    case _ => Nil
  }

  def getNarratedItems: List[DocumentItem] = items

  /** returns the list of children of the document (excluding narration) */
  def getItems: List[XRef] = items mapPartial {
    case r: XRef => Some(r)
    case _ => None
  }

  /** returns the list of local children (as opposed to remotely referenced ones) in document-order */
  def getLocalItems: List[XRef] = getItems filter (_.isGenerated)

  /** returns the list of modules declared in the document (not user-written references) */
  def getModulesResolved(lib: Lookup): List[Module] = getItems collect {
    case r: MRef if r.isGenerated => lib.getModule(r.target)
  }

  /**
   * @param controller Controller for looking up documents
   * @return list of modules declared/referenced anywhere in this Document (depth first)
   */
  def collectModules(controller: frontend.Controller): List[MPath] = getItems flatMap {
    case r: MRef => List(r.target)
    case d: DRef => controller.get(d.target).asInstanceOf[Document].collectModules(controller)
  }

  /** adds a child at the end of the documents */
  def add(i: DocumentItem) {
    items = items ::: List(i)
  }

  val parent: DPath = path ^!

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

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
 * @param root true if this does not have a directly containing document (true for modules seen as documents)
 * @param contentAncestor the closest container of this document that is a module (if any)
 * @param nsMap the namespaces declared in this document
 */
class Document(val path: DPath, val root: Boolean = false, val contentAncestor: Option[Body] = None, inititems: List[NarrativeElement] = Nil, val nsMap: NamespaceMap = NamespaceMap.empty)
     extends NarrativeElement with ContainerElement[NarrativeElement] {
  /** the containing document if root == false; otherwise, the URI without path */ 
  def parentOpt = if (root) None else Some(parent)
  val parent: DPath = if (root) path ^^ else path ^
  val name = if (root) path.name else LocalName(path.name.last)

  private var items: List[NarrativeElement] = inititems
  def domain = items.map(_.name)

  /** returns the namespace map that was used while parsing the document */
  def getNamespaceMap: NamespaceMap = nsMap

  /** returns the list of children of the document (including narration) */
  def getDeclarations: List[NarrativeElement] = items

  /** returns the list of modules declared in the document */
  def getModulesResolved(lib: Lookup): List[Module] = items collect {
    case r: MRef => lib.getModule(r.target)
  }
 
  /**
   * @param controller Controller for looking up documents
   * @return list of modules declared/referenced anywhere in this Document (depth first)
   */
  def collectModules(controller: frontend.Controller): List[MPath] = items flatMap {
    case d: Document => d.collectModules(controller)
    case r: MRef => List(r.target)
    case d: DRef => controller.get(d.target).asInstanceOf[Document].collectModules(controller)
    case _: SRef => Nil
    case _ => Nil
  }

  def getMostSpecific(name: LocalName) : Option[(NarrativeElement, LocalName)] = {
     if (name.isEmpty) Some((this, LocalName.empty))
     else {
        items.find(i => name.startsWith(i.name)).map {ne =>
           val rest = name.drop(ne.name.length)
           (ne, rest)
        }
     }
   }
  
  /** retrieves a descendant
   *  
   *  This may fail for two reasons:
   *  - a segment of n does not exist
   *  - a segment of n exists but is not a [[Document]]
   *  
   *  use Library.getNarrative for smarter dereferencing
   */
  def getO(name: LocalName): Option[NarrativeElement] = { 
     getMostSpecific(name).flatMap {
       case (ne, LocalName.empty) => Some(ne)
       case (parDoc: Document, rest) => parDoc.getO(rest)
       case _ => None
     }
  }
  /** same as get */
  def getLocally(name: LocalName) = getO(name)
  
  /**
   * adds a child after a given child or at the end
   */
  def add(i: NarrativeElement, afterOpt: Option[LocalName] = None) {
     // default: insert at end
     def defaultPos = items.length
     val pos = afterOpt match {
        case Some(a) => items.indexWhere(_.name == a) match {
           case -1 => defaultPos // maybe issue warning that afterOpt not found
           case i => i
        }
        case None => defaultPos 
     }
     
     val (bef,aft) = items.splitAt(pos) // items(pos) == aft.head
     items = bef ::: (aft match {
       case Nil => i :: aft
       case hd::tl => hd :: i :: tl
     })
  }
  
  /** updates or adds a child */
  def update(ne: NarrativeElement) {
     val i = items.indexWhere(_.name == ne.name)
     if (i != -1)
       items = items.take(i) ::: ne :: items.drop(i+1)
     else
       add(ne)
  }
  
  /** deletes a child */
  def delete(n: LocalName) = {
     val d = getO(n)
     items = items.filterNot(_.name == n)
     d
  }
  /** moves ln to the end */
  def reorder(ln: LocalName) {
     items.find(_.name == ln) match {
        case Some(i) => 
          delete(ln)
          items = items ::: List(i)
        case None => throw ImplementationError("element does not exist")
     }
  }

  override def toString: String = "document " + path + items.map("\n  " + _.toString).mkString + "\n"

  def toNode: Elem = if (root) <omdoc base={path.toPath}>{getMetaDataNode}{items.map(_.toNode)}</omdoc>
    else <omdoc name={name.toPath}>{getMetaDataNode}{items.map(_.toNode)}</omdoc>

  override def toNode(rh: presentation.RenderingHandler) {
    rh( s"""<omdoc base="${path.toPath}">""")
    rh(getMetaDataNode)
    items.foreach { i =>
      i.toNode(rh)
    }
    rh("</omdoc>")
  }

  /** prints document with all generated references expanded 
   *  @param lib the library where reference are looked up
   *  @param expandAll whether to expand all references or only generated ones
   *  false by default
  */
  def toNodeResolved(lib: Lookup, expandAll : Boolean = false): Elem =
      <omdoc base={path.toPath}>
        {getMetaDataNode}
        {items map {
           case d: Document => d.toNodeResolved(lib)
           case r: NRef if (r.isGenerated || expandAll) => lib.get(r.target).toNode
           case i => i.toNode
        }}
     </omdoc>
}

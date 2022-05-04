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
 * @param level the role of this document in a hierarchy of nested documents, default is [[FileLevel]] the role of source files containing modules
 * @param contentAncestor the closest container of this document that is a module (if any)
 * @param initNsMap the namespace map that applies at the beginning of the document
 */
//TODO check if we can mixin DefaultLookup
class Document(val path: DPath, val level: DocumentLevel = FileLevel, val contentAncestor: Option[ModuleOrLink] = None,
               inititems: List[NarrativeElement] = Nil, initNsMap: NamespaceMap = NamespaceMap.empty)
     extends NarrativeElement with ContainerElement[NarrativeElement] with DefaultMutability[NarrativeElement] {
  val feature = "document:" + level
  /** not a section (which is stored as part of some other document) */
  def root: Boolean = !(List(SectionLevel,SectionInModuleLevel) contains level)
  
  /** the containing document if root == false; otherwise, the URI without path */
  def parentOpt = if (root) None else Some(parent)
  val parent: DPath = if (root) path ^^ else path ^
  val name = if (root) path.name else LocalName(path.name.last)

  private var items: List[NarrativeElement] = inititems
  def domain = items.map(_.name)

  /** returns the interpretation contex that applies at the end of this document */
  def getIIContext: InterpretationInstructionContext = {
    var iiC = new InterpretationInstructionContext(initNsMap)
    items foreach {
      case ii:InterpretationInstruction => iiC process ii
      case _ =>
    }
    iiC
  }

  /** returns the namespace map that applies at the end of this document */
  def getNamespaceMap: NamespaceMap = getIIContext.namespaces

  /** returns the list of children of the document (including narration) */
  def getDeclarations: List[NarrativeElement] = items

  /* used by DefaultMutability for add, delete, etc. */
  protected def setDeclarations(nes: List[NarrativeElement]) {
    items = nes
  }

  /**
   * @return list of modules declared in this Document or its children (depth first)
   */
  def getModules(lib: Lookup): List[MPath] = items flatMap {
    case d: Document => d.getModules(lib)
    case r: MRef => List(r.target)
    case d: DRef if !d.isGenerated => lib.get(d.target).asInstanceOf[Document].getModules(lib)
    case _: SRef => Nil
    case _ => Nil
  }

  /** like getModules but resolves all modules */
  def getModulesResolved(lib: Lookup): List[Module] = getModules(lib) map {mp => lib.getModule(mp)}

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

  override def toString: String = "document " + path + items.map("\n  " + _.toString).mkString + "\n"

  def toNode: Elem = if (root) <omdoc base={path.toPath}>{getMetaDataNode}{items.map(_.toNode)}</omdoc>
    else <omdoc name={name.toPath}>{getMetaDataNode}{items.map(_.toNode)}</omdoc>

  override def toNode(rh: presentation.RenderingHandler) {
    rh( s"""<omdoc base="${path.toPath}" level="${level.toXML}">""")
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


/** the role of a [[Document]] in a hierarchy of nested documents
 *  root documents are the ones at [[FileLevel]] - the ones containing modules
 *  Any not-semantically-relevant nesting structure above or below is also represented by documents.
 *  
 *  The levels are organized into triples of root-inner-leaf, where each leaf is the root of another triple:
 *  * Hub-Group-Archive: the structure above projects.
 *  * Archive-Folder-File: the structure inside projects.
 *  * File-Section-Module: the structure inside source files.
 *  * Module-SectionInModule-Module: the structure inside modules 
 */
sealed abstract class DocumentLevel(internal: String, external: String) {
  override def toString = external
  def toXML = internal
}
case object HubLevel extends DocumentLevel("hub", "Archive Hub")
case object GroupLevel extends DocumentLevel("group", "Archive Group")
case object ArchiveLevel extends DocumentLevel("archive", "Archive")
case object FolderLevel extends DocumentLevel("folder", "Folder")
case object FileLevel extends DocumentLevel("file", "Source File")
case object SectionLevel extends DocumentLevel("section", "Section")
case object ModuleLevel extends DocumentLevel("theory", "Module")
case object SectionInModuleLevel extends DocumentLevel("section-in-module", "Section")

object DocumentLevel {
  val all = List(HubLevel, GroupLevel, ArchiveLevel, FolderLevel, FileLevel, SectionLevel, ModuleLevel, SectionInModuleLevel)
  def parseO(s: String) = {
    all.find(_.toXML == s)
  }
  def parse(s: String) = parseO(s).getOrElse {
      throw ParseError("illegal document level: " + s)
    }
}
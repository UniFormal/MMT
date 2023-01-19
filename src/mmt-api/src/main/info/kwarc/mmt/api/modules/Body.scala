package info.kwarc.mmt.api.modules
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.documents._
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.symbols._
import info.kwarc.mmt.api.utils._

import scala.xml.Node

/**
  * This class carries the common properties of complex structural elements, in particular the body and the optional definiens
  * Instances are of two type: [[Theory]] and [[View]]s are [[Module]]s; [[View]]s and [[Structure]]s are [[Link]]s
  *
  * It stores both the logical [[Declaration]]s as well as their narrative structure.
  * For the former, a mutable map from [[LocalName]] to [[Declaration]] is used. It deliberately ignores any
  * narrative structure.
  * In particular, declaration names must be unique independent of the narrative grouping.
  * For the latter, a [[Document]] holding [[SRef]]s to the logical declarations is used.
  */
trait ModuleOrLink extends ContentElement with ContainerElement[Declaration] with HasDefiniens {self =>
  /** this element as a module expression */
  def toTerm : Term

  /** path if seen as a module */
  def modulePath: MPath = path.toMPath

  /** the context of all declarations in this body */
  def getInnerContext: Context

  /**
    * the set of named statements, indexed by name
    * if a statement has an alternativeName, it occurs twice in this map
    */
  protected val statements = new scala.collection.mutable.HashMap[LocalName,Declaration]

  /** anything pertaining to the narrative structure */
  private object narrativeStructure {
    /** the DPath of this Body as a document */
    val dpath = path.toMPath.toDPath
    /** this Body as a document (sharing the same metadata) */
    val document = new Document(dpath, ModuleLevel, Some(self))
    document.metadata = metadata
    /** call a function on all logical declarations and their parent document */
    def traverse(f: (Document,SRef) => Unit): Unit = {traverse(document, f)}
    private def traverse(doc: Document, f: (Document,SRef) => Unit): Unit = {
      doc.getDeclarations.foreach {
        case r: SRef => f(doc, r)
        case childDoc: Document => traverse(childDoc, f)
        case _ =>
      }
    }
  }
  import narrativeStructure._

  /**
    * Get all transitive [[IncludeData inclusions]], but not inclusion of itself.
    */
  def getAllIncludes: List[IncludeData]

  /**
    * Get self [[IncludeData inclusion]].
    */
  def selfInclude: IncludeData

  /**
    * Get all transitive-reflexive [[IncludeData inclusions]].
    */
  def getAllIncludesWithSelf: List[IncludeData] = getAllIncludes :+ selfInclude

  /** true iff a declaration for a name is present */
  def declares(name: LocalName) = statements.isDefinedAt(name)
  /** the list of names of all declarations */
  def domain: List[LocalName] = getDeclarations.map(_.name)

  /** retrieve a declaration */
  def getO(name : LocalName) : Option[Declaration] = statements.get(name)

  def getMostSpecific(name: LocalName): Option[(Declaration, LocalName)] = getMostSpecific(name, LocalName(Nil))

  /** retrieves the most specific applicable declaration
    * @param name the name of the declaration
    * @param rest the suffix that has been split off so far; this argument should be omitted in calls from outside this class
    * @return the most specific (longest prefix of name) known declaration (if any) and the remaining suffix
    */
  private def getMostSpecific(name: LocalName, rest : LocalName) : Option[(Declaration, LocalName)] =
    statements.get(name) match {
      case Some(d) => Some((d, rest))
      case None => name match {
        case LocalName(Nil) => None //should be impossible
        case !(n) => None
        case ln \ n => getMostSpecific(ln, n / rest)
      }
    }
  /** adds a named declaration, throws exception if name already declared
    *  @param d declaration to add
    *  @param at the position where to add, at end by default
    * this throws an errors if a declaration for that name already exists; exception: if the two declarations are equivalent, the old one is overridden
    */
  def add(d : Declaration, at: AddPosition = AtEnd): Unit = {
    val existedAlready = addDecl(d)
    if (!existedAlready)
      addRef(d, at, false)
  }

  /** like add, but treats the second argument as the name of a [[NarrativeElement]] */
  def addAfterNarrative(d: Declaration, after: LocalName): Unit = {
    addDecl(d)
    addRef(d, After(after), true)
  }

  /** add a declaration to the content hash map only
    * return true if an equivalent element was overridden
    */
  private def addDecl(s : Declaration) : Boolean = {
    val name = s.name
    val existedAlready = statements.get(name) match {
      case None =>
        false
      case Some(old) =>
        if (s equivalentTo old) {
          if (!s.isGenerated && old.isGenerated) {
            // this error is needed, e.g., to avoid replacing an induced include with a later original one, thus changing the logical order
            throw AddError(s, s"redundancy, an equivalent declaration for $name was already generated")
          }
        } else {
          throw AddError(s, s"name clash, a declaration for $name already exists")
        }
        true
    }
    statements(name) = s
    addAlternativeNames(s)
    existedAlready
  }

  /** delete a named declaration (does not have to exist)
    *  @return the deleted declaration
    */
  def delete(name : LocalName): Option[Declaration] = {
    statements.get(name) map {s =>
      statements -= s.name
      deleteAlternativeNames(s)
      deleteRef(name)
      s
    }
  }

  /* adding/deleting the entry in the document
   *
   * @param afterNarrative if at==After(n), whether n is a narrative name
   */
  private def addRef(s: Declaration, at: AddPosition = AtEnd, afterNarrative: Boolean = true): Unit = {
    val inDoc = s.relativeDocumentHome
    val doc = asDocument.getLocally(inDoc) match {
      case Some(d: Document) => d
      case Some(_) => throw AddError(s, s"narrative element $inDoc exists in theory $path but is not a document")
      case _ => throw AddError(s, s"document $inDoc does not exist in theory $path")
    }
    val ref = SRef(doc.path, s.path)
    val afterSRef = at match {
      case After(a) =>
        val aN = if (afterNarrative) a
        else SRef(doc.path, path.toMPath ? a).name // name of SRef to a
        After(aN)
      case Before(a) =>
        val aN = if (afterNarrative) a
        else SRef(doc.path, path.toMPath ? a).name // name of SRef to a
        Before(aN)
      case at => at
    }
    doc.add(ref, afterSRef)
  }
  /** delete the SRef for the Declaration with local name 'name' */
  private def deleteRef(name: LocalName): Unit = {
    traverse {case (parDoc,r) =>
      if (r.target.name == name) parDoc.delete(r.name)
    }
  }

  /* adding/deleting hashmap entry for the alias */

  private def addAlternativeNames(s: Declaration): Unit = {
    s.alternativeNames foreach {a =>
      if (statements.isDefinedAt(a))
        throw AddError(s, s"name clash, a declaration for the alternative name $a already exists")
      statements(a) = s
    }
  }
  private def deleteAlternativeNames(s: Declaration): Unit = {
    s.alternativeNames foreach {a => statements -= a}
  }

  /** updates a named declaration (preserving the order) */
  def update(s : Declaration): Unit = {
    statements.get(s.name) match {
      case Some(old) =>
        deleteAlternativeNames(old)
        statements(s.name) = s
        addAlternativeNames(s)
      case None =>
        add(s)
    }
  }
  /** moves a declaration to the end of its section (if the relDocHome of ln has changed, it is also moved to the new section)
    *  also moves all subsequent ln/X declarations (and updates their relDocHome)
    */
  def reorder(ln: LocalName): Unit = {
    statements.get(ln) match {
      case Some(s) =>
        deleteRef(ln)
        addRef(s)
        // now reorder all SRefs to declarations ln/X
        traverse {case (doc, r) =>
          val rln = r.target.name
          if (rln.startsWith(ln) && rln != ln) {
            val rs = statements(rln)
            rs.setDocumentHome(s.relativeDocumentHome)
            deleteRef(ln)
            addRef(s)
          }
        }
      case None => throw ImplementationError("declaration does not exist")
    }
  }

  /** true iff no declarations present */
  def isEmpty = statements.isEmpty
  /** the narrative structure */
  def asDocument = document
  /** the list of declarations in narrative order, includes generated declarations */
  def getDeclarations: List[Declaration] = {
    var decs: List[Declaration] = Nil
    traverse {case (_,r) =>
      val s = statements(r.target.name)
      decs ::= s
    }
    decs.reverse
  }

  /** header as a string (without definiens) */
  protected def outerString : String
  /** body as a string */
  def innerString = {
    def makeStrings(doc: Document, indent: Int): List[(Int,String)] = {
      doc.getDeclarations.flatMap {
        case r: SRef =>
          val s = statements(r.target.name)
          if (!s.isGenerated) List((indent,s.toString)) else Nil
        case d: Document =>
          (indent, "document " + d.name.last.toPath) :: makeStrings(d, indent+1)
        case ne => List((indent, ne.toString))
      }
    }
    makeStrings(document,1).map {case (ind, s) => repeatString("  ", ind) + s}.mkString("\n")
  }
  /** outerString, definiens, innerString */
  override def toString = outerString + df.map(d => " = " + d.toString).getOrElse("") + "\n" + innerString

  /** common inner nodes: definiens (metadata is part of document) */
  protected def headerNodes: Seq[Node] = df.toList.map(d => <definition>{d.toNode}</definition>)

  /** getPrimitiveDeclarations, with narrative structure */
  protected def innerNodes = {
    def makeNodes(doc: Document): scala.xml.NodeSeq = {
      val nodes = doc.getDeclarations.flatMap {
        case r: SRef =>
          val s = statements(r.target.name)
          if (!s.isGenerated) s.toNode else Nil
        case oe: opaque.OpaqueElement => oe.toNode
        case d: Document =>
          <document name={d.name.last.toPath}>{makeNodes(d)}</document>
      }
      doc.getMetaDataNode ++ nodes
    }
    makeNodes(document)
  }
  def streamInnerNodes(rh: presentation.RenderingHandler): Unit = {
    def streamNodes(doc: Document): Unit = {
      headerNodes.foreach(rh(_))
      doc.getMetaDataNode.foreach(rh(_))
      doc.getDeclarations.foreach {
        case r: SRef =>
          val s = statements(r.target.name)
          if (!s.isGenerated)
            s.toNode(rh)
        case oe: opaque.OpaqueElement =>
          oe.toNode(rh)
        case d: Document =>
          rh << s"""<omdoc name="${d.name.last.toPath}">"""
          streamNodes(d)
          rh << "</omdoc>"
      }
    }
    streamNodes(document)
  }

  /** getDeclarationsElaborated, without narrative structure */
  protected def innerNodesElab = getDeclarationsElaborated.map(_.toNode)
}
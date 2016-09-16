package info.kwarc.mmt.api.modules
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.libraries._
import info.kwarc.mmt.api.symbols._
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.utils._

import scala.xml.Node
import documents._

/**
 * Body represents the content of modules, i.e., a set of declarations.
 * 
 * It is mixed into elements that contain declarations, e.g., declared theories.
 * 
 * It stores both the logical [[Declaration]]s as well as their narrative structure.
 * The former uses a hash from [[LocalName]] to [[Declaration]], which completely ignores narrative structure.
 * In particular, declaration names must be unique independent of the narrative grouping.
 * The latter is stored as a [[Document]], which holds [[SRef]] to the logical declarations.  
*/
trait Body extends ContentElement with ContainerElement[Declaration] {self =>
   /** the set of named statements, indexed by name
    * if a statement has an alternativeName, it occurs twice in this map
    */
   protected val statements = new scala.collection.mutable.HashMap[LocalName,Declaration]

   /** anything pertaining to the narrative structure */
   private object narrativeStructure {
      /** the DPath of this Body as a document */
      val dpath = path.toMPath.toDPath
      /** this Body as a document (sharing the same metadata) */
      val document = new Document(dpath, true, Some(self))
      document.metadata = metadata
      /** call a function on all logical declarations and their parent document */
      def traverse(f: (Document,SRef) => Unit) {traverse(document, f)}
      private def traverse(doc: Document, f: (Document,SRef) => Unit) {
         doc.getDeclarations.foreach {
            case r: SRef => f(doc, r)
            case childDoc: Document => traverse(childDoc, f)
            case _ =>
         }
      }
   }
   import narrativeStructure._

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
    *  @param afterOpt the name of the [[Declaration]] after which to add; if omitted, add at end
    */
   def add(d : Declaration, afterOpt: Option[LocalName] = None) {
      addDecl(d)
      addRef(d, afterOpt map {n => After(n, false)})
   }
   
   /** like add, but treats the second argument as the name of a [[NarrativeElement]] */
   def addAfterNarrative(d: Declaration, after: LocalName) {
      addDecl(d)
      addRef(d, Some(After(after, true)))
   }
   
   /** add a declaration to the content hash map only */
   private def addDecl(s : Declaration) {
      val name = s.name
      if (statements.isDefinedAt(name)) {
         throw AddError("a declaration for the name " + name + " already exists")
      }
      statements(name) = s
      addAlternativeNames(s)
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

   /** name of the declaration after which to add a declaration */ 
   private case class After(name: LocalName, isNarrativeName: Boolean)
   /* adding/deleting the entry in the document */
   private def addRef(s: Declaration, afterOpt: Option[After]) {
      val inDoc = s.relativeDocumentHome
      val doc = asDocument.getLocally(inDoc) match {
         case Some(d: Document) => d
         case Some(_) => throw AddError(s"narrative element $inDoc exists in theory $path but is not a document")
         case _ => throw AddError(s"document $inDoc does not exist in theory $path")
      }
      val ref = SRef(doc.path, s.path)
      val afterSRef = afterOpt map {a =>
        if (a.isNarrativeName) a.name
        else SRef(doc.path, path.toMPath ? a.name).name // name of SRef to afterOpt
      }
      doc.add(ref, afterSRef)
   }
   /** delete the SRef for the Declaration with local name 'name' */
   private def deleteRef(name: LocalName) {
      traverse {case (parDoc,r) =>
         if (r.target.name == name) parDoc.delete(r.name)
      }
   }
   
   /* adding/deleting hashmap entry for the alias */
   
   private def addAlternativeNames(s: Declaration) {
      s.alternativeNames foreach {a =>
         if (statements.isDefinedAt(a))
            throw AddError("a declaration for the name " + a + " already exists")
         statements(a) = s
      }
   }
   private def deleteAlternativeNames(s: Declaration) {
      s.alternativeNames foreach {a => statements -= a}
   }
   
   /** updates a named declaration (preserving the order) */
   def update(s : Declaration) {
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
   def reorder(ln: LocalName) {
      statements.get(ln) match {
         case Some(s) =>
            deleteRef(ln)
            addRef(s, None)
            // now reorder all SRefs to declarations ln/X
            traverse {case (doc, r) =>
               val rln = r.target.name
               if (rln.startsWith(ln) && rln != ln) {
                  val rs = statements(rln)
                  rs.setDocumentHome(s.relativeDocumentHome)
                  deleteRef(ln)
                  addRef(s, None)
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
   /** the list of declarations in the order of addition, excludes generated declarations */
   def getPrimitiveDeclarations = getDeclarations.filterNot(_.isGenerated)
   /** the list of declarations using elaborated declarations where possible */
   def getDeclarationsElaborated = getDeclarations.filterNot(ElaboratedElement.is)
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
   def streamInnerNodes(rh: presentation.RenderingHandler) {
      def streamNodes(doc: Document) {
         rh(doc.getMetaDataNode)
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
}
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
trait Body extends ContentElement {self =>
   /** the set of named statements, indexed by name
    * if a statement has an alternativeName, it occurs twice in this map
    */
   protected val statements = new scala.collection.mutable.HashMap[LocalName,Declaration]

   /** anything pertaining to the narrative structure */
   private object narrativeStructure {
      /** the DPath of this Body as a document */
      val dpath = path.toMPath.toDPath
      /** this Body as a document */
      val document = new Document(dpath, true, Some(self))
      /** retrieval of a nested Document */
      def getDocument(dn: LocalName): Option[Document] = {
         if (dn.length == 0) Some(document)
         else {
            getDocument(dn.init).flatMap {parDoc =>
               parDoc.getO(LocalName(dn.last)).flatMap {
                 case d: Document => Some(d)
                 case _ => None
               }
            }
         }
      }
      /** call a function on all logical declarations and their parent document */
      def traverse(f: (Document,LocalName) => Unit) {traverse(document, f)}
      private def traverse(doc: Document, f: (Document,LocalName) => Unit) {
         doc.getDeclarations.foreach {
            case r: SRef => f(doc, r.name)
            case childDoc: Document => traverse(childDoc, f)
            case _ => // impossible
         }
      }
   }
   import narrativeStructure._

   /** true iff a declaration for a name is present */ 
   def declares(name: LocalName) = statements.isDefinedAt(name)
   /** the set of names of all declarations */
   def domain = statements.keySet
   /** retrieve a named declaration, may throw exception if not present */ 
   def get(name : LocalName) : Declaration = statements(name)
   /** retrieve a declaration */ 
   def getO(name : LocalName) : Option[Declaration] = statements.get(name)
   /** same as get(LocalName(name)) */ 
   def get(name : String) : Declaration = get(LocalName(name))
   /** retrieves the most specific applicable declaration
    * @param name the name of the declaration
    * @param rest the suffix that has been split off so far; this argument should be omitted in calls from outside this class 
    * @return the most specific (longest prefix of name) known declaration (if any) and the remaining suffix
    */
   def getMostSpecific(name: LocalName, rest : LocalName = LocalName(Nil)) : Option[(Declaration, LocalName)] =
      statements.get(name) match {
         case Some(d) => Some((d, rest))
         case None => name match {
            case LocalName(Nil) => None //should be impossible
            case !(n) => None
            case ln \ n => getMostSpecific(ln, n / rest)
         }
      }
   /** adds a named declaration, throws exception if name already declared
    *  @param afterOpt if given, the new declaration is inserted after that one; otherwise at end
    *  @param inDoc nested document in which to insert; toplevel by default otherwise  
    */
   def add(s : Declaration, afterOpt: Option[LocalName] = None, inDoc: LocalName = LocalName.empty) {
      val name = s.name
      if (statements.isDefinedAt(name)) {
         throw AddError("a declaration for the name " + name + " already exists")
      }
      statements(name) = s
      addAlternativeName(s)
      val doc = getDocument(inDoc).getOrElse {
         throw AddError(s"document $inDoc does not exist in theory $path")
      }
      val ref = new SRef(doc.path, s.name, s.path)
      doc.add(ref, afterOpt)
   }
   /** adds narrative structure */
   def addNarration(ne: NarrativeElement, afterOpt: Option[LocalName] = None) {
      val inDoc = dpath.dropPrefix(ne.path).getOrElse {
         throw AddError(s"document ${ne.path} does have prefix $dpath")
      }
      val parDoc = getDocument(inDoc).getOrElse {
         throw AddError(s"document $inDoc does not exist in theory $path")
      }
      parDoc.add(ne, afterOpt)
   }
   /** delete a named declaration (does not have to exist)
    *  @return the deleted declaration
    */
   def delete(name : LocalName): Option[Declaration] = {
      statements.get(name) map {s =>
         statements -= s.name
         deleteAlternativeName(s)
         traverse {case (parDoc,ln) =>
            if (ln == name) parDoc.delete(ln)
         }
         s
      }
   }
   private def addAlternativeName(s: Declaration) {
      s.alternativeName foreach {a =>
         if (statements.isDefinedAt(a))
            throw AddError("a declaration for the name " + a + " already exists")
         statements(a) = s
      }
   }
   private def deleteAlternativeName(s: Declaration) {
      s.alternativeName.foreach {a => statements -= a}
   }
   /** updates a named declaration (preserving the order) */
   def update(s : Declaration) = {
      if (statements.isDefinedAt(s.name)) {
         deleteAlternativeName(statements(s.name))
         statements(s.name) = s
         addAlternativeName(s)
      } else
         add(s)
   }
   /** true iff no declarations present */
   def isEmpty = statements.isEmpty
   /** the narrative structure */
   def asDocument = document
   /** the list of declarations in narrative order, includes generated declarations */
   def getDeclarations: List[Declaration] = {
      var decs: List[Declaration] = Nil
      traverse {case (_,ln) =>
         decs ::= statements(ln)
      }
      decs.reverse
   }
   /** the list of declarations in the order of addition, excludes declarations generated by MMT */
   def getPrimitiveDeclarations = getDeclarations.filterNot(_.isGenerated)
   /** the list of declarations using elaborated declarations where possible */
   def getDeclarationsElaborated = getDeclarations.filterNot(_.hasBeenElaborated)
   /** getPrimitiveDeclarations, with narrative structure */
   protected def innerNodes = document.getDeclarations.flatMap {
      case r: SRef =>
         val s = statements(r.name)
         if (s.isGenerated) s.toNode else Nil
      case d: Document =>
         throw ImplementationError("documents inside modules not fully implemented yet")
      case ne => ne.toNode 
   }
   /** getDeclarationsElaborated, without narrative structure */
   protected def innerNodesElab = getDeclarationsElaborated.map(_.toNode)
   protected def innerString =
      if (getPrimitiveDeclarations.isEmpty) ""
      else getPrimitiveDeclarations.map("\t" + _.toString).mkString(" = {\n", "\n", "\n}")
}
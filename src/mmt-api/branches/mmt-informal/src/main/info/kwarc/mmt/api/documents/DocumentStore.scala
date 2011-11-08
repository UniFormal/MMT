package info.kwarc.mmt.api.documents
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.frontend._

/**
 * A DocStore holds a set of documents indexed by their URIs.
 */
class DocStore(abox : ontology.RelStore, report : Report) {
   private val documents = new scala.collection.mutable.HashMap[DPath,Document]
   /** adds a document to the DocStore */
   def add(d : NarrativeElement) {d match {
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
      catch {case _ => throw NotFound(p)}
   }
   /**
    * deletes a document
    * @param the document to be deleted
    * @return the list now orphaned documents and modules that are declared in this documents
    * */
   def delete(p: DPath) : List[Path] = {
      documents.get(p) match {
         case None => Nil
         case Some(doc) =>
            documents -= p
            doc.getLocalItems map (_.target)
      }
   }
   /** deletes all documents */
   def clear {documents.clear}
}
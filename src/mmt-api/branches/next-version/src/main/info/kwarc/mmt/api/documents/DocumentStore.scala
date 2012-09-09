package info.kwarc.mmt.api.documents
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.frontend._

abstract class RODocStore {
   def get(p : DPath) : Document
}

/**
 * A DocStore holds a set of documents indexed by their URIs.
 */
class DocStore(mem : ROMemory, report : Report) extends RODocStore {
   private val documents = new scala.collection.mutable.HashMap[DPath,Document]
   /** adds a document to the DocStore */
   def add(d : NarrativeElement) {d match {
      case d : Document =>
         documents(d.path) = d
         mem.ontology += ontology.IsDocument(d.path)
      case r : XRef =>
         val d = try {documents(r.parent)} catch {case _ => throw AddError("document does not exist in " + r)}
         d.add(r)
         mem.ontology += ontology.Declares(d.path, r.target)
   }}
   /** retrieves a document from the DocStore */
   def get(p : DPath) = {
      try {documents(p)}
      catch {case _ => throw NotFound(p)}
   }
   /** retrieves all documents in any order */
   def getDocuments : List[Document] = documents.values.toList 
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
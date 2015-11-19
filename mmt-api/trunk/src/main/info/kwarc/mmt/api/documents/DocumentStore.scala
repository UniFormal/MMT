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
   def add(n : NarrativeElement) {
      n.parentOpt.map {p =>
         val doc = documents.get(p).getOrElse {
            throw AddError("containing document does not exist " + p)
         }
         doc.add(n)
      }
      n match {
         case d : Document =>
            documents(d.path) = d
         case _ =>
      }
   }
   /** retrieves a document from the DocStore */
   def get(p : DPath) = {
      try {documents(p)}
      catch {case _ : Exception => throw NotFound(p)}
   }
   /** retrieves all documents in any order */
   def getDocuments : List[Document] = documents.values.toList 
   /**
    * deletes a document
    * @param the path of the document to be deleted
    * @return the deleted document
    * */
   def delete(p: DPath) : Option[Document] = {
      documents.get(p) map {doc =>
         documents -= p
         doc
      }
   }
   /** deletes all documents */
   def clear {documents.clear}
}
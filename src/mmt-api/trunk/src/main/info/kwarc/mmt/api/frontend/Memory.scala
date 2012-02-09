package info.kwarc.mmt.api.frontend
import info.kwarc.mmt.api._
import presentation._
import libraries._
import documents._
import ontology._

abstract class ROMemory {
   val ontology : RelStore
   val content : Lookup
}

class Memory(val report : Report) extends ROMemory {
  
   /** maintains the ontology */
   val ontology = new RelStore(report)
   /** maintains all content elements */
   val content = new Library(this, report)
   /** maintains all presentation elements */
   val presentation = new NotationStore(this, report)
   /** maintains all narrative elements */
   val narration = new DocStore(this, report)
 
}
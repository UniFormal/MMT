package info.kwarc.mmt.api.frontend

import info.kwarc.mmt.api.documents._
import info.kwarc.mmt.api.libraries._
import info.kwarc.mmt.api.ontology._
import info.kwarc.mmt.api.notations._
import info.kwarc.mmt.api.ontology.rdf.Database

/** A read-only abstraction of memory */
abstract class ROMemory {
  val database: Database
  val content: Lookup
  val ontology : RelStore
}

/** Groups all stateful objects of the controller that store MMT data */
class Memory(extman: ExtensionManager, val database: Database, val report: Report) extends ROMemory {

  /** maintains the ontology */
  val ontology = new RelStore(database,report)
  /** maintains all previous versions (if any) of content elements */
  val previousContent = new Library(extman, report, None)
  /** maintains all content elements */
  val content = new Library(extman, report, Some(previousContent))
  
  /** forgets everything */
  def clear {
    database.clear
    content.clear
  }
}

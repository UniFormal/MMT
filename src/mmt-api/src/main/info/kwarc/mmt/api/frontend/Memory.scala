package info.kwarc.mmt.api.frontend

import info.kwarc.mmt.api.documents._
import info.kwarc.mmt.api.libraries._
import info.kwarc.mmt.api.ontology._

/** A read-only abstraction of memory */
abstract class ROMemory {
  val ontology: RelStore
  val content: Lookup
}

/** Groups all stateful objects of the controller that store MMT data */
class Memory(extman: ExtensionManager, val report: Report) extends ROMemory {

  /** maintains the ontology */
  val ontology = new RelStore(report)
  /** maintains all previous versions (if any) of content elements */
  val previousContent = new Library(extman, report, None)
  /** maintains all content elements */
  val content = new Library(extman, report, Some(previousContent))

  /** forgets everything */
  def clear {
    ontology.clear
    content.clear
  }
}

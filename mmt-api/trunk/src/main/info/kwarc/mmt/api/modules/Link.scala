package info.kwarc.mmt.api.modules
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.symbols._
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.utils._

/**
 * A Link represents an MMT link unifying structures and views.
 */
trait Link {
   /** the domain of the link */
   val from : TheoryObj
   /** the codomain of the link */
   val to : TheoryObj
   protected def main_components : List[Content]
}

 /**
  * A DeclaredLink represents an MMT link given by a set assignments.<p> 
  *
  * Declared links are constructed empty. {@link info.kwarc.mmt.api.modules.StatementSet[Assignment]} is derived to hold a set of name-indexed assignments.
  */
trait DeclaredLink extends Link with Body[Assignment, LinkImport] {
   /** the set of assignments */
   //def isMapped(source : LocalPath) : Boolean = assignments.isDefinedAt(source)
   def components = main_components ::: valueList
}

  /**
   * A DeclaredLink represents an MMT link given by an existing morphism. 
   */
trait DefinedLink extends Link with ModuleDefiniens[Morph] {
   /** the definiens of the link */
   def components = main_components ::: List(df)
}
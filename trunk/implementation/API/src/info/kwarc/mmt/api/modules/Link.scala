package info.kwarc.mmt.api.modules
import jomdoc._
import jomdoc.symbols._
import jomdoc.objects._
import jomdoc.utils._

/**
 * A Link represents an MMT link unifying structures and views.
 */
trait Link {
   /** the domain of the link */
   val from : MPath
   /** the codomain of the link */
   val to : MPath
   protected def main_components : List[Content]
}

 /**
  * A DeclaredLink represents an MMT link given by a set assignments.<p> 
  *
  * Declared links are constructed empty. {@link jomdoc.modules.StatementSet[Assignment]} is derived to hold a set of name-indexed assignments.
  */
trait DeclaredLink extends Link with StatementSet[Assignment, LinkImport] {
   /** the set of assignments */
   //def isMapped(source : LocalPath) : Boolean = assignments.isDefinedAt(source)
   protected def statementsToNode = valueList.map(_.toNode)
   protected def statementsToString = valueList.map("\t" + _.toString).mkString("\n{", "\n", "\n}")
   def components = main_components ::: valueList
}

  /**
   * A DeclaredLink represents an MMT link given by an existing morphism. 
   */
trait DefinedLink extends Link {
   /** the definiens of the link */
   val df : Morph
   protected def dfToNode = <definition>{df.toOBJNode}</definition>
   protected def dfToString = df.toString
   def components = main_components ::: List(df)
}
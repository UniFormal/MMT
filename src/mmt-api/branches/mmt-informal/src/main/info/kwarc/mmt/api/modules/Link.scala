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
   val from : Term
   /** the codomain of the link */
   val to : Term
   def toTerm : Term
   def path : ContentPath
   val isImplicit : Boolean
   
   protected def innerNodes : Seq[scala.xml.Node]
   /** name, from, to, etc */
   protected def outerComponents : List[Content]
   /** the body of the link */
   protected def innerComponents : List[Content]
   /** outerComponents as a string */
   protected def outerString : String
   /** innerComponents as a string */
   protected def innerString : String
   def components = outerComponents ::: innerComponents
   override def toString = outerString + innerString
}

 /**
  * A DeclaredLink represents an MMT link given by a set of assignments.<p>
  *
  * Declared links are constructed empty. {@link info.kwarc.mmt.api.modules.StatementSet[Assignment]} is derived to hold a set of name-indexed assignments.
  */
trait DeclaredLink extends Link with Body[Assignment]

  /**
   * A DeclaredLink represents an MMT link given by an existing morphism.
   */
trait DefinedLink extends Link with ModuleDefiniens
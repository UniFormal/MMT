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
   def from : Term
   /** the codomain of the link */
   def to : Term
   def codomainAsContext = to match {
       case ComplexTheory(cont) => cont
       case _ => throw ImplementationError("codomain of link must be theory")
    }
   
   def toTerm : Term
   def path : ContentPath
   val isImplicit : Boolean
   
   protected def innerNodes : Seq[scala.xml.Node]
   /** header as a string */
   protected def outerString : String
   /** body as a string */
   protected def innerString : String
   override def toString = outerString + innerString
}

 /**
  * A DeclaredLink represents an MMT link given by a set of assignments.<p>
  *
  * Declared links are constructed empty. Body is derived to hold a set of name-indexed assignments.
  */
trait DeclaredLink extends Link with Body {
   val metamorph: Option[Term] = None
 }

  /**
   * A DeclaredLink represents an MMT link given by an existing morphism.
   */
trait DefinedLink extends Link with ModuleDefiniens
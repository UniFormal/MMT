package info.kwarc.mmt.api.modules
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.symbols._
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.utils._

/**
 * represents an MMT link unifying structures and views.
 */
trait Link extends ContentElement {
   /** the domain of the link */
   def from : Term
   /** the codomain of the link */
   def to : Term
   def codomainAsContext = to match {
       case ComplexTheory(cont) => cont
       case _ => throw ImplementationError("codomain of link must be theory")
    }

   def toTerm : Term
   val isImplicit : Boolean

   /** the prefix used when translating declarations along this link */
   def namePrefix: LocalName

   protected def innerNodes : Seq[scala.xml.Node]
   /** header as a string */
   protected def outerString : String
   /** body as a string */
   protected def innerString : String
   override def toString = outerString + innerString
}

/**
  * represents an MMT link given by a set of assignments.
  */
trait DeclaredLink extends Link with Body {
   val metamorph: Option[Term] = None
 }

  /**
   * represents an MMT link given by an existing morphism
   */
trait DefinedLink extends Link with ModuleDefiniens {
   def df = dfC.get.getOrElse(Morph.empty)
}

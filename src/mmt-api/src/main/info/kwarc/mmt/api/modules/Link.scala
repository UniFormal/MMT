package info.kwarc.mmt.api.modules
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.symbols._
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.utils._

/**
 * abstract representation of an atomic MMT morphism, unifies structures and views
 * 
 * Assignments are
 * 1) [[Constant]]s, whose name is the qualified name (always starts with [[ComplexStep]])
 * of a domain [[Constant]]
 * and whose definiens is codomain [[Term]]
 * or
 * 2) or accordingly with [[DeclaredStructure]]s with definiens
 */
trait Link extends ModuleOrLink {
   /** the domain of the link */
   def fromC: TermContainer
   /** the codomain of the link (mutable for views but not structures) */
   def toC: AbstractTermContainer
   
   @deprecated("only used by deprecated methods", "")
   private def unknownTheory = OMSemiFormal(Text("unchecked", "not inferred"))
   @deprecated("use fromC", "")
   def from = fromC.get.getOrElse(unknownTheory)
   @deprecated("use toC", "")
   def to = toC.get.getOrElse(unknownTheory)
   def codomainAsContext = to match {
       case ComplexTheory(cont) => cont
       case _ => throw ImplementationError("codomain of link must be theory")
    }

   def toTerm : Term
   val isImplicit : Boolean

   /** like getIncludes but also with includes of parametric theories and their instantiations */
   def getIncludes: List[(MPath,Term)] = getDeclarations.flatMap {
     case LinkInclude(_, from, df) => List((from,df))
     case _ => Nil
   }
   
   /** the prefix used when translating declarations along this link */
   def namePrefix: LocalName

   protected def innerNodes : Seq[scala.xml.Node]
   /** header as a string */
   protected def outerString : String
   /** body as a string */
   protected def innerString : String
   override def toString = outerString + "\n" + innerString
}
